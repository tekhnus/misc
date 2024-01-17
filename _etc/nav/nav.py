#!/usr/bin/env python
import sys
import json
import curses
import contextlib
import collections
import traceback
import tree_sitter as ts
import curio as cu
import curio.debug

comms = cu.Queue()
fromui = cu.Queue()


async def maincoro():
    fname = sys.argv[1]
    outname = sys.argv[2]
    async with cu.TaskGroup(wait="all") as g:
        comm_task = await g.spawn(die_on_error, comm, fname)
        outcomm_task = await g.spawn(die_on_error, outcomm, outname)
        ui_task = await g.spawn(die_on_error, ui)


async def comm(fname):
    lang = ts.Language("/home/zahaaar/local/tree-sitter-langs/bin/cpp.so", "cpp")
    states = collections.defaultdict(lambda: new_state(lang))
    current = None
    async with cu.aopen(fname, "rb") as strm:
        while (msg := await strm.readline()):
            deser = json.loads(msg)
            if "show" in deser:
                current = deser["show"]
                assert current in states
            if "content" in deser:
                bufer = deser["buffer"]
                parser, _, _, _, oldloc, _ = states[bufer]
                newtext = deser["content"].encode()
                newtree = parser.parse(newtext)
                cursor = newtree.walk()
                res = process(cursor)[0]
                rawres = rawprocess(cursor)[0]
                states[bufer] = (parser, newtree, res, rawres, oldloc, newtext)
            if "loc" in deser:
                bufer = deser["buffer"]
                parser, oldtree, oldres, oldrawres, _, oldtext = states[bufer]
                newloc = tuple(deser["loc"])
                states[bufer] = (parser, oldtree, oldres, oldrawres, newloc, oldtext)
            if current is not None:
                await comms.put(("state", (states, current)))
    comms.put(None)


async def outcomm(fname):
    async with cu.aopen(fname, "wb") as strm:
        while (msg := await fromui.get()):
            await strm.write((json.dumps(msg) + "\n").encode())
            await strm.flush()


def new_state(language):
    parser = ts.Parser()
    parser.set_language(language)
    tree = parser.parse(b"")
    cursor = tree.walk()
    res = process(cursor)[0]
    rawres = rawprocess(cursor)[0]
    return (parser, tree, res, rawres, (0, 0), b"")


def traverse_children(cur):
    if not (has_child := cur.goto_first_child()):
        return
    ok = True
    while ok:
        yield
        ok = cur.goto_next_sibling()
    cur.goto_parent()


def rawprocess(cur):
    sp, ep = cur.node.start_point, cur.node.end_point
    if not cur.node.is_named:
        return []
    n = (cur.current_field_name() or "") + ":" + cur.node.type
    res = [(sp, ep, n, n)]
    for _ in traverse_children(cur):
        if not cur.node.is_named:
            continue
        res.extend(rawprocess(cur))
    return [res] 


def process(cur):
    sp, ep = cur.node.start_point, cur.node.end_point

    if not cur.node.is_named:
        return []

    if cur.node.type in ["preproc_call", "preproc_include", "comment", "alias_declaration", "access_specifier", "namespace_alias_definition", "enum_specifier", "using_declaration", "init_declarator", "static_assert_declaration"]:
        return []

    if cur.node.type in ["translation_unit"]:
        children = []
        for _ in traverse_children(cur):
            children.extend(process(cur))
        return [[(sp, ep, str(cur.node.type), "file")] + children]

    if cur.node.type in ["class_specifier", "struct_specifier"]:
        children = []
        name = "<name not found>"
        for _ in traverse_children(cur):
            if cur.current_field_name() == "name":
                name = cur.node.text.decode()
            if cur.current_field_name() == "body":
                for _ in traverse_children(cur):
                    children.extend(process(cur))
        return [[(sp, ep, "class", name)] + children]

    if cur.node.type in ["namespace_definition"]:
        children = []
        name = "<anonymous>"
        for _ in traverse_children(cur):
            if cur.current_field_name() == "name":
                name = cur.node.text.decode()
            if cur.current_field_name() == "body":
                for _ in traverse_children(cur):
                    children.extend(process(cur))
        if not name:
            name = "<anonymous>"
        return [[(sp, ep, "namespace", name)] + children]

    if cur.node.type in ["declaration", "field_declaration"]:
        res = []
        for _ in traverse_children(cur):
            if cur.current_field_name() == "type":
                # nested types
                res = process(cur)
            if cur.current_field_name() == "declarator":
                res = process(cur)
        if res:
            child, = res
            _, _, kind, name = child[0]
            child[0] = (sp, ep, kind, name)
            res = [child]
        return res

    if cur.node.type in ["reference_declarator", "pointer_declarator", "friend_declaration"]:
        res = []
        for _ in traverse_children(cur):
            if cur.node.is_named:
                res = process(cur)
        return res

    if cur.node.type in ["function_definition"]:
        res = []
        for _ in traverse_children(cur):
            if cur.current_field_name() == "declarator":
                res = process(cur)
        if res:
            child, = res
            _, _, kind, name = child[0]
            child[0] = (sp, ep, kind, name)
            res = [child]
        return res

    if cur.node.type in ["preproc_function_def"]:
        res = []
        name = "<name not found>"
        for _ in traverse_children(cur):
            if cur.current_field_name() == "name":
                name = cur.node.text.decode()
        return [[(sp, ep, "macro", name)]]

    if cur.node.type in ["function_declarator"]:
        res = []
        name = "<name not found>"
        for _ in traverse_children(cur):
            if cur.current_field_name() == "declarator":
                name = cur.node.text.decode()
        return [[(sp, ep, "function", name)]]

    if cur.node.type in ["field_identifier"]:
        return []
        # return [[(sp, ep, "variable", cur.node.text.decode())]]

    if cur.node.type in ["template_declaration"]:
        res = []
        for _ in traverse_children(cur):
            if cur.node.is_named:
                res = process(cur)
        return res

    res = [(sp, ep, "?" + str(cur.node.type), "!" + str(cur.node.type))]
    for _ in traverse_children(cur):
        if not cur.node.is_named:
            continue
        kind = (cur.current_field_name() or "") + ":" + cur.node.type
        res.append([(sp, ep, kind, "!" + kind)])
    return [res] 


async def ui():
    # raise RuntimeError("recv")
    async with any_event_mode(), setup_curses() as stdscr, cu.TaskGroup() as g:
        stdscr.nodelay(True)
        stdscr.keypad(True)
        curses.mousemask(curses.ALL_MOUSE_EVENTS | curses.REPORT_MOUSE_POSITION)

        await g.spawn(die_on_error, inputgetter, stdscr, comms)
        curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_BLACK)
        curses.init_pair(2, curses.COLOR_GREEN, curses.COLOR_BLACK)
        curses.init_pair(3, curses.COLOR_MAGENTA, curses.COLOR_BLACK)
        root, rawroot, loc = None, None, None
        current = None
        hlrow = None
        start, end = None, None
        rows = []
        highlights = []
        showraw = False
        def procitem(ite):
            nonlocal row, ident, highlights
            content, *children = ite
            if len(content) != 4:
                raise ValueError(f"wrong content: {content}")
            start, end, kind, name = content
            st = " " * ident + " " + name
            if start <= loc:
                highlights = [len(rows)]
            if kind == "function":
                attrs = curses.color_pair(2)
            elif kind == "class" or kind == "namespace":
                attrs = curses.color_pair(3)
            else:
                attrs = curses.color_pair(1)
            rows.append((content, st, attrs))
            ident += 1
            for child in children:
                procitem(child)
            ident -= 1
        while (commsg := await comms.get()) is not None:
            tag, d = commsg
            if tag == "state":
                state, current = d
                _, _, root, rawroot, loc, _ = state[current]
                ident = 0
                rows = []
                highlights = []
                procitem(root if not showraw else rawroot)
            elif tag == "mouse":
                _, x, y, _, buttons = d
                hlrow = int(y)
                if buttons & curses.BUTTON1_CLICKED:
                    clicked_row = hlrow + start
                    if 0 <= clicked_row < len(rows):
                        nod, _, _ = rows[clicked_row]
                        await fromui.put(("clicked", (current, nod)))                    
            elif tag == "keyboard":
                ch = d
                if ch == ord("r"):
                    showraw = not showraw
                    ident = 0
                    rows = []
                    highlights = []
                    procitem(root if not showraw else rawroot)
            else:
                fatal(f"unknown tag: {tag}")

            nrows, ncols = stdscr.getmaxyx()
            stdscr.erase()
            if highlights:
                main_hl = max(highlights)
            else:
                main_hl = 0
            start = main_hl - nrows // 2
            if start < 0:
                start = 0
            end = start + nrows
            if end > len(rows):
                end = len(rows)
            r = 0
            for row, (_, st, attrs) in enumerate(rows):
                if not (start <= row < end):
                    continue
                if highlights and row == max(highlights):
                    attrs |= curses.A_STANDOUT
                if r == hlrow:
                    attrs |= curses.A_UNDERLINE
                if len(st) >= ncols:
                    st = st[:ncols - 4] + "..."
                stdscr.addstr(r, 0, st, attrs)
                r += 1
            stdscr.refresh()


async def inputgetter(stdscr, q):
    while True:
        char = stdscr.getch()
        if char == curses.ERR:
            await curio.sleep(0.1)
            continue
        if char == curses.KEY_MOUSE:
            ev = ("mouse", curses.getmouse())
        else:
            ev = ("keyboard", char)
        await q.put(ev)


@contextlib.asynccontextmanager
async def setup_curses():
    try:
        # Initialize curses
        stdscr = curses.initscr()

        # Turn off echoing of keys, and enter cbreak mode,
        # where no buffering is performed on keyboard input
        curses.noecho()
        curses.cbreak()

        # In keypad mode, escape sequences for special keys
        # (like the cursor keys) will be interpreted and
        # a special value like curses.KEY_LEFT will be returned
        stdscr.keypad(1)

        # Start color, too.  Harmless if the terminal doesn't have
        # color; user can test with has_color() later on.  The try/catch
        # works around a minor bit of over-conscientiousness in the curses
        # module -- the error return from C start_color() is ignorable.
        try:
            curses.start_color()
        except:
            pass

        yield stdscr
    finally:
        # Set everything back to normal
        if 'stdscr' in locals():
            stdscr.keypad(0)
            curses.echo()
            curses.nocbreak()
            curses.endwin()


@contextlib.asynccontextmanager
async def any_event_mode():
    sys.stdout.write("\033[?1003h\n")
    try:
        yield
    finally:
        sys.stdout.write("\033[?1003h\n")


async def die_on_error(coro, *args, **kwargs):
    try:
        await coro(*args, **kwargs)
    except Exception as e:
        fatal(traceback.format_exc())


def fatal(msg):
    print(msg, file=sys.stderr)
    sys.exit(1)


def main():
    cu.run(die_on_error, maincoro)
