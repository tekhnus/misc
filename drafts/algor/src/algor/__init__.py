"""
gl = graph represented by list
gh = graph represented by hash
s.c.c = strongly connected component
"""
import collections


def graph_list_to_hash(gl):
    vs, es = gl
    g = {}
    for v in vs:
        g[v] = []
    for (s, t) in es:
        g[s].append(t)
    return g


def gh_reversed(g):
    vs = list(g.keys())
    es = [(t, s) for s, ts in g.items() for t in ts]
    return graph_list_to_hash((vs, es))


class GraphHashView:
    def __init__(self, g):
        self._graph = g

    def successors(self, v):
        return self._graph[v]

    def reversed(self):
        return GraphHashView(gh_reversed(self._graph))


class GraphFilteredView:
    def __init__(self, gv, vs):
        self._orig_view = gv
        self._vset = vs

    def successors(self, v):
        return [u for u in self._orig_view.successors(v) if u in self._vset]


def dfs_recursive(g, vs):
    visited = set()

    def _dfs(v):
        yield ("enter", v)
        visited.add(v)
        for u in g.successors(v):
            if not u in visited:
                yield from _dfs(u)
        yield ("exit", v)

    while vs:
        yield from _dfs(vs.pop())


def dfs_iterative(g, vs):
    visited = set()
    st = collections.deque([("enter", v) for v in vs])
    while st:
        item = label, u = st.pop()
        if label == "exit":
            yield item
            continue
        if u in visited:
            continue
        yield item
        visited.add(u)
        st.append(("exit", u))
        st.extend(("enter", w) for w in reversed(g.successors(u)))


def bfs(g, v):
    visited = set()
    st = collections.deque([v])
    while st:
        u = st.popleft()
        if u in visited:
            continue
        yield u
        visited.add(u)
        st.extend(g.successors(u))


def topo_sort(vs, g):
    """
    Output all vertices of g reachable from any of vs.

    When the last element of an s.c.c. is outputted,
    all elements of all successor s.c.c. are already outputted.
    """
    for label, v in dfs_iterative(g, vs):
        if label == "exit":
            yield v


def strong_components(vs, g):
    """
    Output all vertices of g reachable from any of vs enumerated by strong components.

    The components are outputted in reversed topological order.
    """
    c = collections.deque(topo_sort(vs, g))
    nest = 0
    current_comp = 0
    for label, v in dfs_iterative(GraphFilteredView(g.reversed(), set(c)), c):
        if label == "enter":
            nest += 1
            yield current_comp, v
        else:
            nest -= 1
        if nest == 0:
            current_comp += 1
