# This was an attempt to write a tiny framework
# for writing an application cosisting of a several
# producer/consumer routines forming a tree.
# Written in January 2020.
from collections import defaultdict


class Forest:
    def __init__(self):
        self._id = 0

        self._programs = {}
        self._child_ids = defaultdict(list)
        self._active = {}

    def add(self, program, parent=None):
        id_ = self._id
        self._id += 1

        self._programs[id_] = program
        self._child_ids[parent].append(id_)
        self._active[id_] = True

        return id_

    def run(self):
        for program in self._programs.values():
            next(program)

        should_continue = True
        while should_continue:
            should_continue = any(
                self._step_subtree(child, None)
                for child in self._child_ids[None]
            )

    def _step_subtree(self, id_, parent_value):
        if not self._active[id_]:
            return False

        values = (...,)
        while values and values[-1] == ...:
            try:
                values = self._programs[id_].send(parent_value)
            except StopIteration:
                self._active[id_] = False
                for child in self._child_ids[id_]:
                    self._close_subtree(child)
                return False
            if not isinstance(values, tuple):
                raise TypeError("Expected tuple, got {!r}".format(type(values)))
            for value in values:
                if value == ...:
                    continue
                for child in self._child_ids[id_]:
                    self._step_subtree(child, value)
            parent_value = None

        return True

    def _close_subtree(self, id_):
        self._programs[id_].close()
        self._active[id_] = False
        for child in self._child_ids[id_]:
            self._close_subtree(child)


def test():
    f = Forest()

    def a():
        yield
        yield "hello",
        yield "goodbye",
        print("a closed")

    def b():
        try:
            x = yield
            while True:
                print("b received", x)
                yield x + " from b", ...
                x = yield x + " from b again",
        finally:
            print("b closed")

    def c():
        try:
            x = yield
            while True:
                print("c received", x)
                x = yield ()
        finally:
            print("c closed")

    def d():
        try:
            x = yield
            while True:
                print("d received", x)
                x = yield ()
        finally:
            print("d closed")

    ax = f.add(a())
    bx = f.add(b(), ax)
    cx = f.add(c(), bx)
    dx = f.add(d(), bx)
    f.run()
    print("exiting")


if __name__ == "__main__":
    test()
