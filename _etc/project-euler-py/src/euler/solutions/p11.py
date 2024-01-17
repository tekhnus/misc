import functools
import operator
import pathlib

from .. import grid


DIRECTIONS = [
    (+1, +0),
    (+0, +1),
    (+1, +1),
    (+1, -1),
]


def p11():
    with open(pathlib.Path(__file__).parent / "p11.txt") as f:
        g = [[int(n) for n in row.split()] for row in f]

    positions = grid.iter_positions(g)
    group_slices = (grid.make_slice(p, d, 4) for p in positions for d in DIRECTIONS)
    group_values = (v for s in group_slices if (v := grid.get(g, s)) is not None)
    products = (functools.reduce(operator.mul, v) for v in group_values)
    return max(products)


if __name__ == "__main__":
    print(p11())
