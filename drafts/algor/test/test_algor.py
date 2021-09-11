import pytest
import algor
import random
import itertools
import collections
import re
import scipy.spatial


g1 = algor.Graph(
    [1, 2, 3, 4],
    [
        ("a", 1, 2),
        ("b", 1, 3),
        ("c", 2, 3),
        ("d", 3, 4),
    ],
)
ws = {
    "a": 3,
    "b": 1,
    "c": 7,
    "d": 5,
}


@pytest.mark.parametrize(
    "gr,exp",
    [
        (
            g1,
            [
                ("enter", 1),
                ("enter", 2),
                ("enter", 3),
                ("enter", 4),
                ("exit", 4),
                ("exit", 3),
                ("exit", 2),
                ("exit", 1),
            ],
        )
    ],
)
@pytest.mark.parametrize("f", [algor.dfs_recursive, algor.dfs_iterative])
def test_dfs(gr, exp, f):
    assert list(f(gr, [1])) == exp


@pytest.mark.parametrize("gr,exp", [(g1, [("a", 1, 2), ("b", 1, 3), ("d", 3, 4)])])
def test_bfs(gr, exp):
    assert list(algor.bfs(gr, 1)) == exp


@pytest.mark.parametrize("gr,vs,exp", [(g1, [3, 1, 2, 4], [4, 3, 2, 1])])
def test_topo_sort(gr, vs, exp):
    assert list(algor.topo_sort(vs, gr)) == exp


@pytest.mark.parametrize("gr,vs,exp", [(g1, [1], [(0, 1), (1, 2), (2, 3), (3, 4)])])
def test_strong_components(gr, vs, exp):
    assert list(algor.strong_components(vs, gr)) == exp


@pytest.mark.parametrize(
    "v,gr,ws,exp", [(1, g1, ws, [{1: 0, 2: 3, 3: 1, 4: 6}, {2: 1, 3: 1, 4: 3}])]
)
def test_ford_bellman(v, gr, ws, exp):
    assert list(algor.ford_bellman(v, gr, ws)) == exp


@pytest.mark.parametrize(
    "v,gr,ws,exp", [(1, g1, ws, [{1: 0, 2: 3, 3: 1, 4: 6}, {2: 1, 3: 1, 4: 3}])]
)
def test_dijkstra(v, gr, ws, exp):
    assert list(algor.dijkstra(v, gr, ws)) == exp


@pytest.mark.parametrize(
    "gr,ws,exp",
    [
        (
            g1,
            ws,
            {
                (1, 1): 0,
                (1, 2): 3,
                (1, 3): 1,
                (1, 4): 6,
                (2, 2): 0,
                (2, 3): 7,
                (2, 4): 12,
                (3, 3): 0,
                (3, 4): 5,
                (4, 4): 0,
            },
        )
    ],
)
def test_pairwise_distances(gr, ws, exp):
    assert algor.pairwise_distances(gr, ws) == exp


@pytest.mark.parametrize(
    "gr,ws,exp",
    [
        (
            g1,
            ws,
            (
                {
                    (1, 1): 0,
                    (1, 2): 3,
                    (1, 3): 1,
                    (1, 4): 6,
                    (2, 2): 0,
                    (2, 3): 7,
                    (2, 4): 12,
                    (3, 3): 0,
                    (3, 4): 5,
                    (4, 4): 0,
                },
                {(1, 2): 1, (1, 3): 1, (1, 4): 3, (2, 3): 2, (2, 4): 3, (3, 4): 3},
            ),
        )
    ],
)
def test_pairwise_distances(gr, ws, exp):
    assert algor.floyd_warshall(gr, ws) == exp


@pytest.mark.parametrize(
    "gr,ws,exp",
    [
        (
            g1,
            ws,
            (
                9,
                ["b", "a", "d"],
            ),
        )
    ],
)
def test_kruscal(gr, ws, exp):
    assert algor.kruskal(gr, ws) == exp


@pytest.mark.parametrize(
    "gr,ws,exp",
    [
        (
            g1,
            ws,
            (
                9,
                {2: 1, 3: 1, 4: 3},
            ),
        )
    ],
)
def test_prim(gr, ws, exp):
    assert algor.prim(1, gr, ws) == exp


@pytest.mark.parametrize(
    "s, t, gr, ws, exp",
    [
        (
            1,
            4,
            g1,
            ws,
            (
                4,
                {
                    "a": 3,
                    "b": 1,
                    "c": 3,
                    "d": 4,
                },
            ),
        )
    ],
)
def test_edmonds_karp(s, t, gr, ws, exp):
    assert algor.edmonds_karp(s, t, gr, ws) == exp


short_sequences = [list(seq) for seq in itertools.product(list(range(4)), repeat=4)]


@pytest.mark.parametrize(
    "xs", [[3, 1, 2], [5], [], [7, 7], [7, 7, 7], *short_sequences]
)
def test_quicksort(xs):
    xs = xs[:]
    exp = sorted(xs)
    algor.quicksort(xs, rng=random.Random(123))
    assert xs == exp


@pytest.mark.parametrize(
    "xs", [[3, 1, 2], [5], [], [7, 7], [7, 7, 7], *short_sequences]
)
@pytest.mark.parametrize("f", [algor.mergesort_top_down, algor.mergesort_bottom_up])
def test_mergesort(xs, f):
    xs = xs[:]
    exp = sorted(xs)
    f(xs)
    assert xs == exp


SKIP = object()


@pytest.mark.parametrize(
    "xs", [[3, 1, 2], [5], [], [7, 7], [7, 7, 7], *short_sequences]
)
@pytest.mark.parametrize("counter", [lambda: algor.ListSortedCounter(-100, 100), SKIP])
def test_counting_sort(xs, counter):
    args = {}
    if counter is not SKIP:
        args["counter_cls"] = counter
    res = algor.counting_sort(xs, **args)
    exp = sorted(xs)
    assert res == exp


trng = random.Random(1234)


@pytest.mark.parametrize(
    "xs", [[3, 1, 2], [5], [], [7, 7], [7, 7, 7], trng.choices(range(10000), k=100)]
)
def test_radix_sort(xs):
    keys = [algor.nth_digit_getter(n) for n in range(10, -1, -1)]
    res = algor.radix_sort(xs, keys=keys)
    exp = sorted(xs)
    assert res == exp


def random_instruction(rngen):
    if rngen.random() > 0.5:
        return ("insert", rngen.choice(range(100)), rngen.choice([1, 2, 3]))
    else:
        return ("delete", rngen.choice(range(100)))


def try_delete(m, k):
    try:
        del m[k]
    except KeyError as e:
        return e
    return None


@pytest.mark.parametrize(
    "insertions",
    [
        [],
        [("insert", "a", 42), ("insert", "b", 33), ("delete", "a")],
        [random_instruction(trng) for _ in range(10_000)],
    ],
)
@pytest.mark.parametrize(
    "cls",
    [algor.HashTable, algor.OpenHashTable, algor.Treap],
)
def test_hash_table(insertions, cls):
    h = cls()
    d = {}
    allkeys = set()
    for cmd, *args in insertions:
        if cmd == "insert":
            k, v = args
            h[k] = v
            d[k] = v
            allkeys.add(k)
        elif cmd == "delete":
            (k,) = args
            assert (try_delete(h, k) is not None) == (try_delete(d, k) is not None)
        else:
            raise ValueError("wrong test")
        assert len(h) == len(d)
        assert sorted(h.items()) == sorted(d.items())
        for k, v in d.items():
            assert h[k] == v
        for k in allkeys - d.keys():
            with pytest.raises(KeyError):
                h[k]


def _dumb_first_matching(pred, xs):
    for i, x in enumerate(xs):
        if pred(x):
            return i
    return len(xs)


def _dumb_equal_range(value, xs):
    return _dumb_first_matching(lambda x: x >= value, xs), _dumb_first_matching(
        lambda x: x > value, xs
    )


@pytest.mark.parametrize(
    "xs",
    itertools.chain(
        *[itertools.combinations_with_replacement([0, 1, 2], n) for n in range(9)]
    ),
)
def test_equal_range(xs):
    assert algor.equal_range(1, xs) == _dumb_equal_range(1, xs)


class _DumbSegmentTree:
    def __init__(self, size, monoid=algor.addition):
        self._monoid = monoid
        self._data = [monoid.unit for _ in range(size)]

    def op(self, rng, val):
        monoid = self._monoid
        for i in range(*rng):
            self._data[i] = monoid.op(self._data[i], val)

    def __getitem__(self, rng):
        monoid = self._monoid
        res = monoid.unit
        for i in range(*rng):
            res = monoid.op(res, self._data[i])
        return res


def _gen_segment_action(size):
    cmd = "op" if trng.random() < 0.5 else "get"
    left = trng.randrange(size)
    right = trng.randrange(size)
    val = trng.randrange(300)
    return cmd, ((left, right), val)


@pytest.mark.parametrize("actions", [[_gen_segment_action(10) for _ in range(100)]])
def test_segment_tree(actions):
    t = algor.SegmentTree(10)
    e = _DumbSegmentTree(10)
    for cmd, arg in actions:
        print(cmd, *arg)
        if cmd == "op":
            leafrng, val = arg
            t.op(leafrng, val)
            e.op(leafrng, val)
        elif cmd == "get":
            leafrng, _ = arg
            res = t[leafrng]
            exp = e[leafrng]
            assert res == exp


PATTERNS = ["".join(s) for s in itertools.product("abc", repeat=3)]
TEXTS = ["".join(s) for s in itertools.product("abc", repeat=5)]


@pytest.fixture(scope="module")
def matchers(request):
    return {s: algor.Matcher(s) for s in PATTERNS}


@pytest.mark.parametrize("pattern", PATTERNS)
@pytest.mark.parametrize("text", TEXTS)
def test_substring_search(pattern, text, matchers):
    aut = matchers[pattern]
    res = list(algor.search(aut, text))
    exp = list(i for i in range(len(text)) if text[i : i + len(pattern)] == pattern)
    assert res == exp


def _convex_hull(points):
    h = scipy.spatial.ConvexHull(points)
    return h.vertices


def _random_point():
    return (trng.random(), trng.random())


def _random_ngon(n):
    return [_random_point() for _ in range(n)]


@pytest.mark.parametrize("points", [_random_ngon(n) for n in range(3, 20) for _ in range(10)])
def test_convex_hull(points):
    res = algor.convex_hull(points)
    exp = _convex_hull(points)
    assert sorted(res) == sorted(exp)
