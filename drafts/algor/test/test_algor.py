import pytest
import algor
import random


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


@pytest.mark.parametrize("xs", [[3, 1, 2], [5], [], [7, 7], [7, 7, 7]])
def test_quicksort(xs):
    exp = sorted(xs)
    algor.quicksort(xs, rng=random.Random(123))
    assert xs == exp
