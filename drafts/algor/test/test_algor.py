import pytest
import algor


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


@pytest.mark.parametrize("gr,exp", [(g1, [1, 2, 3, 4])])
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
