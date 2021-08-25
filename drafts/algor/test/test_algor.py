import pytest
import algor


g1 = (
    [1, 2, 3, 4],
    [
        (1, 2),
        (1, 3),
        (2, 3),
        (3, 4),
    ],
)


@pytest.mark.parametrize("gr,exp", [(g1, [1, 2, 3, 4])])
@pytest.mark.parametrize("f", [algor.dfs_recursive, algor.dfs_iterative])
def test_dfs(gr, exp, f):
    g = algor.GraphHashView(algor.graph_list_to_hash(gr))
    assert list(f(g, 1)) == exp


@pytest.mark.parametrize("gr,exp", [(g1, [1, 2, 3, 4])])
def test_bfs(gr, exp):
    g = algor.GraphHashView(algor.graph_list_to_hash(gr))
    assert list(algor.bfs(g, 1)) == exp
