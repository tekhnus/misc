import collections


def graph_list_to_hash(gl):
    vs, es = gl
    g = {}
    for v in vs:
        g[v] = []
    for (s, t) in es:
        g[s].append(t)
    return g


class GraphHashView:
    def __init__(self, g):
        self._graph = g

    def successors(self, v):
        return self._graph[v]


def dfs_recursive(g, s):
    visited = set()

    def _dfs(v):
        yield v
        visited.add(v)
        for u in g.successors(v):
            if not u in visited:
                yield from _dfs(u)

    return _dfs(s)


def dfs_iterative(g, v):
    visited = set()
    st = collections.deque([v])
    while st:
        u = st.pop()
        if u in visited:
            continue
        yield u
        visited.add(u)
        st.extend(reversed(g.successors(u)))


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
