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
    If u ---> v and v -/-> u, then v will be outputted before u.
    """
    vs = set(vs)
    for label, v in dfs_iterative(g, vs):
        if label == "exit" and v in vs:
            yield v
