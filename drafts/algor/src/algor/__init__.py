"""
gl = graph represented by list
gh = graph represented by hash
s.c.c = strongly connected component
"""
import collections
import math
import heapq


class IndexedHeap:
    def __init__(self, items):
        self._items = items

    def __bool__(self):
        return bool(self._items)

    def pop(self):
        best = min(self._items, key=lambda x: x[1])
        self._items.remove(best)
        return best

    def decrease_priority(self, key, p):
        for i, (k, _) in enumerate(self._items):
            if k == key:
                break
        else:
            raise ValueError(f"no such key: {key}")
        del self._items[i]
        self._items.append((key, p))

    def get_priority(self, key):
        for k, p in self._items:
            if k == key:
                return p
        return None


class Graph:
    def __init__(self, vs, es):
        self._vs = []
        self._es = {}
        self._ix = collections.defaultdict(list)

        self.vs_extend(vs)
        self.es_extend(es)

    @property
    def vs(self):
        return self._vs

    @property
    def edges(self):
        return ((eid, u, v) for eid, (u, v) in self._es.items())

    def vs_extend(self, vs):
        self._vs.extend(vs)

    def es_extend(self, es):
        for eid, u, v in es:
            self._es[eid] = (u, v)
            self._ix[u].append(eid)

    def successors(self, u):
        return [self._es[eid][1] for eid in self._ix[u]]

    def outbound_edges(self, u):
        return [(eid, self._es[eid][1]) for eid in self._ix[u]]

    def reversed(self):
        return Graph(self._vs, [(eid, v, u) for eid, (u, v) in self._es.items()])


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


def ford_bellman(v, g, wg):
    best = {v: 0}
    pred = {}

    def upd(edgedata):
        eid, u, v = edgedata
        bestu = best.get(u)
        if bestu is None:
            return False
        cand = bestu + wg[eid]
        if (bestv := best.get(v) is None) or bestv > cand:
            best[v] = cand
            pred[v] = u
            return True
        return False

    n = len(g.vs)
    for _ in range(n - 1):
        if not any(upd(edata) for edata in g.edges):
            break
    else:
        if any(upd(edata) for edata in g.edges):
            raise ValueError("graph contains a nevative loop")
    return best, pred


def dijkstra(v, g, wg):
    best = {}
    pred = {}

    q = IndexedHeap([(u, math.inf) for u in g.vs if u != v] + [(v, 0)])

    while q:
        vert, dist = q.pop()
        best[vert] = dist
        for edg, ver in g.outbound_edges(vert):
            curbest = q.get_priority(ver)
            if curbest is None:
                continue
            x = dist + wg[edg]
            if x < curbest:
                newix = q.decrease_priority(ver, x)
                pred[ver] = vert

    return best, pred
