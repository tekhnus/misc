"""
gl = graph represented by list
gh = graph represented by hash
s.c.c = strongly connected component
"""
import collections
import math
import heapq


class Heap:
    def __init__(self, items):
        self._items = []
        for i in items:
            self.push(i)

    def pop(self):
        val = self._items[0]
        self._items[0] = self._items.pop()
        self._move_to_bottom(0)
        return val

    def push(self, val):
        self._items.append(val)
        self._move_to_top(len(self._items) - 1)

    def _move_to_top(self, index):
        it = self._items
        while index > 0 and it[index] < it[(parent := ((index - 1) // 2))]:
            it[index], it[parent] = it[parent], it[index]
            index = parent

    def _move_to_bottom(self, index):
        it = self._items
        n = len(it)
        while True:
            left = 2 * index + 1
            if left >= n:
                break
            if it[index] > it[left]:
                it[index], it[left] = it[left], it[index]
                index = left
                continue
            right = left + 1
            if right >= n:
                break
            if it[index] > it[right]:
                it[index], it[right] = it[right], it[index]
                index = right
                continue
            break


class IndexedHeap:
    def __init__(self, items):
        items = list(items)
        self._heap = Heap([(p, k) for k, p in items])
        self._keys = set(k for k, _ in items)

    def __bool__(self):
        return bool(self._keys)

    def __contains__(self, k):
        return k in self._keys

    def pop(self):
        pri, key = None, object()
        while key not in self._keys:
            pri, key = self._heap.pop()
        self._keys.remove(key)
        return key, pri

    def push_or_update(self, key, p):
        self._heap.push((p, key))


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
    infty = sum(x for x in wg.values() if x > 0) + 1
    best = {**{u: infty for u in g.vs}, v: 0}
    pred = {}

    def upd(edgedata):
        eid, u, v = edgedata
        cand = best[u] + wg[eid]
        if best[v] > cand:
            best[v], pred[v] = cand, u
            return True
        return False

    for _ in range(len(g.vs) - 1):
        if not any(upd(edata) for edata in g.edges):
            break
    else:
        if any(upd(edata) for edata in g.edges):
            raise ValueError("graph contains a nevative cycle")
    return best, pred


def dijkstra(v, g, wg):
    infty = sum(x for x in wg.values() if x > 0) + 1
    best = {}
    pred = {}
    q = IndexedHeap([*[(u, (infty, None)) for u in g.vs if u != v], (v, (0, None))])
    while q:
        vert, (dist, prd) = q.pop()
        best[vert] = dist
        pred[vert] = prd
        for edg, ver in g.outbound_edges(vert):
            if ver not in q:
                continue
            x = dist + wg[edg]
            q.push_or_update(ver, (x, vert))
    del pred[v]
    return best, pred
