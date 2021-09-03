"""
gl = graph represented by list
gh = graph represented by hash
s.c.c = strongly connected component
"""
import collections
import math
import heapq
import operator
import dataclasses


class Heap:
    def __init__(self, items):
        self._items = []
        for i in items:
            self.push(i)

    def pop(self):
        val = self._items[0]
        if len(self._items) > 1:
            self._items[0] = self._items.pop()
            self._move_to_bottom(0)
        else:
            self._items = []
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


class EquivalenceRelation:
    def __init__(self, objects):
        self._sets = {obj: {obj} for obj in objects}

    def are_equivalent(self, a, b):
        return b in self._sets[a]

    def declare_equivalent(self, a, b):
        self._sets[a] = self._sets[b] = self._sets[a] | self._sets[b]


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
        self._keys.add(key)


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

    def edge_ends(self, e):
        return self._es[e]

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
    st = collections.deque([(None, None, v)])
    while st:
        e, pred, u = st.popleft()
        if u in visited:
            continue
        if pred is not None:
            yield e, pred, u
        visited.add(u)
        st.extend((eid, u, w) for eid, w in g.outbound_edges(u))


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


def greedy_tree(v, g, pri):
    q = IndexedHeap([(v, (0, None))])
    reached = set()
    while q:
        vert, (dist, prd) = q.pop()
        yield vert, (dist, prd)
        reached.add(vert)
        for edg, ver in g.outbound_edges(vert):
            if ver in reached:
                continue
            x = pri(edg, dist)
            q.push_or_update(ver, (x, vert))


def dijkstra(v, g, wg):
    best = {}
    pred = {}
    for vert, (dist, prd) in greedy_tree(v, g, lambda edg, dist: dist + wg[edg]):
        best[vert] = dist
        pred[vert] = prd
    del pred[v]
    return best, pred


@dataclasses.dataclass
class Ring:
    zero: object
    sum: object
    product: object


def pairwise_distances(g, wg):
    infty = sum(x for x in wg.values() if x > 0) + 1
    ring = Ring(zero=infty, sum=min, product=operator.add)
    m1, _ = weight_matrix(g, wg, infty)
    m = matrix_power(m1, len(g.vs) - 1, ring=ring)
    mcheck = matrix_mul(m, m1, ring=ring)
    if mcheck != m:
        raise ValueError("graph contains a negative cycle")
    return remove_infinity(m, infty)


def weight_matrix(g, wg, infty):
    res = {(u, v): infty for u in g.vs for v in g.vs}
    pred = {}
    for eid, u, v in g.edges:
        w = wg[eid]
        if res[u, v] > w:
            res[u, v] = w
            pred[u, v] = u
    for u in g.vs:
        res[u, u] = 0
    return res, pred


def matrix_power(m, k, *, ring):
    if k == 0:
        raise ValueError("not implemented")
    if k == 1:
        return m
    mhalfk = matrix_power(m, k // 2, ring=ring)
    malmostk = matrix_mul(mhalfk, mhalfk, ring=ring)
    if k % 2 == 0:
        return malmostk
    return matrix_mul(malmostk, m, ring=ring)


def matrix_mul(x, y, *, ring):
    is_ = [i for i, _ in x.keys()]
    js = [j for _, j in x.keys()]
    ks = [k for _, k in y.keys()]
    res = {}
    for i in is_:
        for k in ks:
            v = ring.zero
            for j in js:
                v = ring.sum(v, ring.product(x[i, j], y[j, k]))
            res[i, k] = v
    return res


def remove_infinity(m, infty):
    return {k: v for k, v in m.items() if v != infty}


def floyd_warshall(g, wg):
    infty = sum(x for x in wg.values() if x > 0) + 1
    m, pred = weight_matrix(g, wg, infty)
    for v in g.vs:
        for i in g.vs:
            for j in g.vs:
                thru_v = m[i, v] + m[v, j]
                if thru_v < m[i, j]:
                    m[i, j] = thru_v
                    pred[i, j] = pred[v, j]
    return (remove_infinity(m, infty), pred)


def kruskal(g, wg):
    e = EquivalenceRelation(g.vs)
    sorted_edges = sorted((w, eid) for eid, w in wg.items())
    weight = 0
    tree = []
    for w, eid in sorted_edges:
        u, v = g.edge_ends(eid)
        if e.are_equivalent(u, v):
            continue
        weight += w
        tree.append(eid)
        e.declare_equivalent(u, v)
    return weight, tree


def prim(v, g, wg):
    weight = 0
    pred = {}
    for vert, (dist, prd) in greedy_tree(v, g, lambda edg, _: wg[edg]):
        weight += dist
        pred[vert] = prd
    del pred[v]
    return weight, pred


class Matrix:
    def __init__(self, vs, wghs=None):
        self._vs = vs
        self._wghs = wghs or {}

    @property
    def vs(self):
        return self._vs

    def copy(self):
        return Matrix(self._vs.copy(), self._wghs.copy())

    def __getitem__(self, k):
        return self._wghs.get(k, 0)

    def __setitem__(self, k, v):
        self._wghs[k] = v

    def __sub__(self, m):
        w = collections.defaultdict(int, self._wghs.copy())
        for k, v in m._wghs.items():
            w[k] -= v
        return Matrix(self._vs, dict(w))

    def __eq__(self, m):
        if not isinstance(m, Matrix):
            return False
        for u in self.vs:
            for v in self.vs:
                if self[u, v] != m[u, v]:
                    return False
        return True

    def __repr__(self):
        return repr(self._wghs)


def graph_to_matrix(g, wg):
    wghs = collections.defaultdict(int)
    for eid, u, v in g.edges:
        wghs[u, v] += wg[eid]
    return Matrix(g.vs, dict(wghs))


class MatrixGraphView:
    def __init__(self, mat, efilter):
        self._mat = mat
        self._efilter = efilter

    def successors(self, u):
        for v in self._mat.vs:
            if self._efilter(u, v, self._mat[u, v]):
                yield v


def nonzero_graph(mat):
    return MatrixGraphView(mat, lambda _1, _2, wgh: wgh > 0)


def find_path_bfs(s, t, g):
    pred = {}
    for e, u, v in bfs(g, s):
        pred[v] = (e, u)
        if v == t:
            break
    else:
        return None
    path = []
    x = t
    while x != s:
        p = pred[x]
        path.append((p, x))
        x = p
    return list(reversed(path))


def edmonds_karp(s, t, mat):
    rest = mat.copy()
    wgh = 0
    g = nonzero_graph(rest)
    while (path := find_path_bfs(s, t, g)) is not None:
        pathwgh = min(mat[u, v] for u, v in path)
        wgh += pathwgh
        for u, v in path:
            rest[u, v] -= pathwgh
            rest[v, u] += pathwgh
    return wgh, mat - rest
