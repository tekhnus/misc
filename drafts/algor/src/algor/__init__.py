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
import random


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

    def outbound_edges(self, u):
        return [(eid, self._es[eid][1]) for eid in self._ix[u]]

    def reversed(self):
        return Graph(self._vs, [(eid, v, u) for eid, (u, v) in self._es.items()])


class GraphFilteredView:
    def __init__(self, gv, vs):
        self._orig_view = gv
        self._vset = vs

    def outbound_edges(self, v):
        return [(e, u) for e, u in self._orig_view.outbound_edges(v) if u in self._vset]


def dfs_recursive(g, vs):
    visited = set()

    def _dfs(v):
        yield ("enter", v)
        visited.add(v)
        for _, u in g.outbound_edges(v):
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
        st.extend(("enter", w) for _, w in reversed(g.outbound_edges(u)))


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
        ed, p = pred[x]
        path.append((ed, p, x))
        x = p
    return list(reversed(path))


class GraphFilteredByEdgeWeight:
    def __init__(self, g, wg):
        self._g = g
        self._wg = wg

    def outbound_edges(self, u):
        for eid, v in self._g.outbound_edges(u):
            if self._wg[eid]:
                yield eid, v


def forward(eid):
    return (eid, False)


def backward(eid):
    return (eid, True)


def reverse(eid):
    e, flg = eid
    return (e, not flg)


def double_graph(g):
    e = [
        *[(forward(eid), u, v) for eid, u, v in g.edges],
        *[(backward(eid), v, u) for eid, u, v in g.edges],
    ]
    return Graph(g.vs, e)


def edmonds_karp(s, t, g, wg):
    rest = double_graph(g)
    rest_wg = {
        **{forward(eid): w for eid, w in wg.items()},
        **{backward(eid): 0 for eid in wg.keys()},
    }
    wgh = 0
    g = GraphFilteredByEdgeWeight(rest, rest_wg)
    while (path := find_path_bfs(s, t, g)) is not None:
        pathwgh = min(rest_wg[e] for e, _, _ in path)
        wgh += pathwgh
        for e, _, _ in path:
            rest_wg[e] -= pathwgh
            rest_wg[reverse(e)] += pathwgh
    used_wgh = {e: rest_wg[backward(e)] for e in wg}
    return wgh, used_wgh


def partition(xs, bounds, pivot_value):
    left, right = bounds
    i = left
    j = right - 1

    while i <= j:
        # The slices [left:i] and [j+1:right] are good.
        while i <= j and xs[i] < pivot_value:
            i += 1
        # Either we're done or i is swappable.
        while i <= j and xs[j] > pivot_value:
            j -= 1
        # Either we're done or j is swappable.
        if i > j:
            # We're done.
            break
        # i and j are swappable.
        xs[i], xs[j] = xs[j], xs[i]
        # The slices [left:i+1] and [j:right] are good.
        i += 1
        j -= 1
        # The slices [left:i] and [j+1:right] are good.
    # The slices [left:i] and [j+1:right] are good.
    # i > j, e.g. i >= j+1.
    # Thus the slices [left:j+1] and [i:right] are good.
    # Therefore the slice [j+1:i] contains the pivot value.

    # The first loop iteration moved both i and j,
    # so both [left:j+1] and [i:right] are smaller than [left:right]
    return j + 1, i


def _quicksort(xs, bounds, rng):
    left, right = bounds
    if right - left < 2:
        return
    pivot_value = xs[rng.randrange(left, right)]
    m1, m2 = partition(xs, bounds, pivot_value)
    _quicksort(xs, (left, m1), rng)
    _quicksort(xs, (m2, right), rng)


def quicksort(xs, rng=None):
    if rng is None:
        rng = random
    bounds = (0, len(xs))
    _quicksort(xs, bounds, rng)
