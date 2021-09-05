"""
gl = graph represented by list
gh = graph represented by hash
s.c.c = strongly connected component
"""
import collections
import itertools
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

    # The following invariants are maintained during the rest of the procedure:
    # * for all x in xs[left:i]: x <= pivot_value
    # * for all x in xs[j+1:right]: x >= pivot_value
    while i <= j:
        while i <= j and xs[i] < pivot_value:
            i += 1
        # Either i-th element is swappable or slices meet/overlap.
        while i <= j and xs[j] > pivot_value:
            j -= 1
        # Either j-th element is swappable or slices meet/overlap.
        if i > j:
            # Slices meet/overlap.
            break
        # Slices do not meet/overlap and
        # i-th and j-th elements are swappable.
        xs[i], xs[j] = xs[j], xs[i]
        i += 1
        j -= 1
    # Slices meet/overlap.
    # Therefore the slice xs[j+1:i] contains only the pivot value.

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


def mergesort_top_down(xs):
    bounds = (0, len(xs))
    xs2 = [x for x in xs]
    _mergesort_top_down(xs2, bounds, xs)


def _mergesort_top_down(src, bounds, dst):
    left, right = bounds
    if right - left < 2:
        return
    mid = (left + right + 1) // 2
    _mergesort_top_down(dst, (left, mid), src)
    _mergesort_top_down(dst, (mid, right), src)
    _mergesorted(src, (left, mid), src, (mid, right), dst, bounds)


def _mergesorted(src_a, bounds_a, src_b, bounds_b, dst, dst_bounds):
    a, a_end = bounds_a
    b, b_end = bounds_b
    d, d_end = dst_bounds
    while a != a_end or b != b_end:
        if b == b_end or (a != a_end and src_a[a] <= src_b[b]):
            dst[d] = src_a[a]
            a += 1
        else:
            dst[d] = src_b[b]
            b += 1
        d += 1


def mergesort_bottom_up(xs):
    chunk = 1
    buf_a = xs
    buf_b = [None for _ in xs]
    while chunk < len(xs):
        _merge_chunks(buf_a, buf_b, chunk)
        chunk *= 2
        buf_a, buf_b = buf_b, buf_a
    if buf_a is not xs:
        for i, x in enumerate(buf_a):
            xs[i] = x


def _merge_chunks(buf_a, buf_b, chunk):
    for chunk_x in range(0, len(buf_a), 2 * chunk):
        chunk_y = min(chunk_x + chunk, len(buf_a))
        chunk_y_end = min(chunk_x + 2 * chunk, len(buf_a))
        _mergesorted(
            buf_a,
            (chunk_x, chunk_y),
            buf_a,
            (chunk_y, chunk_y_end),
            buf_b,
            (chunk_x, chunk_y_end),
        )


class ListSortedCounter:
    def __init__(self, lower, upper):
        self._freq = [0 for _ in range(upper - lower)]
        self._lower = lower

    def __setitem__(self, k, v):
        self._freq[k - self._lower] = v

    def __getitem__(self, k):
        return self._freq[k - self._lower]

    def items(self):
        for k, v in enumerate(self._freq):
            if v == 0:
                continue
            yield k + self._lower, v


class CounterBasedCounter:
    def __init__(self):
        self._cnt = collections.Counter()

    def __setitem__(self, k, v):
        self._cnt[k] = v

    def __getitem__(self, k):
        return self._cnt[k]

    def items(self):
        return sorted(self._cnt.items())


def counting_sort(xs, key=lambda x: x, counter_cls=CounterBasedCounter):
    index = counter_cls()
    for x in xs:
        index[key(x)] += 1
    prev = 0
    for v, cnt in index.items():
        index[v] = prev
        prev += cnt
    res = [None for _ in xs]
    for x in xs:
        k = key(x)
        res[index[k]] = x
        index[k] += 1
    return res


def radix_sort(xs, keys, inner_sort=None):
    if inner_sort is None:
        inner_sort = lambda ys, key: counting_sort(ys, key)
    ys = xs
    for key in reversed(keys):
        ys = inner_sort(ys, key=key)
    return ys


def nth_digit_getter(n):
    p = 10 ** n

    def fn(k):
        return (k // p) % 10

    return fn


class DLCons:
    def __init__(self, val, prev, next_):
        self.val = val
        self.prev = prev
        self.next = next_


class DLList:
    def __init__(self):
        self._head = None

    def iter_forward(self):
        elem = self._head
        while elem is not None:
            yield elem
            elem = elem.next

    def iter_values_forward(self):
        return (e.val for e in self.iter_forward())

    def prepend(self, v):
        self._head = DLCons(v, None, self._head)
        if self._head.next is not None:
            self._head.next.prev = self._head

    def delete(self, e):
        if e.prev is not None:
            e.prev.next = e.next
        else:
            self._head = e.next
        if e.next is not None:
            e.next.prev = e.prev


class StaticHashTable:
    def __init__(self, bucket_count):
        self._element_count = 0
        self._bucket_count = bucket_count
        self._buckets = [DLList() for _ in range(self._bucket_count)]

    def update(self, items):
        for k, v in items:
            self[k] = v

    def __len__(self):
        return self._element_count

    def __getitem__(self, k):
        h = self._get_bucket(k)
        for kk, vv in self._buckets[h].iter_values_forward():
            if kk == k:
                return vv
        raise KeyError(k)

    def _get_bucket(self, k):
        return hash(k) % self._bucket_count

    def __setitem__(self, k, v):
        h = self._get_bucket(k)
        for e in self._buckets[h].iter_forward():
            kk, _ = e.val
            if kk == k:
                e.val = (k, v)
                return
        self._element_count += 1
        self._buckets[h].prepend((k, v))

    def pop(self, k):
        h = self._get_bucket(k)
        for e in self._buckets[h].iter_forward():
            kk, vv = e.val
            if kk == k:
                self._buckets[h].delete(e)
                self._element_count -= 1
                return vv
        raise KeyError(k)

    def __delitem__(self, k):
        self.pop(k)

    def items(self):
        for b in self._buckets:
            yield from b.iter_values_forward()


def move_hash_table(old_table, *args, **kwargs):
    new_table = StaticHashTable(*args, **kwargs)
    new_table.update(old_table.items())

    return new_table


class HashTable:
    def __init__(self, max_load_factor=0.75):
        self._tab = None
        self._max_load_factor = max_load_factor
        self._max_element_count = None

        self._set_tab(StaticHashTable(11))

    def _set_tab(self, newtab):
        self._tab = newtab
        self._max_element_count = int(newtab._bucket_count * self._max_load_factor)

    def __len__(self):
        return len(self._tab)

    def __getitem__(self, k):
        return self._tab[k]

    def __setitem__(self, k, v):
        if len(self._tab) >= self._max_element_count:
            self._set_tab(move_hash_table(self._tab, 2 * self._tab._bucket_count))
        self._tab[k] = v

    def pop(self):
        return self._tab.pop()

    def __delitem__(self, k):
        del self._tab[k]

    def items(self):
        return self._tab.items()


class StaticOpenAddressingTable:
    def __init__(self, size):
        self._size = size
        self._data = [None for _ in range(size)]
        self._len = 0

    def __len__(self):
        return self._len

    def update(self, items):
        for k, v in items:
            self[k] = v

    def __getitem__(self, k):
        visited = set()
        for i in self._seq(k):
            if i in visited:
                raise RuntimeError("loop!")
            kv = self._data[i]
            if kv is None:
                raise KeyError(k)
            kk, v = kv
            if kk == k:
                return v
            visited.add(i)

    def _seq(self, k):
        size = self._size
        h = hash(k)
        for n in itertools.count():
            h = (h + 1) % size
            yield h

    def set(self, k, v):
        for i in self._seq(k):
            kv = self._data[i]
            if kv is None:
                self._data[i] = (k, v)
                self._len += 1
                return False, None
            kk, oldv = kv
            if kk == k:
                self._data[i] = (k, v)
                return True, oldv

    def __setitem__(self, k, v):
        self.set(k, v)

    def items(self):
        for kv in self._data:
            if kv is None:
                continue
            yield kv


class StaticOpenAddressingTableWDeletion(StaticOpenAddressingTable):
    deleted = object()

    def __init__(self, size):
        super().__init__(size)
        self._len_wo_deleted = 0

    def len_with_deleted(self):
        return super().__len__()

    def __len__(self):
        return self._len_wo_deleted

    def __getitem__(self, k):
        v = super().__getitem__(k)
        if v is self.deleted:
            raise KeyError(k)
        return v

    def pop(self, k):
        oldval = super().__getitem__(k)
        if oldval is self.deleted:
            raise KeyError(k)
        super().__setitem__(k, self.deleted)
        self._len_wo_deleted -= 1

    def __delitem__(self, k):
        self.pop(k)

    def __setitem__(self, k, v):
        updated, oldval = self.set(k, v)
        if not updated or oldval is self.deleted:
            self._len_wo_deleted += 1

    def items(self):
        for k, v in super().items():
            if v is self.deleted:
                continue
            yield k, v


def move_open_table(old_table, *args, **kwargs):
    new_table = StaticOpenAddressingTableWDeletion(*args, **kwargs)
    new_table.update(old_table.items())

    return new_table


class OpenHashTable:
    def __init__(self, max_load_factor=0.75):
        self._tab = None
        self._max_load_factor = max_load_factor
        self._max_element_count = None

        self._set_tab(StaticOpenAddressingTableWDeletion(11))

    def _set_tab(self, newtab):
        self._tab = newtab
        self._max_element_count = int(newtab._size * self._max_load_factor)

    def __len__(self):
        return len(self._tab)

    def __getitem__(self, k):
        return self._tab[k]

    def __setitem__(self, k, v):
        if self._tab.len_with_deleted() >= self._max_element_count:
            self._set_tab(move_open_table(self._tab, 2 * self._tab._size))
        self._tab[k] = v

    def pop(self):
        return self._tab.pop()

    def __delitem__(self, k):
        del self._tab[k]

    def items(self):
        return self._tab.items()
