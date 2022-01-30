"""
gl = graph represented by list
gh = graph represented by hash
s.c.c = strongly connected component
"""
import collections
import itertools
import functools
import math
import operator
import dataclasses
import random


class Heap:
    def __init__(self, items, key=lambda x: x):
        self._key = key
        self._items = []
        for i in items:
            self.push(i)

    def __len__(self):
        return len(self._items)

    def top(self):
        return self._items[0]

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
        key = self._key
        it = self._items
        while index > 0 and key(it[index]) < key(it[(parent := ((index - 1) // 2))]):
            it[index], it[parent] = it[parent], it[index]
            index = parent

    def _move_to_bottom(self, index):
        key = self._key
        it = self._items
        n = len(it)
        while True:
            left = 2 * index + 1
            if left >= n:
                break
            if key(it[index]) > key(it[left]):
                it[index], it[left] = it[left], it[index]
                index = left
                continue
            right = left + 1
            if right >= n:
                break
            if key(it[index]) > key(it[right]):
                it[index], it[right] = it[right], it[index]
                index = right
                continue
            break


class Cell:
    def __init__(self, val, prev, next_):
        self.val = val
        self.prev = prev
        self.next = next_


class LinkedList:
    def __init__(self, seq=()):
        self._head = None
        self._tail = None
        self._len = 0
        for x in seq:
            self.append(x)

    def __len__(self):
        return self._len

    def __repr__(self):
        return repr(list(self.iter_values_forward()))

    def __iter__(self):
        return self.iter_values_forward()

    def iter_forward(self):
        elem = self._head
        while elem is not None:
            yield elem
            elem = elem.next

    def iter_values_forward(self):
        return (e.val for e in self.iter_forward())

    def prepend(self, v):
        self._head = Cell(v, None, self._head)
        if self._head.next is not None:
            self._head.next.prev = self._head
        else:
            self._tail = self._head
        self._len += 1

    def append(self, v):
        self._tail = Cell(v, self._tail, None)
        if self._tail.prev is not None:
            self._tail.prev.next = self._tail
        else:
            self._head = self._tail
        self._len += 1

    def splice(self, another):
        if another._head is None:
            return
        if self._tail is None:
            self._head = another._head
            self._tail = another.tail
            another._head = another._tail = None
            another._len = 0
            return
        self._tail.next = another._head
        another._head.prev = self._tail
        self._tail = another._tail
        another._head = another._tail = None
        self._len += another._len
        another._len = 0

    def delete(self, e):
        if e.prev is not None:
            e.prev.next = e.next
        else:
            self._head = e.next
        if e.next is not None:
            e.next.prev = e.prev
        self._len -= 1


@functools.singledispatch
def splice(dst, src):
    dst.splice(src)


@splice.register
def _(dst: list, src):
    dst.extend(src)
    src.clear()


class EquivalenceRelation:
    def __init__(self, container=LinkedList):
        self._container = container
        self._s = {}

    def are_equivalent(self, a, b):
        s = self._s
        a_class = s.setdefault(a, self._container([a]))
        b_class = s.setdefault(b, self._container([b]))
        return a_class is b_class

    def declare_equivalent(self, a, b):
        if self.are_equivalent(a, b):
            return
        s = self._s
        if len(s[a]) < len(s[b]):
            a, b = b, a
        sb = s[b]
        for x in sb:
            s[x] = s[a]
        splice(s[a], sb)


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


# Question: if in topo sort algorithm we replace
# dfs with dijkstra/prim/something else, do we get something interesting?

# Related question: is there some interesting optimization version of the topo sort problem,
# where we are required to output not just any topologically sorted sequence,
# but the best one w.r.t. some cost function?

# Question: if in the strong components algorithm
# in phase 2 we use not the g.reversed(),
# but some other arbitrary subgraph h not related to g,
# do we get some interpretable result?

# Question: can floyd-warshall algorithm be viewed
# as a special case of matrix pairwise distance algorithm
# with some peculiar matrix multiplication algorithm?

# DFS:                         neighbour function + initial vertices
# BFS:                         neighbour function + initial vertices
# topo sort:                   neighbour function + initial vertices
# dijkstra:                    neighbour function + weight function + initial vertices
# prim:                        neighbour function + weight function + initial vertices
# s.c.c.:                      neighbour function + reverse function + initial vertices
# edmonds-karp:                edges + weight function + s/t vertices
# ford-bellman:                edges + weight function + initial vertices
# kruskal:                     edges + weight function
# pairwise distance by matrix: weight matrix
# floyd-warshall:              weight matrix


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


def dfs_recursive(gf, vs):
    visited = set()

    def _dfs(v):
        yield ("enter", v)
        visited.add(v)
        for _, u in gf(v):
            if u not in visited:
                yield from _dfs(u)
        yield ("exit", v)

    while vs:
        yield from _dfs(vs.pop())


def dfs_iterative(gf, vs):
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
        st.extend(("enter", w) for _, w in reversed(gf(u)))


def bfs(gf, v):
    visited = set()
    st = collections.deque([(None, None, v)])
    while st:
        e, pred, u = st.popleft()
        if u in visited:
            continue
        if pred is not None:
            yield e, pred, u
        visited.add(u)
        st.extend((eid, u, w) for eid, w in gf(u))


def topo_sort(vs, gf):
    """
    Output all vertices of g reachable from any of vs.

    When the last element of an s.c.c. is outputted,
    all elements of all successor s.c.c. are already outputted.
    """
    for label, v in dfs_iterative(gf, vs):
        if label == "exit":
            yield v


def strong_components(vs, g):
    """
    Output all vertices of g reachable from any of vs enumerated by strong components.

    The components are outputted in reversed topological order.
    """
    c = collections.deque(topo_sort(vs, g.outbound_edges))
    nest = 0
    current_comp = 0
    for label, v in dfs_iterative(GraphFilteredView(g.reversed(), set(c)).outbound_edges, c):
        if label == "enter":
            nest += 1
            yield current_comp, v
        else:
            nest -= 1
        if nest == 0:
            current_comp += 1


def ford_bellman(vs, g, wg):
    best = {v: 0 for v in vs}
    pred = {}

    def upd(edgedata):
        eid, u, v = edgedata
        cand = best[u] + wg[eid]
        if v not in best:
            res = 2
        elif best[v] > cand:
            res = 1
        else:
            res = 0
        if res > 0:
            best[v], pred[v] = cand, u
        return res

    while True:
        agg_res = max(upd(edata) for edata in g.edges)
        if agg_res == 0:
            # No new vertices and no updates.
            return best, pred
        if agg_res == 1:
            # No new vertices, but there are updates.
            raise ValueError("graph contains a nevative cycle")
        # New vertices were found.


def greedy_tree(vs, gf, pri):
    q = IndexedHeap([(v, (0, None)) for v in vs])
    reached = set()
    while q:
        vert, (dist, prd) = q.pop()
        yield vert, (dist, prd)
        reached.add(vert)
        for edg, ver in gf(vert):
            if ver in reached:
                continue
            x = pri(edg, dist)
            q.push_or_update(ver, (x, vert))


def dijkstra(vs, gf, wg):
    best = {}
    pred = {}
    for vert, (dist, prd) in greedy_tree(vs, gf, lambda edg, dist: dist + wg[edg]):
        best[vert] = dist
        pred[vert] = prd
    for v in vs:
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
    e = EquivalenceRelation()
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


def prim(vs, gf, wg):
    weight = 0
    pred = {}
    for vert, (dist, prd) in greedy_tree(vs, gf, lambda edg, _: wg[edg]):
        weight += dist
        pred[vert] = prd
    for v in vs:
        del pred[v]
    return weight, pred


def find_path_bfs(s, t, gf):
    pred = {}
    for e, u, v in bfs(gf, s):
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

    def outbound_edges_filtered_by_weight(v):
        for eid, w in rest.outbound_edges(v):
            if rest_wg[eid]:
                yield eid, w

    while (path := find_path_bfs(s, t, outbound_edges_filtered_by_weight)) is not None:
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


def quicksort_partial(xs, k, rng=None):
    if rng is None:
        rng = random
    bounds = (0, len(xs))
    if not (0 <= k < len(xs)):
        raise ValueError("k is out of bounds")
    return _quicksort_partial(xs, bounds, k, rng)


def _quicksort_partial(xs, bounds, k, rng):
    left, right = bounds
    assert left <= k < right
    if right - left == 1:
        # assert k == 0
        return xs[left]
    if right - left == 0:
        return None
        raise ValueError(k)
    pivot_value = xs[rng.randrange(left, right)]
    m1, m2 = partition(xs, bounds, pivot_value)
    if m1 <= k < m2:
        return pivot_value
    if k < m1:
        return _quicksort_partial(xs, (left, m1), k, rng)
    return _quicksort_partial(xs, (m2, right), k, rng)


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


class StaticHashTable:
    def __init__(self, bucket_count):
        self._element_count = 0
        self._bucket_count = bucket_count
        self._buckets = [LinkedList() for _ in range(self._bucket_count)]

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


class TreapNode:
    def __init__(self, k, v, w, left, right):
        self.k = k
        self.v = v
        self.w = w
        self.left = left
        self.right = right


class Treap:
    def __init__(self):
        self._root = None
        self._len = 0

    def __getitem__(self, k):
        ok, n = self._get_node(k)
        if not ok:
            raise KeyError(k)
        return n.v

    def __setitem__(self, k, v):
        ok, n = self._get_node(k)
        if ok:
            n.v = v
            return
        w = random.random()
        e = self._get_root_edge()
        while (c := self._get_child(e)) is not None and c.w < w:
            e = self._get_corresponding_edge(c, k)
            if e is None:
                raise RuntimeError("impossible")
        left, right = self._split(c, k)
        self._set_child(e, TreapNode(k, v, w, left, right))
        self._len += 1

    def __delitem__(self, k):
        ok, edge = self._get_edge(k)
        if not ok:
            raise KeyError(k)
        node = self._get_child(edge)
        new_subtree = self._merge(node.left, node.right)
        self._set_child(edge, new_subtree)
        self._len -= 1

    def __len__(self):
        return self._len

    def items(self):
        return ((node.k, node.v) for node in self._traverse(self._root))

    def _get_node(self, k):
        ok, e = self._get_edge(k)
        if not ok:
            return False, None
        return True, self._get_child(e)

    def _get_edge(self, k):
        e = self._get_root_edge()
        while (c := self._get_child(e)) is not None:
            new_e = self._get_corresponding_edge(c, k)
            if new_e is None:
                return True, e
            e = new_e
        return False, None

    def _traverse(self, node):
        if node is None:
            return
        yield from self._traverse(node.left)
        yield node
        yield from self._traverse(node.right)

    def _split(self, node, k):
        if node is None:
            return None, None
        if node.k == k:
            raise ValueError("key is already present")
        if node.k > k:
            left, mid = self._split(node.left, k)
            node.left = mid
            right = node
        else:
            mid, right = self._split(node.right, k)
            node.right = mid
            left = node
        return left, right

    def _merge(self, left, right):
        if left is None:
            return right
        if right is None:
            return left
        if left.w <= right.w:
            result = left
            left.right = self._merge(left.right, right)
        else:
            result = right
            right.left = self._merge(left, right.left)
        return result

    def _get_root_edge(self):
        return (None, None)

    def _get_corresponding_edge(self, node, k):
        if node.k > k:
            return self._get_left_edge(node)
        if node.k < k:
            return self._get_right_edge(node)
        return None

    def _get_left_edge(self, node):
        return (node, "left")

    def _get_right_edge(self, node):
        return (node, "right")

    def _get_child(self, edge):
        par, which = edge
        if par is None:
            return self._root
        if which == "left":
            return par.left
        if which == "right":
            return par.right
        raise RuntimeError("incorrect")

    def _set_child(self, edge, node):
        par, which = edge
        if par is None:
            self._root = node
            return
        if which == "left":
            par.left = node
            return
        if which == "right":
            par.right = node
            return
        raise RuntimeError("incorrect")


def first_matching(pred, left, right):
    # invariant: left <= result <= right
    while left < right:
        mid = (left + right) // 2
        # left <= mid < right
        if pred(mid):
            right = mid  # decreases right
        else:
            left = mid + 1  # increases left
    # left == right
    return left


def lower_bound(value, xs):
    return first_matching(lambda i: xs[i] >= value, 0, len(xs))


def upper_bound(value, xs):
    return first_matching(lambda i: xs[i] > value, 0, len(xs))


def equal_range(value, xs):
    return lower_bound(value, xs), upper_bound(value, xs)


class Monoid:
    def mul(self, n, v):
        result = self.unit
        for _ in range(n):
            result = self.op(result, v)
        return result


addition = Monoid()
addition.unit = 0
addition.op = lambda x, y: x + y


class BinaryTree:
    def __init__(self, size, init_value):
        act_size = 1
        while act_size < size:
            act_size *= 2
        self._act_size = act_size
        self._data = [init_value for _ in range(2 * act_size)]

    def __getitem__(self, k):
        return self._data[k]

    def __setitem__(self, k, v):
        self._data[k] = v

    def leaf_key(self, leaf_index):
        return self._act_size - 1 + leaf_index

    def path_to(self, k):
        p = [k]
        while k != 0:
            k = (k - 1) // 2
            p.append(k)

        return reversed(p)

    def nodes_covering_leaf_range(self, leafrng):
        a, b = leafrng
        if a >= b:
            return ()
        return self._nclr(leafrng, 0)

    def _nclr(self, leafrng, node):
        comm = self._common_cnt(node, leafrng)
        sz = self._node_leaf_cnt(node)
        if comm == 0:
            return
        if comm == sz:
            yield True, comm, node
            return
        yield False, comm, node
        for child in self._children(node):
            yield from self._nclr(leafrng, child)

    def _common_cnt(self, node, leafrng):
        return self._ranges_common_cnt(self._node_range(node), leafrng)

    def _node_leaf_cnt(self, node):
        a, b = self._node_range(node)
        return b - a

    def _ranges_common_cnt(self, a, b):
        ax, ay = a
        bx, by = b
        return max(0, min(ay, by) - max(ax, bx))

    def _children(self, node):
        return (2 * node + 1, 2 * node + 2)

    def _node_range(self, node):
        if node >= self._act_size - 1:
            leaf = node - self._act_size + 1
            return (leaf, leaf + 1)
        l, r = self._children(node)
        x, _ = self._node_range(l)
        _, y = self._node_range(r)
        return x, y


# This version doesn't propagate the lazy updates,
# so the retrieval might be O((log(n))**2). Not sure though.
class SegmentTree:
    def __init__(self, size, monoid=addition):
        self._monoid = monoid
        self._data = BinaryTree(size, (monoid.unit, monoid.unit))

    def op(self, k, v):
        monoid = self._monoid
        data = self._data

        for isleaf, sz, node in self._data.nodes_covering_leaf_range(k):
            own, prop = data[node]
            sum_v = monoid.mul(sz, v)
            if not isleaf:
                own = monoid.op(own, sum_v)
            else:
                prop = monoid.op(prop, v)
            data[node] = own, prop

    def __getitem__(self, k):
        monoid = self._monoid
        data = self._data

        result = monoid.unit
        for isleaf, sz, node in self._data.nodes_covering_leaf_range(k):
            own, prop = data[node]
            result = monoid.op(result, monoid.mul(sz, prop))
            if not isleaf:
                continue
            result = monoid.op(result, own)
        return result


def online_median():
    om = _online_median()
    next(om)
    return om


def _online_median():
    lower_half = Heap([], key=lambda x: -x)
    upper_half = Heap([])

    median = 0
    while True:
        # lengths differ not more than by one
        x = yield median
        if x >= median:
            upper_half.push(x)
        else:
            lower_half.push(x)
        diff = len(upper_half) - len(lower_half)
        if diff > 1:
            lower_half.push(upper_half.pop())
            diff = 0
        elif diff < -1:
            upper_half.push(lower_half.pop())
            diff = 0
        if diff >= 0:
            median = upper_half.top()
        else:
            median = lower_half.top()


def endswith(s1, s2):
    if not s2:
        return True
    return s1[-len(s2):] == s2


def _matching_automaton(pattern):
    aut = {}
    prefixes = [tuple(pattern[:i]) for i in range(len(pattern) + 1)]
    for b in prefixes[1:]:
        binit, blast = b[:-1], b[-1]
        for a in prefixes:
            if endswith(a, binit):
                aut[len(a), blast] = len(b)
    return aut


class Matcher:
    def __init__(self, seq):
        seq = tuple(seq)

        self._seq = seq
        self._aut = _matching_automaton(seq)

    def processor(self):
        matching_state = len(self._seq)
        aut = self._aut

        state = 0
        while True:
            x = yield (matching_state if state == matching_state else None)
            state = aut.get((state, x), 0)


def search(matcher, text):
    p = matcher.processor()
    p.send(None)
    for i, x in enumerate(text):
        res = p.send(x)
        if res is not None:
            yield i - res + 1


def convex_hull(points):
    points = [(point, i) for i, point in enumerate(points)]
    points = sorted(points)
    higher_arc = _convex_arc(points)
    lower_arc = _convex_arc([((-x, -y), i) for ((x, y), i) in reversed(points)])
    return higher_arc[:-1] + lower_arc[:-1]


def _convex_arc(points):
    res = [0, 1]
    for i in range(2, len(points)):
        while len(res) > 1 and not _is_above(
            points[res[-1]], points[res[-2]], points[i]
        ):
            res.pop()
        res.append(i)
    return [points[i][1] for i in res]


def _is_above(p, a, b):
    (px, py), _ = p
    (ax, ay), _ = a
    (bx, by), _ = b
    if px - ax == 0:
        return True
    if bx - ax == 0:
        raise RuntimeError("oops")
    return ((py - ay) / (px - ax)) > ((by - ay) / (bx - ax))


def line_intersection(a, b):
    t, x = _mutual_position(a, b)
    if t is COINCIDE or t is PARALLEL:
        return t, None
    alpha, _ = x
    ap, av = a
    return INTERSECT, point_add(ap, vector_mul(alpha, av))


def segment_intersection(a, b):
    t, x = _mutual_position(a, b)
    if t is PARALLEL:
        return EMPTY, None
    if t is COINCIDE:
        ap, av = a
        bp, bv = b
        d = point_diff(bp, ap)
        p = vector_dot(d, av) / vector_dot(av, av)
        r = vector_dot(vector_add(d, bv), av) / vector_dot(av, av)
        if r < p:
            p, r = r, p
        if common_length((0, 1), (p, r)) > 0:
            return OVERLAP, None
        return EMPTY, None
    alpha, beta = x
    if not (0 <= alpha <= 1) or not (0 <= beta <= 1):
        return EMPTY, None
    ap, av = a
    return INTERSECT, point_add(ap, vector_mul(alpha, av))


def common_length(a, b):
    ab, ae = a
    bb, be = b
    return max(0, min(ae, be) - max(ab, bb))


def _mutual_position(a, b):
    ap, av = a
    bp, bv = b
    d = point_diff(bp, ap)
    if vector_collinear(av, bv):
        if vector_collinear(av, d):
            return COINCIDE, None
        return PARALLEL, None
    alpha, beta = coeffs_in_basis(av, vector_mul(-1, bv), d)
    return INTERSECT, (alpha, beta)


COINCIDE = object()
PARALLEL = object()
EMPTY = object()
INTERSECT = object()
OVERLAP = object()


def point_diff(a, b):
    ax, ay = a
    bx, by = b
    return (ax - bx, ay - by)


def vector_collinear(a, b):
    return det(a, b) == 0


def coeffs_in_basis(b1, b2, v):
    d = det(b1, b2)
    return det(v, b2) / d, det(b1, v) / d


def vector_mul(k, v):
    x, y = v
    return (k * x, k * y)


def vector_dot(a, b):
    ax, ay = a
    bx, by = b
    return ax * bx + ay * by


def vector_add(a, b):
    ax, ay = a
    bx, by = b
    return (ax + bx, ay + by)


def point_add(p, v):
    px, py = p
    vx, vy = v
    return (px + vx, py + vy)


def det(a, b):
    ax, ay = a
    bx, by = b
    return ax * by - ay * bx


def area(xs):
    b = xs[0]
    res = 0
    for i in range(len(xs)):
        res += det(point_diff(xs[i - 1], b), point_diff(xs[i], b))
    return res / 2


class WindingCounter:
    def __init__(self, pt, first_v):
        self._pt = pt
        self._first_v = first_v
        self._v = first_v
        self._angle = 0

    def send(self, v):
        self._angle += angle(point_diff(self._v, self._pt), point_diff(v, self._pt))
        self._v = v

    def close_curve(self):
        self.send(self._first_v)
        return round(self._angle / math.tau)


def angle(v1, v2):
    v1rot = rotate_90_counter(v1)
    cos, sin = coeffs_in_basis(v1, v1rot, v2)
    return math.atan2(sin, cos)


def rotate_90_counter(v):
    x, y = v
    return (y, -x)


def point_inside_polygon(poly, pt, winding_counter=WindingCounter):
    pit = iter(poly)
    w = winding_counter(pt, next(pit))
    for vert in pit:
        w.send(vert)
    num = w.close_curve()
    return num != 0


def closest_pair(pts):
    pts = sorted((pt, i) for i, pt in enumerate(pts))
    return _closest_pair(pts, 0, len(pts))


def _closest_pair(pts, left, right):
    if right - left < 2:
        return None, None, None
    median = (left + right + 1) // 2
    la, lb, ld = _closest_pair(pts, left, median)
    ra, rb, rd = _closest_pair(pts, median, right)
    if ld is None or (rd is not None and rd < ld):
        a, b, d = ra, rb, rd
    else:
        a, b, d = la, lb, ld
    s = _stripe(pts, left, right, median, d)
    # TODO: можно обойтись без копирования и сортировки,
    # "встроив" пересортировку по y слиянием (см. e-maxx).
    s = sorted(s, key=lambda pt: pt[0][1])

    sa, sb, sd = _closest_pair_in_stripe(s, d)
    if sd is None or (d is not None and d < sd):
        return a, b, d
    return sa, sb, sd


def _closest_pair_in_stripe(s, max_d):
    max_flg = max_d is not None
    sa, sb, sd = None, None, None
    for i, (pt_i, id_i) in enumerate(s):
        for j in range(i + 1, len(s)):
            pt_j, id_j = s[j]
            if max_flg and (pt_j[1] - pt_i[1]) ** 2 >= max_d:
                break
            d_ij = point_distance_square(pt_i, pt_j)
            if sd is None or d_ij < sd:
                sa, sb, sd = id_i, id_j, d_ij
    return sa, sb, sd


def point_distance_square(a, b):
    ax, ay = a
    bx, by = b
    return (ax - bx) ** 2 + (ay - by) ** 2


def _stripe(pts, left, right, mid, d):
    if d is None:
        return pts[left:right]

    result = []
    (x_mid, _), _ = pts[mid]
    for i in range(left, right):
        (x_i, _), _ = pts[i]
        if abs(x_i - x_mid) ** 2 < d:
            result.append(pts[i])

    return result


class SillySortedDict:
    def __init__(self):
        self._d = {}

    def __setitem__(self, k, v):
        self._d[k] = v

    def lower_bound(self, min_k, default):
        return min([(k, v) for k, v in self._d.items() if k >= min_k], default=default)

    def upper_bound(self, max_k, default):
        return max([(k, v) for k, v in self._d.items() if k <= max_k], default=default)

    def remove(self, the_v):
        for k, v in self._d.items():
            if v == the_v:
                del self._d[k]
                return


BEGIN = -1
END = -2


class SillyTree:
    def __init__(self):
        self._t = [(-1, None), (-2, None)]
        self._next_idx = 0

    def lower_bound(self, f):
        for node, val in self._t:
            if node != -1 and (node == -2 or f(val)):
                return node
        raise RuntimeError("!!!")

    def pred(self, node):
        r = reversed(self._t)
        for n, _ in r:
            if n == node:
                break
        else:
            raise RuntimeError("!!!")
        node, _ = next(r)
        return node

    def succ(self, node):
        r = iter(self._t)
        for n, _ in r:
            if n == node:
                break
        else:
            raise RuntimeError("!!!")
        node, _ = next(r)
        return node

    def remove(self, node):
        for array_idx, (n, _) in enumerate(self._t):
            if n == node:
                break
        else:
            raise RuntimeError("!!!")
        del self._t[array_idx]

    def has_value(self, node):
        return node >= 0

    def get_value(self, node):
        for n, val in self._t[1:-1]:
            if n == node:
                return val
        raise RuntimeError("!!!")

    def insert_before(self, node, value):
        idx = self._next_idx
        self._next_idx += 1
        for array_idx, (n, _) in enumerate(self._t):
            if n == node:
                break
        else:
            raise RuntimeError("!!!")
        self._t.insert(array_idx, (idx, value))
        return idx


def find_intersecting_figures(figs, d_cls=SillyTree):
    start_pt = {i: fig.minimal_point_for_x() for i, fig in enumerate(figs)}
    end_pt = {i: fig.maximal_point_for_x() for i, fig in enumerate(figs)}

    events = [(start_pt[i][0], -1, i) for i in range(len(figs))] + [
        (end_pt[i][0], +1, i) for i in range(len(figs))
    ]

    events = sorted(events)
    # print(events)
    d = d_cls()
    figs_to_d = {}
    for x, kind, idx in events:
        if kind == -1:  # start
            keyfn = lambda idx2: (figs[idx2].some_point_at_x(x)[1] >= start_pt[idx][1])
            upper_neighbour = d.lower_bound(keyfn)
            if d.has_value(upper_neighbour):
                upper_idx = d.get_value(upper_neighbour)
                if figs[idx].intersects(figs[upper_idx]):
                    return idx, upper_idx
            if d.has_value(d.pred(upper_neighbour)):
                lower_idx = d.get_value(d.pred(upper_neighbour))
                if figs[idx].intersects(figs[lower_idx]):
                    return idx, lower_idx
            node = d.insert_before(upper_neighbour, idx)
            figs_to_d[idx] = node
        else:
            node = figs_to_d[idx]
            if d.has_value(d.pred(node)) and d.has_value(d.succ(node)):
                prev_idx = d.get_value(d.pred(node))
                next_idx = d.get_value(d.succ(node))
                if figs[prev_idx].intersects(figs[next_idx]):
                    return prev_idx, next_idx
            d.remove(node)
            del figs_to_d[idx]

    return None


class Segment:
    def __init__(self, p, v):
        self._p = p
        self._v = v

    def __repr__(self):
        return f"Segment({self._p}, {self._v})"

    def intersects(self, fig):
        return intersects(self, fig)

    def minimal_point_for_x(self):
        px, _ = p = self._p
        qx, _ = q = point_add(p, self._v)
        if px > qx:
            return q
        return p

    def maximal_point_for_x(self):
        px, _ = p = self._p
        qx, _ = q = point_add(p, self._v)
        if px > qx:
            return p
        return q

    def some_point_at_x(self, x):
        t, p = line_intersection((self._p, self._v), ((x, 0), (0, 1)))
        if t != INTERSECT:
            raise RuntimeError("!!!")
        return p


class DoubleDispatch:
    def __init__(self, fn):
        self._default = fn
        self._overloads = {}

    def register(self, ta, tb):
        def wrap(fn):
            self._overloads[ta, tb] = fn
            return fn

        return wrap

    def __call__(self, a, b):
        fn = self._overloads[type(a), type(b)]
        return fn(a, b)


@DoubleDispatch
def intersects(a, b):
    raise NotImplementedError("Not implemented")


@intersects.register(Segment, Segment)
def _(a, b):
    t, _ = segment_intersection((a._p, a._v), (b._p, b._v))
    return t == INTERSECT or t == OVERLAP
