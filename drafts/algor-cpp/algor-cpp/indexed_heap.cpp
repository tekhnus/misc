#include <algorithm>
#include <iostream>
#include <iterator>
#include <list>
#include <map>
#include <queue>
#include <stack>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

using namespace std;

template <typename A, typename B> A fst(tuple<A, B> t) { return get<0>(t); }

template <typename CompareFirst, typename... T> class compare_first {
public:
  bool operator()(const tuple<T...> &a, const tuple<T...> &b) {
    return cmp(get<0>(a), get<0>(b));
  }

private:
  CompareFirst cmp;
};

template <typename K, typename V, typename Compare> class indexed_heap {
public:
  indexed_heap() : q{}, s{} {}

  template <typename I> indexed_heap(I begin, I end) : q{begin, end}, s{} {
    transform(begin, end, inserter(s, s.end()), fst<V, K>);
  }

  bool empty() { return s.empty(); }

  tuple<K, V> pop() {
    for (;;) {
      auto [v, k] = q.top();
      q.pop();
      auto it = s.find(k);
      if (it != s.end()) {
        s.erase(it);
        return {k, v};
      }
    }
  }

  void push_or_update(K key, V val) {
    q.push({val, key});
    s.insert(key);
  }

private:
  priority_queue<tuple<V, K>, vector<tuple<V, K>>, compare_first<Compare, V, K>>
      q;
  unordered_set<K> s;
};

template <typename V, typename E> class Graph {
public:
  typedef typename unordered_set<V>::const_iterator v_iterator;
  typedef typename unordered_map<E, pair<V, V>>::const_iterator e_iterator;
  typedef typename list<tuple<V, E>>::const_reverse_iterator
      reverse_local_v_iterator;

  template <typename VI> void vs_extend(VI begin, VI end) {
    vs.insert(begin, end);
    for (auto it = begin; it != end; ++it) {
      ix[*it]; // instantiates default
    }
  }

  template <typename EI> void es_extend(EI begin, EI end) {
    for (EI it = begin; it != end; ++it) {

      auto [eid, uv] = *it;
      auto [u, v] = uv;
      es[eid] = {u, v};
      ix[u].push_back({v, eid});
    }
  }

  v_iterator vertices_begin() const { return vs.cbegin(); }

  v_iterator vertices_end() const { return vs.cend(); }

  e_iterator edges_begin() const { return es.cbegin(); }

  e_iterator edges_end() const { return es.cend(); }

  size_t vertices_size() const {
    return distance(vertices_begin(), vertices_end());
  }

  reverse_local_v_iterator vertices_rbegin(V u) const {
    return ix.at(u).crbegin();
  }

  reverse_local_v_iterator vertices_rend(V u) const { return ix.at(u).crend(); }

private:
  unordered_set<V> vs;
  unordered_map<E, pair<V, V>> es;
  unordered_map<V, list<tuple<V, E>>> ix;
};

template <typename V, typename E>
Graph<V, E> graph_reverse(Graph<V, E> const &g) {
  Graph<V, E> res;
  res.vs_extend(g.vertices_begin(), g.vertices_end());
  vector<pair<E, pair<V, V>>> es;
  transform(g.edges_begin(), g.edges_end(), back_inserter(es),
            [](auto e) -> pair<E, pair<V, V>> {
              auto [ei, uv] = e;
              auto [u, v] = uv;
              return {ei, {v, u}};
            });
  res.es_extend(es.begin(), es.end());
  return res;
}

enum class DFSEvent {
  END,
  ENTER,
  EXIT,
};

template <typename V, typename E, typename VI>
auto dfs(VI begin, VI end, Graph<V, E> const &g) {
  unordered_set<V> visited;
  stack<tuple<DFSEvent, V, optional<E>>> s;

  return [=]() mutable -> tuple<DFSEvent, V, optional<E>> {
    tuple<DFSEvent, V, optional<E>> item;
    do {
      if (!s.empty()) {
        item = s.top();
        s.pop();
      } else if (begin != end) {
        item = {DFSEvent::ENTER, *begin++, {}};
      } else {
        item = {DFSEvent::END, {}, {}};
      }
    } while (get<DFSEvent>(item) == DFSEvent::ENTER &&
             visited.find(get<V>(item)) != visited.end());
    if (get<DFSEvent>(item) == DFSEvent::ENTER) {
      auto u = get<V>(item);

      visited.insert(u);
      s.push({DFSEvent::EXIT, u, {}});

      for (auto i = g.vertices_rbegin(u); i != g.vertices_rend(u); ++i) {
        auto [v, e] = *i;
        s.push({DFSEvent::ENTER, v, e});
      }
    }
    return item;
  };
}

template <typename V, typename E, typename VI, typename VO>
void topo_sort(VO outp, VI begin, VI end, Graph<V, E> const &g) {
  auto d = dfs(begin, end, g);
  for (;;) {
    auto evt = d();
    if (get<DFSEvent>(evt) == DFSEvent::END) {
      break;
    }
    if (get<DFSEvent>(evt) == DFSEvent::EXIT) {
      *outp++ = get<V>(evt);
    }
  }
}

template <typename V, typename E, typename VI, typename VO>
void strong_components(VO outp, VI begin, VI end, Graph<V, E> const &g) {
  vector<V> vs;
  topo_sort(back_inserter(vs), begin, end, g);
  auto d = dfs(vs.begin(), vs.end(), graph_reverse(g));
  int component = 0;
  for (;;) {
    auto [evt, v, e] = d();
    if (evt == DFSEvent::END) {
      break;
    }
    if (evt == DFSEvent::ENTER) {
      if (!e.has_value()) {
        ++component;
      }
      *outp++ = pair<V, int>{v, component};
    }
  }
}

class Root : public monostate {};
class Unreachable : public monostate {};

template <typename V, typename VI, typename E, typename WS, typename DS,
          typename PS>
void ford_bellman(DS &dist, PS &path, VI begin, VI end, Graph<V, E> const &g,
                  WS const &w) {
  using W = typename WS::mapped_type;

  for (auto i = g.vertices_begin(); i != g.vertices_end(); ++i) {
    path[*i] = Unreachable{};
  }
  for (auto i = begin; i != end; ++i) {
    path[*i] = Root{};
    dist[*i] = 0;
  }

  auto relax = [&](pair<E, pair<V, V>> const &edge) -> bool {
    auto [e, uv] = edge;
    auto [u, t] = uv;

    if (holds_alternative<Unreachable>(path[u])) {
      return false;
    }
    W relaxed = dist[u] + w.at(e);
    if (holds_alternative<Unreachable>(path[t]) || dist[t] > relaxed) {
      dist[t] = relaxed;
      path[t] = e;
      return true;
    }
    return false;
  };

  bool break_ = false;
  for (size_t i = 1; i < g.vertices_size() && !break_; ++i) {
    break_ = true;
    for (auto i = g.edges_begin(); i != g.edges_end(); ++i) {
      if (relax(*i)) {
        break_ = false;
      }
    }
  }
  if (!break_) {
    for (auto i = g.edges_begin(); i != g.edges_end(); ++i) {
      if (relax(*i)) {
        throw runtime_error("graph contains a negative cycle");
      }
    }
  }
}

template <typename V, typename E, typename VI, typename F>
auto greedy_tree(VI begin, VI end, Graph<V, E> const &g, F f) {
  using W = int;

  unordered_set<V> visited{begin, end};
  indexed_heap<V, tuple<W, E>, greater<tuple<W, E>>> q;

  for (auto i = begin; i != end; ++i) {
    for (auto j = g.vertices_rbegin(*i); j != g.vertices_rend(*i); ++j) {
      auto [v2, e] = *j;
      W x = f(e, 0);
      q.push_or_update(v2, {x, e});
    }
  }

  return [=]() mutable -> optional<tuple<V, tuple<W, E>>> {
    if (q.empty()) {
      return {};
    }
    auto nxt = q.pop();
    auto [v, we] = nxt;
    auto [weight, _] = we;
    visited.insert(v);
    for (auto i = g.vertices_rbegin(v); i != g.vertices_rend(v); ++i) {
      auto [v2, e] = *i;
      if (visited.find(v2) != visited.end()) {
        continue;
      }
      W x = f(e, weight);
      q.push_or_update(v2, {x, e});
    }
    return nxt;
  };
}

template <typename V, typename E, typename WS, typename VI>
auto dijkstra(VI begin, VI end, Graph<V, E> const &g, WS const &ws) {
  using W = typename WS::mapped_type;

  auto f = [&](E e, W w) { return w + ws.at(e); };

  return greedy_tree(begin, end, g, f);
}

template <typename R, typename I> class Matrix {
public:
  using T = typename R::value_type;
  using K = typename I::value_type;

  Matrix(I begin, I end) : begin{begin}, end{end}, m{} {
    for (auto i = begin; i != end; ++i) {
      for (auto j = begin; j != end; ++j) {
        m[{*i, *j}] = ring.zero;
      }
    }
  }

  T &operator[](tuple<K, K> const &ij) { return m.at(ij); }

  T const &operator[](tuple<K, K> const &ij) const { return m.at(ij); }

  Matrix<R, I> operator*(Matrix<R, I> const &m) const {
    auto res = Matrix<R, I>{begin, end};
    for (auto i = begin; i != end; ++i) {
      for (auto k = begin; k != end; ++k) {
        auto v = ring.zero;
        for (auto j = begin; j != end; ++j) {
          v = ring.sum(v, ring.prod((*this)[{*i, *j}], m[{*j, *k}]));
        }
        res[{*i, *k}] = v;
      }
    }
    return res;
  }

  bool operator==(Matrix<R, I> const &m) const {
    for (auto i = begin; i != end; ++i) {
      for (auto j = begin; j != end; ++j) {
        if ((*this)[{*i, *j}] != m[{*i, *j}]) {
          return false;
        }
      }
    }
    return true;
  }

private:
  R ring;
  I begin, end;
  map<tuple<K, K>, T> m;
};

template <typename R, typename I> Matrix<R, I> make_matrix(I begin, I end) {
  return {begin, end};
}

template <typename T> class comparison_ring {
public:
  using value_type = optional<T>;

  optional<T> zero;
  optional<T> sum(optional<T> a, optional<T> b) const {
    if (!a.has_value()) {
      return b;
    }
    if (!b.has_value()) {
      return a;
    }
    return min(a.value(), b.value());
  }
  optional<T> prod(optional<T> a, optional<T> b) const {
    if (!a.has_value() || !b.has_value()) {
      return zero;
    }
    return a.value() + b.value();
  }
};

template <typename V, typename E, typename WS>
auto weight_matrix(Graph<V, E> const &g, WS const &ws) {
  using W = typename WS::mapped_type;

  auto m =
      make_matrix<comparison_ring<W>>(g.vertices_begin(), g.vertices_end());
  map<pair<V, V>, pair<V, E>> pred;
  for (auto v = g.vertices_begin(); v != g.vertices_end(); ++v) {
    m[{*v, *v}] = 0;
  }
  for (auto i = g.edges_begin(); i != g.edges_end(); ++i) {
    auto [e, uv] = *i;
    auto [u, v] = uv;
    auto weight = ws.at(e);
    if (!m[{u, v}].has_value() || m[{u, v}].value() > weight) {
      m[{u, v}] = weight;
      pred[{u, v}] = {u, e};
    }
  }

  return make_tuple(m, pred);
}

template <typename T> auto fast_pow(T const &x, int n) {
  if (n == 0) {
    throw runtime_error("not implemented");
  }
  if (n == 1) {
    return x;
  }
  auto mhalfk = fast_pow(x, n / 2);
  auto mlamostk = mhalfk * mhalfk;
  if (n % 2 == 1) {
    mlamostk = mlamostk * x;
  }
  return mlamostk;
}

template <typename V, typename E, typename WS>
auto pairwise_distances(Graph<V, E> const &g, WS const &ws) {
  auto [m, _] = weight_matrix(g, ws);
  auto d = fast_pow(m, g.vertices_size() - 1);
  auto chk = d * m;
  if (chk != d) {
    throw runtime_error("graph contains a negative cycle");
  }
  return d;
}

template <typename V, typename E, typename WS>
auto floyd_warshall(Graph<V, E> const &g, WS const &ws) {
  auto [m, pred] = weight_matrix(g, ws);
  for (auto v = g.vertices_begin(); v != g.vertices_end(); ++v) {
    for (auto i = g.vertices_begin(); i != g.vertices_end(); ++i) {
      if (!m[{*i, *v}].has_value()) {
        continue;
      }
      for (auto j = g.vertices_begin(); j != g.vertices_end(); ++j) {
        if (!m[{*v, *j}].has_value()) {
          continue;
        }
        auto through_v = m[{*i, *v}].value() + m[{*v, *j}].value();
        if (!m[{*i, *j}].has_value() || m[{*i, *j}].value() > through_v) {
          m[{*i, *j}] = through_v;
          pred[{*i, *j}] = pred[{*v, *j}];
        }
      }
    }
  }
  return make_tuple(m, pred);
}
