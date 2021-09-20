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

template <typename CompareSecond, typename... T> class compare_second {
public:
  bool operator()(const tuple<T...> &a, const tuple<T...> &b) {
    return cmp(get<1>(a), get<1>(b));
  }

private:
  CompareSecond cmp;
};

template <typename K, typename V, typename Compare> class keyed_heap {
public:
  keyed_heap() : q{}, s{} {}

  template <typename I> keyed_heap(I begin, I end) : q{begin, end}, s{} {}

  bool empty() {
    pop_while_removed();
    return q.empty();
  }

  tuple<K, V> const &top() {
    pop_while_removed();
    return q.top();
  }

  void pop() {
    pop_while_removed();
    K const &k = get<0>(q.top());
    s.erase(k);
    q.pop();
  }

  void push_or_update(K const &key, V const &val) {
    if (s.find(key) != s.end()) {
      throw runtime_error("readding the removed elements is not supported yet");
    }
    q.push({key, val});
  }

private:
  void pop_while_removed() {
    for (;;) {
      if (q.empty()) {
        return;
      }
      auto [k, v] = q.top();
      auto it = s.find(k);
      if (it == s.end()) {
        return;
      }
      q.pop();
    }
  }
  priority_queue<tuple<K, V>, vector<tuple<K, V>>,
                 compare_second<Compare, K, V>>
      q;
  unordered_set<K> s;
};

template <typename V, typename E> class graph {
public:
  typedef typename unordered_set<V>::const_iterator vertex_iterator;
  typedef typename unordered_map<E, pair<V, V>>::const_iterator edge_iterator;
  typedef typename list<tuple<V, E>>::const_reverse_iterator
      reverse_local_vertex_iterator;

  graph() = default;

  template <typename I> graph(I begin, I end) { vertices_insert(begin, end); }

  template <typename I> void vertices_insert(I begin, I end) {
    vertices.insert(begin, end);
    for (auto it = begin; it != end; ++it) {
      successors[*it]; // instantiates default
    }
  }

  template <typename I> void edges_insert(I begin, I end) {
    edges.insert(begin, end);
    for (I it = begin; it != end; ++it) {
      auto [e, uv] = *it;
      auto [u, v] = uv;
      successors[u].push_back({v, e});
    }
  }

  vertex_iterator vertices_cbegin() const { return vertices.cbegin(); }

  vertex_iterator vertices_cend() const { return vertices.cend(); }

  edge_iterator edges_cbegin() const { return edges.cbegin(); }

  edge_iterator edges_cend() const { return edges.cend(); }

  size_t vertices_size() const {
    return distance(vertices_cbegin(), vertices_cend());
  }

  reverse_local_vertex_iterator vertices_crbegin(V u) const {
    return successors.at(u).crbegin();
  }

  reverse_local_vertex_iterator vertices_crend(V u) const {
    return successors.at(u).crend();
  }

private:
  unordered_set<V> vertices;
  unordered_map<E, pair<V, V>> edges;
  unordered_map<V, list<tuple<V, E>>> successors;
};

template <typename V, typename E>
pair<E, pair<V, V>> edge_reverse(pair<E, pair<V, V>> edge) {
  auto [e, uv] = edge;
  auto [u, v] = uv;
  return {e, {v, u}};
}

template <typename V, typename E>
graph<V, E> graph_reverse(graph<V, E> const &g) {
  graph<V, E> res(g.vertices_cbegin(), g.vertices_cend());
  vector<pair<E, pair<V, V>>> reversed_edges;
  transform(g.edges_cbegin(), g.edges_cend(), back_inserter(reversed_edges),
            edge_reverse<V, E>);
  res.edges_insert(reversed_edges.begin(), reversed_edges.end());
  return res;
}

enum class DFSEvent {
  END,
  ENTER,
  EXIT,
};

template <typename V, typename E, typename I>
auto dfs(I begin, I end, graph<V, E> const &g) {
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

      for (auto i = g.vertices_crbegin(u); i != g.vertices_crend(u); ++i) {
        auto [v, e] = *i;
        s.push({DFSEvent::ENTER, v, e});
      }
    }
    return item;
  };
}

template <typename V, typename E, typename VI, typename VO>
void topo_sort(VO outp, VI begin, VI end, graph<V, E> const &g) {
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
void strong_components(VO outp, VI begin, VI end, graph<V, E> const &g) {
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
void ford_bellman(DS &dist, PS &path, VI begin, VI end, graph<V, E> const &g,
                  WS const &w) {
  using W = typename WS::mapped_type;

  for (auto i = g.vertices_cbegin(); i != g.vertices_cend(); ++i) {
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
    for (auto i = g.edges_cbegin(); i != g.edges_cend(); ++i) {
      if (relax(*i)) {
        break_ = false;
      }
    }
  }
  if (!break_) {
    for (auto i = g.edges_cbegin(); i != g.edges_cend(); ++i) {
      if (relax(*i)) {
        throw runtime_error("graph contains a negative cycle");
      }
    }
  }
}

template <typename V, typename E, typename VI, typename F>
auto greedy_tree(VI begin, VI end, graph<V, E> const &g, F f) {
  using W = int;

  unordered_set<V> visited{begin, end};
  keyed_heap<V, tuple<W, E>, greater<tuple<W, E>>> q;

  for (auto i = begin; i != end; ++i) {
    for (auto j = g.vertices_crbegin(*i); j != g.vertices_crend(*i); ++j) {
      auto [v2, e] = *j;
      W x = f(e, 0);
      q.push_or_update(v2, {x, e});
    }
  }

  return [=]() mutable -> optional<tuple<V, tuple<W, E>>> {
    if (q.empty()) {
      return {};
    }
    auto nxt = q.top();
    q.pop();
    auto [v, we] = nxt;
    auto [weight, _] = we;
    visited.insert(v);
    for (auto i = g.vertices_crbegin(v); i != g.vertices_crend(v); ++i) {
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
auto dijkstra(VI begin, VI end, graph<V, E> const &g, WS const &ws) {
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
auto weight_matrix(graph<V, E> const &g, WS const &ws) {
  using W = typename WS::mapped_type;

  auto m =
      make_matrix<comparison_ring<W>>(g.vertices_cbegin(), g.vertices_cend());
  map<pair<V, V>, pair<V, E>> pred;
  for (auto v = g.vertices_cbegin(); v != g.vertices_cend(); ++v) {
    m[{*v, *v}] = 0;
  }
  for (auto i = g.edges_cbegin(); i != g.edges_cend(); ++i) {
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
auto pairwise_distances(graph<V, E> const &g, WS const &ws) {
  auto [m, _] = weight_matrix(g, ws);
  auto d = fast_pow(m, g.vertices_size() - 1);
  auto chk = d * m;
  if (chk != d) {
    throw runtime_error("graph contains a negative cycle");
  }
  return d;
}

template <typename V, typename E, typename WS>
auto floyd_warshall(graph<V, E> const &g, WS const &ws) {
  auto [m, pred] = weight_matrix(g, ws);
  for (auto v = g.vertices_cbegin(); v != g.vertices_cend(); ++v) {
    for (auto i = g.vertices_cbegin(); i != g.vertices_cend(); ++i) {
      if (!m[{*i, *v}].has_value()) {
        continue;
      }
      for (auto j = g.vertices_cbegin(); j != g.vertices_cend(); ++j) {
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
