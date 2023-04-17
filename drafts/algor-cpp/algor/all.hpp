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

template <typename CompareSecond, typename... T> class compare_second {
public:
  bool operator()(const std::tuple<T...> &a, const std::tuple<T...> &b) {
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

  std::tuple<K, V> const &top() {
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
      throw std::runtime_error(
          "readding the removed elements is not supported yet");
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
  std::priority_queue<std::tuple<K, V>, std::vector<std::tuple<K, V>>,
                      compare_second<Compare, K, V>>
      q;
  std::unordered_set<K> s;
};

template <typename V, typename E> class graph {
public:
  typedef typename std::unordered_set<V>::const_iterator vertex_iterator;
  typedef typename std::unordered_map<E, std::pair<V, V>>::const_iterator
      edge_iterator;
  typedef typename std::list<std::tuple<V, E>>::const_reverse_iterator
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
    return std::distance(vertices_cbegin(), vertices_cend());
  }

  reverse_local_vertex_iterator vertices_crbegin(V u) const {
    return successors.at(u).crbegin();
  }

  reverse_local_vertex_iterator vertices_crend(V u) const {
    return successors.at(u).crend();
  }

private:
  std::unordered_set<V> vertices;
  std::unordered_map<E, std::pair<V, V>> edges;
  std::unordered_map<V, std::list<std::tuple<V, E>>> successors;
};

template <typename V, typename E>
std::pair<E, std::pair<V, V>> edge_reverse(std::pair<E, std::pair<V, V>> edge) {
  auto [e, uv] = edge;
  auto [u, v] = uv;
  return {e, {v, u}};
}

template <typename V, typename E>
graph<V, E> graph_reverse(graph<V, E> const &g) {
  graph<V, E> res(g.vertices_cbegin(), g.vertices_cend());
  std::vector<std::pair<E, std::pair<V, V>>> reversed_edges;
  transform(g.edges_cbegin(), g.edges_cend(), back_inserter(reversed_edges),
            edge_reverse<V, E>);
  res.edges_insert(reversed_edges.begin(), reversed_edges.end());
  return res;
}

template <typename V, typename E> struct dfs_enter {
  V v;
  std::optional<E> e;
  bool operator==(dfs_enter const &x) const = default;
};
template <typename V> struct dfs_exit {
  V v;
  bool operator==(dfs_exit const &x) const = default;
};
struct dfs_end : public std::monostate {};

template <typename V, typename E>
using dfs_item = std::variant<dfs_enter<V, E>, dfs_exit<V>, dfs_end>;

template <typename V, typename E, typename I>
auto dfs(I begin, I end, graph<V, E> const &g) {
  std::unordered_set<V> visited;
  std::stack<dfs_item<V, E>> s;

  return [=]() mutable -> dfs_item<V, E> {
    dfs_item<V, E> item;
    do {
      if (!s.empty()) {
        item = s.top();
        s.pop();
      } else if (begin != end) {
        item = dfs_enter<V, E>{*begin++, {}};
      } else {
        item = dfs_end{};
      }
    } while (holds_alternative<dfs_enter<V, E>>(item) &&
             visited.find(get<dfs_enter<V, E>>(item).v) != visited.end());
    if (holds_alternative<dfs_enter<V, E>>(item)) {
      auto u = get<dfs_enter<V, E>>(item).v;

      visited.insert(u);
      s.push(dfs_exit<V>{u});

      for (auto i = g.vertices_crbegin(u); i != g.vertices_crend(u); ++i) {
        auto [v, e] = *i;
        s.push(dfs_enter<V, E>{v, e});
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
    if (holds_alternative<dfs_end>(evt)) {
      break;
    }
    if (holds_alternative<dfs_exit<V>>(evt)) {
      *outp++ = get<dfs_exit<V>>(evt).v;
    }
  }
}

template <typename V, typename E, typename VI, typename VO>
void strong_components(VO outp, VI begin, VI end, graph<V, E> const &g) {
  std::vector<V> vs;
  topo_sort(back_inserter(vs), begin, end, g);
  auto d = dfs(vs.begin(), vs.end(), graph_reverse(g));
  int component = 0;
  for (;;) {
    auto evt = d();
    if (holds_alternative<dfs_end>(evt)) {
      break;
    }
    if (holds_alternative<dfs_enter<V, E>>(evt)) {
      if (!get<dfs_enter<V, E>>(evt).e.has_value()) {
        ++component;
      }
      *outp++ = std::pair<V, int>{get<dfs_enter<V, E>>(evt).v, component};
    }
  }
}

class root : public std::monostate {};
class unreachable : public std::monostate {};

template <typename V, typename VI, typename E, typename WS, typename DS,
          typename PS>
void ford_bellman(DS &dist, PS &path, VI begin, VI end, graph<V, E> const &g,
                  WS const &w) {
  using W = typename WS::mapped_type;

  for (auto i = g.vertices_cbegin(); i != g.vertices_cend(); ++i) {
    path[*i] = unreachable{};
  }
  for (auto i = begin; i != end; ++i) {
    path[*i] = root{};
    dist[*i] = 0;
  }

  auto relax = [&](std::pair<E, std::pair<V, V>> const &edge) -> bool {
    auto [e, uv] = edge;
    auto [u, t] = uv;

    if (holds_alternative<unreachable>(path[u])) {
      return false;
    }
    W relaxed = dist[u] + w.at(e);
    if (holds_alternative<unreachable>(path[t]) || dist[t] > relaxed) {
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
        throw std::runtime_error("graph contains a negative cycle");
      }
    }
  }
}

template <typename V, typename E, typename VI, typename F>
auto greedy_tree(VI begin, VI end, graph<V, E> const &g, F f) {
  using W = int;

  std::unordered_set<V> visited{begin, end};
  keyed_heap<V, std::tuple<W, E>, std::greater<std::tuple<W, E>>> q;

  for (auto i = begin; i != end; ++i) {
    for (auto j = g.vertices_crbegin(*i); j != g.vertices_crend(*i); ++j) {
      auto [v2, e] = *j;
      W x = f(e, 0);
      q.push_or_update(v2, {x, e});
    }
  }

  return [=]() mutable -> std::optional<std::tuple<V, std::tuple<W, E>>> {
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

template <typename R, typename I> class matrix {
public:
  using T = typename R::value_type;
  using K = typename I::value_type;

  matrix(I begin, I end) : begin{begin}, end{end}, m{} {
    for (auto i = begin; i != end; ++i) {
      for (auto j = begin; j != end; ++j) {
        m[{*i, *j}] = ring.zero;
      }
    }
  }

  T &operator[](std::tuple<K, K> const &ij) { return m.at(ij); }

  T const &operator[](std::tuple<K, K> const &ij) const { return m.at(ij); }

  matrix<R, I> operator*(matrix<R, I> const &m) const {
    auto res = matrix<R, I>{begin, end};
    for (auto i = begin; i != end; ++i) {
      for (auto k = begin; k != end; ++k) {
        auto v = ring.zero;
        for (auto j = begin; j != end; ++j) {
          v = ring.sum(v, ring.product((*this)[{*i, *j}], m[{*j, *k}]));
        }
        res[{*i, *k}] = v;
      }
    }
    return res;
  }

  bool operator==(matrix<R, I> const &m) const {
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
  std::map<std::tuple<K, K>, T> m;
};

template <typename R, typename I> matrix<R, I> make_matrix(I begin, I end) {
  return {begin, end};
}

template <typename T> class min_sum_semiring {
public:
  using value_type = std::optional<T>;

  value_type zero;
  value_type sum(value_type a, value_type b) const {
    if (!a.has_value()) {
      return b;
    }
    if (!b.has_value()) {
      return a;
    }
    return std::min(a.value(), b.value());
  }
  value_type product(value_type a, value_type b) const {
    if (!a.has_value() || !b.has_value()) {
      return zero;
    }
    return a.value() + b.value();
  }
};

template <typename E, typename W> class path_semiring {
public:
  using value_type = std::optional<std::pair<W, std::vector<E>>>;

  value_type zero = std::nullopt;
  value_type sum(value_type a, value_type b) const {
    if (!a.has_value()) {
      return b;
    }
    if (!b.has_value()) {
      return a;
    }
    return std::min(a, b);
  }
  value_type product(value_type a, value_type b) const {
    if (!a.has_value() || !b.has_value()) {
      return zero;
    }
    std::vector<E> path{a->second};
    std::copy(std::begin(b->second), std::end(b->second), std::back_inserter(path));
    return {{a->first + b->first, path}};
  }
};

template <typename V, typename E, typename WS>
auto weight_matrix(graph<V, E> const &g, WS const &ws) {
  using W = typename WS::mapped_type;

  auto m =
      make_matrix<min_sum_semiring<W>>(g.vertices_cbegin(), g.vertices_cend());
  std::map<std::pair<V, V>, std::pair<V, E>> pred;
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

template <typename V, typename E, typename WS>
auto graph_to_matrix(graph<V, E> const &g, WS const &ws) {
  using W = typename WS::mapped_type;

  auto m =
    make_matrix<path_semiring<E, W>>(g.vertices_cbegin(), g.vertices_cend());
  for (auto v = g.vertices_cbegin(); v != g.vertices_cend(); ++v) {
    m[{*v, *v}] = std::optional{std::pair{0, std::vector<E>{}}};
  }
  for (auto i = g.edges_cbegin(); i != g.edges_cend(); ++i) {
    auto [e, uv] = *i;
    auto [u, v] = uv;
    auto weight = ws.at(e);
    if (!m[{u, v}].has_value() || m[{u, v}].value().first > weight) {
      m[{u, v}] = std::optional{std::pair{weight, std::vector{e}}};
    }
  }

  return m;
}

template <typename T> auto power(T const &x, int n) {
  if (n == 0) {
    throw std::runtime_error("not implemented");
  }
  if (n == 1) {
    return x;
  }
  auto res = power(x, n / 2);
  res = res * res;
  if (n % 2 == 1) {
    res = res * x;
  }
  return res;
}

template <typename V, typename E, typename WS>
auto path_matrix(graph<V, E> const &g, WS const &ws) {
  auto m = graph_to_matrix(g, ws);
  auto d = power(m, g.vertices_size() - 1);
  auto check = d * m;
  if (check != d) {
    throw std::runtime_error("graph contains a negative cycle");
  }
  return d;
}

template <typename V, typename E, typename WS>
auto floyd_warshall(graph<V, E> const &g, WS const &ws) {
  using W = typename WS::mapped_type;
  auto m = path_matrix(g, ws);
  path_semiring<E, W> r;
  for (auto v = g.vertices_cbegin(); v != g.vertices_cend(); ++v) {
    for (auto i = g.vertices_cbegin(); i != g.vertices_cend(); ++i) {
      if (!m[{*i, *v}].has_value()) {
        continue;
      }
      for (auto j = g.vertices_cbegin(); j != g.vertices_cend(); ++j) {
        if (!m[{*v, *j}].has_value()) {
          continue;
        }
        auto through_v = r.product(m[{*i, *v}], m[{*v, *j}]);
        if (!m[{*i, *j}].has_value() || (through_v.has_value() && m[{*i, *j}].value().first > through_v->first)) {
          m[{*i, *j}] = through_v;
        }
      }
    }
  }
  return m;
}
