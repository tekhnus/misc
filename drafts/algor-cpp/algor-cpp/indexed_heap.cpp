#include <algorithm>
#include <iostream>
#include <iterator>
#include <list>
#include <queue>
#include <stack>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

using namespace std;

template <typename A, typename B> A fst(tuple<A, B> t) { return get<0>(t); }

template <typename K, typename V> class indexed_heap {
public:
  template <typename I> indexed_heap(I begin, I end) : q{begin, end}, s{} {
    transform(begin, end, inserter(s, s.end()), fst<V, K>);
  }

  bool empty() { return s.empty(); }

  tuple<K, V> pop() {
    for (;;) {
      auto [v, k] = q.pop();
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
  priority_queue<tuple<V, K>> q;
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
  stack<tuple<DFSEvent, V, E>> s;

  return [=]() mutable -> tuple<DFSEvent, V, E> {
    tuple<DFSEvent, V, E> item;
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
      if (e == E{}) {
        ++component;
      }
      *outp++ = pair<V, int>{v, component};
    }
  }
}

template <typename V, typename E, typename WS, typename DS, typename PS>
void ford_bellman(DS &dist, PS &path, V const &v, Graph<V, E> const &g,
                  WS const &w) {
  using W = typename WS::mapped_type;
  V null_v{};

  for (auto it = g.vertices_begin(); it != g.vertices_end(); ++it) {
    path[*it] = null_v;
  }
  path[v] = v;
  dist[v] = 0;

  auto relax = [&](pair<E, pair<V, V>> const &edge) -> bool {
    auto [e, uv] = edge;
    auto [u, v] = uv;

    if (path[u] == null_v) {
      return false;
    }
    W relaxed = dist[u] + w.at(e);
    if (path[v] == null_v || dist[v] > relaxed) {
      dist[v] = relaxed;
      path[v] = u;
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
