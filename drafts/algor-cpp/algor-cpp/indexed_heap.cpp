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
  typedef typename list<tuple<E, V>>::reverse_iterator reverse_iterator;

  template <typename VI> void vs_extend(VI begin, VI end) {
    vs.insert(begin, end);
  }

  template <typename EI> void es_extend(EI begin, EI end) {
    for (EI it = begin; it != end; ++it) {
      auto [eid, u, v] = *it;
      es[eid] = {u, v};
      ix[u].push_back({eid, v});
    }
  }

  list<tuple<E, V>> outbound_edges(V u) {
    list<tuple<E, V>> res;
    for (auto &[eid, v] : ix[u]) {
      res.push_back({eid, v});
    }
    return res;
  }

  reverse_iterator outbound_edges_rbegin(V u) { return ix[u].rbegin(); }

  reverse_iterator outbound_edges_rend(V u) { return ix[u].rend(); }

private:
  unordered_set<V> vs;
  unordered_map<E, tuple<V, V>> es;
  unordered_map<V, list<tuple<E, V>>> ix;
};

enum class DFSEvent {
  DFS_END,
  DFS_ENTER,
  DFS_EXIT,
};

template <typename V, typename E, typename VI>
auto dfs(VI begin, VI end, Graph<V, E> &g) {
  unordered_set<V> visited;
  stack<tuple<DFSEvent, V, E>> s;

  auto it = begin;
  return [=]() mutable -> tuple<DFSEvent, V, E> {
    for (; !s.empty() || it != end;) {
      if (s.empty()) {
        s.push({DFSEvent::DFS_ENTER, *it++, {}});
      }
      auto [cmd, u, eid] = s.top();
      s.pop();

      if (cmd == DFSEvent::DFS_EXIT) {
        return {DFSEvent::DFS_EXIT, u, {}};
      }
      if (visited.find(u) != visited.end()) {
        continue;
      }
      visited.insert(u);
      s.push({DFSEvent::DFS_EXIT, u, {}});
      for (auto eit = g.outbound_edges_rbegin(u);
           eit != g.outbound_edges_rend(u); ++eit) {
        auto [eid2, v] = *eit;
        s.push({DFSEvent::DFS_ENTER, v, eid2});
      }
      return {DFSEvent::DFS_ENTER, u, eid};
    }
    return {DFSEvent::DFS_END, {}, {}};
  };
}

/*
template <typename V, typename E>
class DFS {
public:
  template <typename VI>
  DFS(VI begin, VI end, Graph<V, E> g) {}
private:
};
*/

vector<tuple<int, int>> v;
indexed_heap<int, int> h{v.begin(), v.end()};
Graph<int, int> g;
