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
  template <typename VI> void vs_extend(VI begin, VI end) {
    vs.insert(begin, end);
  }

  template <typename EI> void es_extend(EI begin, EI end) {
    for (EI it = begin; it != end; ++it) {
      auto [eid, u, v] = *it;
      es[eid] = {u, v};
      ix[u].push_back(eid);
    }
  }

  list<tuple<E, V>> outbound_edges(V u) {
    list<tuple<E, V>> res;
    for (auto &eid : ix[u]) {
      res.push_back({eid, get<1>(es[eid])});
    }
    return res;
  }

private:
  unordered_set<V> vs;
  unordered_map<E, tuple<V, V>> es;
  unordered_map<V, list<E>> ix;
};

enum class DFSEvent {
  DFS_ENTER,
  DFS_EXIT,
};

template <typename V, typename E, typename VI>
auto dfs(VI begin, VI end, Graph<V, E> &g) {
  unordered_set<V> visited;
  stack<tuple<DFSEvent, E, V>> s;

  for (auto it = begin; it != end; ++it) {
    s.push({DFSEvent::DFS_ENTER, {}, *it});
  }

  return [=]() mutable {
    for (; !s.empty();) {
      auto item = s.top();
      s.pop();
      auto [cmd, eid, u] = item;

      if (cmd == DFSEvent::DFS_EXIT) {
        return tuple<bool, DFSEvent, E, V>{true, DFSEvent::DFS_EXIT, eid, u};
      }
      if (visited.find(u) != visited.end()) {
        continue;
      }
      visited.insert(u);
      s.push({DFSEvent::DFS_EXIT, {}, u});
      auto edges = g.outbound_edges(u);
      for (auto eit = edges.rbegin(); eit != edges.rend(); ++eit) {
        auto [eid2, v] = *eit;
        s.push({DFSEvent::DFS_ENTER, eid2, v});
      }
      return tuple<bool, DFSEvent, E, V>{true, DFSEvent::DFS_ENTER, eid, u};
    }
    return tuple<bool, DFSEvent, E, V>{false, {}, {}, {}};
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
