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
  typedef typename list<tuple<V, E>>::reverse_iterator reverse_iterator;

  template <typename VI> void vs_extend(VI begin, VI end) {
    vs.insert(begin, end);
  }

  template <typename EI> void es_extend(EI begin, EI end) {
    for (EI it = begin; it != end; ++it) {
      auto [eid, u, v] = *it;
      es[eid] = {u, v};
      ix[u].push_back({v, eid});
    }
  }

  reverse_iterator vertices_rbegin(V u) { return ix[u].rbegin(); }

  reverse_iterator vertices_rend(V u) { return ix[u].rend(); }

private:
  unordered_set<V> vs;
  unordered_map<E, tuple<V, V>> es;
  unordered_map<V, list<tuple<V, E>>> ix;
};

enum class DFSEvent {
  END,
  ENTER,
  EXIT,
};

template <typename V, typename E, typename VI>
auto dfs(VI begin, VI end, Graph<V, E> &g) {
  unordered_set<V> visited;
  stack<tuple<DFSEvent, V, E>> s;

  return [=]() mutable -> tuple<DFSEvent, V, E> {
    tuple<DFSEvent, V, E> item;
    bool cont = true;
    while (cont) {
      if (s.empty() && begin == end) {
        item = tuple<DFSEvent, V, E>{DFSEvent::END};
        cont = false;
      } else if (s.empty()) {
        item = tuple<DFSEvent, V, E>{DFSEvent::ENTER, *begin++, {}};
        cont = false;
      } else {
        item = s.top();
        s.pop();
        cont = get<DFSEvent>(item) == DFSEvent::ENTER &&
               visited.find(get<V>(item)) != visited.end();
      }
    }
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
