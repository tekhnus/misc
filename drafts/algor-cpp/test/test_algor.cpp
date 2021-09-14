#include <algor-cpp/indexed_heap.cpp>
#include <iostream>

int main() {
  vector<int> vs{1, 2, 3, 4};
  vector<tuple<string, int, int>> es{
      {"a", 1, 2},
      {"b", 1, 3},
      {"c", 2, 3},
      {"d", 3, 4},
  };
  Graph<int, string> g;
  g.vs_extend(vs.begin(), vs.end());
  g.es_extend(es.begin(), es.end());
  auto d = dfs(vs.begin(), vs.begin() + 1, g);
  for (;;) {
    auto [ev, v, eid] = d();
    if (ev == DFSEvent::DFS_END) {
      break;
    }
    cout << static_cast<std::underlying_type<DFSEvent>::type>(ev) << " " << eid
         << " " << v << endl;
  }
  return EXIT_SUCCESS;
}
