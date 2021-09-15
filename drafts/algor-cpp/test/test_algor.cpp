#include <algor-cpp/indexed_heap.cpp>
#include <iostream>
#include <iterator>
#include <map>

void test_dfs() {
  vector<int> vs{1, 2, 3, 4};
  vector<pair<string, pair<int, int>>> es{
      {"a", {1, 2}},
      {"b", {1, 3}},
      {"c", {2, 3}},
      {"d", {3, 4}},
  };
  Graph<int, string> g;
  g.vs_extend(vs.begin(), vs.end());
  g.es_extend(es.begin(), es.end());
  auto d = dfs(vs.begin(), vs.begin() + 1, g);
  for (;;) {
    auto [ev, v, eid] = d();
    if (ev == DFSEvent::END) {
      break;
    }
    cout << static_cast<std::underlying_type<DFSEvent>::type>(ev) << " " << eid
         << " " << v << endl;
  }
  cout << "----------" << endl;
}

void test_topo_sort() {
  vector<int> vs{1, 2, 3, 4};
  vector<pair<string, pair<int, int>>> es{
      {"a", {1, 2}},
      {"b", {1, 3}},
      {"c", {2, 3}},
      {"d", {3, 4}},
  };
  vector<int> res;
  Graph<int, string> g;
  g.vs_extend(vs.begin(), vs.end());
  g.es_extend(es.begin(), es.end());
  topo_sort(back_inserter(res), vs.begin(), vs.end(), g);
  for (auto &v : res) {
    cout << v << endl;
  }
  cout << "----------" << endl;
}


void test_strong_components() {
  vector<int> vs{1, 2, 3, 4};
  vector<pair<string, pair<int, int>>> es{
      {"a", {1, 2}},
      {"b", {1, 3}},
      {"c", {2, 3}},
      {"d", {3, 4}},
  };
  map<int, int> res;
  Graph<int, string> g;
  g.vs_extend(vs.begin(), vs.end());
  g.es_extend(es.begin(), es.end());
  strong_components(inserter(res, res.end()), vs.begin(), vs.end(), g);
  for (auto &[c, v] : res) {
    cout << c << " " << v << endl;
  }
  cout << "----------" << endl;
}

int main() {
  test_dfs();
  test_topo_sort();
  test_strong_components();
  return EXIT_SUCCESS;
}
