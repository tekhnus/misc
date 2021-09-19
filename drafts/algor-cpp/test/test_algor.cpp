#include <algor-cpp/indexed_heap.cpp>
#include <iostream>
#include <iterator>
#include <map>
#include <sstream>
#include <variant>

vector<int> vs{1, 2, 3, 4};
vector<pair<string, pair<int, int>>> es{
    {"a", {1, 2}},
    {"b", {1, 3}},
    {"c", {2, 3}},
    {"d", {3, 4}},
};
Graph<int, string> g;
map<string, int> w = {
    {"a", 3},
    {"b", 1},
    {"c", 7},
    {"d", 5},
};
map<pair<int, int>, tuple<int, optional<string>>> exp_dist = {
    {{1, 1}, {0, {}}},   {{1, 2}, {3, "a"}}, {{1, 3}, {1, "b"}},
    {{1, 4}, {6, "d"}},  {{2, 2}, {0, {}}},  {{2, 3}, {7, "c"}},
    {{2, 4}, {12, "d"}}, {{3, 3}, {0, {}}},  {{3, 4}, {5, "d"}},
    {{4, 4}, {0, {}}},
};

template <typename T, typename dummy = void>
void assert_equal(T const &a, T const &b) {
  if (a != b) {
    stringstream s;
    s << &a << " != " << &b;
    throw runtime_error(s.str());
  }
}

// template <typename T,
//           typename std::enable_if_t<std::is_same_v<
//               decltype(std::cout << std::declval<T>()), std::ostream &>>>
// void assert_equal(T const &a, T const &b) {
//   if (a == b) {
//     stringstream s;
//     s << a << " != " << b;
//     throw runtime_error(s.str());
//   }
// }

void test_dfs() {
  auto d = dfs(vs.begin(), vs.begin() + 1, g);
  vector<tuple<DFSEvent, int, string>> res;
  generate_n(back_inserter(res), 9, ref(d));
  for (auto [ev, v, eid] : res) {
    cout << static_cast<std::underlying_type<DFSEvent>::type>(ev) << " " << eid
         << " " << v << endl;
  }
  cout << "----------" << endl;
}

void test_topo_sort() {
  vector<int> res;
  topo_sort(back_inserter(res), vs.begin(), vs.end(), g);
  for (auto &v : res) {
    cout << v << endl;
  }
  cout << "----------" << endl;
}

void test_strong_components() {
  map<int, int> res;
  strong_components(inserter(res, res.end()), vs.begin(), vs.end(), g);
  for (auto &[c, v] : res) {
    cout << c << " " << v << endl;
  }
  cout << "----------" << endl;
}

string val_of(string *s) {
  if (s == nullptr) {
    return "(null string)";
  }
  return *s;
}

void test_ford_bellman() {
  map<int, int> best;
  map<int, variant<string, Root, Unreachable>> pred;
  vector<int> start{1};
  ford_bellman(best, pred, start.begin(), start.end(), g, w);
  for (auto &v : vs) {
    cout << v << " " << best[v] << " " << val_of(get_if<string>(&pred[v]))
         << endl;
  }
  cout << "----------" << endl;
}

void test_dijkstra() {
  auto d = dijkstra(vs.begin(), vs.begin() + 1, g, w);
  vector<optional<pair<int, pair<int, string>>>> res;
  generate_n(back_inserter(res), 5, ref(d));
  for (auto item : res) {
    if (!item.has_value()) {
      continue;
    }
    auto [v, we] = item.value();
    auto [w, e] = we;
    cout << v << " " << w << " " << e << endl;
  }
  cout << "----------" << endl;
}

void test_pairwise_distances() {
  vector<int> start{1};
  auto d = pairwise_distances(g, w);
  for (auto &v : vs) {
    for (auto &u : vs) {
      if (d[{v, u}].has_value()) {
        assert_equal(d[{v, u}].value(), get<0>(exp_dist[{v, u}]));
      } else {
        assert_equal(exp_dist.find({v, u}), exp_dist.end());
      }
    }
  }
  cout << "----------" << endl;
}

void test_floyd_warshall() {
  vector<int> start{1};
  auto [d, pred] = floyd_warshall(g, w);
  for (auto &v : vs) {
    for (auto &u : vs) {
      if (exp_dist.find({v, u}) != exp_dist.end()) {
        assert_equal(d[{v, u}].value(), get<0>(exp_dist[{v, u}]));
        if (get<1>(exp_dist[{v, u}]).has_value()) {
          assert_equal({get<1>(pred[{v, u}])}, get<1>(exp_dist[{v, u}]));
        }
      } else {
        assert_equal(exp_dist.find({v, u}), exp_dist.end());
      }
    }
  }
  cout << "----------" << endl;
}

int main() {
  g.vs_extend(vs.begin(), vs.end());
  g.es_extend(es.begin(), es.end());

  test_dfs();
  test_topo_sort();
  test_strong_components();
  test_ford_bellman();
  test_dijkstra();
  test_pairwise_distances();
  test_floyd_warshall();

  return EXIT_SUCCESS;
}
