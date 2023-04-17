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
graph<int, string> g;
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
  vector<variant<dfs_enter<int, string>, dfs_exit<int>, dfs_end>> res;
  generate_n(back_inserter(res), 9, ref(d));
  vector<variant<dfs_enter<int, string>, dfs_exit<int>, dfs_end>> exp = {
      {dfs_enter<int, string>{1, {}}, dfs_enter<int, string>{2, "a"},
       dfs_enter<int, string>{3, "c"}, dfs_enter<int, string>{4, "d"},
       dfs_exit<int>{4}, dfs_exit<int>{3}, dfs_exit<int>{2}, dfs_exit<int>{1},
       dfs_end{}}};
  assert_equal(res, res);
}

void test_topo_sort() {
  vector<int> res;
  topo_sort(back_inserter(res), vs.begin(), vs.end(), g);
  assert_equal(res, {4, 3, 2, 1});
}

void test_strong_components() {
  map<int, int> res;
  strong_components(inserter(res, res.end()), vs.begin(), vs.end(), g);
  assert_equal(res, {{1, 1}, {2, 1}, {3, 1}, {4, 1}});
}

string val_of(string *s) {
  if (s == nullptr) {
    return "(null string)";
  }
  return *s;
}

void test_ford_bellman() {
  map<int, int> best;
  map<int, variant<string, root, unreachable>> pred;
  vector<int> start{1};
  ford_bellman(best, pred, start.begin(), start.end(), g, w);

  for (auto &u : vs) {
    if (exp_dist.find({1, u}) != exp_dist.end()) {
      assert_equal(best[u], get<0>(exp_dist[{1, u}]));
      if (get<1>(exp_dist[{1, u}]).has_value()) {
        assert_equal(get<0>(pred[u]), get<1>(exp_dist[{1, u}]).value());
      } else {
        assert_equal(holds_alternative<root>(pred[u]), true);
      }
    } else {
      assert_equal(holds_alternative<unreachable>(pred[u]), true);
    }
  }
}

void test_dijkstra() {
  auto d = dijkstra(vs.begin(), vs.begin() + 1, g, w);
  vector<optional<tuple<int, tuple<int, string>>>> res;
  generate_n(std::back_inserter(res), 5, ref(d));
  for (auto item : res) {
    if (!item.has_value()) {
      continue;
    }
    auto [v, we] = item.value();
    auto [w, e] = we;
    assert_equal(exp_dist[{1, v}], tuple<int, optional<string>>{w, e});
  }
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
}

void run_test(string name, void (*test)()) {
  try {
    test();
  } catch (...) {
    cout << "FAIL " << name << endl;
    return;
  }
  cout << "OK   " << name << endl;
}

int main() {
  g.vertices_insert(vs.begin(), vs.end());
  g.edges_insert(es.begin(), es.end());

  run_test("dfs", test_dfs);
  run_test("topo_sort", test_topo_sort);
  run_test("strong_components", test_strong_components);
  run_test("ford_bellman", test_ford_bellman);
  run_test("dijkstra", test_dijkstra);
  run_test("pairwise_distances", test_pairwise_distances);
  run_test("floyd_warshall", test_floyd_warshall);

  return EXIT_SUCCESS;
}
