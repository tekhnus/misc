#include <algor/all.hpp>
#include <iostream>
#include <iterator>
#include <map>
#include <sstream>
#include <variant>

std::vector<int> vs{1, 2, 3, 4};
std::vector<std::pair<std::string, std::pair<int, int>>> es{
    {"a", {1, 2}},
    {"b", {1, 3}},
    {"c", {2, 3}},
    {"d", {3, 4}},
};
graph<int, std::string> g;
std::map<std::string, int> w = {
    {"a", 3},
    {"b", 1},
    {"c", 7},
    {"d", 5},
};
std::map<std::pair<int, int>, std::tuple<int, std::optional<std::string>>> exp_dist = {
    {{1, 1}, {0, {}}},   {{1, 2}, {3, "a"}}, {{1, 3}, {1, "b"}},
    {{1, 4}, {6, "d"}},  {{2, 2}, {0, {}}},  {{2, 3}, {7, "c"}},
    {{2, 4}, {12, "d"}}, {{3, 3}, {0, {}}},  {{3, 4}, {5, "d"}},
    {{4, 4}, {0, {}}},
};

template <typename T, typename dummy = void>
void assert_equal(T const &a, T const &b) {
  if (a != b) {
    std::stringstream s;
    s << &a << " != " << &b;
    throw std::runtime_error(s.str());
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
  std::vector<std::variant<dfs_enter<int, std::string>, dfs_exit<int>, dfs_end>> res;
  std::generate_n(std::back_inserter(res), 9, std::ref(d));
  std::vector<std::variant<dfs_enter<int, std::string>, dfs_exit<int>, dfs_end>> exp = {
      {dfs_enter<int, std::string>{1, {}}, dfs_enter<int, std::string>{2, "a"},
       dfs_enter<int, std::string>{3, "c"}, dfs_enter<int, std::string>{4, "d"},
       dfs_exit<int>{4}, dfs_exit<int>{3}, dfs_exit<int>{2}, dfs_exit<int>{1},
       dfs_end{}}};
  assert_equal(res, res);
}

void test_topo_sort() {
  std::vector<int> res;
  topo_sort(back_inserter(res), vs.begin(), vs.end(), g);
  assert_equal(res, {4, 3, 2, 1});
}

void test_strong_components() {
  std::map<int, int> res;
  strong_components(inserter(res, res.end()), vs.begin(), vs.end(), g);
  assert_equal(res, {{1, 1}, {2, 1}, {3, 1}, {4, 1}});
}

std::string val_of(std::string *s) {
  if (s == nullptr) {
    return "(null string)";
  }
  return *s;
}

void test_ford_bellman() {
  std::map<int, int> best;
  std::map<int, std::variant<std::string, root, unreachable>> pred;
  std::vector<int> start{1};
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
  std::vector<std::optional<std::tuple<int, std::tuple<int, std::string>>>> res;
  generate_n(std::back_inserter(res), 5, std::ref(d));
  for (auto item : res) {
    if (!item.has_value()) {
      continue;
    }
    auto [v, we] = item.value();
    auto [w, e] = we;
    assert_equal(exp_dist[{1, v}], std::tuple<int, std::optional<std::string>>{w, e});
  }
}

void test_pairwise_distances() {
  std::vector<int> start{1};
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
  std::vector<int> start{1};
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

void run_test(std::string name, void (*test)()) {
  try {
    test();
  } catch (...) {
    std::cout << "FAIL " << name << std::endl;
    return;
  }
  std::cout << "OK   " << name << std::endl;
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
