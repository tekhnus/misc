#include <algorithm>
#include <iostream>

#include "utils/apply_to_each.hpp"
#include "utils/misc.hpp"

#include "evaluation.hpp"

template <typename V, size_t ROWS, size_t COLS> struct DenseMatrix {
  using Value = V;
  constexpr static size_t const rows = ROWS;
  constexpr static size_t const cols = COLS;

  Value data[ROWS][COLS];

  Value const &at(size_t i, size_t k) const { return data[i][k]; }

  constexpr static unsigned int AccessCost() { return 1; }
};

template <typename Left, typename Right> struct LazyProductMatrix {
  using Value = typename Left::Value;
  constexpr static size_t const rows = Left::rows;
  constexpr static size_t const cols = Right::cols;

  const Left &left;
  const Right &right;

  LazyProductMatrix(const Left &left, const Right &right)
      : left{left}, right{right} {}

  Value at(size_t i, size_t k) const {
    Value value = 0;
    for (size_t j = 0; j < Left::cols; ++j) {
      value += left.at(i, j) * right.at(j, k);
    }
    return value;
  }

  constexpr static unsigned int AccessCost() {
    return Left::cols * Right::rows * Left::AccessCost() * Right::AccessCost();
  }
};

template <typename Left, typename Right>
DenseMatrix<typename Left::Value, Left::rows, Right::cols>
ComputeProduct(const Left &left, const Right &right) {
  DenseMatrix<typename Left::Value, Left::rows, Right::cols> product;
  for (size_t i = 0; i < Left::rows; ++i) {
    for (size_t j = 0; j < Left::cols; ++j) {
      for (size_t k = 0; k < Right::cols; ++k) {
        product.data[i][k] += left.at(i, j) * right.at(j, k);
      }
    }
  }
  return product;
}

template <typename T> constexpr bool IsDenseMatrix(const T &x) {
  using x_type = typename std::remove_reference<decltype(x)>::type;
  using target_type = typename x_type::Result;
  return std::is_same<target_type, DenseMatrix<int, 5, 5>>::value;
}

template <typename Left, typename Right> struct LazyProductStep {
  using Result = LazyProductMatrix<Left, Right>;

  constexpr unsigned int EvaluationCost() const { return 0; }

  Result Eval(const std::tuple<Left, Right> &arguments) const {
    return LazyProductMatrix{std::get<0>(arguments), std::get<1>(arguments)};
  }
};

template <typename Left, typename Right> struct ComputeProductStep {
  using Result = DenseMatrix<typename Left::Value, Left::rows, Right::cols>;

  constexpr unsigned int EvaluationCost() const {
    return Left::rows * Right::rows * Left::cols * Right::cols *
           Left::AccessCost() * Right::AccessCost();
  }

  Result Eval(const std::tuple<Left, Right> &arguments) const {
    return ComputeProduct(std::get<0>(arguments), std::get<1>(arguments));
  }
};

struct MatrixProductOperation {
  template <typename Arguments>
  constexpr static auto const GetAllPossibleSteps() {
    using Left = typename std::tuple_element<0, Arguments>::type;
    using Right = typename std::tuple_element<1, Arguments>::type;
    return std::tuple{LazyProductStep<Left, Right>{},
                      ComputeProductStep<Left, Right>{}};
  }
};

template <typename Arguments_> struct MatrixProductExpression {
  using Arguments = Arguments_;
  using Operation = MatrixProductOperation;
  const Arguments arguments;
};

template <typename Left, typename Right>
MatrixProductExpression<std::tuple<Left, Right>> operator*(const Left &left,
                                                           const Right &right) {
  return {{left, right}};
}

int main() {
  DenseMatrix<int, 5, 5> a{}, b{}, c{};
  const auto some_expression = (a * b) * c;
  constexpr const auto all_evaluation_trees =
      Evaluator<decltype(some_expression)>::GetAllPossibleEvaluationTrees();
  const auto evaluation_trees = tuple_filter(
      all_evaluation_trees, [](const auto &x) { return IsDenseMatrix(x); });
  constexpr const auto scores =
      apply_to_each(evaluation_trees, [](const auto &tree) {
        return SumOverEvaluationSteps(
            [](const auto &step) { return step.EvaluationCost(); }, tree);
      });
  constexpr const size_t best_index = tuple_argmin(scores);
  constexpr const auto best_score = std::get<best_index>(scores);
  constexpr const auto best_evaluation_tree = std::get<best_index>(evaluation_trees);
  const auto result = best_evaluation_tree.Eval(some_expression);
  std::cout << "result type: " << typeid(result).name() << std::endl;

  std::cout << "best score: " << best_score << std::endl;
  std::cout << "best tree: " << typeid(best_evaluation_tree).name() << std::endl;
  apply_to_each(all_evaluation_trees, [](const auto &x) {
    using x_type = typename std::remove_reference<decltype(x)>::type;
    using target_type = typename x_type::Result;
    unsigned int cost = SumOverEvaluationSteps(
        [](const auto &y) { return y.EvaluationCost(); }, x);
    std::cout << "tree: " << typeid(target_type).name() << " score: " << cost << std::endl;
    return 0;
  });
  return 0;
}
