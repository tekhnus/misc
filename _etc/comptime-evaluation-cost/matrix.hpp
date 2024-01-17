#pragma once

#include <iostream>

#include "evaluation.hpp"

template <typename V, size_t ROWS, size_t COLS> struct DenseMatrix {
  constexpr static bool is_lazy = false;
  using Value = V;
  constexpr static size_t const rows = ROWS;
  constexpr static size_t const cols = COLS;

  Value data[ROWS][COLS];

  Value const &at(size_t i, size_t k) const { return data[i][k]; }

  constexpr static unsigned int AccessCost() { return 1; }

  friend auto operator<<(std::ostream &os, DenseMatrix const &m)
      -> std::ostream & {
    return os << "matrix " << rows << "x" << cols;
  }
};

template <typename Left, typename Right> struct LazyProductMatrix {
  constexpr static bool is_lazy = true;
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

template <typename Left, typename Right> struct LazySumMatrix {
  constexpr static bool is_lazy = true;
  using Value = typename Left::Value;
  constexpr static size_t const rows = Left::rows;
  constexpr static size_t const cols = Left::cols;

  const Left &left;
  const Right &right;

  LazySumMatrix(const Left &left, const Right &right)
      : left{left}, right{right} {}

  Value at(size_t i, size_t k) const { return left.at(i, k) + right.at(i, k); }

  constexpr static unsigned int AccessCost() {
    return Left::AccessCost() + Right::AccessCost();
  }
};

template <typename Left, typename Right> struct LazyProductStep {
  using Result = LazyProductMatrix<Left, Right>;

  constexpr unsigned int EvaluationCost() const { return 0; }

  Result Eval(const std::tuple<Left, Right> &arguments) const {
    return LazyProductMatrix{std::get<0>(arguments), std::get<1>(arguments)};
  }

  friend auto operator<<(std::ostream &os, LazyProductStep const &m)
      -> std::ostream & {
    return os << "lazy*";
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

  friend auto operator<<(std::ostream &os, ComputeProductStep const &m)
      -> std::ostream & {
    return os << "compute*";
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

template <typename Left, typename Right> struct LazySumStep {
  using Result = LazySumMatrix<Left, Right>;

  constexpr unsigned int EvaluationCost() const { return 0; }

  Result Eval(const std::tuple<Left, Right> &arguments) const {
    return LazySumMatrix{std::get<0>(arguments), std::get<1>(arguments)};
  }

  friend auto operator<<(std::ostream &os, LazySumStep const &m)
      -> std::ostream & {
    return os << "lazy+";
  }
};

struct MatrixSumOperation {
  template <typename Arguments>
  constexpr static auto const GetAllPossibleSteps() {
    using Left = typename std::tuple_element<0, Arguments>::type;
    using Right = typename std::tuple_element<1, Arguments>::type;
    return std::tuple{LazySumStep<Left, Right>{}};
  }
};

template <typename Arguments_> struct MatrixSumExpression {
  using Arguments = Arguments_;
  using Operation = MatrixSumOperation;
  const Arguments arguments;
};

template <typename Left, typename Right>
MatrixSumExpression<std::tuple<Left, Right>> operator+(const Left &left,
                                                       const Right &right) {
  return {{left, right}};
}

template <typename Trees>
constexpr auto
GetBestMatrixEvaluationTreeToNonLazyMatrix(const Trees &evaluation_trees) {
  const auto non_lazy_evaluation_trees =
      tuple_filter(evaluation_trees, [](const auto &tree) {
        using Tree = typename std::remove_reference<decltype(tree)>::type;
        return !Tree::Result::is_lazy;
      });
  constexpr const auto scores =
      apply_to_each(non_lazy_evaluation_trees, [](const auto &tree) {
        return SumOverEvaluationSteps(
            [](const auto &step) { return step.EvaluationCost(); }, tree);
      });
  constexpr const size_t best_index = tuple_argmin(scores);
  return std::get<best_index>(non_lazy_evaluation_trees);
}
