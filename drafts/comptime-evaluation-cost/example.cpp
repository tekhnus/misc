#include <iostream>

#include "utils/cartesian_product.hpp"
#include "utils/apply_to_each.hpp"

template <typename V, size_t ROWS, size_t COLS> struct DenseMatrix {
  using Value = V;
  static size_t const rows = ROWS;
  static size_t const cols = COLS;

  Value data[ROWS][COLS];

  Value const &at(size_t i, size_t k) const { return data[i][k]; }

  static unsigned int AccessCost() { return 1; }
};

template <typename Left, typename Right> struct ProductMatrix {
  using Value = typename Left::Value;
  static size_t const rows = Left::rows;
  static size_t const cols = Right::cols;

  const Left &left;
  const Right &right;

  ProductMatrix(const Left &left, const Right &right)
      : left{left}, right{right} {}

  Value at(size_t i, size_t k) const {
    Value value = 0;
    for (size_t j = 0; j < Left::cols; ++j) {
      value += left.at(i, j) * right.at(j, k);
    }
    return value;
  }

  static unsigned int AccessCost() { return Left::cols * Right::rows * Left::AccessCost() * Right::AccessCost(); }
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

template <typename Left, typename Right> struct Sum {
  const Left &left;
  const Right &right;
};

template <typename Left, typename Right>
Sum<Left, Right> operator+(const Left &left, const Right &right) {
  return Sum<Left, Right>{left, right};
}

template <typename Left, typename Right> struct Product {
  const Left &left;
  const Right &right;
};

template <typename Left, typename Right>
Product<Left, Right> operator*(const Left &left, const Right &right) {
  return Product<Left, Right>{left, right};
}

template <typename T>
struct TrivialStrategy {
  using Target = T;
  unsigned int EvaluationCost() const { return 0; }
};

template <typename Left, typename Right>
struct LazyProductStep {
  using Target = ProductMatrix<Left, Right>;

  unsigned int EvaluationCost() const { return 0; }
};

template <typename Left, typename Right>
struct EagerProductStep {
  using Target = DenseMatrix<typename Left::Value, Left::rows, Right::cols>;

  unsigned int EvaluationCost() const { return Left::rows * Right::rows * Left::cols * Right::cols * Left::AccessCost() * Right::AccessCost(); }
};

template <typename Functor, typename LeftStrategy, typename RightStrategy>
struct FunctorStrategy {
  const Functor f;
  const LeftStrategy left;
  const RightStrategy right;
  using Target = typename Functor::Target;

  FunctorStrategy(const Functor& f, const LeftStrategy &left, const RightStrategy &right)
      : f(f), left(left), right(right) {}
  unsigned int EvaluationCost() const { return left.EvaluationCost() + right.EvaluationCost() + f.EvaluationCost(); }
};

template <typename Expression> struct Evaluator;

template <typename Left, typename Right>
struct Evaluator<const Product<Left, Right>> {
  using LeftEvaluator = Evaluator<Left>;
  using RightEvaluator = Evaluator<Right>;

  static auto GetStrategies() {
    auto left_strategies = LeftEvaluator::GetStrategies();
    auto right_strategies = RightEvaluator::GetStrategies();
    auto product = cartesian_product(left_strategies, right_strategies);
    auto pair_to_lazy_strategy = [](const auto &pair) {
      using pair_type = typename std::remove_reference<decltype(pair)>::type;
      using first_strategy_type = typename std::tuple_element<0, pair_type>::type;
      using second_strategy_type = typename std::tuple_element<1, pair_type>::type;
      return FunctorStrategy{LazyProductStep<typename first_strategy_type::Target, typename second_strategy_type::Target>{}, std::get<0>(pair), std::get<1>(pair)};
    };
    auto pair_to_eager_strategy = [](const auto &pair) {
      using pair_type = typename std::remove_reference<decltype(pair)>::type;
      using first_strategy_type = typename std::tuple_element<0, pair_type>::type;
      using second_strategy_type = typename std::tuple_element<1, pair_type>::type;
      return FunctorStrategy{EagerProductStep<typename first_strategy_type::Target, typename second_strategy_type::Target>{}, std::get<0>(pair), std::get<1>(pair)};
    };
    return std::tuple_cat(apply_to_each(product, pair_to_lazy_strategy),
                          apply_to_each(product, pair_to_eager_strategy));
  }
};

template <typename Left, typename Right>
struct Evaluator<Product<Left, Right>> : Evaluator<const Product<Left, Right>> {
};

template <typename V, size_t ROWS, size_t COLS>
struct Evaluator<DenseMatrix<V, ROWS, COLS>> {
  static auto GetStrategies() { return std::tuple{TrivialStrategy<DenseMatrix<V, ROWS, COLS>>{}}; }
};

int main() {
  DenseMatrix<int, 5, 5> a, b, c;
  ProductMatrix{ProductMatrix{a, b}, c}.at(0, 0);
  ComputeProduct(ComputeProduct(a, b), c).at(0, 0);
  ComputeProduct(ProductMatrix{a, b}, c).at(0, 0);

  const auto abstract_product = (a * b) * c;
  Evaluator<decltype(abstract_product)> e;
  apply_to_each(e.GetStrategies(), [](const auto &x) {
    using x_type = typename std::remove_reference<decltype(x)>::type;
    using target_type = typename x_type::Target;
    std::cout << typeid(target_type).name() << " " << x.EvaluationCost() << std::endl;
    return 0;
  });
  return 0;
}
