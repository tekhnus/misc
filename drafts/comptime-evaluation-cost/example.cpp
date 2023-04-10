#include <algorithm>
#include <iostream>

#include "utils/apply_to_each.hpp"
#include "utils/cartesian_product.hpp"

template <typename V, size_t ROWS, size_t COLS> struct DenseMatrix {
  using Value = V;
  constexpr static size_t const rows = ROWS;
  constexpr static size_t const cols = COLS;

  Value data[ROWS][COLS];

  Value const &at(size_t i, size_t k) const { return data[i][k]; }

  constexpr static unsigned int AccessCost() { return 1; }
};

template <typename Left, typename Right> struct ProductMatrix {
  using Value = typename Left::Value;
  constexpr static size_t const rows = Left::rows;
  constexpr static size_t const cols = Right::cols;

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

template <typename Left, typename Right> struct LazyProductStep {
  using Target = ProductMatrix<Left, Right>;

  constexpr unsigned int EvaluationCost() const { return 0; }

  Target Eval(const Left &left, const Right &right) {
    return ProductMatrix{left, right};
  }
};

template <typename Left, typename Right> struct EagerProductStep {
  using Target = DenseMatrix<typename Left::Value, Left::rows, Right::cols>;

  constexpr unsigned int EvaluationCost() const {
    return Left::rows * Right::rows * Left::cols * Right::cols *
           Left::AccessCost() * Right::AccessCost();
  }

  Target Eval(const Left &left, const Right &right) const {
    return ComputeProduct(left, right);
  }
};

struct ProductMeta {
  template <typename Left, typename Right>
  constexpr static auto const GetSteps() {
    return std::tuple{LazyProductStep<Left, Right>{},
                      EagerProductStep<Left, Right>{}};
  }
};

template <typename Left_, typename Right_> struct Product {
  using Meta = ProductMeta;
  using Left = Left_;
  using Right = Right_;

  const Left left;
  const Right right;
};

template <typename Left, typename Right>
Product<Left, Right> operator*(const Left &left, const Right &right) {
  return Product<Left, Right>{left, right};
}

template <typename T> struct TrivialStrategy {
  using Target = T;
  constexpr unsigned int EvaluationCost() const { return 0; }

  Target Eval(const Target &expression) const { return expression; }
};

template <typename Functor, typename LeftStrategy, typename RightStrategy>
struct FunctorStrategy {
  const Functor f;
  const LeftStrategy left;
  const RightStrategy right;
  using Target = typename Functor::Target;

  constexpr FunctorStrategy(const Functor &f, const LeftStrategy &left,
                            const RightStrategy &right)
      : f(f), left(left), right(right) {}
  constexpr unsigned int EvaluationCost() const {
    return left.EvaluationCost() + right.EvaluationCost() + f.EvaluationCost();
  }

  template <typename T> constexpr bool operator<(const T &another) {
    return EvaluationCost() < another.EvaluationCost();
  }

  template <typename Source> Target Eval(const Source &expression) const {
    return f.Eval(left.Eval(expression.left), right.Eval(expression.right));
  }
};

template <typename Expression, typename = void> struct Evaluator {
  constexpr static auto GetStrategies() {
    return std::tuple{TrivialStrategy<Expression>{}};
  }
};

template <typename Expression>
struct Evaluator<Expression, std::void_t<typename Expression::Meta>> {
  constexpr static auto GetStrategies() {
    using LeftEvaluator = Evaluator<typename Expression::Left>;
    using RightEvaluator = Evaluator<typename Expression::Right>;

    auto left_strategies = LeftEvaluator::GetStrategies();
    auto right_strategies = RightEvaluator::GetStrategies();
    auto product = cartesian_product(left_strategies, right_strategies);
    auto pair_to_strategies = [](const auto &pair) {
      using PairOfStrategies =
          typename std::remove_reference<decltype(pair)>::type;
      using Left =
          typename std::tuple_element<0, PairOfStrategies>::type::Target;
      using Right =
          typename std::tuple_element<1, PairOfStrategies>::type::Target;

      auto const value = Expression::Meta::template GetSteps<Left, Right>();

      return apply_to_each(value, [&pair](const auto &step) {
        return FunctorStrategy{step, std::get<0>(pair), std::get<1>(pair)};
      });
    };
    return std::apply(
        [](const auto &...args) { return std::tuple_cat(args...); },
        apply_to_each(product, pair_to_strategies));
  }
};

int main() {
  DenseMatrix<int, 5, 5> a{}, b{}, c{};
  const auto abstract_product = (a * b) * c;
  constexpr Evaluator<decltype(abstract_product)> e;
  constexpr const auto strategies = e.GetStrategies();
  constexpr const auto scores = apply_to_each(strategies, [](const auto &x) {
    using x_type = typename std::remove_reference<decltype(x)>::type;
    using target_type = typename x_type::Target;
    unsigned int cost = x.EvaluationCost();
    if (!std::is_same<target_type, DenseMatrix<int, 5, 5>>::value) {
      cost = 100500u;
    }
    return std::pair{cost, x};
  });
  constexpr const size_t best_index = std::apply(
      [](const auto &...args) {
        size_t best_score = 0;
        auto best = std::min({args.first...});
        size_t best_index = 0;
        size_t index = 0;
        ((args.first == best ? (best_index = index++) : index++), ...);
        return best_index;
      },
      scores);
  constexpr const auto best_score = std::get<best_index>(scores);
  const auto [sc, st] = best_score;
  std::cout << sc << std::endl;
  std::cout << typeid(st).name() << std::endl;
  st.Eval(abstract_product);

  apply_to_each(strategies, [](const auto &x) {
    using x_type = typename std::remove_reference<decltype(x)>::type;
    using target_type = typename x_type::Target;
    std::cout << typeid(target_type).name() << " " << x.EvaluationCost()
              << std::endl;
    return 0;
  });
  return 0;
}
