#include <algorithm>
#include <iostream>

#include "utils/apply_to_each.hpp"
#include "utils/cartesian_product.hpp"

#include "evaluation.hpp"

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

  Target Eval(const std::tuple<Left, Right>& elements) const {
    return ProductMatrix{std::get<0>(elements), std::get<1>(elements)};
  }
};

template <typename Left, typename Right> struct EagerProductStep {
  using Target = DenseMatrix<typename Left::Value, Left::rows, Right::cols>;

  constexpr unsigned int EvaluationCost() const {
    return Left::rows * Right::rows * Left::cols * Right::cols *
           Left::AccessCost() * Right::AccessCost();
  }

  Target Eval(const std::tuple<Left, Right>& elements) const {
    return ComputeProduct(std::get<0>(elements), std::get<1>(elements));
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

  const std::tuple<Left, Right> arguments;
};

template <typename Left, typename Right>
Product<Left, Right> operator*(const Left &left, const Right &right) {
  return Product<Left, Right>{{left, right}};
}

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
