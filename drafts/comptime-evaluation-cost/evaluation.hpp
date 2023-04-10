#include "utils/apply_to_each.hpp"

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
