#include "utils/apply_to_each.hpp"

template <typename EvaluationStep, typename ArgumentEvaluationTrees>
struct EvaluationTree {
  using Result = typename EvaluationStep::Result;

  const EvaluationStep step;
  const ArgumentEvaluationTrees argument_trees;

  constexpr EvaluationTree(const EvaluationStep &step,
                            const ArgumentEvaluationTrees &argument_trees)
      : step(step), argument_trees(argument_trees) {}

  template <typename Expression> Result Eval(const Expression &expression) const {
    return std::apply(
        [&](const auto &...argument_trees_) {
          return std::apply(
              [&](const auto &...arguments) {
                return step.Eval({argument_trees_.Eval(arguments)...});
              },
              expression.arguments);
        },
        argument_trees);
  }
};

template <typename T> struct EvaluationTreeLeaf {
  using Result = T;

  Result Eval(const Result &expression) const { return expression; }
};

template <typename Function, typename Tree>
constexpr unsigned int SumOverEvaluationSteps(Function function, const Tree& tree) {
  return std::apply(
             [&](const auto &...args) {
               return (SumOverEvaluationSteps(function, args) + ...);
             },
             tree.argument_trees) +
         function(tree.step);
}

template <typename Function, typename Tree>
constexpr unsigned int SumOverEvaluationSteps(Function function, const EvaluationTreeLeaf<Tree>& tree) {
  return 0;
}

template <typename Expression, typename = void> struct Evaluator {
  constexpr static auto GetAllPossibleEvaluationTrees() {
    return std::tuple{EvaluationTreeLeaf<Expression>{}};
  }
};

template <typename Expression>
struct Evaluator<Expression, std::void_t<typename Expression::Meta>> {
  constexpr static auto GetAllPossibleEvaluationTrees() {
    using LeftEvaluator = Evaluator<typename Expression::Left>;
    using RightEvaluator = Evaluator<typename Expression::Right>;

    auto left_strategies = LeftEvaluator::GetAllPossibleEvaluationTrees();
    auto right_strategies = RightEvaluator::GetAllPossibleEvaluationTrees();
    auto product = cartesian_product(left_strategies, right_strategies);
    auto pair_to_strategies = [](const auto &pair) {
      using PairOfStrategies =
          typename std::remove_reference<decltype(pair)>::type;
      using Left =
          typename std::tuple_element<0, PairOfStrategies>::type::Result;
      using Right =
          typename std::tuple_element<1, PairOfStrategies>::type::Result;

      auto const value = Expression::Meta::template GetSteps<Left, Right>();

      return apply_to_each(value, [&pair](const auto &step) {
        return EvaluationTree{step, pair};
      });
    };
    return std::apply(
        [](const auto &...args) { return std::tuple_cat(args...); },
        apply_to_each(product, pair_to_strategies));
  }
};
