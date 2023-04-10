#pragma once

#include "utils/apply_to_each.hpp"
#include "utils/misc.hpp"

template <typename EvaluationStep, typename ArgumentEvaluationTrees>
struct EvaluationTree {
  using Result = typename EvaluationStep::Result;

  const EvaluationStep step;
  const ArgumentEvaluationTrees argument_trees;

  constexpr EvaluationTree(const EvaluationStep &step,
                           const ArgumentEvaluationTrees &argument_trees)
      : step(step), argument_trees(argument_trees) {}

  template <typename Expression>
  Result Eval(const Expression &expression) const {
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
constexpr unsigned int SumOverEvaluationSteps(Function function,
                                              const Tree &tree) {
  return std::apply(
             [&](const auto &...args) {
               return (SumOverEvaluationSteps(function, args) + ...);
             },
             tree.argument_trees) +
         function(tree.step);
}

template <typename Function, typename Tree>
constexpr unsigned int
SumOverEvaluationSteps(Function function,
                       const EvaluationTreeLeaf<Tree> &tree) {
  return 0;
}

template <typename T>
struct get_result_types_from_tree_types;

template <typename T, typename... Rest>
struct get_result_types_from_tree_types<const std::tuple<T, Rest...>> {
  using value = tuple_cat_t<std::tuple<typename T::Result>, typename get_result_types_from_tree_types<const std::tuple<Rest...>>::value>;
};

template <>
struct get_result_types_from_tree_types<const std::tuple<>> {
  using value = const std::tuple<>;
};

template <typename Expression, typename = void> struct Evaluator {
  constexpr static auto GetAllPossibleEvaluationTrees() {
    return std::tuple{EvaluationTreeLeaf<Expression>{}};
  }
};

template <typename Expression>
struct Evaluator<Expression, std::void_t<typename Expression::Meta>> {
  constexpr static auto GetAllPossibleEvaluationTrees() {
    auto left_argument_trees =
        Evaluator<typename Expression::Left>::GetAllPossibleEvaluationTrees();
    auto right_argument_trees =
        Evaluator<typename Expression::Right>::GetAllPossibleEvaluationTrees();
    auto argument_eval_tree_combinations = cartesian_product(left_argument_trees, right_argument_trees);
    auto get_all_possible_tree_completions = [](const auto &argument_eval_trees) {
      using ArgumentEvaluationTrees =
          typename std::remove_reference<decltype(argument_eval_trees)>::type;
      using EvaluatedArguments = typename get_result_types_from_tree_types<ArgumentEvaluationTrees>::value;

      auto const all_possible_steps = Expression::Meta::template GetAllPossibleSteps<EvaluatedArguments>();

      return apply_to_each(all_possible_steps, [&](const auto &step) {
        return EvaluationTree{step, argument_eval_trees};
      });
    };
    return std::apply(
        [](const auto &...args) { return std::tuple_cat(args...); },
        apply_to_each(argument_eval_tree_combinations, get_all_possible_tree_completions));
  }
};
