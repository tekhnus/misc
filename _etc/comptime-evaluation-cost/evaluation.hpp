#pragma once

#include <iostream>
#include <tuple>

#include "utils/apply_to_each.hpp"
#include "utils/cartesian_product.hpp"
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

  friend auto
  operator<<(std::ostream &os,
             EvaluationTree<EvaluationStep, ArgumentEvaluationTrees> const &m)
      -> std::ostream & {
    return std::apply(
        [&](const auto &...argument_trees_) -> std::ostream & {
          return ((os << m.step << "(") << ... << argument_trees_) << ")";
        },
        m.argument_trees);
  }
};

template <typename T> struct EvaluationTreeLeaf {
  using Result = T;

  Result Eval(const Result &expression) const { return expression; }

  friend auto operator<<(std::ostream &os, EvaluationTreeLeaf<T> const &m)
      -> std::ostream & {
    return os;
  }
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

template <typename T> struct get_result_types_from_tree_types;

template <typename T, typename... Rest>
struct get_result_types_from_tree_types<const std::tuple<T, Rest...>> {
  using type = tuple_cat_t<std::tuple<typename T::Result>,
                           typename get_result_types_from_tree_types<
                               const std::tuple<Rest...>>::type>;
};

template <> struct get_result_types_from_tree_types<const std::tuple<>> {
  using type = const std::tuple<>;
};

template <typename Expression, typename = void> struct Evaluator {
  constexpr static auto GetAllPossibleEvaluationTrees() {
    return std::tuple{EvaluationTreeLeaf<Expression>{}};
  }
};

template <typename Expression>
struct Evaluator<Expression, std::void_t<typename Expression::Operation>> {
  constexpr static auto GetAllPossibleEvaluationTrees() {
    using LeftArgument =
        typename std::tuple_element<0, typename Expression::Arguments>::type;
    using RightArgument =
        typename std::tuple_element<1, typename Expression::Arguments>::type;
    auto left_argument_trees =
        Evaluator<LeftArgument>::GetAllPossibleEvaluationTrees();
    auto right_argument_trees =
        Evaluator<RightArgument>::GetAllPossibleEvaluationTrees();
    auto argument_eval_tree_combinations =
        cartesian_product(left_argument_trees, right_argument_trees);
    auto get_all_possible_tree_completions = [](const auto
                                                    &argument_eval_trees) {
      using ArgumentEvaluationTrees =
          typename std::remove_reference<decltype(argument_eval_trees)>::type;
      using EvaluatedArguments = typename get_result_types_from_tree_types<
          ArgumentEvaluationTrees>::type;

      auto const all_possible_steps =
          Expression::Operation::template GetAllPossibleSteps<
              EvaluatedArguments>();

      return apply_to_each(all_possible_steps, [&](const auto &step) {
        return EvaluationTree{step, argument_eval_trees};
      });
    };
    return std::apply(
        [](const auto &...args) { return std::tuple_cat(args...); },
        apply_to_each(argument_eval_tree_combinations,
                      get_all_possible_tree_completions));
  }
};

template <typename T>
constexpr static auto GetAllPossibleEvaluationTrees(const T &) {
  return Evaluator<T>::GetAllPossibleEvaluationTrees();
}
