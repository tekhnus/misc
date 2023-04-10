#include <algorithm>
#include <iostream>

#include "utils/apply_to_each.hpp"
#include "utils/misc.hpp"

#include "matrix.hpp"

int main() {
  DenseMatrix<int, 5, 5> a{}, b{}, c{}, d{}, e{};
  const auto some_expression = (a * b * c) * (c + d * e);
  constexpr const auto evaluation_trees =
      GetAllPossibleEvaluationTrees(some_expression);
  constexpr const auto best_evaluation_tree =
      GetBestMatrixEvaluationTreeToNonLazyMatrix(evaluation_trees);
  const auto result = best_evaluation_tree.Eval(some_expression);
  std::cout << "result: " << result << std::endl;

  // Some debug info.
  std::cout << "best tree: " << best_evaluation_tree << std::endl;
  apply_to_each(evaluation_trees, [](const auto &tree) {
    unsigned int cost = SumOverEvaluationSteps(
        [](const auto &y) { return y.EvaluationCost(); }, tree);
    std::cout << "tree: " << tree << " score: " << cost << std::endl;
    return 0;
  });
  return 0;
}
