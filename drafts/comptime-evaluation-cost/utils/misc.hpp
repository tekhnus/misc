#include <tuple>

template <typename T> constexpr const auto tuple_argmin(const T &t) {
  return std::apply(
      [](const auto &...args) {
        auto best = std::min({args...});
        size_t best_index = 0;
        size_t index = 0;
        ((args == best ? (best_index = index++) : index++), ...);
        return best_index;
      },
      t);
}
