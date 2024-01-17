#pragma once

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

// Taken from
// https://stackoverflow.com/questions/53394100/concatenating-tuples-as-types
template <typename... input_t>
using tuple_cat_t = decltype(std::tuple_cat(std::declval<input_t>()...));

// Taken from https://stackoverflow.com/a/50971858/1617738
template <typename T, typename F> constexpr auto tuple_filter(T tup, F f) {
  return std::apply(
      [&](auto first, auto... rest) {
        auto filtered_rest = [&] {
          if constexpr (sizeof...(rest)) {
            return tuple_filter(std::tuple{rest...}, f);
          } else {
            return std::tuple{};
          }
        }();

        if constexpr (f(first)) {
          return std::tuple_cat(std::tuple{first}, filtered_rest);
        } else {
          return filtered_rest;
        }
      },
      tup);
}
