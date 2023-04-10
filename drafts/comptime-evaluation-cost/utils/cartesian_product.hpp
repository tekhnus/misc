// Taken from https://codereview.stackexchange.com/a/217496/196900
#pragma once

#include <tuple>

namespace detail {
template <class T>
constexpr std::size_t tuple_size_v = std::tuple_size_v<std::decay_t<T>>;

template <std::size_t I, class T, std::size_t... N>
static constexpr auto index_helper(std::index_sequence<N...>) noexcept {
  return (1 * ... * (I < N ? tuple_size_v<std::tuple_element_t<N, T>> : 1));
}
template <std::size_t N, std::size_t I, class T>
static constexpr auto index() noexcept {
  return N / index_helper<I, T>(std::make_index_sequence<tuple_size_v<T>>()) %
         tuple_size_v<std::tuple_element_t<I, T>>;
}

template <std::size_t N, class T, std::size_t... I>
static constexpr auto cartesian_product(T t,
                                        std::index_sequence<I...>) noexcept {
  return std::forward_as_tuple(std::get<index<N, I, T>()>(std::get<I>(t))...);
}
template <class T, std::size_t... N>
static constexpr auto cartesian_product(T t,
                                        std::index_sequence<N...>) noexcept {
  return std::make_tuple(
      cartesian_product<N>(t, std::make_index_sequence<tuple_size_v<T>>())...);
}

template <class T> auto tuple_no_ref(T t) {
  return std::apply([](auto &&...x) { return std::make_tuple(x...); }, t);
}
template <class T> auto tuple2_no_ref(T t) {
  return std::apply(
      [](auto &&...x) { return std::make_tuple(tuple_no_ref(x)...); }, t);
}
} // namespace detail

template <class... T> constexpr auto cartesian_product_ref(T &&...t) noexcept {
  constexpr auto N = sizeof...(T) ? (1 * ... * detail::tuple_size_v<T>) : 0;
  return detail::cartesian_product(std::forward_as_tuple(t...),
                                   std::make_index_sequence<N>());
}

template <class... T>
constexpr auto
cartesian_product(T &&...t) noexcept(noexcept(decltype(detail::tuple2_no_ref(
    cartesian_product_ref(t...)))(cartesian_product_ref(t...)))) {
  auto r = cartesian_product_ref(t...);
  using R = decltype(detail::tuple2_no_ref(r));
  return R(r);
}
