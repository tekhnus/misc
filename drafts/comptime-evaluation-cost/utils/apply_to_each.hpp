// Taken from https://codereview.stackexchange.com/a/193461/196900

#include <tuple>

namespace details {
template <typename Tuple, typename Mapping> struct return_type;

template <template <typename...> typename Tuple, typename... Types,
          typename Mapping>
struct return_type<Tuple<Types...>, Mapping> {
  using type = Tuple<std::invoke_result_t<Mapping, Types>...>;
};
template <template <typename, std::size_t> typename Array, typename T,
          std::size_t Size, typename Mapping>
struct return_type<Array<T, Size>, Mapping> {
  using type = Array<std::invoke_result_t<Mapping, T>, Size>;
};

template <typename Tuple, typename Mapping>
using return_type_t = typename return_type<Tuple, Mapping>::type;

template <typename Tuple, typename Mapping, std::size_t... Indices>
constexpr return_type_t<std::decay_t<Tuple>, std::decay_t<Mapping>>
apply_to_each(Tuple &&tup, Mapping &&mapping, std::index_sequence<Indices...>) {
  return {mapping(std::get<Indices>(std::forward<Tuple>(tup)))...};
}
} // namespace details

template <typename Tuple, typename Mapping,
          std::size_t Size = std::tuple_size<std::decay_t<Tuple>>::value>
constexpr auto apply_to_each(Tuple &&tup, Mapping &&mapping) {
  return details::apply_to_each(std::forward<Tuple>(tup),
                                std::forward<Mapping>(mapping),
                                std::make_index_sequence<Size>{});
}
