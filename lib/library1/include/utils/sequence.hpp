#pragma once 

namespace core::meta::utils {

template <size_t... indices>
struct index_tuple {
  template <size_t N>
  using apply = index_tuple<indices..., N>;
};

template <size_t N>
struct next_index_tuple {
  using type = typeof_t<next_index_tuple<N - 1>>::template apply<N - 1>;
};

template <>
struct next_index_tuple<0> {
  using type = index_tuple<>;
};

template <size_t N>
using next_index_tuple_t = typeof_t<next_index_tuple<N>>;

template <size_t N>
using make_index_tuple = next_index_tuple_t<N>;

template <typename... Args>
using index_tuple_for = make_index_tuple<sizeof_v<Args...>>;

template <typename T, T... indices>
struct integer_sequence {
  using type = T;

  static constexpr decltype(auto) size() noexcept { return sizeof...(indices); }
};

template <typename T, typename U, bool B>
struct duple;

template <typename T, T... indices>
struct duple<T, integer_sequence<T, indices...>, false> {
  using type =
      integer_sequence<T, indices..., (sizeof...(indices) + indices)...>;
};

template <typename T, T... indices>
struct duple<T, integer_sequence<T, indices...>, true> {
  using type =
      integer_sequence<T, indices..., (sizeof...(indices) + indices)...,
                       sizeof...(indices) * 2>;
};

template <typename T, typename U, bool B>
using duple_t = typeof_t<duple<T, U, B>>;

template <typename T, size_t N>
struct next_integer_sequence
    : duple<T, typeof_t<next_integer_sequence<T, N / 2>>, N % 2> {};

template <typename T>
struct next_integer_sequence<T, 1> {
  using type = integer_sequence<T, 0>;
};

template <typename T>
struct next_integer_sequence<T, 0> {
  using type = integer_sequence<T>;
};

template <typename T, size_t N>
using next_integer_sequence_t = typeof_t<next_integer_sequence<T, N>>;

template <typename T, size_t N>
using make_integer_sequence = next_integer_sequence_t<T, N>;

template <typename T, typename... Args>
using integer_sequence_for = make_integer_sequence<T, sizeof_v<Args...>>;

template <size_t... indices>
using index_sequence = integer_sequence<size_t, indices...>;

template <size_t N>
using make_index_sequence = make_integer_sequence<size_t, N>;

template <typename... Args>
using index_sequence_for = make_index_sequence<sizeof_v<Args...>>;

template <auto N>
using index_sequence_of_c = std::make_index_sequence<N>;

template <typename T, auto N = 0>
using index_sequence_of_t = index_sequence_of_c<sizeof_t_v<T> - N>;
}  // namespace core::meta::utils