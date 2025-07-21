/**
 * @file sequence.hpp
 * @brief Template metaprogramming utilities for index sequences and integer
 * sequences
 *
 * This header provides compile-time utilities for generating and manipulating
 * index sequences and integer sequences, which are fundamental for tuple
 * operations, variadic template expansions, and other template metaprogramming
 * tasks.
 */

#pragma once

namespace core::meta::utils {

/**
 * @brief Basic index tuple structure holding a sequence of indices
 * @tparam indices... The sequence of size_t indices
 *
 * This serves as the foundation for index sequence generation and manipulation.
 */
template <size_t... indices>
struct index_tuple {
  /**
   * @brief Appends a new index to the sequence
   * @tparam N The index to append
   */
  template <size_t N>
  using apply = index_tuple<indices..., N>;
};

/**
 * @brief Generates an index tuple containing indices [0..N-1]
 * @tparam N The number of indices to generate
 */
template <size_t N>
struct next_index_tuple {
  using type = typeof_t<next_index_tuple<N - 1>>::template apply<N - 1>;
};

/// @brief Specialization for base case (N=0)
template <>
struct next_index_tuple<0> {
  using type = index_tuple<>;
};

/**
 * @brief Helper type for next_index_tuple
 */
template <size_t N>
using next_index_tuple_t = typeof_t<next_index_tuple<N>>;

/**
 * @brief Creates an index tuple with indices [0..N-1]
 */
template <size_t N>
using make_index_tuple = next_index_tuple_t<N>;

/**
 * @brief Creates an index tuple matching the size of a parameter pack
 */
template <typename... Args>
using index_tuple_for = make_index_tuple<sizeof_v<Args...>>;

/**
 * @brief Basic integer sequence structure
 * @tparam T The integer type (size_t, int, etc.)
 * @tparam indices... The sequence of integer values
 *
 * Similar to std::integer_sequence but with additional functionality.
 */
template <typename T, T... indices>
struct integer_sequence {
  using type = T;  ///< The underlying integer type

  /**
   * @brief Returns the size of the sequence
   * @return constexpr size_t The number of elements in the sequence
   */
  static constexpr decltype(auto) size() noexcept { return sizeof...(indices); }
};

/**
 * @brief Doubles the size of an integer sequence with different strategies
 * @tparam T The integer type
 * @tparam U The input sequence
 * @tparam B Strategy flag (true for odd-sized sequences)
 */
template <typename T, typename U, bool B>
struct duple;

/// @brief Specialization for even-sized doubling
template <typename T, T... indices>
struct duple<T, integer_sequence<T, indices...>, false> {
  using type =
      integer_sequence<T, indices..., (sizeof...(indices) + indices)...>;
};

/// @brief Specialization for odd-sized doubling
template <typename T, T... indices>
struct duple<T, integer_sequence<T, indices...>, true> {
  using type =
      integer_sequence<T, indices..., (sizeof...(indices) + indices)...,
                       sizeof...(indices) * 2>;
};

/**
 * @brief Helper type for duple
 */
template <typename T, typename U, bool B>
using duple_t = typeof_t<duple<T, U, B>>;

/**
 * @brief Generates an integer sequence through recursive doubling
 * @tparam T The integer type
 * @tparam N The desired sequence length
 */
template <typename T, size_t N>
struct next_integer_sequence
    : duple<T, typeof_t<next_integer_sequence<T, N / 2>>, N % 2> {};

/// @brief Base case for sequence generation (N=1)
template <typename T>
struct next_integer_sequence<T, 1> {
  using type = integer_sequence<T, 0>;
};

/// @brief Empty sequence case (N=0)
template <typename T>
struct next_integer_sequence<T, 0> {
  using type = integer_sequence<T>;
};

/**
 * @brief Helper type for next_integer_sequence
 */
template <typename T, size_t N>
using next_integer_sequence_t = typeof_t<next_integer_sequence<T, N>>;

/**
 * @brief Creates an integer sequence of type T with values [0..N-1]
 */
template <typename T, size_t N>
using make_integer_sequence = next_integer_sequence_t<T, N>;

/**
 * @brief Creates an integer sequence matching the size of a parameter pack
 */
template <typename T, typename... Args>
using integer_sequence_for = make_integer_sequence<T, sizeof_v<Args...>>;

/**
 * @brief Specialization for size_t index sequences
 * @tparam indices... The sequence of indices
 */
template <size_t... indices>
using index_sequence = integer_sequence<size_t, indices...>;

/**
 * @brief Creates an index sequence [0..N-1]
 */
template <size_t N>
using make_index_sequence = make_integer_sequence<size_t, N>;

/**
 * @brief Creates an index sequence matching the size of a parameter pack
 */
template <typename... Args>
using index_sequence_for = make_index_sequence<sizeof_v<Args...>>;

/**
 * @brief Creates an index sequence for a compile-time constant
 * @tparam N The compile-time constant value
 */
template <auto N>
using index_sequence_of_c = std::make_index_sequence<N>;

/**
 * @brief Creates an index sequence based on type size
 * @tparam T The input type
 * @tparam N Optional offset (default 0)
 *
 * Creates sequence [0..sizeof(T)-1-N]
 */
template <typename T, auto N = 0>
using index_sequence_of_t = index_sequence_of_c<sizeof_t_v<T> - N>;

}  // namespace core::meta::utils