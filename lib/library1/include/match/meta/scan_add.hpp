#pragma once

#include <type_traits>
#include <utility>

#include "./fold_add.hpp"

namespace pm::meta {

/**
 * @brief Structure for compile-time prefix sum (scan with addition) of integral
 * values.
 *
 * Computes a prefix sum (inclusive scan) of the provided integral values at
 * compile time. The result is available as a std::integer_sequence of the
 * partial sums.
 */
template <IntegralType Integral, Integral... Ns>
struct scan_add;

/**
 * @brief Type alias for convenient access to the scan addition result.
 *
 * Provides the resulting std::integer_sequence of partial sums.
 */
template <IntegralType Integral, Integral... Ns>
using scan_add_t = typename scan_add<Integral, Ns...>::type;

/**
 * @brief Helper structure implementing the scan addition operation.
 *
 * Recursively builds the sequence of partial sums through template
 * specialization.
 */
template <IntegralType Integral, typename Result, Integral... Ns>
struct scan_add_impl;

/**
 * @brief Base case specialization for single value scan addition.
 *
 * Terminates the recursion with the final accumulated value.
 */
template <IntegralType Integral, Integral N, Integral... Ms>
struct scan_add_impl<Integral, std::integer_sequence<Integral, Ms...>, N> {
  using type = std::integer_sequence<Integral, Ms..., N>;
};

/**
 * @brief Recursive case specialization for multiple values scan addition.
 *
 * Accumulates values and builds the partial sums sequence incrementally.
 */
template <IntegralType Integral, Integral N1, Integral N2, Integral... Ns,
          Integral... Ms>
struct scan_add_impl<Integral, std::integer_sequence<Integral, Ms...>, N1, N2,
                     Ns...> {
  using type =
      typename scan_add_impl<Integral,
                             std::integer_sequence<Integral, Ms..., N1>,
                             N1 + N2, Ns...>::type;
};

/**
 * @brief Primary template for compile-time prefix sum calculation.
 *
 * Initiates the scan addition process with an empty sequence.
 */
template <IntegralType Integral, Integral... Ns>
struct scan_add {
  using type = typename scan_add_impl<Integral, std::integer_sequence<Integral>,
                                      Ns...>::type;
};

}  // namespace meta
}  // namespace pm