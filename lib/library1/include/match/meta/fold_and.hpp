#pragma once

#include <type_traits>

#include "bools.hpp"

namespace pm::meta {

namespace detail {

/**
 * @brief Template structure that converts any value to true.
 *
 * This structure is used internally to convert arbitrary values to true
 * for boolean folding operations.
 */
template <bool b>
struct as_true {
  static constexpr bool value = true;
};

}  // namespace detail

/**
 * @brief Structure for compile-time logical AND folding.
 *
 * Computes the logical AND of all provided boolean values at compile time.
 * Returns true if all values are true, false otherwise.
 */
template <bool... bs>
struct fold_and
    : std::integral_constant<bool,
                             std::is_same<bools<detail::as_true<bs>::value...>,
                                          bools<bs...>>::value> {};

/**
 * @brief Variable template for logical AND folding.
 *
 * Provides the result of logical AND operation on the provided boolean values
 * as a constexpr value.
 */
template <bool... bs>
constexpr bool fold_and_v = fold_and<bs...>::value;

}  // namespace meta
}  // namespace pm