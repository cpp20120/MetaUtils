#pragma once

#include <type_traits>

#include "./fold_and.hpp"

namespace pm::meta {

/**
 * @brief Structure for compile-time logical OR folding.
 *
 * Computes the logical OR operation on all provided boolean values at compile
 * time. Returns true if any value is true, false only if all values are false.
 * Implemented using De Morgan's laws by negating the inputs and using fold_and.
 */
template <bool... bs>
struct fold_or : std::integral_constant<bool, !fold_and<(!bs)...>::value> {};

/**
 * @brief Variable template for logical OR folding.
 *
 * Provides the result of logical OR operation on the provided boolean values
 * as a constexpr value. Equivalent to (bs || ...) in C++17 fold expressions.
 */
template <bool... bs>
constexpr bool fold_or_v = fold_or<bs...>::value;

}  // namespace meta
}  // namespace pm