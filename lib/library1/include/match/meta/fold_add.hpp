#pragma once

#include <type_traits>

namespace pm::meta {

/**
 * @brief Concept for integral types.
 *
 * This concept checks if a given type is an integral type.
 */
template <typename T>
concept IntegralType = std::is_integral_v<T>;

/**
 * @brief Structure for compile-time addition folding of integral values.
 *
 * This structure computes the sum of provided integral values at compile time.
 */
template <IntegralType Integral, Integral... ns>
struct fold_add;

/**
 * @brief Partial specialization for single integral value case.
 *
 * This serves as the base case for the fold operation with one value.
 */
template <IntegralType Integral, Integral n>
struct fold_add<Integral, n> : std::integral_constant<Integral, n> {};

/**
 * @brief Partial specialization for folding multiple integral values.
 *
 * Recursively computes the sum of all provided integral values.
 */
template <IntegralType Integral, Integral n, Integral... ns>
struct fold_add<Integral, n, ns...>
    : std::integral_constant<Integral, n + fold_add<Integral, ns...>::value> {};

/**
 * @brief Variable template for addition folding of integral values.
 *
 * Provides the sum of provided integral values as a constexpr value.
 */
template <IntegralType Integral, Integral... ns>
constexpr Integral fold_add_v = fold_add<Integral, ns...>::value;

}  // namespace pm::meta