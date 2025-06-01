#pragma once

#include <type_traits>

#include "./type_utility.hpp"

namespace core::meta::type_traits {

/**
 * @brief Computes the sum type of given types
 * @tparam Ts Types to compute sum for
 */
template <typename... Ts>
using sum_type = type_identity<decltype((... + std::declval<Ts>()))>;

/**
 * @brief Helper alias for sum_type
 * @tparam Ts Types to compute sum for
 */
template <typename... Ts>
using sum_type_t = typename sum_type<Ts...>::type;

/**
 * @brief Checks if type T is present in the given pack
 * @tparam T Type to check for
 * @tparam Pack Pack of types to search in
 */
template <typename T, typename... Pack>
struct is_type_in_pack : std::bool_constant<(std::is_same_v<T, Pack> || ...)> {
};

/**
 * @brief Specialization for empty pack
 * @tparam T Type to check for
 */
template <typename T>
struct is_type_in_pack<T> : std::false_type {};

/**
 * @brief Helper variable template for is_type_in_pack
 * @tparam T Type to check for
 * @tparam Pack Pack of types to search in
 */
template <typename T, typename... Pack>
constexpr inline bool is_type_in_pack_v = is_type_in_pack<T, Pack...>::value;

/**
 * @brief Extracts the first type from a pack
 * @tparam Pack Pack of types
 */
template <typename... Pack>
struct peel_first : type_identity<void> {};

/**
 * @brief Specialization for non-empty pack
 * @tparam First First type in pack
 * @tparam Pack Rest of types
 */
template <typename First, typename... Pack>
struct peel_first<First, Pack...> : type_identity<First> {};

/**
 * @brief Helper alias for peel_first
 * @tparam Pack Pack of types
 */
template <typename... Pack>
using peel_first_t = typename peel_first<Pack...>::type;

/**
 * @brief Extracts the last type from a pack
 * @tparam Pack Pack of types
 */
template <typename... Pack>
struct peel_last : type_identity<void> {};

/**
 * @brief Recursive case for peel_last
 * @tparam First First type in pack
 * @tparam Pack Rest of types
 */
template <typename First, typename... Pack>
struct peel_last<First, Pack...>
    : type_identity<typename peel_last<Pack...>::type> {};

/**
 * @brief Base case for peel_last
 * @tparam Last Last type in pack
 */
template <typename Last>
struct peel_last<Last> : type_identity<Last> {};

/**
 * @brief Helper alias for peel_last
 * @tparam Pack Pack of types
 */
template <typename... Pack>
using peel_last_t = typename peel_last<Pack...>::type;

/**
 * @brief Checks if all types in a pack are the same
 * @tparam Pack Pack of types to check
 */
template <typename... Pack>
struct is_pack_uniform {};

/**
 * @brief Specialization for non-empty pack
 * @tparam T First type in pack
 * @tparam Pack Rest of types to compare
 */
template <typename T, typename... Pack>
struct is_pack_uniform<T, Pack...>
    : std::bool_constant<(std::is_same_v<T, Pack> && ...)> {};

/**
 * @brief Helper variable template for is_pack_uniform
 * @tparam Pack Pack of types to check
 */
template <typename... Pack>
constexpr inline bool is_pack_uniform_v = is_pack_uniform<Pack...>::value;

/**
 * @brief Checks if pack contains only the given type T
 * @tparam T Type to check for
 * @tparam Pack Pack of types
 */
template <typename T, typename... Pack>
struct is_pack_only : std::conjunction<is_pack_uniform<Pack...>,
                                       std::is_same<T, peel_first_t<Pack...>>> {
};

/**
 * @brief Specialization for empty pack
 * @tparam T Type to check for
 */
template <typename T>
struct is_pack_only<T> : std::false_type {};

/**
 * @brief Helper variable template for is_pack_only
 * @tparam T Type to check for
 * @tparam Pack Pack of types
 */
template <typename T, typename... Pack>
constexpr inline bool is_pack_only_v = is_pack_only<T, Pack...>::value;

}  // namespace core::meta::type_traits