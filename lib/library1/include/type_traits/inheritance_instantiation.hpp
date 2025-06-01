#pragma once

#include <type_traits>

#include "type_utilities.hpp"

namespace core::meta::type_traits {

/**
 * @brief Template struct for inheriting from multiple types.
 * @tparam Args The types to inherit from.
 */
template <typename... Args>
struct inherit : Args... {};

/**
 * @brief Template struct to determine if a type is inheritable.
 * @tparam T The type to check.
 */
template <typename T, typename = std::void_t<>>
struct is_inheritable : std::false_type {};

/**
 * @brief Specialization of is_inheritable for inheritable types.
 * @tparam T The type to check.
 */
template <typename T>
struct is_inheritable<T, std::void_t<inherit<T>>> : std::true_type {};

/**
 * @brief Extracts the value from a type that is inheritable.
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr auto is_inheritable_v = typev<is_inheritable<T>>;

/**
 * @brief Template struct to determine if a pack of types is inheritable.
 * @tparam Args The types to check.
 */
template <typename... Args>
struct is_inheritable_pack : bool_<(is_inheritable_v<Args> && ...)> {};

/**
 * @brief Extracts the value from a type that is inheritable in a pack of types.
 * @tparam Args The types to check.
 */
template <typename... Args>
inline constexpr auto is_inheritable_pack_v =
    typev<is_inheritable_pack<Args...>>;

/**
 * @brief Template struct to determine if a type is instantiable.
 * @tparam T The type to check.
 */
template <typename T>
struct is_instantiable : std::negation<std::is_abstract<T>> {};

/**
 * @brief Extracts the value from a type that is instantiable.
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr auto is_instantiable_v = typev<is_instantiable<T>>;

}  // namespace core::meta::type_traits