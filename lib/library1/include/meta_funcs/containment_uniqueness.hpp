#pragma once

#include <type_traits>

#include "../constants.hpp"
#include "type_utility.hpp"

namespace core::meta::meta_funcs {

/**
 * @brief Template struct to determine if a type is contained in a list of
 * types.
 * @tparam Args The types to check.
 */
template <typename... Args>
struct contains : std::false_type {};

/**
 * @brief Specialization of contains for types that are contained.
 * @tparam T The type to check.
 * @tparam Args The types to check.
 */
template <typename T, typename... Args>
struct contains<T, Args...> : bool_<(std::is_same_v<T, Args> || ...)> {};

/**
 * @brief Extracts the value from a type that is contained in a list of types.
 * @tparam Args The types to check.
 */
template <typename...>
inline constexpr auto contains_v = std::false_type{};

/**
 * @brief Extracts the value from a type that is contained in a list of types.
 * @tparam T The type to check.
 * @tparam Args The types to check.
 */
template <typename T, typename... Args>
inline constexpr auto contains_v<T, Args...> = (std::is_same_v<T, Args> || ...);

/**
 * @brief Template struct to determine if a list of values comprises a specific
 * value.
 * @tparam values The values to check.
 */
template <auto... values>
struct comprise : std::false_type {};

/**
 * @brief Specialization of comprise for values that are comprised.
 * @tparam value The value to check.
 * @tparam values The values to check.
 */
template <auto value, auto... values>
struct comprise<value, values...> : bool_<((value == values) || ...)> {};

/**
 * @brief Extracts the value from a type that comprises a specific value.
 * @tparam values The values to check.
 */
template <auto...>
inline constexpr auto comprise_v = std::false_type{};

/**
 * @brief Extracts the value from a type that comprises a specific value.
 * @tparam value The value to check.
 * @tparam values The values to check.
 */
template <auto value, auto... values>
inline constexpr auto comprise_v<value, values...> = ((value == values) || ...);

/**
 * @brief Template struct to determine if a type exists in a list of types.
 * @tparam B The base type.
 * @tparam Args The types to check.
 */
template <typename B, typename...>
struct exists_type : B {};

/**
 * @brief Specialization of exists_type for types that exist.
 * @tparam B The base type.
 * @tparam T The type to check.
 * @tparam Args The types to check.
 */
template <typename B, typename T, typename... Args>
struct exists_type<B, T, Args...>
    : std::conditional_t<contains_v<T, Args...>, std::negation<B>,
                         exists_type<B, Args...>> {};

/**
 * @brief Template alias for extracting the type from an exists_type.
 * @tparam B The base type.
 * @tparam Args The types to check.
 */
template <typename B, typename... Args>
using exists_type_t = typeof_t<exists_type<B, Args...>>;

/**
 * @brief Extracts the value from a type that exists in a list of types.
 * @tparam B The base type.
 * @tparam Args The types to check.
 */
template <typename B, typename... Args>
inline constexpr auto exists_type_v = typev<exists_type_t<B, Args...>>;

/**
 * @brief Template alias for determining if a list of types is unique.
 * @tparam Args The types to check.
 */
template <typename... Args>
using is_unique_type = exists_type<std::true_type, Args...>;

/**
 * @brief Extracts the value from a type that is unique in a list of types.
 * @tparam Args The types to check.
 */
template <typename...>
inline constexpr auto is_unique_type_v = std::true_type{};

/**
 * @brief Extracts the value from a type that is unique in a list of types.
 * @tparam T The type to check.
 * @tparam Args The types to check.
 */
template <typename T, typename... Args>
inline constexpr auto is_unique_type_v<T, Args...> =
    !contains_v<T, Args...> && is_unique_type_v<Args...>;

/**
 * @brief Template alias for determining if a list of types has duplicates.
 * @tparam Args The types to check.
 */
template <typename... Args>
using has_duplicates_type = exists_type<std::false_type, Args...>;

/**
 * @brief Extracts the value from a type that has duplicates in a list of types.
 * @tparam Args The types to check.
 */
template <typename...>
inline constexpr auto has_duplicates_type_v = std::false_type{};

/**
 * @brief Extracts the value from a type that has duplicates in a list of types.
 * @tparam T The type to check.
 * @tparam Args The types to check.
 */
template <typename T, typename... Args>
inline constexpr auto has_duplicates_type_v<T, Args...> =
    contains_v<T, Args...> || has_duplicates_type_v<Args...>;

/**
 * @brief Template struct to determine if a list of values is unique.
 * @tparam values The values to check.
 */
template <auto... values>
using is_unique_value = exists_value<std::true_type, values...>;

/**
 * @brief Extracts the value from a type that is unique in a list of values.
 * @tparam values The values to check.
 */
template <auto...>
inline constexpr auto is_unique_value_v = std::true_type{};

/**
 * @brief Extracts the value from a type that is unique in a list of values.
 * @tparam value The value to check.
 * @tparam values The values to check.
 */
template <auto value, auto... values>
inline constexpr auto is_unique_value_v<value, values...> =
    negav<comprise<value, values...>> && is_unique_value_v<values...>;

/**
 * @brief Template alias for determining if a list of values has duplicates.
 * @tparam values The values to check.
 */
template <auto... values>
using has_duplicates_value = exists_value<std::false_type, values...>;

/**
 * @brief Extracts the value from a type that has duplicates in a list of
 * values.
 * @tparam values The values to check.
 */
template <auto...>
inline constexpr auto has_duplicates_value_v = std::false_type{};

/**
 * @brief Extracts the value from a type that has duplicates in a list of
 * values.
 * @tparam value The value to check.
 * @tparam values The values to check.
 */
template <auto value, auto... values>
inline constexpr auto has_duplicates_value_v<value, values...> =
    typev<comprise<value, values...>> || has_duplicates_value_v<values...>;

/**
 * @brief Template struct to determine if a type exists based on a boolean
 * condition.
 * @tparam B The boolean condition.
 * @tparam T The type to check.
 */
template <bool B, typename T>
struct exists;

/**
 * @brief Specialization of exists for template types with a boolean condition.
 * @tparam T The template to check.
 * @tparam Args The types to check.
 */
template <template <typename...> typename T, typename... Args>
struct exists<true, T<Args...>> {
  using type = is_unique_type<Args...>;
};

/**
 * @brief Specialization of exists for template types with a boolean condition.
 * @tparam T The template to check.
 * @tparam U The type to check.
 * @tparam values The values to check.
 */
template <template <typename, auto...> typename T, typename U, auto... values>
struct exists<true, T<U, values...>> {
  using type = is_unique_value<values...>;
};

/**
 * @brief Specialization of exists for template types with a boolean condition.
 * @tparam T The template to check.
 * @tparam Args The types to check.
 */
template <template <typename...> typename T, typename... Args>
struct exists<false, T<Args...>> {
  using type = has_duplicates_type<Args...>;
};

/**
 * @brief Specialization of exists for template types with a boolean condition.
 * @tparam T The template to check.
 * @tparam U The type to check.
 * @tparam values The values to check.
 */
template <template <typename, auto...> typename T, typename U, auto... values>
struct exists<false, T<U, values...>> {
  using type = has_duplicates_value<values...>;
};

/**
 * @brief Template alias for extracting the type from an exists.
 * @tparam B The boolean condition.
 * @tparam T The type to check.
 */
template <bool B, typename T>
using exists_t = typeof_t<exists<B, T>>;

/**
 * @brief Extracts the value from a type that exists based on a boolean
 * condition.
 * @tparam B The boolean condition.
 * @tparam T The type to check.
 */
template <bool B, typename T>
inline constexpr auto exists_v = typev<exists_t<B, T>>;

/**
 * @brief Template alias for determining if a type is unique.
 * @tparam T The type to check.
 */
template <typename T>
using is_unique = exists<true, T>;

/**
 * @brief Template alias for extracting the type from an is_unique.
 * @tparam T The type to check.
 */
template <typename T>
using is_unique_t = typeof_t<is_unique<T>>;

/**
 * @brief Extracts the value from a type that is unique.
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr auto is_unique_v = typev<is_unique_t<T>>;

/**
 * @brief Template alias for determining if a type has duplicates.
 * @tparam T The type to check.
 */
template <typename T>
using has_duplicates = exists<false, T>;

/**
 * @brief Template alias for extracting the type from a has_duplicates.
 * @tparam T The type to check.
 */
template <typename T>
using has_duplicates_t = typeof_t<has_duplicates<T>>;

/**
 * @brief Extracts the value from a type that has duplicates.
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr auto has_duplicates_v = typev<has_duplicates_t<T>>;

}  // namespace core::meta::type_traits