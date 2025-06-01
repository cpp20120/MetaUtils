#pragma once

#include <type_traits>

#include "type_utilityx.hpp"

namespace core::meta::type_traits {

/**
 * @brief Template struct to determine if a type has a nested type.
 * @tparam T The type to check.
 */
template <typename T, typename = std::void_t<>>
struct has_type : std::false_type {};

/**
 * @brief Specialization of has_type for types with a nested type.
 * @tparam T The type to check.
 */
template <typename T>
struct has_type<T, std::void_t<typename T::type>> : std::true_type {};

/**
 * @brief Extracts the value from a type with a nested type.
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr auto has_type_v = typev<has_type<T>>;

/**
 * @brief Template struct to determine if a type has a nested value_type.
 * @tparam T The type to check.
 */
template <typename T, typename = std::void_t<>>
struct has_value_type : std::false_type {
  using value_type = int;
};

/**
 * @brief Specialization of has_value_type for types with a nested value_type.
 * @tparam T The type to check.
 */
template <typename T>
struct has_value_type<T, std::void_t<value_t<T>>> : std::true_type {
  using value_type = value_t<T>;
};

/**
 * @brief Template alias for extracting the nested value_type from a type.
 * @tparam T The type to check.
 */
template <typename T>
using has_value_type_t = value_t<has_value_type<T>>;

/**
 * @brief Extracts the value from a type with a nested value_type.
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr auto has_value_type_v = typev<has_value_type<T>>;

/**
 * @brief Template struct to determine if a type has a custom operator new.
 * @tparam T The type to check.
 */
template <typename T, typename = std::void_t<>>
struct has_new : std::false_type {};

/**
 * @brief Specialization of has_new for types with a custom operator new.
 * @tparam T The type to check.
 */
template <typename T>
struct has_new<T, std::void_t<decltype(T::operator new(0))>> : std::true_type {
};

/**
 * @brief Extracts the value from a type with a custom operator new.
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr auto has_new_v = typev<has_new<T>>;

/**
 * @brief Template struct to determine if a type has a custom operator delete.
 * @tparam T The type to check.
 */
template <typename T, typename = std::void_t<>>
struct has_delete : std::false_type {};

/**
 * @brief Specialization of has_delete for types with a custom operator delete.
 * @tparam T The type to check.
 */
template <typename T>
struct has_delete<T, std::void_t<decltype(T::operator delete(nullptr))>>
    : std::true_type {};

/**
 * @brief Extracts the value from a type with a custom operator delete.
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr auto has_delete_v = typev<has_delete<T>>;

/**
 * @brief Template struct to determine if a type is complete.
 * @tparam T The type to check.
 */
template <typename T, typename = std::void_t<>>
struct is_type_complete : std::false_type {};

/**
 * @brief Specialization of is_type_complete for complete types.
 * @tparam T The type to check.
 */
template <typename T>
struct is_type_complete<T, std::void_t<decltype(sizeof(T))>> : std::true_type {
};

/**
 * @brief Extracts the value from a type that is complete.
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr auto is_type_complete_v = typev<is_type_complete<T>>;

/**
 * @brief Template struct to determine if a type is a base template of another
 * type.
 * @tparam B The base template to check.
 * @tparam T The type to check.
 */
template <template <typename...> typename B, typename T,
          typename = std::void_t<>>
struct is_base_template_of : std::false_type {};

/**
 * @brief Specialization of is_base_template_of for types that are base
 * templates.
 * @tparam B The base template to check.
 * @tparam T The type to check.
 */
template <template <typename...> typename B, typename T>
struct is_base_template_of<
    B, T, std::void_t<decltype([]<typename... Args>(B<Args...> *) {
    }(std::declval<T *>()))>> : std::true_type {};

/**
 * @brief Extracts the value from a type that is a base template of another
 * type.
 * @tparam B The base template to check.
 * @tparam T The type to check.
 */
template <template <typename...> typename B, typename T>
inline constexpr auto is_base_template_of_v = typev<is_base_template_of<B, T>>;

}  // namespace core::meta::type_traits