#pragma once

#include <type_traits>

#include "../constants.hpp"

namespace core::meta::type_traits {

/**
 * @brief Template alias for creating a tuple type.
 * @tparam Args The types of the tuple elements.
 */
template <typename... Args>
using tuple_t = std::tuple<Args...>;

/**
 * @brief Template alias for creating an index sequence.
 * @tparam N The indices of the sequence.
 */
template <auto... N>
using is = std::index_sequence<N...>;

/**
 * @brief Extracts the value from a type with a static value member.
 * @tparam T The type with a static value member.
 */
template <typename T>
inline constexpr auto typev = T::value;

/**
 * @brief Template alias for extracting the value type from a type.
 * @tparam T The type with a value_type member.
 */
template <typename T>
using value_t = typename T::value_type;

/**
 * @brief Negates a boolean value.
 * @tparam T The type with a boolean value.
 */
template <typename T>
inline constexpr auto negav = std::negation_v<T>;

/**
 * @brief Template struct to determine if a type has a nested type.
 * @tparam T The type to check.
 */
template <typename T, typename = std::void_t<>>
struct typeof {
  using type = T;
  static constexpr auto value = 0;
};

/**
 * @brief Specialization of typeof for types with a nested type.
 * @tparam T The type to check.
 */
template <typename T>
struct typeof<T, std::void_t<typename T::type>> {
  using type = typename T::type;
  static constexpr auto value = 1;
};

/**
 * @brief Template alias for extracting the nested type from a type.
 * @tparam T The type to check.
 */
template <typename T>
using typeof_t = typename typeof<T>::type;

/**
 * @brief Extracts the value from a type with a nested type.
 * @tparam T The type to check.
 */
template <typename T>
inline constexpr auto typeof_v = typev<typeof_t<T>>;

/**
 * @brief Template alias for selecting a type based on a boolean condition.
 * @tparam B The boolean condition.
 * @tparam T The type to select if the condition is true.
 * @tparam U The type to select if the condition is false.
 */
template <bool B, typename T, typename U>
using type_if = typeof_t<std::conditional_t<B, T, U>>;

/**
 * @brief Extracts the value from a type selected based on a boolean condition.
 * @tparam B The boolean condition.
 * @tparam T The type to select if the condition is true.
 * @tparam U The type to select if the condition is false.
 */
template <bool B, typename T, typename U>
inline constexpr auto type_if_v = typev<type_if<B, T, U>>;

/**
 * @brief Extracts the value from a type selected based on a boolean condition.
 * @tparam B The boolean condition.
 * @tparam T The type to select if the condition is true.
 * @tparam U The type to select if the condition is false.
 */
template <bool B, typename T, typename U>
inline constexpr auto value_if = typev<std::conditional_t<B, T, U>>;

/**
 * @brief Template alias for selecting a type based on a type condition.
 * @tparam T The type condition.
 * @tparam U The type to select if the condition is true.
 * @tparam V The type to select if the condition is false.
 */
template <typename T, typename U, typename V>
using conditional_of = std::conditional_t<typev<T>, U, V>;

/**
 * @brief Identity type that holds the given type T
 * @tparam T The type to hold
 */
template <typename T>
struct type_identity {
  using type = T;
};

/**
 * @brief Removes const, volatile and reference qualifiers from a type
 * @tparam T Type to remove qualifiers from
 */
template <typename T>
using remove_cvref = std::remove_cv<std::remove_reference_t<T>>;

/**
 * @brief Helper alias for remove_cvref
 * @tparam T Type to remove qualifiers from
 */
template <typename T>
using remove_cvref_t = typename remove_cvref<T>::type;

}  // namespace core::meta::type_traits