#pragma once

#include <ostream>
#include <type_traits>

#include "../meta_functions/meta_functions.hpp"
#include "type_utilities.hpp"

namespace core::meta::type_traits {

/**
 * @brief Checks if a type can be printed to std::ostream
 * @tparam T Type to check
 */
template <typename T, typename = void>
struct is_printable : std::false_type {};

/**
 * @brief Specialization for printable types
 * @tparam T Printable type
 */
template <typename T>
struct is_printable<T, std::void_t<decltype(std::declval<std::ostream &>()
                                            << std::declval<T>())>>
    : std::true_type {};

/**
 * @brief Helper variable template for is_printable
 * @tparam T Type to check
 */
template <typename T>
constexpr inline bool is_printable_v = is_printable<T>::value;

/**
 * @brief Checks if type T can be streamed into type S
 * @tparam S The stream type
 * @tparam T The type to be streamed
 */
template <typename S, typename T, typename = std::void_t<>>
struct is_streamable : std::false_type {};

/**
 * @brief Specialization of is_streamable for streamable types
 */
template <typename S, typename T>
struct is_streamable<
    S, T,
    std::void_t<core::meta::meta_funcs::disable_if_t<std::is_same_v<S, T>>,
                decltype(std::declval<std::add_lvalue_reference_t<S>>()
                         << std::declval<T>())>> : std::true_type {};

/**
 * @brief Helper alias for is_streamable::type
 */
template <typename S, typename T>
using is_streamable_t = typeof_t<is_streamable<S, T>>;

/**
 * @brief Helper variable template for is_streamable::value
 */
template <typename S, typename T>
inline constexpr auto is_streamable_v = typev<is_streamable_t<S, T>>;

/**
 * @brief Checks if two types are equality comparable
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U, typename = void>
struct are_equality_comparable : std::false_type {};

/**
 * @brief Specialization for equality comparable types
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U>
struct are_equality_comparable<
    T, U, std::void_t<decltype(std::declval<T>() == std::declval<U>())>>
    : std::true_type {};

/**
 * @brief Helper variable template for are_equality_comparable
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U>
constexpr inline bool are_equality_comparable_v =
    are_equality_comparable<T, U>::value;

/**
 * @brief Checks if two types are inequality comparable
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U, typename = void>
struct are_inequality_comparable : std::false_type {};

/**
 * @brief Specialization for inequality comparable types
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U>
struct are_inequality_comparable<
    T, U, std::void_t<decltype(std::declval<T>() != std::declval<U>())>>
    : std::true_type {};

/**
 * @brief Helper variable template for are_inequality_comparable
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U>
constexpr inline bool are_inequality_comparable_v =
    are_inequality_comparable<T, U>::value;

/**
 * @brief Checks if two types are less comparable
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U, typename = void>
struct are_less_comparable : std::false_type {};

/**
 * @brief Specialization for less comparable types
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U>
struct are_less_comparable<
    T, U, std::void_t<decltype(std::declval<T>() < std::declval<U>())>>
    : std::true_type {};

/**
 * @brief Helper variable template for are_less_comparable
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U>
constexpr inline bool are_less_comparable_v = are_less_comparable<T, U>::value;

/**
 * @brief Checks if two types are less or equal comparable
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U, typename = void>
struct are_less_eq_comparable : std::false_type {};

/**
 * @brief Specialization for less or equal comparable types
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U>
struct are_less_eq_comparable<
    T, U, std::void_t<decltype(std::declval<T>() <= std::declval<U>())>>
    : std::true_type {};

/**
 * @brief Helper variable template for are_less_eq_comparable
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U>
constexpr inline bool are_less_eq_comparable_v =
    are_less_eq_comparable<T, U>::value;

/**
 * @brief Checks if two types are greater comparable
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U, typename = void>
struct are_greater_comparable : std::false_type {};

/**
 * @brief Specialization for greater comparable types
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U>
struct are_greater_comparable<
    T, U, std::void_t<decltype(std::declval<T>() > std::declval<U>())>>
    : std::true_type {};

/**
 * @brief Helper variable template for are_greater_comparable
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U>
constexpr inline bool are_greater_comparable_v =
    are_greater_comparable<T, U>::value;

/**
 * @brief Checks if two types are greater or equal comparable
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U, typename = void>
struct are_greater_eq_comparable : std::false_type {};

/**
 * @brief Specialization for greater or equal comparable types
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U>
struct are_greater_eq_comparable<
    T, U, std::void_t<decltype(std::declval<T>() >= std::declval<U>())>>
    : std::true_type {};

/**
 * @brief Helper variable template for are_greater_eq_comparable
 * @tparam T First type
 * @tparam U Second type
 */
template <typename T, typename U>
constexpr inline bool are_greater_eq_comparable_v =
    are_greater_eq_comparable<T, U>::value;

}  // namespace core::meta::type_traits