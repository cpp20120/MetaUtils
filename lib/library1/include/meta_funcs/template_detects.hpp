#pragma once

#include <type_traits>

#include "type_utility.hpp"

namespace core::meta::type_traits {

/**
 * @brief Function to determine if a type is a template.
 * @tparam T The type to check.
 * @return false for non-template types.
 */
template <typename T>
constexpr bool is_template() {
  return false;
}

/**
 * @brief Function to determine if a type is a template.
 * @tparam T The template to check.
 * @return true for template types.
 */
template <template <auto...> typename T>
constexpr bool is_template() {
  return true;
}

/**
 * @brief Function to determine if a type is a template.
 * @tparam T The template to check.
 * @return true for template types.
 */
template <template <typename...> typename T>
constexpr bool is_template() {
  return true;
}

/**
 * @brief Function to determine if a type is a template.
 * @tparam T The template to check.
 * @return true for template types.
 */
template <template <typename, auto...> typename T>
constexpr bool is_template() {
  return true;
}

/**
 * @brief Function to determine if a type is a template.
 * @tparam T The template to check.
 * @return true for template types.
 */
template <template <template <auto...> typename...> typename T>
constexpr bool is_template() {
  return true;
}

/**
 * @brief Function to determine if a type is a template.
 * @tparam T The template to check.
 * @return true for template types.
 */
template <template <template <typename...> typename...> typename T>
constexpr bool is_template() {
  return true;
}

/**
 * @brief Function to determine if a type is a template.
 * @tparam T The template to check.
 * @return true for template types.
 */
template <template <template <typename, auto...> typename...> typename T>
constexpr bool is_template() {
  return true;
}

/**
 * @brief Checks if type T is a variadic template with type parameters
 */
template <typename T>
struct is_variadic_type : std::false_type {};

/**
 * @brief Specialization for variadic type templates
 */
template <template <typename...> typename T, typename... Args>
struct is_variadic_type<T<Args...>> : std::true_type {};

/**
 * @brief Helper alias for is_variadic_type::type
 */
template <typename T>
using is_variadic_type_t = typeof_t<is_variadic_type<T>>;

/**
 * @brief Helper variable template for is_variadic_type::value
 */
template <typename T>
inline constexpr auto is_variadic_type_v = typev<is_variadic_type_t<T>>;

/**
 * @brief Checks if type T is a variadic template with value parameters
 */
template <typename T>
struct is_variadic_value : std::false_type {};

/**
 * @brief Specialization for value-only variadic templates
 */
template <template <auto...> typename T, auto... Args>
struct is_variadic_value<T<Args...>> : std::true_type {};

/**
 * @brief Specialization for mixed type/value variadic templates
 */
template <template <typename, auto...> typename T, typename U, auto... Args>
struct is_variadic_value<T<U, Args...>> : std::true_type {};

/**
 * @brief Helper alias for is_variadic_value::type
 */
template <typename T>
using is_variadic_value_t = typeof_t<is_variadic_value<T>>;

/**
 * @brief Helper variable template for is_variadic_value::value
 */
template <typename T>
inline constexpr auto is_variadic_value_v = typev<is_variadic_value_t<T>>;

/**
 * @brief Checks if type T is any kind of variadic template
 */
template <typename T>
struct is_variadic : bool_<is_variadic_type_v<T> || is_variadic_value_v<T>> {};

/**
 * @brief Helper alias for is_variadic::type
 */
template <typename T>
using is_variadic_t = typeof_t<is_variadic<T>>;

/**
 * @brief Helper variable template for is_variadic::value
 */
template <typename T>
inline constexpr auto is_variadic_v = typev<is_variadic_t<T>>;

/**
 * @brief Checks if all types in Args are variadic type templates
 */
template <typename... Args>
struct is_variadic_type_pack : bool_<(is_variadic_type_v<Args> && ...)> {};

/**
 * @brief Helper alias for is_variadic_type_pack::type
 */
template <typename... Args>
using is_variadic_type_pack_t = typeof_t<is_variadic_type_pack<Args...>>;

/**
 * @brief Helper variable template for is_variadic_type_pack::value
 */
template <typename... Args>
inline constexpr auto is_variadic_type_pack_v =
    typev<is_variadic_type_pack_t<Args...>>;

/**
 * @brief Checks if all types in Args are variadic value templates
 */
template <typename... Args>
struct is_variadic_value_pack : bool_<(is_variadic_value_v<Args> && ...)> {};

/**
 * @brief Helper alias for is_variadic_value_pack::type
 */
template <typename... Args>
using is_variadic_value_pack_t = typeof_t<is_variadic_value_pack<Args...>>;

/**
 * @brief Helper variable template for is_variadic_value_pack::value
 */
template <typename... Args>
inline constexpr auto is_variadic_value_pack_v =
    typev<is_variadic_value_pack_t<Args...>>;

/**
 * @brief Checks if all types in Args are any kind of variadic templates
 */
template <typename... Args>
struct is_variadic_pack : bool_<is_variadic_type_pack_v<Args...> ||
                                is_variadic_value_pack_v<Args...>> {};

/**
 * @brief Helper alias for is_variadic_pack::type
 */
template <typename... Args>
using is_variadic_pack_t = typeof_t<is_variadic_pack<Args...>>;

/**
 * @brief Helper variable template for is_variadic_pack::value
 */
template <typename... Args>
inline constexpr auto is_variadic_pack_v = typev<is_variadic_pack_t<Args...>>;

/**
 * @brief Checks if type Args is an instantiation of template T
 * @tparam T Template to check against
 * @tparam Args Type to test
 */
template <template <typename...> typename T, typename Args>
struct is_instance_of : std::false_type {};

/**
 * @brief Specialization for matching template instantiations
 */
template <template <typename...> typename T, typename... Args>
struct is_instance_of<T, T<Args...>> : std::true_type {};

/**
 * @brief Helper variable template for is_instance_of::value
 */
template <template <typename...> typename T, typename Args>
inline constexpr auto is_instance_of_v = typev<is_instance_of<T, Args>>;

/**
 * @brief Checks if type Args is a sequence type (like integer_sequence)
 * @tparam T Sequence template to check against
 * @tparam Args Type to test
 */
template <template <typename, auto...> typename T, typename Args>
struct is_sequence_of : std::false_type {};

/**
 * @brief Specialization for sequence types
 */
template <template <typename, auto...> typename T, typename U, auto... values>
struct is_sequence_of<T, T<U, values...>> : std::true_type {};

/**
 * @brief Helper variable template for is_sequence_of::value
 */
template <template <typename, auto...> typename T, typename Args>
inline constexpr auto is_sequence_of_v = typev<is_sequence_of<T, Args>>;

/**
 * @brief Checks if type T is a sequence (integer_sequence or similar)
 */
template <typename T>
struct is_sequence : is_sequence_of<std::integer_sequence, T> {};

/**
 * @brief Helper variable template for is_sequence::value
 */
template <typename T>
inline constexpr auto is_sequence_v = typev<is_sequence<T>>;

/**
 * @brief Checks if a type is a specialization of a template
 * @tparam _Type Type to check
 * @tparam _Template Template to check against
 */
template <class _Type, template <class...> class _Template>
constexpr bool is_specialization_v = false;

/**
 * @brief Specialization for template matches
 */
template <template <class...> class _Template, class... _Types>
constexpr bool is_specialization_v<_Template<_Types...>, _Template> = true;

/**
 * @brief Type trait for template specialization check
 */
template <class _Type, template <class...> class _Template>
struct is_specialization
    : bool_constant<is_specialization_v<_Type, _Template>> {};

}  // namespace core::meta::type_traits