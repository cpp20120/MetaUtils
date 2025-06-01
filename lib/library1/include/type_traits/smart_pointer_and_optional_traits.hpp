#pragma once

#include <memory>
#include <optional>
#include <type_traits>

#include "../dynamic_optional.hpp"
#include "type_utilities.hpp"

namespace core::meta::type_traits {

namespace impl {
/**
 * @brief Implementation helper for is_tuple
 * @tparam Ts Types to check
 */
template <typename... Ts>
struct is_tuple_impl : std::false_type {};

/**
 * @brief Specialization for std::tuple
 * @tparam Ts Tuple element types
 */
template <typename... Ts>
struct is_tuple_impl<std::tuple<Ts...>> : std::true_type {};
}  // namespace impl

/**
 * @brief Checks if a type is a std::tuple
 * @tparam T Type to check
 */
template <typename T>
struct is_tuple : impl::is_tuple_impl<remove_cvref_t<T>> {};

/**
 * @brief Helper variable template for is_tuple
 * @tparam T Type to check
 */
template <typename T>
constexpr inline bool is_tuple_v = is_tuple<T>::value;

namespace impl {
/**
 * @brief Implementation helper for is_pair
 * @tparam T Type to check
 */
template <typename T>
struct is_pair_impl : std::false_type {};

/**
 * @brief Specialization for std::pair
 * @tparam First First element type
 * @tparam Second Second element type
 */
template <typename First, typename Second>
struct is_pair_impl<std::pair<First, Second>> : std::true_type {};
}  // namespace impl

/**
 * @brief Checks if a type is a std::pair
 * @tparam T Type to check
 */
template <typename T>
struct is_pair : impl::is_pair_impl<remove_cvref_t<T>> {};

/**
 * @brief Helper variable template for is_pair
 * @tparam T Type to check
 */
template <typename T>
constexpr inline bool is_pair_v = is_pair<T>::value;

/**
 * @brief Checks if a type is a smart pointer
 * @tparam T Type to check
 */
template <typename T>
struct is_smart_pointer : std::false_type {};

/**
 * @brief Specialization for std::unique_ptr
 * @tparam T Element type
 */
template <typename T>
struct is_smart_pointer<std::unique_ptr<T>> : std::true_type {};

/**
 * @brief Specialization for std::unique_ptr with custom deleter
 * @tparam T Element type
 * @tparam U Deleter type
 */
template <typename T, typename U>
struct is_smart_pointer<std::unique_ptr<T, U>> : std::true_type {};

/**
 * @brief Specialization for std::shared_ptr
 * @tparam T Element type
 */
template <typename T>
struct is_smart_pointer<std::shared_ptr<T>> : std::true_type {};

/**
 * @brief Specialization for std::weak_ptr
 * @tparam T Element type
 */
template <typename T>
struct is_smart_pointer<std::weak_ptr<T>> : std::true_type {};

/**
 * @brief Helper variable template for is_smart_pointer
 * @tparam T Type to check
 */
template <typename T>
constexpr inline bool is_smart_pointer_v = is_smart_pointer<T>::value;

/**
 * @brief Checks if a type is std::optional
 * @tparam T Type to check
 */
template <class T>
struct is_optional : std::false_type {};

/**
 * @brief Specialization for std::optional
 * @tparam T Optional value type
 */
template <class T>
struct is_optional<std::optional<T>> : std::true_type {};

/**
 * @brief Helper variable template for is_optional
 * @tparam T Type to check
 */
template <class T>
inline constexpr bool is_optional_v = is_optional<T>::value;

/**
 * @brief Checks if a type is dynamic_optional
 * @tparam T Type to check
 */
template <class T>
struct is_dynamic_optional : std::false_type {};

/**
 * @brief Specialization for dynamic_optional
 * @tparam T Optional value type
 */
template <class T>
struct is_dynamic_optional<::core::meta::dynamic_optional::dynamic_optional<T>>
    : std::true_type {};

/**
 * @brief Helper variable template for is_dynamic_optional
 * @tparam T Type to check
 */
template <class T>
inline constexpr bool is_dynamic_optional_v = is_dynamic_optional<T>::value;

}  // namespace core::meta::type_traits