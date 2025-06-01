#pragma once

#include <iterator>
#include <type_traits>

#include "type_utilities.hpp"

namespace core::meta::type_traits {

/**
 * @brief Checks if a type is iterable (has begin() and end())
 * @tparam T Type to check
 */
template <typename T, typename = void>
struct is_iterable : std::false_type {};

/**
 * @brief Specialization for iterable types
 * @tparam T Iterable type
 */
template <typename T>
struct is_iterable<T, std::void_t<decltype(std::begin(std::declval<T &>())),
                                  decltype(std::end(std::declval<T &>()))>>
    : std::true_type {};

/**
 * @brief Helper variable template for is_iterable
 * @tparam T Type to check
 */
template <typename T>
constexpr inline bool is_iterable_v = is_iterable<T>::value;

/**
 * @brief Checks if a type is an iterator (has * and ++ operators)
 * @tparam T Type to check
 */
template <typename T, typename = void>
struct is_iterator : std::false_type {};

/**
 * @brief Specialization for iterator types
 * @tparam T Iterator type
 */
template <typename T>
struct is_iterator<T, std::void_t<decltype(*std::declval<T>()),
                                  decltype(++std::declval<T &>())>>
    : std::true_type {};

/**
 * @brief Helper variable template for is_iterator
 * @tparam T Type to check
 */
template <typename T>
constexpr inline bool is_iterator_v = is_iterator<T>::value;

/**
 * @brief Checks if an iterator is a forward iterator
 * @tparam T Iterator type to check
 */
template <typename T>
struct is_forward
    : std::is_base_of<std::forward_iterator_tag,
                      typename std::iterator_traits<T>::iterator_category> {};

/**
 * @brief Helper variable template for is_forward
 * @tparam T Iterator type to check
 */
template <typename T>
constexpr inline bool is_forward_v = is_forward<T>::value;

/**
 * @brief Checks if an iterator is a bidirectional iterator
 * @tparam T Iterator type to check
 */
template <typename T>
struct is_bidirectional
    : std::is_base_of<std::bidirectional_iterator_tag,
                      typename std::iterator_traits<T>::iterator_category> {};

/**
 * @brief Helper variable template for is_bidirectional
 * @tparam T Iterator type to check
 */
template <typename T>
constexpr inline bool is_bidirectional_v = is_bidirectional<T>::value;

/**
 * @brief Checks if an iterator is a random access iterator
 * @tparam T Iterator type to check
 */
template <typename T>
struct is_random_access
    : std::is_base_of<std::random_access_iterator_tag,
                      typename std::iterator_traits<T>::iterator_category> {};

/**
 * @brief Helper variable template for is_random_access
 * @tparam T Iterator type to check
 */
template <typename T>
constexpr inline bool is_random_access_v = is_random_access<T>::value;

/**
 * @brief Checks if type T is a standard container
 */
template <typename T, typename = std::void_t<>>
struct is_container : std::false_type {};

/**
 * @brief Specialization for container types
 */
template <typename T>
struct is_container<
    T,
    std::void_t<typename T::value_type, typename T::size_type,
                typename T::allocator_type, typename T::iterator,
                typename T::const_iterator, decltype(std::declval<T>().size()),
                decltype(std::declval<T>().begin()),
                decltype(std::declval<T>().end()),
                decltype(std::declval<T>().cbegin()),
                decltype(std::declval<T>().cend())>> : std::true_type {};

/**
 * @brief Helper variable template for is_container::value
 */
template <typename T>
inline constexpr auto is_container_v = typev<is_container<T>>;

}  // namespace core::meta::type_traits