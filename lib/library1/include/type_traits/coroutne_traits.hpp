#pragma once

#include <coroutine>
#include <type_traits>

#include "type_utilities.hpp"

namespace core::meta::type_traits {

/**
 * @brief Checks if a type is a coroutine handle
 * @tparam Type Type to check
 */
template <typename Type>
struct is_coroutine_handle : std::false_type {};

/**
 * @brief Specialization for coroutine handles
 * @tparam Promise Promise type
 */
template <typename Promise>
struct is_coroutine_handle<std::coroutine_handle<Promise>> : std::true_type {};

/**
 * @brief Checks if a type is a valid await_suspend return type
 * @tparam Type Type to check
 */
template <typename Type>
struct is_valid_await_suspend_return_type
    : std::disjunction<std::is_void<Type>, std::is_same<Type, bool>,
                       is_coroutine_handle<Type>> {};

/**
 * @brief Checks if a type has a valid await_suspend method
 * @tparam Type Type to check
 */
template <typename Type>
struct is_valid_await_suspend : std::false_type {};

/**
 * @brief Specialization for coroutine handles
 * @tparam Promise Promise type
 */
template <typename Promise>
struct is_valid_await_suspend_return_type<std::coroutine_handle<Promise>>
    : std::true_type {};

/**
 * @brief Checks if a type has an await_suspend method with valid return type
 * @tparam Type Type to check
 */
template <typename Type>
using is_await_suspend_method = is_valid_await_suspend_return_type<
    decltype(std::declval<Type>().await_suspend(
        std::declval<std::coroutine_handle<>>()))>;

/**
 * @brief Checks if a type has an await_ready method returning bool
 * @tparam Type Type to check
 */
template <typename Type>
using is_await_ready_method =
    std::is_constructible<bool, decltype(std::declval<Type>().await_ready())>;

/**
 * @brief Checks if a type is awaitable
 * @tparam Type Type to check
 */
template <typename Type, typename = std::void_t<>>
struct is_awaitable : std::false_type {};

/**
 * @brief Specialization for awaitable types
 * @tparam Type Awaitable type
 */
template <typename Type>
struct is_awaitable<Type,
                    std::void_t<decltype(std::declval<Type>().await_ready()),
                                decltype(std::declval<Type>().await_suspend(
                                    std::declval<std::coroutine_handle<>>())),
                                decltype(std::declval<Type>().await_resume())>>
    : std::conjunction<is_await_ready_method<Type>,
                       is_await_suspend_method<Type>> {};

/**
 * @brief Helper variable template for is_awaitable
 * @tparam Type Type to check
 */
template <typename Type>
constexpr bool is_awaitable_v = is_awaitable<Type>::value;

}  // namespace core::meta::type_traits