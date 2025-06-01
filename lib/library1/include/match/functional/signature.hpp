#pragma once

#include <boost/config.hpp>
#include <tuple>
#include <type_traits>

namespace pm {

/**
 * @brief Metafunction for extracting the signature of a callable object.
 *
 * This metafunction specializes for various types of callable objects,
 * including function pointers, method pointers, and functors.
 *
 * @tparam C Type of the callable object.
 */
template <class C>
struct signature : signature<decltype(&std::decay_t<C>::operator())> {};

// Specialization for method pointers
template <class C, typename Result, typename... Args>
struct signature<Result (C::*)(Args...)> : signature<Result(Args...)> {};

// Specialization for const method pointers
template <class C, typename Result, typename... Args>
struct signature<Result (C::*)(Args...) const> : signature<Result(Args...)> {};

// Specialization for volatile method pointers
template <class C, typename Result, typename... Args>
struct signature<Result (C::*)(Args...) volatile> : signature<Result(Args...)> {
};

// Specialization for const volatile method pointers
template <class C, typename Result, typename... Args>
struct signature<Result (C::*)(Args...) const volatile>
    : signature<Result(Args...)> {};

// Specialization for function pointers
template <typename Result, typename... Args>
struct signature<Result (*)(Args...)> : signature<Result(Args...)> {};

// Specialization for function references
template <typename Result, typename... Args>
struct signature<Result (&)(Args...)> : signature<Result(Args...)> {};

/**
 * @brief Represents a function signature.
 *
 * This structure provides compile-time information about function argument
 * types and return type.
 *
 * @tparam Result Function return type.
 * @tparam Args   Function argument types.
 */
template <typename Result, typename... Args>
struct signature<Result(Args...)> {
  /// Number of function arguments
  static constexpr auto num_args = sizeof...(Args);

  /// Function signature type
  typedef Result type(Args...);

  /// Function return type
  using result_type = Result;

  /// Number of function arguments
  static constexpr size_t number_of_arguments = sizeof...(Args);

  /**
   * @brief Represents the type of the nth function argument.
   *
   * @tparam n Argument index.
   */
  template <size_t n>
  using argument = typename std::tuple_element<n, std::tuple<Args...>>;
};

/**
 * @brief Concept for checking if an object can be called with specified
 * arguments.
 *
 * This concept checks if an object can be called with specified arguments
 * and return a result of the expected type.
 *
 * @tparam Callable Type of the callable object.
 */
template <typename Callable>
concept Invocable = requires(Callable&& f) {
  {
    std::forward<Callable>(f)(
        std::declval<
            typename signature<std::decay_t<Callable>>::template argument<0>>())
  } -> std::same_as<typename signature<std::decay_t<Callable>>::result_type>;
};

/**
 * @brief Concept for checking if an object can be called without throwing
 * exceptions.
 *
 * This concept checks if an object can be called with specified arguments
 * without throwing exceptions and return a result of the expected type.
 *
 * @tparam Callable Type of the callable object.
 */
template <typename Callable>
concept NothrowInvocable = requires(Callable&& f) {
  {
    std::forward<Callable>(f)(
        std::declval<
            typename signature<std::decay_t<Callable>>::template argument<0>>())
  } noexcept
      -> std::same_as<typename signature<std::decay_t<Callable>>::result_type>;
};

/**
 * @brief Alias for callable object signature type.
 *
 * This alias provides a simple way to get the signature type of a callable
 * object.
 *
 * @tparam Callable Type of the callable object.
 */
template <typename Callable>
using signature_t = typename signature<std::decay_t<Callable>>::type;

/**
 * @brief Alias for callable object result type.
 *
 * This alias provides a simple way to get the result type of a callable object.
 *
 * @tparam Callable Type of the callable object.
 */
template <typename Callable>
using result_t = typename signature<std::decay_t<Callable>>::result_type;

}  // namespace pm