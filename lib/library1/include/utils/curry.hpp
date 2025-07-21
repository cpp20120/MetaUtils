#pragma once

#include <functional>
#include <print>
#include <tuple>
#include <type_traits>
#include <utility>

namespace core::meta::utils {

/**
 * @class curried
 * @brief A class template that implements function currying - transforming a
 * function taking multiple arguments into a sequence of functions each taking a
 * single argument.
 *
 * @tparam F The type of the callable object to be curried
 * @tparam BoundArgs The types of the arguments already bound to the function
 *
 * This class stores a function and already bound arguments, and provides
 * operator() to either:
 * 1. Invoke the function if enough arguments are provided
 * 2. Return a new curried object with additional arguments bound
 */
template <typename F, typename... BoundArgs>
class curried {
  F func;                          ///< The callable object to be curried
  std::tuple<BoundArgs...> bound;  ///< Tuple of already bound arguments

  /**
   * @brief Helper function to invoke the stored function with bound and new
   * arguments
   *
   * @tparam Args Types of additional arguments
   * @tparam Is Index sequence for unpacking the bound arguments tuple
   * @param f The callable object (forwarded)
   * @param tup Tuple of bound arguments
   * @param seq Index sequence for tuple unpacking
   * @param args Additional arguments to pass to the function
   * @return Result of invoking the function with all arguments
   *
   * @note This is noexcept when the function invocation is noexcept
   */
  template <typename... Args, std::size_t... Is>
  static constexpr decltype(auto) invoke_impl(
      F&& f, std::tuple<BoundArgs...>& tup, std::index_sequence<Is...>,
      Args&&... args) noexcept(std::is_nothrow_invocable_v<F, BoundArgs...,
                                                           Args...>) {
    return std::invoke(std::forward<F>(f), std::get<Is>(tup)...,
                       std::forward<Args>(args)...);
  }

 public:
  /**
   * @brief Construct a new curried object
   *
   * @param f The callable object to curry
   * @param args Initial arguments to bind
   *
   * @note Constructor is noexcept if both F and all BoundArgs are nothrow
   * move-constructible
   */
  constexpr explicit curried(F f, BoundArgs... args) noexcept(
      std::is_nothrow_move_constructible_v<F> &&
      (std::is_nothrow_move_constructible_v<BoundArgs> && ...))
      : func(std::move(f)), bound(std::make_tuple(std::move(args)...)) {}

  /**
   * @brief Const lvalue overload of function call operator for complete
   * invocation
   *
   * @tparam Args Types of additional arguments
   * @param args Additional arguments to pass to the function
   * @return Result of function invocation
   *
   * @note This overload is only available when the function can be invoked with
   * the current bound arguments and provided arguments
   */
  template <typename... Args>
  constexpr decltype(auto) operator()(Args&&... args) const&
    requires std::is_invocable_v<const F&, const BoundArgs&..., Args...>
  {
    return invoke_impl(func, const_cast<std::tuple<BoundArgs...>&>(bound),
                       std::index_sequence_for<BoundArgs...>{},
                       std::forward<Args>(args)...);
  }

  /**
   * @brief Rvalue overload of function call operator for complete invocation
   *
   * @tparam Args Types of additional arguments
   * @param args Additional arguments to pass to the function
   * @return Result of function invocation
   *
   * @note This overload is only available when the function can be invoked with
   * the current bound arguments and provided arguments
   */
  template <typename... Args>
  constexpr decltype(auto) operator()(Args&&... args) &&
    requires std::is_invocable_v<F, BoundArgs..., Args...>
  {
    return invoke_impl(std::move(func), bound,
                       std::index_sequence_for<BoundArgs...>{},
                       std::forward<Args>(args)...);
  }

  /**
   * @brief Const lvalue overload of function call operator for partial
   * application
   *
   * @tparam Args Types of additional arguments
   * @param args Additional arguments to bind
   * @return A new curried object with additional arguments bound
   *
   * @note This overload is only available when the function cannot yet be
   * invoked with the current bound arguments and provided arguments
   */
  template <typename... Args>
  constexpr auto operator()(Args&&... args) const&
    requires(!std::is_invocable_v<const F&, const BoundArgs&..., Args...>)
  {
    return curried<F, BoundArgs..., std::decay_t<Args>...>(
        func, std::get<BoundArgs>(bound)..., std::forward<Args>(args)...);
  }

  /**
   * @brief Rvalue overload of function call operator for partial application
   *
   * @tparam Args Types of additional arguments
   * @param args Additional arguments to bind
   * @return A new curried object with additional arguments bound
   *
   * @note This overload is only available when the function cannot yet be
   * invoked with the current bound arguments and provided arguments
   */
  template <typename... Args>
      constexpr auto operator()(Args&&... args) &&
      requires(!std::is_invocable_v<F, BoundArgs..., Args...>) {
        return curried<F, BoundArgs..., std::decay_t<Args>...>(
            std::move(func), std::move(std::get<BoundArgs>(bound))...,
            std::forward<Args>(args)...);
      }

      /**
       * @brief Const lvalue overload of zero-argument function call operator
       * @return Result of function invocation with just the bound arguments
       *
       * @note This overload is only available when the function can be invoked
       * with just the bound arguments
       */
      constexpr decltype(auto) operator()() const&
        requires std::is_invocable_v<const F&, const BoundArgs&...>
  {
    return invoke_impl(func, const_cast<std::tuple<BoundArgs...>&>(bound),
                       std::index_sequence_for<BoundArgs...>{});
  }

  /**
   * @brief Rvalue overload of zero-argument function call operator
   * @return Result of function invocation with just the bound arguments
   *
   * @note This overload is only available when the function can be invoked with
   * just the bound arguments
   */
  constexpr decltype(auto) operator()() &&
    requires std::is_invocable_v<F, BoundArgs...>
  {
    return invoke_impl(std::move(func), bound,
                       std::index_sequence_for<BoundArgs...>{});
  }
};

/**
 * @brief Creates a curried function object with no initially bound arguments
 *
 * @tparam F Type of the callable object
 * @param f The callable object to curry
 * @return A curried wrapper around f
 *
 * @note This is noexcept if constructing the curried wrapper is noexcept
 */
template <typename F>
constexpr auto curry(F&& f) noexcept(
    std::is_nothrow_constructible_v<std::decay_t<F>, F>) {
  return curried<std::decay_t<F>>(std::forward<F>(f));
}

/**
 * @brief Creates a curried function object with initially bound arguments
 *
 * @tparam F Type of the callable object
 * @tparam Args Types of the arguments to bind initially
 * @param f The callable object to curry
 * @param args Arguments to bind initially
 * @return A curried wrapper around f with args bound
 *
 * @note This is noexcept if constructing the curried wrapper with arguments is
 * noexcept
 * @note The returned object is marked [[nodiscard]] to prevent accidental
 * discarding
 */
[[nodiscard]] template <typename F, typename... Args>
constexpr auto curry(F&& f, Args&&... args) noexcept(
    noexcept(curried<std::decay_t<F>, std::decay_t<Args>...>(
        std::forward<F>(f), std::forward<Args>(args)...))) {
  return curried<std::decay_t<F>, std::decay_t<Args>...>(
      std::forward<F>(f), std::forward<Args>(args)...);
}

}  // namespace core::meta::utils