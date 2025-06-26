/**
 * @file fold_operations.hpp
 * @brief Provides fold operations for variadic arguments and tuples
 */

#pragma once

#include <type_traits>
#include <utility>
namespace core::meta::utils {
/**
 * @brief A helper struct for fold operations
 * @tparam F The function type to apply
 * @tparam T The value type to fold
 */
template <typename F, typename T>
struct foldable {
  /**
   * @brief Combines two foldable objects using the stored function
   * @tparam U The type of the right-hand foldable's value
   * @param r The right-hand foldable to combine with
   * @return A new foldable with the combined result
   */
  template <typename U>
  constexpr decltype(auto) operator*(foldable<F, U> &&r) {
    return foldable{f, f(std::move(t), std::move(r.t))};
  }

  F f;  ///< The function to apply
  T t;  ///< The value to fold
};

/**
 * @brief Left-fold over variadic arguments
 * @tparam F The function type
 * @tparam Args The argument types
 * @param f The function to apply
 * @param args The arguments to fold
 * @return The result of folding all arguments from left to right
 */
template <typename F, typename... Args>
  requires(sizeof...(Args) > 0)
constexpr decltype(auto) foldl_over(F &&f, Args &&...args) {
  return (... * foldable{std::forward<F>(f), std::forward<Args>(args)}).t;
}

/**
 * @brief Right-fold over variadic arguments
 * @tparam F The function type
 * @tparam Args The argument types
 * @param f The function to apply
 * @param args The arguments to fold
 * @return The result of folding all arguments from right to left
 */
template <typename F, typename... Args>
  requires(sizeof...(Args) > 0)
constexpr decltype(auto) foldr_over(F &&f, Args &&...args) {
  return (foldable{std::forward<F>(f), std::forward<Args>(args)} * ...).t;
}

/**
 * @brief Creates a left-fold operation
 * @tparam F The function type
 * @param f The function to apply
 * @return A lambda that performs left-folding when called with arguments
 */
template <typename F>
constexpr decltype(auto) foldl_over(F &&f) {
  return [&]<typename... Args>(Args &&...args) mutable {
    return foldl_over(std::forward<F>(f), std::forward<Args>(args)...);
  };
}

/**
 * @brief Creates a right-fold operation
 * @tparam F The function type
 * @param f The function to apply
 * @return A lambda that performs right-folding when called with arguments
 */
template <typename F>
constexpr decltype(auto) foldr_over(F &&f) {
  return [&]<typename... Args>(Args &&...args) mutable {
    return foldr_over(std::forward<F>(f), std::forward<Args>(args)...);
  };
}

/**
 * @brief Left-fold over a tuple's elements
 * @tparam F The function type
 * @tparam T The tuple type
 * @param f The function to apply
 * @param t The tuple to fold
 * @return The result of folding all tuple elements from left to right
 */
template <typename F, typename T>
constexpr decltype(auto) foldl_apply(F &&f, T &&t) {
  return std::apply(foldl_over(std::forward<F>(f)), std::forward<T>(t));
}

/**
 * @brief Right-fold over a tuple's elements
 * @tparam F The function type
 * @tparam T The tuple type
 * @param f The function to apply
 * @param t The tuple to fold
 * @return The result of folding all tuple elements from right to left
 */
template <typename F, typename T>
constexpr decltype(auto) foldr_apply(F &&f, T &&t) {
  return std::apply(foldr_over(std::forward<F>(f)), std::forward<T>(t));
}

/**
 * @brief Helper function to create a foldable object
 * @tparam F The function type
 * @tparam T The value type
 * @param f The function to store
 * @param t The value to store
 * @return A foldable object containing the function and value
 */
template <typename F, typename T>
constexpr auto make_foldable(F &&f, T &&t) {
  return foldable<std::decay_t<F>, std::decay_t<T>>{std::forward<F>(f),
                                                    std::forward<T>(t)};
}
}  // namespace core::meta::utils