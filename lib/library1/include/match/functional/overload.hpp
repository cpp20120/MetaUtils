#pragma once

#include <type_traits>
#include <utility>

namespace pm {
/**
 * @brief The overloaded class provides a mechanism for creating objects that
 * overload the () operator.
 *
 * @tparam Lambdas Template parameters that should be lambda expressions or
 * functional objects.
 */
template <typename... Lambdas>
struct overloaded {};

/**
 * @brief Specialization of the overloaded class for multiple template
 * parameters.
 *
 * @tparam Lambda1 First lambda expression or functional object.
 * @tparam Lambdas Remaining lambda expressions or functional objects.
 */
template <typename Lambda1, typename... Lambdas>
struct overloaded<Lambda1, Lambdas...> : public Lambda1,
                                         public overloaded<Lambdas...> {
  using Lambda1::operator();
  using overloaded<Lambdas...>::operator();

  /**
   * @brief Constructor initializing the overloaded object with passed lambda
   * expressions or functional objects.
   *
   * @tparam UniLambda1 Type of the first passed lambda expression or functional
   * object.
   * @tparam UniLambdas Types of the remaining passed lambda expressions or
   * functional objects.
   * @param l1 First lambda expression or functional object.
   * @param lambdas Remaining lambda expressions or functional objects.
   */
  template <typename UniLambda1, typename... UniLambdas>
  overloaded(UniLambda1&& l1, UniLambdas&&... lambdas)
      : Lambda1{std::forward<UniLambda1>(l1)},
        overloaded<Lambdas...>{std::forward<UniLambdas>(lambdas)...} {}
};

/**
 * @brief Specialization of the overloaded class for a single template
 * parameter.
 *
 * @tparam Lambda1 Single lambda expression or functional object.
 */
template <typename Lambda1>
struct overloaded<Lambda1> : public Lambda1 {
  using Lambda1::operator();

  /**
   * @brief Constructor initializing the overloaded object with a passed lambda
   * expression or functional object.
   *
   * @tparam UniLambda1 Type of the passed lambda expression or functional
   * object.
   * @param l1 Lambda expression or functional object.
   */
  template <typename UniLambda1, typename = std::enable_if_t<std::is_same<
                                     Lambda1, std::decay_t<UniLambda1>>::value>>
  overloaded(UniLambda1&& l1) : Lambda1{std::forward<UniLambda1>(l1)} {}
};

namespace detail {
/**
 * @brief Template structure free_function_wrapper wraps free functions in
 * lambda expressions.
 */
template <typename Function>
struct free_function_wrapper {
  /**
   * @brief Applies the passed free function.
   *
   * @tparam F Type of the passed function.
   * @param f Free function.
   * @return decltype(auto) Function return value.
   */
  template <typename F>
  static decltype(auto) apply(F&& f) {
    return std::forward<F>(f);
  }
};

/**
 * @brief Partial specialization of free_function_wrapper for pointers to free
 * functions.
 */
template <typename R, typename... Args>
struct free_function_wrapper<R (*)(Args...)> {
  /**
   * @brief Applies the passed pointer to a free function.
   *
   * @tparam F Type of the passed pointer to a free function.
   * @param f Pointer to a free function.
   * @return auto Function return value.
   */
  template <typename F>
  static auto apply(F* f) {
    return [f](Args... args) { return f(std::forward<Args>(args)...); };
  }
};

/**
 * @brief Wraps the passed function in a lambda expression.
 *
 * @tparam Function Type of the passed function.
 * @param f Function or pointer to a free function.
 * @return decltype(auto) Wrapped function.
 */
template <typename Function>
decltype(auto) wrap_free_function(Function&& f) {
  return free_function_wrapper<std::decay_t<Function>>::apply(
      std::forward<Function>(f));
}

/**
 * @brief Creates an overloaded object with passed lambda expressions or
 * functional objects.
 *
 * @tparam Lambdas Types of the passed lambda expressions or functional objects.
 * @param lambdas Lambda expressions or functional objects.
 * @return auto Created overloaded object.
 */
template <typename... Lambdas>
auto overload(Lambdas&&... lambdas) -> overloaded<std::decay_t<Lambdas>...> {
  return {wrap_free_function(std::forward<Lambdas>(lambdas))...};
}

}  // namespace detail

/**
 * @brief Creates an overloaded object with passed lambda expressions or
 * functional objects.
 *
 * @tparam Lambdas Types of the passed lambda expressions or functional objects.
 * @param lambdas Lambda expressions or functional objects.
 * @return decltype(auto) Created overloaded object.
 */
template <typename... Lambdas>
decltype(auto) overload(Lambdas&&... lambdas) {
  return detail::overload(
      detail::wrap_free_function(std::forward<Lambdas>(lambdas))...);
}
}  // namespace pm