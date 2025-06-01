#pragma once

#include "../meta/fold_add.hpp"
#include "../meta/fold_or.hpp"
#include "../meta/try_catch.hpp"

#include <boost/optional.hpp>
#include <optional>

#include "./signature.hpp"

namespace pm {

/**
 * @brief Function for dynamic type casting.
 */
template <typename>
void dyn_cast();

namespace detail {

/**
 * @brief Template structure representing the target type.
 *
 * This structure is used to denote the target type in internal operations.
 */
template <typename TargetType>
struct target_type {};

/**
 * @brief Function for dynamic type casting using meta::try_t.
 *
 * @tparam TargetType The target type to cast to
 * @tparam SourceType The source type to cast from
 * @return The result of dyn_cast operation
 */
template <typename TargetType, typename SourceType>
[[maybe_unused]] constexpr auto matcher_dyn_cast(meta::try_t,
                                                 target_type<TargetType>,
                                                 SourceType* p)
    -> decltype(dyn_cast<TargetType>(p)) {
  return dyn_cast<TargetType>(p);
}

/**
 * @brief Function for dynamic type casting using meta::catch_t as fallback.
 *
 * @tparam TargetType The target type to cast to
 * @tparam SourceType The source type to cast from
 * @return The result of dynamic_cast operation
 */
template <typename TargetType, typename SourceType>
constexpr auto matcher_dyn_cast(meta::catch_t, target_type<TargetType>,
                                SourceType* p)
    -> decltype(dynamic_cast<TargetType*>(p)) {
  return dynamic_cast<TargetType*>(p);
}

/**
 * @brief Structure for storing function result information.
 *
 * This structure stores information about function results and the result type.
 */
template <typename Function, typename... Functions>
struct result {
  using type = typename pm::signature<Function>::result_type;

  template <typename...>
  struct type_list {};

  template <typename F>
  using as_type = std::decay_t<type>;

  using all_results =
      type_list<typename pm::signature<Functions>::result_type...>;
  using expected_results = type_list<as_type<Functions>...>;

  static_assert(std::is_same<all_results, expected_results>::value,
                "All functions must have the same result type.");
};

/**
 * @brief Structure for storing function result type information.
 *
 * This structure stores information about function result types and is used
 * during matching.
 */
template <typename... Functions>
struct result_type_info {
  static constexpr size_t sum_of_arguments =
      meta::fold_add_v<size_t, signature<Functions>::number_of_arguments...>;
  using wrapped_result_t = typename result<Functions...>::type;
  using result_t = std::conditional_t<
      std::is_same<void, wrapped_result_t>::value, bool,
      std::conditional_t<sum_of_arguments == size_t(sizeof...(Functions)),
                         std::optional<wrapped_result_t>, wrapped_result_t>>;
};

/**
 * @brief Structure for invoking matching functions.
 *
 * This structure is used to invoke matching functions considering their
 * results.
 */
template <typename ResultType>
struct invoker {
  template <typename Function>
  static constexpr decltype(auto) apply(Function& f) {
    return f();
  }

  template <typename Function, typename Arg>
  static constexpr decltype(auto) apply(Function& f, Arg& a) {
    return f(a);
  }
};

template <>
struct invoker<void> {
  template <typename Function>
  static constexpr decltype(auto) apply(Function& f) {
    return f(), true;
  }

  template <typename Function, typename Arg>
  static constexpr decltype(auto) apply(Function& f, Arg& a) {
    return f(a), true;
  }
};

/**
 * @brief Helper function for implementing matching.
 *
 * @tparam ResultTypeInfo The result type information
 * @tparam Type The type being matched
 * @tparam Lambda The lambda function type
 * @tparam Lambdas Additional lambda function types
 * @param x Pointer to the value being matched
 * @param l Lambda function to try
 * @param ls Additional lambda functions to try
 * @return The result of the matching operation
 */
template <typename ResultTypeInfo, typename Type, typename Lambda,
          typename... Lambdas>
constexpr auto matcher_impl(Type* x, Lambda& l, Lambdas&... ls)
    -> std::enable_if_t<signature<Lambda>::number_of_arguments == 1,
                        typename ResultTypeInfo::result_t> {
  if (auto result = l(*x); result.has_value())
    return result.value();
  else
    return matcher_impl<ResultTypeInfo>(x, ls...);
}

template <typename ResultTypeInfo, typename Type>
constexpr auto matcher_impl(Type*) -> typename ResultTypeInfo::result_t {
  return std::nullopt;
}

template <typename ResultTypeInfo, typename Type, typename Lambda1,
          typename Lambda2, typename... Lambdas>
constexpr auto matcher_impl(Type* x, Lambda1& l1, Lambda2& l2, Lambdas&... ls)
    -> std::enable_if_t<signature<Lambda1>::number_of_arguments == 0,
                        typename ResultTypeInfo::result_t> {
  if (auto result = l1(); result.has_value())
    return result.value();
  else
    return matcher_impl<ResultTypeInfo>(x, l2, ls...);
}

template <typename ResultTypeInfo, typename Type, typename Lambda,
          typename... Lambdas>
constexpr auto matcher_impl(Type*, Lambda& l)
    -> std::enable_if_t<signature<Lambda>::number_of_arguments == 0,
                        typename ResultTypeInfo::result_t> {
  if (auto result = l(); result.has_value())
    return result.value();
  else
    return std::nullopt;
}

/**
 * @brief Function for setting a default value.
 *
 * @tparam Value The type of the default value
 * @param value The default value to return
 * @return A lambda that returns the default value
 */
template <typename Value>
constexpr auto otherwise(Value value) {
  return [value = std::move(value)] { return value; };
}

/**
 * @brief Function for creating a matcher.
 *
 * Creates a matcher based on the provided lambda functions.
 *
 * @tparam Lambdas Types of the lambda functions
 * @param lambdas Lambda functions to use for matching
 * @return A matcher function
 */
template <typename... Lambdas>
constexpr auto matcher(Lambdas&&... lambdas) {
  static_assert(
      meta::fold_and_v<((signature<Lambdas>::number_of_arguments == 0) or
                        (signature<Lambdas>::number_of_arguments == 1))...>,
      "Can only match on lambdas with one argument.");

  using result_info_t = ::pm::detail::result_type_info<Lambdas...>;

  constexpr auto num_args = result_info_t::sum_of_arguments;
  static_assert(
      num_args == sizeof...(Lambdas) or num_args == sizeof...(Lambdas) - 1,
      "There can be only one default value defined per matcher.");

  return [=](auto& x) -> typename result_info_t::result_t {
    return ::pm::detail::matcher_impl<result_info_t>(&x, lambdas...);
  };
}

/**
 * @brief Function for creating a matcher for a value.
 *
 * @tparam Type The type of the value to match
 * @param x The value to match
 * @return A function that accepts matcher lambdas
 */
template <typename Type>
auto match(Type&& x) {
  return
      [x = std::forward<Type>(x)](auto&&... lambdas) mutable -> decltype(auto) {
        return matcher(std::forward<decltype(lambdas)>(lambdas)...)(x);
      };
}

/**
 * @brief Function for creating a matcher for a value (lvalue reference
 * version).
 *
 * @tparam Type The type of the value to match
 * @param x The value to match (lvalue reference)
 * @return A function that accepts matcher lambdas
 */
template <typename Type>
auto match(Type& x) {
  return [&x](auto&&... lambdas) mutable -> decltype(auto) {
    return matcher(std::forward<decltype(lambdas)>(lambdas)...)(x);
  };
}

}  // namespace pm