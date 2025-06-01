#pragma once

#include <type_traits>

#include "type_utility.hpp"

namespace core::meta::type_traits {

/**
 * @brief Template alias for ternary conditional type selection.
 * @tparam A The first boolean condition.
 * @tparam B The second boolean condition.
 * @tparam X The type to select if both conditions are true.
 * @tparam Y The type to select if the first condition is true and the second is
 * false.
 * @tparam Z The type to select if the first condition is false.
 */
template <bool A, bool B, typename X, typename Y, typename Z>
using ternary_conditional = std::conditional<A, std::conditional_t<B, X, Y>, Z>;

/**
 * @brief Template alias for extracting the type from a ternary_conditional.
 * @tparam A The first boolean condition.
 * @tparam B The second boolean condition.
 * @tparam X The type to select if both conditions are true.
 * @tparam Y The type to select if the first condition is true and the second is
 * false.
 * @tparam Z The type to select if the first condition is false.
 */
template <bool A, bool B, typename X, typename Y, typename Z>
using ternary_conditional_t = typeof_t<ternary_conditional<A, B, X, Y, Z>>;

/**
 * @brief Makes a const lvalue reference type
 * @tparam T Type to convert
 */
template <typename T>
struct make_const_ref
    : type_identity<
          std::add_lvalue_reference_t<std::add_const_t<remove_cvref_t<T>>>> {};

/**
 * @brief Helper alias for make_const_ref
 * @tparam T Type to convert
 */
template <typename T>
using make_const_ref_t = typename make_const_ref<T>::type;

/**
 * @brief Checks if a type is same as T
 * @tparam T Type to compare against
 */
template <typename T>
struct is_same_as {
  /**
   * @brief Functor to check type equality
   * @tparam U Type to compare
   */
  template <typename U>
  struct func : std::is_same<T, U> {};

  /**
   * @brief Helper variable template for func
   * @tparam U Type to compare
   */
  template <typename U>
  constexpr inline static bool func_v = func<U>::value;
};

/**
 * @brief Composes multiple predicates with conjunction
 * @tparam PREDS Predicates to compose
 */
template <template <typename> typename... PREDS>
struct conjunction_compose {
  /**
   * @brief Applies conjunction to predicates for type T
   * @tparam T Type to check
   */
  template <typename T>
  using func = std::conjunction<PREDS<T>...>;

  /**
   * @brief Helper variable template for func
   * @tparam T Type to check
   */
  template <typename T>
  constexpr inline static bool func_v = func<T>::value;
};

/**
 * @brief Composes multiple predicates with disjunction
 * @tparam PREDS Predicates to compose
 */
template <template <typename> typename... PREDS>
struct disjunction_compose {
  /**
   * @brief Applies disjunction to predicates for type T
   * @tparam T Type to check
   */
  template <typename T>
  using func = std::disjunction<PREDS<T>...>;

  /**
   * @brief Helper variable template for func
   * @tparam T Type to check
   */
  template <typename T>
  constexpr inline static bool func_v = func<T>::value;
};

/**
 * @brief Partially applies a binary function with first argument fixed
 * @tparam FUNC Binary function to apply
 * @tparam First First argument to fix
 */
template <template <typename, typename> typename FUNC, typename First>
struct binary_partial_apply {
  /**
   * @brief Applies function with first argument fixed
   * @tparam Second Second argument
   */
  template <typename Second>
  using func = FUNC<First, Second>;

  /**
   * @brief Helper alias for func
   * @tparam Second Second argument
   */
  template <typename Second>
  using func_t = typename func<Second>::type;

  /**
   * @brief Helper variable template for func
   * @tparam Second Second argument
   */
  template <typename Second>
  constexpr inline static decltype(func<Second>::value) func_v =
      func<Second>::value;
};

/**
 * @brief Applies multiple metafunctions sequentially
 * @tparam T Initial type
 * @tparam FUNC First metafunction to apply
 * @tparam FUNC_PACK Remaining metafunctions to apply
 */
template <typename T, template <typename> typename FUNC,
          template <typename> typename... FUNC_PACK>
struct sequential_apply
    : sequential_apply<typename FUNC<T>::type, FUNC_PACK...> {};

/**
 * @brief Base case for sequential_apply
 * @tparam T Type to transform
 * @tparam FUNC Metafunction to apply
 */
template <typename T, template <typename> typename FUNC>
struct sequential_apply<T, FUNC> : FUNC<T> {};

/**
 * @brief Helper alias for sequential_apply
 * @tparam T Initial type
 * @tparam FUNC First metafunction to apply
 * @tparam FUNC_PACK Remaining metafunctions to apply
 */
template <typename T, template <typename> typename FUNC,
          template <typename> typename... FUNC_PACK>
using sequential_apply_t =
    typename sequential_apply<T, FUNC, FUNC_PACK...>::type;

/**
 * @brief Helper variable template for sequential_apply
 * @tparam T Initial type
 * @tparam FUNC First metafunction to apply
 * @tparam FUNC_PACK Remaining metafunctions to apply
 */
template <typename T, template <typename> typename FUNC,
          template <typename> typename... FUNC_PACK>
constexpr inline auto sequential_apply_v =
    sequential_apply<T, FUNC, FUNC_PACK...>::value;

/**
 * @brief Applies multiple metafunctions sequentially
 * @tparam FUNCS Metafunctions to apply
 */
template <template <typename> typename... FUNCS>
struct sequential_applicator {
  /**
   * @brief Applies metafunctions to type T
   * @tparam T Type to transform
   */
  template <typename T>
  struct func : sequential_apply<T, FUNCS...> {};

  /**
   * @brief Trait to detect if a type is a specialization of a variadic
   * template.
   *
   * @tparam T The type to check.
   *
   * @details Defaults to false. Specialization matches any template that takes
   * variadic type parameters.
   */
  template <typename T>
  struct is_variadic_type : std::false_type {};

  /**
   * @brief Specialization for variadic template instances.
   *
   * @tparam T The variadic template.
   * @tparam Args Template arguments.
   */
  template <template <typename...> typename T, typename... Args>
  struct is_variadic_type<T<Args...>> : std::true_type {};

  /**
   * @brief Shorthand boolean constant for is_variadic_type.
   */
  template <typename T>
  inline constexpr bool is_variadic_type_v = is_variadic_type<T>::value;

  /**
   * @brief Wraps a type together with a constant index.
   *
   * @tparam N The index value.
   * @tparam T The type to associate with the index.
   *
   * @details Useful for working with indexed types in template metaprogramming,
   * such as iterating over parameter packs with indices.
   */
  template <auto N, typename T>
  struct index_type {
    using type = T;                   ///< The wrapped type.
    static constexpr auto index = N;  ///< The index value.
  };

  /**
   * @brief Disable a type if condition is true (negation of std::enable_if).
   *
   * @tparam B Boolean condition.
   * @tparam T Type to conditionally define.
   */
  template <bool B, typename T = std::void_t<>>
  struct disable_if {};

  /**
   * @brief Specialization to allow type T only if B is false.
   */
  template <typename T>
  struct disable_if<false, T> : std::type_identity<T> {};

  /**
   * @brief Shorthand for disable_if.
   */
  template <bool B, typename T = std::void_t<>>
  using disable_if_t = typename disable_if<B, T>::type;

  /**
   * @brief Get number of types in a parameter pack.
   */
  template <typename... Args>
  inline constexpr auto sizeof_v = sizeof...(Args);

  /**
   * @brief Trait to extract size of a variadic template type.
   */
  template <typename T>
  struct variadic_size;

  /**
   * @brief Specialization for class templates with variadic arguments.
   */
  template <template <typename...> typename T, typename... Args>
  struct variadic_size<T<Args...>> {
    static constexpr auto value = sizeof_v<Args...>;
  };

  /**
   * @brief Shorthand to get variadic size of a template type.
   */
  template <typename T>
  inline constexpr auto variadic_size_v = variadic_size<T>::value;

  /**
   * @brief Trait to deduce the size of a type.
   */
  template <typename T, typename = std::void_t<>>
  struct sizeof_t : std::integral_constant<std::size_t, sizeof(T)> {};

  /**
   * @brief Specialization for index_type wrapper types.
   */
  template <auto N, typename T>
  struct sizeof_t<index_type<N, T>> : sizeof_t<T> {};

  /**
   * @brief Specialization for types with a static size() member.
   */
  template <typename T>
  struct sizeof_t<T, std::void_t<decltype(T::size())>>
      : std::integral_constant<std::size_t, T::size()> {};

  /**
   * @brief Specialization for variadic types.
   */
  template <typename T>
  struct sizeof_t<T, std::enable_if_t<is_variadic_type_v<T>>>
      : std::integral_constant<std::size_t, variadic_size_v<T>> {};

  /**
   * @brief Fallback for types with static value member.
   */
  template <typename T>
  struct sizeof_t<T, std::void_t<std::enable_if_t<!is_variadic_type_v<T>>,
                                 decltype(T::value + 1)>> : T {};

  /**
   * @brief Shorthand to get sizeof_t value.
   */
  template <typename T>
  inline constexpr auto sizeof_t_v = sizeof_t<T>::value;

  /**
   * @brief Type trait to compare if sizeof(T) < sizeof(U).
   */
  template <typename T, typename U>
  using less_t = std::bool_constant<(sizeof_t_v<T> < sizeof_t_v<U>)>;

  template <typename T, typename U>
  inline constexpr auto less_v = less_t<T, U>::value;

  /**
   * @brief Type trait to compare if sizeof(T) <= sizeof(U).
   */
  template <typename T, typename U>
  using less_equal_t = std::bool_constant<(sizeof_t_v<T> <= sizeof_t_v<U>)>;

  template <typename T, typename U>
  inline constexpr auto less_equal_v = less_equal_t<T, U>::value;

  /**
   * @brief Type trait to compare if sizeof(T) == sizeof(U).
   */
  template <typename T, typename U>
  using equal_t = std::bool_constant<(sizeof_t_v<T> == sizeof_t_v<U>)>;

  template <typename T, typename U>
  inline constexpr auto equal_v = equal_t<T, U>::value;

  /**
   * @brief Type trait to compare if sizeof(T) >= sizeof(U).
   */
  template <typename T, typename U>
  using greater_equal_t = std::bool_constant<(sizeof_t_v<T> >= sizeof_t_v<U>)>;

  template <typename T, typename U>
  inline constexpr auto greater_equal_v = greater_equal_t<T, U>::value;

  /**
   * @brief Type trait to compare if sizeof(T) > sizeof(U).
   */
  template <typename T, typename U>
  using greater_t = std::bool_constant<(sizeof_t_v<T> > sizeof_t_v<U>)>;

  template <typename T, typename U>
  inline constexpr auto greater_v = greater_t<T, U>::value;

  /**
   * @brief Difference in size between T and U.
   */
  template <typename T, typename U>
  inline constexpr auto size_diff = sizeof_t_v<T> - sizeof_t_v<U>;

  /**
   * @brief Removes all template arguments from a template type.
   */
  template <typename T>
  struct clear : std::type_identity<T> {};

  template <template <typename...> typename T, typename... Args>
  struct clear<T<Args...>> : std::type_identity<T<>> {};

  template <template <typename, auto...> typename T, typename U, auto... Args>
  struct clear<T<U, Args...>> : std::type_identity<T<U>> {};

  /**
   * @brief Shorthand to clear a template type.
   */
  template <typename T>
  using clear_t = typename clear<T>::type;

  /**
   * @brief Conditionally clears a template's arguments if B is true.
   */
  template <bool B, typename T>
  struct clear_if : std::conditional_t<B, clear<T>, std::type_identity<T>> {};

  template <bool B, typename T>
  using clear_if_t = typename clear_if<B, T>::type;

  /**
   * @brief Curries a function: partial application of arguments until
   * invocable.
   *
   * @tparam F Function type.
   * @tparam Ps Partially applied arguments.
   * @param f The function to curry.
   * @param ps The arguments to apply.
   * @return Curried function or result of invocation.
   */
  template <typename F, typename... Ps>
  constexpr decltype(auto) curry(F f, Ps... ps) {
    if constexpr (core::meta::concepts::InvocableWith<F, Ps...>) {
      return std::invoke(f, ps...);
    } else {
      return [f, ps...](auto... qs) -> decltype(auto) {
        return curry(f, ps..., qs...);
      };
    }
  }
