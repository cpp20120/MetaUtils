#pragma once

#include <type_traits>

#include "./type_utility.hpp"

namespace core::meta::type_traits {

/**
 * @brief Template struct for creating an identity type.
 * @tparam N The index.
 * @tparam T The type.
 */
template <int N, typename T>
struct identity {
  using type = T;
};

/**
 * @brief Template alias for extracting the type from an identity.
 * @tparam N The index.
 * @tparam T The type.
 */
template <int N, typename T>
using identity_t = typeof_t<identity<N, T>>;

/**
 * @brief Function to ignore a value.
 * @tparam N The index.
 * @tparam T The type of the value.
 * @param t The value to ignore.
 * @return The value.
 */
template <auto N, typename T>
constexpr decltype(auto) ignore(T &&t) {
  return std::forward<T>(t);
}

/**
 * @brief Template struct for creating a wrapper type.
 * @tparam N The index.
 * @tparam T The type.
 */
template <auto N, typename T>
struct wrapper : wrapper<N - 1, std::type_identity<T>> {};

/**
 * @brief Specialization of wrapper for index 0.
 * @tparam T The type.
 */
template <typename T>
struct wrapper<0, T> : std::type_identity<T> {};

/**
 * @brief Template alias for extracting the type from a wrapper.
 * @tparam N The index.
 * @tparam T The type.
 */
template <auto N, typename T>
using wrapper_t = typeof_t<wrapper<N, T>>;

/**
 * @brief Template struct for creating an index type.
 * @tparam N The index.
 * @tparam T The type.
 */
template <auto N, typename T>
struct index_type : std::type_identity<T> {
  static constexpr auto value = N;
};

/**
 * @brief Template alias for creating an upper index type.
 * @tparam N The index.
 * @tparam T The type.
 */
template <auto N, typename T>
using index_upper = wrapper_t<1, index_type<N, T>>;

/**
 * @brief Template struct for creating an alias type.
 * @tparam T The type.
 * @tparam Args The additional types.
 */
template <typename T, typename...>
struct alias {
  using type = T;
};

/**
 * @brief Template alias for extracting the type from an alias.
 * @tparam Args The types.
 */
template <typename... Args>
using alias_t = typeof_t<alias<Args...>>;

/**
 * @brief Template struct for wrapping types in a template.
 * @tparam F The template to wrap in.
 * @tparam Args The types to wrap.
 */
template <template <typename...> typename F, typename... Args>
struct wrapin {
  using type = tuple_t<F<Args>...>;
};

/**
 * @brief Template alias for extracting the type from a wrapin.
 * @tparam F The template to wrap in.
 * @tparam Args The types to wrap.
 */
template <template <typename...> typename F, typename... Args>
using wrapin_t = typeof_t<wrapin<F, Args...>>;

/**
 * @brief Template alias for wrapping a type in a template based on a boolean
 * condition.
 * @tparam B The boolean condition.
 * @tparam F The template to wrap in.
 * @tparam T The type to wrap.
 */
template <bool B, template <typename...> typename F, typename T>
using wrapin_if = std::conditional_t<B, F<T>, T>;

/**
 * @brief Integral constant representing a size_t index
 * @tparam idx The index value
 */
template <std::size_t idx>
using index_constant = std::integral_constant<std::size_t, idx>;

}  // namespace core::meta::type_traits