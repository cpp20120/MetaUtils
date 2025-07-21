/**
 * @file fill_utils.hpp
 * @brief Template metaprogramming utilities for filling and assigning types
 *
 * This header provides utilities for creating filled sequences of types and
 * assigning types to other type containers at compile-time.
 */

#pragma once

#include "./expand.hpp"

namespace core::meta::utils {

/**
 * @brief Fills a container with a type or value N times
 * @tparam n The number of times to fill
 * @tparam T The type or value to fill with
 *
 * This template creates a sequence containing the type T repeated n times.
 * The result is adapted to match the input container type (variadic, tuple,
 * etc).
 */
template <auto n, typename T>
struct fill {
  /**
   * @brief Implementation helper for filling with types
   */
  template <auto N, typename U>
  struct impl : U {};

  /**
   * @brief Applies a filler function
   */
  template <template <auto, typename> typename F>
  using call = expand<F, T, index_sequence_of_c<n>>;

  /**
   * @brief Selects appropriate filling strategy
   */
  using curr = type_if<!has_type_v<T> || !has_value_type_v<T>, call<identity>,
                       call<impl>>;

  /**
   * @brief The resulting filled type sequence
   */
  using type = rename_if_t<is_variadic_type_v<T>, curr, std::tuple<>>;
};

/**
 * @brief Helper type for fill
 */
template <auto n, typename T>
using fill_t = typeof_t<fill<n, T>>;

/**
 * @brief Fills with a constant value
 * @tparam n Number of times to fill
 * @tparam v The constant value to fill with
 */
template <auto n, auto v>
using fill_c = typeof_t<fill<n, c_<v>>>;

/**
 * @brief Assigns a type to fill another type's container
 * @tparam T The target type to assign to
 * @tparam U The type to fill with
 *
 * Creates a new container of the same type as T but filled with U
 */
template <typename T, typename U>
struct assign : rename<fill_t<sizeof_t_v<T>, U>, clear_t<T>> {};

/**
 * @brief Helper type for assign
 */
template <typename T, typename U>
using assign_t = typeof_t<assign<T, U>>;

/**
 * @brief Creates a nested fill structure
 * @tparam N The nesting depth
 * @tparam T The template to nest
 *
 * Creates a recursively nested structure of the template T, N levels deep
 */
template <auto N, template <typename...> typename T>
struct nest_fill : to_nest<fill_t<N, T<>>> {};

/**
 * @brief Helper type for nest_fill
 */
template <auto N, template <typename...> typename T>
using nest_fill_t = typeof_t<nest_fill<N, T>>;

}  // namespace core::meta::utils