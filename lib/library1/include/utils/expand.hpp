#pragma once

#include <type_traits>

#include "../meta_funcs/metafuncs_utility.hpp"
#include "./sequence.hpp"
#include "./tuple_utils.hpp"

namespace core::meta::utils {

/**
 * @brief Expands a template with index-based transformations
 *
 * @tparam F Template template parameter to apply
 * @tparam T Input type to transform
 * @tparam U Index sequence for expansion
 * @tparam values... Sequence of values
 * @tparam N Sequence of indices
 * @tparam V Offset value for indices
 *
 * Specialization for types with value parameters and true condition
 */
template <template <auto, typename> typename F,
          template <typename, auto...> typename T, typename U, auto... values,
          auto... N>
struct expand<F, T<U, values...>, std::index_sequence<N...>, true> {
  using args = T<U, values...>;  ///< Original argument type
  using type = tuple_t<c_<typev<F<N, args>>, U>...>;  ///< Resulting tuple type
};

/**
 * @brief Expands a template with index-based transformations
 *
 * Specialization for types with value parameters and false condition
 */
template <template <auto, typename> typename F,
          template <typename, auto...> typename T, typename U, auto... values,
          auto... N>
struct expand<F, T<U, values...>, std::index_sequence<N...>, false> {
  using args = T<U, values...>;  ///< Original argument type
  using type = tuple_t<
      index_type<N, c_<typev<F<N, args>>, U>>...>;  ///< Resulting tuple type
                                                    ///< with indexed elements
};

/**
 * @brief Empty expansion case for regular template types
 *
 * Specialization when index sequence is empty
 */
template <template <auto, typename> typename F,
          template <typename...> typename T, typename... Args, auto V>
struct expand<F, T<Args...>, std::index_sequence<>, V> {
  using type = clear_t<T<Args...>>;  ///< Cleared input type
};

/**
 * @brief Empty expansion case for value-parameterized types
 *
 * Specialization when index sequence is empty
 */
template <template <auto, typename> typename F,
          template <typename, auto...> typename T, typename U, auto... values,
          auto V>
struct expand<F, T<U, values...>, std::index_sequence<>, V> {
  using type = clear_t<T<U, values...>>;  ///< Cleared input type
};

/**
 * @brief General expansion implementation
 *
 * Handles both tuple-like and integer sequence outputs
 */
template <template <auto, typename> typename F, typename T, auto... N, auto V>
struct expand<F, T, std::index_sequence<N...>, V> {
  /**
   * @brief Implementation for tuple-like output
   */
  template <typename W, bool b>
  struct impl {
    using type =
        tuple_t<typeof_t<F<N + V, W>>...>;  ///< Tuple of transformed types
  };

  /**
   * @brief Implementation for integer sequence output
   */
  template <typename W>
  struct impl<W, false> {
    using type = std::integer_sequence<
        W, typev<F<N + V, W>>...>;  ///< Integer sequence of transformed values
  };

  using head = typeof_t<
      F<head_v<std::index_sequence<N...>, V>, T>>;  ///< First transformed type
  using type =
      typeof_t<impl<T, negav<has_value_type<head>>>>;  ///< Select appropriate
                                                       ///< implementation
};

/**
 * @brief Empty expansion case
 */
template <template <auto, typename> typename F, typename T, auto V>
struct expand<F, T, std::index_sequence<>, V> {
  using type = tuple_t<>;  ///< Empty tuple result
};

/**
 * @brief Convenience alias for expand::type
 */
template <template <auto, typename> typename F, typename T,
          typename U = index_sequence_of_t<T>, auto V = 0>
using expand_t = typeof_t<expand<F, T, U, V>>;

/**
 * @brief Conditional expansion
 *
 * Expands only if condition B is true, otherwise returns original type
 */
template <bool B, template <auto, typename> typename F, typename T,
          typename U = index_sequence_of_t<T>, auto V = 0>
using expand_if =
    std::conditional_t<B, expand<F, T, U, V>, std::type_identity<T>>;

/**
 * @brief Convenience alias for expand_if::type
 */
template <bool B, template <auto, typename> typename F, typename T,
          typename U = index_sequence_of_t<T>, auto V = 0>
using expand_if_t = typeof_t<expand_if<B, F, T, U, V>>;

/**
 * @brief Expansion using element extraction
 */
template <typename T, typename U = index_sequence_of_t<T>, auto V = 0>
using expand_of = expand<element, T, U, V>;

/**
 * @brief Convenience alias for expand_of::type
 */
template <typename T, typename U = index_sequence_of_t<T>, auto V = 0>
using expand_of_t = typeof_t<expand_of<T, U, V>>;

}  // namespace core::meta::utils