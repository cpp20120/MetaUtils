#pragma once

#include <type_traits>

#include "type_utility.hpp"

namespace core::meta::type_traits {

/**
 * @brief Template struct for creating a pair of types.
 * @tparam T The first type.
 * @tparam U The second type.
 */
template <typename T, typename U>
struct pair_t {
  using first = T;
  using second = U;
};

/**
 * @brief Template alias for extracting the first type from a pair.
 * @tparam T The pair type.
 */
template <typename T>
using first_t = typename T::first;

/**
 * @brief Template alias for extracting the second type from a pair.
 * @tparam T The pair type.
 */
template <typename T>
using second_t = typename T::second;

/**
 * @brief Template struct for creating a pair of values.
 * @tparam p The first value.
 * @tparam q The second value.
 */
template <int p, int q>
struct pair_v {
  static constexpr auto first = p;
  static constexpr auto second = q;
};

/**
 * @brief Extracts the first value from a pair.
 * @tparam T The pair type.
 */
template <typename T>
inline constexpr auto first_v = T::first;

/**
 * @brief Extracts the second value from a pair.
 * @tparam T The pair type.
 */
template <typename T>
inline constexpr auto second_v = T::second;

/**
 * @brief Computes the difference between the second and first values of a pair.
 * @tparam T The pair type.
 */
template <typename T>
inline constexpr auto pair_diff = second_v<T> - first_v<T>;

/**
 * @brief Template struct for creating a triple of values.
 * @tparam p The first value.
 * @tparam q The second value.
 * @tparam T The type.
 */
template <auto p, auto q, typename T>
struct triple : std::type_identity<T> {
  static constexpr auto first = p;
  static constexpr auto second = q;
};

/**
 * @brief Pair of size_t indices
 * @tparam t_first First index
 * @tparam t_second Second index
 */
template <std::size_t t_first, std::size_t t_second>
struct index_pair {
  constexpr inline static std::size_t first{t_first};
  constexpr inline static std::size_t second{t_second};
};

}  // namespace core::meta::type_traits