/**
 * @file
 * @brief Meta-programming utilities for working with packs of types and values.
 */

#pragma once

#include <type_traits>
#include <utility>

namespace core::meta::utils {

/**
 * @brief A pack of auto (non-type) template parameters.
 * @tparam Args The non-type template parameters.
 */
template <auto... Args>
struct auto_pack {
  using type = auto_pack<Args...>;  ///< The type of this pack.

  static constexpr size_t value =
      sizeof...(Args);  ///< Number of elements in the pack.

  /**
   * @brief Gets the number of elements in the pack.
   * @return The size of the pack.
   */
  static constexpr size_t size() noexcept { return value; }
};

/**
 * @brief A pack of type template parameters.
 * @tparam Args The type template parameters.
 */
template <typename... Args>
struct type_pack {
  using type = type_pack<Args...>;  ///< The type of this pack.

  static constexpr size_t value =
      sizeof...(Args);  ///< Number of elements in the pack.

  /**
   * @brief Gets the number of elements in the pack.
   * @return The size of the pack.
   */
  static constexpr size_t size() noexcept { return value; }
};

/**
 * @brief A pack combining a type and auto template parameters.
 * @tparam T The leading type parameter.
 * @tparam Args The non-type template parameters.
 */
template <typename T, auto... Args>
struct args_pack {
  using type = args_pack<T, Args...>;  ///< The type of this pack.

  static constexpr size_t value =
      sizeof...(Args);  ///< Number of non-type elements in the pack.

  /**
   * @brief Gets the number of non-type elements in the pack.
   * @return The size of the pack.
   */
  static constexpr size_t size() noexcept { return value; }
};

/**
 * @brief Primary template for getting the size of a pack.
 * @tparam T The pack type to examine.
 * @note The primary template has size 0.
 */
template <typename T>
struct pack_size : std::integral_constant<size_t, 0> {};

/**
 * @brief Specialization for auto_pack-like types.
 * @tparam T The template template parameter.
 * @tparam U The non-type template parameters.
 */
template <template <auto...> typename T, auto... U>
struct pack_size<T<U...>> : std::integral_constant<size_t, sizeof...(U)> {};

/**
 * @brief Specialization for type_pack-like types.
 * @tparam T The template template parameter.
 * @tparam U The type template parameters.
 */
template <template <typename...> typename T, typename... U>
struct pack_size<T<U...>> : std::integral_constant<size_t, sizeof...(U)> {};

/**
 * @brief Specialization for args_pack-like types.
 * @tparam T The template template parameter.
 * @tparam U The leading type parameter.
 * @tparam V The non-type template parameters.
 */
template <template <typename, auto...> typename T, typename U, auto... V>
struct pack_size<T<U, V...>> : std::integral_constant<size_t, sizeof...(V)> {};

/**
 * @brief Helper variable template for pack_size.
 * @tparam T The pack type to examine.
 */
template <typename T>
inline constexpr auto pack_size_v = pack_size<T>::value;

}  // namespace core::meta::utils