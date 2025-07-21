/**
 * @file slice_utils.hpp
 * @brief Advanced container slicing utilities for C++
 *
 * This header provides comprehensive slicing functionality for various
 * container types, including contiguous containers, strings, fixed-size arrays,
 * and general ranges. It offers both compile-time and runtime slicing with
 * bounds checking and normalization.
 */

#pragma once

#include <cassert>
#include <concepts>
#include <iostream>
#include <iterator>
#include <list>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

namespace core::meta::utils {

/**
 * @brief Concept for contiguous containers
 *
 * A contiguous container is a sized range with contiguous elements.
 */
template <typename T>
concept ContiguousContainer =
    std::ranges::contiguous_range<T> && std::ranges::sized_range<T>;

/**
 * @brief Concept for string-like types
 *
 * Matches either std::string or std::string_view (with cv-ref qualifiers
 * removed).
 */
template <typename T>
concept StringLike = std::same_as<std::remove_cvref_t<T>, std::string> ||
                     std::same_as<std::remove_cvref_t<T>, std::string_view>;

/**
 * @brief Concept for fixed-size arrays
 *
 * Matches traditional C-style arrays with non-zero extent.
 */
template <typename T>
concept FixedSizeArray = std::is_array_v<std::remove_reference_t<T>> &&
                         std::extent_v<std::remove_reference_t<T>> != 0;

/**
 * @brief Concept for general ranges
 *
 * Matches sized ranges that aren't contiguous, string-like, or fixed-size
 * arrays.
 */
template <typename T>
concept GeneralRange =
    std::ranges::range<T> && std::ranges::sized_range<T> &&
    !ContiguousContainer<T> && !StringLike<T> && !FixedSizeArray<T>;

/**
 * @brief Concept for types that can be sliced into spans
 *
 * Includes contiguous containers, strings, and fixed-size arrays.
 */
template <typename T>
concept Sliceable =
    ContiguousContainer<T> || StringLike<T> || FixedSizeArray<T>;

/**
 * @brief Concept for types that can be sliced into subranges
 *
 * Includes general ranges that aren't sliceable into spans.
 */
template <typename T>
concept SubrangeSliceable = GeneralRange<T>;

/**
 * @brief Concept for types that have a value_type member
 */
template <typename T>
concept HasValueType =
    requires { typename std::remove_cvref_t<T>::value_type; };

/**
 * @brief Type trait to detect value_type member
 *
 * Inherits from std::true_type if T has value_type member, std::false_type
 * otherwise.
 */
template <typename T, typename = void>
struct has_value_type : std::false_type {};

/// @brief Specialization for types with value_type member
template <typename T>
struct has_value_type<T,
                      std::void_t<typename std::remove_cvref_t<T>::value_type>>
    : std::true_type {};

/// @brief Helper variable template for has_value_type
template <typename T>
inline constexpr bool has_value_type_v = has_value_type<T>::value;

/**
 * @brief Helper to determine the value type of a container
 *
 * Provides the appropriate value type based on container characteristics:
 * - char for strings
 * - array element type for fixed-size arrays
 * - value_type member for other containers
 */
template <typename T>
struct value_type_helper {
  using U = std::remove_cvref_t<T>;
  using type = std::conditional_t<
      StringLike<U>, char,
      std::conditional_t<FixedSizeArray<U>, std::remove_extent_t<U>,
                         std::conditional_t<has_value_type_v<U>,
                                            typename U::value_type, void>>>;
};

/**
 * @brief Safe version of value_type trait that provides better error messages
 *
 * Fails with static_assert if the type doesn't have a valid value_type.
 */
template <typename T, typename = void>
struct safe_value_type {
  static_assert(sizeof(T) == 0, "Type has no value_type");
};

/// @brief Specialization for string-like types
template <typename T>
struct safe_value_type<T, std::enable_if_t<StringLike<T>>> {
  using type = char;
};

/// @brief Specialization for fixed-size arrays
template <typename T>
struct safe_value_type<T, std::enable_if_t<FixedSizeArray<T>>> {
  using type = std::remove_extent_t<T>;
};

/// @brief Specialization for types with value_type member
template <typename T>
struct safe_value_type<T, std::enable_if_t<has_value_type_v<T>>> {
  using type = typename std::remove_cvref_t<T>::value_type;
};

/**
 * @brief Gets the value type of a container
 *
 * Uses value_type_helper to determine the appropriate type.
 */
template <typename T>
using value_type_t = typename value_type_helper<T>::type;

/**
 * @brief Safe version of value_type_t with better error messages
 */
template <typename T>
using safe_value_type_t = typename safe_value_type<T>::type;

/**
 * @brief Determines the appropriate span type for a container
 *
 * Creates const span if the container is const, otherwise non-const span.
 */
template <typename T>
using span_type_t =
    std::conditional_t<std::is_const_v<std::remove_reference_t<T>>,
                       std::span<const value_type_t<T>>,
                       std::span<value_type_t<T>>>;

namespace detail {

/**
 * @brief Validates slice bounds for a container
 *
 * @tparam T Container type
 * @tparam Iter Iterator type
 * @param begin Start iterator of slice
 * @param end End iterator of slice
 * @param container The container being sliced
 * @return true if the slice is valid, false otherwise
 */
template <typename T, typename Iter>
constexpr bool is_valid_slice(Iter begin, Iter end, T&& container) {
  const auto container_begin = std::ranges::begin(container);
  const auto container_end = std::ranges::end(container);
  const auto container_size = std::ranges::size(container);

  if (begin == end && begin == container_end) return true;

  if (begin == container_end || end == container_begin) return false;

  auto distance_begin_to_end = std::distance(begin, end);
  if (distance_begin_to_end < 0) return false;

  auto distance_container_begin_to_begin =
      std::distance(container_begin, begin);
  if (distance_container_begin_to_begin < 0) return false;

  auto distance_container_begin_to_end = std::distance(container_begin, end);
  if (distance_container_begin_to_end >
      static_cast<std::ptrdiff_t>(container_size)) {
    return false;
  }

  if (static_cast<std::size_t>(distance_begin_to_end) > container_size) {
    return false;
  }

  return true;
}

/**
 * @brief Normalizes an index to valid container bounds
 *
 * Handles negative indices (counting from end) and out-of-bounds indices.
 *
 * @tparam T Container type
 * @param index The index to normalize
 * @param container The container being indexed
 * @return Normalized index in [0, size] range
 */
template <typename T>
constexpr std::ptrdiff_t normalize_index(std::ptrdiff_t index, T&& container) {
  const auto size = std::ranges::size(container);
  if (index < 0) {
    index += size;
    if (index < 0) index = 0;
  } else if (static_cast<std::size_t>(index) > size) {
    index = size;
  }
  return index;
}

/**
 * @brief Trait to determine if a type is sliceable
 */
template <typename T, typename = void>
struct is_sliceable_impl : std::false_type {};

/// @brief Specialization for Sliceable types
template <typename T>
struct is_sliceable_impl<T, std::enable_if_t<Sliceable<T>>> : std::true_type {};

/// @brief Specialization for SubrangeSliceable types
template <typename T>
struct is_sliceable_impl<T, std::enable_if_t<SubrangeSliceable<T>>>
    : std::true_type {};

}  // namespace detail

/**
 * @brief Compile-time slicing for Sliceable containers
 *
 * Creates a span over the specified range of elements.
 *
 * @tparam left Left bound (default 0)
 * @tparam right Right bound (default 0)
 * @tparam T Container type (must satisfy Sliceable concept)
 * @param container The container to slice
 * @return std::span over the specified range
 *
 * @note Negative left or positive right bounds are not allowed at compile-time
 */
template <int left = 0, int right = 0, Sliceable T>
constexpr auto slice(T&& container) {
  static_assert(
      left >= 0 || right <= 0,
      "Negative left or positive right not allowed in compile-time slicing");

  auto&& c = std::forward<T>(container);
  const auto size = std::ranges::size(c);

  auto begin = std::ranges::begin(c);
  if constexpr (left > 0) {
    begin += left;
  }

  auto end = std::ranges::end(c);
  if constexpr (right < 0) {
    end += right;
  } else if constexpr (right > 0) {
    end = begin + (right - left);
  }

  assert(detail::is_valid_slice(begin, end, c) && "Invalid slice bounds");
  if (!detail::is_valid_slice(begin, end, c)) {
    return span_type_t<T>{};
  }

  return span_type_t<T>(begin, end);
}

/**
 * @brief Compile-time slicing for SubrangeSliceable containers
 *
 * Creates a subrange over the specified range of elements.
 *
 * @tparam left Left bound (default 0)
 * @tparam right Right bound (default 0)
 * @tparam T Container type (must satisfy SubrangeSliceable concept)
 * @param container The container to slice
 * @return std::ranges::subrange over the specified range
 *
 * @note Negative left or positive right bounds are not allowed at compile-time
 */
template <int left = 0, int right = 0, SubrangeSliceable T>
constexpr auto slice(T&& container) {
  static_assert(
      left >= 0 || right <= 0,
      "Negative left or positive right not allowed in compile-time slicing");

  auto&& c = std::forward<T>(container);
  const auto size = std::ranges::size(c);

  auto begin = std::ranges::begin(c);
  if constexpr (left > 0) {
    begin = std::next(begin, left);
  }

  auto end = std::ranges::end(c);
  if constexpr (right < 0) {
    end = std::next(end, right);
  } else if constexpr (right > 0) {
    end = std::next(begin, right - left);
  }

  if (!detail::is_valid_slice(begin, end, c)) {
    return std::ranges::subrange(std::ranges::begin(c), std::ranges::begin(c));
  }

  return std::ranges::subrange(begin, end);
}

/**
 * @brief Runtime slicing for any sliceable container
 *
 * Creates either a span (for Sliceable) or subrange (for SubrangeSliceable)
 * over the specified range of elements.
 *
 * @tparam T Container type (must be sliceable)
 * @param container The container to slice
 * @param left Left bound index
 * @param right Right bound index
 * @return Slice view of the specified range
 *
 * @note Handles negative indices (counting from end) and normalizes
 * out-of-bounds indices
 */
template <typename T,
          typename = std::enable_if_t<detail::is_sliceable_impl<T>::value>>
constexpr auto slice(T&& container, std::ptrdiff_t left, std::ptrdiff_t right) {
  auto&& c = std::forward<T>(container);
  const auto size = std::ranges::size(c);

  left = detail::normalize_index(left, c);
  right = detail::normalize_index(right, c);

  if (left > right) {
    assert(!"Left index must be less than or equal to right index");
    if constexpr (Sliceable<T>) {
      return span_type_t<T>{};
    } else {
      return std::ranges::subrange(std::ranges::begin(c),
                                   std::ranges::begin(c));
    }
  }

  auto begin = std::ranges::begin(c);
  auto end = std::ranges::begin(c);
  if constexpr (Sliceable<T>) {
    begin += left;
    end += right;
  } else {
    begin = std::next(begin, left);
    end = std::next(end, right);
  }

  if constexpr (Sliceable<T>) {
    return span_type_t<T>(begin, end);
  } else {
    return std::ranges::subrange(begin, end);
  }
}

}  // namespace core::meta::utils