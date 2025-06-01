#pragma once
/**
* TODO iterator_category and difference_type for unified_iterator
* TODO Validation for step_ and count_
* TODO Support for operator-- and biderectional_iterator
* TODO Concept std::ranges::input_range for unified_view_fn
*/
#include <iostream>
#include <ranges>
#include <type_traits>
#include <vector>

/**
 * @namespace unified_views
 * @brief Namespace for unified access views
 *
 * Provides view adapters for iteration with a specified step and repeat count
 */
namespace unified_views {

/**
 * @brief Iterator for unified_view
 * @tparam R Range type
 *
 * Implements an iterator that traverses the range with a given step,
 * repeating each element a specified number of times
 */
template <typename R>
struct unified_iterator {
  using base = std::ranges::iterator_t<R>;           ///< Base iterator type
  using value_type = std::ranges::range_value_t<R>;  ///< Value type
  using reference_type = std::ranges::range_reference_t<R>;  ///< Reference type

  /**
   * @brief Iterator constructor
   * @param start Starting position
   * @param end End position
   * @param step Step size
   * @param count Number of repeats for each element
   */
  constexpr unified_iterator(base start, base end,
                             std::ranges::range_difference_t<R> step,
                             std::ranges::range_difference_t<R> count)
      : pos_(start), end_(end), step_(step), count_(count), step_count_(0) {}

  /**
   * @brief Post-increment operator
   * @return Iterator before increment
   */
  constexpr unified_iterator operator++(int) {
    auto ret = *this;
    if (step_count_ < count_ - 1) {
      ++step_count_;
    } else {
      step_count_ = 0;
      pos_ = std::ranges::next(pos_, step_, end_);
    }
    return ret;
  }

  /**
   * @brief Pre-increment operator
   * @return Reference to this iterator
   */
  constexpr unified_iterator& operator++() {
    if (step_count_ < count_ - 1) {
      ++step_count_;
    } else {
      step_count_ = 0;
      pos_ = std::ranges::next(pos_, step_, end_);
    }
    return *this;
  }

  /**
   * @brief Dereference operator
   * @return Reference to current element
   */
  constexpr reference_type operator*() const { return *pos_; }

  /**
   * @brief Equality comparison operator
   * @param other Another iterator
   * @return true if iterators are equal
   */
  constexpr bool operator==(unified_iterator const& other) const {
    return pos_ == other.pos_ && step_count_ == other.step_count_;
  }

  /**
   * @brief Get current position
   * @return Base iterator
   */
  constexpr base const value() const { return pos_; }

 private:
  base pos_;                                       ///< Current position
  base end_;                                       ///< End position
  std::ranges::range_difference_t<R> step_;        ///< Step size
  std::ranges::range_difference_t<R> count_;       ///< Repeat count
  std::ranges::range_difference_t<R> step_count_;  ///< Current repeat counter
};

/**
 * @brief Sentinel for unified_view
 * @tparam R Range type
 */
template <typename R>
struct unified_sentinel {
  using base = std::ranges::iterator_t<R>;  ///< Base iterator type

  unified_sentinel() = default;

  /**
   * @brief Sentinel constructor
   * @param end End iterator
   */
  constexpr unified_sentinel(base end) : end_(end) {}

  /**
   * @brief Comparison operator with iterator
   * @param it Iterator to compare
   * @return true if iterator reached the end
   */
  constexpr bool operator==(unified_iterator<R> const& it) const {
    return end_ == it.value();
  }

 private:
  base end_;  ///< End position
};

/**
 * @brief View for iteration with step and repeats
 * @tparam R Base range type
 *
 * Represents a range that iterates over elements with a specified step,
 * repeating each element a given number of times
 */
template <std::ranges::view R>
struct unified_view : public std::ranges::view_interface<unified_view<R>> {
 private:
  R base_;                                    ///< Base range
  std::ranges::range_difference_t<R> step_;   ///< Step size
  std::ranges::range_difference_t<R> count_;  ///< Repeat count

 public:
  unified_view() = default;

  /**
   * @brief View constructor
   * @param base Base range
   * @param step Step size
   * @param count Repeat count for each element
   */
  constexpr unified_view(R base, std::ranges::range_difference_t<R> step,
                         std::ranges::range_difference_t<R> count)
      : base_(std::move(base)), step_(step), count_(count) {}

  /**
   * @brief Get the base range (lvalue)
   * @return Copy of the base range
   */
  constexpr R base() const&
    requires std::copy_constructible<R>
  {
    return base_;
  }

  /**
   * @brief Get the base range (rvalue)
   * @return Moved base range
   */
  constexpr R base() && { return std::move(base_); }

  /**
   * @brief Get begin iterator
   * @return Iterator to the first element
   */
  constexpr auto begin() {
    return unified_iterator<R>(std::ranges::begin(base_),
                               std::ranges::end(base_), step_, count_);
  }

  /**
   * @brief Get const begin iterator
   * @return Const iterator to the first element
   */
  constexpr auto begin() const
    requires std::ranges::range<R const>
  {
    return unified_iterator<R const>(std::ranges::begin(base_),
                                     std::ranges::end(base_), step_, count_);
  }

  /**
   * @brief Get sentinel to the end
   * @return Sentinel to the end of the range
   */
  constexpr auto end() { return unified_sentinel<R>{std::ranges::end(base_)}; }

  /**
   * @brief Get const sentinel to the end
   * @return Const sentinel to the end of the range
   */
  constexpr auto end() const
    requires std::ranges::range<R const>
  {
    return unified_sentinel<R const>{std::ranges::end(base_)};
  }

  /**
   * @brief Get the size of the range (const)
   * @return Size accounting for step and repeat count
   */
  constexpr auto size() const
    requires std::ranges::sized_range<R const>
  {
    auto d = std::ranges::size(base_);
    return step_ == 1 ? d * count_ : static_cast<int>((d + 1) / step_) * count_;
  }

  /**
   * @brief Get the size of the range
   * @return Size accounting for step and repeat count
   */
  constexpr auto size()
    requires std::ranges::sized_range<R>
  {
    auto d = std::ranges::size(base_);
    return step_ == 1 ? d * count_ : static_cast<int>((d + 1) / step_) * count_;
  }
};

/**
 * @brief Deduction guide for unified_view
 * @tparam R Range type
 */
template <class R>
unified_view(R&& base, std::ranges::range_difference_t<R> step,
             std::ranges::range_difference_t<R> count)
    -> unified_view<std::ranges::views::all_t<R>>;

namespace details {
/**
 * @brief Functional object for creating unified_view
 */
struct unified_view_fn {
  /**
   * @brief Create a unified_view
   * @tparam R Range type
   * @param r Range
   * @param step Step size
   * @param count Repeat count
   * @return unified_view for the given parameters
   */
  template <std::ranges::range R>
  constexpr auto operator()(R&& r, std::ranges::range_difference_t<R> step,
                            std::ranges::range_difference_t<R> count) const {
    return unified_view(std::forward<R>(r), step, count);
  }
};
}  // namespace details

namespace views {
/**
 * @brief View adapter for creating unified_view
 *
 * Usage example:
 * @code
 * std::vector<int> v{1, 2, 3, 4, 5};
 * for (int i : v | unified_views::unified(2, 3)) {
 *   // Each second element will be repeated 3 times
 * }
 * @endcode
 */
inline constexpr details::unified_view_fn unified;
}  // namespace views

}  // namespace unified_views
