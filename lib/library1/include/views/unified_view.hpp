#pragma once

#include <cassert>
#include <iostream>
#include <ranges>
#include <type_traits>
#include <vector>

namespace unified_views {

    /**
     * @brief Iterator for the unified_view adapter.
     *
     * This iterator walks through a range with a custom stepping pattern:
     * For each position, it repeats the element `count` times before advancing
     * `step` positions in the underlying range.
     *
     * @tparam R The type of the underlying range.
     */
    template <typename R>
    struct unified_iterator {
        using base = std::ranges::iterator_t<R>;
        using value_type = std::ranges::range_value_t<R>;
        using reference = std::ranges::range_reference_t<R>;
        using difference_type = std::ranges::range_difference_t<R>;
        using pointer = typename std::iterator_traits<base>::pointer;

        using iterator_concept =
            std::conditional_t<
            std::ranges::random_access_iterator<base>,
            std::random_access_iterator_tag,
            std::conditional_t<
            std::ranges::bidirectional_iterator<base>,
            std::bidirectional_iterator_tag,
            std::input_iterator_tag>>;

        using iterator_category = iterator_concept;

        /**
         * @brief Default constructor.
         */
        constexpr unified_iterator() = default;

        /**
         * @brief Constructs a unified_iterator with specified parameters.
         *
         * @param start Starting iterator in the underlying range.
         * @param end End iterator of the underlying range.
         * @param step Number of positions to advance in the underlying range after each cycle.
         * @param count Number of times to repeat each element before advancing.
         */
        constexpr unified_iterator(base begin, base start, base end,
            difference_type step,
            difference_type count)
            : begin_(begin),
            pos_(start),
            end_(end),
            step_(step),
            count_(count),
            step_count_(0)
        {
            assert(step_ > 0);
            assert(count_ > 0);
        }

        /**
         * @brief Arrow operator (for random_access support).
         *
         * @return Pointer to the current element.
         */
        constexpr pointer operator->() const
            requires std::contiguous_iterator<base>
        {
            return std::to_address(pos_);
        }

        /**
         * @brief Pre-increment operator.
         *
         * Advances the iterator. If the current element has been repeated fewer than
         * `count` times, it stays on the same element. Otherwise, it advances `step`
         * positions in the underlying range.
         *
         * @return Reference to the incremented iterator.
         */
        constexpr unified_iterator& operator++() {
            if (step_count_ + 1 < count_) {
                ++step_count_;
            }
            else {
                step_count_ = 0;
                pos_ = std::ranges::next(pos_, step_, end_);
            }
            return *this;
        }

        /**
         * @brief Post-increment operator.
         *
         * @return A copy of the iterator before incrementing.
         */
        constexpr unified_iterator operator++(int) {
            auto tmp = *this;
            ++(*this);
            return tmp;
        }

        /**
         * @brief Dereference operator.
         *
         * @return Reference to the current element.
         */
        constexpr reference operator*() const { return *pos_; }

        /**
         * @brief Equality comparison.
         *
         * @param other Iterator to compare with.
         * @return true if both iterators point to the same position and have the same repeat count.
         */
        constexpr bool operator==(unified_iterator const& other) const {
            return pos_ == other.pos_ && step_count_ == other.step_count_;
        }

        /**
         * @brief Returns the underlying iterator position.
         *
         * @return Current iterator in the underlying range.
         */
        constexpr base value() const { return pos_; }

        /**
         * @brief Pre-decrement operator (bidirectional support).
         *
         * Requires the underlying iterator to be bidirectional.
         * Moves backward in the iteration sequence.
         */
        constexpr unified_iterator& operator--()
            requires std::ranges::bidirectional_iterator<base>
        {
            if (step_count_ > 0) {
                --step_count_;
            }
            else {
                step_count_ = count_ - 1;
                pos_ = std::ranges::prev(pos_, step_, begin_);
            }
            return *this;
        }

        /**
         * @brief Post-decrement operator (bidirectional support).
         *
         * Requires the underlying iterator to be bidirectional.
         * @return A copy of the iterator before decrementing.
         */
        constexpr unified_iterator operator--(int)
            requires std::ranges::bidirectional_iterator<base>
        {
            auto tmp = *this;
            --(*this);
            return tmp;
        }

        /**
         * @brief Addition operator (random_access support).
         *
         * @param n Number of positions to advance.
         * @return Advanced iterator.
         */
        constexpr unified_iterator operator+(difference_type n) const
            requires std::ranges::random_access_iterator<base>
        {
            auto tmp = *this;
            tmp += n;
            return tmp;
        }

        /**
         * @brief Addition assignment operator (random_access support).
         *
         * @param n Number of positions to advance.
         * @return Reference to the updated iterator.
         */
        constexpr unified_iterator& operator+=(difference_type n)
            requires std::ranges::random_access_iterator<base>
        {
            if (n > 0) {
                // Calculate total cycles to advance
                difference_type total_cycles = step_count_ + n;
                difference_type full_cycles = total_cycles / count_;
                difference_type new_step_count = total_cycles % count_;

                // Advance the underlying iterator
                pos_ = std::ranges::next(pos_, step_ * full_cycles, end_);
                step_count_ = new_step_count;
            }
            else if (n < 0) {
                *this -= -n;
            }
            return *this;
        }

        /**
         * @brief Subtraction operator (random_access support).
         *
         * @param n Number of positions to move backward.
         * @return Moved iterator.
         */
        constexpr unified_iterator operator-(difference_type n) const
            requires std::ranges::random_access_iterator<base>
        {
            auto tmp = *this;
            tmp -= n;
            return tmp;
        }

        /**
         * @brief Subtraction assignment operator (random_access support).
         *
         * @param n Number of positions to move backward.
         * @return Reference to the updated iterator.
         */
        constexpr unified_iterator& operator-=(difference_type n)
            requires std::ranges::random_access_iterator<base>
        {
            if (n > 0) {
                // Calculate total cycles to move backward
                difference_type total_cycles = (count_ - 1 - step_count_) + n;
                difference_type full_cycles = total_cycles / count_;
                difference_type new_step_count = (count_ - 1 - (total_cycles % count_)) % count_;

                // Move backward in underlying iterator
                pos_ = std::ranges::prev(pos_, step_ * full_cycles, begin_);
                step_count_ = new_step_count;
            }
            else if (n < 0) {
                *this += -n;
            }
            return *this;
        }

        /**
         * @brief Difference between two iterators (random_access support).
         *
         * @param other Iterator to subtract.
         * @return Distance between iterators.
         */
        constexpr difference_type operator-(unified_iterator const& other) const
            requires std::ranges::random_access_iterator<base>
        {
            // Calculate position in the underlying range
            auto base_dist = (pos_ - other.pos_) / step_;

            // Convert to distance in unified view
            return (base_dist * count_) + step_count_ - other.step_count_;
        }

        /**
         * @brief Subscript operator (random_access support).
         *
         * @param n Index offset.
         * @return Reference to element at offset.
         */
        constexpr reference operator[](difference_type n) const
            requires std::ranges::random_access_iterator<base>
        {
            return *(*this + n);
        }

        /**
         * @brief Spaceship operator (C++20) for ordering.
         */
        constexpr auto operator<=>(unified_iterator const& other) const
            requires std::ranges::random_access_iterator<base>&&
        std::totally_ordered<base>
        {
            if (pos_ != other.pos_) {
                return pos_ <=> other.pos_;
            }
            return step_count_ <=> other.step_count_;
        }

        /**
         * @brief Inequality operator.
         */
        constexpr bool operator!=(unified_iterator const& other) const {
            return !(*this == other);
        }

        /**
         * @brief Less than operator.
         */
        constexpr bool operator<(unified_iterator const& other) const
            requires std::ranges::random_access_iterator<base>
        {
            return pos_ < other.pos_ || (pos_ == other.pos_ && step_count_ < other.step_count_);
        }

        /**
         * @brief Greater than operator.
         */
        constexpr bool operator>(unified_iterator const& other) const
            requires std::ranges::random_access_iterator<base>
        {
            return other < *this;
        }

        /**
         * @brief Less than or equal operator.
         */
        constexpr bool operator<=(unified_iterator const& other) const
            requires std::ranges::random_access_iterator<base>
        {
            return !(other < *this);
        }

        /**
         * @brief Greater than or equal operator.
         */
        constexpr bool operator>=(unified_iterator const& other) const
            requires std::ranges::random_access_iterator<base>
        {
            return !(*this < other);
        }

    private:
        base begin_{};              ///< Begin of underlying range
        base pos_{};                ///< Current position in underlying range
        base end_{};                ///< End of underlying range
        difference_type step_{ 1 }; ///< Step size in underlying range
        difference_type count_{ 1 };///< Repeat count per element
        difference_type step_count_{ 0 }; ///< Current repeat counter (0 to count-1)
    };

    // Forward declaration for operator+ with iterator on left side
    template <typename R>
    constexpr unified_iterator<R> operator+(
        typename unified_iterator<R>::difference_type n,
        unified_iterator<R> const& it)
        requires std::random_access_iterator<typename unified_iterator<R>::base>
    {
        return it + n;
    }

    /**
     * @brief Sentinel for unified_view.
     *
     * Marks the end of iteration by comparing with the underlying range's end.
     *
     * @tparam R The type of the underlying range.
     */
    template <typename R>
    struct unified_sentinel {
        using base = std::ranges::iterator_t<R>;

        unified_sentinel() = default;

        /**
         * @brief Constructs a sentinel from an underlying end iterator.
         *
         * @param end End iterator of the underlying range.
         */
        constexpr explicit unified_sentinel(base end) : end_(end) {}

        /**
         * @brief Equality comparison with unified_iterator.
         *
         * @param it Iterator to compare with.
         * @return true if the iterator has reached the end of the underlying range.
         */
        constexpr bool operator==(unified_iterator<R> const& it) const {
            return it.value() == end_;
        }

    private:
        base end_{}; ///< End iterator of the underlying range
    };

    /**
     * @brief A C++20 range adapter that repeats each element multiple times with stepping.
     *
     * This view takes an input range and produces a new range where:
     * 1. Each element is repeated `count` times consecutively.
     * 2. After repeating an element `count` times, it advances `step` positions
     *    in the underlying range (instead of just 1).
     *
     * Example: unified_view(v, 2, 3) on [1,2,3,4,5,6] produces:
     * [1,1,1,3,3,3,5,5,5]
     *
     * @tparam R The type of the underlying range (must satisfy std::ranges::view).
     */
    template <std::ranges::view R>
    struct unified_view : std::ranges::view_interface<unified_view<R>> {
    private:
        R base_;                                    ///< Underlying range
        std::ranges::range_difference_t<R> step_;   ///< Step size
        std::ranges::range_difference_t<R> count_;  ///< Repeat count

    public:
        unified_view() = default;

        /**
         * @brief Constructs a unified_view.
         *
         * @param base Underlying range.
         * @param step Number of positions to advance in the base range after each cycle.
         * @param count Number of times to repeat each element.
         */
        constexpr unified_view(R base,
            std::ranges::range_difference_t<R> step,
            std::ranges::range_difference_t<R> count)
            : base_(std::move(base)), step_(step), count_(count) {
            assert(step_ > 0);
            assert(count_ > 0);
        }

        /**
         * @brief Returns a copy of the underlying range (const lvalue overload).
         *
         * @return Copy of the base range.
         */
        constexpr R base() const&
            requires std::copy_constructible<R>
        {
            return base_;
        }

        /**
         * @brief Returns the underlying range (rvalue overload).
         *
         * @return Moved base range.
         */
        constexpr R base()&& { return std::move(base_); }

        /**
         * @brief Returns an iterator to the first element.
         *
         * @return unified_iterator positioned at the beginning.
         */
        constexpr auto begin() {
            auto b = std::ranges::begin(base_);
            auto e = std::ranges::end(base_);
            return unified_iterator<R>(b, b, e, step_, count_);
        }

        /**
         * @brief Const overload of begin().
         */
        constexpr auto begin() const
            requires std::ranges::range<R const>
        {
            auto b = std::ranges::begin(base_);
            auto e = std::ranges::end(base_);
            return unified_iterator<R const>(b, b, e, step_, count_);
        }

        /**
         * @brief Returns a sentinel marking the end.
         *
         * @return unified_sentinel for the end of the view.
         */
        constexpr auto end()
            requires std::ranges::bidirectional_range<R> && !std::ranges::random_access_range<R>
        {
            auto b = std::ranges::begin(base_);
            auto e = std::ranges::end(base_);

            if (b == e) {
                return unified_iterator<R>(b, e, e, step_, count_);
            }

            auto last = std::ranges::prev(e);
            return unified_iterator<R>(b, last, e, step_, count_);
        }

        /**
         * @brief Returns an end iterator for random_access ranges.
         */
        constexpr auto end()
            requires std::ranges::random_access_range<R>
        {
            auto b = std::ranges::begin(base_);
            auto e = std::ranges::end(base_);

            // Calculate end position: start + size() steps
            auto size = std::ranges::size(base_);
            auto last_index = ((size + step_ - 1) / step_ - 1) * step_;
            if (last_index < 0) last_index = 0;

            auto last_pos = b + last_index;
            if (last_pos >= e) last_pos = b;

            return unified_iterator<R>(b, last_pos, e, step_, count_) + (count_ - 1);
        }

        /**
         * @brief Const overload of end() for bidirectional ranges.
         */
        constexpr auto end() const
            requires std::ranges::bidirectional_range<R const> && !std::ranges::random_access_range<R const>
        {
            auto b = std::ranges::begin(base_);
            auto e = std::ranges::end(base_);

            if (b == e) {
                return unified_iterator<R const>(b, e, e, step_, count_);
            }

            auto last = std::ranges::prev(e);
            return unified_iterator<R const>(b, last, e, step_, count_);
        }

        /**
         * @brief Const overload of end() for random_access ranges.
         */
        constexpr auto end() const
            requires std::ranges::random_access_range<R const>
        {
            auto b = std::ranges::begin(base_);
            auto e = std::ranges::end(base_);

            auto size = std::ranges::size(base_);
            auto last_index = ((size + step_ - 1) / step_ - 1) * step_;
            if (last_index < 0) last_index = 0;

            auto last_pos = b + last_index;
            if (last_pos >= e) last_pos = b;

            return unified_iterator<R const>(b, last_pos, e, step_, count_) + (count_ - 1);
        }

        constexpr auto end()
            requires (!std::ranges::bidirectional_range<R>)
        {
            return unified_sentinel<R>{std::ranges::end(base_)};
        }

        /**
         * @brief Returns the size of the view if the underlying range is sized.
         *
         * The size is calculated as: ceil(base_size / step) * count
         */
        constexpr auto size() const
            requires std::ranges::sized_range<R const>
        {
            auto n = std::ranges::size(base_);
            return ((n + step_ - 1) / step_) * count_;
        }

        /**
         * @brief Non-const overload of size().
         */
        constexpr auto size()
            requires std::ranges::sized_range<R>
        {
            auto n = std::ranges::size(base_);
            return ((n + step_ - 1) / step_) * count_;
        }

        /**
         * @brief Subscript operator for random_access_range support.
         */
        constexpr auto operator[](std::ranges::range_difference_t<R> n) const
            requires std::ranges::random_access_range<const R>
        {
            return *std::ranges::next(begin(), n);
        }

        /**
         * @brief Subscript operator for random_access_range support (non-const).
         */
        constexpr auto operator[](std::ranges::range_difference_t<R> n)
            requires std::ranges::random_access_range<R>
        {
            return *std::ranges::next(begin(), n);
        }
    };

    // Deduction guide for unified_view
    template <class R>
    unified_view(R&&, std::ranges::range_difference_t<R>,
        std::ranges::range_difference_t<R>)
        -> unified_view<std::ranges::views::all_t<R>>;

    namespace details {

        /**
         * @brief Function object for creating unified_view.
         */
        struct unified_view_fn {
            /**
             * @brief Creates a unified_view from a range, step, and count.
             *
             * @tparam R Input range type.
             * @param r Input range.
             * @param step Step size in the underlying range.
             * @param count Repeat count per element.
             * @return A unified_view adaptor.
             */
            template <std::ranges::input_range R>
            constexpr auto operator()(R&& r,
                std::ranges::range_difference_t<R> step,
                std::ranges::range_difference_t<R> count) const {
                return unified_view(std::forward<R>(r), step, count);
            }

            /**
             * @brief Pipe operator for range adaptor.
             */
            template <std::ranges::input_range R>
            constexpr auto operator()(std::ranges::range_difference_t<R> step,
                std::ranges::range_difference_t<R> count) const {
                return std::views::transform([step, count](auto&& rng) {
                    return unified_view(std::forward<decltype(rng)>(rng), step, count);
                    });
            }
        };

    } // namespace details

    /**
     * @brief Namespace containing range adaptor objects (RAOs).
     */
    namespace views {
        /**
         * @brief Range adaptor object for unified_view.
         *
         * Can be used in pipe notation: `range | views::unified(step, count)`
         * or as a function call: `views::unified(range, step, count)`
         */
        inline constexpr details::unified_view_fn unified;
    }

} // namespace unified_views
