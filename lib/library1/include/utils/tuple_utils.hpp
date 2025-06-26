/**
 * @file tuple_utils.hpp
 * @brief A comprehensive collection of utility functions for manipulating
 * std::tuple objects
 * @namespace core::meta::utils
 * @details This header provides a wide range of metaprogramming utilities for
 * working with tuples, including selection, transformation, folding, slicing,
 * and various other operations.
 */

#pragma once

#include <tuple>
#include <type_traits>
#include <utility>

namespace core::meta::utils {

/**
 * @brief Selects elements from a tuple based on specified indices
 * @tparam indices The indices sequence to select (as std::index_sequence)
 * @tparam T The tuple type (deduced)
 * @param t The input tuple (forwarding reference)
 * @return A new tuple containing only the selected elements
 */
template <typename indices, typename T>
constexpr decltype(auto) tuple_select(T &&t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(indices());
}

/**
 * @brief Extracts specific elements from a tuple by their compile-time indices
 * @tparam N The indices to extract (variadic non-type template parameters)
 * @tparam Args The tuple element types
 * @param t The input tuple
 * @return A new tuple containing only the specified elements
 */
template <auto... N, typename... Args>
constexpr decltype(auto) tuple_extract(const std::tuple<Args...> &t) {
  return tuple_select<std::index_sequence<N...>>(t);
}

/**
 * @brief Gathers elements from a tuple by their types
 * @tparam Args The types of elements to gather
 * @tparam T The tuple type (deduced)
 * @param t The input tuple (forwarding reference)
 * @return A new tuple containing only elements of the specified types
 */
template <typename... Args, typename T>
constexpr decltype(auto) tuple_gather(T &&t) {
  return std::make_tuple(std::get<Args>(std::forward<T>(t))...);
}

/**
 * @brief Gathers elements by index and invokes a function with them
 * @tparam N The indices of elements to gather (variadic non-type template
 * parameters)
 * @tparam T The tuple type (deduced)
 * @tparam F The callable type (deduced)
 * @param t The input tuple (forwarding reference)
 * @param f The callable to invoke (forwarding reference)
 * @return Result of invoking f with the gathered elements
 */
template <auto... N, typename T, typename F>
constexpr decltype(auto) tuple_gather_invoke(T &&t, F &&f) {
  return std::invoke(std::forward<F>(f), std::get<N>(std::forward<T>(t))...);
}

/**
 * @brief Gathers elements by type and invokes a function with them
 * @tparam Args The types of elements to gather
 * @tparam T The tuple type (deduced)
 * @tparam F The callable type (deduced)
 * @param t The input tuple (forwarding reference)
 * @param f The callable to invoke (forwarding reference)
 * @return Result of invoking f with the gathered elements
 */
template <typename... Args, typename T, typename F>
constexpr decltype(auto) tuple_gather_invoke(T &&t, F &&f) {
  return std::invoke(std::forward<F>(f), std::get<Args>(std::forward<T>(t))...);
}

/**
 * @brief Creates a tuple with elements in axially symmetric order
 * @tparam Args The tuple element types
 * @param t The input tuple
 * @return A new tuple with elements in reversed order
 */
template <typename... Args>
constexpr decltype(auto) tuple_axial_symmetry(const std::tuple<Args...> &t) {
  return tuple_select<axial_symmetry_t<std::index_sequence_for<Args...>>>(t);
}

/**
 * @brief Helper function for tuple folding operations
 * @tparam N The fold termination index
 * @tparam index The current index
 * @tparam F The callable type (deduced)
 * @tparam T The tuple type (deduced)
 * @param f The folding function (forwarding reference)
 * @param t The input tuple (forwarding reference)
 * @return The folded result
 */
template <size_t N, size_t index, typename F, typename T>
constexpr decltype(auto) tuple_fold(F &&f, T &&t) {
  if constexpr (N == index)
    return std::get<index>(t);
  else if constexpr (N == 0)
    return std::invoke(f, tuple_fold<N, index - 1>(f, t), std::get<index>(t));
  else
    return std::invoke(f, std::get<index>(t), tuple_fold<N, index + 1>(f, t));
}

/**
 * @brief Left-fold operation over tuple elements
 * @tparam F The callable type (deduced)
 * @tparam T The tuple type (deduced)
 * @param f The folding function (forwarding reference)
 * @param t The input tuple (forwarding reference)
 * @return The result of left-folding the tuple
 */
template <typename F, typename T>
constexpr decltype(auto) tuple_foldl(F &&f, T &&t) {
  return tuple_fold<0, std::tuple_size_v<std::decay_t<T>> - 1>(
      std::forward<F>(f), std::forward<T>(t));
}

/**
 * @brief Right-fold operation over tuple elements
 * @tparam F The callable type (deduced)
 * @tparam T The tuple type (deduced)
 * @param f The folding function (forwarding reference)
 * @param t The input tuple (forwarding reference)
 * @return The result of right-folding the tuple
 */
template <typename F, typename T>
constexpr decltype(auto) tuple_foldr(F &&f, T &&t) {
  return tuple_fold<std::tuple_size_v<std::decay_t<T>> - 1, 0>(
      std::forward<F>(f), std::forward<T>(t));
}

/**
 * @brief Creates a callable that captures arguments for later invocation
 * @tparam F The callable type (deduced)
 * @tparam Args The argument types (deduced)
 * @param f The callable to capture (forwarding reference)
 * @param args The arguments to capture (forwarding references)
 * @return A lambda that when called will invoke f with captured args plus new
 * args
 */
template <typename F, typename... Args>
constexpr decltype(auto) capture_invoke(F &&f, Args &&...args) {
  return [f = std::forward<F>(f), ... args = std::move(args)]<typename... T>(
             T &&...t) mutable -> decltype(auto) {
    return std::invoke(std::forward<F>(f), std::forward<T>(t)...,
                       std::forward<Args>(args)...);
  };
}

/**
 * @brief Applies a function to a tuple's elements
 * @tparam F The callable type (deduced)
 * @tparam Args The tuple element types
 * @param t The input tuple
 * @param f The function to apply (forwarding reference)
 * @return The result of invoking f with the tuple's elements
 */
template <typename F, typename... Args>
constexpr decltype(auto) tuple_apply(const std::tuple<Args...> &t, F &&f) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::invoke(std::forward<F>(f), std::get<N>(t)...);
  }(std::index_sequence_for<Args...>());
}

/**
 * @brief Applies a function to each element of a tuple
 * @tparam F The callable type (deduced)
 * @tparam Args The tuple element types
 * @param t The input tuple
 * @param f The function to apply (forwarding reference)
 * @return The function f (for chaining)
 */
template <typename F, typename... Args>
constexpr decltype(auto) tuple_for_each(const std::tuple<Args...> &t, F &&f) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    (std::invoke(std::forward<F>(f), std::get<N>(t)), ...);
    return f;
  }(std::index_sequence_for<Args...>());
}

/**
 * @brief Transforms a tuple with additional arguments
 * @tparam F The callable type (deduced)
 * @tparam Args The tuple element types
 * @tparam args Additional argument types (deduced)
 * @param f The transformation function (forwarding reference)
 * @param t The input tuple
 * @param rest Additional arguments (forwarding references)
 */
template <typename F, typename... Args, typename... args>
constexpr decltype(auto) tuple_transform(F &&f, const std::tuple<Args...> &t,
                                         args &&...rest) {
  [&]<size_t... N>(const std::index_sequence<N...> &) {
    (std::forward<F>(f)(std::get<N>(t), index_t<N>(),
                        std::forward<args>(rest)...),
     ...);
  }(std::index_sequence_for<Args...>());
}

/**
 * @brief Reverses the order of elements in a tuple
 * @tparam Args The tuple element types
 * @param t The input tuple
 * @return A new tuple with elements in reverse order
 */
template <typename... Args>
constexpr decltype(auto) tuple_reverse(const std::tuple<Args...> &t) {
  constexpr auto size = sizeof_v<Args...>;
  auto reversed = reverse_t<std::tuple<Args...>>();

  tuple_transform(
      [&](auto &&e, auto &&index, auto &tuple) {
        std::get<size - std::remove_cvref_t<decltype(index)>::value - 1>(
            tuple) = e;
      },
      t, reversed);

  return reversed;
};

/**
 * @brief Prepends elements to a tuple
 * @tparam Args The original tuple element types
 * @tparam T The types of elements to prepend (deduced)
 * @param tuple The original tuple
 * @param t The elements to prepend (forwarding references)
 * @return A new tuple with elements prepended
 */
template <typename... Args, typename... T>
constexpr decltype(auto) tuple_prepend(const std::tuple<Args...> &tuple,
                                       T &&...t) {
  return std::tuple_cat(std::make_tuple(std::forward<T>(t)...), tuple);
}

/**
 * @brief Appends elements to a tuple
 * @tparam Args The original tuple element types
 * @tparam T The types of elements to append (deduced)
 * @param tuple The original tuple
 * @param t The elements to append (forwarding references)
 * @return A new tuple with elements appended
 */
template <typename... Args, typename... T>
constexpr decltype(auto) tuple_append(const std::tuple<Args...> &tuple,
                                      T &&...t) {
  return std::tuple_cat(tuple, std::make_tuple(std::forward<T>(t)...));
}

/**
 * @brief Removes an element at a specific index from a tuple
 * @tparam i The index to remove
 * @tparam Args The tuple element types
 * @param t The input tuple
 * @return A new tuple with the specified element removed
 */
template <auto i, typename... Args>
constexpr decltype(auto) tuple_remove(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(erase_at_t<i, std::index_sequence_for<Args...>>());
}

/**
 * @brief Swaps two elements in a tuple
 * @tparam i First index to swap
 * @tparam j Second index to swap
 * @tparam Args The tuple element types
 * @param t The input tuple
 * @return A new tuple with elements at i and j swapped
 */
template <auto i, auto j, typename... Args>
constexpr decltype(auto) tuple_swap(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<swap_index<i, j, N>()>(t)...);
  }(std::index_sequence_for<Args...>());
}

/**
 * @brief Extracts a range of elements from a tuple
 * @tparam i Start index of range
 * @tparam j End index of range (exclusive)
 * @tparam Args The tuple element types
 * @param t The input tuple
 * @return A new tuple containing elements from i to j-1
 */
template <auto i, auto j, typename... Args>
constexpr decltype(auto) tuple_range(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(integer_sequence_t<size_t, j - i, i, 1>());
}

/**
 * @brief Transforms each element of a tuple and returns as a new tuple
 * @tparam F The transformation function type (deduced)
 * @tparam Args The tuple element types
 * @param f The transformation function (forwarding reference)
 * @param t The input tuple
 * @return A new tuple with transformed elements
 */
template <typename F, typename... Args>
constexpr decltype(auto) transform_as_tuple(F &&f,
                                            const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::tuple<std::invoke_result_t<F(Args)>...>(
        std::forward<F>(f)(std::get<N>(t))...);
  }(std::index_sequence_for<Args...>());
}

/**
 * @brief Applies a transformation to adjacent pairs of elements
 * @tparam F The transformation function type (deduced)
 * @tparam Args The tuple element types
 * @param f The transformation function (forwarding reference)
 * @param t The input tuple
 * @return A new tuple with transformed adjacent pairs
 */
template <typename F, typename... Args>
constexpr decltype(auto) tuple_adjacent_transform(
    F &&f, const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(
        std::invoke(std::forward<F>(f), std::get<N>(t), std::get<N + 1>(t))...);
  }(index_sequence_of_c<sizeof_v<Args...> - 1>());
}

/**
 * @brief Extracts a subset of elements from a tuple
 * @tparam pos Starting position of subset
 * @tparam len Length of subset
 * @tparam Args The tuple element types
 * @param t The input tuple
 * @return A new tuple containing the specified subset
 */
template <auto pos, auto len, typename... Args>
constexpr decltype(auto) tuple_subset(const std::tuple<Args...> &t) {
  return tuple_range<pos, pos + len>(t);
}

/**
 * @brief Erases a range of elements from a tuple
 * @tparam i Start index of range to erase
 * @tparam j End index of range to erase (exclusive)
 * @tparam Args The tuple element types
 * @param t The input tuple
 * @return A new tuple with specified range removed
 */
template <auto i, auto j, typename... Args>
constexpr decltype(auto) tuple_erase(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(erase_t<i, j, std::index_sequence_for<Args...>>());
}

/**
 * @brief Reverses a range within a tuple
 * @tparam i Start index of range to reverse
 * @tparam j End index of range to reverse (exclusive)
 * @tparam Args The tuple element types
 * @param t The input tuple
 * @return A new tuple with specified range reversed
 */
template <auto i, auto j, typename... Args>
constexpr decltype(auto) tuple_reverse_range(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(reverse_range_t<i, j, std::index_sequence_for<Args...>>());
}

/**
 * @brief Inserts elements into a tuple at specified position
 * @tparam i Insertion position
 * @tparam Args Original tuple element types
 * @tparam T Types of elements to insert (deduced)
 * @param tuple The original tuple
 * @param t Elements to insert (forwarding references)
 * @return A new tuple with elements inserted
 */
template <auto i, typename... Args, typename... T>
constexpr decltype(auto) tuple_insert(const std::tuple<Args...> &tuple,
                                      T &&...t) {
  return std::tuple_cat(tuple_range<0, i>(tuple),
                        std::make_tuple(std::forward<T>(t)...),
                        tuple_range<i, sizeof_v<Args...>>(tuple));
}

/**
 * @brief Replaces an element at specified position
 * @tparam i Replacement position
 * @tparam Args Original tuple element types
 * @tparam T Types of replacement elements (deduced)
 * @param tuple The original tuple
 * @param t Replacement elements (forwarding references)
 * @return A new tuple with element at position i replaced
 */
template <auto i, typename... Args, typename... T>
constexpr decltype(auto) tuple_replace(const std::tuple<Args...> &tuple,
                                       T &&...t) {
  return std::tuple_cat(tuple_range<0, i>(tuple),
                        std::make_tuple(std::forward<T>(t)...),
                        tuple_range<i + 1, sizeof_v<Args...>>(tuple));
}

/**
 * @brief Replaces all occurrences of a value in the tuple
 * @tparam Args Tuple element types
 * @tparam T Type of value to replace (deduced)
 * @tparam U Type of replacement value (deduced)
 * @param tuple The original tuple
 * @param t Value to replace (forwarding reference)
 * @param u Replacement value (forwarding reference)
 * @return A new tuple with all occurrences replaced
 */
template <typename... Args, typename T, typename U>
constexpr decltype(auto) tuple_replace(const std::tuple<Args...> &tuple, T &&t,
                                       U &&u) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple([&v = std::get<N>(tuple)](auto &&src, auto &&dst) {
      return v == src ? dst : src;
    }(std::forward<T>(t), std::forward<U>(u))...);
  }(std::index_sequence_for<Args...>());
}

/**
 * @brief Rotates a range within a tuple
 * @tparam i Start index of range
 * @tparam j End index of range (exclusive)
 * @tparam k Rotation count
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple with specified range rotated
 */
template <auto i, auto j, auto k, typename... Args>
constexpr decltype(auto) tuple_rotate(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(rotate_t<i, j, k, std::index_sequence_for<Args...>>());
}

/**
 * @brief Shifts elements left by specified amount
 * @tparam i Shift amount
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple with elements shifted left
 */
template <auto i, typename... Args>
constexpr decltype(auto) tuple_shift_left(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(shift_left_t<i, std::index_sequence_for<Args...>>());
}

/**
 * @brief Shifts elements right by specified amount
 * @tparam i Shift amount
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple with elements shifted right
 */
template <auto i, typename... Args>
constexpr decltype(auto) tuple_shift_right(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(shift_right_t<i, std::index_sequence_for<Args...>>());
}

/**
 * @brief Gets all elements except the last one
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple without the last element
 */
template <typename... Args>
constexpr decltype(auto) tuple_head(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(head_t<std::index_sequence_for<Args...>>());
}

/**
 * @brief Gets all elements except the first one
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple without the first element
 */
template <typename... Args>
constexpr decltype(auto) tuple_tail(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(tail_t<std::index_sequence_for<Args...>>());
}

/**
 * @brief Takes the first n elements from a tuple
 * @tparam n Number of elements to take
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple with first n elements
 */
template <auto n, typename... Args>
constexpr decltype(auto) tuple_take(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(index_sequence_of_c<n>());
}

/**
 * @brief Takes the last n elements from a tuple
 * @tparam n Number of elements to take
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple with last n elements
 */
template <auto n, typename... Args>
constexpr decltype(auto) tuple_take_last(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(integer_sequence_t<size_t, n, sizeof_v<Args...> - n, 1>());
}

/**
 * @brief Drops the first n elements from a tuple
 * @tparam n Number of elements to drop
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple without first n elements
 */
template <auto n, typename... Args>
constexpr decltype(auto) tuple_drop(const std::tuple<Args...> &t) {
  return tuple_take_last<sizeof_v<Args...> - n>(t);
}

/**
 * @brief Drops the last n elements from a tuple
 * @tparam n Number of elements to drop
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple without last n elements
 */
template <auto n, typename... Args>
constexpr decltype(auto) tuple_drop_last(const std::tuple<Args...> &t) {
  return tuple_take<sizeof_v<Args...> - n>(t);
}

/**
 * @brief Fills a tuple starting from specified index
 * @tparam N Starting index
 * @tparam T Tuple type (deduced)
 * @param t The tuple to fill
 */
template <size_t N, typename T>
constexpr decltype(auto) tuple_fill(T &t) {}

/**
 * @brief Fills a tuple with provided values starting from specified index
 * @tparam N Starting index
 * @tparam T Tuple type (deduced)
 * @tparam U Type of first value (deduced)
 * @tparam Args Types of remaining values (deduced)
 * @param t The tuple to fill
 * @param u First value to fill (forwarding reference)
 * @param args Remaining values to fill (forwarding references)
 */
template <size_t N, typename T, typename U, typename... Args>
constexpr decltype(auto) tuple_fill(T &t, U &&u, Args &&...args) {
  if constexpr (N < sizeof_t_v<T>) {
    std::get<N>(t) = u;
    tuple_fill<N + 1>(t, std::forward<Args>(args)...);
  }
}

/**
 * @brief Repeats an element at specified index n times
 * @tparam i Index of element to repeat
 * @tparam n Number of repetitions
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple with repeated element
 */
template <auto i, auto n, typename... Args>
constexpr decltype(auto) tuple_splat(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<typev<identity_t<N, index_t<i>>>>(t)...);
  }(index_sequence_of_c<n>());
}

/**
 * @brief Removes duplicate elements from a tuple
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple with duplicates removed
 */
template <typename... Args>
constexpr decltype(auto) tuple_unique(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(unique_index_t<std::tuple<Args...>>());
}

/**
 * @brief Slices a tuple using specified indices
 * @tparam indices The indices sequence to slice with
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple with elements from specified indices
 */
template <typename indices, typename... Args>
constexpr decltype(auto) tuple_slice(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(indices());
}

/**
 * @brief Creates a view of a tuple's elements
 * @tparam T Tuple type (deduced)
 * @param t The input tuple (forwarding reference)
 * @return A callable that provides access to tuple elements
 */
template <typename T>
constexpr decltype(auto) tuple_view(T &&t) {
  return []<auto... N>(const std::index_sequence<N...> &) {
    return
        []<typename F>(F &&f) { return call_operator<N...>(decltype(f)(f)); };
  }(index_sequence_of_t<std::remove_cvref_t<T>>())([&]<auto... N>() {
    return std::forward_as_tuple(std::get<N>(std::forward<T>(t))...);
  });
}

/**
 * @brief Creates a tuple by repeating a value n times
 * @tparam N Number of repetitions
 * @tparam T Type of value to repeat (deduced)
 * @param t The value to repeat
 * @return A tuple with N copies of the value
 */
template <size_t N, typename T>
constexpr decltype(auto) tuple_repeat(const T &t) {
  return [&]<size_t... M>(const std::index_sequence<M...> &) {
    return std::make_tuple(ignore<M>(t)...);
  }(index_sequence_of_c<N>());
}

/**
 * @brief Filters a tuple based on a predicate
 * @tparam F Predicate template
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple with elements satisfying the predicate
 */
template <template <typename...> typename F, typename... Args>
constexpr decltype(auto) tuple_filter(const std::tuple<Args...> &t) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return std::make_tuple(std::get<N>(t)...);
  }(find_index_t<F, std::tuple<Args...>>());
}

/**
 * @brief Gets the Nth element as a tuple if unique
 * @tparam N Element index
 * @tparam T Tuple type (deduced)
 * @param t The input tuple (forwarding reference)
 * @return A tuple containing the element if unique, empty tuple otherwise
 */
template <size_t N, typename T>
constexpr decltype(auto) element_as_tuple(T &&t) {
  using type = std::remove_cvref_t<T>;

  return [&]<auto... n>(const std::index_sequence<n...> &) {
    using curr = std::tuple_element_t<N, type>;

    if constexpr (!(std::is_same_v<curr, std::tuple_element_t<n, type>> || ...))
      return std::tuple<curr>(std::get<N>(std::forward<T>(t)));
    else
      return std::make_tuple();
  }(index_sequence_of_c<N>());
}

/**
 * @brief Concatenates tuples while removing duplicates
 * @tparam Args Tuple types to concatenate (deduced)
 * @param args Tuples to concatenate (forwarding references)
 * @return A new tuple with all unique elements
 */
template <typename... Args>
constexpr decltype(auto) tuple_cat_unique(Args &&...args) {
  auto tuple = std::tuple_cat(std::forward<Args>(args)...);

  return [&]<auto... N>(const std::index_sequence<N...> &) {
    return std::tuple_cat(element_as_tuple<N>(std::move(tuple))...);
  }(index_sequence_of_c<(sizeof_t_v<std::decay_t<Args>> + ...)>());
}

/**
 * @brief Divides a tuple according to a strategy
 * @tparam F Division strategy template
 * @tparam N Division parameter
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A tuple of tuples divided according to strategy
 */
template <template <auto, typename> typename F, auto N, typename... Args>
constexpr decltype(auto) tuple_divvy(const std::tuple<Args...> &t) {
  return [&]<typename... indices>(std::tuple<indices...>) {
    return std::make_tuple(tuple_slice<indices>(t)...);
  }(typeof_t<F<N, std::index_sequence_for<Args...>>>());
}

/**
 * @brief Divides a tuple into chunks of size N
 * @tparam N Chunk size
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A tuple of tuples where each inner tuple has N elements
 */
template <auto N, typename... Args>
constexpr decltype(auto) tuple_chunk(const std::tuple<Args...> &t) {
  return tuple_divvy<chunk, N>(t);
}

/**
 * @brief Creates sliding windows of size N over the tuple
 * @tparam N Window size
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A tuple of tuples representing the sliding windows
 */
template <auto N, typename... Args>
constexpr decltype(auto) tuple_slide(const std::tuple<Args...> &t) {
  return tuple_divvy<slide, N>(t);
}

/**
 * @brief Takes every Nth element from the tuple
 * @tparam N Stride value
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A new tuple with strided elements
 */
template <auto N, typename... Args>
constexpr decltype(auto) tuple_stride(const std::tuple<Args...> &t) {
  return tuple_slice<stride_t<N, std::index_sequence_for<Args...>>>(t);
}

/**
 * @brief Divides tuple into all possible subranges of size N
 * @tparam N Subrange size
 * @tparam Args Tuple element types
 * @param t The input tuple
 * @return A tuple containing all possible subranges
 */
template <auto N, typename... Args>
constexpr decltype(auto) tuple_subranges(const std::tuple<Args...> &t) {
  return tuple_divvy<subranges, N>(t);
}

/**
 * @brief Concatenates tuples with proper forwarding
 * @tparam Args Tuple types to concatenate (deduced)
 * @param args Tuples to concatenate (forwarding references)
 * @return A new tuple containing all elements
 */
template <typename... Args>
constexpr decltype(auto) tuple_cat(Args &&...args) {
  auto t = std::forward_as_tuple(std::forward<Args>(args)...);

  if constexpr (!sizeof...(Args))
    return t;
  else {
    return [&]<template <typename...> typename T, template <auto...> typename U,
               auto... m, auto... n>(T<U<m, n>...>) {
      return std::forward_as_tuple(std::get<n>(std::get<m>(t))...);
    }(rank_pack(std::forward<Args>(args)...));
  }
}

/**
 * @brief Gets the size of the Nth element in a tuple
 * @tparam N Element index
 * @tparam T Tuple type
 */
template <auto N, typename T>
inline constexpr auto tuple_size_of = sizeof_t_v<std::tuple_element_t<N, T>>;

}  // namespace core::meta::utils