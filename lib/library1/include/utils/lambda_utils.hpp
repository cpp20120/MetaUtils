/**
 * @file
 * @brief Lambda-based tuple manipulation utilities.
 *
 * This header provides a set of utilities for manipulating and composing
 * tuples using lambda expressions, enabling functional-style operations
 * on parameter packs.
 */
#pragma once 

#include <utility>

namespace core::meta::utils {
/**
 * @brief Creates a lambda tuple from arguments.
 * @tparam Args Argument types
 * @param args Arguments to wrap
 * @return A callable that when invoked with a function, applies that function to the stored arguments
 */
template <typename... Args>
constexpr decltype(auto) make_lambda_tuple(Args &&...args) {
  return [... args = element_wrapper<Args>(args)]<typename F>(
             F &&f) mutable -> decltype(auto) {
    return std::invoke(std::forward<F>(f), args.value...);
  };
}
/**
 * @brief Gets the Nth element from a lambda tuple.
 * @tparam N Index of element to get
 * @tparam F Lambda tuple type
 * @param f Lambda tuple to access
 * @return The Nth element of the tuple
 */
template <size_t N, typename F>
constexpr decltype(auto) lambda_tuple_get(F &&f) {
  return std::invoke(std::forward<F>(f),
                     []<typename... Args>(Args &&...args) -> decltype(auto) {
                       return nth_value_v<N>(std::forward<Args>(args)...);
                     });
}
/**
 * @brief Applies a function to a lambda tuple.
 * @tparam F Function type
 * @tparam T Lambda tuple type
 * @param f Function to apply
 * @param t Lambda tuple to apply to
 * @return Result of applying f to tuple elements
 */
template <typename F, typename T>
constexpr decltype(auto) lambda_tuple_apply(F &&f, T &&t) {
  return std::invoke(std::forward<T>(t),
                     [&]<typename... Args>(Args &&...args) -> decltype(auto) {
                       return std::invoke(std::forward<F>(f),
                                          std::forward<Args>(args)...);
                     });
}
/**
 * @brief Applies a function to each element of a lambda tuple.
 * @tparam F Function type
 * @tparam T Lambda tuple type
 * @param f Function to apply
 * @param t Lambda tuple to iterate
 * @return The original function (for chaining)
 */
template <typename F, typename T>
constexpr decltype(auto) lambda_tuple_for_each(F &&f, T &&t) {
  return std::invoke(
      std::forward<T>(t),
      [&]<typename... Args>(Args &&...args) -> decltype(auto) {
        (std::invoke(std::forward<F>(f), std::forward<Args>(args)), ...);

        return f;
      });
}
/**
 * @brief Gets the size of a lambda tuple.
 * @tparam F Lambda tuple type
 * @param f Lambda tuple to measure
 * @return Number of elements in the tuple
 */
template <typename F>
constexpr decltype(auto) lambda_tuple_size(F &&f) {
  return std::invoke(std::forward<F>(f),
                     []<typename... Args>(Args &&...args) -> decltype(auto) {
                       return sizeof...(Args);
                     });
}
/**
 * @brief Base case for lambda capture concatenation.
 * @tparam F Callable type
 * @param f Callable to return
 * @return The input callable
 */
template <typename F>
constexpr decltype(auto) lambda_capture_cat(F &&f) {
  return std::invoke(std::forward<F>(f));
}
/**
 * @brief Concatenates lambda captures from multiple callables.
 * @tparam F Callable type
 * @tparam T First callable to concatenate
 * @tparam Args Remaining callables
 * @param f Primary callable
 * @param t First callable to concatenate
 * @param args Remaining callables
 * @return A new callable combining all captures
 */
template <typename F, typename T, typename... Args>
constexpr decltype(auto) lambda_capture_cat(F &&f, T &&t, Args &&...args);
/**
 * @brief Concatenates two callables' captures.
 * @tparam F First callable type
 * @tparam T Second callable type
 * @param f First callable
 * @param t Second callable
 * @return A new callable combining both captures
 */
template <typename F, typename T>
constexpr decltype(auto) lambda_capture_cat(F &&f, T &&t) {
  return [&f, &t]<typename... Outer>(Outer &&...outer) {
    return std::invoke(std::forward<T>(t), [&f, &outer...]<typename... Inner>(
                                               Inner &&...inner) {
      return std::invoke(std::forward<F>(f), std::forward<Inner>(inner)...,
                         std::forward<Outer>(outer)...);
    });
  };
}

template <typename F, typename T, typename... Args>
constexpr decltype(auto) lambda_capture_cat(F &&f, T &&t, Args &&...args) {
  return lambda_capture_cat(
      [&f, &t]<typename... Outer>(Outer &&...outer) {
        return std::invoke(
            std::forward<T>(t),
            [&f, &outer...]<typename... Inner>(Inner &&...inner) {
              return std::invoke(std::forward<F>(f),
                                 std::forward<Inner>(inner)...,
                                 std::forward<Outer>(outer)...);
            });
      },
      std::forward<Args>(args)...);
}

template <typename F, typename... Args>
constexpr decltype(auto) lambda_tuple_cat(F &&f, Args &&...args) {
  return lambda_capture_cat(
      [&f]<typename... Outer>(Outer &&...outer) {
        return std::invoke(std::forward<F>(f), [&outer...]<typename... Inner>(
                                                   Inner &&...inner) {
          return make_lambda_tuple(std::forward<Inner>(inner)...,
                                   std::forward<Outer>(outer)...);
        });
      },
      std::forward<Args>(args)...);
}
/**
 * @brief Creates a reversed concatenation of lambda tuples.
 * @tparam F First tuple type
 * @tparam Args Additional tuple types
 * @param f First tuple
 * @param args Additional tuples
 * @return A new tuple with elements in reverse order
 */
template <typename F, typename... Args>
constexpr decltype(auto) lambda_tuple_reverse_cat(F &&f, Args &&...args) {
  constexpr auto M = sizeof_v<Args...>;

  return [&]<size_t... N>(const std::index_sequence<N...> &) -> decltype(auto) {
    return lambda_tuple_cat(
        nth_value_v<M - N - 1>(std::forward<Args>(args)...)...,
        std::forward<F>(f));
  }(std::make_index_sequence<M>());
}
/**
 * @brief Selects elements from a lambda tuple using an index sequence.
 * @tparam indices Index sequence type
 * @tparam F Lambda tuple type
 * @param f Lambda tuple to select from
 * @return New tuple with selected elements
 */
template <typename indices, typename F>
constexpr decltype(auto) lambda_tuple_select(F &&f) {
  return [&]<size_t... N>(const std::index_sequence<N...> &) {
    return make_lambda_tuple(lambda_tuple_get<N>(std::forward<F>(f))...);
  }(indices());
}
/**
 * @brief Extracts specific elements from a lambda tuple.
 * @tparam N Indices of elements to extract
 * @tparam F Lambda tuple type
 * @param f Lambda tuple to extract from
 * @return New tuple with extracted elements
 */
template <auto... N, typename F>
constexpr decltype(auto) lambda_tuple_extract(F &&f) {
  return lambda_tuple_select<std::index_sequence<N...>>(std::forward<F>(f));
}
/**
 * @brief Gathers specified elements from a lambda tuple.
 * @tparam Args Index types to gather
 * @tparam F Lambda tuple type
 * @param f Lambda tuple to gather from
 * @return New tuple with gathered elements
 */
template <typename... Args, typename F>
decltype(auto) lambda_tuple_gather(F &&f) {
  return make_lambda_tuple(lambda_tuple_get<Args>(std::forward<F>(f))...);
}
/**
 * @brief Invokes a function with specific elements from a lambda tuple.
 * @tparam N Indices of elements to pass
 * @tparam T Lambda tuple type
 * @tparam F Function type
 * @param t Lambda tuple to extract from
 * @param f Function to invoke
 * @return Result of function invocation
 */
template <auto... N, typename T, typename F>
decltype(auto) lambda_tuple_gather_invoke(T &&t, F &&f) {
  return std::invoke(std::forward<F>(f),
                     lambda_tuple_get<N>(std::forward<T>(t))...);
}

template <typename... Args, typename T, typename F>
decltype(auto) lambda_tuple_gather_invoke(T &&t, F &&f) {
  return std::invoke(std::forward<F>(f),
                     lambda_tuple_get<Args>(std::forward<T>(t))...);
}
/**
 * @brief Reverses the elements of a lambda tuple.
 * @tparam F Lambda tuple type
 * @param f Lambda tuple to reverse
 * @return New tuple with reversed elements
 */
template <typename F>
constexpr decltype(auto) lambda_tuple_reverse(F &&f) {
  return std::invoke(
      std::forward<F>(f),
      [&f]<typename... Args>(Args &&...args) -> decltype(auto) {
        return lambda_tuple_select<reverse_index_sequence<sizeof_v<Args...>>>(
            std::forward<F>(f));
      });
};
/**
 * @brief Gets a range of elements from a lambda tuple.
 * @tparam i Start index
 * @tparam j End index
 * @tparam F Lambda tuple type
 * @param f Lambda tuple to slice
 * @return New tuple with elements in range [i,j)
 */
template <auto i, auto j, typename F>
constexpr decltype(auto) lambda_tuple_range(F &&f) {
  return lambda_tuple_select<index_range<i, j>>(std::forward<F>(f));
}
/**
 * @brief Gets the first element of a specific type from a lambda tuple.
 * @tparam T Type to find
 * @tparam F Lambda tuple type
 * @param f Lambda tuple to search
 * @return The first element of type T
 */
template <typename T, typename F>
constexpr decltype(auto) lambda_tuple_get(F &&f) {
  return std::invoke(
      std::forward<F>(f),
      []<typename... Args>(Args &&...args) -> decltype(auto) {
        return nth_value_v<find_v<T, std::tuple<std::decay_t<Args>...>>>(
            std::forward<Args>(args)...);
      });
};
/**
 * @brief Rotates elements in a lambda tuple.
 * @tparam i First rotation index
 * @tparam j Second rotation index
 * @tparam k Third rotation index
 * @tparam F Lambda tuple type
 * @param f Lambda tuple to rotate
 * @return New tuple with rotated elements
 */
template <auto i, auto j, auto k, typename F>
constexpr decltype(auto) lambda_tuple_rotate(F &&f) {
  return std::invoke(std::forward<F>(f),
                     [&f]<typename... Args>(Args &&...args) -> decltype(auto) {
                       return lambda_tuple_select<
                           rotate_t<i, j, k, std::index_sequence_for<Args...>>>(
                           std::forward<F>(f));
                     });
}
/**
 * @brief Left-fold over a lambda tuple.
 * @tparam F Function type
 * @tparam T Lambda tuple type
 * @param f Binary operation
 * @param t Tuple to fold
 * @return Result of folding
 */
template <typename F, typename T>
constexpr decltype(auto) lambda_tuple_foldl(F &&f, T &&t) {
  return lambda_tuple_apply(foldl_over(std::forward<F>(f)), std::forward<T>(t));
}
/**
 * @brief Right-fold over a lambda tuple.
 * @tparam F Function type
 * @tparam T Lambda tuple type
 * @param f Binary operation
 * @param t Tuple to fold
 * @return Result of folding
 */
template <typename F, typename T>
constexpr decltype(auto) lambda_tuple_foldr(F &&f, T &&t) {
  return lambda_tuple_apply(foldr_over(std::forward<F>(f)), std::forward<T>(t));
}
}  // namespace core::meta::utils