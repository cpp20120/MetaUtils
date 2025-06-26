#pragma once

namespace core::meta::utils {

/**
 * @brief Template for declaring a friend function that can be later
 * specialized.
 * @tparam I The unique identifier for this Getter instance.
 *
 * This serves as a forward declaration of a friend function that will be
 * specialized by Injector to store values associated with specific types.
 */
template <auto I>
struct Getter {
  friend constexpr auto Magic(Getter<I>);
};

/**
 * @brief Injects a specialization of Magic for a specific Getter instance.
 * @tparam I The identifier to associate with the value.
 * @tparam Value The value to store (defaults to 0).
 *
 * This specializes the Magic function for Getter<I> to return Value,
 * effectively creating a compile-time key-value store.
 */
template <auto I, auto Value = 0>
struct Injector {
  friend constexpr auto Magic(Getter<I>) { return Value; };
};

/**
 * @brief A type list container for holding multiple types.
 * @tparam Ts... The types contained in the list.
 */
template <typename... Ts>
struct TypeList {};

/**
 * @brief A constexpr instance of TypeList for convenience.
 * @tparam Ts... The types contained in the list.
 */
template <typename... Ts>
constexpr TypeList<Ts...> kTypeList{};

/**
 * @brief Associates a tag with a type for unique identification.
 * @tparam Tag A unique tag type to distinguish different type contexts.
 * @tparam T The type to be associated with the tag.
 */
template <typename Tag, typename T>
struct Type {};

/**
 * @brief A helper function that ignores all arguments.
 * @param ... Variadic arguments to be ignored.
 * @return Nothing (void).
 *
 * Used for forcing expression evaluation in requires clauses.
 */
consteval auto Ignore(...) {};

/**
 * @brief Implementation of type list comparison.
 * @tparam F A unique tag type (defaults to a unique lambda).
 * @tparam Ts... Types from the first list.
 * @tparam TTs... Types from the second list.
 * @param first Dummy parameter for the first type list.
 * @param second Dummy parameter for the second type list.
 * @return true if all types in the first list exist in the second list.
 *
 * This works by injecting Magic specializations for types in the second list,
 * then checking if all types from the first list have corresponding Magic
 * specializations.
 */
template <typename F = decltype([] {}), typename... Ts, typename... TTs>
consteval auto CompareImpl(TypeList<Ts...>, TypeList<TTs...>) {
  Ignore(Injector<Type<F, TTs>{}, 0>{}...);  // NOLINT

  return requires { Ignore(Magic(Getter<Type<F, Ts>{}>{})...); };
};

/**
 * @brief Compares two type lists for equality (same types regardless of order
 * or duplicates).
 * @tparam Ts... Types from the first list.
 * @tparam TTs... Types from the second list.
 * @tparam F First unique tag (defaults to a unique lambda).
 * @tparam FF Second unique tag (defaults to a different unique lambda).
 * @param first The first type list to compare.
 * @param second The second type list to compare.
 * @return true if both lists contain exactly the same set of types, false
 * otherwise.
 *
 * Performs bidirectional comparison to ensure both lists contain all types from
 * each other.
 */
template <typename... Ts, typename... TTs, auto F = [] {}, auto FF = [] {}>
consteval auto Compare(TypeList<Ts...> first, TypeList<TTs...> second) {
  return CompareImpl<decltype(F)>(first, second) &&
         CompareImpl<decltype(FF)>(second, first);
};

static_assert(Compare(kTypeList<int, float>, kTypeList<float, int>));
static_assert(Compare(kTypeList<int, float>, kTypeList<int, float>));
static_assert(Compare(kTypeList<int, float>,
                      kTypeList<int, int, float, float>));

static_assert(!Compare(kTypeList<int, float>, kTypeList<int>));
static_assert(!Compare(kTypeList<int, float>, kTypeList<int, double>));
static_assert(!Compare(kTypeList<int>, kTypeList<int, float>));
static_assert(!Compare(kTypeList<int, double>, kTypeList<int, float>));

}