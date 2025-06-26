#pragma once

namespace core::meta::utils {

template<auto I> struct Getter {
  friend constexpr auto Magic(Getter<I>);
};
template <auto I, auto Value = 0>
struct Injector {
  friend constexpr auto Magic(Getter<I>) { return Value; };
};

template <typename... Ts>
struct TypeList {};

template <typename... Ts>
constexpr TypeList<Ts...> kTypeList{};

template <typename Tag, typename T>
struct Type {};

consteval auto Ignore(...) {};

template <typename F = decltype([] {}), typename... Ts, typename... TTs>
consteval auto CompareImpl(TypeList<Ts...>, TypeList<TTs...>) {
  Ignore(Injector<Type<F, TTs>{}, 0>{}...);  // NOLINT

  return requires { Ignore(Magic(Getter<Type<F, Ts>{}>{})...); };
};

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