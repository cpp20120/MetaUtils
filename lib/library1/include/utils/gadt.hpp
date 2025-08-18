#pragma once

#include <array>
#include <charconv>
#include <type_traits>
#include <utility>

namespace core::meta::utils {

template <typename T>
struct Expr {
  using result_type = T;
};
template <auto N>
struct Lit : Expr<decltype(N)> {
  static constexpr auto value = N;
};
template <char Name>
struct Var : Expr<int> {
  static constexpr char name = Name;
};

template <typename X>
struct Neg : Expr<typename X::result_type> {
  using arg = X;
};
template <typename L, typename R>
struct Add : Expr<typename L::result_type> {
  using left = L;
  using right = R;
};
template <typename L, typename R>
struct Sub : Expr<typename L::result_type> {
  using left = L;
  using right = R;
};
template <typename L, typename R>
struct Mul : Expr<typename L::result_type> {
  using left = L;
  using right = R;
};
template <typename L, typename R>
struct Div : Expr<typename L::result_type> {
  using left = L;
  using right = R;
};
template <typename B, typename E>
struct Pow : Expr<typename B::result_type> {
  using left = B;
  using right = E;
};

template <typename T>
struct Eval;
template <auto N>
struct Eval<Lit<N>> {
  static constexpr auto value = N;
};
template <char X>
struct Eval<Var<X>> {
  static_assert(X != '?', "Var in Eval without env");
};
template <typename X>
struct Eval<Neg<X>> {
  static constexpr auto value = -Eval<X>::value;
};
template <typename L, typename R>
struct Eval<Add<L, R>> {
  static constexpr auto value = Eval<L>::value + Eval<R>::value;
};
template <typename L, typename R>
struct Eval<Sub<L, R>> {
  static constexpr auto value = Eval<L>::value - Eval<R>::value;
};
template <typename L, typename R>
struct Eval<Mul<L, R>> {
  static constexpr auto value = Eval<L>::value * Eval<R>::value;
};
template <typename L, typename R>
struct Eval<Div<L, R>> {
  static constexpr auto value = Eval<L>::value / Eval<R>::value;
};
template <typename B, typename E>
struct Eval<Pow<B, E>> {
  static constexpr auto value = [] {
    auto b = Eval<B>::value;
    auto e = Eval<E>::value;
    int r = 1;
    while (e--) r *= b;
    return r;
  }();
};

template <typename T>
struct Simplify {
  using type = T;
};
template <auto N>
struct Simplify<Lit<N>> {
  using type = Lit<N>;
};

template <typename X>
struct Simplify<Neg<X>> {
  using S = typename Simplify<X>::type;
  using type = std::conditional_t<std::is_same_v<S, Lit<0>>, Lit<0>, Neg<S>>;
};

template <typename L, typename R>
struct Simplify<Add<L, R>> {
  using SL = typename Simplify<L>::type;
  using SR = typename Simplify<R>::type;
  using type = std::conditional_t<
      std::is_same_v<SL, Lit<0>>, SR,
      std::conditional_t<
          std::is_same_v<SR, Lit<0>>, SL,
          std::conditional_t<std::is_same_v<SL, Lit<SL::value>> &&
                                 std::is_same_v<SR, Lit<SR::value>>,
                             Lit<SL::value + SR::value>, Add<SL, SR>>>>;
};

template <typename L, typename R>
struct Simplify<Sub<L, R>> {
  using SL = typename Simplify<L>::type;
  using SR = typename Simplify<R>::type;
  using type = std::conditional_t<std::is_same_v<SR, Lit<0>>, SL, Sub<SL, SR>>;
};

template <typename L, typename R>
struct Simplify<Mul<L, R>> {
  using SL = typename Simplify<L>::type;
  using SR = typename Simplify<R>::type;
  using type = std::conditional_t<
      std::is_same_v<SL, Lit<1>>, SR,
      std::conditional_t<
          std::is_same_v<SR, Lit<1>>, SL,
          std::conditional_t<std::is_same_v<SL, Lit<SL::value>> &&
                                 std::is_same_v<SR, Lit<SR::value>>,
                             Lit<SL::value * SR::value>, Mul<SL, SR>>>>;
};

template <typename L, typename R>
struct Simplify<Div<L, R>> {
  using SL = typename Simplify<L>::type;
  using SR = typename Simplify<R>::type;
  using type = std::conditional_t<
      std::is_same_v<SR, Lit<1>>, SL,
      std::conditional_t<std::is_same_v<SL, Lit<SL::value>> &&
                             std::is_same_v<SR, Lit<SR::value>>,
                         Lit<SL::value / SR::value>, Div<SL, SR>>>;
};

template <typename B, typename E>
struct Simplify<Pow<B, E>> {
  using SB = typename Simplify<B>::type;
  using SE = typename Simplify<E>::type;
  using type = std::conditional_t<
      std::is_same_v<SE, Lit<0>>, Lit<1>,
      std::conditional_t<
          std::is_same_v<SE, Lit<1>>, SB,
          std::conditional_t<std::is_same_v<SB, Lit<SB::value>> &&
                                 std::is_same_v<SE, Lit<SE::value>>,
                             Lit<Eval<Pow<SB, SE>>::value>, Pow<SB, SE>>>>;
};

struct EmptyEnv {
  template <char>
  static constexpr int get() {
    static_assert(true, "var not bound");
    return 0;
  }
};

template <char K, int V, typename Rest = EmptyEnv>
struct Env : Rest {
  template <char X>
  static constexpr int get() {
    if constexpr (X == K)
      return V;
    else
      return Rest::template get<X>();
  }
};

template <typename ExprT, typename EnvT>
constexpr auto eval(const ExprT&, const EnvT& env) {
  if constexpr (std::is_base_of_v<Lit<ExprT::value>, ExprT>)
    return Eval<ExprT>::value;
  else if constexpr (std::is_base_of_v<Var<ExprT::name>, ExprT>)
    return env.template get<ExprT::name>();
  else if constexpr (std::is_same_v<ExprT, Add<typename ExprT::left,
                                               typename ExprT::right>>)
    return eval(typename ExprT::left{}, env) +
           eval(typename ExprT::right{}, env);
  else if constexpr (std::is_same_v<ExprT, Sub<typename ExprT::left,
                                               typename ExprT::right>>)
    return eval(typename ExprT::left{}, env) -
           eval(typename ExprT::right{}, env);
  else if constexpr (std::is_same_v<ExprT, Mul<typename ExprT::left,
                                               typename ExprT::right>>)
    return eval(typename ExprT::left{}, env) *
           eval(typename ExprT::right{}, env);
  else if constexpr (std::is_same_v<ExprT, Div<typename ExprT::left,
                                               typename ExprT::right>>)
    return eval(typename ExprT::left{}, env) /
           eval(typename ExprT::right{}, env);
  else if constexpr (std::is_same_v<ExprT, Pow<typename ExprT::left,
                                               typename ExprT::right>>) {
    auto b = eval(typename ExprT::left{}, env);
    auto e = eval(typename ExprT::right{}, env);
    int r = 1;
    for (int i = 0; i < e; ++i) r *= b;
    return r;
  }
}

template <char X, typename T>
struct Deriv {
  using type = Lit<0>;
};
template <char X>
struct Deriv<X, Var<X>> {
  using type = Lit<1>;
};
template <char X, char Y>
struct Deriv<X, Var<Y>> {
  using type = Lit<0>;
};
template <char X, auto N>
struct Deriv<X, Lit<N>> {
  using type = Lit<0>;
};

template <char X, typename L, typename R>
struct Deriv<X, Add<L, R>> {
  using type = Add<typename Deriv<X, L>::type, typename Deriv<X, R>::type>;
};
template <char X, typename L, typename R>
struct Deriv<X, Sub<L, R>> {
  using type = Sub<typename Deriv<X, L>::type, typename Deriv<X, R>::type>;
};
template <char X, typename L, typename R>
struct Deriv<X, Mul<L, R>> {
  using type = Add<Mul<typename Deriv<X, L>::type, R>,
                   Mul<L, typename Deriv<X, R>::type>>;
};

template <char X, typename A>
struct Deriv<X, Neg<A>> {
  using type = Neg<typename Deriv<X, A>::type>;
};
template <char X, typename L, typename R>
struct Deriv<X, Div<L, R>> {
  using Num = Sub<Mul<typename Deriv<X, L>::type, R>,
                  Mul<L, typename Deriv<X, R>::type>>;
  using Den = Pow<R, Lit<2>>;
  using type = Div<Num, Den>;
};
template <char X, typename B, typename E>
struct Deriv<X, Pow<B, E>> {
  using type = Mul<E, Mul<Pow<B, Sub<E, Lit<1>>>, typename Deriv<X, B>::type>>;
};

template <typename T>
struct ToString;
template <auto N>
struct ToString<Lit<N>> {
  static constexpr auto value() {
    char buf[24] = {};
    auto [p, ec] = std::to_chars(buf, buf + 23, N);
    std::array<char, 32> out{};
    for (int i = 0; i < (p - buf); ++i) out[i] = buf[i];
    return out;
  }
};

template <char Name>
struct ToString<Var<Name>> {
  static constexpr auto value() { return std::array<char, 2>{Name, '\0'}; }
};

#define DECL_TOSTR(OP, CH)                                     \
  template <typename L, typename R>                            \
  struct ToString<OP<L, R>> {                                  \
    static constexpr auto value() {                            \
      auto l = ToString<L>::value(), r = ToString<R>::value(); \
      std::array<char, 64> o{};                                \
      size_t i = 0;                                            \
      o[i++] = '(';                                            \
      for (char c : l)                                         \
        if (c) o[i++] = c;                                     \
      o[i++] = CH;                                             \
      for (char c : r)                                         \
        if (c) o[i++] = c;                                     \
      o[i++] = ')';                                            \
      return o;                                                \
    }                                                          \
  }
DECL_TOSTR(Add, '+');
DECL_TOSTR(Sub, '-');
DECL_TOSTR(Mul, '*');
DECL_TOSTR(Div, '/');
DECL_TOSTR(Pow, '^');
template <typename X>
struct ToString<Neg<X>> {
  static constexpr auto value() {
    auto x = ToString<X>::value();
    std::array<char, 64> o{};
    size_t i = 0;
    o[i++] = '-';
    for (char c : x)
      if (c) o[i++] = c;
    return o;
  }
};

template <std::size_t N>
struct Parser {
  const char (&s)[N];
  consteval std::size_t skip(std::size_t i) const {
    while (i < N && (s[i] == ' ' || s[i] == '\t')) ++i;
    return i;
  }

  consteval auto atom(std::size_t i) const {
    i = skip(i);
    if (s[i] == '(') {
      auto [e, ni] = expr(i + 1);
      return std::pair{e, skip(ni) + 1};
    }
    if (s[i] == '-') {
      auto [e, ni] = atom(i + 1);
      return std::pair{Neg<decltype(e)>{}, ni};
    }
    if (s[i] >= 'a' && s[i] <= 'z') {
      return std::pair{Var<s[i]>{}, i + 1};
    }

    int v = 0;
    auto [p, ec] = std::from_chars(s + i, s + N, v);
    return std::pair{Lit<v>{}, static_cast<std::size_t>(p - s)};
  }

  consteval auto pow_(std::size_t i) const {
    auto [lhs, ni] = atom(i);
    ni = skip(ni);
    while (ni < N && s[ni] == '^') {
      auto [rhs, n2] = pow_(ni + 1);
      lhs = Pow<decltype(lhs), decltype(rhs)>{};
      ni = skip(n2);
    }
    return std::pair{lhs, ni};
  }

  consteval auto mul(std::size_t i) const {
    auto [l, ni] = pow_(i);
    ni = skip(ni);
    while (ni < N && (s[ni] == '*' || s[ni] == '/')) {
      char op = s[ni];
      auto [r, n2] = pow_(ni + 1);
      l = (op == '*' ? Mul<decltype(l), decltype(r)>{}
                     : Div<decltype(l), decltype(r)>{});
      ni = skip(n2);
    }
    return std::pair{l, ni};
  }

  consteval auto expr(std::size_t i) const {
    auto [l, ni] = mul(i);
    ni = skip(ni);
    while (ni < N && (s[ni] == '+' || s[ni] == '-')) {
      char op = s[ni];
      auto [r, n2] = mul(ni + 1);
      l = (op == '+' ? Add<decltype(l), decltype(r)>{}
                     : Sub<decltype(l), decltype(r)>{});
      ni = skip(n2);
    }
    return std::pair{l, ni};
  }

  consteval auto parse() const {
    using raw = decltype(expr(0).first);
    return typename Simplify<raw>::type{};
  }
};

template <char... Cs>
consteval auto operator"" _expr() {
  constexpr char str[]{Cs..., '\0'};
  return Parser<sizeof...(Cs) + 1>{str}.parse();
}

template <typename T, template <typename...> class Primary>
struct is_specialization : std::false_type {};

template <template <typename...> class Primary, typename... Args>
struct is_specialization<Primary<Args...>, Primary> : std::true_type {};

template <typename T, template <typename...> class Primary>
inline constexpr bool is_specialization_v =
    is_specialization<T, Primary>::value;

template <typename T>
struct Matcher {
  T expr;
  template <typename Node, typename Fn>
  constexpr auto on(Fn&& fn) const {
    if constexpr (std::is_same_v<T, Node>)
      return fn(expr);
    else
      return *this;
  }
  template <template <typename...> class Op, typename Fn>
  constexpr auto on(Fn&& fn) const {
    if constexpr (is_specialization_v<T, Op>)
      return fn(expr);
    else
      return *this;
  }
  template <typename Fn>
  constexpr auto otherwise(Fn&& fn) const {
    return fn(expr);
  }
};
template <typename T>
constexpr auto match(T&& e) {
  return Matcher<T>{e};
}
}  // namespace core::meta::utils