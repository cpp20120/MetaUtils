#pragma once

#include "./auto_pack.hpp"
#include "./sequence.hpp"
#include "./fill.hpp"
#include "../meta_funcs/pair_triple_utility.hpp"
#include "../meta_funcs/metafuncs_utility.hpp"

namespace core::meta::utils {

template <typename T>
using rank = std::make_index_sequence<pack_size_v<std::decay_t<T>>>;

template <auto m, template <typename, auto...> typename T, typename U,
          auto... n>
constexpr decltype(auto) over(T<U, n...>) {
  return type_pack<auto_pack<m, n>...>();
}

template <typename... Args>
constexpr decltype(auto) rank_pack(Args &&...args) {
  return []<auto... N>(std::index_sequence<N...>) {
    return (... + over<N>(rank<Args>()));
  }(std::index_sequence_for<Args...>());
}

template <typename T>
struct matrix_index_sequences {
  template <auto N, typename U>
  using outer = fill_c<sizeof_t_v<element_t<N, U>>, N>;

  template <auto N, typename U>
  using inner = index_sequence_of_t<element_t<N, U>>;

  template <template <auto, typename> typename F>
  using call = concat_map_t<F, T>;

  using type = pair_t<alter_t<call<outer>>, call<inner>>;
};

template <typename T>
using matrix_index_sequences_t = typeof_t<matrix_index_sequences<T>>;

template <typename F, typename... Args>
constexpr decltype(auto) multi_apply(F &&f, Args &&...args) {
  using type = std::tuple<std::decay_t<Args>...>;
  using pair = matrix_index_sequences_t<type>;

  if constexpr (sizeof_t_v<type> == 0)
    return std::forward<F>(f)();
  else
    return [&]<typename T, auto... m, auto... n>(
               std::index_sequence<m...>, std::index_sequence<n...>, T &&t) {
      return std::invoke(std::forward<F>(f),
                         std::get<n>(std::get<m>(std::move(t)))...);
    }(first_t<pair>(), second_t<pair>(),
           std::forward_as_tuple(std::forward<Args>(args)...));
}

template <typename F, typename... Args>
constexpr decltype(auto) reverse_multi_apply(F &&f, Args &&...args) {
  return reverse_invoke(
      [&]<typename... Inner>(Inner &&...inner) {
        return multi_apply(std::forward<F>(f), std::forward<Inner>(inner)...);
      },
      std::forward<Args>(args)...);
}

template <typename limit, typename T, template <typename...> typename F,
          bool ASC = true>
  requires is_sequence_v<limit>
struct loop_indices {
  static constexpr auto depth = sizeof_t_v<limit>;

  template <int i, typename U, typename V, bool = (i >= 0)>
  struct impl {
    static constexpr auto curr = get_v<i, U> + 1;
    static constexpr int last = get_v<i, limit>;

    static constexpr auto cond = (ASC && curr >= last || !ASC && curr <= 1);
    static constexpr auto value = i + 1 >= depth;

    static constexpr auto j = cond ? i - 1 : i + !value;
    static constexpr auto end = !cond && value;

    using indices = sub_t<i, cond ? (ASC ? -1 : last) : curr - 2 * !ASC, U>;
    using type = typeof_t<impl<
        j, indices,
        type_if<end, F<V, alter_if_t<end, indices>>, std::type_identity<V>>>>;
  };

  template <int i, typename U, typename V>
  struct impl<i, U, V, false> : std::type_identity<V> {};

  using type =
      typeof_t<impl<0, std::conditional_t<ASC, fill_c<depth, -1>, limit>, T>>;
};

template <typename limit, typename T, template <typename...> typename F,
          bool ASC = true>
using loop_indices_t = typeof_t<loop_indices<limit, T, F, ASC>>;

template <typename limit, typename T, template <typename...> typename F,
          bool ASC = true>
struct unique_indices {
  template <typename U, typename indices>
  struct impl {
    static constexpr auto value = is_unique_v<indices>;
    using type = type_if<value, F<U, indices>, std::type_identity<U>>;
  };

  using type = loop_indices_t<limit, T, impl, ASC>;
};

template <typename limit, typename T, template <typename...> typename F,
          bool ASC = true>
using unique_indices_t = typeof_t<unique_indices<limit, T, F, ASC>>;

template <auto N>
struct permutation_index
    : unique_indices<fill_c<N, N>, tuple_t<>, append, true> {};

template <auto N>
using permutation_index_t = typeof_t<permutation_index<N>>;

template <typename T, auto N = sizeof_t_v<T>>
struct loop_permutation {
  static constexpr auto depth = sizeof_t_v<T>;

  template <typename U, typename indices>
  struct impl : append<U, range_t<0, N, expand_of_t<T, indices>>> {};

  using type = unique_indices_t<fill_c<depth, depth>, tuple_t<>, impl, true>;
};

template <typename T, auto N = sizeof_t_v<T>>
using loop_permutation_t = typeof_t<loop_permutation<T, N>>;

template <typename T, T depth, bool ASC = true>
  requires std::is_integral_v<T> && std::is_signed_v<T>
struct loop_indices_generator {
  using indices_t = std::array<T, depth>;

  template <typename F>
  void operator()(F &&f) {
    indices_t asc;
    indices_t curr(limit);

    std::fill(asc.begin(), asc.end(), -1);

    if (ASC) curr.swap(asc);

    int i = 0;
    constexpr auto N = nary_v<F, fill_t<depth, T>>;

    while (i >= 0) {
      auto upper = limit[i];

      if ((ASC && curr[i] + 1 >= upper) || (!ASC && curr[i] <= 0)) {
        curr[i--] = ASC ? -1 : upper;

        continue;
      }

      curr[i] += 1 - 2 * !ASC;

      if (i + 1 >= depth)
        std::apply(std::forward<F>(f), array_take<N>(curr));
      else
        ++i;
    }
  }

  indices_t limit;
};

template <bool ASC, typename... Args>
constexpr decltype(auto) loop_for(Args... indices) {
  return loop_indices_generator<std::common_type_t<Args...>, sizeof_v<Args...>,
                                ASC>{indices...};
}

template <typename T, typename... Args>
constexpr decltype(auto) call_apply(Args &&...args) {
  return T::template apply(std::forward<Args>(args)...);
}

template <size_t i, size_t j, typename indices>
struct next_cartesian_product;

template <size_t i, size_t j, auto... N>
struct next_cartesian_product<i, j, std::index_sequence<N...>> {
  template <typename T, typename U>
  static constexpr decltype(auto) apply(T &&t, U &&u) {
    using next = next_cartesian_product<i + 1, j, std::index_sequence<N...>>;

    return std::tuple_cat(
        std::make_tuple(std::make_pair(std::get<i>(t), std::get<N>(u)))...,
        call_apply<next>(std::forward<T>(t), std::forward<U>(u)));
  }
};

template <size_t j, auto... N>
struct next_cartesian_product<j, j, std::index_sequence<N...>> {
  template <typename T, typename U>
  static constexpr decltype(auto) apply(T &&t, U &&u) {
    return std::tuple<>();
  }
};

template <typename T, typename U>
constexpr decltype(auto) tuple_cartesian_product(T &&t, U &&u) {
  using indices = index_sequence_of_t<std::remove_cvref_t<U>>;
  using next =
      next_cartesian_product<0, sizeof_t_v<std::remove_cvref_t<T>>, indices>;

  return call_apply<next>(std::forward<T>(t), std::forward<U>(u));
}

template <typename T>
inline constexpr auto length_v = sizeof_t_v<std::decay_t<T>>;

template <auto N, typename T>
inline constexpr auto tuple_size_of = sizeof_t_v<std::tuple_element_t<N, T>>;

template <auto m, auto n, typename T, auto... N>
constexpr decltype(auto) indexof(const std::index_sequence<N...> &) {
  return m / (1 * ... * (n < N ? tuple_size_of<N, T> : 1)) %
         tuple_size_of<n, T>;
}

template <typename... Args>
constexpr decltype(auto) tuple_cartesian_product(Args &&...args) {
  constexpr auto size = (sizeof...(Args) != 0) * (1 * ... * length_v<Args>);

  return [&]<typename T, auto... N>(T &&t, const std::index_sequence<N...> &) {
    auto indices = std::make_index_sequence<std::tuple_size_v<T>>();

    return std::make_tuple(
        [&]<auto m, auto... n>(const std::index_sequence<n...> &) {
          return std::forward_as_tuple(
              std::get<indexof<m, n, T>(indices)>(std::get<n>(t))...);
        }.template operator()<N>(indices)...);
  }(std::forward_as_tuple(std::forward<Args>(args)...),
         std::make_index_sequence<size>());
}

template <typename... Args>
constexpr decltype(auto) make_matrix_indices() {
  constexpr auto N = (length_v<Args> + ...);

  std::array<std::pair<size_t, size_t>, N> indices;
  std::array<size_t, sizeof_v<Args...>> len{length_v<Args>...};

  for (size_t k = 0, i = 0; i != len.size(); ++i)
    for (size_t j = 0; j != len[i]; ++j) indices[k++] = std::make_pair(i, j);

  return indices;
}

template <typename T>
constexpr decltype(auto) make_matrix_indices(T &&t) {
  return std::apply(
      []<typename... Args>(Args &&...args) {
        return make_matrix_indices<std::decay_t<Args>...>();
      },
      std::forward<T>(t));
}

template <int N>
struct holder {
  static constexpr auto value = N;
};

template <typename T>
struct cartesian_product_indices {
  using indices = index_sequence_of_t<T>;

  template <auto N, typename U>
  using next = pair_v<N, indexof<typev<U>, N, T>(indices())>;

  template <auto N, typename U>
  using impl = expand_t<next, holder<N>, indices>;

  template <typename... Args>
  using call = index_sequence_of_c<(sizeof_v<Args...> != 0) *
                                   (1 * ... * sizeof_t_v<Args>)>;

  using type = expand_t<impl, holder<0>, unpack_t<call, T>>;
};

template <typename T>
using cartesian_product_indices_t = typeof_t<cartesian_product_indices<T>>;
template <auto row, auto col, typename T>
struct matrix : fill<row, fill_t<col, T>> {};

template <auto row, auto col, typename T>
using matrix_t = typeof_t<matrix<row, col, T>>;

template <auto N, typename T>
struct square_matrix : matrix<N, N, T> {};

template <auto N, typename T>
using square_matrix_t = typeof_t<square_matrix<N, T>>;

template <typename T>
struct matrix_row_size : sizeof_t<T> {};

template <typename T>
inline constexpr auto matrix_row_size_v = typev<matrix_row_size<T>>;

template <typename T>
struct matrix_col_size
    : sizeof_t<type_if<is_variadic_v<T> && sizeof_t_v<T>, element<0, T>, c_0>> {
};

template <typename T>
inline constexpr auto matrix_col_size_v = typev<matrix_col_size<T>>;

template <typename T>
struct matrix_dim : c_<matrix_row_size_v<T> * matrix_col_size_v<T>> {};

template <typename T>
using matrix_dim_t = typeof_t<matrix_dim<T>>;

template <typename T>
inline constexpr auto matrix_dim_v = typev<matrix_dim_t<T>>;

template <typename T>
struct is_matrix : std::false_type {};

template <template <typename...> typename T, typename U, typename... Args>
struct is_matrix<T<U, Args...>>
    : bool_<((sizeof_t_v<U> == sizeof_t_v<Args>) && ...)> {};

template <typename T>
inline constexpr auto is_matrix_v = typev<is_matrix<T>>;

template <typename T>
  requires is_matrix_v<T>
struct is_square_matrix : bool_<matrix_row_size_v<T> == matrix_col_size_v<T>> {
};

template <typename T>
inline constexpr auto is_square_matrix_v = typev<is_square_matrix<T>>;

template <auto row, auto col, typename T>
struct get_matrix_element : element<col, element_t<row, T>> {};

template <auto row, auto col, typename T>
using get_matrix_element_t = typeof_t<get_matrix_element<row, col, T>>;

template <auto row, auto col, typename T>
inline constexpr auto get_matrix_element_v =
    typev<get_matrix_element_t<row, col, T>>;

template <auto row, auto col, typename T, typename U>
struct set_matrix_element
    : change<row, change_t<col, T, element_t<row, U>>, U> {};

template <auto row, auto col, typename T, typename U>
using set_matrix_element_t = typeof_t<set_matrix_element<row, col, T, U>>;

template <auto row, auto col, auto v, typename U>
using set_matrix_element_c = set_matrix_element_t<row, col, c_<v>, U>;

template <auto row, auto col, typename T, typename U, int m = 1, int n = 1>
struct matrix_element_summator : c_<m * get_matrix_element_v<row, col, T> +
                                    n * get_matrix_element_v<row, col, U>> {};

template <auto row, auto col, typename T, typename U, int m = 1, int n = 1>
using matrix_element_summator_t =
    typeof_t<matrix_element_summator<row, col, T, U, m, n>>;

template <auto row, auto col, typename T, typename U, int m = 1, int n = 1>
inline constexpr auto matrix_element_summator_v =
    typev<matrix_element_summator_t<row, col, T, U, m, n>>;

template <auto row, auto col, typename T, typename U>
struct matrix_element_add : matrix_element_summator<row, col, T, U> {};

template <auto row, auto col, typename T, typename U>
using matrix_element_add_t = typeof_t<matrix_element_add<row, col, T, U>>;

template <auto row, auto col, typename T, typename U>
inline constexpr auto matrix_element_add_v =
    typev<matrix_element_add_t<row, col, T, U>>;

template <auto row, auto col, typename T, typename U>
struct matrix_element_sub : matrix_element_summator<row, col, T, U, 1, -1> {};

template <auto row, auto col, typename T, typename U>
using matrix_element_sub_t = typeof_t<matrix_element_sub<row, col, T, U>>;

template <auto row, auto col, typename T, typename U>
inline constexpr auto matrix_element_sub_v =
    typev<matrix_element_sub_t<row, col, T, U>>;

template <auto N, typename T, typename U, bool B = false>
  requires(matrix_row_size_v<T> == matrix_col_size_v<U> || B)
struct set_matrix_row : change<N, T, U> {};

template <auto N, typename T, typename U, bool B = false>
using set_matrix_row_t = typeof_t<set_matrix_row<N, T, U, B>>;

template <auto lower, auto upper, typename T,
          template <auto, auto, typename> typename F>
struct matrix_row_transform : F<lower, upper, T> {};

template <auto lower, auto upper, typename T,
          template <auto, auto, typename> typename F>
using matrix_row_transform_t =
    typeof_t<matrix_row_transform<lower, upper, T, F>>;

template <auto lower, auto upper, typename T,
          template <auto, auto, typename> typename F, bool B1 = false,
          bool B2 = false, bool B3 = false, bool B4 = false>
struct matrix_col_transform {
  template <int i, int j, typename U>
  struct impl {
    using call = typeof_t<
        F<lower, upper, type_if<!B1, element<i, U>, index_upper<i, U>>>>;
    using type = typeof_t<impl<
        i + 1, j,
        type_if<!B2, set_matrix_row<i, call, U, 1>, std::type_identity<call>>>>;
  };

  template <int j, typename U>
  struct impl<j, j, U> : std::type_identity<U> {};

  using type = typeof_t<impl<B4 * lower, !B4 ? matrix_row_size_v<T> : upper,
                             type_if<!B3, std::type_identity<T>, row_type<T>>>>;
};

template <auto lower, auto upper, typename T,
          template <auto, auto, typename> typename F, bool B1 = false,
          bool B2 = false, bool B3 = false, bool B4 = false>
using matrix_col_transform_t =
    typeof_t<matrix_col_transform<lower, upper, T, F, B1, B2, B3, B4>>;

template <auto N, typename T, typename U, bool B = false>
  requires(matrix_row_size_v<T> == matrix_row_size_v<U> || B)
struct set_matrix_col : change<N, T, U> {
  template <int i, int j, typename V>
  struct impl
      : set_matrix_element<typev<V>, N, element_t<typev<V>, T>, typeof_t<V>> {};

  using type = matrix_col_transform_t<N, N, U, impl, 1, 1>;
};

template <auto N, typename T, typename U, bool B = false>
using set_matrix_col_t = typeof_t<set_matrix_col<N, T, U, B>>;

template <auto row, auto col = row>
struct zero_matrix : matrix<row, col, c_0> {};

template <auto row, auto col = row>
using zero_matrix_t = typeof_t<zero_matrix<row, col>>;

template <auto N>
struct identity_matrix {
  template <int i, int j, typename V>
  struct impl : set_matrix_element<typev<V>, typev<V>, c_1, typeof_t<V>> {};

  using type = matrix_col_transform_t<N, N, zero_matrix_t<N>, impl, 1, 1>;
};

template <auto N>
using identity_matrix_t = typeof_t<identity_matrix<N>>;

template <typename T>
  requires is_square_matrix_v<T>
struct get_matrix_diagonal {
  template <int i, int j, typename V>
  struct impl
      : append<typeof_t<V>, get_matrix_element_t<typev<V>, typev<V>, T>> {};

  using type = matrix_col_transform_t<0, 0, T, impl, 1, 1, 1>;
};

template <typename T>
using get_matrix_diagonal_t = typeof_t<get_matrix_diagonal<T>>;

template <typename T, typename U>
  requires(matrix_row_size_v<T> == matrix_row_size_v<U> &&
           is_square_matrix_v<U>)
struct set_matrix_diagonal {
  template <int i, int j, typename V>
  struct impl {
    static constexpr auto row = typev<V>;
    using type = set_matrix_element_t<row, row, element_t<row, T>, typeof_t<V>>;
  };

  using type = matrix_col_transform_t<0, 0, U, impl, 1, 1>;
};

template <typename T, typename U>
using set_matrix_diagonal_t = typeof_t<set_matrix_diagonal<T, U>>;

template <typename T>
struct trace : sum<get_matrix_diagonal_t<T>> {};

template <typename T>
using trace_t = typeof_t<trace<T>>;

template <typename T>
inline constexpr auto trace_v = typev<trace_t<T>>;

template <auto N, typename T>
struct get_matrix_row : element<N, T> {};

template <auto N, typename T>
using get_matrix_row_t = typeof_t<get_matrix_row<N, T>>;

template <auto N, typename T>
struct get_matrix_col {
  template <int i, int j, typename U>
  struct impl : append<typeof_t<U>, get_matrix_element_t<typev<U>, N, T>> {};

  using type = matrix_col_transform_t<N, N, T, impl, 1, 1, 1>;
};

template <auto N, typename T>
using get_matrix_col_t = typeof_t<get_matrix_col<N, T>>;

template <auto N, typename T, typename U>
  requires(matrix_row_size_v<T> == matrix_col_size_v<U>)
struct insert_matrix_row : insert<N, U, T> {};

template <auto N, typename T, typename U>
using insert_matrix_row_t = typeof_t<insert_matrix_row<N, T, U>>;

template <auto N, typename T, typename U>
  requires(matrix_row_size_v<T> == matrix_row_size_v<U>)
struct insert_matrix_col {
  template <int i, int j, typename V>
  struct impl
      : insert<i, element_t<typev<V>, typeof_t<V>>, element_t<typev<V>, T>> {};

  using type = matrix_col_transform_t<N, N, U, impl, 1>;
};

template <auto N, typename T, typename U>
using insert_matrix_col_t = typeof_t<insert_matrix_col<N, T, U>>;

template <typename T, typename U>
struct vertical_prepend : insert_matrix_row<0, T, U> {};

template <typename T, typename U>
using vertical_prepend_t = typeof_t<vertical_prepend<T, U>>;

template <typename T, typename U>
struct vertical_append : insert_matrix_row<matrix_row_size_v<U>, T, U> {};

template <typename T, typename U>
using vertical_append_t = typeof_t<vertical_append<T, U>>;

template <typename T, typename U>
struct horizontal_prepend : insert_matrix_col<0, T, U> {};

template <typename T, typename U>
using horizontal_prepend_t = typeof_t<horizontal_prepend<T, U>>;

template <typename T, typename U>
struct horizontal_append : insert_matrix_col<matrix_col_size_v<U>, T, U> {};

template <typename T, typename U>
using horizontal_append_t = typeof_t<horizontal_append<T, U>>;

template <auto N, typename T>
struct remove_matrix_row : erase_at<N, T> {};

template <auto N, typename T>
using remove_matrix_row_t = typeof_t<remove_matrix_row<N, T>>;

template <auto N, typename T>
struct remove_matrix_col {
  template <int i, int j, typename U>
  using impl = erase_at<i, U>;

  using type = matrix_col_transform_t<N, N, T, impl>;
};

template <auto N, typename T>
using remove_matrix_col_t = typeof_t<remove_matrix_col<N, T>>;

template <auto row, auto col, typename T>
struct remove_matrix_row_col
    : remove_matrix_col<col, remove_matrix_row_t<row, T>> {};

template <auto row, auto col, typename T>
using remove_matrix_row_col_t = typeof_t<remove_matrix_row_col<row, col, T>>;

template <auto col, auto row, typename T>
struct remove_matrix_col_row
    : remove_matrix_row<row, remove_matrix_col_t<col, T>> {};

template <auto col, auto row, typename T>
using remove_matrix_col_row_t = typeof_t<remove_matrix_col_row<col, row, T>>;

template <typename T>
  requires is_square_matrix_v<T>
struct det {
  template <int i, int j, int k>
  struct impl {
    static constexpr auto value = get_matrix_element_v<0, i, T>;
    using next = type_if<value != 0, remove_matrix_row_col<0, i, T>,
                         std::type_identity<T>>;

    static constexpr auto curr = (i % 2 ? -1 : 1) * value;
    using type = typeof_t<
        impl<i + 1, j, k + curr * type_if_v<value != 0, det<next>, c_0>>>;
  };

  template <int j, int k>
  struct impl<j, j, k> : c_<k> {};

  static constexpr auto row = matrix_row_size_v<T>;
  using type = type_if<row + matrix_col_size_v<T> == 2,
                       get_matrix_element<0, 0, T>, impl<0, row, 0>>;
};

template <typename T>
using det_t = typeof_t<det<T>>;

template <typename T>
inline constexpr auto det_v = typev<det_t<T>>;

template <typename T, template <typename> typename F1,
          template <auto, typename> typename F2,
          template <typename...> typename F3>
struct line_summator {
  template <int i, int j, typename U>
  struct impl : append<typeof_t<U>, typeof_t<F3<typeof_t<F2<typev<U>, T>>>>> {};

  using type = matrix_col_transform_t<0, typev<F1<T>>, T, impl, 1, 1, 1, 1>;
};

template <typename T, template <typename> typename F1,
          template <auto, typename> typename F2,
          template <typename...> typename F3>
using line_summator_t = typeof_t<line_summator<T, F1, F2, F3>>;

template <auto N, typename T>
struct sum_row : sum<get_matrix_row_t<N, T>> {};

template <auto N, typename T>
using sum_row_t = typeof_t<sum_row<N, T>>;

template <auto N, typename T>
inline constexpr auto sum_row_v = typev<sum_row_t<N, T>>;

template <auto N, typename T>
struct sum_col : sum<get_matrix_col_t<N, T>> {};

template <auto N, typename T>
using sum_col_t = typeof_t<sum_col<N, T>>;

template <auto N, typename T>
inline constexpr auto sum_col_v = typev<sum_col_t<N, T>>;

template <auto N, typename T>
struct product_row : mul<get_matrix_row_t<N, T>> {};

template <auto N, typename T>
using product_row_t = typeof_t<product_row<N, T>>;

template <auto N, typename T>
inline constexpr auto product_row_v = typev<product_row_t<N, T>>;

template <auto N, typename T>
struct product_col : mul<get_matrix_col_t<N, T>> {};

template <auto N, typename T>
using product_col_t = typeof_t<product_col<N, T>>;

template <auto N, typename T>
inline constexpr auto product_col_v = typev<product_col_t<N, T>>;

template <template <typename...> typename F, typename T>
struct matrix_row_apply : line_summator<T, matrix_row_size, get_matrix_row, F> {
};

template <template <typename...> typename F, typename T>
using matrix_row_apply_t = typeof_t<matrix_row_apply<F, T>>;

template <template <typename...> typename F, typename T>
struct matrix_col_apply : line_summator<T, matrix_col_size, get_matrix_col, F> {
};

template <template <typename...> typename F, typename T>
using matrix_col_apply_t = typeof_t<matrix_col_apply<F, T>>;

template <typename T>
struct matrix_row_sum : matrix_col_apply<sum, T> {};

template <typename T>
using matrix_row_sum_t = typeof_t<matrix_row_sum<T>>;

template <typename T>
struct matrix_col_sum : matrix_row_apply<sum, T> {};

template <typename T>
using matrix_col_sum_t = typeof_t<matrix_col_sum<T>>;

template <typename T>
struct matrix_row_means : matrix_col_apply<means, T> {};

template <typename T>
using matrix_row_means_t = typeof_t<matrix_row_means<T>>;

template <typename T>
struct matrix_col_means : matrix_row_apply<means, T> {};

template <typename T>
using matrix_col_means_t = typeof_t<matrix_col_means<T>>;

template <typename T>
struct matrix_sum : sum<matrix_col_sum_t<T>> {};

template <typename T>
using matrix_sum_t = typeof_t<matrix_sum<T>>;

template <typename T>
inline constexpr auto matrix_sum_v = typev<matrix_sum_t<T>>;

template <typename T>
struct matrix_means : c_<matrix_sum_v<T> / matrix_dim_v<T>> {};

template <typename T>
using matrix_means_t = typeof_t<matrix_means<T>>;

template <typename T>
inline constexpr auto matrix_means_v = typev<matrix_means_t<T>>;

template <typename T>
struct matrix_row_mul : matrix_col_apply<mul, T> {};

template <typename T>
using matrix_row_mul_t = typeof_t<matrix_row_mul<T>>;

template <typename T>
struct matrix_col_mul : matrix_row_apply<mul, T> {};

template <typename T>
using matrix_col_mul_t = typeof_t<matrix_col_mul<T>>;

template <typename T>
struct matrix_mul : mul<matrix_col_mul_t<T>> {};

template <typename T>
using matrix_mul_t = typeof_t<matrix_mul<T>>;

template <typename T>
inline constexpr auto matrix_mul_v = typev<matrix_mul_t<T>>;

template <typename T>
struct matrix_row_maximum : matrix_row_apply<max_element, T> {};

template <typename T>
using matrix_row_maximum_t = typeof_t<matrix_row_maximum<T>>;

template <typename T>
struct matrix_col_maximum : matrix_col_apply<max_element, T> {};

template <typename T>
using matrix_col_maximum_t = typeof_t<matrix_col_maximum<T>>;

template <typename T>
struct matrix_row_minimum : matrix_row_apply<min_element, T> {};

template <typename T>
using matrix_row_minimum_t = typeof_t<matrix_row_minimum<T>>;

template <typename T>
struct matrix_col_minimum : matrix_col_apply<min_element, T> {};

template <typename T>
using matrix_col_minimum_t = typeof_t<matrix_col_minimum<T>>;

template <typename T>
struct matrix_row_midpoint : matrix_row_apply<midpoint, T> {};

template <typename T>
using matrix_row_midpoint_t = typeof_t<matrix_row_midpoint<T>>;

template <typename T>
struct matrix_col_midpoint : matrix_col_apply<midpoint, T> {};

template <typename T>
using matrix_col_midpoint_t = typeof_t<matrix_col_midpoint<T>>;

template <auto row, typename T, typename U>
struct row_multiply
    : inner_dot<get_matrix_row_t<row, T>, get_matrix_row_t<row, U>> {};

template <auto row, typename T, typename U>
using row_multiply_t = typeof_t<row_multiply<row, T, U>>;

template <auto row, typename T, typename U>
struct col_multiply
    : inner_dot<get_matrix_col_t<row, T>, get_matrix_col_t<row, U>> {};

template <auto row, typename T, typename U>
using col_multiply_t = typeof_t<col_multiply<row, T, U>>;

template <typename T, typename U>
  requires(matrix_row_size_v<T> == matrix_row_size_v<U> &&
           matrix_col_size_v<T> == matrix_col_size_v<U>)
struct matrix_elementwise_multiply {
  template <int i, int j, typename V>
  struct impl {
    static constexpr auto row = typev<V>;
    using type = set_matrix_row_t<row, row_multiply_t<row, T, U>, typeof_t<V>>;
  };

  using type = matrix_col_transform_t<0, 0, T, impl, 1, 1>;
};

template <typename T, typename U>
using matrix_elementwise_multiply_t =
    typeof_t<matrix_elementwise_multiply<T, U>>;

template <auto N, typename T, typename U,
          template <typename, typename> typename F>
  requires(matrix_row_size_v<T> == matrix_col_size_v<U>)
struct matrix_row_operator
    : set_matrix_row<N, typeof_t<F<T, get_matrix_row_t<N, U>>>, U> {};

template <auto N, typename T, typename U,
          template <typename, typename> typename F>
using matrix_row_operator_t = typeof_t<matrix_row_operator<N, T, U, F>>;

template <auto N, typename T, typename U, bool B>
  requires(matrix_row_size_v<T> == matrix_row_size_v<U>)
struct matrix_col_operator {
  template <int i, int j, typename V>
  struct impl {
    using curr = element_t<typev<V>, typeof_t<V>>;

    static constexpr auto p = element_v<i, curr>;
    static constexpr auto q = element_v<typev<V>, T>;

    using type = change_t<i, c_<B ? p + q : p * q>, curr>;
  };

  using type = matrix_col_transform_t<N, N, U, impl, 1>;
};

template <auto N, typename T, typename U, bool B>
using matrix_col_operator_t = typeof_t<matrix_col_operator<N, T, U, B>>;

template <auto N, typename T, typename U>
struct add_matrix_row : matrix_row_operator<N, T, U, inner_sum> {};

template <auto N, typename T, typename U>
using add_matrix_row_t = typeof_t<add_matrix_row<N, T, U>>;

template <auto N, typename T, typename U>
struct add_matrix_col : matrix_col_operator<N, T, U, 1> {};

template <auto N, typename T, typename U>
using add_matrix_col_t = typeof_t<add_matrix_col<N, T, U>>;

template <auto N, typename T, typename U>
struct mul_matrix_row : matrix_row_operator<N, T, U, inner_dot> {};

template <auto N, typename T, typename U>
using mul_matrix_row_t = typeof_t<mul_matrix_row<N, T, U>>;

template <auto N, typename T, typename U>
struct mul_matrix_col : matrix_col_operator<N, T, U, 0> {};

template <auto N, typename T, typename U>
using mul_matrix_col_t = typeof_t<mul_matrix_col<N, T, U>>;

template <auto N, auto M, typename T>
struct scale_row : inner_mul<M, get_matrix_row_t<N, T>> {};

template <auto N, auto M, typename T>
using scale_row_t = typeof_t<scale_row<N, M, T>>;

template <auto N, auto M, typename T>
struct scale_col : inner_mul<M, get_matrix_col_t<N, T>> {};

template <auto N, auto M, typename T>
using scale_col_t = typeof_t<scale_col<N, M, T>>;

template <auto N, auto M, auto P, typename T>
struct scale_add_row : inner_sum<get_matrix_row_t<P, T>,
                                 inner_mul_t<M, get_matrix_row_t<N, T>>> {};

template <auto N, auto M, auto P, typename T>
using scale_add_row_t = typeof_t<scale_add_row<N, M, P, T>>;

template <auto N, auto M, auto P, typename T>
struct scale_add_col : inner_sum<get_matrix_col_t<P, T>,
                                 inner_mul_t<M, get_matrix_col_t<N, T>>> {};

template <auto N, auto M, auto P, typename T>
using scale_add_col_t = typeof_t<scale_add_col<N, M, P, T>>;

template <auto N, auto M, auto P, typename T>
struct scale_mul_row : inner_dot<get_matrix_row_t<P, T>,
                                 inner_mul_t<M, get_matrix_row_t<N, T>>> {};

template <auto N, auto M, auto P, typename T>
using scale_mul_row_t = typeof_t<scale_mul_row<N, M, P, T>>;

template <auto N, auto M, auto P, typename T>
struct scale_mul_col : inner_dot<get_matrix_col_t<P, T>,
                                 inner_mul_t<M, get_matrix_col_t<N, T>>> {};

template <auto N, auto M, auto P, typename T>
using scale_mul_col_t = typeof_t<scale_mul_col<N, M, P, T>>;

template <auto lower, auto upper, auto M, typename T>
struct scale_range
    : replace_range<lower, upper, T, inner_mul_range_t<lower, upper, M, T>> {};

template <auto lower, auto upper, auto M, typename T>
using scale_range_t = typeof_t<scale_range<lower, upper, M, T>>;

template <auto N, auto M, typename T>
struct scale_matrix_row : set_matrix_row<N, scale_row_t<N, M, T>, T> {};

template <auto N, auto M, typename T>
using scale_matrix_row_t = typeof_t<scale_matrix_row<N, M, T>>;

template <auto N, auto M, typename T>
struct scale_matrix_col : set_matrix_col<N, scale_col_t<N, M, T>, T> {};

template <auto N, auto M, typename T>
using scale_matrix_col_t = typeof_t<scale_matrix_col<N, M, T>>;

template <auto lower, auto upper, auto M, typename T>
struct scale_matrix_row_range {
  template <int i, int j, typename U>
  using impl = inner_mul<M, U>;

  using type = matrix_col_transform_t<lower, upper, T, impl, 0, 0, 0, 1>;
};

template <auto lower, auto upper, auto M, typename T>
using scale_matrix_row_range_t =
    typeof_t<scale_matrix_row_range<lower, upper, M, T>>;

template <auto lower, auto upper, auto M, typename T>
struct scale_matrix_col_range {
  template <int i, int j, typename U>
  using impl = scale_range<i, j, M, U>;

  using type = matrix_col_transform_t<lower, upper, T, impl>;
};

template <auto lower, auto upper, auto M, typename T>
using scale_matrix_col_range_t =
    typeof_t<scale_matrix_col_range<lower, upper, M, T>>;

template <auto M, typename T>
struct scale_matrix {
  template <int i, int j, typename U>
  using impl = inner_mul<M, U>;

  using type = matrix_col_transform_t<M, M, T, impl>;
};

template <auto M, typename T>
using scale_matrix_t = typeof_t<scale_matrix<M, T>>;

template <auto N, auto M, auto P, typename T,
          template <auto, auto, auto, typename> typename F1,
          template <auto, typename, typename> typename F2>
struct scale_matrix_operator : F2<P, typeof_t<F1<N, M, P, T>>, T> {};

template <auto N, auto M, auto P, typename T,
          template <auto, auto, auto, typename> typename F1,
          template <auto, typename, typename> typename F2>
using scale_matrix_operator_t =
    typeof_t<scale_matrix_operator<N, M, P, T, F1, F2>>;

template <auto N, auto M, auto P, typename T,
          template <auto, auto, auto, typename> typename F>
using scale_row_operator = scale_matrix_operator<N, M, P, T, F, set_matrix_row>;

template <auto N, auto M, auto P, typename T,
          template <auto, auto, auto, typename> typename F>
using scale_col_operator = scale_matrix_operator<N, M, P, T, F, set_matrix_col>;

template <auto N, auto M, auto P, typename T>
struct scale_add_matrix_row : scale_row_operator<N, M, P, T, scale_add_row> {};

template <auto N, auto M, auto P, typename T>
using scale_add_matrix_row_t = typeof_t<scale_add_matrix_row<N, M, P, T>>;

template <auto N, auto M, auto P, typename T>
struct scale_add_matrix_col : scale_col_operator<N, M, P, T, scale_add_col> {};

template <auto N, auto M, auto P, typename T>
using scale_add_matrix_col_t = typeof_t<scale_add_matrix_col<N, M, P, T>>;

template <auto N, auto M, auto P, typename T>
struct scale_mul_matrix_row : scale_row_operator<N, M, P, T, scale_mul_row> {};

template <auto N, auto M, auto P, typename T>
using scale_mul_matrix_row_t = typeof_t<scale_mul_matrix_row<N, M, P, T>>;

template <auto N, auto M, auto P, typename T>
struct scale_mul_matrix_col : scale_col_operator<N, M, P, T, scale_mul_col> {};

template <auto N, auto M, auto P, typename T>
using scale_mul_matrix_col_t = typeof_t<scale_mul_matrix_col<N, M, P, T>>;

template <auto m, auto n, auto p, auto q, typename T>
struct swap_matrix_element {
  using prev = get_matrix_element_t<m, n, T>;
  using next = get_matrix_element_t<p, q, T>;

  using type =
      set_matrix_element_t<p, q, prev, set_matrix_element_t<m, n, next, T>>;
};

template <auto m, auto n, auto p, auto q, typename T>
using swap_matrix_element_t = typeof_t<swap_matrix_element<m, n, p, q, T>>;

template <auto M, auto N, typename T>
struct swap_matrix_row : matrix_row_transform<M, N, T, swap> {};

template <auto M, auto N, typename T>
using swap_matrix_row_t = typeof_t<swap_matrix_row<M, N, T>>;

template <auto M, auto N, typename T>
struct swap_matrix_col : matrix_col_transform<M, N, T, swap> {};

template <auto M, auto N, typename T>
using swap_matrix_col_t = typeof_t<swap_matrix_col<M, N, T>>;

template <auto lower, auto upper, typename T>
struct matrix_row_range : matrix_row_transform<lower, upper, T, range> {};

template <auto lower, auto upper, typename T>
using matrix_row_range_t = typeof_t<matrix_row_range<lower, upper, T>>;

template <auto lower, auto upper, typename T>
struct matrix_col_range : matrix_col_transform<lower, upper, T, range> {};

template <auto lower, auto upper, typename T>
using matrix_col_range_t = typeof_t<matrix_col_range<lower, upper, T>>;

template <auto N, typename T>
struct drop_row_up : matrix_row_range<N, matrix_row_size_v<T>, T> {};

template <auto N, typename T>
using drop_row_up_t = typeof_t<drop_row_up<N, T>>;

template <auto N, typename T>
struct drop_row_down : matrix_row_range<0, matrix_row_size_v<T> - N, T> {};

template <auto N, typename T>
using drop_row_down_t = typeof_t<drop_row_down<N, T>>;

template <auto N, typename T>
struct drop_col_left : matrix_col_range<N, matrix_col_size_v<T>, T> {};

template <auto N, typename T>
using drop_col_left_t = typeof_t<drop_col_left<N, T>>;

template <auto N, typename T>
struct drop_col_right : matrix_col_range<0, matrix_col_size_v<T> - N, T> {};

template <auto N, typename T>
using drop_col_right_t = typeof_t<drop_col_right<N, T>>;

template <auto row_lower, auto row_upper, auto col_lower, auto col_upper,
          typename T>
struct sub_matrix
    : matrix_col_range<col_lower, col_upper,
                       matrix_row_range_t<row_lower, row_upper, T>> {};

template <auto row_lower, auto row_upper, auto col_lower, auto col_upper,
          typename T>
using sub_matrix_t =
    typeof_t<sub_matrix<row_lower, row_upper, col_lower, col_upper, T>>;

template <auto row, auto col, auto row_len, auto col_len, typename T>
struct matrix_subset : sub_matrix<row, row + row_len, col, col + col_len, T> {};

template <auto row, auto col, auto row_len, auto col_len, typename T>
using matrix_subset_t = typeof_t<matrix_subset<row, col, row_len, col_len, T>>;

template <auto lower, auto upper, typename T>
struct matrix_row_erase : matrix_row_transform<lower, upper, T, erase> {};

template <auto lower, auto upper, typename T>
using matrix_row_erase_t = typeof_t<matrix_row_erase<lower, upper, T>>;

template <auto lower, auto upper, typename T>
struct matrix_col_erase : matrix_col_transform<lower, upper, T, erase> {};

template <auto lower, auto upper, typename T>
using matrix_col_erase_t = typeof_t<matrix_col_erase<lower, upper, T>>;

template <auto row_lower, auto row_upper, auto col_lower, auto col_upper,
          typename T>
struct matrix_row_col_erase
    : matrix_col_erase<col_lower, col_upper,
                       matrix_row_erase_t<row_lower, row_upper, T>> {};

template <auto row_lower, auto row_upper, auto col_lower, auto col_upper,
          typename T>
using matrix_row_col_erase_t = typeof_t<
    matrix_row_col_erase<row_lower, row_upper, col_lower, col_upper, T>>;

template <auto col_lower, auto col_upper, auto row_lower, auto row_upper,
          typename T>
struct matrix_col_row_erase
    : matrix_row_erase<row_lower, row_upper,
                       matrix_col_erase_t<col_lower, col_upper, T>> {};

template <auto col_lower, auto col_upper, auto row_lower, auto row_upper,
          typename T>
using matrix_col_row_erase_t = typeof_t<
    matrix_col_row_erase<col_lower, col_upper, row_lower, row_upper, T>>;

template <auto pos, auto len, typename T>
struct matrix_row_subset : matrix_row_range<pos, pos + len, T> {};

template <auto pos, auto len, typename T>
using matrix_row_subset_t = typeof_t<matrix_row_subset<pos, len, T>>;

template <auto pos, auto len, typename T>
struct matrix_col_subset : matrix_col_range<pos, pos + len, T> {};

template <auto pos, auto len, typename T>
using matrix_col_subset_t = typeof_t<matrix_col_subset<pos, len, T>>;

template <auto i, auto j, auto k, typename T>
struct matrix_row_rotate : rotate<i, j, k, T> {};

template <auto i, auto j, auto k, typename T>
using matrix_row_rotate_t = typeof_t<matrix_row_rotate<i, j, k, T>>;

template <auto i, auto j, auto k, typename T>
struct matrix_col_rotate {
  template <auto p, auto q, typename U>
  using impl = rotate<p, q, k, U>;

  using type = matrix_col_transform_t<i, j, T, impl>;
};

template <auto i, auto j, auto k, typename T>
using matrix_col_rotate_t = typeof_t<matrix_col_rotate<i, j, k, T>>;

template <auto i, typename T>
struct matrix_row_pivot : matrix_row_rotate<0, i, matrix_row_size_v<T>, T> {};

template <auto i, typename T>
using matrix_row_pivot_t = typeof_t<matrix_row_pivot<i, T>>;

template <auto i, typename T>
struct matrix_col_pivot : matrix_col_rotate<0, i, matrix_col_size_v<T>, T> {};

template <auto i, typename T>
using matrix_col_pivot_t = typeof_t<matrix_col_pivot<i, T>>;

template <auto N, typename T>
struct matrix_shift_left : matrix_col_pivot<N, T> {};

template <auto N, typename T>
using matrix_shift_left_t = typeof_t<matrix_shift_left<N, T>>;

template <auto N, typename T>
struct matrix_shift_right : matrix_col_pivot<matrix_col_size_v<T> - N, T> {};

template <auto N, typename T>
using matrix_shift_right_t = typeof_t<matrix_shift_right<N, T>>;

template <auto N, typename T>
struct matrix_shift_up : matrix_row_pivot<N, T> {};

template <auto N, typename T>
using matrix_shift_up_t = typeof_t<matrix_shift_up<N, T>>;

template <auto N, typename T>
struct matrix_shift_down : matrix_row_pivot<matrix_row_size_v<T> - N, T> {};

template <auto N, typename T>
using matrix_shift_down_t = typeof_t<matrix_shift_down<N, T>>;

template <auto N, typename T>
struct reverse_row : reverse<get_matrix_row_t<N, T>> {};

template <auto N, typename T>
using reverse_row_t = typeof_t<reverse_row<N, T>>;

template <auto N, typename T>
struct reverse_col : reverse<get_matrix_col_t<N, T>> {};

template <auto N, typename T>
using reverse_col_t = typeof_t<reverse_col<N, T>>;

template <auto N, typename T>
struct matrix_row_reverse : set_matrix_row<N, reverse_row_t<N, T>, T> {};

template <auto N, typename T>
using matrix_row_reverse_t = typeof_t<matrix_row_reverse<N, T>>;

template <auto N, typename T>
struct matrix_col_reverse : set_matrix_col<N, reverse_col_t<N, T>, T> {};

template <auto N, typename T>
using matrix_col_reverse_t = typeof_t<matrix_col_reverse<N, T>>;

template <auto p, auto q, typename T, template <auto, typename> typename F1,
          template <auto, typename> typename F2>
struct matrix_reverse : F2<q, typeof_t<F1<p, T>>> {};

template <auto p, auto q, typename T, template <auto, typename> typename F1,
          template <auto, typename> typename F2>
using matrix_reverse_t = typeof_t<matrix_reverse<p, q, T, F1, F2>>;

template <auto row, auto col, typename T>
struct matrix_row_col_reverse
    : matrix_reverse<row, col, T, matrix_row_reverse, matrix_col_reverse> {};

template <auto row, auto col, typename T>
using matrix_row_col_reverse_t = typeof_t<matrix_row_col_reverse<row, col, T>>;

template <auto col, auto row, typename T>
struct matrix_col_row_reverse
    : matrix_reverse<col, row, T, matrix_col_reverse, matrix_row_reverse> {};

template <auto col, auto row, typename T>
using matrix_col_row_reverse_t = typeof_t<matrix_col_row_reverse<col, row, T>>;

template <auto lower, auto upper, typename T,
          template <auto, typename> typename F>
struct matrix_range_operator {
  template <int i, int j, typename U>
  struct impl : F<typev<U>, typeof_t<U>> {};

  using type = matrix_col_transform_t<lower, upper, T, impl, 1, 1, 0, 1>;
};

template <auto lower, auto upper, typename T,
          template <auto, typename> typename F>
using matrix_range_operator_t =
    typeof_t<matrix_range_operator<lower, upper, T, F>>;

template <auto lower, auto upper, typename T>
struct matrix_row_reverse_range
    : matrix_range_operator<lower, upper, T, matrix_row_reverse> {};

template <auto lower, auto upper, typename T>
using matrix_row_reverse_range_t =
    typeof_t<matrix_row_reverse_range<lower, upper, T>>;

template <auto lower, auto upper, typename T>
struct matrix_col_reverse_range
    : matrix_range_operator<lower, upper, T, matrix_col_reverse> {};

template <auto lower, auto upper, typename T>
using matrix_col_reverse_range_t =
    typeof_t<matrix_col_reverse_range<lower, upper, T>>;

template <typename T>
struct matrix_rowwise_reverse
    : matrix_row_reverse_range<0, matrix_row_size_v<T>, T> {};

template <typename T>
using matrix_rowwise_reverse_t = typeof_t<matrix_rowwise_reverse<T>>;

template <typename T>
struct matrix_colwise_reverse
    : matrix_col_reverse_range<0, matrix_col_size_v<T>, T> {};

template <typename T>
using matrix_colwise_reverse_t = typeof_t<matrix_colwise_reverse<T>>;

template <auto row_lower, auto row_upper, auto col_lower, auto col_upper,
          typename T>
struct matrix_row_col_reverse_range
    : matrix_col_reverse_range<
          col_lower, col_upper,
          matrix_row_reverse_range_t<row_lower, row_upper, T>> {};

template <auto row_lower, auto row_upper, auto col_lower, auto col_upper,
          typename T>
using matrix_row_col_reverse_range_t =
    typeof_t<matrix_row_col_reverse_range<row_lower, row_upper, col_lower,
                                          col_upper, T>>;

template <auto col_lower, auto col_upper, auto row_lower, auto row_upper,
          typename T>
struct matrix_col_row_reverse_range
    : matrix_row_reverse_range<
          row_lower, row_upper,
          matrix_col_reverse_range_t<col_lower, col_upper, T>> {};

template <auto col_lower, auto col_upper, auto row_lower, auto row_upper,
          typename T>
using matrix_col_row_reverse_range_t =
    typeof_t<matrix_col_row_reverse_range<col_lower, col_upper, row_lower,
                                          row_upper, T>>;

template <auto row_lower, auto row_upper, auto col_lower, auto col_upper,
          typename T>
struct sub_matrix_reverse {
  using curr = sub_matrix_t<row_lower, row_upper, col_lower, col_upper, T>;
  using type = matrix_col_reverse_range_t<
      0, col_upper - col_lower,
      matrix_row_reverse_range_t<0, row_upper - row_lower, curr>>;
};

template <auto row_lower, auto row_upper, auto col_lower, auto col_upper,
          typename T>
using sub_matrix_reverse_t =
    typeof_t<sub_matrix_reverse<row_lower, row_upper, col_lower, col_upper, T>>;

template <auto row, auto col, typename T,
          template <auto, auto, typename> typename F>
struct matrix_operator {
  template <int i, int j, int k, int l, typename U>
  struct impl : impl<i, j, k + 1, l, typeof_t<F<i, k, U>>> {};

  template <int i, int j, int l, typename U>
  struct impl<i, j, l, l, U> : impl<i + 1, j, 0, l, U> {};

  template <int j, int k, int l, typename U>
  struct impl<j, j, k, l, U> : std::type_identity<U> {};

  using type = typeof_t<impl<0, row, 0, col, T>>;
};

template <auto row, auto col, typename T,
          template <auto, auto, typename> typename F>
using matrix_operator_t = typeof_t<matrix_operator<row, col, T, F>>;

template <typename T, bool B = false>
struct matrix_transpose {
  static constexpr auto row = matrix_row_size_v<T>;
  static constexpr auto col = matrix_col_size_v<T>;

  template <int i, int j, typename U>
  struct impl
      : set_matrix_element<
            B ? i : j, B ? j : i,
            get_matrix_element_t<B ? row - j - 1 : i, B ? col - i - 1 : j, T>,
            U> {};

  using type = matrix_operator_t<B ? col : row, B ? row : col,
                                 fill_t<col, get_matrix_col_t<0, T>>, impl>;
};

template <typename T, bool B = false>
using matrix_transpose_t = typeof_t<matrix_transpose<T, B>>;

template <typename T>
struct matrix_transpose_l : matrix_transpose<T, false> {};

template <typename T>
using matrix_transpose_l_t = typeof_t<matrix_transpose_l<T>>;

template <typename T>
struct matrix_transpose_r : matrix_transpose<T, true> {};

template <typename T>
using matrix_transpose_r_t = typeof_t<matrix_transpose_r<T>>;

template <typename T>
struct rotate_matrix_left : matrix_colwise_reverse<matrix_transpose_t<T>> {};

template <typename T>
using rotate_matrix_left_t = typeof_t<rotate_matrix_left<T>>;

template <typename T>
struct rotate_matrix_right : matrix_rowwise_reverse<matrix_transpose_t<T>> {};

template <typename T>
using rotate_matrix_right_t = typeof_t<rotate_matrix_right<T>>;

template <typename T, typename U, int m = 1, int n = 1>
  requires(matrix_row_size_v<T> == matrix_row_size_v<U> &&
           matrix_col_size_v<T> == matrix_col_size_v<U>)
struct matrix_summator {
  template <int i, int j, typename V>
  struct impl
      : set_matrix_element<i, j, matrix_element_summator_t<i, j, T, U, m, n>,
                           V> {};

  using type =
      matrix_operator_t<matrix_row_size_v<T>, matrix_col_size_v<T>, T, impl>;
};

template <typename T, typename U, int m = 1, int n = 1>
using matrix_summator_t = typeof_t<matrix_summator<T, U, m, n>>;

template <typename T, typename U>
struct matrix_add : matrix_summator<T, U> {};

template <typename T, typename U>
using matrix_add_t = typeof_t<matrix_add<T, U>>;

template <typename T, typename U>
struct matrix_sub : matrix_summator<T, U, 1, -1> {};

template <typename T, typename U>
using matrix_sub_t = typeof_t<matrix_sub<T, U>>;

template <auto row, auto col, typename T, typename U>
  requires(matrix_row_size_v<T> <= matrix_row_size_v<U> &&
           matrix_col_size_v<T> <= matrix_col_size_v<U>)
struct sub_matrix_replace {
  template <int i, int j, typename V>
  struct impl
      : set_matrix_element<row + i, col + j, get_matrix_element_t<i, j, T>, V> {
  };

  using type =
      matrix_operator_t<matrix_row_size_v<T>, matrix_col_size_v<T>, U, impl>;
};

template <auto row, auto col, typename T, typename U>
using sub_matrix_replace_t = typeof_t<sub_matrix_replace<row, col, T, U>>;

template <typename T, typename U>
  requires(matrix_col_size_v<T> == matrix_row_size_v<U>)
struct matrix_multiply {
  static constexpr auto len = matrix_row_size_v<U>;

  template <int i, int j, typename V>
  struct impl {
    template <int p, int q, typename W>
    struct sum {
      static constexpr auto n = typev<W>;
      static constexpr auto m =
          get_matrix_element_v<i, n, T> * get_matrix_element_v<n, j, U>;

      using curr = typeof_t<W>;
      using type =
          set_matrix_element_c<i, j, get_matrix_element_v<i, j, curr> + m,
                               curr>;
    };

    using type = matrix_col_transform_t<0, len, V, sum, 1, 1, 0, 1>;
  };

  static constexpr auto row = matrix_row_size_v<T>;
  static constexpr auto col = matrix_col_size_v<U>;

  using type = matrix_operator_t<row, col, matrix_t<row, col, c_0>, impl>;
};

template <typename T, typename U>
using matrix_multiply_t = typeof_t<matrix_multiply<T, U>>;

template <typename T, auto N>
struct matrix_power {
  using half = typeof_t<matrix_power<T, N / 2>>;
  using curr = matrix_multiply_t<half, half>;

  using type =
      type_if<N % 2, matrix_multiply<T, curr>, std::type_identity<curr>>;
};

template <typename T>
struct matrix_power<T, 1> : std::type_identity<T> {};

template <typename T>
struct matrix_power<T, 0> : identity_matrix<matrix_row_size_v<T>> {};

template <typename T, auto N>
using matrix_power_t = typeof_t<matrix_power<T, N>>;

template <typename T, auto N>
struct matrix_element_power {
  template <int i, int j, typename U>
  struct impl {
    using curr = power<get_matrix_element_v<i, j, U>, N>;
    using type = set_matrix_element_c<i, j, typev<curr>, U>;
  };

  using type =
      matrix_operator_t<matrix_row_size_v<T>, matrix_col_size_v<T>, T, impl>;
};

template <typename T, auto N>
using matrix_element_power_t = typeof_t<matrix_element_power<T, N>>;

template <auto N>
struct pascal_matrix {
  template <int i, int j, typename T>
  struct impl {
    using prev = type_if<i && j, get_matrix_element<i, j - 1, T>, c_0>;
    using next = type_if<i && j, get_matrix_element<i - 1, j, T>, c_1>;

    using type = set_matrix_element_t<i, j, plus_t<prev, next>, T>;
  };

  using type = matrix_operator_t<N, N, identity_matrix_t<N>, impl>;
};

template <auto N>
using pascal_matrix_t = typeof_t<pascal_matrix<N>>;

template <typename T>
struct matrix_negate {
  template <int i, int j, typename U>
  struct impl {
    using curr = get_matrix_element_t<i, j, U>;
    using type = set_matrix_element_c<i, j, -typev<curr>, U>;
  };

  using type =
      matrix_operator_t<matrix_row_size_v<T>, matrix_col_size_v<T>, T, impl>;
};

template <typename T>
using matrix_negate_t = typeof_t<matrix_negate<T>>;

template <typename T, bool B>
struct triangular_matrix {
  template <int i, int j, typename U>
  struct impl {
    static constexpr auto value = B && i > j || !B && i < j;
    using type =
        type_if<value, set_matrix_element<i, j, c_0, U>, std::type_identity<U>>;
  };

  using type =
      matrix_operator_t<matrix_row_size_v<T>, matrix_col_size_v<T>, T, impl>;
};

template <typename T, bool B>
using triangular_matrix_t = typeof_t<triangular_matrix<T, B>>;

template <typename T>
struct upper_triangular_matrix : triangular_matrix<T, true> {};

template <typename T>
using upper_triangular_matrix_t = typeof_t<upper_triangular_matrix<T>>;

template <typename T>
struct lower_triangular_matrix : triangular_matrix<T, false> {};

template <typename T>
using lower_triangular_matrix_t = typeof_t<lower_triangular_matrix<T>>;

template <typename T, typename U>
  requires(matrix_row_size_v<T> == matrix_row_size_v<U>)
struct toeplitz_matrix {
  using w = reverse_t<U>;
  static constexpr auto N = matrix_row_size_v<T>;

  template <int i, int j, typename V>
  struct impl
      : impl<i + 1, j,
             append_t<V,
                      concat_t<subset_t<N - i - 1, i, w>, drop_last_t<i, T>>>> {
  };

  template <int j, typename V>
  struct impl<j, j, V> : std::type_identity<V> {};

  using type = typeof_t<impl<1, N, tuple_t<T>>>;
};

template <typename T, typename U>
using toeplitz_matrix_t = typeof_t<toeplitz_matrix<T, U>>;

template <auto row, auto col, typename T, typename U>
  requires(matrix_col_size_v<T> == matrix_row_size_v<U>)
struct matrix_row_col_dot
    : sum<inner_dot_t<get_matrix_row_t<row, T>, get_matrix_col_t<col, U>>> {};

template <auto row, auto col, typename T, typename U>
using matrix_row_col_dot_t = typeof_t<matrix_row_col_dot<row, col, T, U>>;

template <auto row, auto col, typename T, typename U>
inline constexpr auto matrix_row_col_dot_v =
    typev<matrix_row_col_dot_t<row, col, T, U>>;

template <typename T>
struct row_matrix : std::type_identity<std::tuple<T>> {};

template <typename T>
using row_matrix_t = typeof_t<row_matrix<T>>;

template <typename T>
struct col_matrix;

template <template <typename...> typename T, typename... Args>
struct col_matrix<T<Args...>> : std::type_identity<T<T<Args>...>> {};

template <template <typename, auto...> typename T, typename U, auto... Args>
struct col_matrix<T<U, Args...>>
    : std::type_identity<std::tuple<T<U, Args>...>> {};

template <typename T>
using col_matrix_t = typeof_t<col_matrix<T>>;

template <auto col, auto row, typename T, typename U>
  requires(matrix_row_size_v<T> == matrix_col_size_v<U>)
struct matrix_col_row_dot
    : matrix_multiply<col_matrix_t<get_matrix_col_t<col, T>>,
                      row_matrix_t<get_matrix_row_t<row, U>>> {};

template <auto col, auto row, typename T, typename U>
using matrix_col_row_dot_t = typeof_t<matrix_col_row_dot<col, row, T, U>>;

template <auto row_lower, auto row_upper, auto col_lower, auto col_upper,
          typename T>
struct sub_matrix_transpose
    : matrix_transpose<
          sub_matrix_t<row_lower, row_upper, col_lower, col_upper, T>> {};

template <auto row_lower, auto row_upper, auto col_lower, auto col_upper,
          typename T>
using sub_matrix_transpose_t = typeof_t<
    sub_matrix_transpose<row_lower, row_upper, col_lower, col_upper, T>>;

template <typename T>
struct transpose {
  using base = row_type_t<T>;

  template <int i, int j, int k, int l, typename U, typename V>
  struct impl {
    using row = append_t<U, get_matrix_element_t<i, k, T>>;
    using type = typeof_t<impl<i + 1, j, k, l, row, V>>;
  };

  template <int j, int k, int l, typename U, typename V>
  struct impl<j, j, k, l, U, V> {
    using type = typeof_t<impl<0, j, k + 1, l, base, append_t<V, U>>>;
  };

  template <int i, int j, int l, typename U, typename V>
  struct impl<i, j, l, l, U, V> : std::type_identity<V> {};

  using type = typeof_t<
      impl<0, matrix_row_size_v<T>, 0, matrix_col_size_v<T>, base, clear_t<T>>>;
};

template <typename T>
using transpose_t = typeof_t<transpose<T>>;

template <typename T, typename U>
  requires(matrix_row_size_v<T> == matrix_row_size_v<U>)
struct matrix_horizontal_concat {
  template <int i, int j, typename V>
  struct impl {
    static constexpr auto row = typev<V>;

    using curr = typeof_t<V>;
    using type = set_matrix_row_t<
        row, append_range_t<element_t<row, curr>, get_matrix_row_t<row, U>>,
        curr, 1>;
  };

  using type = matrix_col_transform_t<0, 0, T, impl, 1, 1>;
};

template <typename T, typename U>
using matrix_horizontal_concat_t = typeof_t<matrix_horizontal_concat<T, U>>;

template <typename T, typename U>
  requires(matrix_col_size_v<T> == matrix_col_size_v<U>)
struct matrix_vertical_concat : append_range<T, U> {};

template <typename T, typename U>
using matrix_vertical_concat_t = typeof_t<matrix_vertical_concat<T, U>>;

template <template <typename...> typename F, typename... Args>
  requires is_variadic_pack_v<Args...>
struct zip_transform : matrix_col_apply<F, std::tuple<Args...>> {};

template <template <typename...> typename F, typename... Args>
using zip_transform_t = typeof_t<zip_transform<F, Args...>>;

template <typename T>
struct is_variadic_tuple : unpack_t<is_variadic_pack, T> {};

template <typename T>
using is_variadic_tuple_t = typeof_t<is_variadic_tuple<T>>;

template <typename T>
inline constexpr auto is_variadic_tuple_v = typev<is_variadic_tuple_t<T>>;

template <typename indices, typename T>
  requires(is_sequence_v<indices> && is_variadic_tuple_v<T> &&
           sizeof_t_v<indices> == sizeof_t_v<T>)
struct elements {
  template <typename U, typename V, typename W>
  struct impl;

  template <template <typename, auto...> typename U, typename V, auto n,
            auto... N, template <typename...> typename W, typename X,
            typename... Args, typename Y>
  struct impl<U<V, n, N...>, W<X, Args...>, Y>
      : impl<U<V, N...>, W<Args...>, append_t<Y, element_t<n, X>>> {};

  template <template <typename, auto...> typename U, typename V, auto n,
            template <typename...> typename W, typename X, typename Y>
  struct impl<U<V, n>, W<X>, Y> : append<Y, element_t<n, X>> {};

  using type = typeof_t<impl<indices, T, row_type_t<T>>>;
};

template <typename indices, typename T>
using elements_t = typeof_t<elements<indices, T>>;

template <auto N, typename T, typename... Args>
struct cat_element {
  static constexpr auto size = sizeof_t_v<T>;
  using type = type_if < N<size, element<N, T>, cat_element<N - size, Args...>>;
};

template <auto N, typename T>
struct cat_element<N, T> : element<N, T> {};

template <auto N, typename... Args>
using cat_element_t = typeof_t<cat_element<N, Args...>>;

template <auto N, typename... Args>
inline constexpr auto cat_element_v = typev<cat_element_t<N, Args...>>;

template <typename T, typename U>
  requires(is_variadic_tuple_v<T> && is_variadic_tuple_v<U>)
struct apply_indices {
  template <typename V, typename W>
  struct impl;

  template <template <typename...> typename V, typename W, typename... Args,
            typename X>
  struct impl<V<W, Args...>, X>
      : impl<V<Args...>, append_t<X, elements_t<W, U>>> {};

  template <template <typename...> typename V, typename W, typename X>
  struct impl<V<W>, X> : append<X, elements_t<W, U>> {};

  using type = typeof_t<impl<T, tuple_t<>>>;
};

template <typename T, typename U>
using apply_indices_t = typeof_t<apply_indices<T, U>>;

template <typename... Args>
  requires(is_variadic_v<Args> && ...)
struct combinations {
  template <typename T, typename indices>
  struct impl : append<T, elements_t<indices, tuple_t<Args...>>> {};

  using type = loop_indices_t<std::integer_sequence<int, sizeof_t_v<Args>...>,
                              tuple_t<>, impl, true>;
};

template <typename... Args>
using combinations_t = typeof_t<combinations<Args...>>;

template <template <typename...> typename F, typename... Args>
struct product {
  template <auto N, typename T>
  struct impl {
    using type = unary_t<F, 0, 1, element_t<N, T>>;
  };

  using type = expand_t<impl, combinations_t<Args...>>;
};

template <template <typename...> typename F, typename... Args>
using product_t = typeof_t<product<F, Args...>>;

template <typename indices, typename T>
struct expand_cartesian_product {
  static constexpr auto value = is_sequence_v<element_t<0, T>>;

  template <typename U>
  using call = get_matrix_element_t<first_v<U>, second_v<U>, T>;

  template <typename U>
  using impl = apply_if_t<value, to_sequence, transform_t<call, U>>;

  using type = transform_t<impl, indices>;
};

template <typename indices, typename T>
using expand_cartesian_product_t =
    typeof_t<expand_cartesian_product<indices, T>>;

template <typename T>
  requires is_square_matrix_v<T>
struct matrix_combinations
    : apply_indices<next_permutation_list<index_sequence_of_t<T>>, T> {};

template <typename T>
using matrix_combinations_t = typeof_t<matrix_combinations<T>>;

template <int p, int r, typename T, typename outer, typename inner,
          template <typename, typename> typename comparator = less_equal_t>
struct partition_matrix {
  template <auto i, typename V>
  using call = get_matrix_element_t<pos<i, outer>, pos<i, inner>, V>;

  using x = call<r - 1, T>;

  template <int i, int j, typename V, bool = false>
  struct next : std::type_identity<V> {};

  template <int i, int j, typename V>
  struct next<i, j, V, true>
      : swap_matrix_element<pos<i, outer>, pos<i, inner>, pos<j, outer>,
                            pos<j, inner>, V> {};

  template <int i, int j, int k, typename V>
  struct impl {
    static constexpr auto value = typev<comparator<call<j, V>, x>>;

    using cond = typeof_t<next<i + 1, j, V, value>>;
    using type = typeof_t<impl<i + value, j + 1, k, cond>>;
  };

  template <int i, int k, typename V>
  struct impl<i, k, k, V> {
    using type = index_type<i + 1, typeof_t<next<i + 1, r - 1, V, 1>>>;
  };

  using type = typeof_t<impl<p - 1, p, r - 1, T>>;
};

template <int p, int r, typename T, typename outer, typename inner,
          template <typename, typename> typename comparator = less_equal_t>
using partition_matrix_t =
    typeof_t<partition_matrix<p, r, T, outer, inner, comparator>>;

template <int b, int e, int p, typename T, template <typename...> typename F>
struct gather {
  using prev = stable_partition_t<b, p, T, negaf<F>::template apply>;
  using next = stable_partition_t<p, e, typeof_t<prev>, F>;

  using type = triple<typev<prev>, typev<next>, typeof_t<next>>;
};

template <auto N, typename T>
struct sort_row : quick_sort<get_matrix_row_t<N, T>> {};

template <auto N, typename T>
using sort_row_t = typeof_t<sort_row<N, T>>;

template <auto N, typename T>
struct sort_col : quick_sort<get_matrix_col_t<N, T>> {};

template <auto N, typename T>
using sort_col_t = typeof_t<sort_col<N, T>>;

template <auto N, typename T>
struct matrix_row_sort : set_matrix_row<N, sort_row_t<N, T>, T> {};

template <auto N, typename T>
using matrix_row_sort_t = typeof_t<matrix_row_sort<N, T>>;

template <auto N, typename T>
struct matrix_col_sort : set_matrix_col<N, sort_col_t<N, T>, T> {};

template <auto N, typename T>
using matrix_col_sort_t = typeof_t<matrix_col_sort<N, T>>;

template <auto lower, auto upper, typename T>
struct matrix_row_sort_range
    : matrix_range_operator<lower, upper, T, matrix_row_sort> {};

template <auto lower, auto upper, typename T>
using matrix_row_sort_range_t =
    typeof_t<matrix_row_sort_range<lower, upper, T>>;

template <auto lower, auto upper, typename T>
struct matrix_col_sort_range
    : matrix_range_operator<lower, upper, T, matrix_col_sort> {};

template <auto lower, auto upper, typename T>
using matrix_col_sort_range_t =
    typeof_t<matrix_col_sort_range<lower, upper, T>>;

template <typename T>
struct matrix_rowwise_sort : matrix_row_sort_range<0, matrix_row_size_v<T>, T> {
};

template <typename T>
using matrix_rowwise_sort_t = typeof_t<matrix_rowwise_sort<T>>;

template <typename T>
struct matrix_colwise_sort : matrix_col_sort_range<0, matrix_col_size_v<T>, T> {
};

template <typename T>
using matrix_colwise_sort_t = typeof_t<matrix_colwise_sort<T>>;

template <typename T>
struct matrix_row_col_sort : matrix_colwise_sort<matrix_rowwise_sort_t<T>> {};

template <typename T>
using matrix_row_col_sort_t = typeof_t<matrix_row_col_sort<T>>;

template <typename T>
struct matrix_col_row_sort : matrix_rowwise_sort<matrix_colwise_sort_t<T>> {};

template <typename T>
using matrix_col_row_sort_t = typeof_t<matrix_col_row_sort<T>>;

template <typename T, typename U>
  requires(is_matrix_v<T> && is_variadic_v<U>)
struct is_in_matrix {
  static constexpr int len = sizeof_t_v<U>;

  static constexpr int row = matrix_row_size_v<T>;
  static constexpr int col = matrix_col_size_v<T>;

  using head = front_t<U>;
  using mark = sentinel_t<U>;

  template <int i, int j, int k, typename V>
  struct find {
    struct next {
      using curr = set_matrix_element_t<i, j, mark, V>;

      template <int m, int n, int p = 2 * n - 1>
      using call = find<i + m * p, j + !m * p, k + 1, curr>;

      using type = disjunction_invoke_t<typeof_t, call<0, 0>, call<0, 1>,
                                        call<1, 0>, call<1, 1>>;
    };

    template <typename W, bool>
    struct impl
        : std::conditional_t<
              !std::is_same_v<get_matrix_element_t<i, j, V>, element_t<k, U>>,
              std::false_type, next> {};

    template <typename W>
    struct impl<W, false> : std::false_type {};

    using type = type_if < len <= k, std::true_type, impl < V,
          0 <= i &&i<row && 0 <= j && j<col>>;
  };

  template <int i, int j, int k, int l>
  struct impl {
    template <typename V, bool = std::is_same_v<V, head>>
    struct next : std::conditional_t<typeof_v<find<i, j, 0, T>>, std::true_type,
                                     impl<i, j + 1, k, l>> {};

    template <typename V>
    struct next<V, false> : impl<i, j + 1, k, l> {};

    using type = typeof_t<next<get_matrix_element_t<i, j, T>>>;
  };

  template <int i, int k, int l>
  struct impl<i, l, k, l> : impl<i + 1, 0, k, l> {};

  template <int j, int k, int l>
  struct impl<k, j, k, l> : std::false_type {};

  using type = typeof_t<impl<0, 0, row, col>>;
};

template <typename T, typename U>
using is_in_matrix_t = typeof_t<is_in_matrix<T, U>>;

template <typename T, typename U>
inline constexpr auto is_in_matrix_v = typev<is_in_matrix_t<T, U>>;

template <int R, typename T>
struct rank_to_permutation {
  static constexpr auto N = sizeof_t_v<T>;

  template <typename U, int i, int j>
  struct next : next<sub_t<i, element_v<i + 1, U>, U>, i + 1, j> {};

  template <typename U, int j>
  struct next<U, j, j> : std::type_identity<U> {};

  template <typename U, typename V, int f, int r, int p, int q>
  struct impl {
    static constexpr auto i = r / f;

    using curr = typeof_t<next<V, i, N - 1>>;
    using type = typeof_t<impl<sub_t<p, element_v<i, V>, U>, curr,
                               f / (N - p - 1), r % f, p + 1, q>>;
  };

  template <typename U, typename V, int f, int r, int q>
  struct impl<U, V, f, r, q, q> {
    using type = sub_t<q, element_v<0, V>, U>;
  };

  using type =
      typeof_t<impl<T, index_range<0, N>, factorial_v<N - 1>, R, 0, N - 1>>;
};

template <int R, typename T>
using rank_to_permutation_t = typeof_t<rank_to_permutation<R, T>>;

template <typename T>
struct permutation_to_rank {
  static constexpr auto N = sizeof_t_v<T>;

  template <typename U, int n, int i, int j>
  struct next : next<U, n + !element_v<i, U>, i + 1, j> {};

  template <typename U, int n, int j>
  struct next<U, n, j, j> {
    static constexpr auto value = n;
  };

  template <typename U, int f, int r, int p, int q>
  struct impl {
    static constexpr auto upper = element_v<p, T>;

    static constexpr auto index = r + typev<next<U, 0, 0, upper>> * f;
    static constexpr auto value =
        typev<impl<sub_t<upper, 1, U>, f / (N - p - 1), index, p + 1, q>>;
  };

  template <typename U, int f, int r, int q>
  struct impl<U, f, r, q, q> {
    static constexpr auto value = r;
  };

  static constexpr auto value =
      typev<impl<fill_c<N, 0>, factorial_v<N - 1>, 0, 0, N - 1>>;
};

template <typename T>
inline constexpr auto permutation_to_rank_v = typev<permutation_to_rank<T>>;

template <typename P, typename T>
struct next_rank_permutation {
  static constexpr auto N = sizeof_t_v<T>;

  template <typename U, typename V, int i, int j>
  struct impl : impl<U, append_t<V, element_t<element_v<i, U>, T>>, i + 1, j> {
  };

  template <typename U, typename V, int j>
  struct impl<U, V, j, j> : std::type_identity<V> {};

  using type = typeof_t<impl<P, clear_t<T>, 0, N>>;
};

template <typename P, typename T>
using next_rank_permutation_t = typeof_t<next_rank_permutation<P, T>>;

template <typename T>
struct rank_permutation {
  static constexpr auto N = sizeof_t_v<T>;

  template <typename U, typename V, int i, int j>
  struct impl {
    using perm = rank_to_permutation_t<i, U>;
    using next = append_t<V, next_rank_permutation_t<perm, T>>;

    static constexpr auto rank = permutation_to_rank_v<perm>;
    using type = typeof_t<impl<perm, next, rank + 1, j>>;
  };

  template <typename U, typename V, int j>
  struct impl<U, V, j, j> : std::type_identity<V> {};

  using type = typeof_t<impl<fill_c<N, 0>, std::tuple<>, 0, factorial_v<N>>>;
};

template <typename T>
using rank_permutation_t = typeof_t<rank_permutation<T>>;
}