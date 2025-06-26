#pragma once

#include "./expand.hpp"

namespace core::meta::utils {

template <auto n, typename T>
struct fill {
  template <auto N, typename U>
  struct impl : U {};

  template <template <auto, typename> typename F>
  using call = expand<F, T, index_sequence_of_c<n>>;

  using curr = type_if<!has_type_v<T> || !has_value_type_v<T>, call<identity>,
                       call<impl>>;
  using type = rename_if_t<is_variadic_type_v<T>, curr, std::tuple<>>;
};

template <auto n, typename T>
using fill_t = typeof_t<fill<n, T>>;

template <auto n, auto v>
using fill_c = typeof_t<fill<n, c_<v>>>;

template <typename T, typename U>
struct assign : rename<fill_t<sizeof_t_v<T>, U>, clear_t<T>> {};

template <typename T, typename U>
using assign_t = typeof_t<assign<T, U>>;

template <auto N, template <typename...> typename T>
struct nest_fill : to_nest<fill_t<N, T<>>> {};

template <auto N, template <typename...> typename T>
using nest_fill_t = typeof_t<nest_fill<N, T>>;
}  // namespace core::meta::utils