#pragma once

#include <concepts>
#include <coroutine>
#include <optional>
#include <type_traits>

namespace core::meta::type_traits {

/**
 * @brief Constant for the mathematical constant e (Euler's number).
 * @tparam T The type of the constant.
 */
template <typename T>
inline constexpr T e = T(2.7182818284590452353);

/**
 * @brief Constant for the mathematical constant pi.
 * @tparam T The type of the constant.
 */
template <typename T>
inline constexpr T pi = T(3.1415926535897932385);

/**
 * @brief Template alias for creating a compile-time constant.
 * @tparam N The value of the constant.
 * @tparam T The type of the constant.
 */
template <auto N, typename T = std::remove_cvref_t<decltype(N)>>
using c_ = std::integral_constant<T, N>;

/**
 * @brief Compile-time constant for 0.
 */
using c_0 = c_<0>;

/**
 * @brief Compile-time constant for 1.
 */
using c_1 = c_<1>;

/**
 * @brief Compile-time constant for 2.
 */
using c_2 = c_<2>;

/**
 * @brief Compile-time constant for 3.
 */
using c_3 = c_<3>;

/**
 * @brief Compile-time constant for 4.
 */
using c_4 = c_<4>;

/**
 * @brief Compile-time constant for 5.
 */
using c_5 = c_<5>;

/**
 * @brief Compile-time constant for 6.
 */
using c_6 = c_<6>;

/**
 * @brief Compile-time constant for 7.
 */
using c_7 = c_<7>;

/**
 * @brief Compile-time constant for 8.
 */
using c_8 = c_<8>;

/**
 * @brief Compile-time constant for 9.
 */
using c_9 = c_<9>;
/**
 * @brief Template alias for creating a compile-time boolean constant.
 * @tparam N The boolean value of the constant.
 */
template <auto N>
using bool_ = std::bool_constant<N>;

/**
 * @brief Template alias for creating an index type.
 * @tparam N The value of the index.
 */
template <size_t N>
using index_t = c_<N, size_t>;

/**
 * @brief Compile-time index constant.
 * @tparam N The value of the index.
 */
template <size_t N>
constexpr index_t<N> index = {};

/**
 * @brief Template alias for creating a compile-time constant of a given value.
 * @tparam v The value of the constant.
 */
template <auto v>
using constant_t = c_<v, std::decay_t<decltype(v)>>;

/**
 * @brief Compile-time constant of a given value.
 * @tparam v The value of the constant.
 */
template <auto v>
constexpr constant_t<v> constant = {};

}  // namespace core::meta::type_traits