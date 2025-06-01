#pragma once

#include <cstddef>

namespace pm {

namespace meta {
/**
 * @brief Template structure for compile-time dispatch.
 *
 * This structure provides compile-time dispatch capability
 * using recursive template mechanism.
 *
 * @tparam level The dispatch level
 */
template <size_t level = 0>
struct dispatch : dispatch<level - 1> {};

/**
 * @brief Specialization of the dispatch template structure to stop recursion.
 *
 * This base case specialization terminates the recursive inheritance chain
 * for the dispatch mechanism.
 */
template <>
struct dispatch<0> {};

}  // namespace meta

}  // namespace pm