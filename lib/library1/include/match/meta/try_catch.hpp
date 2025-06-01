#pragma once

#include "./dispatch.hpp"

namespace pm::meta {

/**
 * @brief Tag type for try operation.
 *
 * This tag type is used to select the primary implementation
 * in a try-catch style template metaprogramming pattern.
 */
using try_t = dispatch<1>;

/**
 * @brief Tag type for catch operation.
 *
 * This tag type is used to select the fallback implementation
 * in a try-catch style template metaprogramming pattern.
 */
using catch_t = dispatch<0>;

}  // namespace meta
}  // namespace pm