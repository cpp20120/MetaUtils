#pragma once

#include <any>

namespace std {
/**
 * @brief Casts a pointer to std::any object to a pointer to Target type.
 *
 * If the std::any object contains a value of type Target or its derived type,
 * returns a pointer to the Target object. Otherwise returns nullptr.
 *
 * @tparam Target The target type to cast to
 * @param a Pointer to the std::any object
 * @return Target* Pointer to the Target object or nullptr
 */
template <typename Target>
Target const* dyn_cast(any const* a) {
  return any_cast<Target const>(a);
}

/**
 * @brief Casts a pointer to std::any object to a pointer to Target type
 * (non-const version).
 *
 * If the std::any object contains a value of type Target or its derived type,
 * returns a pointer to the Target object. Otherwise returns nullptr.
 *
 * @tparam Target The target type to cast to
 * @param a Pointer to the std::any object
 * @return Target* Pointer to the Target object or nullptr
 */
template <typename Target>
Target* dyn_cast(any* a) {
  return any_cast<Target>(a);
}
}  // namespace std