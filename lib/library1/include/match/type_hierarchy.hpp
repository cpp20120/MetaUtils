/**
 * @file type_hierarchy.hpp
 * @brief Compile-time type hierarchy system with runtime identification
 * @defgroup TypeHierarchy Type Hierarchy System
 * @ingroup MetaProgramming
 *
 * Provides a configurable type hierarchy system that enables:
 * - Type identification at runtime
 * - Safe downcasting between hierarchy levels
 * - Compile-time configuration of hierarchy depth and width
 * - Efficient type checking using bitmask operations
 */

#pragma once

#include <boost/assert.hpp>
#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <utility>

#include "./meta/fold_add.hpp"
#include "./meta/scan_add.hpp"

namespace pm {

/**
 * @namespace type_hierarchy_detail
 * @brief Implementation details of the type hierarchy system
 * @private
 */
namespace type_hierarchy_detail {

/**
 * @brief Tag type representing a level in the type hierarchy
 * @tparam Level The hierarchy level this tag represents
 */
template <int Level>
struct level_tag {};

/**
 * @brief Metafunction to compute a type's level in the hierarchy
 * @tparam T The type to check
 * @tparam LevelTag Current level being checked (used for recursion)
 *
 * This metafunction recursively checks if a type is convertible to
 * level tags to determine its position in the hierarchy.
 */
template <typename T, typename LevelTag = level_tag<0>>
struct level_of;

/**
 * @brief Specialization for level computation at level N
 * @tparam T The type being checked
 * @tparam N The current level being tested
 *
 * Uses SFINAE to recursively check higher levels if the type is
 * convertible to the current level tag.
 */
template <typename T, int N>
struct level_of<T, level_tag<N>>
    : std::conditional_t<std::is_convertible<T, level_tag<N>>::value,
                         level_of<T, level_tag<N + 1>>,
                         std::integral_constant<int, N - 1>> {};

/**
 * @brief Base case for non-hierarchy types
 * @tparam T The type being checked
 *
 * Specialization for level 0 that handles:
 * - Types not in the hierarchy (returns -1)
 * - Types convertible to level_tag<0> (continues to level 1)
 */
template <typename T>
struct level_of<T, level_tag<0>>
    : std::conditional_t<std::is_convertible<T, level_tag<0>>::value,
                         level_of<T, level_tag<1>>,
                         std::integral_constant<int, -1>> {};

/**
 * @brief Specialization for void type
 * @details Explicitly sets void to level 0 in the hierarchy
 */
template <>
struct level_of<void, level_tag<0>> : std::integral_constant<int, 0> {};

/**
 * @brief Helper value for a type's hierarchy level
 * @tparam T The type to check
 */
template <typename T>
constexpr int level_of_v = level_of<T>::value;

/**
 * @brief Main type identification holder for hierarchy types
 * @tparam Config The hierarchy configuration
 * @tparam T... Type parameters (implementation detail)
 *
 * This template is specialized for three cases:
 * 1. Root type (no derived/super types)
 * 2. Derived type with no explicit supertype
 * 3. Derived type with explicit supertype
 */
template <typename Config, typename... T>
struct id_holder;

/**
 * @brief Helper for holding configuration information
 * @tparam Config The configuration type
 */
template <typename Config>
struct config_holder {};

/**
 * @brief Metafunction to extract configuration from hierarchy types
 * @tparam T The type to extract configuration from
 *
 * Uses SFINAE to detect if a type has a configuration and returns:
 * - The configuration if found
 * - void if no configuration exists
 */
template <typename T>
struct get_config {
  template <typename Config>
  static constexpr auto deduce(config_holder<Config>*) -> Config;
  template <typename Config>
  static constexpr auto deduce(const config_holder<Config>*) -> Config;
  static constexpr auto deduce(void*) -> void;

  using type = decltype(deduce(std::declval<T*>()));
};

/**
 * @brief Helper alias for configuration type extraction
 * @tparam T The type to get configuration from
 */
template <typename T>
using get_config_t = typename get_config<T>::type;

/**
 * @brief Root type specialization of id_holder
 * @tparam Config The hierarchy configuration
 *
 * Provides:
 * - Base type inheritance
 * - Level 0 tagging
 * - Configuration storage
 * - Type ID storage
 */
template <typename Config>
struct id_holder<Config>
    : Config::base_type, level_tag<0>, config_holder<Config> {
 private:
  typename Config::id_t m_type_hierarchy_id__;
  template <typename, typename...>
  friend struct id_holder;

 protected:
  id_holder() = default;

 public:
  /**
   * @brief Gets the type's hierarchy identifier
   * @return The unique type identifier in the hierarchy
   */
  typename Config::id_t type_hierarchy_id__() const {
    return m_type_hierarchy_id__;
  }
};

/**
 * @brief Specialization for derived types without explicit supertype
 * @tparam Config The hierarchy configuration
 * @tparam Derived The derived type
 *
 * Automatically places the type at level 1 in the hierarchy.
 */
template <typename Config, typename Derived>
struct id_holder<Config, Derived, void> : id_holder<Config>, level_tag<1> {
 private:
  static const typename Config::id_t s_type_hierarchy__;
  static typename Config::id_t type_hierarchy_init_id__();
  template <typename, typename...>
  friend struct id_holder;

 public:
  /**
   * @brief Constructor that initializes the type ID
   */
  id_holder() { id_holder::m_type_hierarchy_id__ = s_type_hierarchy__; }
};

/**
 * @brief Specialization for derived types with explicit supertype
 * @tparam Config The hierarchy configuration
 * @tparam Derived The derived type
 * @tparam SuperType The immediate parent type
 *
 * Places the type at one level above its supertype in the hierarchy.
 */
template <typename Config, typename Derived, typename SuperType>
struct id_holder<Config, Derived, SuperType>
    : SuperType, level_tag<level_of_v<SuperType> + 1> {
 private:
  static_assert(level_of_v<SuperType> <
                    typename Config::bits_per_level{}.size(),
                "Supported number of hierarchy levels exceeded.");
  static const typename Config::id_t s_type_hierarchy__;
  static typename Config::id_t type_hierarchy_init_id__();
  template <typename, typename...>
  friend struct id_holder;

 public:
  using super_t = id_holder<Config, Derived, SuperType>;

  /**
   * @brief Constructs the type holder
   * @tparam Args Argument types for supertype construction
   * @param args Arguments to forward to supertype constructor
   */
  template <typename... Args>
  constexpr id_holder(Args&&... args) : SuperType{std::forward<Args>(args)...} {
    SuperType::m_type_hierarchy_id__ = s_type_hierarchy__;
  }
};

/**
 * @brief Compile-time array for sequence operations
 * @tparam T Element type
 * @tparam N Array size
 */
template <typename T, size_t N>
struct constexpr_array {
  T data[N];
};

/**
 * @brief Gets an element from a sequence at compile-time
 * @tparam N Index to retrieve
 * @tparam Ns Sequence indices
 * @param seq The integer sequence
 * @return The value at index N
 */
template <int N, int... Ns>
constexpr int get(std::integer_sequence<int, Ns...>) {
  return constexpr_array<int, sizeof...(Ns)>{{Ns...}}.data[N];
}

/**
 * @brief Gets a level shift value from configuration
 * @tparam Config The hierarchy configuration
 * @tparam N The level to check
 */
template <typename Config, int N>
constexpr int lookup_v = get<N>(typename Config::level_shifts{});

/**
 * @brief Computes the bit shift for a type's level
 * @tparam Config The hierarchy configuration
 * @tparam T The type to check
 */
template <typename Config, typename T>
constexpr typename Config::id_t shift_v = lookup_v<Config, level_of_v<T>>;

/**
 * @brief Computes the bitmask for a type's level
 * @tparam Config The hierarchy configuration
 * @tparam T The type to check
 */
template <typename Config, typename T>
constexpr typename Config::id_t mask_v = (1 << shift_v<Config, T>)-1;

/**
 * @brief Generates the next available ID for a hierarchy level
 * @tparam Config The hierarchy configuration
 * @tparam Level The level to generate an ID for
 * @return A new unique ID for the level
 */
template <typename Config, int Level = 0>
typename Config::id_t next_id() {
  static typename Config::id_t id{};
  ++id;
  BOOST_ASSERT_MSG(id < (static_cast<typename Config::id_t>(1)
                         << get<Level>(typename Config::bits_per_level{})),
                   "Ids for level are exhausted.");
  return id;
}

/**
 * @brief Implementation of type conversion checking
 * @tparam TargetType The target type to check against
 * @tparam TargetConfig Target type's configuration
 * @tparam SourceConfig Source type's configuration
 */
template <typename TargetType, typename TargetConfig, typename SourceConfig>
struct convertible_to_impl {
  static constexpr bool apply(id_holder<SourceConfig> const&) { return false; }
};

/**
 * @brief Specialization for types with same configuration
 * @tparam TargetType The target type to check against
 * @tparam Config The shared configuration
 */
template <typename TargetType, typename Config>
struct convertible_to_impl<TargetType, Config, Config> {
  /**
   * @brief Checks if conversion is valid
   * @param src The source instance to check
   * @return true if conversion is valid, false otherwise
   */
  static constexpr bool apply(id_holder<Config> const& src) {
    return (mask_v<Config, TargetType> & src.type_hierarchy_id__()) ==
           TargetType::s_type_hierarchy__;
  }
};

/**
 * @brief Specialization for base type conversion
 * @tparam Config The hierarchy configuration
 */
template <typename Config>
struct convertible_to_impl<typename Config::base_type, void, Config> {
  static constexpr bool apply(id_holder<Config> const&) { return true; }
};

/**
 * @brief Checks if a hierarchy type can be converted to another
 * @tparam TargetType The target type
 * @tparam SourceType The source type
 * @param x The instance to check
 * @return true if conversion is valid, false otherwise
 */
template <typename TargetType, typename SourceType,
          typename = std::enable_if_t<
              std::is_base_of<level_tag<0>, SourceType>::value>>
bool constexpr convertible_to(SourceType const& x) {
  using config_t = get_config_t<SourceType>;
  auto const& src = static_cast<id_holder<config_t> const&>(x);
  return convertible_to_impl<TargetType, get_config_t<TargetType>,
                             config_t>::apply(src);
}

/**
 * @brief Initializes type ID for types without supertype
 * @tparam Config The hierarchy configuration
 * @tparam Derived The derived type
 */
template <typename Config, typename Derived>
typename Config::id_t
id_holder<Config, Derived, void>::type_hierarchy_init_id__() {
  static const typename Config::id_t id = next_id<Config, 0>();
  return id;
}

/**
 * @brief Static ID storage for types without supertype
 * @tparam Config The hierarchy configuration
 * @tparam Derived The derived type
 */
template <typename Config, typename Derived>
const typename Config::id_t
    id_holder<Config, Derived, void>::s_type_hierarchy__ =
        type_hierarchy_init_id__();

/**
 * @brief Initializes type ID for types with supertype
 * @tparam Config The hierarchy configuration
 * @tparam Derived The derived type
 * @tparam SuperType The parent type
 */
template <typename Config, typename Derived, typename SuperType>
typename Config::id_t
id_holder<Config, Derived, SuperType>::type_hierarchy_init_id__() {
  constexpr auto shift = shift_v<Config, SuperType>;
  static const typename Config::id_t id =
      SuperType::type_hierarchy_init_id__() |
      (next_id<Config, level_of_v<SuperType>>() << shift);
  return id;
}

/**
 * @brief Static ID storage for types with supertype
 * @tparam Config The hierarchy configuration
 * @tparam Derived The derived type
 * @tparam SuperType The parent type
 */
template <typename Config, typename Derived, typename SuperType>
const typename Config::id_t
    id_holder<Config, Derived, SuperType>::s_type_hierarchy__ =
        type_hierarchy_init_id__();

/**
 * @brief Safe dynamic cast for hierarchy types
 * @tparam TargetType The target type
 * @tparam Config The hierarchy configuration
 * @param p Pointer to cast
 * @return Cast pointer if valid, nullptr otherwise
 */
template <typename TargetType, typename Config>
constexpr TargetType* dyn_cast(id_holder<Config>* p) {
  return convertible_to<TargetType>(*p) ? static_cast<TargetType*>(p) : nullptr;
}

/**
 * @brief Safe dynamic cast for hierarchy types (no supertype version)
 * @tparam TargetType The target type
 * @tparam Config The hierarchy configuration
 * @tparam T1 Additional type parameter
 * @param p Pointer to cast
 * @return Cast pointer if valid, nullptr otherwise
 */
template <typename TargetType, typename Config, typename T1>
constexpr TargetType* dyn_cast(id_holder<Config, T1, void>* p) {
  return convertible_to<TargetType>(*p) ? static_cast<TargetType*>(p) : nullptr;
}

/**
 * @brief Safe dynamic cast for const hierarchy types
 * @tparam TargetType The target type
 * @tparam Config The hierarchy configuration
 * @param p Pointer to cast
 * @return Cast pointer if valid, nullptr otherwise
 */
template <typename TargetType, typename Config>
constexpr TargetType const* dyn_cast(id_holder<Config> const* p) {
  return convertible_to<TargetType>(*p) ? static_cast<TargetType*>(p) : nullptr;
}

/**
 * @brief Safe dynamic cast for const hierarchy types (no supertype version)
 * @tparam TargetType The target type
 * @tparam Config The hierarchy configuration
 * @tparam D Additional type parameter
 * @param p Pointer to cast
 * @return Cast pointer if valid, nullptr otherwise
 */
template <typename TargetType, typename Config, typename D>
constexpr TargetType const* dyn_cast(id_holder<Config, D, void> const* p) {
  return convertible_to<TargetType>(*p) ? static_cast<TargetType const*>(p)
                                        : nullptr;
}

/**
 * @brief Selects appropriate integer type for given bit count
 * @tparam NumBits Number of bits needed
 */
template <int NumBits>
struct int_for_bits {
  using type = typename int_for_bits<NumBits + 1>::type;
};

/// Specialization for 8 bits
template <>
struct int_for_bits<8> {
  using type = std::uint8_t;
};

/// Specialization for 16 bits
template <>
struct int_for_bits<16> {
  using type = std::uint16_t;
};

/// Specialization for 32 bits
template <>
struct int_for_bits<32> {
  using type = std::uint32_t;
};

/// Specialization for 64 bits
template <>
struct int_for_bits<64> {
  using type = std::uint64_t;
};

/**
 * @brief Configures and builds type hierarchies
 * @tparam BaseType The root type of the hierarchy
 * @tparam BitsPerLevel Number of bits to allocate per hierarchy level
 */
template <typename BaseType, int... BitsPerLevel>
struct builder {
  /**
   * @brief Hierarchy configuration parameters
   */
  struct config {
    using base_type = BaseType;  ///< Root type of the hierarchy
    using bits_per_level =
        std::integer_sequence<int, BitsPerLevel...>;  ///< Bits per level
    using level_shifts =
        meta::scan_add_t<int, 0, BitsPerLevel...>;  ///< Bit shifts per level
    using id_t = typename int_for_bits<
        meta::fold_add_v<int, BitsPerLevel...>>::type;  ///< Type for IDs
  };

  using root_t =
      type_hierarchy_detail::id_holder<config>;  ///< Root type holder
};

/**
 * @brief Default builder specialization (4 levels, 8 bits each)
 * @tparam BaseType The root type of the hierarchy
 */
template <typename BaseType>
struct builder<BaseType> : builder<BaseType, 8, 8, 8, 8> {};

/**
 * @brief Root type alias for configured hierarchies
 * @tparam BaseType The root type of the hierarchy
 * @tparam BitsPerLevel Number of bits to allocate per hierarchy level
 */
template <typename BaseType, int... BitsPerLevel>
using root_t = typename builder<BaseType, BitsPerLevel...>::root_t;

/**
 * @brief Defines subtype relationships in the hierarchy
 * @tparam Derived The derived type
 * @tparam Super The immediate parent type
 */
template <typename Derived, typename Super>
struct sub_type_impl {
  using type = id_holder<get_config_t<Super>, Derived, Super>;
};

/**
 * @brief Specialization for direct root descendants
 * @tparam Derived The derived type
 * @tparam Config The hierarchy configuration
 */
template <typename Derived, typename Config>
struct sub_type_impl<Derived, id_holder<Config>> {
  using type = id_holder<Config, Derived, void>;
};

}  // namespace type_hierarchy_detail

/**
 * @namespace type_hierarchy
 * @brief Public interface for the type hierarchy system
 */
namespace type_hierarchy {

/**
 * @brief Creates a new type hierarchy from a base type
 * @tparam BaseType The root type of the hierarchy
 * @tparam BitsPerLevel Number of bits to allocate per hierarchy level
 */
template <typename BaseType, int... BitsPerLevel>
using from_base = type_hierarchy_detail::root_t<BaseType, BitsPerLevel...>;

/**
 * @brief Defines a subtype in the hierarchy
 * @tparam Derived The derived type
 * @tparam Super The immediate parent type
 */
template <typename Derived, typename Super>
using sub_type =
    typename type_hierarchy_detail::sub_type_impl<Derived, Super>::type;

/// @copydoc convertible_to
using type_hierarchy_detail::convertible_to;

}  // namespace type_hierarchy

/// @copydoc type_hierarchy::convertible_to
using type_hierarchy::convertible_to;

/// @copydoc type_hierarchy::sub_type
using type_hierarchy::sub_type;

/**
 * @def NI_SUB_TYPE(DERIVED, SUPER)
 * @brief Convenience macro for defining hierarchy subtypes
 * @param DERIVED The new derived type name
 * @param SUPER The immediate parent type
 */
#define NI_SUB_TYPE(DERIVED, SUPER) \
  DERIVED:                          \
 public                             \
  pm::sub_type<DERIVED, SUPER>

}  // namespace pm