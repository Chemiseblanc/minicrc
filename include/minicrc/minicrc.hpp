// SPDX-License-Identifier: BSL-1.0
//          Copyright Matthew Gibson 2023.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)
#ifndef MINICRC_HPP
#define MINICRC_HPP

// Core includes
#include <array>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <map>
#include <type_traits>

// Compiler detection
#define MINICRC_COMPILER_UNKNOWN 0
#define MINICRC_COMPILER_MSVC 1
#define MINICRC_COMPILER_CLANG 2
#define MINICRC_COMPILER_GCC 3
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER)
#define MINICRC_COMPILER MINICRC_COMPILER_MSVC
#elif defined(__clang__)
#define MINICRC_COMPILER MINICRC_COMPILER_CLANG
#elif defined(__GNUC__)
#define MINICRC_COMPILER MINICRC_COMPILER_GCC
#else
#define MINICRC_COMPIILER
#endif

// CPU Architecture detection
#define MINICRC_ARCH_UNKNOWN 0
#define MINICRC_ARCH_AMD64 1
#define MINICRC_ARCH_ARM64 2
#if MINICRC_COMPILER == MINICRC_COMPILER_MSVC
#define MINICRC_ARCH MINICRC_ARCH_UNKNOWN
#elif MINICRC_COMPILER == MINICRC_COMPILER_CLANG
#define MINICRC_ARCH MINICRC_ARCH_UNKNOWN
#elif MINICRC_COMPILER == MINICRC_COMPILER_GCC
#else
#define MINICRC_ARCH MINICRC_ARCH_UNKNOWN
#endif

// Compiler warning control
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER)
#define MINICRC_WARNING_PUSH _Pragma("warning(push)")
#define MINICRC_WARNING_POP _Pragma("warning(pop)")
#define MINICRC_SUPPRESS_UNUSED_CONST_VAR
#define MINICRC_SUPPRESS_C4293 _Pragma("warning(suppress:4293)")
#define MINICRC_SUPPRESS_C4333 _Pragma("warning(suppress:4333)")
#define MINICRC_DISABLE_C4333 _Pragma("warning(disable:4333)")
#elif defined(__clang__)
#define MINICRC_WARNING_PUSH _Pragma("clang diagnostic push")
#define MINICRC_WARNING_POP _Pragma("clang diagnostic pop")
#define MINICRC_SUPPRESS_UNUSED_CONST_VAR \
  _Pragma("clang diagnostic ignored \"-Wunused-const-variable\"")
#define MINICRC_SUPPRESS_C4293
#define MINICRC_SUPPRESS_C4333
#define MINICRC_DISABLE_C4333
#else
#define MINICRC_WARNING_PUSH
#define MINICRC_WARNING_POP
#define MINICRC_SUPPRESS_UNUSED_CONST_VAR
#define MINICRC_SUPPRESS_C4293
#define MINICRC_SUPPRESS_C4333
#define MINICRC_DISABLE_C4333
#endif

// extended constexpr
#if defined(__cpp_constexpr) && __cpp_constexpr >= 201304
#define MINICRC_EXTENDED_CONSTEXPR constexpr
#else
#define MINICRC_EXTENDED_CONSTEXPR
#endif

// constexpr with static variables
#if defined(__cpp_constexpr) && __cpp_constexpr >= 202110
#define MINICRC_STATICVAR_CONSTEXPR constexpr
#else
#define MINICRC_STATICVAR_CONSTEXPR
#endif

// if constexpr
#if defined(__cpp_if_constexpr) && __cpp_if_constexpr >= 201606
#define MINICRC_IF_CONSTEXPR if constexpr
#else
#define MINICRC_IF_CONSTEXPR if
#endif

// Feature testing macros
#if defined(__has_include) && __has_include(<version>)
#include <version>
#endif

// array constexpr
#if defined(__cpp_lib_array_constexpr) && __cpp_lib_array_constexpr >= 201603
#define MINICRC_ARRAY_CONSTEXPR constexpr
#else
#define MINICRC_ARRAY_CONSTEXPR
#endif

// is_constant_evaluated
#if defined(__cpp_lib_is_constant_evaluated) && \
    __cpp_lib_is_constant_evaluated >= 201811
#define MINICRC_IF_IS_CONSTEVAL if (std::is_constant_evaluated())
#elif defined(__has_builtint) && __has_builtin(__builtin_is_constant_evaluated)
#define MINICRC_IF_IS_CONSTEVAL if (__builtin_is_constant_evaluated())
#else
#define MINICRC_IF_IS_CONSTEVAL if (false)
#endif

// remove_cvref
#if defined(__cpp_lib_remove_cvref) && __cpp_lib_remove_cvref >= 201711
namespace minicrc {
using std::remove_cvref_t;
}
#else
namespace minicrc {
template <class T>
struct remove_cvref {
  using type =
      typename std::remove_cv<typename std::remove_reference<T>::type>::type;
};
template <class T>
using remove_cvref_t = typename remove_cvref<T>::type;
}  // namespace minicrc
#endif

// byte
#if defined(__cpp_lib_byte) && __cpp_lib_byte >= 201603
#include <cstddef>
namespace minicrc {
using std::byte;
}
#else
namespace minicrc {
using byte = unsigned char;
}
#endif

// Concepts
#define MINICRC_CONCEPTS_NONE 0
#define MINICRC_CONCEPTS_SFINAE 1
#define MINICRC_CONCEPTS_NATIVE 2
#ifndef MINICRC_USE_CONCEPTS
#if defined(__cpp_concepts) && __cpp_concepts >= 202002 && \
    defined(__cpp_lib_concepts) && __cpp_lib_concepts >= 202002
#else
#define MINICRC_USE_CONCEPTS MINICRC_CONCEPTS_SFINAE
#endif
#endif
#if MINICRC_USE_CONCEPTS == MINICRC_CONCEPTS_NATIVE
#include <concepts>
#define MINICRC_USE_CONCEPTS MINICRC_CONCEPTS_NATIVE
#define MINICRC_REQUIRES(Concept, T) Concept T
namespace minicrc {
template <typename T>
concept byte_sized = requires { sizeof(T) == sizeof(byte); };

template <typename T>
concept raw_data_view = requires {
                          T.data()->std::is_pointer_v;
                          T.size()->std::convertible_to(std::size_t);
                        };

using std::unsigned_integral;
}  // namespace minicrc
#elif MINICRC_USE_CONCEPTS == MINICRC_CONCEPTS_SFINAE
#define MINICRC_REQUIRES(Concept, T) typename T, Concept<T> = false
namespace minicrc {
template <typename T>
using byte_sized =
    typename std::enable_if<sizeof(T) == sizeof(byte), bool>::type;

template <typename T>
using raw_data_view = typename std::enable_if<
    std::is_pointer<
        decltype(static_cast<remove_cvref_t<T>*>(nullptr)->data())>::value &&
        std::is_convertible<
            decltype(static_cast<remove_cvref_t<T>*>(nullptr)->size()),
            std::size_t>::value,
    bool>::type;

template <typename T>
using unsigned_integral = typename std::enable_if<
    !std::is_signed<T>::value && std::is_integral<T>::value, bool>::type;
}  // namespace minicrc
#elif MINICRC_USE_CONCEPTS == MINICRC_CONCEPTS_NONE
#define MINICRC_REQUIRES(Concept, T) typename T
#endif

// string_view
#define MINICRC_STRING_VIEW_NONE 0
#define MINICRC_STRING_VIEW_STD 1
#ifndef MINICRC_USE_STRING_VIEW
#if defined(__cpp_lib_string_view) && __cpp_lib_string_view >= 201606
#define MINICRC_USE_STRING_VIEW MINICRC_STRING_VIEW_STD
#else
#define MINICRC_USE_STRING_VIEW MINICRC_STRING_VIEW_NONE
#endif
#endif
#if MINICRC_USE_STRING_VIEW == MINICRC_STRING_VIEW_STD
#include <string_view>
namespace minicrc {
using std::string_view;
}
#elif MINICRC_USE_STRING_VIEW == MINICRC_STRING_VIEW_NONE
#include <cstring>
namespace minicrc {
struct string_view_substitute {
  const char* ptr;
  constexpr string_view_substitute(const char* str) : ptr{str} {}
  constexpr const char* data() const noexcept { return ptr; }
  std::size_t size() const noexcept { return strlen(ptr); }
};
using string_view = string_view_substitute;
}  // namespace minicrc
#endif

// uint_least<N>
namespace minicrc {
/*
For >64bit support create a specialization for uint_least with your choice of
bigint type Eg: template<> struct minicrc::uint_least<minicrc::uint_bigint> {
    using type = ????; //your bigint type
};
*/
constexpr std::size_t uint_8bit = 0;
constexpr std::size_t uint_16bit = 1;
constexpr std::size_t uint_32bit = 2;
constexpr std::size_t uint_64bit = 3;
MINICRC_WARNING_PUSH
MINICRC_SUPPRESS_UNUSED_CONST_VAR
constexpr std::size_t uint_bigint = 4;
MINICRC_WARNING_POP

template <std::size_t N>
struct uint_least {};
template <>
struct uint_least<uint_8bit> {
  using type = std::uint_least8_t;
};
template <>
struct uint_least<uint_16bit> {
  using type = std::uint_least16_t;
};
template <>
struct uint_least<uint_32bit> {
  using type = std::uint_least32_t;
};
template <>
struct uint_least<uint_64bit> {
  using type = std::uint_least64_t;
};

template <std::size_t N>
using uint_least_t =
    typename uint_least<0 + (N > 8) + (N > 16) + (N > 32) + (N > 64)>::type;
}  // namespace minicrc

namespace minicrc {
struct owns_lut {};
struct borrows_lut {};
struct fixed_lut {};

namespace detail {
template <std::size_t N, MINICRC_REQUIRES(unsigned_integral, T)>
MINICRC_EXTENDED_CONSTEXPR T bit_reverse(T t) noexcept {
  remove_cvref_t<T> reversed{0};
  for (std::size_t i = 0; i < N; ++i) {
    reversed |= ((t & (std::size_t{1} << i)) >> i) << (N - std::size_t{1} - i);
  }
  return reversed;
}

template <std::size_t N, MINICRC_REQUIRES(unsigned_integral, T)>
MINICRC_EXTENDED_CONSTEXPR T bit_mask() {
  MINICRC_IF_CONSTEXPR(N == sizeof(T) * CHAR_BIT) {
    return static_cast<T>(~static_cast<T>(0u));
  }
  else {
    MINICRC_SUPPRESS_C4293
    return (std::size_t{1} << N) - std::size_t{1};
  }
}

template <MINICRC_REQUIRES(unsigned_integral, Underlying)>
using lut_type =
    std::array<Underlying, std::numeric_limits<unsigned char>::max() + 1>;

template <std::size_t N, MINICRC_REQUIRES(unsigned_integral, Underlying)>
MINICRC_ARRAY_CONSTEXPR lut_type<Underlying> generate_crc_lut(
    owns_lut, Underlying poly) noexcept {
  using detail::bit_mask;

  lut_type<Underlying> lut{};
  for (std::size_t i = 0; i < lut.size(); ++i) {
    std::size_t value = static_cast<std::size_t>(i);
    for (std::size_t j = 0; j < CHAR_BIT; ++j) {
      if ((value & 1) == 0)
        value >>= 1;
      else
        value = ((value >> 1) ^ poly) & bit_mask<N, Underlying>();
    }
    lut[i] = static_cast<Underlying>(value);
  }
  return lut;
}

template <std::size_t N, MINICRC_REQUIRES(unsigned_integral, Underlying)>
MINICRC_STATICVAR_CONSTEXPR const lut_type<Underlying>& generate_crc_lut(
    borrows_lut, Underlying poly) noexcept {
  static std::map<Underlying, lut_type<Underlying>> map{};
  auto it = map.find(poly);
  if (it != map.end()) {
    return it->second;
  } else {
    return map.emplace(poly, generate_crc_lut<N, Underlying>(owns_lut{}, poly))
        .first->second;
  }
}

template <std::size_t N, MINICRC_REQUIRES(unsigned_integral, Underlying)>
constexpr Underlying generate_crc_lut(fixed_lut, Underlying poly) noexcept {
  return poly;
}

template <typename T, typename Underlying>
struct lookup_table {};
template <typename Underlying>
struct lookup_table<owns_lut, Underlying> {
  using type = typename lut_type<Underlying>;
};
template <typename Underlying>
struct lookup_table<borrows_lut, Underlying> {
  using type = typename lut_type<Underlying> const&;
};
template <typename Underlying>
struct lookup_table<fixed_lut, Underlying> {
  using type = Underlying;
};
}  // namespace detail

template <std::size_t N, typename Underlying = uint_least_t<N>>
struct reverse_poly {
  Underlying value;
};

template <std::size_t N, typename Underlying = uint_least_t<N>>
struct normal_poly {
  Underlying value;
  constexpr operator reverse_poly<N, Underlying>() const noexcept {
    return {detail::bit_reverse<N>(value)};
  }
};

template <std::size_t N, typename Underlying = uint_least_t<N>>
struct all_one {
  constexpr operator Underlying() const noexcept {
    return detail::bit_mask<N, Underlying>();
  }
};

template <std::size_t N, typename Underlying = uint_least_t<N>>
struct all_zero {
  constexpr operator Underlying() const noexcept {
    return static_cast<Underlying>(0);
  }
};

enum class flags : unsigned char {
  no_reflect = 0,
  reflect_input = 1,
  reflect_output = 2,
  reflect = reflect_input | reflect_output
};
constexpr flags operator|(flags a, flags b) {
  return static_cast<flags>(static_cast<unsigned char>(a) |
                            static_cast<unsigned char>(b));
}
constexpr bool operator&(flags a, flags b) {
  return static_cast<bool>(static_cast<unsigned char>(a) &
                           static_cast<unsigned char>(b));
}

namespace impl {
MINICRC_WARNING_PUSH
MINICRC_DISABLE_C4333
template <std::size_t N, typename T,
          MINICRC_REQUIRES(unsigned_integral, Underlying)>
MINICRC_EXTENDED_CONSTEXPR Underlying
crc_software(const flags& stored_flags, const detail::lut_type<Underlying>& lut,
             Underlying state, T* ptr, std::size_t size) noexcept {
  using detail::bit_mask;
  using detail::bit_reverse;
  using unsigned_type = typename std::make_unsigned<remove_cvref_t<T>>::type;
  for (auto data = ptr; data < ptr + size; ++data) {
    const auto ubyte_value = static_cast<unsigned_type>(*data);
    const auto input_byte = (stored_flags & flags::reflect_input)
                                ? ubyte_value
                                : bit_reverse<CHAR_BIT>(ubyte_value);
    const std::size_t index{(static_cast<std::size_t>(state) ^
                             static_cast<std::size_t>(input_byte)) &
                            bit_mask<CHAR_BIT, Underlying>()};
    state = ((state >> CHAR_BIT) ^ lut[index]) & bit_mask<N, Underlying>();
  }
  return state;
}
MINICRC_WARNING_POP

#if MINICRC_ARCH == MINICRC_ARCH_AMD64
#ifndef MINICRC_HW_ENTRYPOINT
#define MINICRC_HW_ENTRYPOINT crc_hardware_amd64
template <std::size_t N, typename T,
          MINICRC_REQUIRES(unsigned_integral, Underlying)>
Underlying crc_hardware_amd64(const flags& stored_flags,
                              const detail::lut_type<Underlying>& lut,
                              Underlying state, T* ptr,
                              std::size_t size) noexcept {
  return crc_software<N, T, Underlying>(stored_flags, lut, state, ptr, size);
}
#endif
#elif MINICRC_ARCH == MINICRC_ARCH_ARM64
#ifndef MINICRC_HW_ENTRYPOINT
#define MINICRC_HW_ENTRYPOINT crc_hardware_arm64
template <std::size_t N, typename T,
          MINICRC_REQUIRES(unsigned_integral, Underlying)>
Underlying crc_hardware_arm64(const flags& stored_flags,
                              const detail::lut_type<Underlying>& lut,
                              Underlying state, T* ptr,
                              std::size_t size) noexcept {
  return crc_software<N, T, Underlying>(stored_flags, lut, state, ptr, size);
}
#endif
#else
#ifndef MINICRC_HW_ENTRYPOINT
#define MINICRC_HW_ENTRYPOINT crc_software
#endif
#endif

template <std::size_t N, typename T,
          MINICRC_REQUIRES(unsigned_integral, Underlying)>
Underlying crc_hardware(const flags& stored_flags,
                        const detail::lut_type<Underlying>& lut,
                        Underlying state, T* ptr, std::size_t size) noexcept {
  // using impl = MINICRC_HW_ENTRYPOINT;
  return MINICRC_HW_ENTRYPOINT<N, T, Underlying>(stored_flags, lut, state, ptr,
                                                 size);
}
}  // namespace impl

template <std::size_t N, typename should_build_lut = borrows_lut,
          typename Underlying = uint_least_t<N>>
class crc {
 public:
  using reverse_poly = minicrc::reverse_poly<N, Underlying>;
  using normal_poly = minicrc::normal_poly<N, Underlying>;
  using all_one = minicrc::all_one<N, Underlying>;
  using all_zero = minicrc::all_zero<N, Underlying>;
  using result_type = Underlying;
  using owning_type = crc<N, owns_lut, Underlying>;
  using borrowing_type = crc<N, borrows_lut, Underlying>;
  using fixed_type = crc<N, fixed_lut, Underlying>;
  using lut_type =
      typename detail::lookup_table<should_build_lut, Underlying>::type;

  MINICRC_ARRAY_CONSTEXPR crc(reverse_poly poly, Underlying initial_state,
                              Underlying final_xor_value,
                              flags f = flags::no_reflect) noexcept
      : state{initial_state},
        xor_value{final_xor_value},
        stored_flags{f},
        lut{detail::generate_crc_lut<N, Underlying>(should_build_lut{},
                                                    poly.value)} {}
  constexpr crc(crc&&) noexcept = default;
  crc& operator=(crc&&) noexcept = default;
  constexpr crc(const owning_type& other) noexcept
      : state{other.state},
        xor_value{other.xor_value},
        stored_flags{other.stored_flags},
        lut{other.lut} {}
  constexpr crc(const borrowing_type& other) noexcept
      : state{other.state},
        xor_value{other.xor_value},
        stored_flags{other.stored_flags},
        lut{other.lut} {}
  constexpr crc(const fixed_type& other) noexcept
      : state{other.state},
        xor_value{other.xor_value},
        stored_flags{other.stored_flags},
        lut{detail::generate_crc_lut<N, Underlying>(should_build_lut{},
                                                    other.lut)} {}

  template <MINICRC_REQUIRES(byte_sized, T)>
  MINICRC_EXTENDED_CONSTEXPR crc& operator()(T* ptr,
                                             std::size_t size) noexcept {
    MINICRC_IF_IS_CONSTEVAL {
      state = impl::crc_software<N, T, Underlying>(stored_flags, lut, state,
                                                   ptr, size);
    }
    else {
      state = impl::crc_hardware<N, T, Underlying>(stored_flags, lut, state,
                                                   ptr, size);
    }
    return *this;
  }
  template <MINICRC_REQUIRES(raw_data_view, T)>
  MINICRC_EXTENDED_CONSTEXPR crc& operator()(T&& t) noexcept {
    return (*this)(t.data(), t.size());
  }
  MINICRC_EXTENDED_CONSTEXPR crc& operator()(const char* str) noexcept {
    return (*this)(string_view{str});
  }

  constexpr result_type checksum() const noexcept {
    using detail::bit_reverse;
    auto output =
        (stored_flags & flags::reflect_output) ? state : bit_reverse<N>(state);
    return output ^ xor_value;
  }
  constexpr operator result_type() const noexcept { return checksum(); }
  constexpr borrowing_type lend() const noexcept { return {*this}; }
  constexpr owning_type clone() const noexcept { return {*this}; }

  template <MINICRC_REQUIRES(byte_sized, T)>
  MINICRC_EXTENDED_CONSTEXPR result_type
  operator()(T* ptr, std::size_t data) const noexcept {
    return lend()(ptr, data);
  }
  template <MINICRC_REQUIRES(raw_data_view, T)>
  MINICRC_EXTENDED_CONSTEXPR result_type operator()(T&& t) const noexcept {
    return lend()(t);
  }
  MINICRC_EXTENDED_CONSTEXPR result_type
  operator()(const char* str) const noexcept {
    return lend()(string_view{str});
  }

 private:
  Underlying state;
  const Underlying xor_value;
  const flags stored_flags;
  lut_type lut;

  friend class crc<N, owns_lut, Underlying>;
  friend class crc<N, borrows_lut, Underlying>;
};

#ifdef MINICRC_LIBRARY
static MINICRC_ARRAY_CONSTEXPR const crc<32>::owning_type crc32{
    crc<32>::normal_poly{0x04C11DB7}, crc<32>::all_one{}, crc<32>::all_one{},
    flags::reflect};
static MINICRC_ARRAY_CONSTEXPR const crc<64>::owning_type crc64_iso{
    crc<64>::normal_poly{0x1B}, crc<64>::all_one{}, crc<64>::all_one{},
    flags::reflect};
#endif  // MINICRC_LIBRARY
}  // namespace minicrc

#endif  // MINICRC_HPP
