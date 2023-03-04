#include <cassert>
#include <cstdlib>

#define MINICRC_LIBRARY
#include "minicrc/minicrc.hpp"

#if defined(__cpp_lib_array_constexpr) && __cpp_lib_array_constexpr >= 201603
    #define POSSIBLY_STATIC_ASSERT(expr) static_assert(expr)
#else
    #define POSSIBLY_STATIC_ASSERT(expr) assert(expr) 
#endif

int main(int, char**) {
    using minicrc::crc;
    using minicrc::flags;
    // To avoid heap allocations, there's crc::owning_type which have the lookup table as a class member
    // and crc::borrowing_type which contain a reference to a lookup table elsewhere.
    // Creating a borrowing_type directly will place the lookup table in a heap allocated global static map.
    // A borrowing_type can be created from another crc object using the lend() method.
    // Directly created borrowing_types cannot be constexpr until C++23 or later.
    MINICRC_ARRAY_CONSTEXPR auto nonowning_type_no_heap = crc<5>::owning_type{crc<5>::normal_poly{0x15}, crc<5>::all_zero{}, crc<5>::all_zero{}, flags::reflect}
                            .lend()("123456789")
                            .checksum();
    POSSIBLY_STATIC_ASSERT(nonowning_type_no_heap == 0x07);
    auto nonowning_type_heap = crc<5>{crc<5>::normal_poly{0x15}, crc<5>::all_zero{}, crc<5>::all_zero{}, flags::reflect}
                            ("123456789")
                            .checksum();
    assert(nonowning_type_heap == 0x07);
    // Using crc algorithm from library for fixed length data
    MINICRC_ARRAY_CONSTEXPR auto nonowning_no_heap_from_library_fixed_data = minicrc::crc32("123456789");
    POSSIBLY_STATIC_ASSERT(nonowning_no_heap_from_library_fixed_data == 0xCBF43926); 
    // Use a library algorithm for streaming data
    crc<32> nonowning_no_heap_from_library_stream_data = minicrc::crc32;
    for (auto str : {"1234", "5678", "9"}) {
        nonowning_no_heap_from_library_stream_data(str);
    }
    assert(nonowning_no_heap_from_library_stream_data == 0xCBF43926);
    return EXIT_SUCCESS;
}
