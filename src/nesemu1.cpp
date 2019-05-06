#include "nesemu1.h"
#include <stdint.h>
#include <signal.h>
#include <assert.h>
#include <cmath>

#include <SDL2/SDL.h>
#include <vector>

typedef uint_least32_t u32;
typedef uint_least16_t u16;
typedef uint_least8_t u8;
typedef int_least8_t s8;

// Bitfield
template<unsigned bitno, unsigned nbits = 1, typename T=u8>
struct RegBit {
    T data;
    enum {
        mask = (1u << nbits) - 1u
    };

    template<typename T2>
    RegBit &operator=(T2 val) {
        data = (data & ~(mask << bitno)) | ((nbits > 1 ? val & mask : !!val) << bitno);
        return *this;
    }

    explicit operator unsigned() const {
        return (data >> bitno) & mask;
    }

    RegBit &operator++() {
        return *this = *this + 1;
    }

    unsigned operator++(int) {
        unsigned r = *this;
        ++*this;
        return r;
    }
};


namespace CPU {
    // CPU registers
    u16 PC = 0xC000;
    u8 A = 0, X = 0, Y = 0, S = 0;

    // status flags
    union {
        u8 raw;
        RegBit<0> C;
    } P;

}

