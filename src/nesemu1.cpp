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
    template<bool write>
    u8 MemAccess(u16 addr, u8 v = 0);

    u8 RB(u16 addr) {
        return MemAccess<0>(addr);
    }

    u8 WB(u16 addr, u8 v) {
        return MemAccess<1>(addr, v);
    }

    void tick() {

    }

    template<bool write>
    u8 MemAccess(u16 addr, u8 v) {
        tick();
        return 0;
    }


    // CPU registers
    u16 PC = 0xC000;
    u8 A = 0, X = 0, Y = 0, S = 0;

    // status flags
    union {
        u8 raw;
        RegBit<0> C; //carry
        RegBit<1> Z; //zero
        RegBit<2> I; //interrupt enable/disable
        RegBit<3> D; //decimal mode (unsupported on NES, but flag exist)
//        RegBit<4> C; (0x10, 0x20) don't exist
//        RegBit<5> C;
        RegBit<6> V; //overflow
        RegBit<7> N; //negative
    } P;

    void Misfire(u16 old, u16 addr) {

    }

    u8 Pop() {
        return RB(0x100 | u8(++S));
    }

    template<u16 op>
    void Ins() {

    }

    void Op() {
        unsigned op = RB(PC++);


        //define function pointers for each opcode (00..FF) and each interrupt (100,101,102)
#define c(n) Ins<0x##n>, Ins<0x##n+1>,
#define o(n) c(n)c(n+2)c(n+4)c(n+6)
        static void (*const i[0x108])() =
                {
                        o(00) o(08) o(10) o(18) o(20) o(28) o(30) o(38)
                        o(40) o(48) o(50) o(58) o(60) o(68) o(70) o(78)
                        o(80) o(88) o(90) o(98) o(A0) o(A8) o(B0) o(B8)
                        o(C0) o(C8) o(D0) o(D8) o(E0) o(E8) o(F0) o(F8) o(100)
                };
#undef o
#undef c
        i[op]();

    }
}

