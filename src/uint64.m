%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2016, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This module provides unsigned 64-bit integers.
%
%---------------------------------------------------------------------------%

:- module uint64.
:- interface.

:- type uint64.

%---------------------------------------------------------------------------%

    % from_int(A, B):
    % Fails if I < 0.
    %
:- pred from_int(int::in, uint64::out) is semidet.

    % As above, but throws a software_error/1 exception instead of failing.
    %
:- func det_from_int(int) = uint64.

    % A synonym for the function det_from_int/1.
    %
:- func uint64(int) = uint64.

%---------------------------------------------------------------------------%
%
% Comparison.
%

:- pred (uint64::in) < (uint64::in) is semidet.

:- pred (uint64::in) > (uint64::in) is semidet.

:- pred (uint64::in) =< (uint64::in) is semidet.

:- pred (uint64::in) >= (uint64::in) is semidet.

:- func max(uint64, uint64) = uint64.

:- func min(uint64, uint64) = uint64.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

:- func uint64 + uint64 = uint64.

:- func uint64 - uint64 = uint64.

:- func uint64 * uint64 = uint64.

    % Throws a math.domain_error/1 exception for division by zero.
    %
:- func uint64 / uint64 = uint64.

    % Behaviour is undefined for division by zero.
    %
:- func unchecked_quotient(uint64, uint64) = uint64.

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

:- func uint64 << int = uint64.

:- func uint64 >> int = uint64.

:- func unchecked_left_shift(uint64, int) = uint64.

:- func unchecked_right_shift(uint64, int) = uint64.

:- func (uint64::in) /\ (uint64::in) = (uint64::out) is det.

:- func (uint64::in) \/ (uint64::in) = (uint64::out) is det.

:- func xor(uint64, uint64) = uint64.

:- func \ (uint64::in) = (uint64::out) is det.

%---------------------------------------------------------------------------%
%
% Conversion to strings.
%

    % Synonym for to_decimal_string/1.
    %
:- func to_string(uint64::in) = (string::uo) is det.

:- func to_binary_string(uint64::in) = (string::uo) is det.

:- func to_decimal_string(uint64::in) = (string::uo) is det.

:- func to_hex_string(uint64::in) = (string::uo) is det.

%---------------------------------------------------------------------------%

    % num_zeros(U) = N:
    % N is the number of zeros in the binary representation of U.
    %
:- func num_zeros(uint64) = int.

    % num_ones(U) = N:
    % N is the number of ones in the binary representation of U.
    %
:- func num_ones(uint64) = int.

    % num_leading_zeros(U) = N:
    % N is the number of leading zeros in the binary representation of U.
    %
:- func num_leading_zeros(uint64) = int.

    % num_trailing_zeros(U) = N:
    % N is the number of trailing zeros in the binary representation of U.
    %
:- func num_trailing_zeros(uint64) = int.

:- func reverse_bytes(uint64) = uint64.

:- func reverse_bits(uint64) = uint64.

%---------------------------------------------------------------------------%
%
% Constants.
%

:- func max_uint8 = uint64.     % 0xff.
:- func max_uint16 = uint64.    % 0xffff.
:- func max_uint32 = uint64.    % 0xffffffff.
:- func max_uint64 = uint64.    % 0xffffffffffffffff.

:- func zero = uint64.
:- func one = uint64.
:- func two = uint64.
:- func eight = uint64.
:- func ten = uint64.
:- func sixteen = uint64.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred uint64_equal(uint64::in, uint64::in) is semidet.

:- pred uint64_compare(comparison_result::uo, uint64::in, uint64::in) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module math.
:- import_module require.

%---------------------------------------------------------------------------%
%
% C implementation.
%

% NOTE: we assume that the version of C here is C99 or later.

:- pragma foreign_decl("C", "

    #include <stdint.h>
    #include <inttypes.h>
    #include <string.h>

    #include ""mercury_string.h""
").

    % XXX can_pass_as_mercury_type would only work 64-bit machines.
    % we should have a way to specify that.
:- pragma foreign_type("C", uint64, "uint64_t",
    [stable])
    where equality is uint64_equal,
          comparison is uint64_compare.

%---------------------------------------------------------------------------%
%
% C# implementation.
%

:- pragma foreign_type("C#", uint64, "ulong")
    where equality is uint64_equal,
          comparison is uint64_compare.

%---------------------------------------------------------------------------%
%
% Java implementation.
%

:- pragma foreign_type("Java", uint64, "java.lang.Long")
    where equality is uint64_equal,
          comparison is uint64_compare.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    uint64_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (A == B) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C#",
    uint64_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A == B);
").

:- pragma foreign_proc("Java",
    uint64_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A.longValue() == B.longValue());
").

uint64_compare(Result, A, B) :-
    ( if A < B then
        Result = (<)
    else if A > B then
        Result = (>)
    else
        Result = (=)
    ).

%---------------------------------------------------------------------------%

uint64(I) = det_from_int(I).

:- pragma foreign_proc("C",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (I < 0) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        U = (uint64_t) I;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (ulong) I;
    SUCCESS_INDICATOR = (I < 0) ? false : true;
").

:- pragma foreign_proc("Java",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (long) I;
    SUCCESS_INDICATOR = (I < 0) ? false : true;
").

det_from_int(I) = U :-
    ( if from_int(I, U0)
    then U = U0
    else error("uint64.det_from_int: cannot convert int to uint64")
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    (A::in) < (B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (A < B);
").

:- pragma foreign_proc("C#",
    (A::in) < (B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A < B);
").

:- pragma foreign_proc("Java",
    (A::in) < (B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A < B) ^ ((A < 0) != (B < 0));
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    (A::in) > (B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (A > B);
").

:- pragma foreign_proc("C#",
    (A::in) > (B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A > B);
").

:- pragma foreign_proc("Java",
    (A::in) > (B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A > B) ^ ((A < 0) != (B < 0));
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    (A::in) =< (B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (A <= B);
").

:- pragma foreign_proc("C#",
    (A::in) =< (B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A <= B);
").

:- pragma foreign_proc("Java",
    (A::in) =< (B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A <= B) ^ ((A < 0) != (B < 0));
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    (A::in) >= (B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (A >= B);
").

:- pragma foreign_proc("C#",
    (A::in) >= (B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A >= B);
").

:- pragma foreign_proc("Java",
    (A::in) >= (B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A >= B) ^ ((A < 0) != (B < 0));
").

%---------------------------------------------------------------------------%

max(X, Y) = ( if X > Y then X else Y ).

min(X, Y) = ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
     (A::in) + (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
     C = A + B;
").

:- pragma foreign_proc("C",
     (A::in) - (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
     C = A - B;
").

:- pragma foreign_proc("C",
     (A::in) * (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
     C = A * B;
").

A / B =
    ( if uint64.is_zero(B)
    then throw(math.domain_error("uint64.'/': division by zero"))
    else unchecked_quotient(A, B)
    ).

:- pragma foreign_proc("C",
    unchecked_quotient(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    C = A / B;
").

:- pragma foreign_proc("C#",
     (A::in) + (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = A + B;
").

:- pragma foreign_proc("C#",
     (A::in) - (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = A - B;
").

:- pragma foreign_proc("C#",
     (A::in) * (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = A * B;
").

:- pragma foreign_proc("C#",
    unchecked_quotient(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = A / B;
").

:- pragma foreign_proc("Java",
     (A::in) + (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = A + B;
").

:- pragma foreign_proc("Java",
     (A::in) - (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = A - B;
").

:- pragma foreign_proc("Java",
     (A::in) * (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = A * B;
").

:- pragma foreign_proc("Java",
    unchecked_quotient(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    // XXX Java 8 only.
    C = java.lang.Long.divideUnsigned(A, B);
").

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

A << B =
    ( if B < 0
    then func_error("uint64.'<<': second argument is negative")
    else unchecked_left_shift(A, B)
    ).

:- pragma foreign_proc("C",
    unchecked_left_shift(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    C = A << B;
").

:- pragma foreign_proc("C#",
    unchecked_left_shift(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = A << B;
").

:- pragma foreign_proc("Java",
    unchecked_left_shift(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = A << B;
").

A >> B =
    ( if B < 0
    then func_error("uint64.'>>': second argument is negative")
    else unchecked_right_shift(A, B)
    ).

:- pragma foreign_proc("C",
    unchecked_right_shift(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    C = A >> B;
").

:- pragma foreign_proc("C#",
    unchecked_right_shift(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = A >> B;
").

:- pragma foreign_proc("Java",
    unchecked_right_shift(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = A >>> B;
").

:- pragma foreign_proc("C",
    (A::in) /\ (B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    C = A & B;
").

:- pragma foreign_proc("C#",
    (A::in) /\ (B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = A & B;
").

:- pragma foreign_proc("Java",
    (A::in) /\ (B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = A & B;
").

:- pragma foreign_proc("C",
    (A::in) \/ (B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    C = A | B;
").

:- pragma foreign_proc("C#",
    (A::in) \/ (B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = A | B;
").

:- pragma foreign_proc("Java",
    (A::in) \/ (B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = A | B;
").

:- pragma foreign_proc("C",
    xor(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    C = A ^ B;
").

:- pragma foreign_proc("C#",
    xor(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = A ^ B;
").

:- pragma foreign_proc("Java",
    xor(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = A ^ B;
").

:- pragma foreign_proc("C",
    \ (A::in) = (B::out),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    B = ~A;
").

:- pragma foreign_proc("C#",
    \ (A::in) = (B::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    B = ~A;
").

:- pragma foreign_proc("Java",
    \ (A::in) = (B::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    B = ~A;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[100];
    sprintf(buffer, ""%"" PRIu64 """", U);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    to_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U.ToString();
").

:- pragma foreign_proc("Java",
    to_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX Java 8 only :-(
    S = java.lang.Long.toUnsignedString(U);
").

to_decimal_string(U) =
    to_string(U).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_binary_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (U == 0) {
        MR_allocate_aligned_string_msg(S, 2, MR_ALLOC_ID);
        S[0] = '0';
        S[1] = '\\0';
    } else {

        char buffer[65];
        int i = 64;

        buffer[64] = '\\0';

        while (U) {
            i--;
            buffer[i] = (U & 1) ? '1' : '0';
            U = U >> 1;
        }
        MR_allocate_aligned_string_msg(S, strlen(buffer + i), MR_ALLOC_ID);
        strcpy(S, buffer + i);
    }
").

:- pragma foreign_proc("C#",
    to_binary_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // NOTE: C# does not provide a format provider for ulong here, so we
    // need to cast U to a long.  The binary representation will be the
    // same in either case.
    S = System.Convert.ToString((long)U, 2);
").

:- pragma foreign_proc("Java",
    to_binary_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Long.toBinaryString(U);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_hex_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[100];
    sprintf(buffer, ""%"" PRIx64 """", U);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    to_hex_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.String.Format(""{0:x}"", U);
").

:- pragma foreign_proc("Java",
    to_hex_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Long.toHexString(U);
").

%---------------------------------------------------------------------------%

% The algorithms in this section are from chapter 5 of ``Hacker's Delight''
% by Henry S. Warren, Jr.
% (Java uses the same.)

num_zeros(U) = 64 - num_ones(U).

:- pragma foreign_proc("C",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = U - ((U >> 1) & UINT64_C(0x5555555555555555));
    U = (U & UINT64_C(0x3333333333333333)) + ((U >> 2) & UINT64_C(0x3333333333333333));
    U = (U + (U >> 4)) & UINT64_C(0x0f0f0f0f0f0f0f0f);
    U = U + (U >> 8);
    U = U + (U >> 16);
    U = U + (U >> 32);
    N = U & UINT64_C(0x7f);
").

:- pragma foreign_proc("C#",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = U - ((U >> 1) & 0x5555555555555555UL);
    U = (U & 0x3333333333333333UL) + ((U >> 2) & 0x3333333333333333UL);
    U = (U + (U >> 4)) & 0x0f0f0f0f0f0f0f0fUL;
    U = U + (U >> 8);
    U = U + (U >> 16);
    U = U + (U >> 32);
    N = (int) (U & 0x7fUL);
").

:- pragma foreign_proc("Java",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Long.bitCount(U);
").

:- pragma foreign_proc("C",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (U == 0) {
        N = 64;
    } else {
        int32_t n = 1;
        uint32_t x = (uint32_t)(U >> 32);
        if (x == 0) { n += 32; x = (uint32_t)U; }
        if (x >> 16 == 0) { n += 16; x <<= 16; }
        if (x >> 24 == 0) { n +=  8; x <<=  8; }
        if (x >> 28 == 0) { n +=  4; x <<=  4; }
        if (x >> 30 == 0) { n +=  2; x <<=  2; }
        N = n - (x >> 31);
    }
").

:- pragma foreign_proc("C#",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (U == 0) {
        N = 64;
    } else {
        int n = 1;
        uint x = (uint)(U >> 32);
        if (x == 0) { n += 32; x = (uint)U; }
        if (x >> 16 == 0) { n += 16; x <<= 16; }
        if (x >> 24 == 0) { n +=  8; x <<=  8; }
        if (x >> 28 == 0) { n +=  4; x <<=  4; }
        if (x >> 30 == 0) { n +=  2; x <<=  2; }
        N = n - (int)(x >> 31);
    }
").

:- pragma foreign_proc("Java",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Long.numberOfLeadingZeros(U);
").

:- pragma foreign_proc("C",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (U == 0) {
        N = 64;
    } else {
        uint32_t x, y;
        int n = 63;
        y = (int32_t) U;
        if (y != 0) {
            n -=  32; x = y;
        } else {
            x = (uint32_t)(U >> 32);
        }
        y = x << 16; if (y != 0) { n -= 16; x = y; }
        y = x <<  8; if (y != 0) { n -=  8; x = y; }
        y = x <<  4; if (y != 0) { n -=  4; x = y; }
        y = x <<  2; if (y != 0) { n -=  2; x = y; }
        N = n - (int)((x << 1) >> 31);
    }
").

:- pragma foreign_proc("C#",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (U == 0) {
        N = 64;
    } else {

        uint x, y;
        int n = 63;
        y = (uint) U; if (y != 0) { n = n - 32; x = y; } else { x = (uint)(U >> 32); }
        y = x << 16; if (y != 0) { n = n -16; x = y; }
        y = x <<  8; if (y != 0) { n = n - 8; x = y; }
        y = x <<  4; if (y != 0) { n = n - 4; x = y; }
        y = x <<  2; if (y != 0) { n = n - 2; x = y; }
        N = n - (int)((x << 1) >> 31);
    }
").

:- pragma foreign_proc("Java",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Long.numberOfTrailingZeros(U);
").

:- pragma foreign_proc("C",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    B = (A & UINT64_C(0x00000000000000ff)) << 56 |
        (A & UINT64_C(0x000000000000ff00)) << 40 |
        (A & UINT64_C(0x0000000000ff0000)) << 24 |
        (A & UINT64_C(0x00000000ff000000)) << 8  |
        (A & UINT64_C(0x000000ff00000000)) >> 8  |
        (A & UINT64_C(0x0000ff0000000000)) >> 24 |
        (A & UINT64_C(0x00ff000000000000)) >> 40 |
        (A & UINT64_C(0xff00000000000000)) >> 56;
").

:- pragma foreign_proc("C#",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (A & 0x00000000000000ffUL) << 56 |
        (A & 0x000000000000ff00UL) << 40 |
        (A & 0x0000000000ff0000UL) << 24 |
        (A & 0x00000000ff000000UL) << 8  |
        (A & 0x000000ff00000000UL) >> 8  |
        (A & 0x0000ff0000000000UL) >> 24 |
        (A & 0x00ff000000000000UL) >> 40 |
        (A & 0xff00000000000000UL) >> 56;
").

:- pragma foreign_proc("Java",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Long.reverseBytes(A);
").

:- pragma foreign_proc("C",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    A = (A & UINT64_C(0x5555555555555555)) << 1 | (A >> 1) & UINT64_C(0x5555555555555555);
    A = (A & UINT64_C(0x3333333333333333)) << 2 | (A >> 2) & UINT64_C(0x3333333333333333);
    A = (A & UINT64_C(0x0f0f0f0f0f0f0f0f)) << 4 | (A >> 4) & UINT64_C(0x0f0f0f0f0f0f0f0f);
    A = (A & UINT64_C(0x00ff00ff00ff00ff)) << 8 | (A >> 8) & UINT64_C(0x00ff00ff00ff00ff);
    A = (A << 48) | ((A & UINT64_C(0xffff0000)) << 16) |
                    ((A >> 16) & UINT64_C(0xffff0000)) | (A >> 48);
    B = A;
").

:- pragma foreign_proc("C#",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    A = (A & 0x5555555555555555UL) << 1 | (A >> 1) & 0x5555555555555555UL;
    A = (A & 0x3333333333333333UL) << 2 | (A >> 2) & 0x3333333333333333UL;
    A = (A & 0x0f0f0f0f0f0f0f0fUL) << 4 | (A >> 4) & 0x0f0f0f0f0f0f0f0fUL;
    A = (A & 0x00ff00ff00ff00ffUL) << 8 | (A >> 8) & 0x00ff00ff00ff00ffUL;
    A = (A << 48) | ((A & 0xffff0000UL) << 16) |
                    ((A >> 16) & 0xffff0000UL) | (A >> 48);
    B = A;
").

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Long.reverse(A);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    max_uint8 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT8_MAX;
").

:- pragma foreign_proc("C#",
    max_uint8 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = byte.MaxValue;
").

:- pragma foreign_proc("Java",
    max_uint8 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 0xffL;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    max_uint16 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT16_MAX;
").

:- pragma foreign_proc("C#",
    max_uint16 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = ushort.MaxValue;
").

:- pragma foreign_proc("Java",
    max_uint16 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 0xffffL;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    max_uint32 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT32_MAX;
").

:- pragma foreign_proc("C#",
    max_uint32 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = uint.MaxValue;
").

:- pragma foreign_proc("Java",
    max_uint32 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 0xffffffffL;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    max_uint64 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT64_MAX;
").

:- pragma foreign_proc("C#",
    max_uint64 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = ulong.MaxValue;
").

:- pragma foreign_proc("Java",
    max_uint64 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 0xffffffffffffffffL;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    zero = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT64_C(0);
").

:- pragma foreign_proc("C#",
    zero = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 0U;
").

:- pragma foreign_proc("Java",
    zero = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 0L;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    one = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT64_C(1);
").

:- pragma foreign_proc("C#",
    one = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 1U;
").

:- pragma foreign_proc("Java",
    one = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 1L;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    two = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT64_C(2);
").

:- pragma foreign_proc("C#",
    two = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 2U;
").

:- pragma foreign_proc("Java",
    two = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 2L;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    eight = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT64_C(8);
").

:- pragma foreign_proc("C#",
    eight = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 8U;
").

:- pragma foreign_proc("Java",
    eight = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 8L;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    ten = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT64_C(10);
").

:- pragma foreign_proc("C#",
    ten = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 10U;
").

:- pragma foreign_proc("Java",
    ten = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 10L;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    sixteen = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT64_C(16);
").

:- pragma foreign_proc("C#",
    sixteen = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 16U;
").

:- pragma foreign_proc("Java",
    sixteen = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 16L;
").

%---------------------------------------------------------------------------%

:- pred is_zero(uint64::in) is semidet.

:- pragma foreign_proc("C",
    is_zero(U::in),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (U == 0U) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C#",
    is_zero(U::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = (U == 0U);
").

:- pragma foreign_proc("Java",
    is_zero(U::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = (U == 0L);
").

%---------------------------------------------------------------------------%
:- end_module uint64.
%---------------------------------------------------------------------------%
