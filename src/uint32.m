%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2016, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This module provides unsigned 32-bit integers.
%
%---------------------------------------------------------------------------%

:- module uint32.
:- interface.

:- type uint32.

%---------------------------------------------------------------------------%

    % from_int(A, B):
    % Fails if A is not in [0, int32.max_int32].
    %
:- pred from_int(int::in, uint32::out) is semidet.

    % As above, but throws a software_error/1 exception instead of failing.
    %
:- func det_from_int(int) = uint32.

    % A synonym for the function det_from_int/1.
    %
:- func uint32(int) = uint32.

    % semidet_to_int(A, B):
    % Fails if A > int.max_int.
    %
:- pred semidet_to_int(uint32::in, int::out) is semidet.

:- func to_int(uint32) = int.

%---------------------------------------------------------------------------%
%
% Comparison.
%

:- pred (uint32::in) < (uint32::in) is semidet.

:- pred (uint32::in) > (uint32::in) is semidet.

:- pred (uint32::in) =< (uint32::in) is semidet.

:- pred (uint32::in) >= (uint32::in) is semidet.

:- func max(uint32, uint32) = uint32.

:- func min(uint32, uint32) = uint32.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

:- func uint32 + uint32 = uint32.

:- func uint32 - uint32 = uint32.

:- func uint32 * uint32 = uint32.

    % Throws a math.domain_error/1 exception for division by zero.
    %
:- func uint32 / uint32 = uint32.

    % Behaviour is undefined for division by zero.
    %
:- func unchecked_quotient(uint32, uint32) = uint32.

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

    % A << B:
    % Aborts if B is not in [0, 31].
    %
:- func uint32 << int = uint32.

    % A >> B:
    % Aborts if B is not in [0, 31].
    %
:- func uint32 >> int = uint32.

:- func unchecked_left_shift(uint32, int) = uint32.

:- func unchecked_right_shift(uint32, int) = uint32.

:- func (uint32::in) /\ (uint32::in) = (uint32::out) is det.

:- func (uint32::in) \/ (uint32::in) = (uint32::out) is det.

:- func xor(uint32, uint32) = uint32.

:- func \ (uint32::in) = (uint32::out) is det.

%---------------------------------------------------------------------------%
%
% Conversion to strings.
%

    % Synonym for to_decimal_string/1.
    %
:- func to_string(uint32::in) = (string::uo) is det.

:- func to_binary_string(uint32::in) = (string::uo) is det.

:- func to_binary_string_lz(uint32::in) = (string::uo) is det.

:- func to_decimal_string(uint32::in) = (string::uo) is det.

:- func to_hex_string(uint32::in) = (string::uo) is det.

%---------------------------------------------------------------------------%

    % num_zeros(U) = N:
    % N is the number of zeros in the binary representation of U.
    %
:- func num_zeros(uint32) = int.

    % num_ones(U) = N:
    % N is the number of ones in the binary representation of U.
    %
:- func num_ones(uint32) = int.

    % num_leading_zeros(U) = N:
    % N is the number of leading zeros in the binary representation of U.
    % Note that num_leading_zeros(uint32.zero) = 32.
    %
:- func num_leading_zeros(uint32) = int.

    % num_trailing_zeros(U) = N:
    % N is the number of trailing zeros in the binary representation of U.
    % Note that num_trailing_zeros(uint32.zero) = 32.
    %
:- func num_trailing_zeros(uint32) = int.

    % reverse_bytes(A) = B:
    % B is the value that results from reversing the bytes in the
    % representation of A.
    %
:- func reverse_bytes(uint32) = uint32.

    % reverse_bits(A) = B:
    % B is the is value that results from reversing the bits in the
    % representation of A.
    %
:- func reverse_bits(uint32) = uint32.

%---------------------------------------------------------------------------%
%
% Constants.
%

:- func max_uint8 = uint32.
:- func max_uint16 = uint32.
:- func max_int32 = uint32.
:- func max_uint32 = uint32.

:- func zero = uint32.
:- func one = uint32.
:- func two = uint32.
:- func eight = uint32.
:- func ten = uint32.
:- func sixteen = uint32.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred uint32_equal(uint32::in, uint32::in) is semidet.

:- pred uint32_compare(comparison_result::uo, uint32::in, uint32::in) is det.

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

:- pragma foreign_type("C", uint32, "uint32_t",
    [can_pass_as_mercury_type, stable])
    where equality is uint32_equal,
          comparison is uint32_compare.

%---------------------------------------------------------------------------%
%
% C# implementation.
%

:- pragma foreign_type("C#", uint32, "uint")
    where equality is uint32_equal,
          comparison is uint32_compare.

%---------------------------------------------------------------------------%
%
% Java implementation.
%

:- pragma foreign_type("Java", uint32, "int")
    where equality is uint32_equal,
          comparison is uint32_compare.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    uint32_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (A == B) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C#",
    uint32_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A == B);
").

:- pragma foreign_proc("Java",
    uint32_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A == B);
").

uint32_compare(Result, A, B) :-
    ( if A < B then
        Result = (<)
    else if A > B then
        Result = (>)
    else
        Result = (=)
    ).

%---------------------------------------------------------------------------%

uint32(I) = det_from_int(I).

:- pragma foreign_proc("C",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (I < 0) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if (I > (MR_Integer) INT32_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        U = (uint32_t) I;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (uint) I;
    SUCCESS_INDICATOR = (I < 0) ? false : true;
").

:- pragma foreign_proc("Java",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = I;
    SUCCESS_INDICATOR = (I < 0) ? false : true;
").

det_from_int(I) = U :-
    ( if from_int(I, U0)
    then U = U0
    else error("uint32.det_from_int: cannot convert int to uint32")
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    semidet_to_int(A::in, B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (sizeof(MR_Integer) == sizeof(uint64_t)) {
        B = A;
        SUCCESS_INDICATOR = MR_TRUE;
    } else if  (A > (uint32_t)INT32_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        B = A;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    semidet_to_int(A::in, B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        B = checked((int)A);
        SUCCESS_INDICATOR = true;
    } catch (System.OverflowException) {
        B = 0;  // Dummy value.
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("Java",
    semidet_to_int(A::in, B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if ((A & 0xffffffffL) > (long)java.lang.Integer.MAX_VALUE) {
        B = 0;  // Dummy value;
        SUCCESS_INDICATOR = false;
    } else {
        B = A;
        SUCCESS_INDICATOR = true;
    }
").

to_int(A) = B :-
    ( if semidet_to_int(A, B0)
    then B = B0
    else error("uint32.to_int: cannot convert uint32 to int")
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
    SUCCESS_INDICATOR = (A & 0xffffffffL) < (B & 0xffffffffL);
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
    SUCCESS_INDICATOR = (A & 0xffffffffL) > (B & 0xffffffffL);
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
    SUCCESS_INDICATOR = (A & 0xffffffffL) <= (B & 0xffffffffL);
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
    SUCCESS_INDICATOR = (A & 0xffffffffL) >= (B & 0xffffffffL);
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
    ( if uint32.is_zero(B)
    then throw(math.domain_error("uint32.'/': division by zero"))
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
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = (int) ((A & 0xffffffffL) / (B & 0xffffffffL));
").

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

A << B =
    ( if (B < 0 ; B > 31)
    then func_error("uint32.'<<': second operand is out of range")
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
    ( if (B < 0 ; B > 31)
    then func_error("uint32.'>>': second operand is out of range")
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
    sprintf(buffer, ""%"" PRIu32 """", U);
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
    S = java.lang.Long.toString(U & 0xffffffffL);
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

        char buffer[33];
        int i = 32;

        buffer[32] = '\\0';

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
    S = System.Convert.ToString(U, 2);
").

:- pragma foreign_proc("Java",
    to_binary_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toBinaryString(U);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = 32;

    MR_allocate_aligned_string_msg(S, 32, MR_ALLOC_ID);
    S[32] = '\\0';
    while (i >= 0) {
        i--;
        S[i] = (U & 1) ? '1' : '0';
        U = U >> 1;
    }
").

:- pragma foreign_proc("C#",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString(U, 2).PadLeft(32, '0');
").

:- pragma foreign_proc("Java",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.String.format(""%32s"",
        java.lang.Integer.toBinaryString(U)).replace(' ', '0');
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_hex_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[100];
    sprintf(buffer, ""%"" PRIx32 """", U);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    to_hex_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString(U, 16);
").

:- pragma foreign_proc("Java",
    to_hex_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toHexString(U);
").

%---------------------------------------------------------------------------%

% The algorithms in this section are from chapter 5 of ``Hacker's Delight''
% by Henry S. Warren, Jr.
% (Java uses the same.)

% Much of the following code assumes that:
%
%   sizeof(uint32_t) == sizeof(unsigned int)
%
% which is true for all the system that are of interest to us.

num_zeros(U) = 32 - num_ones(U).

:- pragma foreign_proc("C",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
/*
** For MSVC we should use __popcnt().
*/
#if (defined(MR_GNUC) || defined(MR_CLANG))
    N = __builtin_popcount(U);
#else
    U = U - ((U >> 1) & UINT32_C(0x55555555));
    U = (U & UINT32_C(0x33333333)) + ((U >> 2) & UINT32_C(0x33333333));
    U = (U + (U >> 4)) & UINT32_C(0x0f0f0f0f);
    U = U + (U >> 8);
    U = U + (U >> 16);
    N = U & UINT32_C(0x3f);
#endif
").

:- pragma foreign_proc("C#",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = U - ((U >> 1) & 0x55555555);
    U = (U & 0x33333333) + ((U >> 2) & 0x33333333);
    U = (U + (U >> 4)) & 0x0f0f0f0f;
    U = U + (U >> 8);
    U = U + (U >> 16);
    N = (int) (U & 0x3f);
").

:- pragma foreign_proc("Java",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.bitCount(U);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (U == 0) {
        N = 32;
    } else {
    /*
    ** XXX for MSVC we should use __lzcnt().
    ** (Note that __lzcnt(0) = 32.)
    */
    #if defined(MR_GNUC) || defined(MR_CLANG)
        /* Note that __builtin_clz(0) is undefined. */
        N = __builtin_clz(U);
    #else
        int32_t n = 1;
        if ((U >> 16) == 0) { n += 16; U <<= 16; }
        if ((U >> 24) == 0) { n += 8;  U <<= 8;  }
        if ((U >> 28) == 0) { n += 4;  U <<= 4;  }
        if ((U >> 30) == 0) { n += 2;  U <<= 2;  }
        N = n - (U >> 31);
    #endif
    }
").

:- pragma foreign_proc("C#",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (U == 0) {
        N = 32;
    } else {
        int n = 1;
        if ((U >> 16) == 0) { n = n + 16; U = U << 16; }
        if ((U >> 24) == 0) { n = n + 8;  U = U << 8;  }
        if ((U >> 28) == 0) { n = n + 4;  U = U << 4;  }
        if ((U >> 30) == 0) { n = n + 2;  U = U << 2;  }
        N = n - (int)(U >> 31);
    }
").

:- pragma foreign_proc("Java",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.numberOfLeadingZeros(U);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (U == 0) {
        N = 32;
    } else {
    #if defined(MR_GNUC) || defined(MR_CLANG)
        N = __builtin_ctz(U);
    #else
        int32_t     n = 31;
        uint32_t    y;
        y = U << 16; if (y != 0) { n -= 16; U = y; }
        y = U <<  8; if (y != 0) { n -= 8;  U = y; }
        y = U <<  4; if (y != 0) { n -= 4;  U = y; }
        y = U <<  2; if (y != 0) { n -= 2;  U = y; }
        y = U <<  1; if (y != 0) { n -= 1; }
        N = n;
    #endif
    }
").

:- pragma foreign_proc("C#",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (U == 0) {
        N = 32;
    } else {
        int     n = 31;
        uint    y;
        y = U << 16; if (y != 0) { n = n -16; U = y; }
        y = U <<  8; if (y != 0) { n = n - 8; U = y; }
        y = U <<  4; if (y != 0) { n = n - 4; U = y; }
        y = U <<  2; if (y != 0) { n = n - 2; U = y; }
        y = U <<  1; if (y != 0) { n = n - 1; }
        N = n;
    }
").

:- pragma foreign_proc("Java",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.numberOfTrailingZeros(U);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if defined(MR_GNUC) || defined(MR_CLANG)
    B = __builtin_bswap32(A);
#else
    B = (A & UINT32_C(0x000000ff)) << 24 |
        (A & UINT32_C(0x0000ff00)) << 8  |
        (A & UINT32_C(0x00ff0000)) >> 8  |
        (A & UINT32_C(0xff000000)) >> 24;
#endif
").

:- pragma foreign_proc("C#",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B =  (A & 0x000000ffU) << 24 | (A & 0x0000ff00U) << 8 |
         (A & 0x00ff0000U) >> 8  | (A & 0xff000000U) >> 24;
").

:- pragma foreign_proc("Java",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Integer.reverseBytes(A);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    A = (A & UINT32_C(0x55555555)) << 1 | (A >> 1) & UINT32_C(0x55555555);
    A = (A & UINT32_C(0x33333333)) << 2 | (A >> 2) & UINT32_C(0x33333333);
    A = (A & UINT32_C(0x0f0f0f0f)) << 4 | (A >> 4) & UINT32_C(0x0f0f0f0f);
    A = (A << 24) | ((A & UINT32_C(0xff00)) << 8) |
                    ((A >> 8) & UINT32_C(0xff00)) | (A >> 24);
    B = A;
").

:- pragma foreign_proc("C#",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    A = (A & 0x55555555) << 1 | (A >> 1) & 0x55555555;
    A = (A & 0x33333333) << 2 | (A >> 2) & 0x33333333;
    A = (A & 0x0f0f0f0f) << 4 | (A >> 4) & 0x0f0f0f0f;
    A = (A << 24) | ((A & 0xff00) << 8) | ((A >> 8) & 0xff00) | (A >> 24);
    B = A;
").

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Integer.reverse(A);
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
    U = 0xff;
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
    U = 0xffff;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    max_int32 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = INT32_MAX;
").

:- pragma foreign_proc("C#",
    max_int32 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = int.MaxValue;
").

:- pragma foreign_proc("Java",
    max_int32 = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = java.lang.Integer.MAX_VALUE;
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
    U = 0xffffffff;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    zero = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT32_C(0);
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
    U = 0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    one = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT32_C(1);
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
    U = 1;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    two = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT32_C(2);
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
    U = 2;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    eight = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT32_C(8);
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
    U = 8;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    ten = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT32_C(10);
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
    U = 10;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    sixteen = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT32_C(16);
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
    U = 16;
").

%---------------------------------------------------------------------------%

:- pred is_zero(uint32::in) is semidet.

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
    SUCCESS_INDICATOR = (U == 0);
").

%---------------------------------------------------------------------------%
:- end_module uint32.
%---------------------------------------------------------------------------%
