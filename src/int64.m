%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2016, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This module provides signed 64-bit integers.
%
%---------------------------------------------------------------------------%

:- module int64.
:- interface.

:- type int64.

%---------------------------------------------------------------------------%

    % from_int(A) = B:
    % Convert an int to a signed 64-bit integer.
    %
:- func from_int(int) = int64.

    % A synonym for the function from_int/1.
    %
:- func int64(int) = int64.

%---------------------------------------------------------------------------%
%
% Comparison.
%

:- pred (int64::in) < (int64::in) is semidet.

:- pred (int64::in) > (int64::in) is semidet.

:- pred (int64::in) =< (int64::in) is semidet.

:- pred (int64::in) >= (int64::in) is semidet.

:- func max(int64, int64) = int64.

:- func min(int64, int64) = int64.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

:- func + int64 = int64.

:- func - int64 = int64.

:- func int64 + int64 = int64.

:- func int64 - int64 = int64.

:- func int64 * int64 = int64.

    % Throws a math.domain_error/1 exception for division by zero.
    %
:- func int64 / int64 = int64.

    % Behaviour is undefined for division by zero.
    %
:- func unchecked_quotient(int64, int64) = int64.

:- func int64 rem int64 = int64.

:- func unchecked_rem(int64, int64)  = int64.

%---------------------------------------------------------------------------%
%
% Other operations.
%

:- func abs(int64) = int64.

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

    % A << B:
    % Aborts if B is not in [0, 63].
    %
:- func int64 << int = int64.

    % A >> B:
    % Aborts if B is not in [0, 63].
    %
:- func int64 >> int = int64.

:- func unchecked_left_shift(int64, int) = int64.

:- func unchecked_right_shift(int64, int) = int64.

:- func (int64::in) /\ (int64::in) = (int64::out) is det.

:- func (int64::in) \/ (int64::in) = (int64::out) is det.

:- func xor(int64, int64) = int64.

:- func \ (int64::in) = (int64::out) is det.

%---------------------------------------------------------------------------%
%
% Conversion to strings.
%

    % Synonym for to_decimal_string/1.
    %
:- func to_string(int64::in) = (string::uo) is det.

:- func to_binary_string(int64::in) = (string::uo) is det.

:- func to_binary_string_lz(int64::in) = (string::uo) is det.

:- func to_decimal_string(int64::in) = (string::uo) is det.

:- func to_hex_string(int64::in) = (string::uo) is det.

%---------------------------------------------------------------------------%

    % num_zeros(I) = N:
    % N is the number of zeros in the binary representation of I.
    %
:- func num_zeros(int64) = int.

    % num_ones(I) = N:
    % N is the number of ones in the binary representation of I.
    %
:- func num_ones(int64) = int.

    % num_leading_zeros(I) = N:
    % N is the number of leading zeros in the binary representation of I.
    %
:- func num_leading_zeros(int64) = int.

    % num_trailing_zeros(I) = N:
    % N is the number of trailing zeros in the binary representation of I.
    %
:- func num_trailing_zeros(int64) = int.

:- func reverse_bytes(int64) = int64.

:- func reverse_bits(int64) = int64.

%---------------------------------------------------------------------------%
%
% Constants.
%

:- func min_int8 = int64.
:- func max_int8 = int64.
:- func min_int16 = int64.
:- func max_int16 = int64.
:- func min_int32 = int64.
:- func max_int32 = int64.
:- func min_int64 = int64.
:- func max_int64 = int64.

:- func zero = int64.
:- func one = int64.
:- func two = int64.
:- func eight = int64.
:- func ten = int64.
:- func sixteen = int64.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred int64_equal(int64::in, int64::in) is semidet.

:- pred int64_compare(comparison_result::uo, int64::in, int64::in) is det.

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
    #include <stdlib.h>
    #include <inttypes.h>
    #include <string.h>

    #include ""mercury_string.h""
").

    % XXX can_pass_as_mercury_type would only work 64-bit machines.
    % we should have a way to specify that.
:- pragma foreign_type("C", int64, "int64_t",
    [stable])
    where equality is int64_equal,
          comparison is int64_compare.

%---------------------------------------------------------------------------%
%
% C# implementation.
%

:- pragma foreign_type("C#", int64, "long")
    where equality is int64_equal,
          comparison is int64_compare.

%---------------------------------------------------------------------------%
%
% Java implementation.
%

:- pragma foreign_type("Java", int64, "java.lang.Long")
    where equality is int64_equal,
          comparison is int64_compare.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    int64_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (A == B) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C#",
    int64_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A == B);
").

:- pragma foreign_proc("Java",
    int64_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A.longValue() == B.longValue());
").

int64_compare(Result, A, B) :-
    ( if A < B then
        Result = (<)
    else if A > B then
        Result = (>)
    else
        Result = (=)
    ).

%---------------------------------------------------------------------------%

int64(I) = from_int(I).

:- pragma foreign_proc("C",
    from_int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    B = A;
").

:- pragma foreign_proc("C#",
    from_int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = A;
").

:- pragma foreign_proc("Java",
    from_int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (long) A;
").

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
    SUCCESS_INDICATOR = (A < B);
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
    SUCCESS_INDICATOR = (A > B);
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
    SUCCESS_INDICATOR = (A <= B);
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
    SUCCESS_INDICATOR = (A >= B);
").

%---------------------------------------------------------------------------%

max(X, Y) = ( if X > Y then X else Y ).

min(X, Y) = ( if X < Y then X else Y ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    - (A::in) = (B::out),
     [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
     B = -A;
").

:- pragma foreign_proc("C",
    + (A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
     B = A;
").

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
    ( if int64.is_zero(B)
    then throw(math.domain_error("int64.'/': division by zero"))
    else unchecked_quotient(A, B)
    ).

:- pragma foreign_proc("C",
    unchecked_quotient(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    C = A / B;
").

A rem B =
    ( if int64.is_zero(B)
    then throw(math.domain_error("int64.'rem': second operand is zero"))
    else unchecked_rem(A, B)
    ).

:- pragma foreign_proc("C",
    unchecked_rem(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    C = A % B;
").

:- pragma foreign_proc("C#",
    - (A::in) = (B::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     B = -A;
").

:- pragma foreign_proc("C#",
    + (A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
     B = A;
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

:- pragma foreign_proc("C#",
    unchecked_rem(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = A % B;
").

:- pragma foreign_proc("Java",
    - (A::in) = (B::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     B = -A;
").

:- pragma foreign_proc("Java",
    + (A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
     B = A;
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
    C = A / B;
").

:- pragma foreign_proc("Java",
    unchecked_rem(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = A % B;
").

%---------------------------------------------------------------------------%
%
% Other operations.
%

:- pragma foreign_proc("C",
    abs(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    /* XXX C doesn't provide a wrapper for abs() with int64_t.
    ** TODO: We may need to handle this differently on some systems.
    */
    B = labs(A);
").

% NOTE: the C# backend uses the following Mercury definition because
% System.Math.Abs() will throw an OverflowException for abs(int64.min_int64).

abs(I) = ( if I < int64.zero then int64.zero - I else I ).

:- pragma foreign_proc("Java",
    abs(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Math.abs(A);
").

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

A << B =
    ( if ( B < 0 ; B > 63)
    then func_error("int64.'<<': second operand is out of range")
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
    ( if ( B < 0 ; B > 63 )
    then func_error("int64.'>>': second operand is out of range")
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
    C = A >> B;
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
    to_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[100];
    sprintf(buffer, ""%"" PRId64 """", I);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    to_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = I.ToString();
").

:- pragma foreign_proc("Java",
    to_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Long.toString(I);
").

to_decimal_string(I) =
    to_string(I).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_binary_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"

    uint64_t U = I;

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
            U >>= 1;
        }
        MR_allocate_aligned_string_msg(S, strlen(buffer + i), MR_ALLOC_ID);
        strcpy(S, buffer + i);
    }
").

:- pragma foreign_proc("C#",
    to_binary_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString(I, 2);
").

:- pragma foreign_proc("Java",
    to_binary_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Long.toBinaryString(I);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_binary_string_lz(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = 64;
    uint64_t U = I;

    MR_allocate_aligned_string_msg(S, 64, MR_ALLOC_ID);
    S[64] = '\\0';
    while (i >= 0) {
        i--;
        S[i] = (U & 1) ? '1' : '0';
        U = U >> 1;
    }
").

:- pragma foreign_proc("C#",
    to_binary_string_lz(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString(I, 2).PadLeft(64, '0');
").

:- pragma foreign_proc("Java",
    to_binary_string_lz(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.String.format(""%64s"",
        java.lang.Long.toBinaryString(I)).replace(' ', '0');
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
    num_ones(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint64_t U = I;
    U = U - ((U >> 1) & UINT64_C(0x5555555555555555));
    U = (U & UINT64_C(0x3333333333333333)) + ((U >> 2) & UINT64_C(0x3333333333333333));
    U = (U + (U >> 4)) & UINT64_C(0x0f0f0f0f0f0f0f0f);
    U = U + (U >> 8);
    U = U + (U >> 16);
    U = U + (U >> 32);
    N = U & 0x7f;
").

:- pragma foreign_proc("C#",
    num_ones(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ulong U = (ulong) I;
    U = U - ((U >> 1) & 0x5555555555555555L);
    U = (U & 0x3333333333333333L) + ((U >> 2) & 0x3333333333333333L);
    U = (U + (U >> 4)) & 0x0f0f0f0f0f0f0f0fL;
    U = U + (U >> 8);
    U = U + (U >> 16);
    U = U + (U >> 32);
    N = (int) (U & 0x7f);
").

:- pragma foreign_proc("Java",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Long.bitCount(U);
").

:- pragma foreign_proc("C",
    num_leading_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint64_t U = I;
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
    num_leading_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (I == 0) {
        N = 64;
    } else {
        ulong U = (ulong) I;
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
    num_trailing_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint64_t U = I;
    if (U == 0) {
        N = 64;
    } else {
        uint32_t x, y;
        int n = 63;
        y = (int32_t) U; if (y != 0) { n -= 32; x = y; } else { x = (uint32_t)(U >> 32); }
        y = x << 16; if (y != 0) { n -= 16; x = y; }
        y = x <<  8; if (y != 0) { n -= 8; x = y; }
        y = x <<  4; if (y != 0) { n -= 4; x = y; }
        y = x <<  2; if (y != 0) { n -= 2; x = y; }
        N = n - (int)((x << 1) >> 31);
    }
").

:- pragma foreign_proc("C#",
    num_trailing_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ulong U = (ulong) I;
    if (U == 0) {
        N = 64;
    } else {
        uint x, y;
        int n = 63;
        y = (uint) U; if (y != 0) { n -= 32; x = y; } else { x = (uint)(U >> 32); }
        y = x << 16; if (y != 0) { n -= 16; x = y; }
        y = x <<  8; if (y != 0) { n -= 8; x = y; }
        y = x <<  4; if (y != 0) { n -= 4; x = y; }
        y = x <<  2; if (y != 0) { n -= 2; x = y; }
        N = n - (int)((x << 1) >> 31);
    }
").

:- pragma foreign_proc("Java",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Long.numberOfTrailingZeros(U);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if defined(MR_GNUC) || defined(MR_CLANG)
    B = (int64_t) __builtin_bswap64(A);
#else
    uint64_t u_A = A;

    B = (int64_t) (
        (u_A & UINT64_C(0x00000000000000ff)) << 56 |
        (u_A & UINT64_C(0x000000000000ff00)) << 40 |
        (u_A & UINT64_C(0x0000000000ff0000)) << 24 |
        (u_A & UINT64_C(0x00000000ff000000)) << 8  |
        (u_A & UINT64_C(0x000000ff00000000)) >> 8  |
        (u_A & UINT64_C(0x0000ff0000000000)) >> 24 |
        (u_A & UINT64_C(0x00ff000000000000)) >> 40 |
        (u_A & UINT64_C(0xff00000000000000)) >> 56);
#endif
").

:- pragma foreign_proc("C#",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ulong u_A = (ulong) A;

    B = (long) (
        (u_A & 0x00000000000000ffUL) << 56 |
        (u_A & 0x000000000000ff00UL) << 40 |
        (u_A & 0x0000000000ff0000UL) << 24 |
        (u_A & 0x00000000ff000000UL) << 8  |
        (u_A & 0x000000ff00000000UL) >> 8  |
        (u_A & 0x0000ff0000000000UL) >> 24 |
        (u_A & 0x00ff000000000000UL) >> 40 |
        (u_A & 0xff00000000000000UL) >> 56);
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
    uint64_t u_A = A;
    u_A = (u_A & UINT64_C(0x5555555555555555)) << 1 | (u_A >> 1) & UINT64_C(0x5555555555555555);
    u_A = (u_A & UINT64_C(0x3333333333333333)) << 2 | (u_A >> 2) & UINT64_C(0x3333333333333333);
    u_A = (u_A & UINT64_C(0x0f0f0f0f0f0f0f0f)) << 4 | (u_A >> 4) & UINT64_C(0x0f0f0f0f0f0f0f0f);
    u_A = (u_A & UINT64_C(0x00ff00ff00ff00ff)) << 8 | (u_A >> 8) & UINT64_C(0x00ff00ff00ff00ff);
    u_A = (u_A << 48) | ((u_A & UINT64_C(0xffff0000)) << 16) |
                    ((u_A >> 16) & UINT64_C(0xffff0000)) | (u_A >> 48);
    B = (int64_t) u_A;
").

:- pragma foreign_proc("C#",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ulong u_A = (ulong) A;
    u_A = (u_A & 0x5555555555555555UL) << 1 | (u_A >> 1) & 0x5555555555555555UL;
    u_A = (u_A & 0x3333333333333333UL) << 2 | (u_A >> 2) & 0x3333333333333333UL;
    u_A = (u_A & 0x0f0f0f0f0f0f0f0fUL) << 4 | (u_A >> 4) & 0x0f0f0f0f0f0f0f0fUL;
    u_A = (u_A & 0x00ff00ff00ff00ffUL) << 8 | (u_A >> 8) & 0x00ff00ff00ff00ffUL;
    u_A = (u_A << 48) | ((u_A & 0xffff0000UL) << 16) |
                    ((u_A >> 16) & 0xffff0000UL) | (u_A >> 48);
    B = (long) u_A;
").

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Long.reverse(A);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    min_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = INT8_MIN;
").

:- pragma foreign_proc("C",
    max_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = INT8_MAX;
").

:- pragma foreign_proc("C#",
    min_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = sbyte.MinValue;
").

:- pragma foreign_proc("C#",
    max_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = sbyte.MaxValue;
").

:- pragma foreign_proc("Java",
    min_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (long) java.lang.Byte.MIN_VALUE;
").

:- pragma foreign_proc("Java",
    max_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (long) java.lang.Byte.MAX_VALUE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    min_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = INT16_MIN;
").

:- pragma foreign_proc("C",
    max_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = INT16_MAX;
").

:- pragma foreign_proc("C#",
    min_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = short.MinValue;
").

:- pragma foreign_proc("C#",
    max_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = short.MaxValue;
").

:- pragma foreign_proc("Java",
    min_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (long) java.lang.Short.MIN_VALUE;
").

:- pragma foreign_proc("Java",
    max_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (long) java.lang.Short.MAX_VALUE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    min_int32 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = INT32_MIN;
").

:- pragma foreign_proc("C",
    max_int32 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = INT32_MAX;
").

:- pragma foreign_proc("C#",
    min_int32 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = int.MinValue;
").

:- pragma foreign_proc("C#",
    max_int32 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = int.MaxValue;
").

:- pragma foreign_proc("Java",
    min_int32 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (long) java.lang.Integer.MIN_VALUE;
").

:- pragma foreign_proc("Java",
    max_int32 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (long) java.lang.Integer.MAX_VALUE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    min_int64 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = INT64_MIN;
").

:- pragma foreign_proc("C",
    max_int64 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = INT64_MAX;
").

:- pragma foreign_proc("C#",
    min_int64 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = long.MinValue;
").

:- pragma foreign_proc("C#",
    max_int64 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = long.MaxValue;
").

:- pragma foreign_proc("Java",
    min_int64 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = java.lang.Long.MIN_VALUE;
").

:- pragma foreign_proc("Java",
    max_int64 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = java.lang.Long.MAX_VALUE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    zero = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = 0;
").

:- pragma foreign_proc("C#",
    zero = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 0;
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
    U = 1;
").

:- pragma foreign_proc("C#",
    one = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 1;
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
    U = 2;
").

:- pragma foreign_proc("C#",
    two = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 2;
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
    U = 8;
").

:- pragma foreign_proc("C#",
    eight = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 8;
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
    U = 10;
").

:- pragma foreign_proc("C#",
    ten = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 10;
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
    U = 16;
").

:- pragma foreign_proc("C#",
    sixteen = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 16;
").

:- pragma foreign_proc("Java",
    sixteen = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = 16L;
").


%---------------------------------------------------------------------------%

:- pred is_zero(int64::in) is semidet.

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
:- end_module int64.
%---------------------------------------------------------------------------%
