%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2016, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This module provides signed 32-bit integers.
%
%---------------------------------------------------------------------------%

:- module int32.
:- interface.

:- type int32.

%---------------------------------------------------------------------------%
%
% Conversion.
%

    % from_int(A, B):
    % Convert an int to a signed 32-bit integer.
    % Fails if A is not in [int32.min_int, int32.max_int].
    %
:- pred from_int(int::in, int32::out) is semidet.

    % As above, but throw an software_error/1 exception instead of failing
    %
:- func det_from_int(int) = int32.

    % A synonym for the function det_from_int/1.
    %
:- func int32(int) = int32.

:- func to_int(int32) = int.

%---------------------------------------------------------------------------%
%
% Comparison.
%

:- pred (int32::in) < (int32::in) is semidet.

:- pred (int32::in) > (int32::in) is semidet.

:- pred (int32::in) =< (int32::in) is semidet.

:- pred (int32::in) >= (int32::in) is semidet.

:- func max(int32, int32) = int32.

:- func min(int32, int32) = int32.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

:- func + int32 = int32.

:- func - int32 = int32.

:- func int32 + int32 = int32.

:- func int32 - int32 = int32.

:- func int32 * int32 = int32.

    % Throws a math.domain_error/1 exception for division by zero.
    %
:- func int32 / int32 = int32.

    % Behaviour is undefined for division by zero.
    %
:- func unchecked_quotient(int32, int32) = int32.

:- func int32 rem int32 = int32.

:- func unchecked_rem(int32, int32) = int32.

%---------------------------------------------------------------------------%
%
% Other operations.
%

:- func abs(int32) = int32.

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

    % A << B:
    % Aborts if B is not in [0, 31].
    %
:- func int32 << int = int32.

    % A >> B:
    % Aborts if B is not in [0, 31].
    %
:- func int32 >> int = int32.

:- func unchecked_left_shift(int32, int) = int32.

:- func unchecked_right_shift(int32, int) = int32.

:- func (int32::in) /\ (int32::in) = (int32::out) is det.

:- func (int32::in) \/ (int32::in) = (int32::out) is det.

:- func xor(int32, int32) = int32.

:- func \ (int32::in) = (int32::out) is det.

%---------------------------------------------------------------------------%
%
% Conversion to strings.
%

    % Synonym for to_decimal_string/1.
    %
:- func to_string(int32::in) = (string::uo) is det.

:- func to_binary_string(int32::in) = (string::uo) is det.

:- func to_binary_string_lz(int32::in) = (string::uo) is det.

:- func to_decimal_string(int32::in) = (string::uo) is det.

:- func to_hex_string(int32::in) = (string::uo) is det.

%---------------------------------------------------------------------------%

    % num_zeros(I) = N:
    % N is the number of zeros in the binary representation of I.
    %
:- func num_zeros(int32) = int.

    % num_ones(I) = N:
    % N is the number of ones in the binary representation of I.
    %
:- func num_ones(int32) = int.

    % num_leading_zeros(I) = N:
    % N is the number of leading zeros in the binary representation of I.
    %
:- func num_leading_zeros(int32) = int.

    % num_trailing_zeros(I) = N:
    % N is the number of trailing zeros in the binary representation of I.
    %
:- func num_trailing_zeros(int32) = int.

    % reverse_bytes(A) = B:
    % B is the value that results from reversing the bytes in the
    % representation of A.
    %
:- func reverse_bytes(int32) = int32.

    % reverse_bits(A) = B:
    % B is the is value that results from reversing the bits in the
    % representation of A.
    %
:- func reverse_bits(int32) = int32.

%---------------------------------------------------------------------------%
%
% Constants.
%

:- func min_int8 = int32.
:- func max_int8 = int32.
:- func min_int16 = int32.
:- func max_int16 = int32.
:- func min_int32 = int32.
:- func max_int32 = int32.

:- func zero = int32.
:- func one = int32.
:- func two = int32.
:- func eight = int32.
:- func ten = int32.
:- func sixteen = int32.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred int32_equal(int32::in, int32::in) is semidet.

:- pred int32_compare(comparison_result::uo, int32::in, int32::in) is det.

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

:- pragma foreign_type("C", int32, "int32_t",
    [can_pass_as_mercury_type, stable])
    where equality is int32_equal,
          comparison is int32_compare.

%---------------------------------------------------------------------------%
%
% C# implementation.
%

:- pragma foreign_type("C#", int32, "int")
    where equality is int32_equal,
          comparison is int32_compare.

%---------------------------------------------------------------------------%
%
% Java implementation.
%

:- pragma foreign_type("Java", int32, "java.lang.Integer")
    where equality is int32_equal,
          comparison is int32_compare.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    int32_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (A == B) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C#",
    int32_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A == B);
").

:- pragma foreign_proc("Java",
    int32_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A.intValue() == B.intValue());
").

int32_compare(Result, A, B) :-
    ( if A < B then
        Result = (<)
    else if A > B then
        Result = (>)
    else
        Result = (=)
    ).

%---------------------------------------------------------------------------%

int32(I) = det_from_int(I).

:- pragma foreign_proc("C",
    from_int(A::in, B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (A > (MR_Integer) INT32_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if (A < (MR_Integer) INT32_MIN) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        B = (int32_t) A;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(A::in, B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = A; // Mercury's 'int' type in the C# grade is 32-bits.
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("Java",
    from_int(A::in, B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = A; // Mercury's 'int' type in the Java grade is 32-bits.
    SUCCESS_INDICATOR = true;
").

det_from_int(I) = U :-
    ( if from_int(I, U0)
    then U = U0
    else error("int32.det_from_int: cannot convert int to int32")
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    B = A;
").

:- pragma foreign_proc("C#",
    to_int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = A;
").

:- pragma foreign_proc("Java",
    to_int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = A;
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
%
% Arithmetic operations.
%

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
    ( if int32.is_zero(B)
    then throw(math.domain_error("int32.'/': division by zero"))
    else unchecked_quotient(A, B)
    ).

:- pragma foreign_proc("C",
    unchecked_quotient(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    C = A / B;
").

A rem B =
    ( if int32.is_zero(B)
    then throw(math.domain_error("int32.'rem': second operand is zero"))
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
    /* XXX C doesn't provide a wrapper for abs() with int32_t.
    ** On pretty much all system we are interested in it will be equivalent
    ** to int.
    */
    B = abs(A);
").

% NOTE: the C# backend uses the following Mercury definition because
% System.Math.Abs() will throw an OverflowException for abs(int32.min_int32).

abs(I) = ( if I < int32.zero then int32.zero - I else I ).

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
    ( if ( B < 0 ; B > 31)
    then func_error("int32.'<<': second operand is out of range")
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
    ( if ( B < 0 ; B > 31)
    then func_error("int32.'>>': second operand is out of range")
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
    sprintf(buffer, ""%"" PRId32 """", I);
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
    S = java.lang.Integer.toString(I);
").

to_decimal_string(U) =
    to_string(U).

%---------------------------------------------------------------------------%


:- pragma foreign_proc("C",
    to_binary_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint32_t U = I;

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
    to_binary_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString(I, 2);
").

:- pragma foreign_proc("Java",
    to_binary_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toBinaryString(I);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_binary_string_lz(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = 32;
    uint32_t U = I;

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
    to_hex_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString(I, 16);
").

:- pragma foreign_proc("Java",
    to_hex_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toHexString(I);
").

%---------------------------------------------------------------------------%

% The algorithms in this section are from chapter 5 of ``Hacker's Delight''
% by Henry S. Warren, Jr.
% (Java uses the same.)

num_zeros(U) = 32 - num_ones(U).

:- pragma foreign_proc("C",
    num_ones(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint32_t U = I;
    U = U - ((U >> 1) & UINT32_C(0x55555555));
    U = (U & UINT32_C(0x33333333)) + ((U >> 2) & UINT32_C(0x33333333));
    U = (U + (U >> 4)) & UINT32_C(0x0f0f0f0f);
    U = U + (U >> 8);
    U = U + (U >> 16);
    N = U & UINT32_C(0x3f);
").

:- pragma foreign_proc("C#",
    num_ones(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    uint U = (uint) I;
    U = U - ((U >> 1) & 0x55555555U);
    U = (U & 0x33333333U) + ((U >> 2) & 0x33333333U);
    U = (U + (U >> 4)) & 0x0f0f0f0fU;
    U = U + (U >> 8);
    U = U + (U >> 16);
    N = (int) (U & 0x3fU);
").

:- pragma foreign_proc("Java",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.bitCount(U);
").

:- pragma foreign_proc("C",
    num_leading_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint32_t U = I;
    if (U == 0) {
        N = 32;
    } else {
        int32_t n = 1;
        if ((U >> 16) == 0) { n += 16; U <<= 16; }
        if ((U >> 24) == 0) { n += 8;  U <<= 8;  }
        if ((U >> 28) == 0) { n += 4;  U <<= 4;  }
        if ((U >> 30) == 0) { n += 2;  U <<= 2;  }
        N = n - (U >> 31);
    }
").

:- pragma foreign_proc("C#",
    num_leading_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    uint U = (uint) I;
    if (U == 0) {
        N = 32;
    } else {
        int n = 1;
        if ((U >> 16) == 0) { n += 16; U <<= 16; }
        if ((U >> 24) == 0) { n += 8;  U <<= 8;  }
        if ((U >> 28) == 0) { n += 4;  U <<= 4;  }
        if ((U >> 30) == 0) { n += 2;  U <<= 2;  }
        N = n - (int)(U >> 31);
    }
").

:- pragma foreign_proc("Java",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.numberOfLeadingZeros(U);
").

:- pragma foreign_proc("C",
    num_trailing_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint32_t U = I;
    if (U == 0) {
        N = 32;
    } else {
        int32_t     n = 31;
        uint32_t    y;
        y = U << 16; if (y != 0) { n = n -16; U = y; }
        y = U <<  8; if (y != 0) { n = n - 8; U = y; }
        y = U <<  4; if (y != 0) { n = n - 4; U = y; }
        y = U <<  2; if (y != 0) { n = n - 2; U = y; }
        y = U <<  1; if (y != 0) { n = n - 1; }
        N = n;
    }
").

:- pragma foreign_proc("C#",
    num_trailing_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    uint U = (uint) I;
    if (U == 0) {
        N = 32;
    } else {
        int     n = 31;
        uint    y;
        y = U << 16; if (y != 0) { n -= 16; U = y; }
        y = U <<  8; if (y != 0) { n -= 8;  U = y; }
        y = U <<  4; if (y != 0) { n -= 4;  U = y; }
        y = U <<  2; if (y != 0) { n -= 2;  U = y; }
        y = U <<  1; if (y != 0) { n -= 1; }
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
    B = (int32_t) __builtin_bswap32(A);
#else
    uint32_t u_A = A;
    B = (u_A & UINT32_C(0x000000ff)) << 24 |
        (u_A & UINT32_C(0x0000ff00)) << 8  |
        (u_A & UINT32_C(0x00ff0000)) >> 8  |
        (u_A & UINT32_C(0xff000000)) >> 24;
#endif
").

:- pragma foreign_proc("C#",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    uint u_A = (uint) A;

    B = (int) ((u_A & 0x000000ffU) << 24 | (u_A & 0x0000ff00U) << 8 |
         (u_A & 0x00ff0000U) >> 8  | (u_A & 0xff000000U) >> 24);
").

:- pragma foreign_proc("Java",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Integer.reverseBytes(A);
").

:- pragma foreign_proc("C",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint32_t u_A = A;

    u_A = (u_A & UINT32_C(0x55555555)) << 1 | (u_A >> 1) & UINT32_C(0x55555555);
    u_A = (u_A & UINT32_C(0x33333333)) << 2 | (u_A >> 2) & UINT32_C(0x33333333);
    u_A = (u_A & UINT32_C(0x0f0f0f0f)) << 4 | (u_A >> 4) & UINT32_C(0x0f0f0f0f);
    u_A = (u_A << 24) | ((u_A & UINT32_C(0xff00)) << 8) |
                    ((u_A >> 8) & UINT32_C(0xff00)) | (u_A >> 24);
    B = (int32_t) u_A;
").

:- pragma foreign_proc("C#",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    uint u_A = (uint) A;

    u_A = (u_A & 0x55555555) << 1 | (u_A >> 1) & 0x55555555;
    u_A = (u_A & 0x33333333) << 2 | (u_A >> 2) & 0x33333333;
    u_A = (u_A & 0x0f0f0f0f) << 4 | (u_A >> 4) & 0x0f0f0f0f;
    u_A = (u_A << 24) | ((u_A & 0xff00) << 8) | ((u_A >> 8) & 0xff00) | (u_A >> 24);

    B = (int) u_A;
").

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Integer.reverse(A);
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
    I = (int) java.lang.Byte.MIN_VALUE;
").

:- pragma foreign_proc("Java",
    max_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (int) java.lang.Byte.MAX_VALUE;
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
    I = (int) java.lang.Short.MIN_VALUE;
").

:- pragma foreign_proc("Java",
    max_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = (int) java.lang.Short.MAX_VALUE;
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
    I = java.lang.Integer.MIN_VALUE;
").

:- pragma foreign_proc("Java",
    max_int32 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = java.lang.Integer.MAX_VALUE;
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
    U = 0;
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
    U = 1;
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
    U = 2;
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
    U = 8;
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
    U = 10;
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
    U = 16;
").

%---------------------------------------------------------------------------%

:- pred is_zero(int32::in) is semidet.

:- pragma foreign_proc("C",
    is_zero(U::in),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (U == 0) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C#",
    is_zero(U::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = (U == 0);
").

:- pragma foreign_proc("Java",
    is_zero(U::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = (U == 0);
").

%---------------------------------------------------------------------------%
:- end_module int32.
%---------------------------------------------------------------------------%
