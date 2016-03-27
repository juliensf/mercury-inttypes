%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2016, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This module provides signed 8-bit integers.
%
%---------------------------------------------------------------------------%

:- module int8.
:- interface.

:- type int8.

%---------------------------------------------------------------------------%
%
% Conversion.
%

    % from_int(A, B):
    % Fails if A is not in [0, int8.max_int8].
    %
:- pred from_int(int::in, int8::out) is semidet.

    % As above, but throws a software_error/1 exception instead of failing.
    %
:- func det_from_int(int) = int8.

    % A synonym for the function det_from_int/1.
    %
:- func int8(int) = int8.

:- func to_int(int8) = int.

%---------------------------------------------------------------------------%
%
% Comparison.
%

:- pred (int8::in) < (int8::in) is semidet.

:- pred (int8::in) > (int8::in) is semidet.

:- pred (int8::in) =< (int8::in) is semidet.

:- pred (int8::in) >= (int8::in) is semidet.

:- func max(int8, int8) = int8.

:- func min(int8, int8) = int8.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

:- func + int8 = int8.

:- func - int8 = int8.

:- func int8 + int8 = int8.

:- func int8 - int8 = int8.

:- func int8 * int8 = int8.

    % Throws a math.domain_error/1 exception for division by zero.
    %
:- func int8 / int8 = int8.

    % Behaviour is undefined for division by zero.
    %
:- func unchecked_quotient(int8, int8) = int8.

:- func int8 rem int8 = int8.

:- func unchecked_rem(int8, int8) = int8.

%---------------------------------------------------------------------------%
%
% Other operations.
%

:- func abs(int8) = int8.

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

:- func int8 << int = int8.

:- func int8 >> int = int8.

:- func unchecked_left_shift(int8, int) = int8.

:- func unchecked_right_shift(int8, int) = int8.

:- func (int8::in) /\ (int8::in) = (int8::out) is det.

:- func (int8::in) \/ (int8::in) = (int8::out) is det.

:- func xor(int8, int8) = int8.

:- func \ (int8::in) = (int8::out) is det.

%---------------------------------------------------------------------------%
%
% Conversion to strings.
%

    % Synonym for to_decimal_string/1.
    %
:- func to_string(int8::in) = (string::uo) is det.

:- func to_binary_string(int8::in) = (string::uo) is det.

:- func to_binary_string_lz(int8::in) = (string::uo) is det.

:- func to_decimal_string(int8::in) = (string::uo) is det.

:- func to_hex_string(int8::in) = (string::uo) is det.

%---------------------------------------------------------------------------%

    % num_zeros(U) = N:
    % N is the number of zeros in the binary representation of U.
    %
:- func num_zeros(int8) = int.

    % num_ones(U) = N:
    % N is the number of ones in the binary representation of U.
    %
:- func num_ones(int8) = int.

    % num_leading_zeros(U) = N:
    % N is the number of leading zeros in the binary representation of U.
    %
:- func num_leading_zeros(int8) = int.

    % num_trailing_zeros(U) = N:
    % N is the number of trailing zeros in the binary representation of U.
    %
:- func num_trailing_zeros(int8) = int.

    % reverse_bits(A) = B:
    % B is the is value that results from reversing the bits in the
    % representation of A.
    %
:- func reverse_bits(int8) = int8.

%---------------------------------------------------------------------------%
%
% Constants.
%

:- func min_int8 = int8.
:- func max_int8 = int8.

:- func zero = int8.
:- func one = int8.
:- func two = int8.
:- func eight = int8.
:- func ten = int8.
:- func sixteen = int8.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred int8_equal(int8::in, int8::in) is semidet.

:- pred int8_compare(comparison_result::uo, int8::in, int8::in) is det.

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

:- pragma foreign_type("C", int8, "int8_t",
    [can_pass_as_mercury_type, stable])
    where equality is int8_equal,
          comparison is int8_compare.

%---------------------------------------------------------------------------%
%
% C# implementation.
%

:- pragma foreign_type("C#", int8, "sbyte")
    where equality is int8_equal,
          comparison is int8_compare.

%---------------------------------------------------------------------------%
%
% Java implementation.
%

:- pragma foreign_type("Java", int8, "java.lang.Byte")
    where equality is int8_equal,
          comparison is int8_compare.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    int8_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (A == B) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C#",
    int8_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A == B);
").

:- pragma foreign_proc("Java",
    int8_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A.intValue() == B.intValue());
").

int8_compare(Result, A, B) :-
    ( if A < B then
        Result = (<)
    else if A > B then
        Result = (>)
    else
        Result = (=)
    ).

%---------------------------------------------------------------------------%

int8(I) = det_from_int(I).

:- pragma foreign_proc("C",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (I < INT8_MIN) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if (I > (MR_Integer) INT8_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        U = (int8_t) I;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (sbyte) I;
    if (I < sbyte.MinValue) {
        SUCCESS_INDICATOR = false;
    } else if (I > sbyte.MaxValue) {
        SUCCESS_INDICATOR = false;
    } else {
        SUCCESS_INDICATOR = true;
    }
").

:- pragma foreign_proc("Java",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (byte) I;
    if (I < java.lang.Byte.MIN_VALUE) {
        SUCCESS_INDICATOR = false;
    } else if (I > java.lang.Byte.MAX_VALUE) {
        SUCCESS_INDICATOR = false;
    } else {
        SUCCESS_INDICATOR = true;
    }
").

det_from_int(I) = U :-
    ( if from_int(I, U0)
    then U = U0
    else error("int8.det_from_int: cannot convert int to int8")
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
    ( if int8.is_zero(B)
    then throw(math.domain_error("int8.'/': division by zero"))
    else unchecked_quotient(A, B)
    ).

:- pragma foreign_proc("C",
    unchecked_quotient(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    C = A / B;
").

A rem B =
    ( if int8.is_zero(B)
    then throw(math.domain_error("int8.'rem': second operand is zero"))
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
     B = (sbyte) -A;
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
     C = (sbyte) (A + B);
").

:- pragma foreign_proc("C#",
     (A::in) - (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = (sbyte) (A - B);
").

:- pragma foreign_proc("C#",
     (A::in) * (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = (sbyte) (A * B);
").

:- pragma foreign_proc("C#",
    unchecked_quotient(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = (sbyte) (A / B);
").

:- pragma foreign_proc("C#",
    unchecked_rem(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = (sbyte) (A % B);
").

:- pragma foreign_proc("Java",
     (A::in) + (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = (byte) (A + B);
").

:- pragma foreign_proc("Java",
    - (A::in) = (B::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     B = (byte) -A;
").

:- pragma foreign_proc("Java",
    + (A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
     B = A;
").

:- pragma foreign_proc("Java",
     (A::in) - (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = (byte) (A - B);
").

:- pragma foreign_proc("Java",
     (A::in) * (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = (byte) (A * B);
").

:- pragma foreign_proc("Java",
    unchecked_quotient(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    C = (byte) (A  / B);
").

:- pragma foreign_proc("Java",
    unchecked_rem(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = (byte) (A % B);
").

%---------------------------------------------------------------------------%
%
% Other operations.
%

:- pragma foreign_proc("C",
    abs(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    /* XXX C doesn't provide a wrapper for abs() with int8_t. */
    B = (int8_t) abs(A);
").

% NOTE: the C# backend uses the following Mercury definition because
% System.Math.Abs() will throw an OverflowException for abs(int8.min_int8).

abs(I) = ( if I < int8.zero then int8.zero - I else I ).

:- pragma foreign_proc("Java",
    abs(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (byte) java.lang.Math.abs(A);
").

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

A << B =
    ( if (B < 0 ; B > 7)
    then func_error("int8.'<<': second operand is out of range")
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
    C = (sbyte) (A << B);
").

:- pragma foreign_proc("Java",
    unchecked_left_shift(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = (byte) (A << B);
").

A >> B =
    ( if (B < 0 ; B > 7)
    then func_error("int8.'>>': second operand is out of range")
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
    C = (sbyte) (A >> B);
").

:- pragma foreign_proc("Java",
    unchecked_right_shift(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = (byte) ((A & 0xffff) >> B);
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
    C = (sbyte) (A & B);
").

:- pragma foreign_proc("Java",
    (A::in) /\ (B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = (byte) (A & B);
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
    C = (sbyte) ((byte)A | (byte)B);
").

:- pragma foreign_proc("Java",
    (A::in) \/ (B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = (byte) (A | B);
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
    C = (sbyte) (A ^ B);
").

:- pragma foreign_proc("Java",
    xor(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = (byte) (A ^ B);
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
    B = (sbyte) (~A);
").

:- pragma foreign_proc("Java",
    \ (A::in) = (B::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    B = (byte) (~A);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[100];
    sprintf(buffer, ""%"" PRId8 """", U);
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
    S = java.lang.Integer.toString(U);
").

to_decimal_string(U) =
    to_string(U).

%---------------------------------------------------------------------------%


:- pragma foreign_proc("C",
    to_binary_string(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint8_t U = I;

    if (U == 0) {
        MR_allocate_aligned_string_msg(S, 2, MR_ALLOC_ID);
        S[0] = '0';
        S[1] = '\\0';
    } else {

        char buffer[9];
        int i = 8;

        buffer[8] = '\\0';

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
    S = System.Convert.ToString((byte)U, 2);
").

:- pragma foreign_proc("Java",
    to_binary_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toBinaryString(U & 0xff);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_binary_string_lz(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = 8;
    uint8_t U = I;

    MR_allocate_aligned_string_msg(S, 8, MR_ALLOC_ID);
    S[8] = '\\0';
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
    S = System.Convert.ToString((byte)U, 2).PadLeft(8, '0');
").

:- pragma foreign_proc("Java",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.String.format(""%8s"",
        java.lang.Integer.toBinaryString(U & 0xff)).replace(' ', '0');
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_hex_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[100];
    sprintf(buffer, ""%"" PRIx8 """", U);
    MR_allocate_aligned_string_msg(S, strlen(buffer), MR_ALLOC_ID);
    strcpy(S, buffer);
").

:- pragma foreign_proc("C#",
    to_hex_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = System.Convert.ToString((byte)U, 16);
").

:- pragma foreign_proc("Java",
    to_hex_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toHexString(U & 0xff);
").

%---------------------------------------------------------------------------%

num_zeros(U) = 8 - num_ones(U).

:- pragma foreign_decl("C", "const uint8_t MITS_int8_num_ones_table[];").

:- pragma foreign_code("C", "

const uint8_t MITS_int8_num_ones_table[256] = {
    0,1,1,2,1,2,2,3,
    1,2,2,3,2,3,3,4,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    4,5,5,6,5,6,6,7,
    5,6,6,7,6,7,7,8
};
").

:- pragma foreign_proc("C",
    num_ones(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    N = MITS_int8_num_ones_table[(uint8_t)I];
").

:- pragma foreign_code("C#", "

public static byte[] num_ones_table = {
    0,1,1,2,1,2,2,3,
    1,2,2,3,2,3,3,4,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,
    2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,
    3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,
    4,5,5,6,5,6,6,7,
    4,5,5,6,5,6,6,7,
    5,6,6,7,6,7,7,8
};

").

:- pragma foreign_proc("C#",
    num_ones(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = mercury.int8.num_ones_table[unchecked((byte)I)];
").

:- pragma foreign_proc("Java",
    num_ones(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.bitCount(I << 24);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    // U.
    N = 0; // XXX NYI.
").

:- pragma foreign_proc("C#",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (U == 0) {
        N = 16;
    } else {
        N = 0; // XXX NYI.
    }
").

:- pragma foreign_proc("Java",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // U.
    N = 0; // XXX NYI.
").

:- pragma foreign_proc("C",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    // U.
    N = 0; // XXX NYI.
").

:- pragma foreign_proc("C#",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (U == 0) {
        N = 8;
    } else {
        N = 0; // XXX NYI.
    }
").

:- pragma foreign_proc("Java",
    num_trailing_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // U.
    N = 0; // XXX NYI.
").

:- pragma foreign_proc("C",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    // B.
    B = A; // XXX NYI.
").

:- pragma foreign_proc("C#",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    // A.
    B = 0;  // XXX NYI.
").

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // A.
    B = 0; // XXX NYI.
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    min_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = INT8_MIN;
").

:- pragma foreign_proc("C#",
    min_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = sbyte.MinValue;
").

:- pragma foreign_proc("Java",
    min_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = java.lang.Byte.MIN_VALUE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    max_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = INT8_MAX;
").

:- pragma foreign_proc("C#",
    max_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = sbyte.MaxValue;
").

:- pragma foreign_proc("Java",
    max_int8 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = java.lang.Byte.MAX_VALUE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    zero = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = INT8_C(0);
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
    U = INT16_C(1);
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
    U = INT8_C(2);
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
    U = INT8_C(8);
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
    U = INT8_C(10);
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
    U = INT8_C(16);
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

:- pred is_zero(int8::in) is semidet.

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
:- end_module int8.
%---------------------------------------------------------------------------%
