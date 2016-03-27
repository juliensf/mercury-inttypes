%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2016, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This module provides signed 16-bit integers.
%
%---------------------------------------------------------------------------%

:- module int16.
:- interface.

:- type int16.

%---------------------------------------------------------------------------%
%
% Conversion.
%

    % from_int(A, B):
    % Fails if A is not in [0, int16.max_int16].
    %
:- pred from_int(int::in, int16::out) is semidet.

    % As above, but throws a software_error/1 exception instead of failing.
    %
:- func det_from_int(int) = int16.

    % A synonym for the function det_from_int/1.
    %
:- func int16(int) = int16.

:- func to_int(int16) = int.

%---------------------------------------------------------------------------%
%
% Comparison.
%

:- pred (int16::in) < (int16::in) is semidet.

:- pred (int16::in) > (int16::in) is semidet.

:- pred (int16::in) =< (int16::in) is semidet.

:- pred (int16::in) >= (int16::in) is semidet.

:- func max(int16, int16) = int16.

:- func min(int16, int16) = int16.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

:- func + int16 = int16.

:- func - int16 = int16.

:- func int16 + int16 = int16.

:- func int16 - int16 = int16.

:- func int16 * int16 = int16.

    % Throws a math.domain_error/1 exception for division by zero.
    %
:- func int16 / int16 = int16.

    % Behaviour is undefined for division by zero.
    %
:- func unchecked_quotient(int16, int16) = int16.

:- func int16 rem int16 = int16.

:- func unchecked_rem(int16, int16) = int16.

%---------------------------------------------------------------------------%
%
% Other operations.
%

:- func abs(int16) = int16.

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

    % A << B:
    % Aborts if B is not in [0, 15].
    %
:- func int16 << int = int16.

    % A >> B:
    % Aborts if B is not in [0, 15].
    %
:- func int16 >> int = int16.

:- func unchecked_left_shift(int16, int) = int16.

:- func unchecked_right_shift(int16, int) = int16.

:- func (int16::in) /\ (int16::in) = (int16::out) is det.

:- func (int16::in) \/ (int16::in) = (int16::out) is det.

:- func xor(int16, int16) = int16.

:- func \ (int16::in) = (int16::out) is det.

%---------------------------------------------------------------------------%
%
% Conversion to strings.
%

    % Synonym for to_decimal_string/1.
    %
:- func to_string(int16::in) = (string::uo) is det.

:- func to_binary_string(int16::in) = (string::uo) is det.

:- func to_binary_string_lz(int16::in) = (string::uo) is det.

:- func to_decimal_string(int16::in) = (string::uo) is det.

:- func to_hex_string(int16::in) = (string::uo) is det.

%---------------------------------------------------------------------------%

    % num_zeros(U) = N:
    % N is the number of zeros in the binary representation of U.
    %
:- func num_zeros(int16) = int.

    % num_ones(U) = N:
    % N is the number of ones in the binary representation of U.
    %
:- func num_ones(int16) = int.

    % num_leading_zeros(U) = N:
    % N is the number of leading zeros in the binary representation of U.
    %
:- func num_leading_zeros(int16) = int.

    % num_trailing_zeros(U) = N:
    % N is the number of trailing zeros in the binary representation of U.
    %
:- func num_trailing_zeros(int16) = int.

    % reverse_bytes(A) = B:
    % B is the value that results from reversing the bytes in the
    % representation of A.
    %
:- func reverse_bytes(int16) = int16.

    % reverse_bits(A) = B:
    % B is the is value that results from reversing the bits in the
    % representation of A.
    %
:- func reverse_bits(int16) = int16.

%---------------------------------------------------------------------------%
%
% Constants.
%

:- func min_int8 = int16.
:- func max_int8 = int16.
:- func min_int16 = int16.
:- func max_int16 = int16.

:- func zero = int16.
:- func one = int16.
:- func two = int16.
:- func eight = int16.
:- func ten = int16.
:- func sixteen = int16.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred int16_equal(int16::in, int16::in) is semidet.

:- pred int16_compare(comparison_result::uo, int16::in, int16::in) is det.

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

:- pragma foreign_type("C", int16, "int16_t",
    [can_pass_as_mercury_type, stable])
    where equality is int16_equal,
          comparison is int16_compare.

%---------------------------------------------------------------------------%
%
% C# implementation.
%

:- pragma foreign_type("C#", int16, "short")
    where equality is int16_equal,
          comparison is int16_compare.

%---------------------------------------------------------------------------%
%
% Java implementation.
%

:- pragma foreign_type("Java", int16, "java.lang.Short")
    where equality is int16_equal,
          comparison is int16_compare.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    int16_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (A == B) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C#",
    int16_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A == B);
").

:- pragma foreign_proc("Java",
    int16_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A.intValue() == B.intValue());
").

int16_compare(Result, A, B) :-
    ( if A < B then
        Result = (<)
    else if A > B then
        Result = (>)
    else
        Result = (=)
    ).

%---------------------------------------------------------------------------%

int16(I) = det_from_int(I).

:- pragma foreign_proc("C",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (I < INT16_MIN) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if (I > (MR_Integer) INT16_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        U = (int16_t) I;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (short) I;
    if (I < System.Int16.MinValue) {
        SUCCESS_INDICATOR = false;
    } else if (I > System.Int16.MaxValue) {
        SUCCESS_INDICATOR = false;
    } else {
        SUCCESS_INDICATOR = true;
    }
").

:- pragma foreign_proc("Java",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (short) I;
    if (I < java.lang.Short.MIN_VALUE) {
        SUCCESS_INDICATOR = false;
    } else if (I > java.lang.Short.MAX_VALUE) {
        SUCCESS_INDICATOR = false;
    } else {
        SUCCESS_INDICATOR = true;
    }
").

det_from_int(I) = U :-
    ( if from_int(I, U0)
    then U = U0
    else error("int16.det_from_int: cannot convert int to int16")
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
    ( if int16.is_zero(B)
    then throw(math.domain_error("int16.'/': division by zero"))
    else unchecked_quotient(A, B)
    ).

:- pragma foreign_proc("C",
    unchecked_quotient(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    C = A / B;
").

A rem B =
    ( if int16.is_zero(B)
    then throw(math.domain_error("int16.'rem': second operand is zero"))
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
     B = (short) -A;
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
     C = (short) (A + B);
").

:- pragma foreign_proc("C#",
     (A::in) - (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = (short) (A - B);
").

:- pragma foreign_proc("C#",
     (A::in) * (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = (short) (A * B);
").

:- pragma foreign_proc("C#",
    unchecked_quotient(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = (short) (A / B);
").

:- pragma foreign_proc("C#",
    unchecked_rem(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = (short) (A % B);
").

:- pragma foreign_proc("Java",
     (A::in) + (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = (short) (A + B);
").

:- pragma foreign_proc("Java",
    - (A::in) = (B::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     B = (short) -A;
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
     C = (short) (A - B);
").

:- pragma foreign_proc("Java",
     (A::in) * (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = (short) (A * B);
").

:- pragma foreign_proc("Java",
    unchecked_quotient(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    C = (short) (A  / B);
").

:- pragma foreign_proc("Java",
    unchecked_rem(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = (short) (A % B);
").

%---------------------------------------------------------------------------%
%
% Other operations.
%

:- pragma foreign_proc("C",
    abs(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    /* XXX C doesn't provide a wrapper for abs() with int16_t. */
    B = (int16_t) abs(A);
").

% NOTE: the C# backend uses the following Mercury definition because
% System.Math.Abs() will throw an OverflowException for abs(int16.min_int16).

abs(I) = ( if I < int16.zero then int16.zero - I else I ).

:- pragma foreign_proc("Java",
    abs(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (short) java.lang.Math.abs(A);
").

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

A << B =
    ( if (B < 0 ; B > 15)
    then func_error("int16.'<<': second operand is out of range")
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
    C = (short) (A << B);
").

:- pragma foreign_proc("Java",
    unchecked_left_shift(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = (short) (A << B);
").

A >> B =
    ( if (B < 0 ; B > 15)
    then func_error("int16.'>>': second operand is out of range")
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
    C = (short) (A >> B);
").

:- pragma foreign_proc("Java",
    unchecked_right_shift(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = (short) (A >> B);
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
    C = (short) (A & B);
").

:- pragma foreign_proc("Java",
    (A::in) /\ (B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = (short) (A & B);
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
    C = (short) (A | B);
").

:- pragma foreign_proc("Java",
    (A::in) \/ (B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = (short)(A | B);
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
    C = (short) (A ^ B);
").

:- pragma foreign_proc("Java",
    xor(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = (short)(A ^ B);
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
    B = (short) (~A);
").

:- pragma foreign_proc("Java",
    \ (A::in) = (B::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    B = (short)(~A);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[100];
    sprintf(buffer, ""%"" PRId16 """", U);
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
    uint16_t U = I;

    if (U == 0) {
        MR_allocate_aligned_string_msg(S, 2, MR_ALLOC_ID);
        S[0] = '0';
        S[1] = '\\0';
    } else {

        char buffer[17];
        int i = 16;

        buffer[16] = '\\0';

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
    S = java.lang.Integer.toBinaryString(U & 0xffff);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_binary_string_lz(I::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = 16;
    uint16_t U = I;

    MR_allocate_aligned_string_msg(S, 16, MR_ALLOC_ID);
    S[16] = '\\0';
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
    S = System.Convert.ToString(U, 2).PadLeft(16, '0');
").

:- pragma foreign_proc("Java",
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.String.format(""%16s"",
        java.lang.Integer.toBinaryString(U & 0xffff)).replace(' ', '0');
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    to_hex_string(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    char buffer[100];
    sprintf(buffer, ""%"" PRIx16 """", U);
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
    S = java.lang.Integer.toHexString(U & 0xffff);
").

%---------------------------------------------------------------------------%

% The algorithms in this section are from chapter 5 of ``Hacker's Delight''
% by Henry S. Warren, Jr.
% (Java uses the same.)

num_zeros(U) = 16 - num_ones(U).

:- pragma foreign_proc("C",
    num_ones(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    uint16_t U = I;
    U = (U & 0x5555) + ((U >> 1) & 0x5555);
    U = (U & 0x3333) + ((U >> 2) & 0x3333);
    U = (U & 0x0f0f) + ((U >> 4) & 0x0f0f);
    U = (U & 0x00ff) + ((U >> 8) & 0x00ff);
    N = U;
").

:- pragma foreign_proc("C#",
    num_ones(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ushort U = (ushort) I;
    U = (ushort)((U & 0x5555) + ((U >> 1) & 0x5555));
    U = (ushort)((U & 0x3333) + ((U >> 2) & 0x3333));
    U = (ushort)((U & 0x0f0f) + ((U >> 4) & 0x0f0f));
    U = (ushort)((U & 0x00ff) + ((U >> 8) & 0x00ff));
    N = U;
").

:- pragma foreign_proc("Java",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.bitCount(U << 16);
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
    N = 0; // XXX NYI.
").

%---------------------------------------------------------------------------%

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
        N = 16;
    } else {
        N = 0; // XXX NYI.
    }
").

:- pragma foreign_proc("Java",
    num_trailing_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = java.lang.Integer.numberOfTrailingZeros(I);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
#if defined(MR_GNUC) || defined(MR_CLANG)
    B = (int16_t) __builtin_bswap16((uint16_t)A);
#else
    B = ((uint16_t)A >> 8) | (A << 8);
#endif
").

:- pragma foreign_proc("C#",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (short) ((A & 0xffU) << 8 | (A & 0xff00U) >> 8);
").

:- pragma foreign_proc("Java",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Short.reverseBytes(A);
").

:- pragma foreign_proc("C",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    B = A; // XXX NYI.
").

:- pragma foreign_proc("C#",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = A; // XXX NYI.
").

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
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
    min_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = INT16_MIN;
").

:- pragma foreign_proc("C#",
    min_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = System.Int16.MinValue;
").

:- pragma foreign_proc("Java",
    min_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = java.lang.Short.MIN_VALUE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    max_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    I = INT16_MAX;
").

:- pragma foreign_proc("C#",
    max_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = System.Int16.MaxValue;
").

:- pragma foreign_proc("Java",
    max_int16 = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = java.lang.Short.MAX_VALUE;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    zero = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = INT16_C(0);
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
    U = INT16_C(2);
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
    U = INT16_C(8);
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
    U = INT16_C(10);
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
    U = INT16_C(16);
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

:- pred is_zero(int16::in) is semidet.

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
:- end_module int16.
%---------------------------------------------------------------------------%
