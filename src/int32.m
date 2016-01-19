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

:- func int32(int) = int32.

:- pred from_int(int::in, int32::out) is semidet.

:- func det_from_int(int) = int32.

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

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

:- func int32 << int = int32.
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

:- func to_decimal_string(int32::in) = (string::uo) is det.

:- func to_hex_string(int32::in) = (string::uo) is det.

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
    SUCCESS_INDICATOR = (A == B);
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
    B = (int) A;
    if (A > (MR_Integer) INT32_MAX) {
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
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    C = A / B;
").

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

A << B =
    ( if B < 0
    then func_error("int32.'<<': amount to shift by is negative")
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
    then func_error("int32.'>>': amount to shift by is negative")
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
    sprintf(buffer, ""%"" PRId32 """", U);
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
    long l_U = U & 0xffffffffL;
    S = java.lang.Long.toString(l_U);
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
