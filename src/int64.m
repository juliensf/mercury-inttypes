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

:- func int64(int) = int64.

:- func from_int(int) = int64.

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

:- func int64 << int = int64.
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

:- func to_decimal_string(int64::in) = (string::uo) is det.

:- func to_hex_string(int64::in) = (string::uo) is det.

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
    SUCCESS_INDICATOR = (A == B);
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

:- pragma foreign_proc("C#",
    abs(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = System.Math.Abs(A);
").

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
    ( if B < 0
    then func_error("int64.'<<': second argument is negative")
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
    then func_error("int64.'>>': second argument is negative")
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
