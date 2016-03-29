%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2016, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% This module provides unsigned 16-bit integers.
%
%---------------------------------------------------------------------------%

:- module uint16.
:- interface.

:- type uint16.

%---------------------------------------------------------------------------%
%
% Conversion.
%

    % from_int(A, B):
    % Fails if A is not in [0, uint16.max_uint16].
    %
:- pred from_int(int::in, uint16::out) is semidet.

    % As above, but throws a software_error/1 exception instead of failing.
    %
:- func det_from_int(int) = uint16.

    % A synonym for the function det_from_int/1.
    %
:- func uint16(int) = uint16.

:- func to_int(uint16) = int.

%---------------------------------------------------------------------------%
%
% Comparison.
%

:- pred (uint16::in) < (uint16::in) is semidet.

:- pred (uint16::in) > (uint16::in) is semidet.

:- pred (uint16::in) =< (uint16::in) is semidet.

:- pred (uint16::in) >= (uint16::in) is semidet.

:- func max(uint16, uint16) = uint16.

:- func min(uint16, uint16) = uint16.

%---------------------------------------------------------------------------%
%
% Arithmetic operations.
%

:- func uint16 + uint16 = uint16.

:- func uint16 - uint16 = uint16.

:- func uint16 * uint16 = uint16.

    % Throws a math.domain_error/1 exception for division by zero.
    %
:- func uint16 / uint16 = uint16.

    % Behaviour is undefined for division by zero.
    %
:- func unchecked_quotient(uint16, uint16) = uint16.

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

    % A << B:
    % Aborts if B is not in [0, 15].
    %
:- func uint16 << int = uint16.

    % A >> B:
    % Aborts if B is not in [0, 15].
    %
:- func uint16 >> int = uint16.

:- func unchecked_left_shift(uint16, int) = uint16.

:- func unchecked_right_shift(uint16, int) = uint16.

:- func (uint16::in) /\ (uint16::in) = (uint16::out) is det.

:- func (uint16::in) \/ (uint16::in) = (uint16::out) is det.

:- func xor(uint16, uint16) = uint16.

:- func \ (uint16::in) = (uint16::out) is det.

%---------------------------------------------------------------------------%
%
% Conversion to strings.
%

    % Synonym for to_decimal_string/1.
    %
:- func to_string(uint16::in) = (string::uo) is det.

:- func to_binary_string(uint16::in) = (string::uo) is det.

:- func to_binary_string_lz(uint16::in) = (string::uo) is det.

:- func to_decimal_string(uint16::in) = (string::uo) is det.

:- func to_hex_string(uint16::in) = (string::uo) is det.

%---------------------------------------------------------------------------%

    % num_zeros(U) = N:
    % N is the number of zeros in the binary representation of U.
    %
:- func num_zeros(uint16) = int.

    % num_ones(U) = N:
    % N is the number of ones in the binary representation of U.
    %
:- func num_ones(uint16) = int.

    % num_leading_zeros(U) = N:
    % N is the number of leading zeros in the binary representation of U.
    %
:- func num_leading_zeros(uint16) = int.

    % num_trailing_zeros(U) = N:
    % N is the number of trailing zeros in the binary representation of U.
    %
:- func num_trailing_zeros(uint16) = int.

    % reverse_bytes(A) = B:
    % B is the value that results from reversing the bytes in the
    % representation of A.
    %
:- func reverse_bytes(uint16) = uint16.

    % reverse_bits(A) = B:
    % B is the is value that results from reversing the bits in the
    % representation of A.
    %
:- func reverse_bits(uint16) = uint16.

%---------------------------------------------------------------------------%
%
% Constants.
%

:- func max_uint8 = uint16.
:- func max_uint16 = uint16.

:- func zero = uint16.
:- func one = uint16.
:- func two = uint16.
:- func eight = uint16.
:- func ten = uint16.
:- func sixteen = uint16.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred uint16_equal(uint16::in, uint16::in) is semidet.

:- pred uint16_compare(comparison_result::uo, uint16::in, uint16::in) is det.

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

:- pragma foreign_type("C", uint16, "uint16_t",
    [can_pass_as_mercury_type, stable])
    where equality is uint16_equal,
          comparison is uint16_compare.

%---------------------------------------------------------------------------%
%
% C# implementation.
%

:- pragma foreign_type("C#", uint16, "ushort")
    where equality is uint16_equal,
          comparison is uint16_compare.

%---------------------------------------------------------------------------%
%
% Java implementation.
%

:- pragma foreign_type("Java", uint16, "java.lang.Short")
    where equality is uint16_equal,
          comparison is uint16_compare.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    uint16_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    SUCCESS_INDICATOR = (A == B) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C#",
    uint16_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A == B);
").

:- pragma foreign_proc("Java",
    uint16_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (A.intValue() == B.intValue());
").

uint16_compare(Result, A, B) :-
    ( if A < B then
        Result = (<)
    else if A > B then
        Result = (>)
    else
        Result = (=)
    ).

%---------------------------------------------------------------------------%

uint16(I) = det_from_int(I).

:- pragma foreign_proc("C",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    if (I < 0) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else if (I > (MR_Integer) UINT16_MAX) {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        U = (uint16_t) I;
        SUCCESS_INDICATOR = MR_TRUE;
    }
").

:- pragma foreign_proc("C#",
    from_int(I::in, U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (ushort) I;
    if (I < 0) {
        SUCCESS_INDICATOR = false;
    } else if (I > System.UInt16.MaxValue) {
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
    if (I < 0) {
        SUCCESS_INDICATOR = false;
    } else if (I > 0xffff) {
        SUCCESS_INDICATOR = false;
    } else {
        SUCCESS_INDICATOR = true;
    }
").

det_from_int(I) = U :-
    ( if from_int(I, U0)
    then U = U0
    else error("uint16.det_from_int: cannot convert int to uint16")
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
    B = A & 0xffff;
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
    SUCCESS_INDICATOR = (A & 0xffff) < (B & 0xffff);
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
    SUCCESS_INDICATOR = (A & 0xffff) > (B & 0xffff);
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
    SUCCESS_INDICATOR = (A & 0xffff) <= (B & 0xffff);
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
    SUCCESS_INDICATOR = (A & 0xffff) >= (B & 0xffff);
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
    ( if uint16.is_zero(B)
    then throw(math.domain_error("uint16.'/': division by zero"))
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
     C = (ushort) (A + B);
").

:- pragma foreign_proc("C#",
     (A::in) - (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = (ushort) (A - B);
").

:- pragma foreign_proc("C#",
     (A::in) * (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = (ushort) (A * B);
").

:- pragma foreign_proc("C#",
    unchecked_quotient(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = (ushort) (A / B);
").

:- pragma foreign_proc("Java",
     (A::in) + (B::in) = (C::out),
     [will_not_call_mercury, promise_pure, thread_safe],
"
     C = (short) (A + B);
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
    C = (short) ((A & 0xffff) / (B & 0xffff));
").

%---------------------------------------------------------------------------%
%
% Bitwise operations.
%

A << B =
    ( if (B < 0 ; B > 15)
    then func_error("uint16.'<<': second operand is out of range")
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
    C = (ushort) (A << B);
").

:- pragma foreign_proc("Java",
    unchecked_left_shift(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = (short) (A << B);
").

A >> B =
    ( if (B < 0 ; B > 15)
    then func_error("uint16.'>>': second operand is out of range")
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
    C = (ushort) (A >> B);
").

:- pragma foreign_proc("Java",
    unchecked_right_shift(A::in, B::in) = (C::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    C = (short) ((A & 0xffff) >>> B);
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
    C = (ushort) (A & B);
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
    C = (ushort) (A | B);
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
    C = (ushort) (A ^ B);
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
    B = (ushort) (~A);
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
    sprintf(buffer, ""%"" PRIu16 """", U);
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
    S = java.lang.Integer.toString(U & 0xffff);
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
    to_binary_string_lz(U::in) = (S::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    int i = 16;

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

num_zeros(U) = 16 - num_ones(U).

:- pragma foreign_proc("C",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = (U & 0x5555) + ((U >> 1) & 0x5555);
    U = (U & 0x3333) + ((U >> 2) & 0x3333);
    U = (U & 0x0f0f) + ((U >> 4) & 0x0f0f);
    U = (U & 0x00ff) + ((U >> 8) & 0x00ff);
    N = U;
").

:- pragma foreign_proc("C#",
    num_ones(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = (ushort) ((U & 0x5555) + ((U >> 1) & 0x5555));
    U = (ushort) ((U & 0x3333) + ((U >> 2) & 0x3333));
    U = (ushort) ((U & 0x0f0f) + ((U >> 4) & 0x0f0f));
    U = (ushort) ((U & 0x00ff) + ((U >> 8) & 0x00ff));
    N = (int) U;
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
    if (U == 0) {
        N = 16;
    } else {
        int n = 1;
        if ((U >> 8) == 0) { n = n + 8;   U = U << 8; }
        if ((U >> 12) == 0) { n = n + 4;  U = U << 4; }
        if ((U >> 14) == 0) { n = n + 2;  U = U << 2; }
        if ((U >> 15) == 0) { n = n + 1;  U = U << 1; }
        N = n - (int)(U >> 15);
    }
").

:- pragma foreign_proc("C#",
    num_leading_zeros(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (U == 0) {
        N = 16;
    } else {
        int n = 1;
        if ((U >> 8) == 0)  { n = n + 8; U = (ushort)(U << 8); }
        if ((U >> 12) == 0) { n = n + 4; U = (ushort)(U << 4); }
        if ((U >> 14) == 0) { n = n + 2; U = (ushort)(U << 2); }
        if ((U >> 15) == 0) { n = n + 1; U = (ushort)(U << 1); }
        N = n - (int)(U >> 15);
    }
").

:- pragma foreign_proc("Java",
    num_leading_zeros(I::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (I == 0) {
        N = 16;
    } else {
        N = java.lang.Integer.numberOfLeadingZeros(I << 16);
    }
").

%---------------------------------------------------------------------------%

num_trailing_zeros(U) =
    16 - num_leading_zeros(\ U /\ (U - one)).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if defined(MR_GNUC) || defined(MR_CLANG)
    B = __builtin_bswap16(A);
#else
    B = (A >> 8) | (A << 8);
#endif
").

:- pragma foreign_proc("C#",
    reverse_bytes(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (ushort) ((A & 0xffU) << 8 | (A & 0xff00U) >> 8);
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
    A = (((~0x5555) & A) >> 1) | ((0x5555 & A) << 1);
    A = (((~0x3333) & A) >> 2) | ((0x3333 & A) << 2);
    A = (((~0x0f0f) & A) >> 4) | ((0x0f0f & A) << 4);
    A = (((~0x00ff) & A) >> 8) | ((0x00ff & A) << 8);
    B = A;
").

:- pragma foreign_proc("C#",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    A = (ushort)((((~0x5555) & A) >> 1) | ((0x5555 & A) << 1));
    A = (ushort)((((~0x3333) & A) >> 2) | ((0x3333 & A) << 2));
    A = (ushort)((((~0x0f0f) & A) >> 4) | ((0x0f0f & A) << 4));
    A = (ushort)((((~0x00ff) & A) >> 8) | ((0x00ff & A) << 8));
    B = A;
").

:- pragma foreign_proc("Java",
    reverse_bits(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (short) (java.lang.Integer.reverse(A << 16) & 0xffff);
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
    U = (short) 0xffff;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    zero = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    U = UINT16_C(0);
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
    U = UINT16_C(1);
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
    U = UINT16_C(2);
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
    U = UINT16_C(8);
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
    U = UINT16_C(10);
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
    U = UINT16_C(16);
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

:- pred is_zero(uint16::in) is semidet.

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
:- end_module uint16.
%---------------------------------------------------------------------------%
