%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bit twiddling operations for unsigned 8-bit integers.

:- module bit_twiddle_uint8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint8.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    %run_twiddle_test(uint8.num_zeros, "num_zeros", !IO),
    %io.nl(!IO),
    %run_twiddle_test(uint8.num_ones, "num_ones", !IO),
    %io.nl(!IO),
    %run_twiddle_test(uint8.num_leading_zeros, "num_leading_zeros", !IO),
    %io.nl(!IO),
    %run_twiddle_test(uint8.num_trailing_zeros, "num_trailing_zeros", !IO),
    %io.nl(!IO),
    run_twiddle_test_b(uint8.reverse_bits, "reverse_bits", !IO).

%---------------------------------------------------------------------------%

% Test uint8 -> int functions.

:- pred run_twiddle_test((func(uint8) = int)::in, string::in,
    io::di, io::uo) is cc_multi.

run_twiddle_test(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_2((func(uint8) = int)::in, string::in,
    uint8::in, io::di, io::uo) is cc_multi.

run_twiddle_test_2(Func, Desc, A, !IO) :-
    ( try []
        Result0 = Func(A)
    then
        int_to_string(Result0, ResultStr)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s(%s) = %s\n",
        [s(Desc), s(to_decimal_string(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

% Test uint8 -> uint8 functions.

:- pred run_twiddle_test_b((func(uint8) = uint8)::in, string::in,
    io::di, io::uo) is cc_multi.

run_twiddle_test_b(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_b_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_b_2((func(uint8) = uint8)::in, string::in,
    uint8::in, io::di, io::uo) is cc_multi.

run_twiddle_test_b_2(Func, Desc, A, !IO) :-
    ( try []
        Result0 = Func(A)
    then
        ResultStr = "0b" ++ to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s(0b%s) = %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(uint8).

numbers = [
    uint8.zero,
    uint8.one,
    uint8.two,
    uint8.eight,
    uint8.ten,
    uint8.sixteen,
    uint8.max_uint8
].

%---------------------------------------------------------------------------%
:- end_module bit_twiddle_uint8.
%---------------------------------------------------------------------------%
