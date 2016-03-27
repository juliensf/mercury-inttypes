%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bit twiddling operations for signed 16-bit integers.

:- module bit_twiddle_int16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int16.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_twiddle_test(int16.num_zeros, "num_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(int16.num_ones, "num_ones", !IO),
    io.nl(!IO),
    %run_twiddle_test(int16.num_leading_zeros, "num_leading_zeros", !IO),
    %io.nl(!IO),
    %run_twiddle_test(int16.num_trailing_zeros, "num_trailing_zeros", !IO),
    %io.nl(!IO),
    %run_twiddle_test_b(int16.reverse_bits, "reverse_bits", !IO),
    io.nl(!IO),
    run_twiddle_test_b(int16.reverse_bytes, "reverse_bytes", !IO).

%---------------------------------------------------------------------------%

:- pred run_twiddle_test((func(int16) = int)::in, string::in,
    io::di, io::uo) is cc_multi.

run_twiddle_test(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_2((func(int16) = int)::in, string::in,
    int16::in, io::di, io::uo) is cc_multi.

run_twiddle_test_2(Func, Desc, A, !IO) :-
    ( try []
        Result0 = Func(A)
    then
        int_to_string(Result0, ResultStr)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s(0b%s) = %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

% Test int16 -> int16 functions.

:- pred run_twiddle_test_b((func(int16) = int16)::in, string::in,
    io::di, io::uo) is cc_multi.

run_twiddle_test_b(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_b_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_b_2((func(int16) = int16)::in, string::in,
    int16::in, io::di, io::uo) is cc_multi.

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

:- func numbers = list(int16).

numbers = [
    int16.min_int16,
    int16.min_int8,
    int16.zero,
    int16.one,
    int16.two,
    int16.eight,
    int16.ten,
    int16.sixteen,
    int16.max_int8,
    int16.max_int16
].

%---------------------------------------------------------------------------%
:- end_module bit_twiddle_int16.
%---------------------------------------------------------------------------%
