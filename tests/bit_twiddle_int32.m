%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bit twiddling operations for signed 32-bit integers.

:- module bit_twiddle_int32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int32.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_twiddle_test(int32.num_zeros, "num_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(int32.num_ones, "num_ones", !IO),
    io.nl(!IO),
    run_twiddle_test(int32.num_leading_zeros, "num_leading_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(int32.num_trailing_zeros, "num_trailing_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test_b(int32.reverse_bits, "reverse_bits", !IO),
    io.nl(!IO),
    run_twiddle_test_b(int32.reverse_bytes, "reverse_bytes", !IO).

%---------------------------------------------------------------------------%

:- pred run_twiddle_test((func(int32) = int)::in, string::in,
    io::di, io::uo) is cc_multi.

run_twiddle_test(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_2((func(int32) = int)::in, string::in,
    int32::in, io::di, io::uo) is cc_multi.

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

% Test int32 -> int32 functions.

:- pred run_twiddle_test_b((func(int32) = int32)::in, string::in,
    io::di, io::uo) is cc_multi.

run_twiddle_test_b(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_b_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_b_2((func(int32) = int32)::in, string::in,
    int32::in, io::di, io::uo) is cc_multi.

run_twiddle_test_b_2(Func, Desc, A, !IO) :-
    ( try []
        Result0 = Func(A)
    then
        ResultStr = to_binary_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s(%s) = %s\n",
        [s(Desc), s(to_binary_string(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(int32).

numbers = [
    int32.min_int32,
    int32.min_int16,
    int32.min_int8,
    int32.zero,
    int32.one,
    int32.two,
    int32.eight,
    int32.ten,
    int32.sixteen,
    int32.max_int8,
    int32.max_int16,
    int32.max_int32
].

%---------------------------------------------------------------------------%
:- end_module bit_twiddle_int32.
%---------------------------------------------------------------------------%
