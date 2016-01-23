%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bit twiddling operations for unsigned 64-bit integers.

:- module bit_twiddle_uint64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint64.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_twiddle_test(uint64.num_zeros, "num_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(uint64.num_ones, "num_ones", !IO),
    io.nl(!IO),
    run_twiddle_test(uint64.num_leading_zeros, "num_leading_zeros", !IO),
    io.nl(!IO),
    run_twiddle_test(uint64.num_trailing_zeros, "num_trailing_zeros", !IO).

%---------------------------------------------------------------------------%

:- pred run_twiddle_test((func(uint64) = int)::in, string::in,
    io::di, io::uo) is cc_multi.

run_twiddle_test(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_2((func(uint64) = int)::in, string::in,
    uint64::in, io::di, io::uo) is cc_multi.

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

:- func numbers = list(uint64).

numbers = [
    uint64.zero,
    uint64.one,
    uint64.two,
    uint64.eight,
    uint64.ten,
    uint64.sixteen,
    uint64.max_uint8,
    uint64.max_uint16,
    uint64.max_uint32,
    uint64.max_uint64
].

%---------------------------------------------------------------------------%
:- end_module bit_twiddle_uint64.
%---------------------------------------------------------------------------%
