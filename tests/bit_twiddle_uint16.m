%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bit twiddling operations for unsigned 16-bit integers.

:- module bit_twiddle_uint16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint16.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    % XXX FIXME - NYI.
    %run_twiddle_test(uint16.num_zeros, "num_zeros", !IO),
    %io.nl(!IO),
    %run_twiddle_test(uint16.num_ones, "num_ones", !IO),
    %io.nl(!IO),
    %run_twiddle_test(uint16.num_leading_zeros, "num_leading_zeros", !IO),
    %io.nl(!IO),
    %run_twiddle_test(uint16.num_trailing_zeros, "num_trailing_zeros", !IO),
    %io.nl(!IO),
    %run_twiddle_test_b(uint16.reverse_bits, "reverse_bits", !IO),
    %io.nl(!IO),
    run_twiddle_test_b(uint16.reverse_bytes, "reverse_bytes", !IO).

%---------------------------------------------------------------------------%

% Test uint16 -> int functions.

:- pred run_twiddle_test((func(uint16) = int)::in, string::in,
    io::di, io::uo) is cc_multi.

run_twiddle_test(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_2((func(uint16) = int)::in, string::in,
    uint16::in, io::di, io::uo) is cc_multi.

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

% Test uint16 -> uint16 functions.

:- pred run_twiddle_test_b((func(uint16) = uint16)::in, string::in,
    io::di, io::uo) is cc_multi.

run_twiddle_test_b(Func, Desc, !IO) :-
    io.format("*** Test function '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_twiddle_test_b_2(Func, Desc), As, !IO).

:- pred run_twiddle_test_b_2((func(uint16) = uint16)::in, string::in,
    uint16::in, io::di, io::uo) is cc_multi.

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

:- func numbers = list(uint16).

numbers = [
    uint16.zero,
    uint16.one,
    uint16.two,
    uint16.eight,
    uint16.ten,
    uint16.sixteen,
    uint16.max_uint8,
    uint16.max_uint16
].

%---------------------------------------------------------------------------%
:- end_module bit_twiddle_uint16.
%---------------------------------------------------------------------------%
