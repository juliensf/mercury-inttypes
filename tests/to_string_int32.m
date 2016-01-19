%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to strings for nsigned 64-bit integers.

:- module to_string_int32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int32.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_to_string_test(to_binary_string, "binary", !IO),
    io.nl(!IO),
    run_to_string_test(to_decimal_string, "decimal", !IO),
    io.nl(!IO),
    run_to_string_test(to_hex_string, "hexadecimal", !IO).

:- pred run_to_string_test((func(int32) = string)::in, string::in,
    io::di, io::uo) is det.

run_to_string_test(ConvFunc, Desc, !IO) :-
    io.format("*** Test int32 conversion to %s string ***\n\n", [s(Desc)], !IO),
    list.foldl(run_to_string_test_2(ConvFunc), numbers, !IO).

:- pred run_to_string_test_2((func(int32) = string)::in, int32::in,
    io::di, io::uo) is det.

run_to_string_test_2(ConvFunc, N, !IO) :-
    S = ConvFunc(N),
    io.format("%s\n", [s(S)], !IO).

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
:- end_module to_string_int32.
%---------------------------------------------------------------------------%
