%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to strings for nsigned 64-bit integers.

:- module to_string_int64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int64.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_to_string_test(to_binary_string, "binary", !IO),
    io.nl(!IO),
    run_to_string_test(to_binary_string_lz, "binary (with leading zeros)", !IO),
    io.nl(!IO),
    run_to_string_test(to_decimal_string, "decimal", !IO),
    io.nl(!IO),
    run_to_string_test(to_hex_string, "hexadecimal", !IO).

:- pred run_to_string_test((func(int64) = string)::in, string::in,
    io::di, io::uo) is det.

run_to_string_test(ConvFunc, Desc, !IO) :-
    io.format("*** Test int64 conversion to %s string ***\n\n", [s(Desc)], !IO),
    list.foldl(run_to_string_test_2(ConvFunc), numbers, !IO).

:- pred run_to_string_test_2((func(int64) = string)::in, int64::in,
    io::di, io::uo) is det.

run_to_string_test_2(ConvFunc, N, !IO) :-
    S = ConvFunc(N),
    io.format("%s\n", [s(S)], !IO).

:- func numbers = list(int64).

numbers = [
    int64.min_int64,
    -int64.max_int64,
    int64.min_int32,
    -int64.max_int32,
    int64.min_int16,
    -int64.max_int16,
    int64.min_int8,
    -int64.max_int8,
    -int64.sixteen,
    -int64.ten,
    -int64.eight,
    -int64.two,
    -int64.one,
    int64.zero,
    int64.one,
    int64.two,
    int64.eight,
    int64.ten,
    int64.sixteen,
    int64.max_int8,
    int64.max_int16,
    int64.max_int32,
    int64.max_int64
].

%---------------------------------------------------------------------------%
:- end_module to_string_int64.
%---------------------------------------------------------------------------%
