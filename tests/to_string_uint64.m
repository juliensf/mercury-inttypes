%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to strings for unsigned 64-bit integers.

:- module to_string_uint64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint64.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_to_string_test(to_binary_string, "binary", !IO),
    io.nl(!IO),
    run_to_string_test(to_decimal_string, "decimal", !IO),
    io.nl(!IO),
    run_to_string_test(to_hex_string, "hexadecimal", !IO).

:- pred run_to_string_test((func(uint64) = string)::in, string::in,
    io::di, io::uo) is det.

run_to_string_test(ConvFunc, Desc, !IO) :-
    io.format("*** Test uint64 conversion to %s string ***\n\n", [s(Desc)], !IO),
    list.foldl(run_to_string_test_2(ConvFunc), numbers, !IO).

:- pred run_to_string_test_2((func(uint64) = string)::in, uint64::in,
    io::di, io::uo) is det.

run_to_string_test_2(ConvFunc, N, !IO) :-
    S = ConvFunc(N),
    io.format("%s\n", [s(S)], !IO).

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
:- end_module to_string_uint64.
%---------------------------------------------------------------------------%
