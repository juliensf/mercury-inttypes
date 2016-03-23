%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to strings for unsigned 8-bit integers.

:- module to_string_uint8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint8.

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

:- pred run_to_string_test((func(uint8) = string)::in, string::in,
    io::di, io::uo) is det.

run_to_string_test(ConvFunc, Desc, !IO) :-
    io.format("*** Test uint8 conversion to %s string ***\n\n", [s(Desc)], !IO),
    list.foldl(run_to_string_test_2(ConvFunc), numbers, !IO).

:- pred run_to_string_test_2((func(uint8) = string)::in, uint8::in,
    io::di, io::uo) is det.

run_to_string_test_2(ConvFunc, N, !IO) :-
    S = ConvFunc(N),
    io.format("%s\n", [s(S)], !IO).

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
:- end_module to_string_uint8.
%---------------------------------------------------------------------------%
