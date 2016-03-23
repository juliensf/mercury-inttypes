%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to strings for unsigned 32-bit integers.

:- module to_string_uint32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint32.

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

:- pred run_to_string_test((func(uint32) = string)::in, string::in,
    io::di, io::uo) is det.

run_to_string_test(ConvFunc, Desc, !IO) :-
    io.format("*** Test uint32 conversion to %s string ***\n\n", [s(Desc)], !IO),
    list.foldl(run_to_string_test_2(ConvFunc), numbers, !IO).

:- pred run_to_string_test_2((func(uint32) = string)::in, uint32::in,
    io::di, io::uo) is det.

run_to_string_test_2(ConvFunc, N, !IO) :-
    S = ConvFunc(N),
    io.format("%s\n", [s(S)], !IO).

:- func numbers = list(uint32).

numbers = [
    uint32.zero,
    uint32.one,
    uint32.two,
    uint32.eight,
    uint32.ten,
    uint32.sixteen,
    uint32.max_uint8,
    uint32.max_uint16,
    uint32.max_uint32
].

%---------------------------------------------------------------------------%
:- end_module to_string_uint32.
%---------------------------------------------------------------------------%
