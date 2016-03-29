%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to int for unsigned 8-bit integers.

:- module to_int_uint8.
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
    list.foldl(to_int_test, numbers, !IO).

:- pred to_int_test(uint8::in, io::di, io::uo) is det.

to_int_test(UInt8, !IO) :-
    Int = uint8.to_int(UInt8),
    io.format("%sU8 -> %d\n", [s(to_string(UInt8)), i(Int)], !IO).

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
:- end_module to_int_uint8.
%---------------------------------------------------------------------------%
