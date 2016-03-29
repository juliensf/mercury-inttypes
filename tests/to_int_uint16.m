%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to int for unsigned 16-bit integers.

:- module to_int_uint16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint16.

%---------------------------------------------------------------------------%

:- import_module list.
:- import_module string.

main(!IO) :-
    list.foldl(to_int_test, numbers, !IO).

:- pred to_int_test(uint16::in, io::di, io::uo) is det.

to_int_test(UInt16, !IO) :-
    Int = uint16.to_int(UInt16),
    io.format("%sU16 -> %d\n", [s(to_string(UInt16)), i(Int)], !IO).

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
:- end_module to_int_uint16.
%---------------------------------------------------------------------------%

