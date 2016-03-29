%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to int for signed 32-bit integers.

:- module to_int_uint32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint32.

%---------------------------------------------------------------------------%

:- import_module list.
:- import_module string.

main(!IO) :-
    list.foldl(to_int_test, numbers, !IO).

:- pred to_int_test(uint32::in, io::di, io::uo) is det.

to_int_test(UInt32, !IO) :-
    ( if semidet_to_int(UInt32, Int) then
        Result = int_to_string(Int)
    else
        Result = "<<out-of-range>>"
    ),
    io.format("%sU32 -> %s\n", [s(to_string(UInt32)), s(Result)], !IO).

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
    uint32.max_int32,
    uint32.max_int32 + uint32.one,
    uint32.max_uint32
].

%---------------------------------------------------------------------------%
:- end_module to_int_uint32.
%---------------------------------------------------------------------------%
