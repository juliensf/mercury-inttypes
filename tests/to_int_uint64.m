%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to int for unsigned 64-bit integers.

:- module to_int_uint64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint64.

%---------------------------------------------------------------------------%

:- import_module list.
:- import_module string.

main(!IO) :-
    list.foldl(to_int_test, numbers, !IO).

:- pred to_int_test(uint64::in, io::di, io::uo) is det.

to_int_test(UInt64, !IO) :-
    ( if semidet_to_int(UInt64, Int) then
        Result = int_to_string(Int)
    else
        Result = "<<out-of-range>>"
    ),
    io.format("%sU64 -> %s\n", [s(to_string(UInt64)), s(Result)], !IO).

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
    uint64.max_uint32 + uint64.one,
    uint64.max_uint64
].

%---------------------------------------------------------------------------%
:- end_module to_int_uint64.
%---------------------------------------------------------------------------%
