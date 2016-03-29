%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to int for signed 16-bit integers.

:- module to_int_int16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int16.

%---------------------------------------------------------------------------%

:- import_module list.
:- import_module string.

main(!IO) :-
    list.foldl(to_int_test, numbers, !IO).

:- pred to_int_test(int16::in, io::di, io::uo) is det.

to_int_test(Int16, !IO) :-
    Int = int16.to_int(Int16),
    io.format("%sI16 -> %d\n", [s(to_string(Int16)), i(Int)], !IO).

:- func numbers = list(int16).

numbers = [
    int16.min_int16,
    -int16.max_int16,
    int16.min_int8,
    -int16.max_int8,
    -int16.sixteen,
    -int16.ten,
    -int16.eight,
    -int16.two,
    -int16.one,
    int16.zero,
    int16.one,
    int16.two,
    int16.eight,
    int16.ten,
    int16.sixteen,
    int16.max_int8,
    int16.max_int16
].

%---------------------------------------------------------------------------%
:- end_module to_int_int16.
%---------------------------------------------------------------------------%

