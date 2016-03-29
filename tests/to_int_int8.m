%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to int for signed 8-bit integers.

:- module to_int_int8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int8.

%---------------------------------------------------------------------------%

:- import_module list.
:- import_module string.

main(!IO) :-
    list.foldl(to_int_test, numbers, !IO).

:- pred to_int_test(int8::in, io::di, io::uo) is det.

to_int_test(Int8, !IO) :-
    Int = int8.to_int(Int8),
    io.format("%sI8 -> %d\n", [s(to_string(Int8)), i(Int)], !IO).

:- func numbers = list(int8).

numbers = [
    int8.min_int8,
    -int8.max_int8,
    -int8.sixteen,
    -int8.ten,
    -int8.eight,
    -int8.two,
    -int8.one,
    int8.zero,
    int8.one,
    int8.two,
    int8.eight,
    int8.ten,
    int8.sixteen,
    int8.max_int8
].

%---------------------------------------------------------------------------%
:- end_module to_int_int8.
%---------------------------------------------------------------------------%

