%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to int for signed 32-bit integers.

:- module to_int_int32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int32.

%---------------------------------------------------------------------------%

:- import_module list.
:- import_module string.

main(!IO) :-
    list.foldl(to_int_test, numbers, !IO).

:- pred to_int_test(int32::in, io::di, io::uo) is det.

to_int_test(Int32, !IO) :-
    Int = int32.to_int(Int32),
    io.format("%sI32 -> %d\n", [s(to_string(Int32)), i(Int)], !IO).

:- func numbers = list(int32).

numbers = [
    int32.min_int32,
    -int32.max_int32,
    int32.min_int16,
    -int32.max_int16,
    int32.min_int8,
    -int32.max_int8,
    -int32.sixteen,
    -int32.ten,
    -int32.eight,
    -int32.two,
    -int32.one,
    int32.zero,
    int32.one,
    int32.two,
    int32.eight,
    int32.ten,
    int32.sixteen,
    int32.max_int8,
    int32.max_int16,
    int32.max_int32
].

%---------------------------------------------------------------------------%
:- end_module to_int_int32.
%---------------------------------------------------------------------------%
