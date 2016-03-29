%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion to int for signed 64-bit integers.

:- module to_int_int64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int64.

%---------------------------------------------------------------------------%

:- import_module list.
:- import_module string.

main(!IO) :-
    list.foldl(to_int_test, numbers, !IO).

:- pred to_int_test(int64::in, io::di, io::uo) is det.

to_int_test(Int64, !IO) :-
    ( if semidet_to_int(Int64, Int) then
        Result = int_to_string(Int)
    else
        Result = "<<out-of-range>>"
    ),
    io.format("%sI64 -> %s\n", [s(to_string(Int64)), s(Result)], !IO).

:- func numbers = list(int64).

numbers = [
    int64.min_int64,
    -int64.max_int64,
    int64.min_int32 - int64.one,
    int64.min_int32,
    -int64.max_int32,
    int64.min_int16,
    -int64.max_int16,
    int64.min_int8,
    -int64.max_int8,
    -int64.sixteen,
    -int64.ten,
    -int64.eight,
    -int64.two,
    -int64.one,
    int64.zero,
    int64.one,
    int64.two,
    int64.eight,
    int64.ten,
    int64.sixteen,
    int64.max_int8,
    int64.max_int16,
    int64.max_int32,
    int64.max_int32 + int64.one,
    int64.max_int64
].

%---------------------------------------------------------------------------%
:- end_module to_int_int64.
%---------------------------------------------------------------------------%
