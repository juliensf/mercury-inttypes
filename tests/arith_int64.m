%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test arithmetic operations for signed 64-bit integers.

:- module arith_int64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int64.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_binop_test(int64.(+), "+", !IO),
    io.nl(!IO),
    run_binop_test(int64.(-), "-", !IO),
    io.nl(!IO),
    run_binop_test(int64.(*), "*", !IO),
    io.nl(!IO),
    run_binop_test(int64.(/), "/", !IO),
    io.nl(!IO),
    run_binop_test(int64.(rem), "rem", !IO).

:- pred run_binop_test((func(int64, int64) = int64)::in, string::in,
    io::di, io::uo) is cc_multi.

run_binop_test(BinOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_binop_test_2(BinOpFunc, Desc, Bs), As, !IO).

:- pred run_binop_test_2((func(int64, int64) = int64)::in, string::in,
    list(int64)::in, int64::in, io::di, io::uo) is cc_multi.

run_binop_test_2(BinOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_binop_test_3(BinOpFunc, Desc, A), Bs, !IO).

:- pred run_binop_test_3((func(int64, int64) = int64)::in, string::in,
    int64::in, int64::in, io::di, io::uo) is cc_multi.

run_binop_test_3(BinOpFunc, Desc, A, B, !IO) :-
    ( try []
        Result0 = BinOpFunc(A, B)
    then
        ResultStr = to_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s %s = %s\n",
        [s(to_string(A)), s(Desc), s(to_string(B)), s(ResultStr)], !IO).

:- func numbers = list(int64).

numbers = [
    int64.min_int64,
    int64.min_int32,
    int64.min_int16,
    int64.min_int8,
    int64.zero,
    int64.one,
    int64.two,
    int64.eight,
    int64.ten,
    int64.sixteen,
    int64.max_int8,
    int64.max_int16,
    int64.max_int32,
    int64.max_int64
].

%---------------------------------------------------------------------------%
:- end_module arith_int64.
%---------------------------------------------------------------------------%
