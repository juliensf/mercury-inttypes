%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test arithmetic operations for signed 16-bit integers.

:- module arith_int16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int16.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_unop_test(int16.(+), "+", !IO),
    io.nl(!IO),
    run_unop_test(int16.(-), "-", !IO),
    io.nl(!IO),
    run_unop_test(int16.abs, "abs", !IO),
    io.nl(!IO),
    run_binop_test(int16.(+), "+", !IO),
    io.nl(!IO),
    run_binop_test(int16.(-), "-", !IO),
    io.nl(!IO),
    run_binop_test(int16.(*), "*", !IO),
    io.nl(!IO),
    run_binop_test(int16.(/), "/", !IO),
    io.nl(!IO),
    run_binop_test(int16.(rem), "rem", !IO).

%---------------------------------------------------------------------------%

:- pred run_unop_test((func(int16) = int16)::in, string::in,
    io::di, io::uo) is cc_multi.

run_unop_test(UnOpFunc, Desc, !IO) :-
    io.format("*** Test unary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_unop_test_2(UnOpFunc, Desc), As, !IO).

:- pred run_unop_test_2((func(int16) = int16)::in, string::in, int16::in,
    io::di, io::uo) is cc_multi.

run_unop_test_2(UnOpFunc, Desc, A, !IO) :-
    ( try []
        Result0 = UnOpFunc(A)
    then
        ResultStr = to_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s = %s\n",
        [s(Desc), s(to_string(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- pred run_binop_test((func(int16, int16) = int16)::in, string::in,
    io::di, io::uo) is cc_multi.

run_binop_test(BinOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_binop_test_2(BinOpFunc, Desc, Bs), As, !IO).

:- pred run_binop_test_2((func(int16, int16) = int16)::in, string::in,
    list(int16)::in, int16::in, io::di, io::uo) is cc_multi.

run_binop_test_2(BinOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_binop_test_3(BinOpFunc, Desc, A), Bs, !IO).

:- pred run_binop_test_3((func(int16, int16) = int16)::in, string::in,
    int16::in, int16::in, io::di, io::uo) is cc_multi.

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

:- func numbers = list(int16).

numbers = [
    int16.min_int16,
    int16.min_int8,
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
:- end_module arith_int16.
%---------------------------------------------------------------------------%
