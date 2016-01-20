%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test arithmetic operations for unsigned 64-bit integers.

:- module arith_uint64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint64.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_binop_test(uint64.(+), "+", !IO),
    io.nl(!IO),
    run_binop_test(uint64.(-), "-", !IO),
    io.nl(!IO),
    run_binop_test(uint64.(*), "*", !IO),
    io.nl(!IO),
    run_binop_test(uint64.(/), "/", !IO).
    %io.nl(!IO),
    %run_binop_test(uint64.(rem), "rem", !IO).

:- pred run_binop_test((func(uint64, uint64) = uint64)::in, string::in,
    io::di, io::uo) is cc_multi.

run_binop_test(BinOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_binop_test_2(BinOpFunc, Desc, Bs), As, !IO).

:- pred run_binop_test_2((func(uint64, uint64) = uint64)::in, string::in,
    list(uint64)::in, uint64::in, io::di, io::uo) is cc_multi.

run_binop_test_2(BinOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_binop_test_3(BinOpFunc, Desc, A), Bs, !IO).

:- pred run_binop_test_3((func(uint64, uint64) = uint64)::in, string::in,
    uint64::in, uint64::in, io::di, io::uo) is cc_multi.

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
    uint64.max_uint64
].

%---------------------------------------------------------------------------%
:- end_module arith_uint64.
%---------------------------------------------------------------------------%
