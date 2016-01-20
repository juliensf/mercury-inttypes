%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test arithmetic operations for unsigned 32-bit integers.

:- module arith_uint32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint32.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_binop_test(uint32.(+), "+", !IO),
    io.nl(!IO),
    run_binop_test(uint32.(-), "-", !IO),
    io.nl(!IO),
    run_binop_test(uint32.(*), "*", !IO),
    io.nl(!IO),
    run_binop_test(uint32.(/), "/", !IO).
    %io.nl(!IO),
    %run_binop_test(uint32.(rem), "rem", !IO).

:- pred run_binop_test((func(uint32, uint32) = uint32)::in, string::in,
    io::di, io::uo) is cc_multi.

run_binop_test(BinOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_binop_test_2(BinOpFunc, Desc, Bs), As, !IO).

:- pred run_binop_test_2((func(uint32, uint32) = uint32)::in, string::in,
    list(uint32)::in, uint32::in, io::di, io::uo) is cc_multi.

run_binop_test_2(BinOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_binop_test_3(BinOpFunc, Desc, A), Bs, !IO).

:- pred run_binop_test_3((func(uint32, uint32) = uint32)::in, string::in,
    uint32::in, uint32::in, io::di, io::uo) is cc_multi.

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
    uint32.max_uint32
].

%---------------------------------------------------------------------------%
:- end_module arith_uint32.
%---------------------------------------------------------------------------%
