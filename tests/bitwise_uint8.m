%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bitwise operations for unsigned 8-bit integers.

:- module bitwise_uint8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint8.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_unop_test(uint8.(\), "\\", !IO),
    io.nl(!IO),
    run_binop_test(uint8.(/\), "/\\", !IO),
    io.nl(!IO),
    run_binop_test(uint8.(\/), "\\/", !IO),
    io.nl(!IO),
    run_binop_test(uint8.(xor), "xor", !IO),
    io.nl(!IO),
    run_shift_test(uint8.(>>), ">>", !IO),
    io.nl(!IO),
    run_shift_test(uint8.(<<), "<<", !IO).

%---------------------------------------------------------------------------%

:- pred run_unop_test((func(uint8) = uint8)::in, string::in,
    io::di, io::uo) is cc_multi.

run_unop_test(UnOpFunc, Desc, !IO) :-
    io.format("*** Test unary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_unop_test_2(UnOpFunc, Desc), As, !IO).

:- pred run_unop_test_2((func(uint8) = uint8)::in, string::in,
    uint8::in, io::di, io::uo) is cc_multi.

run_unop_test_2(UnOpFunc, Desc, A, !IO) :-
    ( try []
        Result0 = UnOpFunc(A)
    then
        ResultStr = to_binary_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s = %s\n",
        [s(Desc), s(to_binary_string(A)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- pred run_binop_test((func(uint8, uint8) = uint8)::in, string::in,
    io::di, io::uo) is cc_multi.

run_binop_test(BinOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_binop_test_2(BinOpFunc, Desc, Bs), As, !IO).

:- pred run_binop_test_2((func(uint8, uint8) = uint8)::in, string::in,
    list(uint8)::in, uint8::in, io::di, io::uo) is cc_multi.

run_binop_test_2(BinOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_binop_test_3(BinOpFunc, Desc, A), Bs, !IO).

:- pred run_binop_test_3((func(uint8, uint8) = uint8)::in, string::in,
    uint8::in, uint8::in, io::di, io::uo) is cc_multi.

run_binop_test_3(BinOpFunc, Desc, A, B, !IO) :-
    ( try []
        Result0 = BinOpFunc(A, B)
    then
        ResultStr = to_binary_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s %s = %s\n",
        [s(to_binary_string(A)), s(Desc),
        s(to_binary_string(B)), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- pred run_shift_test((func(uint8, int) = uint8)::in, string::in,
    io::di, io::uo) is cc_multi.

run_shift_test(ShiftOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = shift_amounts,
    list.foldl(run_shift_test_2(ShiftOpFunc, Desc, Bs), As, !IO).

:- pred run_shift_test_2((func(uint8, int) = uint8)::in, string::in,
    list(int)::in, uint8::in, io::di, io::uo) is cc_multi.

run_shift_test_2(ShiftOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_shift_test_3(ShiftOpFunc, Desc, A), Bs, !IO).

:- pred run_shift_test_3((func(uint8, int) = uint8)::in, string::in,
    uint8::in, int::in, io::di, io::uo) is cc_multi.

run_shift_test_3(ShiftOpFunc, Desc, A, B, !IO) :-
    ( try []
        Result0 = ShiftOpFunc(A, B)
    then
        ResultStr = to_binary_string(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s %s %d = %s\n",
        [s(to_binary_string(A)), s(Desc), i(B), s(ResultStr)], !IO).

%---------------------------------------------------------------------------%

:- func numbers = list(uint8).

numbers = [
    uint8.zero,
    uint8.one,
    uint8.two,
    uint8.eight,
    uint8.ten,
    uint8.sixteen,
    uint8.max_uint8
].

:- func shift_amounts = list(int).

shift_amounts = [
    -1,
    0,
    1,
    2,
    3,
    4,
    8,
    9
].

%---------------------------------------------------------------------------%
:- end_module bitwise_uint8.
%---------------------------------------------------------------------------%
