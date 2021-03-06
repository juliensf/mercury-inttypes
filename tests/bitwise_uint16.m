%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bitwise operations for unsigned 16-bit integers.

:- module bitwise_uint16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint16.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_unop_test(uint16.(\), "\\", !IO),
    io.nl(!IO),
    run_binop_test(uint16.(/\), "/\\", !IO),
    io.nl(!IO),
    run_binop_test(uint16.(\/), "\\/", !IO),
    io.nl(!IO),
    run_binop_test(uint16.(xor), "xor", !IO),
    io.nl(!IO),
    run_shift_test(uint16.(>>), ">>", !IO),
    io.nl(!IO),
    run_shift_test(uint16.(<<), "<<", !IO).

%---------------------------------------------------------------------------%

:- pred run_unop_test((func(uint16) = uint16)::in, string::in,
    io::di, io::uo) is cc_multi.

run_unop_test(UnOpFunc, Desc, !IO) :-
    io.format("*** Test unary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_unop_test_2(UnOpFunc, Desc), As, !IO).

:- pred run_unop_test_2((func(uint16) = uint16)::in, string::in,
    uint16::in, io::di, io::uo) is cc_multi.

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

:- pred run_binop_test((func(uint16, uint16) = uint16)::in, string::in,
    io::di, io::uo) is cc_multi.

run_binop_test(BinOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_binop_test_2(BinOpFunc, Desc, Bs), As, !IO).

:- pred run_binop_test_2((func(uint16, uint16) = uint16)::in, string::in,
    list(uint16)::in, uint16::in, io::di, io::uo) is cc_multi.

run_binop_test_2(BinOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_binop_test_3(BinOpFunc, Desc, A), Bs, !IO).

:- pred run_binop_test_3((func(uint16, uint16) = uint16)::in, string::in,
    uint16::in, uint16::in, io::di, io::uo) is cc_multi.

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

:- pred run_shift_test((func(uint16, int) = uint16)::in, string::in,
    io::di, io::uo) is cc_multi.

run_shift_test(ShiftOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = shift_amounts,
    list.foldl(run_shift_test_2(ShiftOpFunc, Desc, Bs), As, !IO).

:- pred run_shift_test_2((func(uint16, int) = uint16)::in, string::in,
    list(int)::in, uint16::in, io::di, io::uo) is cc_multi.

run_shift_test_2(ShiftOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_shift_test_3(ShiftOpFunc, Desc, A), Bs, !IO).

:- pred run_shift_test_3((func(uint16, int) = uint16)::in, string::in,
    uint16::in, int::in, io::di, io::uo) is cc_multi.

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

:- func numbers = list(uint16).

numbers = [
    uint16.zero,
    uint16.one,
    uint16.two,
    uint16.eight,
    uint16.ten,
    uint16.sixteen,
    uint16.max_uint8,
    uint16.max_uint16
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
    16,
    24,
    32,
    36
].

%---------------------------------------------------------------------------%
:- end_module bitwise_uint16.
%---------------------------------------------------------------------------%
