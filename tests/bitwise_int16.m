%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test bitwise operations for signed 16-bit integers.

:- module bitwise_int16.
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
    run_unop_test(int16.(\), "\\", !IO),
    io.nl(!IO),
    run_binop_test(int16.(/\), "/\\", !IO),
    io.nl(!IO),
    run_binop_test(int16.(\/), "\\/", !IO),
    io.nl(!IO),
    run_binop_test(int16.(xor), "xor", !IO),
    io.nl(!IO),
    run_shift_test(int16.(>>), ">>", !IO),
    io.nl(!IO),
    run_shift_test(int16.(<<), "<<", !IO).

%---------------------------------------------------------------------------%

:- pred run_unop_test((func(int16) = int16)::in, string::in,
    io::di, io::uo) is cc_multi.

run_unop_test(UnOpFunc, Desc, !IO) :-
    io.format("*** Test unary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    list.foldl(run_unop_test_2(UnOpFunc, Desc), As, !IO).

:- pred run_unop_test_2((func(int16) = int16)::in, string::in,
    int16::in, io::di, io::uo) is cc_multi.

run_unop_test_2(UnOpFunc, Desc, A, !IO) :-
    ( try []
        Result0 = UnOpFunc(A)
    then
        ResultStr = "0b" ++ to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("%s 0b%s =\n  %s\n",
        [s(Desc), s(to_binary_string_lz(A)), s(ResultStr)], !IO),
    io.nl(!IO).

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
        ResultStr = "0b" ++ to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("0b%s %s\n0b%s =\n%s\n",
        [s(to_binary_string_lz(A)), s(Desc),
        s(to_binary_string_lz(B)), s(ResultStr)], !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred run_shift_test((func(int16, int) = int16)::in, string::in,
    io::di, io::uo) is cc_multi.

run_shift_test(ShiftOpFunc, Desc, !IO) :-
    io.format("*** Test binary operation '%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = shift_amounts,
    list.foldl(run_shift_test_2(ShiftOpFunc, Desc, Bs), As, !IO).

:- pred run_shift_test_2((func(int16, int) = int16)::in, string::in,
    list(int)::in, int16::in, io::di, io::uo) is cc_multi.

run_shift_test_2(ShiftOpFunc, Desc, Bs, A, !IO) :-
    list.foldl(run_shift_test_3(ShiftOpFunc, Desc, A), Bs, !IO).

:- pred run_shift_test_3((func(int16, int) = int16)::in, string::in,
    int16::in, int::in, io::di, io::uo) is cc_multi.

run_shift_test_3(ShiftOpFunc, Desc, A, B, !IO) :-
    ( try []
        Result0 = ShiftOpFunc(A, B)
    then
        ResultStr = "0b" ++ to_binary_string_lz(Result0)
    catch_any _ ->
        ResultStr = "<<exception>>"
    ),
    io.format("0b%s %s %d =\n%s\n",
        [s(to_binary_string_lz(A)), s(Desc), i(B), s(ResultStr)], !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

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

:- func shift_amounts = list(int).

shift_amounts = [
    -1,
    0,
    1,
    2,
    3,
    4,
    8,
    15,
    16,
    17
].

%---------------------------------------------------------------------------%
:- end_module bitwise_int16.
%---------------------------------------------------------------------------%
