%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test comparison operations for unsigned 8-bit integers.

:- module cmp_uint8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint8.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_cmp_test(unify, "==", !IO),
    io.nl(!IO),
    run_cmp_test(uint8.(<), "<", !IO),
    io.nl(!IO),
    run_cmp_test(uint8.(=<), "=<", !IO),
    io.nl(!IO),
    run_cmp_test(uint8.(>), ">", !IO),
    io.nl(!IO),
    run_cmp_test(uint8.(>=), ">=", !IO).

:- pred run_cmp_test(pred(uint8, uint8)::in(pred(in, in) is semidet),
    string::in, io::di, io::uo) is det.

run_cmp_test(CmpPred, Desc, !IO) :-
    io.format("*** Testing uint8.'%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_cmp_test_2(CmpPred, Desc, Bs), As, !IO).

:- pred run_cmp_test_2(pred(uint8, uint8)::in(pred(in, in) is semidet),
    string::in, list(uint8)::in, uint8::in, io::di, io::uo) is det.

run_cmp_test_2(CmpPred, Desc, Bs, A, !IO) :-
    list.foldl(run_cmp_test_3(CmpPred, Desc, A), Bs, !IO).

:- pred run_cmp_test_3(pred(uint8, uint8)::in(pred(in, in) is semidet), string::in,
    uint8::in, uint8::in, io::di, io::uo) is det.

run_cmp_test_3(CmpPred, Desc, A, B, !IO) :-
    Result = ( if CmpPred(A, B) then "true" else "false" ),
    io.format("%s %s %s = %s\n",
        [s(to_string(A)), s(Desc), s(to_string(B)), s(Result)], !IO).

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

%---------------------------------------------------------------------------%
:- end_module cmp_uint8.
%---------------------------------------------------------------------------%
