%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test comparison operations for unsigned 16-bit integers.

:- module cmp_uint16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint16.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_cmp_test(unify, "==", !IO),
    io.nl(!IO),
    run_cmp_test(uint16.(<), "<", !IO),
    io.nl(!IO),
    run_cmp_test(uint16.(=<), "=<", !IO),
    io.nl(!IO),
    run_cmp_test(uint16.(>), ">", !IO),
    io.nl(!IO),
    run_cmp_test(uint16.(>=), ">=", !IO).

:- pred run_cmp_test(pred(uint16, uint16)::in(pred(in, in) is semidet),
    string::in, io::di, io::uo) is det.

run_cmp_test(CmpPred, Desc, !IO) :-
    io.format("*** Testing uint16.'%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_cmp_test_2(CmpPred, Desc, Bs), As, !IO).

:- pred run_cmp_test_2(pred(uint16, uint16)::in(pred(in, in) is semidet),
    string::in, list(uint16)::in, uint16::in, io::di, io::uo) is det.

run_cmp_test_2(CmpPred, Desc, Bs, A, !IO) :-
    list.foldl(run_cmp_test_3(CmpPred, Desc, A), Bs, !IO).

:- pred run_cmp_test_3(pred(uint16, uint16)::in(pred(in, in) is semidet), string::in,
    uint16::in, uint16::in, io::di, io::uo) is det.

run_cmp_test_3(CmpPred, Desc, A, B, !IO) :-
    Result = ( if CmpPred(A, B) then "true" else "false" ),
    io.format("%s %s %s = %s\n",
        [s(to_string(A)), s(Desc), s(to_string(B)), s(Result)], !IO).

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

%---------------------------------------------------------------------------%
:- end_module cmp_uint16.
%---------------------------------------------------------------------------%
