%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test comparison operations for signed 64-bit integers.

:- module cmp_int64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int64.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_cmp_test(unify, "==", !IO),
    io.nl(!IO),
    run_cmp_test(int64.(<), "<", !IO),
    io.nl(!IO),
    run_cmp_test(int64.(=<), "=<", !IO),
    io.nl(!IO),
    run_cmp_test(int64.(>), ">", !IO),
    io.nl(!IO),
    run_cmp_test(int64.(>=), ">=", !IO).

:- pred run_cmp_test(pred(int64, int64)::in(pred(in, in) is semidet),
    string::in, io::di, io::uo) is det.

run_cmp_test(CmpPred, Desc, !IO) :-
    io.format("*** Testing int64.'%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_cmp_test_2(CmpPred, Desc, Bs), As, !IO).

:- pred run_cmp_test_2(pred(int64, int64)::in(pred(in, in) is semidet),
    string::in, list(int64)::in, int64::in, io::di, io::uo) is det.

run_cmp_test_2(CmpPred, Desc, Bs, A, !IO) :-
    list.foldl(run_cmp_test_3(CmpPred, Desc, A), Bs, !IO).

:- pred run_cmp_test_3(pred(int64, int64)::in(pred(in, in) is semidet), string::in,
    int64::in, int64::in, io::di, io::uo) is det.

run_cmp_test_3(CmpPred, Desc, A, B, !IO) :-
    Result = ( if CmpPred(A, B) then "true" else "false" ),
    io.format("%s %s %s = %s\n",
        [s(to_string(A)), s(Desc), s(to_string(B)), s(Result)], !IO).

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
:- end_module cmp_int64.
%---------------------------------------------------------------------------%
