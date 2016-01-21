%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test comparison operations for signed 32-bit integers.

:- module cmp_int32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int32.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    run_cmp_test(unify, "==", !IO),
    io.nl(!IO),
    run_cmp_test(int32.(<), "<", !IO),
    io.nl(!IO),
    run_cmp_test(int32.(=<), "=<", !IO),
    io.nl(!IO),
    run_cmp_test(int32.(>), ">", !IO),
    io.nl(!IO),
    run_cmp_test(int32.(>=), ">=", !IO).

:- pred run_cmp_test(pred(int32, int32)::in(pred(in, in) is semidet),
    string::in, io::di, io::uo) is det.

run_cmp_test(CmpPred, Desc, !IO) :-
    io.format("*** Testing int32.'%s' ***\n\n", [s(Desc)], !IO),
    As = numbers,
    Bs = numbers,
    list.foldl(run_cmp_test_2(CmpPred, Desc, Bs), As, !IO).

:- pred run_cmp_test_2(pred(int32, int32)::in(pred(in, in) is semidet),
    string::in, list(int32)::in, int32::in, io::di, io::uo) is det.

run_cmp_test_2(CmpPred, Desc, Bs, A, !IO) :-
    list.foldl(run_cmp_test_3(CmpPred, Desc, A), Bs, !IO).

:- pred run_cmp_test_3(pred(int32, int32)::in(pred(in, in) is semidet), string::in,
    int32::in, int32::in, io::di, io::uo) is det.

run_cmp_test_3(CmpPred, Desc, A, B, !IO) :-
    Result = ( if CmpPred(A, B) then "true" else "false" ),
    io.format("%s %s %s = %s\n",
        [s(to_string(A)), s(Desc), s(to_string(B)), s(Result)], !IO).

:- func numbers = list(int32).

numbers = [
    int32.min_int32,
    int32.min_int16,
    int32.min_int8,
    int32.zero,
    int32.one,
    int32.two,
    int32.eight,
    int32.ten,
    int32.sixteen,
    int32.max_int8,
    int32.max_int16,
    int32.max_int32
].

%---------------------------------------------------------------------------%
:- end_module cmp_int32.
%---------------------------------------------------------------------------%
