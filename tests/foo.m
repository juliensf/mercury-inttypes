:- module foo.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int16.

main(!IO) :-
    X = -int16.two,
    S = to_decimal_string(X),
    io.print_line(S, !IO).

