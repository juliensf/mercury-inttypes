%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of Mercury ints to unsigned 32-bit integers.

:- module from_int_uint32.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint32.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_test, numbers, !IO).

:- pred do_test(string::in, io::di, io::uo) is det.

do_test(IntStr, !IO) :-
    io.format("from_int(%s) = ", [s(IntStr)], !IO),
    ( if
        string.to_int(IntStr, Int),
        uint32.from_int(Int, Int32)
    then
        io.format("%s\n", [s(to_decimal_string(Int32))], !IO)
    else
        io.write_string("<<out-of-range>>\n", !IO)
    ).

:- func numbers = list(string).

numbers = [
    "-9223372036854775808",
    "-2147483648",
    "-32768",
    "-128",
    "0",
    "1",
    "2",
    "8",
    "10",
    "16",
    "127",
    "32767",
    "2147483647",
    "9223372036854775807"
].

%---------------------------------------------------------------------------%
:- end_module from_int_uint32.
%---------------------------------------------------------------------------%
