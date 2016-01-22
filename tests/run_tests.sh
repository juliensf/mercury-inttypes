#!/bin/sh

/bin/rm -f Mercury.modules
mmc -f *.m ../src/*.m
touch *.m

if test "$GRADE" != ""
then
    grade_opt="--grade $GRADE"
else
    grade_opt=
fi

TESTS="
    arith_int32
    arith_int64
    arith_uint32
    arith_uint64
    bitwise_int32
    bitwise_int64
    bitwise_uint32
    bitwise_uint64
    cmp_int32
    cmp_int64
    cmp_uint32
    cmp_uint64
    to_string_int32
    to_string_int64
    to_string_uint32
    to_string_uint64
"

for t in $TESTS
do
    mmc $grade_opt --no-verbose-make --make $t 2> /dev/null
    if test $? -ne 0
    then
        printf "ERROR building '$t'\n"
    else
        ./$t > $t.out
        if diff -u $t.exp $t.out > $t.diff
        then
           printf "PASSED: $t\n"
	   /bin/rm -f $t.out $t.diff
        else
           printf "FAILED: $t\n"
        fi
    fi
done
