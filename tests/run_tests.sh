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

# TODO:
# - check arith_int16 expected output

TESTS="
    arith_int8
    arith_int16
    arith_int32
    arith_int64
    arith_uint8
    arith_uint16
    arith_uint32
    arith_uint64
    bit_twiddle_int32
    bit_twiddle_int64
    bit_twiddle_uint8
    bit_twiddle_uint16
    bit_twiddle_uint32
    bit_twiddle_uint64
    bitwise_int8
    bitwise_int16
    bitwise_int32
    bitwise_int64
    bitwise_uint8
    bitwise_uint16
    bitwise_uint32
    bitwise_uint64
    cmp_int8
    cmp_int16
    cmp_int32
    cmp_int64
    cmp_uint8
    cmp_uint16
    cmp_uint32
    cmp_uint64
    from_int_int8
    from_int_int16
    from_int_int32
    from_int_int64
    from_int_uint8
    from_int_uint16
    from_int_uint32
    from_int_uint64
    to_string_int8
    to_string_int16
    to_string_int32
    to_string_int64
    to_string_uint8
    to_string_uint16
    to_string_uint32
    to_string_uint64
"

for t in $TESTS
do
    passed="false"
    mmc $grade_opt --no-verbose-make --make $t 2> /dev/null
    if test $? -ne 0
    then
        printf "ERROR building '$t'\n"
    else
        ./$t > $t.out
	for e in $t.exp*
	do
	    if diff -u $e $t.out > $t.diff
	    then
               passed="true"
               break
	    fi
        done
        if test "$passed" = "true"
        then
            printf "PASSED: $t\n"
	    /bin/rm -f $t.out $t.diff
        else
	    printf "FAILED: $t\n"
        fi
    fi
done
