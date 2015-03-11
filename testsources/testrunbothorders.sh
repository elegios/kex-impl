#!/bin/bash
function testwith {
	times=3
	echo "Matrix multiplication testrun with $1 x $1 size run $times times."
	ts=$1
	echo "$ts $ts $(seq 1 $(echo "$ts*$ts" | bc)) $ts $ts $(seq 1 $(echo "$ts*$ts" | bc))" > testdata
	echo "In order"
	time for (( i = 0; i < $times; i++ )); do
		cat testdata | testsources/main_io_read 
	done
	echo
	echo
	echo "Out of order"
	time for (( i = 0; i < $times; i++ )); do
		cat testdata | testsources/main_ooo_read 
	done
	rm testdata
}

testwith 1000