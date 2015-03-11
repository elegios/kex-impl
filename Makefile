clang ?= clang

runlargematrixtest: testsources/main_io_read
	time ./testsources/generatetestdata.sh | ./testsources/main_io_read

main_io_read: testsources/main_io_read.cpp
	$(clang)++ testsources/main_io_read.cpp -o testsources/main_io_read