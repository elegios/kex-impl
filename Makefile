clang ?= clang
compile = $(clang)++ -O3

runtests: testsources/main_io_read testsources/main_ooo_read testsources/testrunbothorders.sh
	./testsources/testrunbothorders.sh

testsources/main_io_read: testsources/main_io_read.cpp
	$(compile) testsources/main_io_read.cpp -o testsources/main_io_read

testsources/main_ooo_read: testsources/main_ooo_read.cpp
	$(compile) testsources/main_ooo_read.cpp -o testsources/main_ooo_read