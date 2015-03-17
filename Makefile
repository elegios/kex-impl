clang ?= clang
compile = $(clang) -O3 -g

runtests: testsources/main_io_read testsources/main_ooo_read testsources/testrunbothorders.sh
	./testsources/testrunbothorders.sh

testsources/main_io_read: testsources/main_io_read.c
	$(compile) testsources/main_io_read.c -o testsources/main_io_read

testsources/main_ooo_read: testsources/main_ooo_read.c
	$(compile) testsources/main_ooo_read.c -o testsources/main_ooo_read

smallest: testsources/smallest_possible_io.ll

testsources/smallest_possible_io.ll:
	$(clang) -O1 -std=c11 -g -S -emit-llvm testsources/smallest_possible_io.c -o testsources/smallest_possible_io.ll

realmisses1:
	cat datastream | ./testsources/main_ooo_read

# Start instruments in between

realmisses2:
	cat testdata > datastream