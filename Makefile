clang ?= clang
compile = $(clang) -O3 -g

clean:
	rm -rf testsources/*.trace
	rm -rf testsources/objects/*.trace
	rm -f injected injected.ll preinject.ll injected.s
	rm -f testsources/by{row,col}by{row,col}
	rm -f testsources/logicalMatrix/by{row,col}by{row,col}
	rm -f testsources/objects/{objects,sane}

# broken atm
runtests: testsources/main_io_read testsources/main_ooo_read testsources/testrunbothorders.sh
	./testsources/testrunbothorders.sh

testsources/byrowbyrow: testsources/byrowbyrow.c
	$(compile) testsources/byrowbyrow.c -o testsources/byrowbyrow
testsources/byrowbycol: testsources/byrowbycol.c
	$(compile) testsources/byrowbycol.c -o testsources/byrowbycol
testsources/bycolbyrow: testsources/bycolbyrow.c
	$(compile) testsources/bycolbyrow.c -o testsources/bycolbyrow
testsources/bycolbycol: testsources/bycolbycol.c
	$(compile) testsources/bycolbycol.c -o testsources/bycolbycol

smallest: testsources/smallest_possible_io.ll

testsources/smallest_possible_io.ll:
	$(clang) -std=c11 -S -emit-llvm testsources/smallest_possible_io.c -o testsources/smallest_possible_io.ll


testsources/logicalMatrix/bycolbycol: testsources/logicalMatrix/bycolbycol.c
	$(compile) testsources/logicalMatrix/bycolbycol.c -o testsources/logicalMatrix/bycolbycol
testsources/logicalMatrix/byrowbyrow: testsources/logicalMatrix/byrowbyrow.c
	$(compile) testsources/logicalMatrix/byrowbyrow.c -o testsources/logicalMatrix/byrowbyrow
testsources/logicalMatrix/byrowbycol: testsources/logicalMatrix/byrowbycol.c
	$(compile) testsources/logicalMatrix/byrowbycol.c -o testsources/logicalMatrix/byrowbycol
testsources/logicalMatrix/bycolbyrow: testsources/logicalMatrix/bycolbyrow.c
	$(compile) testsources/logicalMatrix/bycolbyrow.c -o testsources/logicalMatrix/bycolbyrow

testsources/objects/objects: testsources/objects/objects.c
	$(compile) testsources/objects/objects.c -o testsources/objects/objects

testsources/objects/sane: testsources/objects/sane.c
	$(compile) testsources/objects/sane.c -o testsources/objects/sane
