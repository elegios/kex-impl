#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {
	struct Linenumbers : public FunctionPass {
		static char ID;
		Linenumbers() : FunctionPass(ID) {}

		bool runOnFunction(Function &F) override {
			errs() << "Linenumbers: ";
			errs().write_escaped(F.getName()) << '\n';
			return false;
		}
	};
}

char Linenumbers::ID = 0;
static RegisterPass<Linenumbers> X("Linenumbers", "Linenumbers Pass", false, false);