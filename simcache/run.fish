function run -a cache main
  cabal build simcache
  and "/Users/elegios/Projects/llvm/clang+llvm-3.4.2-x86_64-apple-darwin10.9 2/bin/clang" -g -S -emit-llvm -O3 $main -o preinject.ll
  and sed -i '' -E 's/^.+call void @llvm.dbg.declare/;/g' preinject.ll
  and dist/build/simcache/simcache $cache preinject.ll | sed 's/@printf1/@printf/g' > injected.ll
  and /Users/elegios/Projects/llvm/install-3.4.2/bin/llc -filetype=obj injected.ll
  and /Users/elegios/Projects/llvm/install-3.4.2/bin/llc -filetype=asm injected.ll
  and ld injected.o -lc -arch x86_64 -o injected
  and time ./injected
end
