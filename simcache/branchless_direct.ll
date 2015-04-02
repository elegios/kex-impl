; ModuleID = 'cachestuff'

@simcache = global [8 x i64] zeroinitializer ; means 8 elements in cache
@outputBlub = private unnamed_addr constant [39 x i8] c"Line % 3d, column % 3d: % 20lu % 20lu\0A\00", align 1

declare void @llvm.prefetch(i8*, i32, i32, i32)
; declare void @llvm.prefetch(i8* <address>, i32 <rw>, i32 <locality>, i32 <cache type>)

define fastcc void @__memory_blub(i8* %p, {i64, i64}* %counter) inlinehint {
  %simcachei8 = bitcast [8 x i64]* @simcache to i8*
  %counteri8 = bitcast {i64, i64}* %counter to i8*
  call void @llvm.prefetch(i8* %simcachei8, i32 1, i32 3, i32 1)
  call void @llvm.prefetch(i8* %counteri8, i32 1, i32 2, i32 1)
  %intptr = ptrtoint i8* %p to i64
  %fixed_p = and i64 %intptr, xor (i64 63, i64 -1)       ; means cache-rows of 64 bytes
  %almostIndex = lshr i64 %intptr, 6                     ; means cache-rows of 64 bytes
  %index = and i64 %almostIndex, 7                       ; means 8 elements
  %cachePtr = getelementptr inbounds [8 x i64]* @simcache, i64 0, i64 %index
  %comparePtr = load i64* %cachePtr
  %found = icmp eq i64 %comparePtr, %fixed_p
  %toIOAdd = select i1 %found, i64 1, i64 0
  %toOOOAdd = select i1 %found, i64 0, i64 1
  %newComparePtr = select i1 %found, i64 %comparePtr, i64 %fixed_p
  store i64 %newComparePtr, i64* %cachePtr
  %ioPointer = getelementptr inbounds {i64, i64}* %counter, i64 0, i32 0
  %prevIO = load i64* %ioPointer
  %newIO = add i64 %prevIO, %toIOAdd
  store i64 %newIO, i64* %ioPointer
  %oooPointer = getelementptr inbounds {i64, i64}* %counter, i64 0, i32 1
  %prevOOO = load i64* %oooPointer
  %newOOO = add i64 %prevOOO, %toOOOAdd
  store i64 %newOOO, i64* %oooPointer
  ret void
}

define void @__printSimCacheData(i64 %line, i64 %column, {i64, i64}* %counter) {
  %hitsPointer = getelementptr inbounds {i64, i64}* %counter, i64 0, i32 0
  %hits = load i64* %hitsPointer
  %missesPointer = getelementptr inbounds {i64, i64}* %counter, i64 0, i32 1
  %misses = load i64* %missesPointer
  tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([39 x i8]* @outputBlub, i64 0, i64 0), i64 %line, i64 %column, i64 %hits, i64 %misses)
  ret void
}

declare i32 @printf(i8*, ...) #1

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
