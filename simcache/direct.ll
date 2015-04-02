; ModuleID = 'cachestuff'

@simcache = global [8 x i64] zeroinitializer ; means 8 elements in cache
@outputBlub = private unnamed_addr constant [39 x i8] c"Line % 3d, column % 3d: % 20lu % 20lu\0A\00", align 1

define fastcc void @__memory_blub(i8* %p, {i64, i64}* %counter) alwaysinline {
  %intptr = ptrtoint i8* %p to i64
  %fixed_p = and i64 %intptr, xor (i64 63, i64 -1)       ; means cache-rows of 64 bytes
  %indexUpshifted = and i64 %fixed_p, 511                ; means 8 elements with cache-rows of 64 bytes
  %index = lshr i64 %indexUpshifted, 6                   ; means cache-rows of 64 bytes
  %cachePtr = getelementptr inbounds [8 x i64]* @simcache, i64 0, i64 %index
  %comparePtr = load i64* %cachePtr
  %found = icmp eq i64 %comparePtr, %fixed_p
  br i1 %found, label %io, label %ooo

io:
  %ioPointer = getelementptr inbounds {i64, i64}* %counter, i64 0, i32 0
  %prevIO = load i64* %ioPointer
  %newIO = add i64 %prevIO, 1
  store i64 %newIO, i64* %ioPointer
  ret void

ooo:
  store i64 %fixed_p, i64* %cachePtr
  %oooPointer = getelementptr inbounds {i64, i64}* %counter, i64 0, i32 1
  %prevOOO = load i64* %oooPointer
  %newOOO = add i64 %prevOOO, 1
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
