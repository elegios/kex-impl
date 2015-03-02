; ModuleID = 'main.c'
; target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
; target triple = "x86_64-apple-macosx10.10.0"

@__func__.multiply = private unnamed_addr constant [9 x i8] c"multiply\00", align 1
@.str = private unnamed_addr constant [7 x i8] c"main.c\00", align 1
@.str1 = private unnamed_addr constant [19 x i8] c"A.ncols == B.nrows\00", align 1
@.str2 = private unnamed_addr constant [19 x i8] c"Dimentions: %d %d\0A\00", align 1
@.str3 = private unnamed_addr constant [4 x i8] c"%d\09\00", align 1
@.str4 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1

; Function Attrs: nounwind ssp uwtable
define { i64, i32* } @multiply(i64 %A.coerce0, i32* nocapture readonly %A.coerce1, i64 %B.coerce0, i32* nocapture readonly %B.coerce1) #0 {
  %1 = lshr i64 %A.coerce0, 32
  %2 = trunc i64 %1 to i32
  %3 = trunc i64 %B.coerce0 to i32
  %4 = icmp eq i32 %2, %3
  br i1 %4, label %6, label %5, !prof !1

; <label>:5                                       ; preds = %0
  tail call void @__assert_rtn(i8* getelementptr inbounds ([9 x i8]* @__func__.multiply, i64 0, i64 0), i8* getelementptr inbounds ([7 x i8]* @.str, i64 0, i64 0), i32 12, i8* getelementptr inbounds ([19 x i8]* @.str1, i64 0, i64 0)) #4
  unreachable

; <label>:6                                       ; preds = %0
  %7 = trunc i64 %A.coerce0 to i32
  %8 = lshr i64 %B.coerce0, 32
  %sext = shl i64 %A.coerce0, 32
  %9 = ashr exact i64 %sext, 30
  %10 = ashr i64 %B.coerce0, 32
  %11 = mul i64 %10, %9
  %12 = tail call i8* @malloc(i64 %11) #3
  %13 = bitcast i8* %12 to i32*
  %14 = icmp sgt i32 %7, 0
  br i1 %14, label %.preheader4.lr.ph, label %._crit_edge10

.preheader4.lr.ph:                                ; preds = %6
  %15 = trunc i64 %8 to i32
  %16 = icmp sgt i32 %15, 0
  %17 = icmp sgt i32 %2, 0
  %18 = lshr i64 %A.coerce0, 32
  %19 = trunc i64 %18 to i32
  %20 = lshr i64 %B.coerce0, 32
  %21 = trunc i64 %20 to i32
  %22 = trunc i64 %A.coerce0 to i32
  br label %.preheader4

.preheader4:                                      ; preds = %._crit_edge7, %.preheader4.lr.ph
  %indvars.iv15 = phi i64 [ 0, %.preheader4.lr.ph ], [ %indvars.iv.next16, %._crit_edge7 ]
  br i1 %16, label %.preheader.lr.ph, label %._crit_edge7

.preheader.lr.ph:                                 ; preds = %.preheader4
  %23 = mul i64 %indvars.iv15, %1
  %sext19 = shl i64 %23, 32
  %24 = ashr exact i64 %sext19, 32
  br label %.preheader

.preheader:                                       ; preds = %._crit_edge, %.preheader.lr.ph
  %indvars.iv11 = phi i64 [ 0, %.preheader.lr.ph ], [ %indvars.iv.next12, %._crit_edge ]
  br i1 %17, label %.lr.ph, label %._crit_edge

.lr.ph:                                           ; preds = %.preheader, %.lr.ph
  %indvars.iv = phi i64 [ %indvars.iv.next, %.lr.ph ], [ 0, %.preheader ]
  %s.02 = phi i32 [ %34, %.lr.ph ], [ 0, %.preheader ]
  %25 = add nsw i64 %indvars.iv, %24
  %26 = getelementptr inbounds i32* %A.coerce1, i64 %25
  %27 = load i32* %26, align 4, !tbaa !2
  %28 = mul i64 %indvars.iv, %8
  %29 = add i64 %28, %indvars.iv11
  %sext21 = shl i64 %29, 32
  %30 = ashr exact i64 %sext21, 32
  %31 = getelementptr inbounds i32* %B.coerce1, i64 %30
  %32 = load i32* %31, align 4, !tbaa !2
  %33 = mul nsw i32 %32, %27
  %34 = add nsw i32 %33, %s.02
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %19
  br i1 %exitcond, label %._crit_edge, label %.lr.ph

._crit_edge:                                      ; preds = %.lr.ph, %.preheader
  %s.0.lcssa = phi i32 [ 0, %.preheader ], [ %34, %.lr.ph ]
  %35 = mul i64 %indvars.iv11, %8
  %36 = add i64 %35, %indvars.iv15
  %sext20 = shl i64 %36, 32
  %37 = ashr exact i64 %sext20, 32
  %38 = getelementptr inbounds i32* %13, i64 %37
  store i32 %s.0.lcssa, i32* %38, align 4, !tbaa !2
  %indvars.iv.next12 = add nuw nsw i64 %indvars.iv11, 1
  %lftr.wideiv13 = trunc i64 %indvars.iv.next12 to i32
  %exitcond14 = icmp eq i32 %lftr.wideiv13, %21
  br i1 %exitcond14, label %._crit_edge7, label %.preheader

._crit_edge7:                                     ; preds = %._crit_edge, %.preheader4
  %indvars.iv.next16 = add nuw nsw i64 %indvars.iv15, 1
  %lftr.wideiv17 = trunc i64 %indvars.iv.next16 to i32
  %exitcond18 = icmp eq i32 %lftr.wideiv17, %22
  br i1 %exitcond18, label %._crit_edge10, label %.preheader4

._crit_edge10:                                    ; preds = %._crit_edge7, %6
  %39 = shl nuw i64 %8, 32
  %40 = and i64 %A.coerce0, 4294967295
  %41 = or i64 %39, %40
  %42 = insertvalue { i64, i32* } undef, i64 %41, 0
  %43 = insertvalue { i64, i32* } %42, i32* %13, 1
  ret { i64, i32* } %43
}

; Function Attrs: noreturn
declare void @__assert_rtn(i8*, i8*, i32, i8*) #1

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #2

; Function Attrs: nounwind ssp uwtable
define i32 @main() #0 {
  %a = alloca [9 x i32], align 16
  %b = alloca [9 x i32], align 16
  %1 = bitcast [9 x i32]* %a to i8*
  call void @llvm.lifetime.start(i64 36, i8* %1) #3
  call void @llvm.memset.p0i8.i64(i8* %1, i8 0, i64 36, i32 16, i1 false)
  %2 = getelementptr [9 x i32]* %a, i64 0, i64 0
  store i32 1, i32* %2, align 16
  %3 = getelementptr [9 x i32]* %a, i64 0, i64 1
  store i32 2, i32* %3, align 4
  %4 = getelementptr [9 x i32]* %a, i64 0, i64 2
  store i32 3, i32* %4, align 8
  %5 = getelementptr inbounds [9 x i32]* %a, i64 0, i64 0
  %6 = bitcast [9 x i32]* %b to i8*
  call void @llvm.lifetime.start(i64 36, i8* %6) #3
  call void @llvm.memset.p0i8.i64(i8* %6, i8 0, i64 36, i32 16, i1 false)
  %7 = getelementptr [9 x i32]* %b, i64 0, i64 0
  store i32 1, i32* %7, align 16
  %8 = getelementptr [9 x i32]* %b, i64 0, i64 1
  store i32 2, i32* %8, align 4
  %9 = getelementptr [9 x i32]* %b, i64 0, i64 2
  store i32 3, i32* %9, align 8
  %10 = getelementptr inbounds [9 x i32]* %b, i64 0, i64 0
  %11 = call { i64, i32* } @multiply(i64 12884901889, i32* %5, i64 4294967299, i32* %10)
  %12 = extractvalue { i64, i32* } %11, 0
  %13 = extractvalue { i64, i32* } %11, 1
  %14 = trunc i64 %12 to i32
  %15 = lshr i64 %12, 32
  %16 = trunc i64 %15 to i32
  %17 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([19 x i8]* @.str2, i64 0, i64 0), i32 %14, i32 %16) #3
  %18 = icmp sgt i32 %14, 0
  br i1 %18, label %.preheader.lr.ph, label %._crit_edge4

.preheader.lr.ph:                                 ; preds = %0
  %19 = icmp sgt i32 %16, 0
  %20 = lshr i64 %12, 32
  %21 = trunc i64 %20 to i32
  %22 = trunc i64 %12 to i32
  br label %.preheader

.preheader:                                       ; preds = %._crit_edge, %.preheader.lr.ph
  %indvars.iv5 = phi i64 [ 0, %.preheader.lr.ph ], [ %indvars.iv.next6, %._crit_edge ]
  br i1 %19, label %.lr.ph, label %._crit_edge

.lr.ph:                                           ; preds = %.preheader
  %23 = mul i64 %indvars.iv5, %12
  %sext = shl i64 %23, 32
  %24 = ashr exact i64 %sext, 32
  br label %25

; <label>:25                                      ; preds = %25, %.lr.ph
  %indvars.iv = phi i64 [ 0, %.lr.ph ], [ %indvars.iv.next, %25 ]
  %26 = add nsw i64 %indvars.iv, %24
  %27 = getelementptr inbounds i32* %13, i64 %26
  %28 = load i32* %27, align 4, !tbaa !2
  %29 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str3, i64 0, i64 0), i32 %28) #3
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %21
  br i1 %exitcond, label %._crit_edge, label %25

._crit_edge:                                      ; preds = %25, %.preheader
  %putchar1 = tail call i32 @putchar(i32 10) #3
  %indvars.iv.next6 = add nuw nsw i64 %indvars.iv5, 1
  %lftr.wideiv7 = trunc i64 %indvars.iv.next6 to i32
  %exitcond8 = icmp eq i32 %lftr.wideiv7, %22
  br i1 %exitcond8, label %._crit_edge4, label %.preheader

._crit_edge4:                                     ; preds = %._crit_edge, %0
  %putchar = tail call i32 @putchar(i32 10) #3
  call void @llvm.lifetime.end(i64 36, i8* %6) #3
  call void @llvm.lifetime.end(i64 36, i8* %1) #3
  ret i32 0
}

; Function Attrs: nounwind
declare void @llvm.lifetime.start(i64, i8* nocapture) #3

; Function Attrs: nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture, i8, i64, i32, i1) #3

; Function Attrs: nounwind
declare i32 @printf(i8* nocapture readonly, ...) #2

; Function Attrs: nounwind
declare void @llvm.lifetime.end(i64, i8* nocapture) #3

; Function Attrs: nounwind
declare i32 @putchar(i32) #3

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { noreturn "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }
attributes #4 = { noreturn nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Apple LLVM version 6.0 (clang-600.0.56) (based on LLVM 3.5svn)"}
!1 = metadata !{metadata !"branch_weights", i32 64, i32 4}
!2 = metadata !{metadata !3, metadata !3, i64 0}
!3 = metadata !{metadata !"int", metadata !4, i64 0}
!4 = metadata !{metadata !"omnipotent char", metadata !5, i64 0}
!5 = metadata !{metadata !"Simple C/C++ TBAA"}
