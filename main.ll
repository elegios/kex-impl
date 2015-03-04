; ModuleID = 'main.c'
; target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
; target triple = "x86_64-apple-macosx10.10.0"

@__func__.multiply = private unnamed_addr constant [9 x i8] c"multiply\00", align 1
@.str = private unnamed_addr constant [7 x i8] c"main.c\00", align 1
@.str1 = private unnamed_addr constant [19 x i8] c"A.ncols == B.nrows\00", align 1
@.str2 = private unnamed_addr constant [19 x i8] c"Dimentions: %d %d\0A\00", align 1
@.str3 = private unnamed_addr constant [4 x i8] c"%d\09\00", align 1

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
  %9 = trunc i64 %8 to i32
  %sext = shl i64 %A.coerce0, 32
  %10 = ashr exact i64 %sext, 30
  %11 = ashr i64 %B.coerce0, 32
  %12 = mul i64 %11, %10
  %13 = tail call i8* @malloc(i64 %12) #3
  %14 = bitcast i8* %13 to i32*
  %15 = icmp sgt i32 %7, 0
  br i1 %15, label %.preheader4.lr.ph, label %._crit_edge11

.preheader4.lr.ph:                                ; preds = %6
  %16 = icmp sgt i32 %9, 0
  %17 = icmp sgt i32 %2, 0
  br i1 %16, label %.preheader.lr.ph.us.preheader, label %._crit_edge11

.preheader.lr.ph.us.preheader:                    ; preds = %.preheader4.lr.ph
  %18 = lshr i64 %A.coerce0, 32
  %19 = add i64 %18, 4294967295
  %20 = and i64 %19, 4294967295
  %21 = add i64 %20, 1
  br label %.preheader.lr.ph.us

._crit_edge7.us-lcssa.us22:                       ; preds = %26, %.preheader
  %indvars.iv.next32 = add nuw nsw i64 %indvars.iv31, 1
  %lftr.wideiv33 = trunc i64 %indvars.iv.next32 to i32
  %exitcond34 = icmp eq i32 %lftr.wideiv33, %7
  br i1 %exitcond34, label %._crit_edge11, label %.preheader.lr.ph.us

.preheader:                                       ; preds = %.preheader.lr.ph.us, %.preheader
  %indvars.iv = phi i64 [ %indvars.iv.next, %.preheader ], [ 0, %.preheader.lr.ph.us ]
  %22 = mul i64 %indvars.iv, %8
  %23 = add i64 %22, %indvars.iv31
  %sext35 = shl i64 %23, 32
  %24 = ashr exact i64 %sext35, 32
  %25 = getelementptr inbounds i32* %14, i64 %24
  store i32 0, i32* %25, align 4, !tbaa !2
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %9
  br i1 %exitcond, label %._crit_edge7.us-lcssa.us22, label %.preheader

.preheader.lr.ph.us:                              ; preds = %.preheader.lr.ph.us.preheader, %._crit_edge7.us-lcssa.us22
  %indvars.iv31 = phi i64 [ %indvars.iv.next32, %._crit_edge7.us-lcssa.us22 ], [ 0, %.preheader.lr.ph.us.preheader ]
  br i1 %17, label %.preheader.lr.ph.split.us.us, label %.preheader

; <label>:26                                      ; preds = %middle.block, %scalar.ph
  %.lcssa = phi i32 [ %63, %scalar.ph ], [ %bin.rdx, %middle.block ]
  %27 = mul i64 %indvars.iv27, %8
  %28 = add i64 %27, %indvars.iv31
  %sext38 = shl i64 %28, 32
  %29 = ashr exact i64 %sext38, 32
  %30 = getelementptr inbounds i32* %14, i64 %29
  store i32 %.lcssa, i32* %30, align 4, !tbaa !2
  %indvars.iv.next28 = add nuw nsw i64 %indvars.iv27, 1
  %lftr.wideiv29 = trunc i64 %indvars.iv.next28 to i32
  %exitcond30 = icmp eq i32 %lftr.wideiv29, %9
  br i1 %exitcond30, label %._crit_edge7.us-lcssa.us22, label %.lr.ph.us.us

.lr.ph.us.us:                                     ; preds = %.preheader.lr.ph.split.us.us, %26
  %indvars.iv27 = phi i64 [ 0, %.preheader.lr.ph.split.us.us ], [ %indvars.iv.next28, %26 ]
  %end.idx = add i64 %20, 1
  %fold = and i64 %19, 1
  %n.mod.vf = xor i64 %fold, 1
  %n.vec = sub i64 %21, %n.mod.vf
  %cmp.zero = icmp eq i64 %21, %n.mod.vf
  br i1 %cmp.zero, label %middle.block, label %vector.body

vector.body:                                      ; preds = %.lr.ph.us.us, %vector.body
  %index = phi i64 [ %index.next, %vector.body ], [ 0, %.lr.ph.us.us ]
  %vec.phi = phi i32 [ %51, %vector.body ], [ 0, %.lr.ph.us.us ]
  %vec.phi41 = phi i32 [ %52, %vector.body ], [ 0, %.lr.ph.us.us ]
  %induction4043 = or i64 %index, 1
  %31 = add nsw i64 %index, %65
  %32 = add nsw i64 %induction4043, %65
  %33 = getelementptr inbounds i32* %A.coerce1, i64 %31
  %34 = getelementptr inbounds i32* %A.coerce1, i64 %32
  %35 = load i32* %33, align 4, !tbaa !2
  %36 = load i32* %34, align 4, !tbaa !2
  %37 = mul i64 %index, %8
  %38 = mul i64 %induction4043, %8
  %39 = add i64 %37, %indvars.iv27
  %40 = add i64 %38, %indvars.iv27
  %41 = shl i64 %39, 32
  %42 = shl i64 %40, 32
  %43 = ashr exact i64 %41, 32
  %44 = ashr exact i64 %42, 32
  %45 = getelementptr inbounds i32* %B.coerce1, i64 %43
  %46 = getelementptr inbounds i32* %B.coerce1, i64 %44
  %47 = load i32* %45, align 4, !tbaa !2
  %48 = load i32* %46, align 4, !tbaa !2
  %49 = mul nsw i32 %47, %35
  %50 = mul nsw i32 %48, %36
  %51 = add nsw i32 %49, %vec.phi
  %52 = add nsw i32 %50, %vec.phi41
  %index.next = add i64 %index, 2
  %53 = icmp eq i64 %index.next, %n.vec
  br i1 %53, label %middle.block, label %vector.body, !llvm.loop !6

middle.block:                                     ; preds = %vector.body, %.lr.ph.us.us
  %resume.val = phi i64 [ 0, %.lr.ph.us.us ], [ %n.vec, %vector.body ]
  %rdx.vec.exit.phi = phi i32 [ 0, %.lr.ph.us.us ], [ %51, %vector.body ]
  %rdx.vec.exit.phi42 = phi i32 [ 0, %.lr.ph.us.us ], [ %52, %vector.body ]
  %bin.rdx = add i32 %rdx.vec.exit.phi42, %rdx.vec.exit.phi
  %cmp.n = icmp eq i64 %end.idx, %resume.val
  br i1 %cmp.n, label %26, label %scalar.ph

scalar.ph:                                        ; preds = %middle.block, %scalar.ph
  %indvars.iv23 = phi i64 [ %indvars.iv.next24, %scalar.ph ], [ %resume.val, %middle.block ]
  %s.02.us.us = phi i32 [ %63, %scalar.ph ], [ %bin.rdx, %middle.block ]
  %54 = add nsw i64 %indvars.iv23, %65
  %55 = getelementptr inbounds i32* %A.coerce1, i64 %54
  %56 = load i32* %55, align 4, !tbaa !2
  %57 = mul i64 %indvars.iv23, %8
  %58 = add i64 %57, %indvars.iv27
  %sext37 = shl i64 %58, 32
  %59 = ashr exact i64 %sext37, 32
  %60 = getelementptr inbounds i32* %B.coerce1, i64 %59
  %61 = load i32* %60, align 4, !tbaa !2
  %62 = mul nsw i32 %61, %56
  %63 = add nsw i32 %62, %s.02.us.us
  %indvars.iv.next24 = add nuw nsw i64 %indvars.iv23, 1
  %lftr.wideiv25 = trunc i64 %indvars.iv.next24 to i32
  %exitcond26 = icmp eq i32 %lftr.wideiv25, %2
  br i1 %exitcond26, label %26, label %scalar.ph, !llvm.loop !9

.preheader.lr.ph.split.us.us:                     ; preds = %.preheader.lr.ph.us
  %64 = mul i64 %indvars.iv31, %1
  %sext36 = shl i64 %64, 32
  %65 = ashr exact i64 %sext36, 32
  br label %.lr.ph.us.us

._crit_edge11:                                    ; preds = %._crit_edge7.us-lcssa.us22, %.preheader4.lr.ph, %6
  %66 = shl nuw i64 %8, 32
  %67 = and i64 %A.coerce0, 4294967295
  %68 = or i64 %66, %67
  %69 = insertvalue { i64, i32* } undef, i64 %68, 0
  %70 = insertvalue { i64, i32* } %69, i32* %14, 1
  ret { i64, i32* } %70
}

; Function Attrs: noreturn
declare void @__assert_rtn(i8*, i8*, i32, i8*) #1

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #2

; Function Attrs: nounwind ssp uwtable
define i32 @main() #0 {
.preheader.lr.ph.us.i:
  %0 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([19 x i8]* @.str2, i64 0, i64 0), i32 1, i32 1) #3
  %1 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str3, i64 0, i64 0), i32 14) #3
  %putchar1 = tail call i32 @putchar(i32 10) #3
  %putchar = tail call i32 @putchar(i32 10) #3
  ret i32 0
}

; Function Attrs: nounwind
declare i32 @printf(i8* nocapture readonly, ...) #2

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
!6 = metadata !{metadata !6, metadata !7, metadata !8}
!7 = metadata !{metadata !"llvm.vectorizer.width", i32 1}
!8 = metadata !{metadata !"llvm.vectorizer.unroll", i32 1}
!9 = metadata !{metadata !9, metadata !7, metadata !8}
