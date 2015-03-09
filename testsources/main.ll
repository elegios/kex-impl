; ModuleID = 'main.c'
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.10.0"

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
  br i1 %15, label %.preheader4.lr.ph, label %._crit_edge10

.preheader4.lr.ph:                                ; preds = %6
  %16 = icmp sgt i32 %9, 0
  %17 = icmp sgt i32 %2, 0
  %18 = lshr i64 %A.coerce0, 32
  %19 = add i64 %18, 4294967295
  %20 = and i64 %19, 4294967295
  %21 = add i64 %20, 1
  br label %.preheader4

.preheader4:                                      ; preds = %._crit_edge7, %.preheader4.lr.ph
  %indvars.iv15 = phi i64 [ 0, %.preheader4.lr.ph ], [ %indvars.iv.next16, %._crit_edge7 ]
  br i1 %16, label %.preheader.lr.ph, label %._crit_edge7

.preheader.lr.ph:                                 ; preds = %.preheader4
  %22 = mul i64 %indvars.iv15, %1
  %sext19 = shl i64 %22, 32
  %23 = ashr exact i64 %sext19, 32
  br label %.preheader

.preheader:                                       ; preds = %.preheader._crit_edge, %.preheader.lr.ph
  %indvars.iv11 = phi i64 [ 0, %.preheader.lr.ph ], [ %indvars.iv.next12, %.preheader._crit_edge ]
  br i1 %17, label %.lr.ph.preheader, label %.preheader._crit_edge

.lr.ph.preheader:                                 ; preds = %.preheader
  %end.idx = add i64 %20, 1
  %fold = and i64 %19, 1
  %n.mod.vf = xor i64 %fold, 1
  %n.vec = sub i64 %21, %n.mod.vf
  %cmp.zero = icmp eq i64 %21, %n.mod.vf
  br i1 %cmp.zero, label %middle.block, label %vector.body

vector.body:                                      ; preds = %.lr.ph.preheader, %vector.body
  %index = phi i64 [ %index.next, %vector.body ], [ 0, %.lr.ph.preheader ]
  %vec.phi = phi i32 [ %44, %vector.body ], [ 0, %.lr.ph.preheader ]
  %vec.phi23 = phi i32 [ %45, %vector.body ], [ 0, %.lr.ph.preheader ]
  %induction2225 = or i64 %index, 1
  %24 = add nsw i64 %index, %23
  %25 = add nsw i64 %induction2225, %23
  %26 = getelementptr inbounds i32* %A.coerce1, i64 %24
  %27 = getelementptr inbounds i32* %A.coerce1, i64 %25
  %28 = load i32* %26, align 4, !tbaa !2
  %29 = load i32* %27, align 4, !tbaa !2
  %30 = mul i64 %index, %8
  %31 = mul i64 %induction2225, %8
  %32 = add i64 %30, %indvars.iv11
  %33 = add i64 %31, %indvars.iv11
  %34 = shl i64 %32, 32
  %35 = shl i64 %33, 32
  %36 = ashr exact i64 %34, 32
  %37 = ashr exact i64 %35, 32
  %38 = getelementptr inbounds i32* %B.coerce1, i64 %36
  %39 = getelementptr inbounds i32* %B.coerce1, i64 %37
  %40 = load i32* %38, align 4, !tbaa !2
  %41 = load i32* %39, align 4, !tbaa !2
  %42 = mul nsw i32 %40, %28
  %43 = mul nsw i32 %41, %29
  %44 = add nsw i32 %42, %vec.phi
  %45 = add nsw i32 %43, %vec.phi23
  %index.next = add i64 %index, 2
  %46 = icmp eq i64 %index.next, %n.vec
  br i1 %46, label %middle.block, label %vector.body, !llvm.loop !6

middle.block:                                     ; preds = %vector.body, %.lr.ph.preheader
  %resume.val = phi i64 [ 0, %.lr.ph.preheader ], [ %n.vec, %vector.body ]
  %rdx.vec.exit.phi = phi i32 [ 0, %.lr.ph.preheader ], [ %44, %vector.body ]
  %rdx.vec.exit.phi24 = phi i32 [ 0, %.lr.ph.preheader ], [ %45, %vector.body ]
  %bin.rdx = add i32 %rdx.vec.exit.phi24, %rdx.vec.exit.phi
  %cmp.n = icmp eq i64 %end.idx, %resume.val
  br i1 %cmp.n, label %.preheader._crit_edge, label %.lr.ph

.lr.ph:                                           ; preds = %middle.block, %.lr.ph
  %indvars.iv = phi i64 [ %indvars.iv.next, %.lr.ph ], [ %resume.val, %middle.block ]
  %s.02 = phi i32 [ %56, %.lr.ph ], [ %bin.rdx, %middle.block ]
  %47 = add nsw i64 %indvars.iv, %23
  %48 = getelementptr inbounds i32* %A.coerce1, i64 %47
  %49 = load i32* %48, align 4, !tbaa !2
  %50 = mul i64 %indvars.iv, %8
  %51 = add i64 %50, %indvars.iv11
  %sext21 = shl i64 %51, 32
  %52 = ashr exact i64 %sext21, 32
  %53 = getelementptr inbounds i32* %B.coerce1, i64 %52
  %54 = load i32* %53, align 4, !tbaa !2
  %55 = mul nsw i32 %54, %49
  %56 = add nsw i32 %55, %s.02
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %lftr.wideiv = trunc i64 %indvars.iv.next to i32
  %exitcond = icmp eq i32 %lftr.wideiv, %2
  br i1 %exitcond, label %.preheader._crit_edge, label %.lr.ph, !llvm.loop !9

.preheader._crit_edge:                            ; preds = %.lr.ph, %middle.block, %.preheader
  %s.0.lcssa = phi i32 [ 0, %.preheader ], [ %56, %.lr.ph ], [ %bin.rdx, %middle.block ]
  %57 = mul i64 %indvars.iv11, %8
  %58 = add i64 %57, %indvars.iv15
  %sext20 = shl i64 %58, 32
  %59 = ashr exact i64 %sext20, 32
  %60 = getelementptr inbounds i32* %14, i64 %59
  store i32 %s.0.lcssa, i32* %60, align 4, !tbaa !2
  %indvars.iv.next12 = add nuw nsw i64 %indvars.iv11, 1
  %lftr.wideiv13 = trunc i64 %indvars.iv.next12 to i32
  %exitcond14 = icmp eq i32 %lftr.wideiv13, %9
  br i1 %exitcond14, label %._crit_edge7, label %.preheader

._crit_edge7:                                     ; preds = %.preheader._crit_edge, %.preheader4
  %indvars.iv.next16 = add nuw nsw i64 %indvars.iv15, 1
  %lftr.wideiv17 = trunc i64 %indvars.iv.next16 to i32
  %exitcond18 = icmp eq i32 %lftr.wideiv17, %7
  br i1 %exitcond18, label %._crit_edge10, label %.preheader4

._crit_edge10:                                    ; preds = %._crit_edge7, %6
  %61 = shl nuw i64 %8, 32
  %62 = and i64 %A.coerce0, 4294967295
  %63 = or i64 %61, %62
  %64 = insertvalue { i64, i32* } undef, i64 %63, 0
  %65 = insertvalue { i64, i32* } %64, i32* %14, 1
  ret { i64, i32* } %65
}

; Function Attrs: noreturn
declare void @__assert_rtn(i8*, i8*, i32, i8*) #1

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #2

; Function Attrs: nounwind ssp uwtable
define i32 @main() #0 {
.preheader4.i:
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
