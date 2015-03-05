; ModuleID = 'simpletest.c'
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.10.0"

; Function Attrs: nounwind readonly ssp uwtable
define i32 @hundred(i32* nocapture readonly %p) #0 {
  br label %1

; <label>:1                                       ; preds = %0, %1
  %indvars.iv = phi i64 [ 0, %0 ], [ %indvars.iv.next, %1 ]
  %sum.01 = phi i32 [ 0, %0 ], [ %4, %1 ]
  %2 = getelementptr inbounds i32* %p, i64 %indvars.iv
  %3 = load i32* %2, align 4, !tbaa !1
  %4 = add nsw i32 %3, %sum.01
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %5 = icmp slt i32 %4, 100
  br i1 %5, label %1, label %6

; <label>:6                                       ; preds = %1
  %7 = trunc i64 %indvars.iv.next to i32
  ret i32 %7
}

attributes #0 = { nounwind readonly ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Apple LLVM version 6.0 (clang-600.0.56) (based on LLVM 3.5svn)"}
!1 = metadata !{metadata !2, metadata !2, i64 0}
!2 = metadata !{metadata !"int", metadata !3, i64 0}
!3 = metadata !{metadata !"omnipotent char", metadata !4, i64 0}
!4 = metadata !{metadata !"Simple C/C++ TBAA"}
