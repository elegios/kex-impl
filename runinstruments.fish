function runinstruments -a executable
  make $executable
  rm -rf $executable".trace"
  for i in (seq 3)
    instruments -t L1D.tracetemplate -D $executable".trace" $executable
  end
  open $executable".trace"
end
