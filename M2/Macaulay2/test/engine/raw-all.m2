runM2 = (filename) -> (
     retval := run("M2 --silent --no-debug --stop " |filename|" -e 'exit 0'");
     if retval != 0 then print("ERROR occured in file "|filename);
     )

runM2 "raw.m2" -- some general tests
runM2 "raw-monomial.m2"
runM2 "raw-monoid.m2"
runM2 "raw-ring.m2"
runM2 "raw-freemodule.m2"
runM2 "raw-schreyer.m2"
runM2 "raw-matrix.m2"
runM2 "raw-weyl.m2" -- not much here yet

runM2 "raw-ring2.m2"
runM2 "raw-ring3.m2"

runM2 "raw-monideal.m2"
runM2 "raw-hilb.m2"
runM2 "raw-decompose.m2"
runM2 "raw-gb.m2" -- this file needs to be split into smaller files
runM2 "raw-res.m2"
runM2 "raw-mutable.m2"
runM2 "raw-numerics.m2"
runM2 "raw-localgb.m2"

runM2 "raw3.m2"

runM2 "ring.m2"

-- Local Variables:
-- compile-command: "M2 -e errorDepth=0 --stop -e 'load \"raw-all.m2\"' -e 'exit 0' "
-- End:
