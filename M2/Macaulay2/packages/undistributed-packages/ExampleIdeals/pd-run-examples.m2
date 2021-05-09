-- You must be in packages/ExampleIdeals to run this...
-- Let's improve that!
loadPackage "ExampleIdeals"
readExampleFile1 = (filename,kk) -> (
     G := separateRegexp("---+", get filename);
     G = apply(G, s -> select(lines s, t -> #t > 0));
     G = select(G, s -> #s > 0);
     new HashTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => demark("\n",drop(G#i,1))))
     )
runExamples = method(Options=>options primaryDecomposition)
runExamples (HashTable,ZZ) := o -> (H,i) -> (
     I := value H#i#1;
     t := timing (C = primaryDecomposition(I, o));
     answer := {i, t#0, H#i#0, char ring I, numgens ring I, numgens I, #C, C/codim//sort//runLengthEncode};
     print answer;
     answer
     )
runExamples (HashTable,List) := o -> (H,L) -> apply(L,a -> runExamples(H,a,o))
runExamples HashTable := (H) -> o -> runExamples(H, sort keys H, o)

viewResults = (L) -> print netList(L, Boxes=>false, HorizontalSpace=>2)

runSingularPD = method()
runSingularPD Ideal := (I) -> (
     "foo.sing"
     << "rtimer=1;\n"
     << "LIB \"normal.lib\";\n"
     << toSingular ring I << toSingular I
     << "int ti=rtimer;\n"
     << "list nor=normal(I);\n"
     << "int ti2=rtimer-ti;\n"
     << "print(\"time used\"); print(ti2);\n"
     << "exit(ti2);\n" << close;
     run "/sw/bin/singular <foo.sing"
     )

runSingPD = method()
runSingPD (HashTable,ZZ) := (H,i) -> (
     I := value H#i#1;
     runSingularIC I;
     answer := {i, H#i#0};
     )
runSingPD (HashTable,List) := (H,L) -> apply(L,a -> runSingIC(H,a))
runSingPD HashTable := (H) -> runSingIC(H, sort keys H)

--H = readExampleFile1 "DGP.m2"
--print netList(apply(keys H, h -> {h, H#h#0}), Boxes=>false, HorizontalSpace=>2)

end
restart
kk = ZZ/32003
load "pd-run-examples.m2"
H = readExampleFile1("DGP.m2", ZZ/32003)

viewResults runExamples(H, {1,2,3})
viewResults runExamples(H, {4,5,6})
viewResults runExamples(H, {7,8,9,10,12})
-- 11  katsura5 (DGP, from POSSO): takes awhile...
-- 13  cyclic roots 5 (DGP, from POSSO): takes awhile
viewResults runExamples(H, {14,15,16})
viewResults runExamples(H, {17,18,19,20,21})
viewResults runExamples(H, {22,23,24,25})
-- 26  amrhein (DGP): takes awhile
viewResults runExamples(H, {27,28,29,30})
viewResults runExamples(H, {31,32,33})
-- 34  amrheim2 (DGP): takes awhile
-- 36  parametric curve (not in published DGP): takes awhile

kk = ZZ/3
load "pd-run-examples.m2"
-- 35  huneke2 (not in published DGP) -- over ZZ/3 is real test: takes awhile

viewResults runExamples(H, {35})
viewResults runExamples(H, {36})
