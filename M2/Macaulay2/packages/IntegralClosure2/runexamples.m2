loadPackage "ExampleIdeals"
loadPackage "IntegralClosure2"
readExampleFile1 = (filename) -> (
     G := separateRegexp("---+", get filename);
     G = apply(G, s -> select(lines s, t -> #t > 0));
     G = select(G, s -> #s > 0);
     new HashTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => () -> demark("\n",drop(G#i,1))))
     )
runExamples = method()
runExamples (HashTable,ZZ) := (H,i) -> (
     I := value H#i#1();
     R := (ring I)/I;
     t := timing (R' = integralClosure2 R);
     answer := {i, H#i#0, t#0};
     print answer;
     answer
     )
runExamples (HashTable,List) := (H,L) -> apply(L,a -> runExamples(H,a))
runExamples HashTable := (H) -> runExamples(H, sort keys H)

runSingularIC = method()
runSingularIC Ideal := (I) -> (
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

runSingIC = method()
runSingIC (HashTable,ZZ) := (H,i) -> (
     I := value H#i#1();
     runSingularIC I;
     answer := {i, H#i#0};
     )
runSingIC (HashTable,List) := (H,L) -> apply(L,a -> runSingIC(H,a))
runSingIC HashTable := (H) -> runSingIC(H, sort keys H)

end
restart
load "runexamples.m2"
H = readExampleFile1 "examples1.m2"

-----------------------------------------------------------
-- The following is an idea of where we stand on 3/13/2009
runExamples(H,1) -- {1, leonard1, .67977}
runExamples(H,2) -- {2, vanHoeij1, .397611}
runExamples(H,3) -- bigger
runExamples(H,4) -- bigger
runExamples(H,5) -- bigger
runExamples(H,6) -- {6, boehm1, .199943}
runExamples(H,7) -- {7, boehm2, .224295}
runExamples(H,8) -- {8, boehm3, .705478}
runExamples(H,9) -- {9, boehm4, 8.99051}
runExamples(H,10) -- {10, boehm5, 68.0163}
runExamples(H,11) -- {11,  boehm6, 7.62081} -- hugefractions?
runExamples(H,12) -- bigger
runExamples(H,13) -- long
runExamples(H,14) -- 
runExamples(H,15) -- {15,  boehm10, .202569} -- fractions are not optimal here
runExamples(H,16) -- {16,  boehm11, .761082}
runExamples(H,17) -- {17,  boehm12, 2.38146}
runExamples(H,18) -- bigger?
runExamples(H,19) -- bug: non unit found
runExamples(H,20) -- bigger?
runExamples(H,21) -- bigger?
runExamples(H,22) -- seqfault
runExamples(H,23) -- segfault
runExamples(H,24) -- {24,  boehm19, .42077}
runExamples(H,25) -- bigger
runExamples(H,26) -- bigger

runExamples(H,27) -- {27, singular-huneke, 2.68845}
runExamples(H,28) -- bigger?
runExamples(H,29) -- {29, singular-theo1, .076252}
runExamples(H,30) -- {30, singular-theo1a, .056603}
runExamples(H,31) -- {31, singular-theo2, .179175}
runExamples(H,32) -- {32, singular-theo2a, .152095}
runExamples(H,33) -- {33, singular-theo3, .256082}
runExamples(H,34) -- {34, singular-theo5, .204141}
runExamples(H,35) -- {35, singular-theo6, .099333}
runExamples(H,36) -- {36, singular-sakai1, .193618}
runExamples(H,37) -- {37, singular-sakai2, .206325}
runExamples(H,38) -- {38, singular-sakai3, .014892}
runExamples(H,39) -- {39, singular-sakai4, 4.09353}
runExamples(H,40) -- bigger?
runExamples(H,41) -- {41, singular-sakai6, .405568}
runExamples(H,42) -- bigger?
runExamples(H,43) -- {43, singular-koelman2, 2.19799} -- big nums again
runExamples(H,44) -- {44, singular-unnamed1, .172449}
runExamples(H,45) -- {45, singular-unnamed2, .031908}
runExamples(H,46) -- {46, singular-unnamed3, 1.44704}
runExamples(H,47) -- bus error!!
runExamples(H,48) -- {48, singular-unnamed5, .125091}
-----------------------------------------------------------


runExamples H
runExamples(H,20)
netList runExamples(H,{1,2,3,4,5,6,7})

runSingIC(H,27)
runSingIC(H,28)  -- 3.42
runSingIC(H,29)  -- 0
runSingIC(H,{30,31,32,33,34,35,36,37,38}) -- all easy
runSingIC(H,39)  -- 1
runSingIC(H,40)  -- 0
runSingIC(H,41)  -- 0
runSingIC(H,42)  -- bigger (> few minutes, at least)
runSingIC(H,43)  -- also not immediate...
runSingIC(H,44)  -- 0
runSingIC(H,45)  -- 0
runSingIC(H,46)  -- 1
runSingIC(H,47)  -- bigger
runSingIC(H,48)  -- 0

runSingIC(H,10)  -- these seem to take a while too...
runSingIC(H,25)  -- 
runSingIC(H,26)  -- 

