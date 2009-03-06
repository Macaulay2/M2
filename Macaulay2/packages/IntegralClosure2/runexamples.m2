loadPackage "IntegralClosure2"
readExampleFile = (filename) -> (
     G := separateRegexp("---+", get filename);
     G = apply(G, s -> select(lines s, t -> #t > 0));
     G = select(G, s -> #s > 0);
     new HashTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => () -> demark("\n",drop(G#i,1))))
     )
runExamples = method()
runExamples (HashTable,ZZ) := (H,i) -> (
     R := value H#i#1();
     t := timing (R' = integralClosure2 R);
     answer := {H#i#0, t#0};
     print answer;
     answer
     )
runExamples (HashTable,List) := (H,L) -> apply(L,a -> runExamples(H,a))
runExamples HashTable := (H) -> runExamples(H, sort keys H)

end
restart
load "runexamples.m2"
H = readExampleFile "examples1.m2"
runExamples H
runExamples(H,1)
netList runExamples(H,{1,2,3,4,5,6,7})

