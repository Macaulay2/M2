loadPackage "ExampleIdeals"

runGBExamples = method(Options=>options gb)
runGBExamples (ExampleTable,ZZ) := o -> (H,i) -> (
     answer'gb = null;
     answer'mingens = null;
     test'code = null;
     I := value H#i#1;
     t := timing(G := gens gb I);
     if answer'mingens =!= null then (
	  << "testing mingens" << endl;
	  assert(answer'mingens == mingens I);
	  );
     if answer'gb =!= null then (
	  << "testing gb" << endl;
	  assert(answer'gb == G);
	  );
     if test'code =!= null then (
	  << "testing provided code" << endl;
	  value test'code;
	  );
     answer := {i, H#i#0, char ring I, numgens ring I, numgens I, numgens source G, t#0};
     print answer;
     answer
     )
runGBExamples (ExampleTable,List) := o -> (H,L) -> apply(L,a -> runGBExamples(H,a,o))
runGBExamples ExampleTable := o -> (H) -> runGBExamples(H, sort keys H, o)
viewResults = (L) -> print netList(L, Boxes=>false, HorizontalSpace=>2)

end

restart
load "run-gb-test-examples.m2"
H = getExampleFile("gb-test-examples.m2", "ZZ/32003")
H = getExampleFile("gb-test-examples-2.m2", "ZZ/32003")
L = runGBExamples H
viewResults L

egHaas (ZZ/101)
debug ExampleIdeals
I = Lichtblau (ZZ/32003)
gens gb I
