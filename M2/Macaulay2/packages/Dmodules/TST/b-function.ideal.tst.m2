-- Copyright 1999-2009 by Anton Leykin and Harrison Tsai

needsPackage "Dmodules"
Dtrace 1
pInfo(1, "testing globalBFunction...")

for str in {IntRing, TryGeneric, NonGeneric, GeneralBernsteinSato} do (
     	  print str;
	  x = symbol x; dx = symbol dx; 
	  R = QQ[x, dx, WeylAlgebra => {x=>dx}];
	  n = 10;
	  f = x^n;     	    	 
	  b = globalBFunction(f, Strategy => str);
	  assert ((n^n * b) == ( 
		    use ring b;
		    s := (ring b)_0;
		    product(n, i -> n * (s + 1) - i)       
		    ))
	  );
	  









