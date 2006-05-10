-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

needs "Dmodules.m2"
Dtrace 1
pInfo(1, "testing globalBFunction...")

scan({IntRing, TryGeneric, NonGeneric}, str->(
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
	  ));
	  









