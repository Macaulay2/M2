-- globalBFunction
clearAll()
load "Dloadfile.m2"
Dtrace 1
pInfo(1, "testing globalBFunction...")

scan({IntRing, TryGeneric, NonGeneric}, str->(
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
	  









