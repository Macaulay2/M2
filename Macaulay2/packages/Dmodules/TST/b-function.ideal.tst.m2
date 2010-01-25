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
	  
clearAll()
pInfo(1, "testing generalB...")
for str in {InitialIdeal, StarIdeal} do (
     	  pInfo(1, "Strategy=>" | toString str);
	  R = QQ[x_1..x_4];
	  F = {x_3*x_1^2 + x_4*x_2^3};
	  b = {1_R,x_1,x_2} / (g->toString factorBFunction generalB (F,g,Strategy=>str));
	  assert(toString b == "{(s+1)*(s+2)*(s+3/2)*(s+4/3)*(s+5/3)*(s+5/6)*(s+7/6), (s+1)*(s+2)*(s+5/2)*(s+4/3)*(s+5/3)*(s+11/6)*(s+13/6), (s+1)*(s+2)*(s+3/2)*(s+5/3)*(s+7/3)*(s+7/6)*(s+11/6)}")
	  );

assert(toString factorBFunction generalB (F,1_R,Exponent=>2) == "(s+1)*(s+2)^2*(s+3)*(s+3/2)*(s+5/2)*(s+4/3)*(s+5/3)*(s+7/3)*(s+8/3)*(s+5/6)*(s+7/6)*(s+11/6)*(s+13/6)")







