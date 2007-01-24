---------------------------- EXAMPLES for globalBFunction
restart
load "Dloadfile.m2"

--choose a strategy
Str = ReducedB
Str = NonGeneric

-- Example 1
R = QQ[x, dx, WeylAlgebra => {x=>dx}]
n = 10
f = x^n     	    	 
b = globalBFunction(f,Strategy=>Str)
factorBFunction(b)
getIntRoots b
(n^n * b) == ( 
     use ring b;
     s := (ring b)_0;
     product(n, i -> n * (s + 1) - i)       
     )

-- Example 1b
R = frac(QQ[p])[x, dx, WeylAlgebra => {x=>dx}]
n = 10
f = x^n     	    	 
b = globalBFunction f
factorBFunction b
getIntRoots b
(n^n * b) == ( 
     use ring b;
     s := (ring b)_0;
     product(n, i -> n * (s + 1) - i)       
     )

-- Example 2 
R = QQ[x, y, dx, dy, WeylAlgebra => {x=>dx, y=>dy}]
f = x^3 - y^2     	    	 
time apply(20, i->globalBFunction f)
setHomSwitch false 
time apply(20, i->globalBFunction f)
b = globalBFunction f
factorBFunction b
getIntRoots b
36 * b == ( use ring b; s := (ring b)_0; (6 * s + 5) * (s + 1) * (6 * s + 7))


-- Examples for globalB
restart
load "Dloadfile.m2"
Dtrace 666
R = QQ[x, dx, WeylAlgebra => {x=>dx}]
f = x^7 
b = globalBFunction(f, Strategy => ViaAnnFs)
b = globalB(ideal dx, f)
factorBFunction b.Bpolynomial 








