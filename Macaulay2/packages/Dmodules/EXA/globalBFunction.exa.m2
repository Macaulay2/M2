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

-- Examples for generalB
restart; 
loadPackage "Dmodules";
QQ[x_1..x_6];
I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, x_5, x_6}}) 
F = I_*;

W = makeWA(QQ[x_1..x_3]);
F = {x_2^2-x_1*x_3, x_1^3-x_3^2}; 
b = {1_W,x_1,x_2,x_3} / (g->time print factorBFunction generalB (F,g))
--            3      7      9      11      13      17      19      23      25
--(s + 2)(s + -)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
--            2      4      4       6       6      12      12      12      12
     -- used 6.17 seconds
--            5      7      9      13      17      23      25      29      31
--(s + 2)(s + -)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
--            2      4      4       6       6      12      12      12      12
     -- used 87.28 seconds
--            5      9      11      11      13      29      31      35      37
--(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
--            2      4       4       6       6      12      12      12      12
     -- used 14.62 seconds
--            5      9      11      17      19      23      25      29      31
--(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
--            2      4       4       6       6      12      12      12      12
     -- used 134.75 seconds

W = makeWA(QQ[x_1..x_3]);
F = {x_1^3-x_2^2, x_2^3-x_3^2};
b = {1_W,x_1,x_2,x_3} / (g->time print factorBFunction generalB (F,g))
///
            4      5      11      13      25      29      31      35      37      41
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6      18      18      18      18      18      18
     -- used 2.35 seconds
            5      7      11      13      17      29      35      37      41      43      49
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6       6      18      18      18      18      18      18
     -- used 3.48 seconds
            7      8      13      17      19      31      35      37      41      43      47
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6       6      18      18      18      18      18      18
     -- used 5.4 seconds
            7      8      11      13      17      19      43      47      49      53      55      59
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6       6       6      18      18      18      18      18      18
     -- used 5.06 seconds
///

W = makeWA(QQ[x_1..x_3]);
F = {x_1^4-x_2^3, x_3^2-x_1*x_2^2}; -- 1 finishes in 1959.81 seconds
b = {1_W,x_1,x_2,x_3} / (g->time print factorBFunction generalB (F,g))
///
            3      11      13      9 2     11 2     13 2     15 2     19      23      25      29      29      31      35      37      41      43      47      49
(s + 2)(s + -)(s + --)(s + --)(s + -) (s + --) (s + --) (s + --) (s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2       6       6      8        8        8        8       12      12      12      12      24      24      24      24      24      24      24      24
     -- used 1878.59 seconds

next one takes > 1 day

///

-- b = {1_W,x_1,x_2,x_3} / (g->print factorBFunction generalB (F,g,GuessedRoots=>{-2}))
--time factorBFunction generalB (F,x_2)
--time factorBFunction generalB (F,1_W)







