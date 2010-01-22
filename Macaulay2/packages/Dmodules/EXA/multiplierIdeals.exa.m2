restart
debug loadPackage "Dmodules"

R = QQ[x_1..x_3];
I = ideal {x_2^2-x_1*x_3, x_1^3-x_3^2}; 
sort(bFunctionRoots generalB I_* / minus)
(jumps,mI) = jumpingCoefficients I
toString oo
///
o8 = {{17/12, 7/4, 11/6, 23/12, 2}, {ideal(x_3,x_2,x_1), ideal(x_3,x_2,x_1^2), ideal(x_3,x_2^2,x_1*x_2,x_1^2),
     ideal(x_3^2,x_2*x_3,x_1*x_3,x_2^2,x_1*x_2,x_1^2), ideal(x_2^2-x_1*x_3,x_1^3-x_3^2)}}
///
assert all(#jumps, i->all( (mI#i)_*,g->isInMultiplierIdeal(g,I,jumps#i) ))


I = ideal {x_1^3-x_2^2, x_2^3-x_3^2};
time sort(bFunctionRoots generalB I_* / minus)
time (jumps,mI) = jumpingCoefficients I
///
     -- used 3.94 seconds
       4  29  31  11  35                                           2               2         2           2               2         3        
o6 = ({-, --, --, --, --, 2}, {ideal (x , x , x ), ideal (x , x , x ), ideal (x , x , x x , x ), ideal (x , x x , x x , x , x x , x ), ideal
       3  18  18   6  18               3   2   1           3   2   1           3   2   1 2   1           3   2 3   1 3   2   1 2   1        
     ---------------------------------------------------------------------------------------------------------------------------------------
       2               2   2     3           3    2   3    2
     (x , x x , x x , x , x x , x ), ideal (x  - x , x  - x )})
       3   2 3   1 3   2   1 2   1           2    3   1    2
///

I = ideal {x_1^4-x_2^3, x_3^2-x_1*x_2^2};
sort(bFunctionRoots generalB I_* / minus)
(jumps,mI) = jumpingCoefficients I


------
R = QQ[x,y,z];
F = { -y^3*z^3+y^5+x^2*z^3-x^2*y^2 }; --intersect( ideal(x^2-y^3),ideal(y^2-z^3) )
b = scan({1_R,x}, g->time print factorBFunction generalB (F,g,Strategy=>InitialIdeal))
b = scan({1_R,x}, g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
lct ideal F --is 19/30
time multiplierIdeal(ideal F,19/30)
jumpingCoefficients ideal F
-----------------------------------------------
-------------Examples from [ELSV]:-------------
-----------------------------------------------
R = QQ[x,y];
F = {x^5+y^4+x^3*y^2}; --Example 2.5 (not all roots are JCs)
lct ideal F -- is ?
--time multiplierIdeal(ideal F, 9/20)
jumpingCoefficients ideal F
--***also compute the q_i's for this one!***

-----------------------------------------------
---------Example from Saito's papers:----------
-----------------------------------------------
R = QQ[x,y];
I = ideal {(x^2-y^2)*(x^2-1)*(y^2-1)}; --"Mult" 5.5
time sort(bFunctionRoots generalB I_* / minus)
jumpingCoefficients I
///
       2               2       2              4 2    2 4    4    4    2    2
o5 = ({-, 1}, {ideal (y  - 1, x  - 1), ideal(x y  - x y  - x  + y  + x  - y )})
       3
///
decompose oo#1#0 -- nontrivial decomposition

-----
R = QQ[x,y];
F = {}; --"Intro" 7.5 are all monomial ideals
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal))
--lct(ideal F) -- is ?
--time multiplierIdeal(ideal F, )

 

-----------------------------------------------
-------Examples from Zach's suggestions:-------
-----------------------------------------------
restart
debug loadPackage "Dmodules"
R = QQ[x,y,z];

---- Line arrangements in \CC^3 (from Section 7 of arXiv:0508308v1):

--The first two are 3 noncollinear/collinear points in \PP^2:

--3 noncollinear points
I = intersect(ideal(x-z,y-z),ideal(3*x-z,y-2*z),ideal(5*y-x,z)); --better
F = flatten entries mingens I;
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)
///
       2     3
(s + 2) (s + -)
             2
     -- used 19.39 seconds
///

time print factorBFunction generalB (F,1_R,Strategy=>StarIdeal)
lct ideal F --Expect: lct = 3/2
jumpingCoefficients ideal F
///
       3                                               2            2              2    2       2           
o6 = ({-, 2}, {ideal (z, y, x), ideal (3x*z + 2y*z - 5z , 3x*y - 15y  + 46y*z - 34z , 9x  - 225y  + 683y*z -
       2
     ---------------------------------------------------------------------------------------------------------
         2   2        2     3
     467z , y z - 3y*z  + 2z )})
///

--3 collinear points
I = intersect(ideal(y,z),ideal(x-2*z,y-z),ideal(2*x-3*z,y-z));
F = flatten entries mingens I;
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)
time print factorBFunction generalB (F,1_R,Strategy=>StarIdeal)
lct ideal F --Expect: lct = 5/3
time jumpingCoefficients ideal F
///
     -- used 0.84 seconds

        5                                        2        2     3
o12 = ({-, 2}, {ideal (z, y, x), ideal (y - z, 2x z - 7x*z  + 6z )})
        3
///

--The next two are 6 points in general position vs. 6 general points on a conic in \PP^2:
--6 points in general position in \PP^2: 
R = QQ[a,b,c];
small = 3;
setRandomSeed 1;
while true do (
     x = apply(6, i -> random small - small//2);
     y = apply(6, i -> random small - small//2);
     z = apply(6, i -> random 2 + 1);
     --z = toList (6:1);
     xym = matrix {x, y, z};
     I = intersect (entries transpose xym /(l -> ideal {l#2*a-l#0*c, l#2*b-l#1*c}));
     if multiplicity I == 15 and all(z,zz->zz!=0) and all(flatten entries gens minors(3, xym), i -> (i!=0)) then break;
     );
F = flatten entries mingens I;
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)
time print factorBFunction generalB (F,1_R,Strategy=>StarIdeal)
lct ideal F --Expect: lct = 1

--6 general points on a^2-b*c in \PP^2: 
R = QQ[a,b,c];
--x = take(unique apply(101, i -> random (coefficientRing R)), 6);
x = { -2,-1,0,1,2,3 };
y = x/(i -> i^2);
xym = matrix {x, y, toList (6:1)};
all(flatten entries gens minors(3, xym), i -> (i!=0))
I = intersect (entries transpose xym /(l -> ideal {a-l#0*c, b-l#1*c}));
F = flatten entries mingens I;
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)
///
       2     3      4      5      7
(s + 2) (s + -)(s + -)(s + -)(s + -)
             2      3      3      3
///
--Note: lct = 4/3
time jumpingCoefficients ideal F
///
     -- used 5247.5 seconds

        4  5                                2             2        2           2            2    3            
o24 = ({-, -, 2}, {ideal (c, b, a), ideal (c , b*c, a*c, b , a*b, a ), ideal (a  - b*c, 3a*b  - b  - 15a*b*c +
        3  3
      --------------------------------------------------------------------------------------------------------
        2         2       2   4      3       2 2        3
      5b c + 12a*c  - 4b*c , b  - 14b c + 49b c  - 36b*c )})
///

--Zach does not know the multiplier ideals for this one.
--11 general points on a^3-b*c^2 in \PP^2: --(This code is courtesy of Manoj Kummini.)
R = QQ[a,b,c];
--x = take(unique apply(101, i -> random (coefficientRing R)), 11);
x = toList(-5..5);
y = x/(i -> i^3);
xym = matrix {x, y, toList (11:1)};
all(flatten entries gens minors(3, xym), i -> (i!=0))
I = intersect (entries transpose xym /(l -> ideal {a-l#0*c, b-l#1*c}));
F = flatten entries mingens I;
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)

--------------------------------------------------------------
---- Monomial curves:
------------------------------------------------------------------
restart
debug loadPackage "Dmodules"

--A monomial curve in \PP^3: 
n = 4;
R = QQ[x_1..x_4];
A = matrix{{1,1,1,1},{0,1,3,4}}; 
H = gkz(A,{0,0})
phi = map(R,ring H, {0,0,0,0,x_1..x_4});
xIA = ideal mingens phi(H);
F = toList apply(numColumns(gens xIA),i-> (gens xIA)_i_0);
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)
--b = {1_W,x_1,F#0,x_1*x_4,x_1*x_2} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
--use roots to decide which multiplier ideals to compute
analyticSpread (ideal dF)
--multiplierIdeal(ideal F,1_QQ)

--A monomial curve in \PP^4: 
A = matrix{{1,1,1,1,1},{0,1,0,1,0},{0,0,1,1,-2}}; 
H = gkz(A,{0,0,0})
apply(1..5,i-> D_i = (gens(ring H))#(i+4) )
IA = ideal {D_2*D_3-D_1*D_4, D_1*D_2^2-D_4^2*D_5, D_1^2*D_2-D_3*D_4*D_5, D_1^3-D_3^2*D_5};
R = QQ[x_1..x_5];
phi = map(R,ring H, {0,0,0,0,0,x_1..x_5});
xIA = phi(IA); --toString gens xIA
use R;
F = {x_2*x_3-x_1*x_4, x_1*x_2^2-x_4^2*x_5, x_1^2*x_2-x_3*x_4*x_5, x_1^3-x_3^2*x_5};
multiplierIdeal(ideal F,1_QQ)

---------------------------------------------------------------------------
---- The "big diagonal" of (\CC^d)^n: 
------------------------------------------------------------------------------
restart
debug loadPackage "Dmodules"
--Note: It would be great if we had a way to mod out by the "small diagonal" ideal(x_1-x_3,x_1-x_5,x_2-x_4,x_2-x_6).

--n=3, d=2 --This is known (and easily computable by hand).
R = QQ[x_1..x_6];
bigDiag = mingens intersect( ideal(x_1-x_3,x_2-x_4), ideal(x_1-x_5,x_2-x_6), ideal(x_3-x_5,x_4-x_6) );
F = flatten entries bigDiag
time print factorBFunction generalB F
time print factorBFunction generalB (F,1_R,Exponent=>2)
jumpingCoefficients ideal F

--n=4, d=2
R = QQ[x_1..x_4,y_1..y_4];
bigDiag = mingens intersect( ideal(x_1-y_1,x_2-y_2), ideal(x_1-x_3,y_1-y_3), ideal(x_1-x_4,y_1-y_4), ideal(x_2-x_3,y_2-y_3), ideal(x_2-x_4,y_2-y_4), ideal(x_3-x_4,y_3-y_4 ));
F = entries bigDiag_0;
time print factorBFunction generalB F
--time print factorBFunction generalB (F,1_R,Exponent=>2)
jumpingCoefficients ideal F

k = binomial(4,2); multiplierIdeal( ideal F, 1/k ) --This tests a conjecture of Kyungyong.
--multiplierIdeal( ideal xF, 1_QQ )

--Is there any hope of computing n=5?
n=5; d=2;
R = QQ[x_1..x_5,y_1..y_5];
W = makeWeylAlgebra R; 
bigDiag = mingens intersect (flatten toList(apply(1..n,i-> toList(apply(1..(i-1),j-> ideal(x_i-x_j, y_i-y_j))))))
F = entries bigDiag_0;
time print factorBFunction generalB (F,1_W) --****Note****: If we can compute this, many people would be very happy!
--time print factorBFunction generalB (F,1_W,Exponent=>2)
--b = {1_W,x_1,x_1^2,x_1*x_2,x_1*x_2*x_3} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
xF = apply(F,h->sub(h,R)) --analyticSpread(ideal xF) --1
k = binomial(4,2); multiplierIdeal( ideal xF, 1/k ) --This tests a conjecture of Kyungyong.
--multiplierIdeal( ideal xF, 1_QQ )

------------------------------------------------------------
-- Below are the most notable comparisons I've looked at to compare the InitialIdeal and StarIdeal Strategies.
-- When one is faster, it's typically InitialIdeal, but they're usually fairly similar.

-- Are these comparisons significant?
R = QQ[x,y,z,w];
F = {x^2+y^2+z^2+w^2};
b = {1_W,x,y,z,w} / (g->time print factorBFunction generalB (F,g,Strategy=>InitialIdeal))
///
(s + 1)(s + 2)
     -- used 0.085398 seconds
(s + 1)(s + 3)
     -- used 0.045261 seconds
(s + 1)(s + 3)
     -- used 0.041292 seconds
(s + 1)(s + 3)
     -- used 0.042321 seconds
(s + 1)(s + 3)
     -- used 0.086999 seconds
///

b = {1_W,x,y,z,w} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
///
(s + 1)(s + 2)
     -- used 0.039125 seconds
(s + 1)(s + 3)
     -- used 0.039227 seconds
(s + 1)(s + 3)
     -- used 0.079084 seconds
(s + 1)(s + 3)
     -- used 0.038432 seconds
(s + 1)(s + 3)
     -- used 0.03973 seconds
///


Ex 5.5
W = makeWeylAlgebra(QQ[x_1..x_3]);
F = {x_2^2 - x_1*x_3, x_1^3 - x_3^2};
b = {1_W,x_1,x_2,x_3} / (g->time print factorBFunction generalB (F,g,Strategy=>InitialIdeal))
///
            3      7      9      11      13      17      19      23      25
(s + 2)(s + -)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4      4       6       6      12      12      12      12
     -- used 0.863465 seconds
            5      7      9      13      17      23      25      29      31
(s + 2)(s + -)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4      4       6       6      12      12      12      12
     -- used 14.1046 seconds
            5      9      11      11      13      29      31      35      37
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4       4       6       6      12      12      12      12
     -- used 2.36621 seconds
            5      9      11      17      19      23      25      29      31
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4       4       6       6      12      12      12      12
     -- used 20.3367 seconds
///

b = {1_W,x_1,x_2,x_3} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
///
            3      7      9      11      13      17      19      23      25
(s + 2)(s + -)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4      4       6       6      12      12      12      12
     -- used 3.13178 seconds
            5      7      9      13      17      23      25      29      31
(s + 2)(s + -)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4      4       6       6      12      12      12      12
     -- used 19.8134 seconds
            5      9      11      11      13      29      31      35      37
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4       4       6       6      12      12      12      12
     -- used 23.4829 seconds
            5      9      11      17      19      23      25      29      31
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4       4       6       6      12      12      12      12
     -- used 24.1593 seconds
///


-- Here, InitialIdeal is faster. This is the most significant difference between them that I found.
-- Shibuta Ex 5.6
W = makeWeylAlgebra(QQ[x_1..x_3]);
F = {x_1^3 - x_2^2, x_2^3 - x_3^2};
b = {1_W,x_1,x_2,x_3} / (g->time print factorBFunction generalB (F,g,Strategy=>InitialIdeal))
///
             4      5      11      13      25      29      31      35      37      41
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6      18      18      18      18      18      18
     -- used 0.312574 seconds
            5      7      11      13      17      29      35      37      41      43      49
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6       6      18      18      18      18      18      18
     -- used 0.621569 seconds
            7      8      13      17      19      31      35      37      41      43      47
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6       6      18      18      18      18      18      18
     -- used 0.816695 seconds
            7      8      11      13      17      19      43      47      49      53      55      59
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6       6       6      18      18      18      18      18      18
     -- used 0.765971 seconds
///

b = {1_W,x_1,x_2,x_3} / (g->time print factorBFunction generalB (F,g,Strategy=>StarIdeal))
///
           4      5      11      13      25      29      31      35      37      41
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6      18      18      18      18      18      18
     -- used 0.72619 seconds
            5      7      11      13      17      29      35      37      41      43      49
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6       6      18      18      18      18      18      18
     -- used 2.40067 seconds
            7      8      13      17      19      31      35      37      41      43      47
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6       6      18      18      18      18      18      18
     -- used 3.24806 seconds
            7      8      11      13      17      19      43      47      49      53      55      59
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6       6       6      18      18      18      18      18      18
     -- used 29.5326 seconds
///
