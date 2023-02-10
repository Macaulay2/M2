restart
loadPackage "Dmodules"

///
R = QQ[x]
I = ideal x^2;
time (jumps,mI) = jumpingCoefficients I
time multiplierIdeal(I, 1/2, Strategy=>ViaLinearAlgebra, DegreeLimit=>2)
time multiplierIdeal(I, 1/2, Strategy=>ViaColonIdeal, DegreeLimit=>2)
///

R = QQ[x_1..x_3];
I = ideal {x_2^2-x_1*x_3, x_1^3-x_3^2}; 
time sort(bFunctionRoots generalB I_* / minus)
time (jumps,mI) = jumpingCoefficients I
toString oo
///
o8 = {{17/12, 7/4, 11/6, 23/12, 2}, {ideal(x_3,x_2,x_1), ideal(x_3,x_2,x_1^2), ideal(x_3,x_2^2,x_1*x_2,x_1^2),
     ideal(x_3^2,x_2*x_3,x_1*x_3,x_2^2,x_1*x_2,x_1^2), ideal(x_2^2-x_1*x_3,x_1^3-x_3^2)}}
///
assert all(#jumps, i->all( (mI#i)_*,g->isInMultiplierIdeal(g,I,jumps#i) ))
time multiplierIdeal(I, 7/4, Strategy=>ViaLinearAlgebra, DegreeLimit=>10)
time multiplierIdeal(I, 7/4, Strategy=>ViaColonIdeal)
time multiplierIdeal(I, 7/4, Strategy=>ViaElimination)


I = ideal {x_1^3-x_2^2, x_2^3-x_3^2}; --Shibuta Ex 5.6
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
I = ideal{x^5+y^4+x^3*y^2}; --Example 2.5 (not all roots are JCs)
time lct I -- is 9/20
time jumpingCoefficients I
///
ideal(s*y^4+y^4,s*x*y^3+x*y^3,10*s^2*y^3+23*s*y^3+13*y^3,s*x^3*y+2*s*y^3+x^3*y+2*y^3,x^5+x^3*y^2+y^4,5*s*x^4+3*s*x^2*y^2+5*x^4+3*x^2*y^2,20*s^2*x^2*y^2+47*s*x^2*y^2+27*x^2*y^2,400*s^3*x*y^2+1400*s^2*x*y^2+1621*s*x*y^2+621*x*y^2,8000000*s^4*y^2+35600000*s^3*y^2-66000*s^2*x^3-40200*s^2*x*y^2+59020000*s^2*y^2-207*s*x^2*y^2-141300*s*x^3-86190*s*x*y^2+43219000*s*y^2-207*x^2*y^2-75300*x^3-45990*x*y^2+11799000*y^2,40000*s^3*x^3+134000*s^2*x^3-3000*s^2*x*y^2-9*s*x^2*y^2+148600*s*x^3-6690*s*x*y^2-9*x^2*y^2+54600*x^3-3690*x*y^2,2000*s^4*x^2*y+9500*s^3*x^2*y+16840*s^2*x^2*y+13201*s*x^2*y+3861*x^2*y,400000*s^6*x*y+2720000*s^5*x*y+7677000*s^4*x*y+11511100*s^3*x*y+9670490*s^2*x*y+4315617*s*x*y+799227*x*y,4000000000*s^6*x^2+26600000000*s^5*x^2+73350000000*s^4*x^2+107351500000*s^3*x^2-300000*s^3*y^2-85500*s^2*x^3-39600*s^2*x*y^2+87945200000*s^2*x^2-1410000*s^2*y^2+54*s*x^2*y^2-204525*s*x^3-95220*s*x*y^2+38235585000*s*x^2-2061750*s*y^2+54*x^2*y^2-119025*x^3-55620*x*y^2+6891885000*x^2-951750*y^2,1600000000*s^9*y+15200000000*s^8*y+63920000000*s^7*y+156158000000*s^6*y+244221230000*s^5*y+253537070000*s^4*y+174696073000*s^3*y+77028097200*s^2*y+19718105211*s*y+2232241011*y,64000000000*s^10*x+659200000000*s^9*x+3042720000000*s^8*x+8287536000000*s^7*x+14749686000000*s^6*x+17921068200000*s^5*x+15052608563000*s^4*x+8629231520900*s^3*x+3230731189110*s^2*x+713185068843*s*x+70475037633*x,256000000000000*s^13+3072000000000000*s^12+16896000000000000*s^11+56383360000000000*s^10+127342723200000000*s^9+205500504000000000*s^8+243718270400000000*s^7+215017045188000000*s^6+141070385814930000*s^5+67957957559690000*s^4+23356087269375000*s^3+5421098380872600*s^2+761337645337269*s+48839201079669)

o8 = {ideal (10s + 7, x, y), ideal (10s + 13, x, y), ideal (10s + 9, x, y), ideal (10s + 11, x, y), ideal (20s + 9, x, y), ideal (20s + 21, x, y), ideal (20s + 19, x, y), ideal (s +
     --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
         5    3 2    4
     1, x  + x y  + y ), ideal (20s + 13, x, y), ideal (20s + 27, x, y), ideal (20s + 23, x, y), ideal (20s + 11, x, y), ideal (20s + 17, x, y)}

     -- used 143.74 seconds
         9  13   7  17   9  19                                2           2        2           2        3           2   2    3           3     2   2    3          5    3 2    4
o14 = ({--, --, --, --, --, --, 1}, {ideal (y, x), ideal (y, x ), ideal (y , x*y, x ), ideal (y , x*y, x ), ideal (y , x y, x ), ideal (y , x*y , x y, x ), ideal(x  + x y  + y )})
        20  20  10  20  10  20
///
--***also compute the q_i's for this one!***
time multiplierIdeal(I, {9/20,19/20}, Strategy=>ViaLinearAlgebra, DegreeLimit=>4)
time multiplierIdeal(I, 17/20, Strategy=>ViaLinearAlgebra, DegreeLimit=>10)      -- used 73.28 seconds
time multiplierIdeal(I, 17/20, Strategy=>ViaColonIdeal) -- used 84.58 seconds   
time multiplierIdeal(I, 17/20, Strategy=>ViaElimination)  -- used 89.45 seconds    

-----------------------------------------------
---------Example from Saito's papers:----------
-----------------------------------------------
restart
needsPackage "Dmodules"
 --"Mult" 5.5
R = QQ[x,y,z];
I = ideal{(x^2 - y^2)*(x^2 - z^2)*(y^2 - z^2)*z}
time sort(bFunctionRoots generalB I_* / minus)
///
     -- used 29672.1 seconds

       3  4  2  5  6     8  9  4  10  11
o11 = {-, -, -, -, -, 1, -, -, -, --, --}
       7  7  3  7  7     7  7  3   7   7
///

multiplierIdeal(I, {3/7, 4/7, 2/3, 5/7, 6/7})
///
 {ideal(z,y,x), ideal(z^2,y*z,x*z,y^2,x*y,x^2), ideal(y^2*z-z^3,x^2*z-z^3,x*y^2-x*z^2,x^2*y-y*z^2), ideal(y^2*z-z^3,x^2*z-z^3,x*y^2-x*z^2,x^2*y-y*z^2),
             ideal(y^2*z^2-z^4,x^2*z^2-z^4,y^3*z-y*z^3,x*y^2*z-x*z^3,x^2*y*z-y*z^3,x^3*z-x*z^3,x*y^3-x*y*z^2,x^2*y^2-z^4,x^3*y-x*y*z^2)}
///

-----------------------------------------------
-------Examples from Zach's suggestions:-------
-----------------------------------------------
restart
debug loadPackage "Dmodules"
R = QQ[x,y,z];

---- Line arrangements in \CC^3 (from Section 7 of arXiv:0508308v1):

--The first two are 3 non-collinear/collinear points in \PP^2:

--3 non-collinear points
I = intersect(ideal(x-z,y-z),ideal(3*x-z,y-2*z),ideal(y-x,z)); --better
F = flatten entries mingens I;
time print factorBFunction generalB (F,1_R,Strategy=>InitialIdeal)
///
       2     3
(s + 2) (s + -)
             2
     -- used 19.39 seconds
///

Dtrace 1
time toString jumpingCoefficients(ideal F,Strategy=>ViaLinearAlgebra,DegreeLimit=>10)
///
 ({3/2, 2, 5/2},{ideal(z,y,x),
      ideal(3*x*z+2*y*z-5*z^2,3*x*y-3*y^2+10*y*z-10*z^2,9*x^2-9*y^2+35*y*z-35*z^2,y^2*z-3*
      y*z^2+2*z^3), ideal(3*x*z^2+2*y*z^2-5*z^3,y^2*z-3*y*z^2+2*z^3,3*x*y*z+y*z^2-4*z^3,9*
      x^2*z+8*y*z^2-17*z^3,3*x*y^2-3*y^3+20*y*z^2-20*z^3,9*x^2*y-9*y^3+70*y*z^2-70*z^3,27*
      x^3-27*y^3+215*y*z^2-215*z^3)})
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
---------------------------------------------------------------------------
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
time print factorBFunction generalB (F,1_W) --****Note****: If we can compute even this, many people would be very happy!
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
