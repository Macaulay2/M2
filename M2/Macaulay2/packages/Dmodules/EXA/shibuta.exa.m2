-- Note especially Ex. 5.10. Shibuta couldn't compute one of the multiplier ideals. Can we?
--
---------------------
--Shibuta's examples:
---------------------
restart; 
loadPackage "Dmodules";

--Ex 5.1
--(i) 
--general
--(ii) 
R = QQ[x_1..x_6];
I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, x_5, x_6}}) 
F = I_*;
b = {1_W,x_1,x_2} / (g->time print factorBFunction generalB (F,g))
///
(s + 2)(s + 3)
     -- used 0.1473 seconds
(s + 2)(s + 4)
     -- used 1.19142 seconds
(s + 2)(s + 4)
     -- used 1.83544 seconds
///

--Ex 5.2
--(i)
R = QQ[x,y];
F = {x^2+y^3};
b = {1_W,x,y} / (g->time print factorBFunction generalB (F,g))
///
            5      7
(s + 1)(s + -)(s + -)
            6      6
     -- used 0.03304 seconds
            11      13
(s + 1)(s + --)(s + --)
             6       6
     -- used 0.028286 seconds
            7      11
(s + 1)(s + -)(s + --)
            6       6
     -- used 0.069248 seconds
///

--(ii) 
F = {x^2,y^3};
b = {1_W,x,y} / (g->time print factorBFunction generalB (F,g))
///
            3      4      5      5      7
(s + 2)(s + -)(s + -)(s + -)(s + -)(s + -)
            2      3      3      6      6
     -- used 0.049639 seconds
            5      4      5      11      13
(s + 2)(s + -)(s + -)(s + -)(s + --)(s + --)
            2      3      3       6       6
     -- used 0.087164 seconds
            3      5      7      7      11
(s + 2)(s + -)(s + -)(s + -)(s + -)(s + --)
            2      3      3      6       6
     -- used 0.051936 seconds
///

--(iii)
R = QQ[x_1..x_4];
F = {x_3*x_1^2 + x_4*x_2^3};
b = {1_W,x_1,x_2} / (g->time print factorBFunction generalB (F,g))
///
                   3      4      5      5      7
(s + 1)(s + 2)(s + -)(s + -)(s + -)(s + -)(s + -)
                   2      3      3      6      6
     -- used 0.111047 seconds
                   5      4      5      11      13
(s + 1)(s + 2)(s + -)(s + -)(s + -)(s + --)(s + --)
                   2      3      3       6       6
     -- used 0.09709 seconds
                   3      5      7      7      11
(s + 1)(s + 2)(s + -)(s + -)(s + -)(s + -)(s + --)
                   2      3      3      6       6
     -- used 0.133331 seconds
///

--Ex 5.3 --not yet verified
R = QQ[x,y];
F = {(x+y)^2-(x-y)^5};
b = {1_W,x,y,x+y,x*y} / (g->time print factorBFunction generalB (F,g))
time multiplierIdeal(F,7/10)
time multiplierIdeal(F,9/10)
time multiplierIdeal(F,1)

--Ex 5.4 --not yet verified
R = QQ[x,y];
F = {x*y*(x+y)*(x+2*y)};
b = {1_W,x,y,x^2,y^2} / (g->time print factorBFunction generalB (F,g))
b = {1_W} / (g->time print factorBFunction generalB (F,g,Exponent=>2))
time multiplierIdeal(F,1/4)
time multiplierIdeal(F,1/2)
time multiplierIdeal(F,3/4)

--Ex 5.5
R = QQ[x_1..x_3];
F = {x_2^2 - x_1*x_3, x_1^3 - x_3^2};
b = {1_W,x_1,x_2,x_3,x_1^2,x_2^2} / (g->time print factorBFunction generalB (F,g))
///
            3      7      9      11      13      17      19      23      25
(s + 2)(s + -)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4      4       6       6      12      12      12      12
     -- used 4.06974 seconds
            5      7      9      13      17      23      25      29      31
(s + 2)(s + -)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4      4       6       6      12      12      12      12
     -- used 23.2604 seconds
            5      9      11      11      13      29      31      35      37
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4       4       6       6      12      12      12      12
     -- used 25.1562 seconds
            5      9      11      17      19      23      25      29      31
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4       4       6       6      12      12      12      12
     -- used 24.6325 seconds
            5      9      11      17      19      25      29      31      35
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4       4       6       6      12      12      12      12
     -- used 208.295 seconds
            5      9      11      17      19      29      31      35      37
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            2      4       4       6       6      12      12      12      12
     -- used 48.1005 seconds
///
time print multiplierIdeal(ideal F,1)
///
{ideal 1}
-- used 15.3632 seconds
///
time print multiplierIdeal(ideal F,17/12)
///
{ideal (x , x , x )}
         3   2   1
     -- used 14.7852 seconds
///
time print multiplierIdeal(ideal F,7/4)
///
                 2
{ideal (x , x , x )}
         3   2   1
     -- used 14.9735 seconds
///
time print multiplierIdeal(ideal F,11/6)
///
             2         2
{ideal (x , x , x x , x )}
         3   2   1 2   1
     -- used 14.146 seconds
///
time print multiplierIdeal(ideal F,23/12)
///
         2               2         2
{ideal (x , x x , x x , x , x x , x )}
         3   2 3   1 3   2   1 2   1
     -- used 14.4837 seconds
///
-----********Didn't check the q_i's.

--Ex 5.6
R = QQ[x_1..x_3];
F = {x_1^3 - x_2^2, x_2^3 - x_3^2};
b = {1_W,x_1,x_2} / (g->time print factorBFunction generalB (F,g))
///
            4      5      11      13      25      29      31      35      37      41
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6      18      18      18      18      18      18
     -- used 0.312574 seconds
            5      7      11      13      17      29      35      37      41      43      49
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6       6      18      18      18      18      18      18
     -- used 0.621569 seconds
///
time multiplierIdeal(ideal F,1) --not yet verified
time multiplierIdeal(ideal F,4/3)
time multiplierIdeal(ideal F,29/18)
time multiplierIdeal(ideal F,31/18)
time multiplierIdeal(ideal F,11/6)
time multiplierIdeal(ideal F,35/8)

--Ex 5.7
R = QQ[x_1..x_3];
F = {x_1^3 - x_2^2, x_3^2 - x_1^2*x_2};
b = {1_W} / (g->time print factorBFunction generalB (F,g))
///
            4      5      11      13      19      23      25      27      29      31
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6      14      14      14      14      14      14
     -- used 2.94278 seconds
///
time multiplierIdeal(ideal F,1) --not yet verified
time multiplierIdeal(ideal F,4/3)
time multiplierIdeal(ideal F,23/14)
time multiplierIdeal(ideal F,25/14)
time multiplierIdeal(ideal F,11/6)
time multiplierIdeal(ideal F,27/14)

--Ex 5.8 --not yet verified
R = QQ[x_1..x_3];
F = {x_1^4 - x_2^3, x_3^2 - x_1*x_2^2};
b = {1_W} / (g->time print factorBFunction generalB (F,g))
time multiplierIdeal(F,1)
time multiplierIdeal(F,9/8)
time multiplierIdeal(F,11/8)
time multiplierIdeal(F,35/24)
time multiplierIdeal(F,19/12)
time multiplierIdeal(F,13/8)
time multiplierIdeal(F,41/24)
time multiplierIdeal(F,43/24)
time multiplierIdeal(F,11/6)
time multiplierIdeal(F,15/8)
time multiplierIdeal(F,23/12)
time multiplierIdeal(F,47/24)

--Ex 5.9 --not yet verified
R = QQ[x_1..x_3];
F = {x_1^2 - x_2*x_3, x_2^2 - x_1*x_3, x_3^2 - x_1*x_2};
b = {1_W,x_1,x_2,x_3} / (g->time print factorBFunction generalB (F,g))
b = {1_W} / (g->time print factorBFunction generalB (F,g,Exponent=>2))
time multiplierIdeal(F,1)
time multiplierIdeal(F,3/2)
time multiplierIdeal(F,2)
time multiplierIdeal(F,5/2)

--Ex 5.10 --not yet verified
R = QQ[x_1..x_3];
F = {x_1^3 - x_2*x_3, x_2^2 - x_1*x_3, x_3^2 - x_1^2*x_2};
b = {1_W} / (g->time print factorBFunction generalB (F,g))
time multiplierIdeal(F,1)
time multiplierIdeal(F,13/9)
time multiplierIdeal(F,16/9)
time multiplierIdeal(F,17/9)
time multiplierIdeal(F,2)
time multiplierIdeal(F,22/9) --*******Shibuta can't do this one. Can we?*******

--Ex 5.11 --not yet verified
R = QQ[x,y,z];
F = {x^3*z^3+y^3*z^3+y^2};
b = {1_W} / (g->time print factorBFunction generalB (F,g))
--Do local/q_i's?

--Ex 5.12 --not yet verified
R = QQ[x,y,z];
F = {x^3-y^2*z, x^2+y^2+z^2-1};
b = {1_W} / (g->time print factorBFunction generalB (F,g))
--Do local/J_f(1)?
time multiplierIdeal(F,1)
time multiplierIdeal(F,11/6)

