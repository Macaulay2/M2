-- Note especially Ex. 5.10. Shibuta couldn't compute one of the multiplier ideals. Can we?
--
---------------------
--Shibuta's examples:
---------------------
restart; 
debug loadPackage "Dmodules";

--Ex 5.1
--(i) 
--general
--(ii) 
R = QQ[x_1..x_6];
I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, x_5, x_6}}) 
F = I_*;
time print factorBFunction generalB F
time jumpingCoefficients I
///
                                                                      2 2                2 2   2                                  2                  2    2                  2 2                2 2       2  
o7 = ({2, 3}, {ideal (x x  - x x , x x  - x x , x x  - x x ), ideal (x x  - 2x x x x  + x x , x x x  - x x x x  - x x x x  + x x x , x x x x  - x x x  - x x x  + x x x x , x x  - 2x x x x  + x x , x x x  -
                       3 5    2 6   3 4    1 6   2 4    1 5           3 5     2 3 5 6    2 6   3 4 5    2 3 4 6    1 3 5 6    1 2 6   2 3 4 5    1 3 5    2 4 6    1 2 5 6   3 4     1 3 4 6    1 6   2 3 4  
     --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                            2       2 2                2 2
     x x x x  - x x x x  + x x x , x x  - 2x x x x  + x x )})
      1 3 4 5    1 2 4 6    1 5 6   2 4     1 2 4 5    1 5
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

--Ex 5.3
R = QQ[x,y];
F = {(x+y)^2-(x-y)^5};
b = {1_R,x,y,x+y,x*y} / (g->time print factorBFunction generalB (F,g))
///
             7       9      11      13
(s + 1)(s + --)(s + --)(s + --)(s + --)
            10      10      10      10
     -- used 0.06 seconds
             9      11      13      17
(s + 1)(s + --)(s + --)(s + --)(s + --)
            10      10      10      10
     -- used 1.3 seconds
             9      11      13      17
(s + 1)(s + --)(s + --)(s + --)(s + --)
            10      10      10      10
     -- used 1.21 seconds
            17      19      21      23
(s + 1)(s + --)(s + --)(s + --)(s + --)
            10      10      10      10
     -- used 0.39 seconds
            11      13      17      19
(s + 1)(s + --)(s + --)(s + --)(s + --)
            10      10      10      10
     -- used 25.86 seconds
///
time jumpingCoefficients ideal F
///
///

--Ex 5.4 --not yet verified
R = QQ[x,y];
F = {x*y*(x+y)*(x+2*y)};
b = {1_R,x,y,x^2,y^2} / (g->time print factorBFunction generalB (F,g))
b = {1_R} / (g->time print factorBFunction generalB (F,g,Exponent=>2))
///
       2       2     1      3      5      3      5      7      9
(s + 1) (s + 2) (s + -)(s + -)(s + -)(s + -)(s + -)(s + -)(s + -)
                     2      2      2      4      4      4      4
     -- used 1.92 seconds
///
time jumpingCoefficients ideal F
///
     -- used 0.57 seconds

        1  3                             2        2          3      2 2       3
o19 = ({-, -, 1}, {ideal (y, x), ideal (y , x*y, x ), ideal(x y + 3x y  + 2x*y )})
        2  4
///

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
b = {1_R,x_1,x_2} / (g->time print factorBFunction generalB (F,g))
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
time jumpingCoefficients ideal F
///
     -- used 4.75 seconds

       4  29  31  11  35                                           2               2         2           2               2         3           2               2   2     3           3    2   3    2
o6 = ({-, --, --, --, --, 2}, {ideal (x , x , x ), ideal (x , x , x ), ideal (x , x , x x , x ), ideal (x , x x , x x , x , x x , x ), ideal (x , x x , x x , x , x x , x ), ideal (x  - x , x  - x )})
       3  18  18   6  18               3   2   1           3   2   1           3   2   1 2   1           3   2 3   1 3   2   1 2   1           3   2 3   1 3   2   1 2   1           2    3   1    2
///

--Ex 5.7
R = QQ[x_1..x_3];
F = {x_1^3 - x_2^2, x_3^2 - x_1^2*x_2};
b = {1_R} / (g->time print factorBFunction generalB (F,g))
///
            4      5      11      13      19      23      25      27      29      31
(s + 2)(s + -)(s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
            3      3       6       6      14      14      14      14      14      14
     -- used 2.94278 seconds
///
time jumpingCoefficients ideal F
///
        4  23  25  11  27                                           2               2         2           2               2         2           2               2         3           3      2   2      2   3  
o12 = ({-, --, --, --, --, 2}, {ideal (x , x , x ), ideal (x , x , x ), ideal (x , x , x x , x ), ideal (x , x x , x x , x , x x , x ), ideal (x , x x , x x , x , x x , x ), ideal (x  - x x , x x  - x , x  -
        3  14  14   6  14               3   2   1           3   2   1           3   2   1 2   1           3   2 3   1 3   2   1 2   1           3   2 3   1 3   2   1 2   1           2    1 3   1 2    3   1  
      -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
       2
      x )})
       2
///
--Ex 5.8 --in multiplierIdeals.exa.m2
R = QQ[x_1..x_3];
F = {x_1^4 - x_2^3, x_3^2 - x_1*x_2^2};
b = {1_R} / (g->time print factorBFunction generalB (F,g))
time jumpingCoefficients ideal F -- could not finish

--Ex 5.9 
R = QQ[x_1..x_3];
F = {x_1^2 - x_2*x_3, x_2^2 - x_1*x_3, x_3^2 - x_1*x_2};
b = {1_R,x_1,x_2,x_3} / (g->time print factorBFunction generalB (F,g))
///
       2     3
(s + 2) (s + -)
             2
     -- used 2.82 seconds
       2     5
(s + 2) (s + -)
             2
     -- used 174.67 seconds
       2     5
(s + 2) (s + -)
             2
     -- used 101.8 seconds
       2     5
(s + 2) (s + -)
             2
     -- used 72.66 seconds
///
b = {1_R} / (g->time print factorBFunction generalB (F,g,Exponent=>2))
///
       2       2     3      5
(s + 2) (s + 3) (s + -)(s + -)
                     2      2
     -- used 2029.66 seconds
///
time jumpingCoefficients ideal F -- could not finish
///
Computing generalB(..., Exponent=>2)
     -- used 5021.17 seconds

       3     5                                   2                 2   2                  2        2            3   2        2   3    3 
o6 = ({-, 2, -, 3}, {ideal (x , x , x ), ideal (x  - x x , x x  - x , x  - x x ), ideal (x x  - x x , x x x  - x , x x  - x x , x  - x ,
       2     2               3   2   1           2    1 3   1 2    3   1    2 3           2 3    1 3   1 2 3    3   1 3    2 3   2    3 
     ---------------------------------------------------------------------------------------------------------------------------------------
        2      2   2        2   3    3           3      3           2    4   4       2      2 2     3    2        2 2      3   2 2         2
     x x  - x x , x x  - x x , x  - x ), ideal (x x  + x x  - 3x x x  + x , x  - 2x x x  + x x , x x  - x x x  - x x  + x x , x x  - 2x x x 
      1 2    2 3   1 2    1 3   1    3           1 3    2 3     1 2 3    3   2     1 2 3    1 3   1 2    1 2 3    2 3    1 3   1 2     1 2 3
     ---------------------------------------------------------------------------------------------------------------------------------------
        4   3        2      2 2      3   4     2        2 2
     + x , x x  - x x x  - x x  + x x , x  - 2x x x  + x x )})
        3   1 2    1 2 3    1 3    2 3   1     1 2 3    2 3
///

--Ex 5.10 --not yet verified
R = QQ[x_1..x_3];
F = {x_1^3 - x_2*x_3, x_2^2 - x_1*x_3, x_3^2 - x_1^2*x_2};
time factorBFunction generalB F
///
     -- used 21613.5 seconds

            2     3      13      14      16      17      19      20
o6 = (s + 2) (s + -)(s + --)(s + --)(s + --)(s + --)(s + --)(s + --)
                  2       9       9       9       9       9       9
///
time jumpingCoefficients ideal F
time multiplierIdeal(F,1)
time multiplierIdeal(F,13/9)
time multiplierIdeal(F,16/9)
time multiplierIdeal(F,17/9)
time multiplierIdeal(F,2)
time multiplierIdeal(F,22/9) --*******Shibuta can't do this one. Can we?*******

--Ex 5.11 --not yet verified
R = QQ[x,y,z];
F = {x^3*z^3+y^3*z^3+y^2};
time print factorBFunction generalB F
///
            3      5 2     7 2
(s + 1)(s + -)(s + -) (s + -)
            2      6       6
     -- used 0.46 seconds
///

--Ex 5.12 --not yet verified
R = QQ[x,y,z];
F = {x^3-y^2*z, x^2+y^2+z^2-1};
time print factorBFunction generalB F
--Do local/J_f(1)?
time jumpingCoefficients ideal F
