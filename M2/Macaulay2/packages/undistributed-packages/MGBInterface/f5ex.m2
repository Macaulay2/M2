-- Taken from M2-dev/mike/sig-gb/test-code/m2/f5ex.m2, 12 April 2013.
if instance(toABC, Symbol) then
toABC = (I) -> (
     R := ring I;
     S := (coefficientRing R)[vars(0..numgens R-1), MonomialOrder=>(monoid ring I).Options.MonomialOrder];
     sub(I, vars S)
     )

-*
LIBRARY: f5ex.lib: Examples of ideals for Groebner basis computations.

AUTHOR: Christian Eder, ederc@mathematik.uni-kl.de

NOTE: The library only includes procedures which produces examples of ideals. In the recent version these examples
      are computed to fit into the behaviour of the f5_library.lib, such that we are working with lists and not ideals.
      Each procedure exports the needed basering and the ideal i which is equivalent to the ideal we want to compute a
      Groebner basis of. There is no data returned at all.

PROCEDURES:

   [A] all()                          this procedure computes all examples of this library with the F5 algorithm using
                                      \"f5_library.lib\" and prints a hopefully nice output on the screen.

   [B] buchberger87()                 characteristic: 7583   variables: h,r,t,x,y,z                            regular? no
       bug_stas()		              characteristic: 2^4(a) variables: V(1),U(15..1)                          regular? no	
        
   [C] cyclic_n(n)                    characteristic: 7583   variables: x(1..n),h                              regular? yes/no
        
   [E] eco6()                         characteristic: 7583   variables: x(1..6),h                              regular? no
        
   [F] f633()                         characteristic: 7583   variables: U(6..2),u(6..2),h                      regular? no
       f744()                         characteristic: 7583   variables: U(7..2),u(7..2),h                      regular? no
       fmtm()                         characteristic:    0   variables: x,y,z,t                                regular? yes
        
   [G] gerdt93()                      characteristic: 7583   variables: h,l,s,x,y,z                            regular? no
       gonnet83()                     characteristic: 7583   variables: a(0),a(2..5),b(0..5),c(0..5),h         regular? no
        
   [H] hairer1                        characteristic: 7583   variables: c(2..3),b(3..1),a(21),a(32),a(31),h    regular? yes
       hfe_segers()                   characteristic:    2   variables: x(1..7),h                              regular? no
        
   [K] katsura5()                     characteristic: 7583   variables: x,y,z,t,u,v,h                          regular? yes
       katsura6()                     characteristic: 7583   variables: x(1..7),h                              regular? yes
       katsura7()                     characteristic: 7583   variables: x(1..8),h                              regular? yes
       katsura8()                     characteristic: 7583   variables: x(1..9),h                              regular? yes
        
   [L] lichtblau()                    characteristic:    0   variables: t,x,y                                  regular? yes
       liu()                          characteristic:    2   variables: x,y,z,t,a,h                            regular? yes
        
   [S] schrans_troost()               characteristic: 7583   variables: x(1..8),h                              regular? yes
       sym33()                        characteristic: 7583   variables: h,x,y,z                                regular? yes
        
   [T] trinks()                       characteristic: 7583   variables: w,p,z,t,s,b,h                          regular? no
      
   [U] uteshev_bikker()               characteristic: 7583   variables: x,y,z,t,h                              regular? yes
        
   [W] weispfenning94()               characteristic: 7583   variables: x,y,z,h                                regular? no
        

*-

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
bugStas = () -> toABC (
     V = getSymbol "V";
     U = getSymbol "U";
     kk = GF(16, Variable=>a);
     R = kk[V_1, reverse(U_1..U_15)];
     I = ideal(
	  a^3*U_15+a^6*U_14+a^12*U_13+a^12*U_12+a^10*U_11+a^9*U_10+a^3*U_9+a^9*U_8+a^6*U_7+a^5*U_6+a^9*U_5+a^3*U_4+a^12*U_3+a^6*U_2,
	  a^5*U_15+a^10*U_14+a^6*U_13+a^5*U_12+a^10*U_11+a^12*U_10+a^12*U_9+a^10*U_8+a^3*U_7+a^5*U_6+a^6*U_5+a^9*U_4+a^3*U_3+a^9*U_2,
	  a^6*U_15+a^12*U_14+a^14*U_13+a^9*U_12+a^5*U_11+a^13*U_10+a^3*U_8+a^7*U_7+a^10*U_6+a^11*U_4,
	  a^5*U_15+a^10*U_14+a^8*U_13+a^5*U_12+U_11+a*U_10+a^4*U_9+a^10*U_8+a^4*U_7+U_6+a^2*U_5+a^2*U_4+a*U_3+a^8*U_2+1,
	  U_15+U_14+a^6*U_13+U_12+a^10*U_11+a^12*U_10+a^11*U_9+U_8+a^3*U_7+a^5*U_6+a^13*U_5+a^9*U_4+a^14*U_3+a^7*U_2,
	  a^12*U_15+a^9*U_14+a^13*U_13+a^3*U_12+a^5*U_11+a^11*U_10+a^6*U_9+a^6*U_8+a^14*U_7+a^10*U_6+a^3*U_5+a^7*U_4+a^9*U_3+a^12*U_2+U_1,
	  a^4*U_15+a^8*U_14+a^12*U_13+a*U_12+a^9*U_10+a^2*U_8+a^6*U_7+a^3*U_4,
	  a*U_15+a^2*U_14+a^7*U_13+a^4*U_12+a^5*U_11+a^14*U_10+a^11*U_9+a^8*U_8+a^11*U_7+a^10*U_6+a^13*U_5+a^13*U_4+a^14*U_3+a^7*U_2,
	  a^8*U_15+a*U_14+a^5*U_13+a^2*U_12+a^5*U_11+a^10*U_10+a^14*U_9+a^4*U_8+a^10*U_7+a^10*U_6+a^7*U_5+a^5*U_4+a^11*U_3+a^13*U_2+U_1,
	  U_15+U_14+a^4*U_13+U_12+a^10*U_11+a^8*U_10+a^11*U_9+U_8+a^2*U_7+a^5*U_6+a^13*U_5+a*U_4+a^14*U_3+a^7*U_2+U_1,
	  V_1*U_1+U_2,
	  V_1*U_2+U_3,
	  V_1*U_3+U_4,
	  V_1*U_4+U_5,
	  V_1*U_5+U_6,
	  V_1*U_6+U_7,
	  V_1*U_7+U_8,
	  V_1*U_8+U_9,
	  V_1*U_9+U_10,
	  V_1*U_10+U_11,
	  V_1*U_11+U_12,
	  V_1*U_12+U_13,
	  V_1*U_13+U_14,
	  V_1*U_14+U_15,
	  V_1*U_15+U_1
	  )
     )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
weispfenning94 = () -> toABC (
  R = ZZ/7583[x,y,z,h];
  p1 = poly"y4 + xy2z + x2h2 - 2xyh2 + y2h2 + z2h2";
  p2 = poly"xy4 + yz4 - 2x2yh2 - 3h5";
  p3 = poly"-x3y2 + xyz3 + y4h + xy2zh - 2xyh3";
  ideal(p1,p2,p3)
  )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
liu = () -> toABC (
     R = ZZ/2[x,y,z,t,a,h];
     I = ideal"
       yz - yt - xh + ah,
       zt - zx - yh + ah,
       tx - yt - zh + ah,
       xy - zx - th + ah"
     )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
buchberger87 = () -> toABC (
  R = ZZ/7583[h,r,t,x,y,z];
  I = ideal"hx - rt,hz - r2,h2y - rt2"
  )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
gerdt93 = () -> toABC (
  R = ZZ/7583[h,l,s,x,y,z];
  I = ideal"hl - l2 - 4ls + hy, h2s - 6ls2 + h2z, xh2 - l2s - h3"
  )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
lichtblau = () -> toABC (
  R = QQ[t,x,y];
  I = ideal"
    x - 110t2 + 495t3 - 1320t4 + 2772t5 - 5082t6 + 7590t7 - 8085t8 + 5555t9 - 2189t10 + 374t11,
    y - 22t + 110t2 - 330t3 + 1848t5 - 3696t6 + 3300t7 - 1650t8 + 550t9 - 88t10 - 22t11"
  )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
trinks = () -> toABC (
  R = ZZ/7583[w,p,z,t,s,b,h];
  I = ideal"
    35p + 40z + 25t - 27s,
    45p + 35s - 165b - 36h,
    -11sb + 3b2 + 99wh,
    25ps - 165b2 + 15wh + 30zh - 18th,
    15pt + 20zs - 9wh,
    -11b3 + wph + 2zth"
  )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
eco6 = () -> toABC (
  x = getSymbol "x";
  R = ZZ/7583[x_1..x_6,h];
  I = ideal(
       x_1 + x_2 + x_3 + x_4 + x_5 + h,
       x_5*x_6 - 5*h^2,
       x_1*x_5*x_6 + x_4*x_6*h - 4*h^3,
       x_1*x_4*x_6 + x_2*x_5*x_6 + x_3*x_6*h - 3*h^3,
       x_1*x_3*x_6 + x_2*x_4*x_6 + x_3*x_5*x_6 + x_2*x_6*h - 2*h^3,
       x_1*x_2*x_6 + x_2*x_3*x_6 + x_3*x_4*x_6 + x_4*x_5*x_6 + x_1*x_6*h - h^3)
  )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
sym33 = () -> toABC (
  R = ZZ/7583[h,x,y,z];
  I = ideal"yz3 + h3x - 2h4, x3z + h3y - 2h4, xy3 + h3z - 2h4"
  )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
hairer1 = () -> toABC (
  c = getSymbol "c";
  b = getSymbol "b";
  a = getSymbol "a";
  R = ZZ/7583[c_2,c_3,b_3,b_2,b_1,a_21,a_32,a_31,h];
  I = ideal(
      b_3 + b_2 + b_1 - h,
      c_3 - a_32 - a_31,
      c_2 - a_21,
      2*c_3*b_3 + 2*c_2*b_2 - h^2,
      6*c_2*b_3*a_32 - h^3,
      3*c_3^2*b_3 + 3*c_2^2*b_2 - h^3)
  )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
f633 = () -> toABC (
  u = getSymbol "u";
  U = getSymbol "U";
  R = ZZ/7583[U_6,U_5,U_4,U_3,U_2,u_6,u_5,u_4,u_3,u_2,h];
  I = ideal(
      2*u_6 + 2*u_5 + 2*u_4 + 2*u_3 + 2*u_2 + h,
      -- the last + on the next line was incorrect in the original file, it was a *
      2*U_6 + 2*U_5 + 2*U_4 + 2*U_3 * 2*U_2 + h,
      4*U_5*u_6 + 4*U_4*u_6 + 4*U_3*u_6 + 4*U_2*u_6 - 4*U_6*u_5 + 4*U_4*u_5 + 4*U_3*u_5 + 4*U_2*u_5 - 4*U_6*u_4 - 4*U_5*u_4 + 4*U_3*u_4 + 4*U_2*u_4 - 4*U_6*u_3 - 4*U_5*u_3 - 4*U_4*u_3 + 4*U_2*u_3 - 4*U_6*u_2 - 4*U_5*u_2 - 4*U_4*u_2 - 4*U_3*u_2 + 2*u_6*h + 2*u_5*h + 2*u_4*h + 2*u_3*h + 2*u_2*h + h^2,
      -4*U_5*u_6 - 4*U_4*u_6 - 4*U_3*u_6 - 4*U_2*u_6 + 4*U_6*u_5 - 4*U_4*u_5 - 4*U_3*u_5 - 4*U_2*u_5 + 4*U_6*u_4 + 4*U_5*u_4 - 4*U_3*u_4 - 4*U_2*u_4 + 4*U_6*u_3 + 4*U_5*u_3 + 4*U_4*u_3 - 4*U_2*u_3 + 4*U_6*u_2 + 4*U_5*u_2 + 4*U_4*u_2 + 4*U_3*u_2 + 2*U_6*h + 2*U_5*h + 2*U_4*h + 2*U_3*h + 2*U_2*h + h^2,
      U_2*u_2 - h^2,
      U_3*u_3 - h^2,
      U_4*u_4 - h^2,
      U_5*u_5 - h^2,
      U_6*u_6 - h^2)
  )

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
katsura5 = () -> toABC (
  R = ZZ/7583[x,y,z,t,u,v,h];
  I = ideal"
    2x2 + 2y2 + 2z2 + 2t2 + 2u2 + v2 - vh,
    xy + yz + 2zt + 2tu + 2uv + uh,
    2xz + 2yt + 2zu + u2 + 2tv - th,
    2xt + 2yu + 2tu + 2zv - zh,
    t2 + 2xv + 2yv + 2zv - yh,
    2x + 2y + 2z + 2t + 2u + v - h"
  )

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
katsura6 = () -> toABC (
  x = getSymbol "x";
  R = ZZ/7583[x_1..x_7,h];
  ideal(
    x_1 + 2*x_2 + 2*x_3 + 2*x_4 + 2*x_5 + 2*x_6 + 2*x_7 - h,
    2*x_3*x_4 + 2*x_2*x_5 + 2*x_1*x_6 + 2*x_2*x_7 - x_6*h,
    x_3^2 + 2*x_2*x_4 + 2*x_1*x_5 + 2*x_2*x_6 + 2*x_3*x_7 - x_5*h,
    2*x_2*x_3 + 2*x_1*x_4 + 2*x_2*x_5 + 2*x_3*x_6 + 2*x_4*x_7 - x_4*h,
    x_2^2 + 2*x_1*x_3 + 2*x_2*x_4 + 2*x_3*x_5 + 2*x_4*x_6 + 2*x_5*x_7 - x_3*h,
    2*x_1*x_2 + 2*x_2*x_3 + 2*x_3*x_4 + 2*x_4*x_5 + 2*x_5*x_6 + 2*x_6*x_7 - x_2*h,
    x_1^2 + 2*x_2^2 + 2*x_3^2 + 2*x_4^2 + 2*x_5^2 + 2*x_6^2 + 2*x_7^2 - x_1*h)
  )

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
katsura7 = () -> toABC (
  x = getSymbol "x";
  R = ZZ/7583[x_1..x_8,h];
  I = ideal(
      x_1^2 + 2*x_2^2 + 2*x_3^2 + 2*x_4^2 + 2*x_5^2 + 2*x_6^2 + 2*x_7^2 + 2*x_8^2 - x_1*h,
      2*x_1*x_2 + 2*x_2*x_3 + 2*x_3*x_4 + 2*x_4*x_5 + 2*x_5*x_6 + 2*x_6*x_7 + 2*x_7*x_8 - x_2*h,
      x_2^2 + 2*x_1*x_3 + 2*x_2*x_4 + 2*x_3*x_5 + 2*x_4*x_6 + 2*x_5*x_7 + 2*x_6*x_8 - x_3*h,
      2*x_2*x_3 + 2*x_1*x_4 + 2*x_2*x_5 + 2*x_3*x_6 + 2*x_4*x_7 + 2*x_5*x_8 - x_4*h,
      x_3^2 + 2*x_2*x_4 + 2*x_1*x_5 + 2*x_2*x_6 + 2*x_3*x_7 + 2*x_4*x_8 - x_5*h,
      2*x_3*x_4 + 2*x_2*x_5 + 2*x_1*x_6 + 2*x_2*x_7 + 2*x_3*x_8 -x_6*h,
      x_4^2 + 2*x_3*x_5 + 2*x_2*x_6 + 2*x_1*x_7 + 2*x_2*x_8 - x_7*h,
      x_1 + 2*x_2 + 2*x_3 + 2*x_4 + 2*x_5 + 2*x_6 + 2*x_7 + 2*x_8 - h)
  )

---------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
katsura8 = () -> toABC (
     x = getSymbol "x";
     R = ZZ/7583[x_1..x_9,h];
     I = ideal(
	  x_1^2 + 2*x_2^2 + 2*x_3^2 + 2*x_4^2 + 2*x_5^2 + 2*x_6^2 + 2*x_7^2 + 2*x_8^2 + 2*x_9^2 - x_1*h,
	  2*x_1*x_2 + 2*x_2*x_3 + 2*x_3*x_4 + 2*x_4*x_5 + 2*x_5*x_6 + 2*x_6*x_7 + 2*x_7*x_8 + 2*x_8*x_9- x_2*h,
	  x_2^2 + 2*x_1*x_3 + 2*x_2*x_4 + 2*x_3*x_5 + 2*x_4*x_6 + 2*x_5*x_7 + 2*x_6*x_8 + 2*x_7*x_9 - x_3*h,
	  2*x_2*x_3 + 2*x_1*x_4 + 2*x_2*x_5 + 2*x_3*x_6 + 2*x_4*x_7 + 2*x_5*x_8 + 2*x_6*x_9 - x_4*h,
	  x_3^2 + 2*x_2*x_4 + 2*x_1*x_5 + 2*x_2*x_6 + 2*x_3*x_7 + 2*x_4*x_8 + 2*x_5*x_9 - x_5*h,
	  2*x_3*x_4 + 2*x_2*x_5 + 2*x_1*x_6 + 2*x_2*x_7 + 2*x_3*x_8 + 2*x_4*x_9 -x_6*h,
	  x_4^2 + 2*x_3*x_5 + 2*x_2*x_6 + 2*x_1*x_7 + 2*x_2*x_8 + 2*x_3*x_9 - x_7*h,
	  2*x_4*x_5 + 2*x_3*x_6 + 2*x_2*x_7 + 2*x_1*x_8 + 2*x_2*x_9 - x_8*h,
	  x_1 + 2*x_2 + 2*x_3 + 2*x_4 + 2*x_5 + 2*x_6 + 2*x_7 + 2*x_8 + 2*x_9 - h)
     )

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
cyclicn = (n) -> () -> toABC (
     x = getSymbol "x";
     R = ZZ/7583[x_1..x_n];
     I1 := ideal for a from 1 to n-1 list (
	  sum for j from 1 to n list (
	       product for k from j to j+a-1 list (
		    if k <= n then x_k else x_(k-n)
		    )
	       )
	  );
     I1 + ideal (-1 + product for j from 1 to n list x_j)
     )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
hcyclicn = (n) -> () -> toABC (
     x = getSymbol "x";
     R = ZZ/7583[x_1..x_n,h];
     I1 := ideal for a from 1 to n-1 list (
	  sum for j from 1 to n list (
	       product for k from j to j+a-1 list (
		    if k <= n then x_k else x_(k-n)
		    )
	       )
	  );
     I1 + ideal (-h^n + product for j from 1 to n list x_j)
     )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
fmtm = () -> toABC (
  R = QQ[x,y,z,t];
  I = ideal"
      yz3 - x2t2,
      xz2 - y2t,
      x2y - z2t"
  )

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
uteshevBikker = () -> toABC (
  R = ZZ/7583[x,y,z,t,h];
  I = ideal"
      x2 + xy + y2 - 2xz - 4yz + 3z2 - 3xt + 2yt + t2 - 3xh - 2yh + 3zh - 2th - 2h2,
      2x2 - xy + y2 - xz - yz - 6z2 - xt + yt - 5zt - 3t2 - 5xh + yh + 5zh + 2th + 5h2,
      x3 + y3 - x2z + xyz - 5y2z - 5xz2 + 7yz2 - 3z3 + xyt - 5z2t + xt2 + 2t3 + x2h - 3xyh - y2h + 2xzh + 2z2h - 3xth - 2zth - 3t2h - xh2 + yh2 + 11zh2 - 2th2 - 3h3,
      -x3 + 6x2y - 12xy2 + 6y3 - x2z - 4xyz + 6y2z + 5xz2 + 4yz2 + 15z3 + 6xyt - 7y2t - xzt + 11xt2 + 4t3 + 3x2h + 2xyh + 2y2h - z2h + 2yth - zth + 5t2h - 35xh2 - 14yh2 + 4zh2 - 10th2 - 15h3"
  )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
hfeSegers = () -> toABC (
  x = getSymbol "x";
  R = ZZ/2[x_1..x_7,h];
  I = ideal(
       x_1*x_2 + x_2^2 + x_3^2 + x_2*x_4 + x_3*x_4 + x_4^2 + x_1*x_5 + x_2*x_5 + x_4*x_5 + x_5^2 + x_1*x_6 + x_2*x_6 + x_6^2 + x_6*x_7 + x_6*h + x_7*h,
       x_1^2 + x_1*x_2 + x_1*x_3 + x_2*x_3 + x_1*x_4 + x_4^2 + x_1*x_5 + x_2*x_5 + x_3*x_5 + x_4*x_5 + x_2*x_6 + x_5*x_6 + x_1*x_7 + x_2*x_7 + x_3*x_7 + x_4*x_7 + x_5*x_7 + x_6*x_7 + x_1*h + x_3*h + x_5*h + h^2,
       x_1*x_2 + x_1*x_3 + x_3^2 + x_1*x_4 + x_2*x_4 + x_4^2 + x_4*x_5 + x_1*x_6 + x_3*x_6 + x_2*x_7 + x_3*x_7 + x_4*x_7 + x_5*x_7 + x_6*x_7 + x_1*h + x_3*h + x_4*h + x_5*h + x_6*h + x_7*h,
       x_1^2 + x_2^2 + x_1*x_3 + x_1*x_4 + x_3*x_4 + x_1*x_5 + x_2*x_5 + x_3*x_5 + x_4*x_5 + x_1*x_6 + x_1*x_7 + x_2*x_7 + x_4*x_7 + x_6*x_7 + x_7^2 + x_1*h + x_2*h + x_3*h + x_4*h + x_5*h + x_7*h + h^2,
       x_1^2 + x_2*x_3 + x_2*x_4 + x_3*x_4 + x_1*x_5 + x_3*x_5 + x_4*x_5 + x_5^2 + x_4*x_6 + x_5*x_6 + x_4*x_7 + x_6*x_7 + x_7^2 +x_2*h + x_3*h + x_5*h + x_6*h + x_7*h,
       x_1^2 + x_1*x_2 + x_2*x_5 + x_5^2 + x_4*x_6 + x_5*x_6 + x_6^2 + x_5*x_7 + x_6*x_7 + x_1*h + x_2*h + x_4*h + x_5*h + x_6*h + x_7*h + h^2,
       x_1*x_3 + x_2*x_3 + x_1*x_4 + x_3*x_4 + x_4^2 + x_1*x_5 + x_2*x_5 + x_4*x_5 + x_5^2 + x_2*x_6 + x_6^2 +x_1*x_7 + x_2*x_7 + x_5*x_7 + x_7^2 + x_2*h + x_5*h + x_6*h,
       x_1^2 + x_1*h,
       x_2^2 + x_2*h,
       -- the first term on the next line was (incorrectly, I believe) x_3^3
       x_3^2 + x_3*h,
       x_4^2 + x_4*h,
       x_5^2 + x_5*h,
       x_6^2 + x_6*h,
       x_7^2 + x_7*h)
  )
------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
gonnet83 = () -> toABC (
  c = getSymbol "c";
  b = getSymbol "b";
  a = getSymbol "a";
  R = ZZ/7583[a_0,a_2..a_5,b_0..b_5,c_0..c_5,h];
  I = ideal(
       a_5*b_5,
       a_5*b_4 + a_4*b_5,
       a_4*b_4,
       a_5*b_3 + a_3*b_5,
       a_5*b_3 + a_3*b_5 + 2*a_5*b_5,
       a_3*b_3 + a_5*b_3 + a_3*b_5 + a_5*b_5,
       2*a_3*b_3 + a_5*b_3 + a_3*b_5,
       a_4*b_2 + a_2*b_4,
       a_2*b_2,
       a_5*b_1 + a_4*b_3 + a_3*b_4 + b_5*h,
       a_4*b_1 + b_4*h,
       a_2*b_1 + b_2*h,
       a_0*b_1 + a_4*b_1 + a_3*b_2 + a_2*b_3 + b_0*h + 2*b_1*h + b_4*h + c_1*h,
       a_5*b_0 + a_5*b_1 + a_4*b_3 + a_3*b_4 + 2*a_5*b_4 + a_0*b_5 + 2*a_4*b_5 + b_5*h + c_5*h,
       a_4*b_0 + a_4*b_1 + a_5*b_2 + a_0*b_4 + 2*a_4*b_4 + a_2*b_5 + b_4*h + c_4*h,
       a_3*b_0 + 2*a_3*b_1 + a_5*b_1 + a_0*b_3 + a_4*b_3 + a_3*b_4 + 2*b_3*h + b_5*h + c_3*h,
       a_3*b_0 + a_5*b_0 + a_3*b_1 + a_5*b_1 + a_0*b_3 + a_4*b_3 + a_3*b_4 + a_5*b_4 + a_0*b_5 + a_4*b_5 + b_3*h + b_5*h + c_3*h + c_5*h - h^2,
       a_2*b_0 + a_2*b_1 + a_0*b_2 + a_4*b_2 + a_2*b_4 + b_2*h + c_2*h,
       a_0*b_0 + a_4*b_0 + a_0*b_1 + a_4*b_1 + a_3*b_2 + a_5*b_2 + a_2*b_3 + a_0*b_4 + a_4*b_4 + a_2*b_5 + b_0*h + b_1*h + b_4*h + c_0*h + c_1*h + c_4*h)
  )

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
f744 = () -> toABC (
  u = getSymbol "u";
  U = getSymbol "U";
  R = ZZ/7583[reverse(U_2..U_7), reverse(u_2..u_7), h];
  I = ideal(
       2*u_7 + 2*u_6 + 2*u_5 + 2*u_4 + 2*u_3 + 2*u_2 + h,
       2*U_7 + 2*U_6 + 2*U_5 + 2*U_4 + 2*U_3 + 2*U_2 + h,
       -- the U_7 in the first monomial on th enext line might be u_7
       8*U_6*U_7 + 8*U_5*u_7 + 8*U_4*u_7 + 8*U_3*u_7 + 8*U_2*u_7 + 8*U_6*u_6 + 8*U_5*u_6 + 8*U_4*u_6 + 8*U_3*u_6 + 8*U_2*u_6 + 8*U_5*u_5 + 8*U_4*u_5 + 8*U_3*u_5 + 8*U_2*u_5 + 8*U_4*u_4 + 8*U_3*u_4 + 8*U_2*u_4 + 8*U_3*u_3 + 8*U_2*u_3 + 8*U_2*u_2 - 17*h^2,
       16*U_5*U_3*u_4 + 16*U_5*U_2*u_4 + 16*U_5*U_2*u_3 + 16*U_4*U_2*u_3 + 8*U_5*u_4*h + 8*U_5*u_3*h + 8*U_4*u_3*h + 8*U_5*u_2*h + 8*U_4*u_2*h + 8*U_3*u_2*h + 18*U_5*h^2 + 18*U_4*h^2 + 18*U_3*h^2 + 18*U_2*h^2 + 11*h^3,
       16*U_4*u_5*u_3 + 16*U_4*u_5*u_2 + 16*U_3*u_5*u_2 + 16*U_3*u_4*u_2 + 8*U_4*u_5*h + 8*U_3*u_5*h + 8*U_2*u_5*h + 8*U_3*u_4*h + 8*U_2*u_4*h + 8*U_2*u_3*h + 18*u_5*h^2 + 18*u_4*h^2 + 18*u_3*h^2 + 18*u_2*h^2 + 11*h^3,
       U_2*u_2 - h^2,
       U_3*u_3 - h^2,
       U_4*u_4 - h^2,
       U_5*u_5 - h^2,
       U_6*u_6 - h^2,
       U_7*u_7 - h^2)
  )

------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------
schransTroost = () -> toABC (
     x = getSymbol "x";
     R = ZZ/7583[x_1..x_8,h];
     I = ideal(
	  8*x_1^2 + 8*x_1*x_2 + 8*x_1*x_3 - 8*x_2*x_3 + 2*x_1*x_4 + 2*x_1*x_5 + 2*x_1*x_6 - 2*x_5*x_6 + 2*x_1*x_7 - 2*x_4*x_7 - x_1*h,
	  8*x_1*x_2 + 8*x_2^2 - 8*x_1*x_3 + 8*x_2*x_3 + 2*x_2*x_4 + 2*x_2*x_5 + 2*x_2*x_6 - 2*x_4*x_6 + 2*x_2*x_7 - 2*x_5*x_7 - x_2*h,
	  -8*x_1*x_2 + 8*x_1*x_3 + 8*x_2*x_3 + 8*x_3^2 + 2*x_3*x_4 + 2*x_3*x_5 - 2*x_4*x_5 + 2*x_3*x_6 + 2*x_3*x_7 - 2*x_6*x_7 - x_3*h,
	  -- first + on next line might be a -??
	  2*x_1*x_4 + 2*x_2*x_4 + 2*x_3*x_4 + 8*x_4^2 - 2*x_3*x_5 + 8*x_4*x_5 - 2*x_2*x_6 + 2*x_4*x_6 - 2*x_1*x_7 + 2*x_4*x_7 + 6*x_4*x_8 - 6*x_5*x_8 - x_4*h,
	  -2*x_1*x_4 - 2*x_2*x_5 - 2*x_3*x_6 + 2*x_1*x_7 + 2*x_2*x_7 + 2*x_3*x_7 + 2*x_4*x_7 + 2*x_5*x_7 + 8*x_6*x_7 + 8*x_7^2 - 6*x_6*x_8 + 6*x_7*x_8 - x_7*h,
	  -2*x_2*x_4 - 2*x_1*x_5 + 2*x_1*x_6 + 2*x_2*x_6 + 2*x_3*x_6 + 2*x_4*x_6 + 2*x_5*x_6 + 8*x_6^2 - 2*x_3*x_7 + 8*x_6*x_7 + 6*x_6*x_8 - 6*x_7*x_8 - x_6*h,
	  -2*x_3*x_4 + 2*x_1*x_5 + 2*x_2*x_5 + 2*x_3*x_5 + 8*x_4*x_5 + 8*x_5^2 - 2*x_1*x_6 + 2*x_5*x_6 - 2*x_2*x_7 + 2*x_5*x_7 - 6*x_4*x_8 + 6*x_5*x_8 - x_5*h,
	  -6*x_4*x_5 - 6*x_6*x_7 + 6*x_4*x_8 + 6*x_5*x_8 + 6*x_6*x_8 + 6*x_7*x_8 + 8*x_8^2 - x_8*h);
     I)

---------------------------------------------------
-- My tests ---------------------------------------
---------------------------------------------------
chowGrass = (n,m,kk) -> (
     R = ZZ/32003[vars(0..m-1), Degrees=>{1..m}];
     Rx = R[X, Join=>false];
     G = X^m + sum(m, i -> R_i * X^(m-1-i));
     ideal lift(last coefficients(X^n % G), R))

chow7'14 = () -> chowGrass(14,7,ZZ/32003)
chow7'15 = () -> chowGrass(15,7,ZZ/32003)
chow7'16 = () -> chowGrass(16,7,ZZ/32003)
chow7'17 = () -> chowGrass(17,7,ZZ/32003)
chow7'18 = () -> chowGrass(18,7,ZZ/32003)
chow7'19 = () -> chowGrass(19,7,ZZ/32003)
chow7'20 = () -> chowGrass(20,7,ZZ/32003)

chow6 = () -> (
     R = ZZ/32003[vars(0..23)];     
     ideal(a+g-m,
	  a*g+b+h-n,
	  b*g+a*h+c+i-o,
	  c*g+b*h+a*i+d+j-p,
	  d*g+c*h+b*i+a*j+e+k-q,
	  e*g+d*h+c*i+b*j+a*k+f+l-r,
	  f*g+e*h+d*i+c*j+b*k+a*l-s,
	  f*h+e*i+d*j+c*k+b*l-t,
	  f*i+e*j+d*k+c*l-u,
	  f*j+e*k+d*l-v,
	  f*k+e*l-w,
	  f*l-x)
     )

jason210 = () -> (
     R = ZZ/32003[a..h];
     I = ideal"a6,b6,a2c4+b2d4+abc2e2+abd2f2+abcdeg+abcdfh";
     I)

bayes148 = () -> toABC (
     p = symbol p;
     R1 = ZZ/32003[reverse(p_(1,1,1,1,1)..p_(2,2,2,2,2)), MonomialSize=>8];
     J1 = ideal(
     -p_(1,1,2,1,1)*p_(2,1,1,1,1)+p_(1,1,1,1,1)*p_(2,1,2,1,1),
     -p_(1,1,2,1,2)*p_(2,1,1,1,2)+p_(1,1,1,1,2)*p_(2,1,2,1,2),
     -p_(1,1,2,2,1)*p_(2,1,1,2,1)+p_(1,1,1,2,1)*p_(2,1,2,2,1),
     -p_(1,1,2,2,2)*p_(2,1,1,2,2)+p_(1,1,1,2,2)*p_(2,1,2,2,2),
     -p_(1,2,2,1,1)*p_(2,2,1,1,1)+p_(1,2,1,1,1)*p_(2,2,2,1,1),
     -p_(1,2,2,1,2)*p_(2,2,1,1,2)+p_(1,2,1,1,2)*p_(2,2,2,1,2),
     -p_(1,2,2,2,1)*p_(2,2,1,2,1)+p_(1,2,1,2,1)*p_(2,2,2,2,1),
     -p_(1,2,2,2,2)*p_(2,2,1,2,2)+p_(1,2,1,2,2)*p_(2,2,2,2,2),
     -p_(1,1,1,1,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,1,2),
     -p_(1,1,1,2,1)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,1),
     -p_(1,1,1,2,1)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,1),
     -p_(1,1,1,2,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,2),
     -p_(1,1,1,2,2)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,2),
     -p_(1,1,1,2,2)*p_(1,2,1,2,1)+p_(1,1,1,2,1)*p_(1,2,1,2,2),
     -p_(1,1,2,1,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,1,2),
     -p_(1,1,2,2,1)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,1),
     -p_(1,1,2,2,1)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,1),
     -p_(1,1,2,2,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,2),
     -p_(1,1,2,2,2)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,2),
     -p_(1,1,2,2,2)*p_(1,2,2,2,1)+p_(1,1,2,2,1)*p_(1,2,2,2,2),
     -p_(1,1,1,1,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,1,2),
     -p_(1,1,1,2,2)*p_(1,2,1,2,1)+p_(1,1,1,2,1)*p_(1,2,1,2,2),
     -p_(1,1,2,1,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,1,2),
     -p_(1,1,2,2,2)*p_(1,2,2,2,1)+p_(1,1,2,2,1)*p_(1,2,2,2,2),
     -p_(1,1,1,2,1)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,1),
     -p_(1,1,1,2,2)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,2),
     -p_(1,1,2,2,1)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,1),
     -p_(1,1,2,2,2)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,2),
     -p_(1,1,1,1,2)*p_(1,1,1,2,1)+p_(1,1,1,1,1)*p_(1,1,1,2,2)
        +p_(1,1,1,2,2)*p_(1,1,2,1,1)-p_(1,1,1,2,1)*p_(1,1,2,1,2)
	-p_(1,1,1,1,2)*p_(1,1,2,2,1)-p_(1,1,2,1,2)*p_(1,1,2,2,1)
	+p_(1,1,1,1,1)*p_(1,1,2,2,2)+p_(1,1,2,1,1)*p_(1,1,2,2,2)
	+p_(1,1,1,2,2)*p_(1,2,1,1,1)+p_(1,1,2,2,2)*p_(1,2,1,1,1)
	-p_(1,1,1,2,1)*p_(1,2,1,1,2)-p_(1,1,2,2,1)*p_(1,2,1,1,2)
	-p_(1,1,1,1,2)*p_(1,2,1,2,1)-p_(1,1,2,1,2)*p_(1,2,1,2,1)
	-p_(1,2,1,1,2)*p_(1,2,1,2,1)+p_(1,1,1,1,1)*p_(1,2,1,2,2)
	+p_(1,1,2,1,1)*p_(1,2,1,2,2)+p_(1,2,1,1,1)*p_(1,2,1,2,2)
	+p_(1,1,1,2,2)*p_(1,2,2,1,1)+p_(1,1,2,2,2)*p_(1,2,2,1,1)
	+p_(1,2,1,2,2)*p_(1,2,2,1,1)-p_(1,1,1,2,1)*p_(1,2,2,1,2)
	-p_(1,1,2,2,1)*p_(1,2,2,1,2)-p_(1,2,1,2,1)*p_(1,2,2,1,2)
	-p_(1,1,1,1,2)*p_(1,2,2,2,1)-p_(1,1,2,1,2)*p_(1,2,2,2,1)
	-p_(1,2,1,1,2)*p_(1,2,2,2,1)-p_(1,2,2,1,2)*p_(1,2,2,2,1)
	+p_(1,1,1,1,1)*p_(1,2,2,2,2)+p_(1,1,2,1,1)*p_(1,2,2,2,2)
	+p_(1,2,1,1,1)*p_(1,2,2,2,2)+p_(1,2,2,1,1)*p_(1,2,2,2,2)))
J1

eco9 = () -> toABC (
  R = QQ[x1,x2,x3,x4,x5,x6,x7,x8, MonomialOrder=>Lex];
  I = ideal(
     x1 + x2*(x1 + x3) + x4*(x3 + x5) + x6*(x5 + x7) -( x8*((1/8) - x7)),
     x2 + x3*(x1 + x5) + x4*(x2 + x6) + x5*x7 -( x8*((2/8) - x6)),
     x3*(1 + x6) + x4*(x1 + x7) + x2*x5 -( x8*((3/8) - x5)),
     x4 + x1*x5 + x2*x6 + x3*x7 -( x8*((4/8) - x4)),
     x5 + x1*x6 + x2*x7 -( x8*((5/8) - x3)),
     x6 + x1*x7 -( x8*((6/8) - x2)),
     x7 -( x8*((7/8) - x1)),
     x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 +1))

hilbscheme = () -> (
     -- found by MES while investigating the Hilbert scheme of curves of degree 3, genus -2.
     R = ZZ/32003[vars(0..17)];
     I = ideal(2*o^2*q^2*r^2-i*o*q^2*r+k*o*q^2*r-4*m*o*q^2*r+o^2*p*r^2-3*n*o*q*r^2+i*m*q^2-k*m*q^2+2*m^2*q^2-2*i*o*p*r+2*k*o*p*r-2*m*o*p*r+i*n*q*r-k*n*q*r+3*m*n*q*r+3*e*o*q*r+3*j*o*q*r-3*l*o*q*r+n^2*r^2+i^2*p-2*i*k*p+k^2*p+2*i*m*p-2*k*m*p+m^2*p-e*i*q-i*j*q+e*k*q+j*k*q+i*l*q-k*l*q-3*e*m*q-3*j*m*q+3*l*m*q-2*e*n*r-2*j*n*r+2*l*n*r+e^2+2*e*j+j^2-2*e*l-2*j*l+l^2,
	  2*o^2*q^3*r-i*o*q^3+k*o*q^3-2*m*o*q^3-3*o^2*p*q*r-3*n*o*q^2*r+2*i*o*p*q-2*k*o*p*q+3*m*o*p*q+i*n*q^2-k*n*q^2+2*m*n*q^2+e*o*q^2+j*o*q^2-l*o*q^2+2*n*o*p*r+n^2*q*r+2*h*o*q*r-i*n*p+k*n*p-m*n*p-e*o*p-j*o*p+l*o*p-h*i*q+h*k*q-2*h*m*q-e*n*q-j*n*q+l*n*q-h*n*r-d*o*r+e*h+d*i+h*j-d*k-h*l+d*m,
	  -2*i*o*q*r^2+2*i*m*q*r+16001*f*o*q*r+g*o*q*r+2*i*n*r^2-2*e*o*r^2-16001*f*m*q-g*m*q-2*i*j*r-2*e*k*r+2*i*l*r+2*e*m*r-16001*f*n*r-g*n*r+b*o*r+16001*c*o*r+16001*e*f+e*g-b*i-16001*c*i+16001*f*j+g*j+b*k+16001*c*k-16001*f*l-g*l-b*m-16001*c*m,
	  -a*o*r+a*i-a*k+a*m-1)
     )

hilbscheme2 = () -> (
     R = ZZ/32003[vars(0..20), Degrees => {{3}, {5}, {6}, {8}, {3}, {3}, {6}, {9}, {5}, {5}, {8}, {7}, {10}, {3}, {6}, {2}, {2}, {8}, {4}, {4}, {6}}];
     I = ideal(2*a*n+c-2*o,-f*p*q+j*p+f*s,-f*p*t+l*p+j*s,a^3-a^2*e-a*f*n+a*g+f*o-h,-a^2*p^2-f*n*p^2+2*b*n*p+g*p^2+2*o*p*q+d*p-2*p*r,-16001*a*f*p^2-5334*e*f*p^2+16001*a^2*p*q+10668*a*e*p*q+a*b*p-10668*b*e*p-10668*a*i*p-5334*g*p*q-16001*a^2*s-10668*a*e*s+5334*k*p+5334*g*s,-f*j*q+f^2*t+j^2-f*l,-f*p*u+l*s,-a^2*f*p-f^2*n*p+2*b*f*n+f*g*p+2*f*o*q+d*f-2*f*r,-16001*a*f^2*p-5334*e*f^2*p+a*b*f-10668*b*e*f-10668*a*f*i+16001*a^2*j+10668*a*e*j-5334*g*j+5334*f*k,-a^2*p*s-f*n*p*s+2*b*n*s+g*p*s+2*o*q*s+d*s-2*r*s,10668*f^2*p^3+b*f*p^2-10668*f*i*p^2+a*f*p*s-10668*e*f*p*s-a^2*p*t-10667*a*e*p*t+b^2*p+10667*b*i*p+2*a*b*s+10667*b*e*s+10667*a*i*s-10668*g*p*t+10668*m*p+10668*k*s,-f*l*q+f^2*u+j*l,-a^2*f*p*q-f^2*n*p*q+f*g*p*q+a^2*f*s+f^2*n*s+2*b*j*n+2*j*o*q-f*g*s+d*j-2*j*r,-16001*a*f^2*p*q-5334*e*f^2*p*q+16001*a^2*j*q+10668*a*e*j*q+16001*a*f^2*s+5334*e*f^2*s-16001*a^2*f*t-10668*a*e*f*t+a*b*j-10668*b*e*j-10668*a*i*j+16001*a^2*l+10668*a*e*l-5334*g*j*q+5334*f*g*t+5334*j*k-5334*g*l,10668*f^3*p^2+b*f^2*p-10668*f^2*i*p+b^2*f+10667*b*f*i-a^2*l-10667*a*e*l-10668*g*l+10668*f*m,10668*f^2*p^2*s+b*f*p*s-10668*f*i*p*s-a^2*p*u-10667*a*e*p*u+b^2*s+10667*b*i*s-10668*g*p*u+10668*m*s,-f*l*t+f*j*u+l^2,-10668*a^2*e^2*p-2*a^2*f*n*p+10668*a*e*f*n*p+2*a^2*b*n-10669*a*b*e*n-10666*a^2*g*p-10668*a*e*g*p-10668*f*g*n*p+a*f*o*p+10668*e*f*o*p+2*a^2*o*q-10669*a*e*o*q+a^2*d+10667*a*d*e-10667*b*g*n+10668*g^2*p-a*h*p-10668*e*h*p-10667*g*o*q-2*a^2*r+10669*a*e*r+10668*d*g+10667*g*r,-a^2*f*p*t-f^2*n*p*t+a^2*j*s+f*j*n*s+f*g*p*t+2*b*l*n+2*l*o*q-g*j*s+d*l-2*l*r,-16001*a*f^2*p*t-5334*e*f^2*p*t+16001*a^2*l*q+10668*a*e*l*q+16001*a*f*j*s+5334*e*f*j*s-16001*a^2*f*u-10668*a*e*f*u+a*b*l-10668*b*e*l-10668*a*i*l-5334*g*l*q+5334*f*g*u+5334*k*l,10668*f^3*p^2*q+b*f^2*p*q-10668*f^2*i*p*q-10668*f^3*p*s-a^2*l*q-10667*a*e*l*q-b*f^2*s+10668*f^2*i*s+a^2*f*u+10667*a*e*f*u+b^2*j+10667*b*i*j-10668*g*l*q+10668*f*g*u+10668*j*m,-a^2*e^2*p^2-3*a^2*f*n*p^2-a*e*f*n*p^2-f^2*n^2*p^2+3*a^2*g*p^2+a*e*g*p^2+2*f*g*n*p^2+a*f*o*p^2+e*f*o*p^2+4*b^2*n^2-g^2*p^2-a*h*p^2-e*h*p^2+8*b*n*o*q+4*o^2*q^2+4*b*d*n+4*d*o*q-8*b*n*r-8*o*q*r+d^2-4*d*r+4*r^2,10668*a^2*e*f*p^2+a*f^2*n*p^2-5334*e*f^2*n*p^2-5334*a^2*e^2*p*q-a^2*f*n*p*q+5334*a*e*f*n*p*q-a*f*g*p^2+5334*e*f*g*p^2+16001*f^2*o*p^2-5333*a^2*g*p*q-5334*a*e*g*p*q-5334*f*g*n*p*q-16001*a*f*o*p*q+5334*e*f*o*p*q+5334*a^2*e^2*s+a^2*f*n*s-5334*a*e*f*n*s+2*a*b^2*n+10667*b^2*e*n+10667*a*b*i*n-16001*f*h*p^2+2*a*b*o*q+10667*b*e*o*q+10667*a*i*o*q+5334*g^2*p*q+16001*a*h*p*q-5334*e*h*p*q+5333*a^2*g*s+5334*a*e*g*s+5334*f*g*n*s+16001*a*f*o*s-5334*e*f*o*s+a*b*d-10668*b*d*e-10668*a*d*i+10668*b*k*n+10668*k*o*q-2*a*b*r-10667*b*e*r-10667*a*i*r-5334*g^2*s-16001*a*h*s+5334*e*h*s+5334*d*k-10668*k*r,-8890*a^2*f^2*p^2-4741*a*e*f^2*p^2+10075*e^2*f^2*p^2+11853*a*e^2*f*p*q-16001*a*f^2*n*p*q-3556*e*f^2*n*p*q+11853*b*e^2*f*p-11853*a*e*f*i*p-11853*f^2*g*p^2-15409*a*b*e^2*q-8297*a^2*e*i*q-7112*b*e*f*n*q-10668*a^2*j*n*q+7112*a*e*j*n*q-3556*a*f*g*p*q+10075*e*f*g*p*q-5334*f^2*o*p*q-3556*a*e^2*f*s+16001*a*f^2*n*s+11853*a^2*e^2*t+10668*a^2*f*n*t-3556*a*e*f*n*t+a^2*b^2+10667*a*b^2*e+15409*b^2*e^2+10667*a^2*b*i+15409*a*b*e*i+15409*a^2*i^2-3556*b*f*g*p+11853*f*g*i*p+1778*a*f*k*p+10075*e*f*k*p+14224*a*b*g*q-8297*b*e*g*q-15409*a*g*i*q-3556*a^2*k*q-8297*a*e*k*q-3556*g*j*n*q+3556*f*k*n*q-10667*b*f*o*q-7112*f*i*o*q+5334*f*h*p*q+10668*a*f*g*s+1778*e*f*g*s-16001*f^2*o*s-3556*a^2*g*t-11853*a*e*g*t-10668*a*f*o*t+3556*e*f*o*t-3556*b^2*g-8297*b*g*i+14224*a*b*k-15409*b*e*k-15409*a*i*k-3556*a^2*m-8297*a*e*m+10667*b*h*q+7112*h*i*q-11853*g*k*q+16001*f*h*s+11853*g^2*t+10668*a*h*t-3556*e*h*t+11853*k^2-11853*g*m,-10668*f^3*p*q*s+10668*f^3*p^2*t+10668*f^3*s^2+b*f^2*p*t-10668*f^2*i*p*t-b*f*j*s+10668*f*i*j*s-a^2*l*t-10667*a*e*l*t+a^2*j*u+10667*a*e*j*u+b^2*l+10667*b*i*l-10668*g*l*t+10668*g*j*u+10668*l*m,-5334*a^2*f^2*p^3+1778*e^2*f^2*p^3+10668*f^3*n*p^3+10668*a^2*e*f*p^2*q-3556*a*e^2*f*p^2*q-16001*a*f^2*n*p^2*q+3556*b*e^2*f*p^2+3556*a*e*f*i*p^2+b*f^2*n*p^2-10668*f^2*i*n*p^2-10668*f^2*g*p^3-10668*a*f*g*p^2*q+1778*e*f*g*p^2*q+16001*f^2*o*p^2*q+10668*a^2*e*f*p*s+3556*a*e^2*f*p*s+16001*a*f^2*n*p*s+10668*e*f^2*n*p*s-10668*a^2*e^2*p*t-2*a^2*f*n*p*t+10668*a*e*f*n*p*t-b*f*g*p^2+10668*f*g*i*p^2-5334*a*f*k*p^2-1778*e*f*k*p^2-16001*f*h*p^2*q+10669*a^2*b*e*s+10667*a^2*e*i*s-10667*b*e*f*n*s+2*a^2*j*n*s-10669*a*e*j*n*s+10666*a*f*g*p*s+8890*e*f*g*p*s+16001*f^2*o*p*s-10666*a^2*g*p*t-10668*a*e*g*p*t-10668*f*g*n*p*t+a*f*o*p*t+10668*e*f*o*p*t+2*b^3*n-10669*b^2*i*n+2*b^2*o*q-10669*b*i*o*q-4*a*b*g*s-10667*b*e*g*s+10669*a*g*i*s+10668*a^2*k*s-10667*g*j*n*s-10668*f*k*n*s-2*b*f*o*s-10667*f*i*o*s-16001*f*h*p*s+10668*g^2*p*t-a*h*p*t-10668*e*h*p*t+b^2*d+10667*b*d*i-10667*b*m*n-10667*m*o*q-2*b^2*r+10669*b*i*r+2*b*h*s+10667*h*i*s-10668*g*k*s+10668*d*m+10667*m*r,2667*a*f^3*p^3-889*e*f^3*p^3-12446*a^2*f^2*p^2*q-13038*a*e*f^2*p^2*q-10964*e^2*f^2*p^2*q-10075*a*e^2*f*p*q^2+8001*a*f^2*n*p*q^2-1778*e*f^2*n*p*q^2-1778*e*f^2*i*p^2-10075*b*e^2*f*p*q+10075*a*e*f*i*p*q+7408*f^2*g*p^2*q+8297*a*b*e^2*q^2+11853*a^2*e*i*q^2-3556*b*e*f*n*q^2-5334*a^2*j*n*q^2+3556*a*e*j*n*q^2-1778*a*f*g*p*q^2-10964*e*f*g*p*q^2-2667*f^2*o*p*q^2+10668*a^2*f^2*p*s-7112*a*e*f^2*p*s+889*e^2*f^2*p*s-1778*a*e^2*f*q*s-8001*a*f^2*n*q*s+3556*a*e^2*f*p*t-16001*a*f^2*n*p*t-10075*a^2*e^2*q*t+5334*a^2*f*n*q*t-1778*a*e*f*n*q*t+2667*f^2*k*p^2-8297*b^2*e^2*q-15409*a*b*e*i*q-8297*a^2*i^2*q-1778*b*f*g*p*q-10075*f*g*i*p*q+889*a*f*k*p*q-10964*e*f*k*p*q+7112*a*b*g*q^2+11853*b*e*g*q^2+8297*a*g*i*q^2-1778*a^2*k*q^2+11853*a*e*k*q^2-1778*g*j*n*q^2+1778*f*k*n*q^2+10668*b*f*o*q^2-3556*f*i*o*q^2+2667*f*h*p*q^2+1778*b*e^2*f*s-1778*a*e*f*i*s-5334*a*e^2*j*s+8000*a*f*j*n*s+889*f^2*g*p*s+5334*a*f*g*q*s+889*e*f*g*q*s+8001*f^2*o*q*s-10668*a*f*g*p*t-1778*e*f*g*p*t+16001*f^2*o*p*t-1778*a^2*g*q*t+10075*a*e*g*q*t-5334*a*f*o*q*t+1778*e*f*o*q*t+1778*a^2*e^2*u-16001*a^2*f*n*u-5334*a*e*f*n*u+a*b^3-10668*b^3*e-a*b^2*i+7112*b^2*e*i+7112*a*b*i^2-7112*b^2*g*q+15409*b*g*i*q+7112*a*b*k*q+8297*b*e*k*q+8297*a*i*k*q-7112*a^2*m*q+15409*a*e*m*q-10668*b*h*q^2+3556*h*i*q^2+10075*g*k*q^2-5334*b*f*g*s+1778*f*g*i*s-16001*a*g*j*s+2667*e*g*j*s+2667*a*f*k*s-889*e*f*k*s-8000*f*j*o*s-8001*f*h*q*s-16001*f*h*p*t-10075*g^2*q*t+5334*a*h*q*t-1778*e*h*q*t-5334*a^2*g*u-1778*a*e*g*u+16001*a*f*o*u+5334*e*f*o*u+5334*b^2*k-3556*b*i*k+10668*a*b*m-3556*b*e*m-3556*a*i*m-10075*k^2*q+8297*g*m*q+8000*h*j*s+1778*g^2*u-16001*a*h*u-5334*e*h*u+1778*k*m,7112*f^4*p^4+10668*b*f^3*p^3-3556*f^3*i*p^3+10669*a^2*f^2*p*q*s+3555*a*e*f^2*p*q*s+3556*e^2*f^2*p*q*s-14225*a^2*f^2*p^2*t-11852*a*e*f^2*p^2*t+8297*e^2*f^2*p^2*t+15409*a*e^2*f*p*q*t+a*f^2*n*p*q*t-3556*e*f^2*n*p*q*t-3556*f^2*i^2*p^2-7112*a*e^2*j*q*s-a*f*j*n*q*s+3556*f^2*g*p*q*s-10669*a^2*f^2*s^2-3555*a*e*f^2*s^2-3556*e^2*f^2*s^2+8297*b*e^2*f*p*t-8297*a*e*f*i*p*t+13038*f^2*g*p^2*t-15409*a*b*e^2*q*t-8297*a^2*e*i*q*t-7112*b*e*f*n*q*t-10668*a^2*j*n*q*t+7112*a*e*j*n*q*t-14224*a*f*g*p*q*t+8297*e*f*g*p*q*t+10667*f^2*o*p*q*t+11853*a^2*e^2*t^2+10668*a^2*f*n*t^2-3556*a*e*f*n*t^2-a*f^2*n*p*u+10668*e*f^2*n*p*u+10668*f^2*m*p^2+7112*b*e^2*j*s-7112*a*e*i*j*s-10667*a*g*j*q*s+3556*e*g*j*q*s+f*j*o*q*s-3556*f^2*g*s^2+15409*b^2*e^2*t+1185*a*b*e*i*t+15409*a^2*i^2*t+7112*b*f*g*p*t+8297*f*g*i*p*t-3556*a*f*k*p*t+11853*e*f*k*p*t+14224*a*b*g*q*t-8297*b*e*g*q*t-15409*a*g*i*q*t-3556*a^2*k*q*t-8297*a*e*k*q*t-3556*g*j*n*q*t+3556*f*k*n*q*t-10667*b*f*o*q*t-7112*f*i*o*q*t-10667*f*h*p*q*t-3556*a^2*g*t^2-11853*a*e*g*t^2-10668*a*f*o*t^2+3556*e*f*o*t^2+14224*a*b*e^2*u-7112*a^2*e*i*u-10667*b*e*f*n*u+a^2*j*n*u+10667*a*e*j*n*u+b^4-10669*b^3*i+14224*b^2*i^2+10667*b*g*j*s+7112*g*i*j*s+10668*a*j*k*s-3556*e*j*k*s-h*j*q*s-14224*b^2*g*t-1185*b*g*i*t+14224*a*b*k*t-15409*b*e*k*t-15409*a*i*k*t-14224*a^2*m*t-1185*a*e*m*t+10667*b*h*q*t+7112*h*i*q*t-11853*g*k*q*t+11853*g^2*t^2+10668*a*h*t^2-3556*e*h*t^2-10669*a*b*g*u-7112*b*e*g*u+14224*a*g*i*u+10668*a^2*k*u-7112*a*e*k*u+10668*g*j*n*u-10668*f*k*n*u-2*b*f*o*u-10667*f*i*o*u-10667*b^2*m-14224*b*i*m+11853*k^2*t-15409*g*m*t+2*b*h*u+10667*h*i*u+3556*g*k*u+3556*m^2)
     )

yang1 = () -> (
    kk = ZZ/101;
    R1=kk[vars(0..47), MonomialSize=>8];
    J1=ideal"dgjm-chjm-dfkm+bhkm+cflm-bglm-dgin+chin+dekn-ahkn-celn+agln+dfio-bhio-dejo+ahjo+belo-aflo-cfip+bgip+cejp-agjp-bekp+afkp,dgjq-chjq-dfkq+bhkq+cflq-bglq-dgir+chir+dekr-ahkr-celr+aglr+dfis-bhis-dejs+ahjs+bels-afls-cfit+bgit+cejt-agjt-bekt+afkt,dgnq-chnq-dfoq+bhoq+cfpq-bgpq-dgmr+chmr+deor-ahor-cepr+agpr+dfms-bhms-dens+ahns+beps-afps-cfmt+bgmt+cent-agnt-beot+afot,dknq-clnq-djoq+bloq+cjpq-bkpq-dkmr+clmr+dior-alor-cipr+akpr+djms-blms-dins+alns+bips-ajps-cjmt+bkmt+cint-aknt-biot+ajot,hknq-glnq-hjoq+floq+gjpq-fkpq-hkmr+glmr+hior-elor-gipr+ekpr+hjms-flms-hins+elns+fips-ejps-gjmt+fkmt+gint-eknt-fiot+ejot,dgju-chju-dfku+bhku+cflu-bglu-dgiv+chiv+dekv-ahkv-celv+aglv+dfiw-bhiw-dejw+ahjw+belw-aflw-cfix+bgix+cejx-agjx-bekx+afkx,dgnu-chnu-dfou+bhou+cfpu-bgpu-dgmv+chmv+deov-ahov-cepv+agpv+dfmw-bhmw-denw+ahnw+bepw-afpw-cfmx+bgmx+cenx-agnx-beox+afox,dknu-clnu-djou+blou+cjpu-bkpu-dkmv+clmv+diov-alov-cipv+akpv+djmw-blmw-dinw+alnw+bipw-ajpw-cjmx+bkmx+cinx-aknx-biox+ajox,hknu-glnu-hjou+flou+gjpu-fkpu-hkmv+glmv+hiov-elov-gipv+ekpv+hjmw-flmw-hinw+elnw+fipw-ejpw-gjmx+fkmx+ginx-eknx-fiox+ejox,dgru-chru-dfsu+bhsu+cftu-bgtu-dgqv+chqv+desv-ahsv-cetv+agtv+dfqw-bhqw-derw+ahrw+betw-aftw-cfqx+bgqx+cerx-agrx-besx+afsx,dkru-clru-djsu+blsu+cjtu-bktu-dkqv+clqv+disv-alsv-citv+aktv+djqw-blqw-dirw+alrw+bitw-ajtw-cjqx+bkqx+cirx-akrx-bisx+ajsx,hkru-glru-hjsu+flsu+gjtu-fktu-hkqv+glqv+hisv-elsv-gitv+ektv+hjqw-flqw-hirw+elrw+fitw-ejtw-gjqx+fkqx+girx-ekrx-fisx+ejsx,doru-cpru-dnsu+bpsu+cntu-botu-doqv+cpqv+dmsv-apsv-cmtv+aotv+dnqw-bpqw-dmrw+aprw+bmtw-antw-cnqx+boqx+cmrx-aorx-bmsx+ansx,horu-gpru-hnsu+fpsu+gntu-fotu-hoqv+gpqv+hmsv-epsv-gmtv+eotv+hnqw-fpqw-hmrw+eprw+fmtw-entw-gnqx+foqx+gmrx-eorx-fmsx+ensx,loru-kpru-lnsu+jpsu+kntu-jotu-loqv+kpqv+lmsv-ipsv-kmtv+iotv+lnqw-jpqw-lmrw+iprw+jmtw-intw-knqx+joqx+kmrx-iorx-jmsx+insx,ay+bz+cA+dB,ey+fz+gA+hB,iy+jz+kA+lB,my+nz+oA+pB,qy+rz+sA+tB,uy+vz+wA+xB,aC+bD+cE+dF,eC+fD+gE+hF,iC+jD+kE+lF,mC+nD+oE+pF,qC+rD+sE+tF,uC+vD+wE+xF,aG+bH+cI+dJ,eG+fH+gI+hJ,iG+jH+kI+lJ,mG+nH+oI+pJ,qG+rH+sI+tJ,uG+vH+wI+xJ,aK+bL+cM+dN,eK+fL+gM+hN,iK+jL+kM+lN,mK+nL+oM+pN,qK+rL+sM+tN,uK+vL+wM+xN,BEHK-AFHK-BDIK+zFIK+ADJK-zEJK-BEGL+AFGL+BCIL-yFIL-ACJL+yEJL+BDGM-zFGM-BCHM+yFHM+zCJM-yDJM-ADGN+zEGN+ACHN-yEHN-zCIN+yDIN,aO+bP+cQ+dR,eO+fP+gQ+hR,iO+jP+kQ+lR,mO+nP+oQ+pR,qO+rP+sQ+tR,uO+vP+wQ+xR,BEHO-AFHO-BDIO+zFIO+ADJO-zEJO-BEGP+AFGP+BCIP-yFIP-ACJP+yEJP+BDGQ-zFGQ-BCHQ+yFHQ+zCJQ-yDJQ-ADGR+zEGR+ACHR-yEHR-zCIR+yDIR,BELO-AFLO-BDMO+zFMO+ADNO-zENO-BEKP+AFKP+BCMP-yFMP-ACNP+yENP+BDKQ-zFKQ-BCLQ+yFLQ+zCNQ-yDNQ-ADKR+zEKR+ACLR-yELR-zCMR+yDMR,BILO-AJLO-BHMO+zJMO+AHNO-zINO-BIKP+AJKP+BGMP-yJMP-AGNP+yINP+BHKQ-zJKQ-BGLQ+yJLQ+zGNQ-yHNQ-AHKR+zIKR+AGLR-yILR-zGMR+yHMR,FILO-EJLO-FHMO+DJMO+EHNO-DINO-FIKP+EJKP+FGMP-CJMP-EGNP+CINP+FHKQ-DJKQ-FGLQ+CJLQ+DGNQ-CHNQ-EHKR+DIKR+EGLR-CILR-DGMR+CHMR,aS+bT+cU+dV,eS+fT+gU+hV,iS+jT+kU+lV,mS+nT+oU+pV,qS+rT+sU+tV,uS+vT+wU+xV,BEHS-AFHS-BDIS+zFIS+ADJS-zEJS-BEGT+AFGT+BCIT-yFIT-ACJT+yEJT+BDGU-zFGU-BCHU+yFHU+zCJU-yDJU-ADGV+zEGV+ACHV-yEHV-zCIV+yDIV,BELS-AFLS-BDMS+zFMS+ADNS-zENS-BEKT+AFKT+BCMT-yFMT-ACNT+yENT+BDKU-zFKU-BCLU+yFLU+zCNU-yDNU-ADKV+zEKV+ACLV-yELV-zCMV+yDMV,BILS-AJLS-BHMS+zJMS+AHNS-zINS-BIKT+AJKT+BGMT-yJMT-AGNT+yINT+BHKU-zJKU-BGLU+yJLU+zGNU-yHNU-AHKV+zIKV+AGLV-yILV-zGMV+yHMV,FILS-EJLS-FHMS+DJMS+EHNS-DINS-FIKT+EJKT+FGMT-CJMT-EGNT+CINT+FHKU-DJKU-FGLU+CJLU+DGNU-CHNU-EHKV+DIKV+EGLV-CILV-DGMV+CHMV,BEPS-AFPS-BDQS+zFQS+ADRS-zERS-BEOT+AFOT+BCQT-yFQT-ACRT+yERT+BDOU-zFOU-BCPU+yFPU+zCRU-yDRU-ADOV+zEOV+ACPV-yEPV-zCQV+yDQV,BIPS-AJPS-BHQS+zJQS+AHRS-zIRS-BIOT+AJOT+BGQT-yJQT-AGRT+yIRT+BHOU-zJOU-BGPU+yJPU+zGRU-yHRU-AHOV+zIOV+AGPV-yIPV-zGQV+yHQV,FIPS-EJPS-FHQS+DJQS+EHRS-DIRS-FIOT+EJOT+FGQT-CJQT-EGRT+CIRT+FHOU-DJOU-FGPU+CJPU+DGRU-CHRU-EHOV+DIOV+EGPV-CIPV-DGQV+CHQV,BMPS-ANPS-BLQS+zNQS+ALRS-zMRS-BMOT+ANOT+BKQT-yNQT-AKRT+yMRT+BLOU-zNOU-BKPU+yNPU+zKRU-yLRU-ALOV+zMOV+AKPV-yMPV-zKQV+yLQV,FMPS-ENPS-FLQS+DNQS+ELRS-DMRS-FMOT+ENOT+FKQT-CNQT-EKRT+CMRT+FLOU-DNOU-FKPU+CNPU+DKRU-CLRU-ELOV+DMOV+EKPV-CMPV-DKQV+CLQV,JMPS-INPS-JLQS+HNQS+ILRS-HMRS-JMOT+INOT+JKQT-GNQT-IKRT+GMRT+JLOU-HNOU-JKPU+GNPU+HKRU-GLRU-ILOV+HMOV+IKPV-GMPV-HKQV+GLQV";
    J1)

--joswig-101
--Systems of this type (we can produce more, if you like this one)
--occur in http://front.math.ucdavis.edu/math.CO/0508180.  This paper
--is to appear in Adv. Math. bugs/1-joswig
-- Currently doable.  The subring contains one element
-- of degree 444.
joswig101 = () -> (
    R1 := ZZ/101[x4,x3,x2,x1,s,t,MonomialOrder=>Eliminate 4];
    ideal (
      1 + s^2  * x1 * x3 + s^8 * x2 * x3 + s^19 * x1 * x2 * x4,
      x1 + s^8 * x1 * x2 * x3 + s^19 * x2 * x4,
      x2 + s^10 * x3 * x4 + s^11 * x1 * x4,
      x3 + s^4 * x1 * x2 + s^19 * x1 * x3 * x4 + s^24 * x2 * x3 * x4,
      x4 + s^31 * x1 * x2 * x3 * x4
      ))

hilbertkunz1 = () -> (
     R1 := ZZ/5[a..d,MonomialSize=>16];
     J1 := ideal{a^3-2*a^2*b-a*b^2-2*b^3+a*b*c-2*b^2*c+2*a*c^2-2*b*c^2-c^3+2*a*b*d
	  -2*b^2*d-a*c*d-2*b*c*d-2*c^2*d+a*d^2+c*d^2-d^3, a^125, b^125,
	  c^125, d^25};
     J1
     )

hilbertkunz2 = () -> (
     R1 := ZZ/5[a..d,MonomialSize=>16];
     J1 := ideal{a^3-2*a^2*b-a*b^2-2*b^3+a*b*c-2*b^2*c+2*a*c^2-2*b*c^2-c^3+2*a*b*d
	  -2*b^2*d-a*c*d-2*b*c*d-2*c^2*d+a*d^2+c*d^2-d^3, a^125, b^125,
	  c^125, d^125};
     J1
     )

random5556 = () -> (
     kk = ZZ/101;
     R1 = kk[a..g, MonomialSize=>8];
     J1 = ideal(50*a^5+12*a^4*b-45*a^3*b^2+29*a^2*b^3+7*a*b^4-8*b^5+26*a^4*c+7*a^3*b*c+21*a^2*b^2*c+48*a*b^3*c+22*b^4*c+28*a^3*c^2-23*a^2*b*c^2-34*a*b^2*c^2-19*b^3*c^2-46*a^2*c^3-14*a*b*c^3+46*b^2*c^3+16*a*c^4+4*b*c^4+36*c^5+37*a^4*d-30*a^3*b*d-11*a^2*b^2*d+7*a*b^3*d+22*b^4*d+34*a^3*c*d-7*a^2*b*c*d+36*a*b^2*c*d+19*b^3*c*d-38*a^2*c^2*d+16*a*b*c^2*d-6*b^2*c^2*d-37*a*c^3*d-27*b*c^3*d-27*c^4*d-50*a^3*d^2-33*a^2*b*d^2+31*a*b^2*d^2-34*b^3*d^2-11*a^2*c*d^2+31*a*b*c*d^2+17*b^2*c*d^2-48*a*c^2*d^2+b*c^2*d^2+17*c^3*d^2+20*a^2*d^3-43*a*b*d^3+30*a*c*d^3+21*b*c*d^3+34*c^2*d^3+18*a*d^4+17*b*d^4-43*c*d^4-25*d^5-28*a^4*e+34*a^3*b*e-44*a^2*b^2*e-8*a*b^3*e+10*b^4*e+32*a^3*c*e-a^2*b*c*e+29*a*b^2*c*e-15*b^3*c*e+42*a^2*c^2*e+28*a*b*c^2*e-23*b^2*c^2*e-27*a*c^3*e+29*b*c^3*e+38*c^4*e-29*a^3*d*e-25*a^2*b*d*e-38*a*b^2*d*e-43*b^3*d*e+30*a^2*c*d*e-34*a*b*c*d*e-31*b^2*c*d*e+35*a*c^2*d*e+49*b*c^2*d*e+10*c^3*d*e-20*a^2*d^2*e+20*a*b*d^2*e+40*b^2*d^2*e-46*a*c*d^2*e-25*b*c*d^2*e-20*c^2*d^2*e-23*a*d^3*e+36*b*d^3*e-38*c*d^3*e-3*d^4*e-31*a^3*e^2+22*a^2*b*e^2-a*b^2*e^2+32*b^3*e^2-9*a^2*c*e^2-2*a*b*c*e^2+44*b^2*c*e^2-40*a*c^2*e^2+44*b*c^2*e^2+7*c^3*e^2+32*a^2*d*e^2+11*a*b*d*e^2-3*b^2*d*e^2-11*a*c*d*e^2-47*b*c*d*e^2+6*c^2*d*e^2-16*a*d^2*e^2-3*b*d^2*e^2-12*c*d^2*e^2+35*d^3*e^2+10*a^2*e^3-48*a*b*e^3+44*b^2*e^3+31*a*c*e^3-47*b*c*e^3-40*c^2*e^3-50*a*d*e^3+18*b*d*e^3-33*c*d*e^3+5*d^2*e^3+43*a*e^4-3*b*e^4+35*c*e^4+34*d*e^4+27*e^5+33*a^4*f+35*a^3*b*f-41*a^2*b^2*f-7*a*b^3*f+26*b^4*f+48*a^3*c*f+30*a^2*b*c*f+16*a*b^2*c*f-13*b^3*c*f-40*a^2*c^2*f-33*a*b*c^2*f-33*b^2*c^2*f+23*a*c^3*f-34*b*c^3*f+42*c^4*f-36*a^3*d*f+2*a^2*b*d*f-23*a*b^2*d*f-13*b^3*d*f-19*a^2*c*d*f+21*a*b*c*d*f-15*b^2*c*d*f+31*a*c^2*d*f-22*b*c^2*d*f-41*c^3*d*f-29*a^2*d^2*f+47*a*b*d^2*f+14*b^2*d^2*f-46*a*c*d^2*f-28*b*c*d^2*f+42*c^2*d^2*f+9*a*d^3*f+35*b*d^3*f-25*c*d^3*f+22*d^4*f-16*a^3*e*f-35*a^2*b*e*f+50*a*b^2*e*f-45*b^3*e*f-14*a^2*c*e*f+11*a*b*c*e*f+14*b^2*c*e*f-7*a*c^2*e*f-37*b*c^2*e*f-34*c^3*e*f-23*a^2*d*e*f-27*a*b*d*e*f-24*b^2*d*e*f+50*a*c*d*e*f-33*b*c*d*e*f+37*c^2*d*e*f+45*a*d^2*e*f+12*b*d^2*e*f+4*c*d^2*e*f-37*d^3*e*f+41*a^2*e^2*f+12*a*b*e^2*f+39*b^2*e^2*f-3*a*c*e^2*f-36*b*c*e^2*f-8*c^2*e^2*f-14*a*d*e^2*f-28*b*d*e^2*f+25*c*d*e^2*f-22*d^2*e^2*f+43*a*e^3*f+6*b*e^3*f+29*c*e^3*f-13*d*e^3*f+44*e^4*f-29*a^3*f^2+20*a^2*b*f^2+28*a*b^2*f^2-45*b^3*f^2-28*a^2*c*f^2+16*a*b*c*f^2+24*b^2*c*f^2-48*a*c^2*f^2+48*b*c^2*f^2+7*c^3*f^2+15*a^2*d*f^2+14*a*b*d*f^2-21*b^2*d*f^2+18*a*c*d*f^2-3*b*c*d*f^2-47*c^2*d*f^2-15*a*d^2*f^2-11*b*d^2*f^2-5*c*d^2*f^2-27*d^3*f^2+28*a^2*e*f^2-21*a*b*e*f^2+4*b^2*e*f^2+32*a*c*e*f^2-15*b*c*e*f^2-43*c^2*e*f^2+26*a*d*e*f^2-42*b*d*e*f^2-46*c*d*e*f^2+32*d^2*e*f^2-35*a*e^2*f^2+b*e^2*f^2-4*c*e^2*f^2-36*d*e^2*f^2-36*e^3*f^2+48*a^2*f^3+42*a*b*f^3+6*b^2*f^3+36*a*c*f^3-14*b*c*f^3+37*c^2*f^3-25*a*d*f^3-9*b*d*f^3-37*c*d*f^3+49*d^2*f^3+40*a*e*f^3+49*b*e*f^3-15*c*e*f^3+49*d*e*f^3-43*e^2*f^3-17*a*f^4+31*b*f^4-23*c*f^4-50*d*f^4+50*e*f^4+23*f^5+44*a^4*g+5*a^3*b*g+22*a^2*b^2*g+38*a*b^3*g-14*b^4*g-7*a^3*c*g+19*a^2*b*c*g-41*a*b^2*c*g-14*b^3*c*g-7*a^2*c^2*g+50*a*b*c^2*g+17*b^2*c^2*g-17*a*c^3*g+b*c^3*g+26*c^4*g-26*a^3*d*g+6*a^2*b*d*g-19*a*b^2*d*g-15*b^3*d*g+32*a^2*c*d*g-43*a*b*c*d*g-41*b^2*c*d*g+35*a*c^2*d*g+26*b*c^2*d*g+46*c^3*d*g+31*a^2*d^2*g+41*a*b*d^2*g-14*b^2*d^2*g-30*a*c*d^2*g+28*b*c*d^2*g+12*c^2*d^2*g-44*b*d^3*g+5*c*d^3*g-18*d^4*g-9*a^3*e*g-19*a^2*b*e*g+4*a*b^2*e*g-13*b^3*e*g+7*a^2*c*e*g-2*a*b*c*e*g-13*b^2*c*e*g+16*a*c^2*e*g+4*b*c^2*e*g-11*c^3*e*g+28*a^2*d*e*g-33*a*b*d*e*g+27*b^2*d*e*g+26*a*c*d*e*g-20*b*c*d*e*g-18*c^2*d*e*g-14*a*d^2*e*g+21*b*d^2*e*g+46*c*d^2*e*g-32*d^3*e*g+2*a^2*e^2*g-3*a*b*e^2*g-45*b^2*e^2*g+17*b*c*e^2*g+20*c^2*e^2*g+26*a*d*e^2*g+42*b*d*e^2*g+29*c*d*e^2*g+5*d^2*e^2*g+5*a*e^3*g+30*b*e^3*g-17*c*e^3*g-6*d*e^3*g-25*e^4*g+33*a^3*f*g+44*a^2*b*f*g+13*a*b^2*f*g+48*b^3*f*g+17*a^2*c*f*g-37*a*b*c*f*g-10*b^2*c*f*g-27*a*c^2*f*g+23*b*c^2*f*g-46*c^3*f*g+26*a^2*d*f*g-29*a*b*d*f*g+17*b^2*d*f*g+49*a*c*d*f*g+44*b*c*d*f*g-49*c^2*d*f*g-12*a*d^2*f*g-30*b*d^2*f*g+30*c*d^2*f*g-13*d^3*f*g+12*a^2*e*f*g+4*a*b*e*f*g+12*b^2*e*f*g-47*a*c*e*f*g-38*b*c*e*f*g-24*c^2*e*f*g+21*a*d*e*f*g-36*b*d*e*f*g+14*c*d*e*f*g-18*d^2*e*f*g-15*a*e^2*f*g-7*b*e^2*f*g-44*c*e^2*f*g-9*d*e^2*f*g-14*e^3*f*g+5*a^2*f^2*g-27*b^2*f^2*g+11*a*c*f^2*g+25*b*c*f^2*g-2*c^2*f^2*g+30*a*d*f^2*g-33*b*d*f^2*g-33*c*d*f^2*g-5*d^2*f^2*g+38*a*e*f^2*g-24*b*e*f^2*g-12*c*e*f^2*g+40*d*e*f^2*g-3*e^2*f^2*g+36*a*f^3*g+4*b*f^3*g-45*c*f^3*g+45*d*f^3*g-41*e*f^3*g-50*f^4*g-45*a^3*g^2+5*a^2*b*g^2-47*a*b^2*g^2-12*b^3*g^2-44*a^2*c*g^2+35*a*b*c*g^2+27*b^2*c*g^2-a*c^2*g^2-42*b*c^2*g^2-39*c^3*g^2-23*a^2*d*g^2+16*a*b*d*g^2+3*b^2*d*g^2+39*a*c*d*g^2-46*b*c*d*g^2+42*c^2*d*g^2+8*a*d^2*g^2+19*b*d^2*g^2-14*c*d^2*g^2+8*d^3*g^2+14*a^2*e*g^2+44*a*b*e*g^2+3*b^2*e*g^2+45*a*c*e*g^2+36*b*c*e*g^2-50*c^2*e*g^2+13*a*d*e*g^2+24*b*d*e*g^2-21*c*d*e*g^2+d^2*e*g^2-2*a*e^2*g^2+11*b*e^2*g^2+7*c*e^2*g^2+45*d*e^2*g^2+e^3*g^2-38*a^2*f*g^2-33*a*b*f*g^2-25*b^2*f*g^2+21*a*c*f*g^2+32*b*c*f*g^2+42*c^2*f*g^2-15*a*d*f*g^2+14*b*d*f*g^2+28*c*d*f*g^2+12*d^2*f*g^2+50*a*e*f*g^2+16*b*e*f*g^2+38*c*e*f*g^2-10*d*e*f*g^2-18*e^2*f*g^2-24*a*f^2*g^2+48*b*f^2*g^2+20*c*f^2*g^2-48*d*f^2*g^2-31*e*f^2*g^2-12*f^3*g^2-5*a^2*g^3+28*a*b*g^3-42*b^2*g^3+35*a*c*g^3-27*b*c*g^3+26*c^2*g^3-33*a*d*g^3-26*b*d*g^3-21*c*d*g^3+47*d^2*g^3+14*a*e*g^3+24*c*e*g^3+4*d*e*g^3-40*e^2*g^3-3*a*f*g^3-30*b*f*g^3+38*c*f*g^3-2*d*f*g^3-46*e*f*g^3+16*f^2*g^3-28*a*g^4-9*b*g^4+35*c*g^4-13*d*g^4-23*e*g^4+30*f*g^4-16*g^5,29*a^5+47*a^4*b+2*a^3*b^2+40*a^2*b^3+5*a*b^4-22*b^5+41*a^4*c-41*a^3*b*c+22*a^2*b^2*c+46*a*b^3*c+33*b^4*c-8*a^3*c^2-45*a^2*b*c^2-a*b^2*c^2+30*b^3*c^2+39*a^2*c^3+4*a*b*c^3-11*b^2*c^3+19*a*c^4+22*b*c^4-44*c^5+46*a^4*d-10*a^3*b*d-37*a^2*b^2*d-14*a*b^3*d-15*b^4*d-42*a^3*c*d+43*a^2*b*c*d-44*a*b^2*c*d+39*b^3*c*d+36*a^2*c^2*d+48*a*b*c^2*d-30*b^2*c^2*d+9*a*c^3*d-44*b*c^3*d-40*c^4*d-12*a^3*d^2-13*a^2*b*d^2+15*a*b^2*d^2-19*b^3*d^2-33*a^2*c*d^2+11*a*b*c*d^2+15*b^2*c*d^2+48*a*c^2*d^2-44*b*c^2*d^2-32*c^3*d^2-38*a^2*d^3-18*a*b*d^3-45*b^2*d^3+44*a*c*d^3+48*b*c*d^3-45*c^2*d^3+12*a*d^4-23*b*d^4-17*c*d^4-14*d^5+29*a^3*b*e-26*a^2*b^2*e-13*a*b^3*e+46*b^4*e+43*a^3*c*e+6*a^2*b*c*e+43*a*b^2*c*e-28*b^3*c*e-45*a^2*c^2*e-29*a*b*c^2*e+12*b^2*c^2*e-22*a*c^3*e+6*b*c^3*e-35*c^4*e+4*a^3*d*e-35*a^2*b*d*e+39*a*b^2*d*e+36*b^3*d*e-20*a^2*c*d*e+3*a*b*c*d*e-7*b^2*c*d*e+44*a*c^2*d*e+9*b*c^2*d*e+22*c^3*d*e+5*a^2*d^2*e-41*a*b*d^2*e-43*b^2*d^2*e-8*a*c*d^2*e+44*b*c*d^2*e-24*c^2*d^2*e+49*a*d^3*e-14*b*d^3*e-31*c*d^3*e-42*d^4*e+9*a^3*e^2+39*a^2*b*e^2+35*a*b^2*e^2-46*b^3*e^2-40*a^2*c*e^2-19*a*b*c*e^2+32*b^2*c*e^2-6*a*c^2*e^2+6*b*c^2*e^2-2*c^3*e^2+29*a^2*d*e^2-16*a*b*d*e^2+22*b^2*d*e^2+19*a*c*d*e^2+5*b*c*d*e^2+11*c^2*d*e^2-34*b*d^2*e^2+25*c*d^2*e^2+21*d^3*e^2-23*a^2*e^3+35*a*b*e^3+6*b^2*e^3-39*a*c*e^3-5*b*c*e^3+7*c^2*e^3-41*a*d*e^3+35*b*d*e^3-42*c*d*e^3+26*d^2*e^3-15*a*e^4+26*b*e^4+17*c*e^4-7*d*e^4-48*e^5+33*a^4*f+17*a^2*b^2*f-32*a*b^3*f-8*b^4*f-21*a^2*b*c*f+16*a*b^2*c*f+36*b^3*c*f+34*a^2*c^2*f-36*a*b*c^2*f+19*b^2*c^2*f-50*a*c^3*f+34*b*c^3*f+3*c^4*f-23*a^3*d*f+45*a^2*b*d*f-19*a*b^2*d*f+38*b^3*d*f-43*a^2*c*d*f-49*a*b*c*d*f-31*b^2*c*d*f-43*a*c^2*d*f+9*b*c^2*d*f-23*c^3*d*f-23*a^2*d^2*f-23*a*b*d^2*f+37*b^2*d^2*f+10*a*c*d^2*f+48*b*c*d^2*f-9*c^2*d^2*f+27*a*d^3*f+41*b*d^3*f+40*c*d^3*f+50*d^4*f-7*a^2*b*e*f-24*a*b^2*e*f-3*b^3*e*f+12*a^2*c*e*f+6*a*b*c*e*f+40*b^2*c*e*f-30*a*c^2*e*f+40*b*c^2*e*f-9*c^3*e*f-22*a^2*d*e*f+6*a*b*d*e*f-30*b^2*d*e*f-2*a*c*d*e*f-42*b*c*d*e*f-18*c^2*d*e*f+8*a*d^2*e*f-50*b*d^2*e*f+5*c*d^2*e*f+10*d^3*e*f+8*a^2*e^2*f-10*a*b*e^2*f-29*b^2*e^2*f-28*a*c*e^2*f+26*b*c*e^2*f-42*c^2*e^2*f-29*a*d*e^2*f+23*b*d*e^2*f-38*c*d*e^2*f+47*d^2*e^2*f-40*a*e^3*f-14*b*e^3*f-46*c*e^3*f+29*d*e^3*f-19*e^4*f-12*a^3*f^2-17*a^2*b*f^2-36*a*b^2*f^2+29*b^3*f^2+17*a^2*c*f^2-23*a*b*c*f^2+36*b^2*c*f^2+28*a*c^2*f^2+b*c^2*f^2+21*c^3*f^2+50*a^2*d*f^2+43*a*b*d*f^2-27*b^2*d*f^2+42*a*c*d*f^2-22*b*c*d*f^2-38*c^2*d*f^2+27*a*d^2*f^2-20*b*d^2*f^2-13*c*d^2*f^2+38*d^3*f^2+27*a^2*e*f^2+15*a*b*e*f^2-46*b^2*e*f^2+3*a*c*e*f^2+45*b*c*e*f^2-9*c^2*e*f^2+19*a*d*e*f^2-14*b*d*e*f^2+6*c*d*e*f^2+34*d^2*e*f^2+42*a*e^2*f^2-36*b*e^2*f^2+c*e^2*f^2-7*d*e^2*f^2+32*e^3*f^2+30*a^2*f^3+49*a*b*f^3+28*b^2*f^3+11*a*c*f^3-17*b*c*f^3-40*c^2*f^3-12*a*d*f^3-14*b*d*f^3-21*c*d*f^3-29*d^2*f^3-44*a*e*f^3-2*b*e*f^3-32*c*e*f^3-2*d*e*f^3-13*e^2*f^3-30*a*f^4-46*b*f^4-39*c*f^4-36*d*f^4-5*e*f^4-26*f^5-34*a^4*g+50*a^3*b*g-33*a^2*b^2*g-6*a*b^3*g-41*b^4*g-40*a^3*c*g-2*a^2*b*c*g-40*a*b^2*c*g-8*b^3*c*g-26*a^2*c^2*g-8*a*b*c^2*g-38*b^2*c^2*g+47*a*c^3*g-49*b*c^3*g-40*c^4*g+6*a^3*d*g-28*a^2*b*d*g+35*a*b^2*d*g-20*b^3*d*g-43*a^2*c*d*g+24*a*b*c*d*g+28*b^2*c*d*g+8*a*c^2*d*g-16*b*c^2*d*g+2*c^3*d*g+5*a^2*d^2*g-21*a*b*d^2*g-25*b^2*d^2*g-15*a*c*d^2*g+30*b*c*d^2*g-43*c^2*d^2*g-25*a*d^3*g-49*b*d^3*g+24*c*d^3*g+28*d^4*g+46*a^3*e*g+38*a^2*b*e*g-47*a*b^2*e*g-42*b^3*e*g-25*a^2*c*e*g-22*a*b*c*e*g-45*b^2*c*e*g+32*a*c^2*e*g+31*b*c^2*e*g+25*c^3*e*g+9*a*b*d*e*g-27*b^2*d*e*g+7*a*c*d*e*g-43*b*c*d*e*g-30*c^2*d*e*g-19*a*d^2*e*g+27*b*d^2*e*g+17*c*d^2*e*g+38*d^3*e*g-6*a^2*e^2*g-37*a*b*e^2*g-28*a*c*e^2*g-30*b*c*e^2*g+8*c^2*e^2*g-34*a*d*e^2*g-11*b*d*e^2*g+39*c*d*e^2*g-31*d^2*e^2*g-5*a*e^3*g-42*b*e^3*g+12*c*e^3*g+46*d*e^3*g+30*e^4*g-8*a^3*f*g+20*a^2*b*f*g+42*a*b^2*f*g-43*b^3*f*g+13*a^2*c*f*g+38*a*b*c*f*g+46*b^2*c*f*g+15*a*c^2*f*g-8*b*c^2*f*g-3*c^3*f*g-21*a^2*d*f*g-41*a*b*d*f*g+34*b^2*d*f*g-15*a*c*d*f*g-28*b*c*d*f*g-18*c^2*d*f*g-21*a*d^2*f*g+35*b*d^2*f*g-31*c*d^2*f*g+47*d^3*f*g+37*a^2*e*f*g-35*a*b*e*f*g-18*b^2*e*f*g-34*a*c*e*f*g-24*b*c*e*f*g+27*c^2*e*f*g+45*a*d*e*f*g+29*b*d*e*f*g-41*c*d*e*f*g+16*d^2*e*f*g+44*a*e^2*f*g+26*b*e^2*f*g-49*c*e^2*f*g+26*d*e^2*f*g+44*e^3*f*g+50*a^2*f^2*g-21*a*b*f^2*g-21*b^2*f^2*g-42*a*c*f^2*g-35*b*c*f^2*g-8*c^2*f^2*g-42*a*d*f^2*g-3*b*d*f^2*g+7*c*d*f^2*g+9*d^2*f^2*g-23*a*e*f^2*g-35*b*e*f^2*g-12*c*e*f^2*g+38*d*e*f^2*g-34*e^2*f^2*g+31*a*f^3*g+41*b*f^3*g+19*c*f^3*g-41*d*f^3*g-35*e*f^3*g-37*f^4*g-6*a^3*g^2+49*a^2*b*g^2+27*a*b^2*g^2-46*b^3*g^2-35*a^2*c*g^2+12*a*b*c*g^2-39*b^2*c*g^2+12*a*c^2*g^2+23*b*c^2*g^2-48*c^3*g^2+42*a^2*d*g^2+20*a*b*d*g^2-29*b^2*d*g^2+25*a*c*d*g^2+15*b*c*d*g^2+22*c^2*d*g^2+15*a*d^2*g^2+21*b*d^2*g^2+39*c*d^2*g^2+18*d^3*g^2-12*a^2*e*g^2+4*a*b*e*g^2+13*b^2*e*g^2-33*a*c*e*g^2+15*b*c*e*g^2+c^2*e*g^2-14*a*d*e*g^2+20*b*d*e*g^2-49*c*d*e*g^2+d^2*e*g^2-18*a*e^2*g^2+10*b*e^2*g^2-47*c*e^2*g^2+28*d*e^2*g^2-19*e^3*g^2+38*a^2*f*g^2-17*a*b*f*g^2+29*b^2*f*g^2+26*a*c*f*g^2+18*b*c*f*g^2-12*c^2*f*g^2+13*a*d*f*g^2+38*b*d*f*g^2-21*c*d*f*g^2+18*a*e*f*g^2-46*b*e*f*g^2+31*c*e*f*g^2-33*d*e*f*g^2-49*e^2*f*g^2+15*a*f^2*g^2+39*b*f^2*g^2-9*c*f^2*g^2-49*d*f^2*g^2+32*e*f^2*g^2+19*f^3*g^2+33*a^2*g^3-2*a*b*g^3+39*b^2*g^3+38*a*c*g^3-46*b*c*g^3+29*a*d*g^3+44*b*d*g^3+22*c*d*g^3+42*d^2*g^3+17*a*e*g^3+50*b*e*g^3-37*c*e*g^3-21*d*e*g^3+8*e^2*g^3+25*a*f*g^3+28*b*f*g^3+29*d*f*g^3-23*e*f*g^3-19*f^2*g^3-14*a*g^4+4*b*g^4-18*c*g^4+47*d*g^4-13*e*g^4-36*f*g^4+13*g^5,-16*a^5-48*a^4*b+14*a^3*b^2+46*a^2*b^3-42*a*b^4-43*b^5-49*a^4*c-7*a^3*b*c+38*a^2*b^2*c+9*a*b^3*c+33*b^4*c+47*a^3*c^2+38*a^2*b*c^2+5*a*b^2*c^2+45*b^3*c^2+8*a^2*c^3-12*a*b*c^3-44*b^2*c^3-36*a*c^4+12*b*c^4-32*c^5-22*a^4*d-30*a^3*b*d-36*a^2*b^2*d+40*a*b^3*d-8*b^4*d-34*a^3*c*d+29*a^2*b*c*d-31*a*b^2*c*d-29*b^3*c*d-17*a^2*c^2*d-18*a*b*c^2*d+20*b^2*c^2*d+8*a*c^3*d-14*b*c^3*d-20*c^4*d-28*a^3*d^2+42*a^2*b*d^2-14*a*b^2*d^2+38*b^3*d^2+47*a^2*c*d^2+27*a*b*c*d^2-49*b^2*c*d^2+39*a*c^2*d^2-42*b*c^2*d^2-48*c^3*d^2-3*a^2*d^3-25*a*b*d^3-5*b^2*d^3+48*a*c*d^3+48*b*c*d^3+12*c^2*d^3+21*a*d^4+49*b*d^4+41*c*d^4-46*d^5-30*a^4*e-26*a^3*b*e-9*a^2*b^2*e+19*a*b^3*e+34*b^4*e+16*a^3*c*e+13*a^2*b*c*e+7*a*b^2*c*e+49*b^3*c*e+40*a^2*c^2*e+37*a*b*c^2*e-14*b^2*c^2*e+22*a*c^3*e-45*b*c^3*e+20*c^4*e+40*a^3*d*e-41*a^2*b*d*e+5*a*b^2*d*e-20*b^3*d*e-41*a^2*c*d*e-4*a*b*c*d*e+15*b^2*c*d*e+14*a*c^2*d*e+35*b*c^2*d*e+6*c^3*d*e-2*a^2*d^2*e+26*a*b*d^2*e-18*b^2*d^2*e+6*a*c*d^2*e+39*b*c*d^2*e+50*c^2*d^2*e-49*a*d^3*e-36*b*d^3*e+37*c*d^3*e+6*d^4*e+9*a^3*e^2-18*a^2*b*e^2+46*a*b^2*e^2-29*b^3*e^2-25*a^2*c*e^2-7*a*b*c*e^2-26*b^2*c*e^2-10*a*c^2*e^2-45*b*c^2*e^2-18*c^3*e^2+19*a^2*d*e^2+35*a*b*d*e^2+44*b^2*d*e^2+37*a*c*d*e^2-50*c^2*d*e^2-42*a*d^2*e^2+21*b*d^2*e^2-16*c*d^2*e^2-11*d^3*e^2+20*a^2*e^3-15*a*b*e^3+41*b^2*e^3-30*a*c*e^3-25*b*c*e^3-39*c^2*e^3+42*a*d*e^3+31*b*d*e^3+15*c*d*e^3+13*d^2*e^3-44*a*e^4-50*b*e^4+46*c*e^4+41*d*e^4-30*e^5+6*a^4*f-a^3*b*f-39*a^2*b^2*f-7*a*b^3*f-18*b^4*f+23*a^3*c*f+20*a^2*b*c*f+2*a*b^2*c*f-21*b^3*c*f-40*a^2*c^2*f-17*a*b*c^2*f-28*b^2*c^2*f+26*a*c^3*f-34*b*c^3*f+8*c^4*f+31*a^3*d*f+7*a^2*b*d*f+38*a*b^2*d*f+26*b^3*d*f-34*a^2*c*d*f-11*a*b*c*d*f+35*b^2*c*d*f-33*a*c^2*d*f-15*b*c^2*d*f-6*c^3*d*f+45*a^2*d^2*f-37*a*b*d^2*f+11*b^2*d^2*f+2*a*c*d^2*f+30*b*c*d^2*f+33*c^2*d^2*f+32*a*d^3*f-46*b*d^3*f-39*c*d^3*f-19*d^4*f-44*a^3*e*f+31*a^2*b*e*f-6*a*b^2*e*f+47*b^3*e*f-35*a^2*c*e*f-10*a*b*c*e*f+33*b^2*c*e*f+33*a*c^2*e*f-17*b*c^2*e*f-13*c^3*e*f-49*a^2*d*e*f-26*a*b*d*e*f-17*b^2*d*e*f-25*a*c*d*e*f-47*b*c*d*e*f-6*c^2*d*e*f-36*a*d^2*e*f+44*b*d^2*e*f-8*c*d^2*e*f-47*d^3*e*f-42*a^2*e^2*f-31*a*b*e^2*f+11*b^2*e^2*f+38*a*c*e^2*f-10*b*c*e^2*f-21*c^2*e^2*f-3*a*d*e^2*f-5*b*d*e^2*f+9*c*d*e^2*f+31*d^2*e^2*f-28*a*e^3*f-3*b*e^3*f-14*c*e^3*f+17*d*e^3*f-39*e^4*f-44*a^3*f^2+a^2*b*f^2+21*a*b^2*f^2-19*b^3*f^2+20*a^2*c*f^2+5*a*b*c*f^2+18*b^2*c*f^2+6*a*c^2*f^2-5*b*c^2*f^2-13*c^3*f^2-33*a^2*d*f^2-20*a*b*d*f^2-10*b^2*d*f^2+50*b*c*d*f^2-6*c^2*d*f^2+8*a*d^2*f^2-11*b*d^2*f^2+24*c*d^2*f^2+6*d^3*f^2-6*a^2*e*f^2-6*a*b*e*f^2-20*b^2*e*f^2-30*a*c*e*f^2+23*b*c*e*f^2-13*c^2*e*f^2+46*a*d*e*f^2-38*b*d*e*f^2-36*c*d*e*f^2-17*d^2*e*f^2+14*a*e^2*f^2+35*b*e^2*f^2+48*c*e^2*f^2-36*d*e^2*f^2+3*e^3*f^2-8*a^2*f^3+9*a*b*f^3-34*b^2*f^3-31*a*c*f^3+26*b*c*f^3-c^2*f^3+49*a*d*f^3+5*b*d*f^3+30*c*d*f^3+49*d^2*f^3-6*a*e*f^3-31*b*e*f^3+4*c*e*f^3+18*d*e*f^3+26*e^2*f^3+8*a*f^4-44*b*f^4-8*c*f^4-17*d*f^4-30*e*f^4+7*f^5+13*a^4*g-36*a^3*b*g-30*a^2*b^2*g-19*a*b^3*g+27*b^4*g+3*a^3*c*g-4*a^2*b*c*g+9*a*b^2*c*g+15*b^3*c*g+45*a^2*c^2*g+2*a*b*c^2*g+3*b^2*c^2*g-6*a*c^3*g+41*b*c^3*g+11*c^4*g+37*a^3*d*g-3*a^2*b*d*g+50*a*b^2*d*g-44*b^3*d*g+23*a^2*c*d*g+30*a*b*c*d*g+45*b^2*c*d*g+21*a*c^2*d*g-11*b*c^2*d*g+22*c^3*d*g+36*a^2*d^2*g-2*a*b*d^2*g+9*b^2*d^2*g-22*a*c*d^2*g-34*b*c*d^2*g-2*c^2*d^2*g-21*a*d^3*g-28*b*d^3*g+11*c*d^3*g-48*d^4*g-14*a^3*e*g-22*a^2*b*e*g+17*a*b^2*e*g-14*b^3*e*g-21*a^2*c*e*g-26*a*b*c*e*g-21*b^2*c*e*g-19*a*c^2*e*g+26*b*c^2*e*g-c^3*e*g-a^2*d*e*g-28*a*b*d*e*g+6*b^2*d*e*g+8*a*c*d*e*g+40*b*c*d*e*g+22*c^2*d*e*g-36*a*d^2*e*g-32*b*d^2*e*g-4*c*d^2*e*g+41*d^3*e*g+5*a^2*e^2*g-12*a*b*e^2*g-23*b^2*e^2*g-3*a*c*e^2*g+29*b*c*e^2*g+10*c^2*e^2*g-5*a*d*e^2*g-32*b*d*e^2*g-48*c*d*e^2*g+34*d^2*e^2*g+12*a*e^3*g-40*b*e^3*g-15*c*e^3*g-49*d*e^3*g-35*e^4*g+44*a^3*f*g+13*a^2*b*f*g+26*a*b^2*f*g-38*b^3*f*g-49*a^2*c*f*g-6*a*b*c*f*g+4*b^2*c*f*g+25*a*c^2*f*g-45*b*c^2*f*g+13*c^3*f*g-17*a^2*d*f*g-48*a*b*d*f*g-33*b^2*d*f*g+27*a*c*d*f*g-33*b*c*d*f*g+38*c^2*d*f*g-41*a*d^2*f*g+36*b*d^2*f*g-13*c*d^2*f*g-47*d^3*f*g-2*a^2*e*f*g+15*a*b*e*f*g+32*b^2*e*f*g-27*a*c*e*f*g-14*b*c*e*f*g-37*a*d*e*f*g+33*b*d*e*f*g+15*c*d*e*f*g-38*d^2*e*f*g-47*a*e^2*f*g-35*b*e^2*f*g+22*c*e^2*f*g+4*d*e^2*f*g+4*e^3*f*g+39*a^2*f^2*g+26*a*b*f^2*g-36*b^2*f^2*g+22*a*c*f^2*g+8*b*c*f^2*g-3*c^2*f^2*g-30*a*d*f^2*g-33*b*d*f^2*g-14*c*d*f^2*g+38*d^2*f^2*g-11*a*e*f^2*g+36*b*e*f^2*g+31*c*e*f^2*g-7*d*e*f^2*g+e^2*f^2*g-12*a*f^3*g-37*b*f^3*g-c*f^3*g+12*d*f^3*g+20*e*f^3*g-16*f^4*g+46*a^3*g^2+35*a^2*b*g^2-46*a*b^2*g^2-34*b^3*g^2+5*a^2*c*g^2+17*a*b*c*g^2+6*b^2*c*g^2+41*a*c^2*g^2+28*b*c^2*g^2+7*c^3*g^2+36*a^2*d*g^2+33*a*b*d*g^2-32*b^2*d*g^2-3*a*c*d*g^2+19*b*c*d*g^2+11*c^2*d*g^2-28*a*d^2*g^2-21*b*d^2*g^2+26*c*d^2*g^2-41*d^3*g^2+27*a^2*e*g^2-21*a*b*e*g^2-7*b^2*e*g^2-13*a*c*e*g^2+32*b*c*e*g^2-38*c^2*e*g^2-17*a*d*e*g^2-39*b*d*e*g^2-36*c*d*e*g^2+42*d^2*e*g^2+43*a*e^2*g^2+39*b*e^2*g^2-27*c*e^2*g^2+5*d*e^2*g^2+11*e^3*g^2+49*a^2*f*g^2+14*a*b*f*g^2+41*b^2*f*g^2-24*a*c*f*g^2+25*b*c*f*g^2-23*c^2*f*g^2+45*a*d*f*g^2-12*b*d*f*g^2+49*c*d*f*g^2+48*d^2*f*g^2-26*a*e*f*g^2+23*b*e*f*g^2-38*c*e*f*g^2-18*d*e*f*g^2-3*e^2*f*g^2+26*a*f^2*g^2+16*b*f^2*g^2-24*c*f^2*g^2-47*d*f^2*g^2+38*e*f^2*g^2+11*f^3*g^2+6*a^2*g^3-14*a*b*g^3-14*b^2*g^3-18*a*c*g^3+20*b*c*g^3-25*c^2*g^3+11*a*d*g^3-34*b*d*g^3-37*c*d*g^3+41*d^2*g^3-43*a*e*g^3-33*b*e*g^3-30*c*e*g^3-21*d*e*g^3+29*e^2*g^3-40*a*f*g^3-18*b*f*g^3-24*c*f*g^3-50*d*f*g^3-2*e*f*g^3+27*f^2*g^3+a*g^4-23*b*g^4-3*c*g^4-35*d*g^4-13*e*g^4+6*f*g^4-36*g^5,-37*a^6+17*a^5*b+49*a^4*b^2+35*a^3*b^3+34*a^2*b^4+32*a*b^5-21*b^6+2*a^5*c+24*a^4*b*c+27*a^3*b^2*c+45*a^2*b^3*c+16*a*b^4*c+b^5*c+35*a^4*c^2+33*a^3*b*c^2+31*a^2*b^2*c^2-5*a*b^3*c^2+41*b^4*c^2-11*a^3*c^3+36*a^2*b*c^3+10*a*b^2*c^3-49*b^3*c^3-41*a^2*c^4+29*a*b*c^4+32*b^2*c^4+33*a*c^5+17*b*c^5+45*c^6-24*a^5*d-12*a^4*b*d-13*a^3*b^2*d-19*a^2*b^3*d-28*a*b^4*d+23*b^5*d+32*a^4*c*d+22*a^3*b*c*d+20*a^2*b^2*c*d+9*a*b^3*c*d+50*b^4*c*d+39*a^3*c^2*d-46*a^2*b*c^2*d-28*a*b^2*c^2*d+b^3*c^2*d-32*a^2*c^3*d-34*a*b*c^3*d+2*b^2*c^3*d-24*a*c^4*d-49*b*c^4*d+46*c^5*d+33*a^4*d^2+40*a^3*b*d^2+9*a^2*b^2*d^2+22*a*b^3*d^2+12*b^4*d^2-32*a^3*c*d^2-18*a^2*b*c*d^2+26*a*b^2*c*d^2+37*b^3*c*d^2-46*a^2*c^2*d^2-47*a*b*c^2*d^2-28*b^2*c^2*d^2+48*b*c^3*d^2-11*c^4*d^2-8*a^3*d^3+22*a^2*b*d^3-3*a*b^2*d^3+8*b^3*d^3-21*a^2*c*d^3-50*a*b*c*d^3-b^2*c*d^3-48*a*c^2*d^3+23*b*c^2*d^3-c^3*d^3+37*a^2*d^4-49*a*b*d^4-50*b^2*d^4+21*a*c*d^4+18*b*c*d^4+26*c^2*d^4+23*a*d^5-25*b*d^5+14*c*d^5-29*d^6+45*a^5*e-22*a^4*b*e+12*a^3*b^2*e+18*a^2*b^3*e+34*a*b^4*e-35*b^5*e+9*a^4*c*e+18*a^3*b*c*e-15*a^2*b^2*c*e-a*b^3*c*e-17*b^4*c*e+42*a^3*c^2*e+44*a^2*b*c^2*e-25*a*b^2*c^2*e-17*b^3*c^2*e+44*a^2*c^3*e+6*a*b*c^3*e-5*b^2*c^3*e-15*a*c^4*e+37*b*c^4*e-32*c^5*e+46*a^4*d*e-18*a^3*b*d*e+44*a^2*b^2*d*e-45*a*b^3*d*e+8*b^4*d*e-44*a^3*c*d*e-20*a^2*b*c*d*e-17*a*b^2*c*d*e+5*b^3*c*d*e-29*a*b*c^2*d*e+26*b^2*c^2*d*e-34*a*c^3*d*e-9*b*c^3*d*e+21*c^4*d*e+10*a^3*d^2*e-43*a^2*b*d^2*e+36*a*b^2*d^2*e-6*b^3*d^2*e+43*a^2*c*d^2*e-24*a*b*c*d^2*e+10*b^2*c*d^2*e-4*a*c^2*d^2*e-16*b*c^2*d^2*e-37*c^3*d^2*e+46*a^2*d^3*e+37*a*b*d^3*e+27*b^2*d^3*e+16*a*c*d^3*e+31*b*c*d^3*e+26*c^2*d^3*e+27*a*d^4*e-27*b*d^4*e-38*c*d^4*e-24*d^5*e-27*a^4*e^2-27*a^3*b*e^2+9*a^2*b^2*e^2-50*a*b^3*e^2+14*b^4*e^2-31*a^3*c*e^2+36*a^2*b*c*e^2-18*a*b^2*c*e^2+38*b^3*c*e^2+32*a^2*c^2*e^2+34*a*b*c^2*e^2+18*b^2*c^2*e^2-13*a*c^3*e^2+49*b*c^3*e^2+30*c^4*e^2-14*a^3*d*e^2+13*a^2*b*d*e^2+27*a*b^2*d*e^2-7*b^3*d*e^2-21*a^2*c*d*e^2-26*a*b*c*d*e^2+32*b^2*c*d*e^2-27*a*c^2*d*e^2+3*b*c^2*d*e^2-46*c^3*d*e^2-42*a^2*d^2*e^2+2*a*b*d^2*e^2-10*b^2*d^2*e^2+31*a*c*d^2*e^2+9*b*c*d^2*e^2-40*c^2*d^2*e^2+5*a*d^3*e^2+24*b*d^3*e^2-5*c*d^3*e^2+15*d^4*e^2+43*a^3*e^3+6*a^2*b*e^3-22*a*b^2*e^3+49*b^3*e^3-47*a^2*c*e^3-35*a*b*c*e^3+29*b^2*c*e^3+26*a*c^2*e^3+34*b*c^2*e^3+50*c^3*e^3+39*a^2*d*e^3-40*a*b*d*e^3-19*b^2*d*e^3+14*a*c*d*e^3+44*b*c*d*e^3+33*c^2*d*e^3+27*a*d^2*e^3-40*b*d^2*e^3-37*c*d^2*e^3-47*a^2*e^4-44*a*b*e^4-19*b^2*e^4+9*a*c*e^4+7*b*c*e^4+29*c^2*e^4-22*a*d*e^4+44*b*d*e^4-19*c*d*e^4+7*d^2*e^4-15*a*e^5+36*b*e^5-15*c*e^5-24*d*e^5-12*e^6-35*a^5*f+39*a^4*b*f+35*a^3*b^2*f+33*a^2*b^3*f+44*a*b^4*f+17*b^5*f+31*a^4*c*f+41*a^3*b*c*f-48*a^2*b^2*c*f-26*a*b^3*c*f-35*b^4*c*f+47*a^3*c^2*f-36*a^2*b*c^2*f+37*a*b^2*c^2*f-16*b^3*c^2*f-14*a^2*c^3*f+43*a*b*c^3*f-4*b^2*c^3*f-23*a*c^4*f+8*b*c^4*f+18*c^5*f+8*a^4*d*f+21*a^3*b*d*f-2*a^2*b^2*d*f-43*a*b^3*d*f-18*b^4*d*f+21*a^3*c*d*f+31*a^2*b*c*d*f-31*a*b^2*c*d*f-5*b^3*c*d*f+8*a^2*c^2*d*f-37*a*b*c^2*d*f+3*b^2*c^2*d*f+7*a*c^3*d*f-22*b*c^3*d*f+34*c^4*d*f+17*a^3*d^2*f+19*a^2*b*d^2*f+35*a*b^2*d^2*f-6*b^3*d^2*f-31*a^2*c*d^2*f+6*a*b*c*d^2*f+35*b^2*c*d^2*f+28*a*c^2*d^2*f-10*b*c^2*d^2*f+2*c^3*d^2*f+49*a^2*d^3*f+16*a*b*d^3*f+b^2*d^3*f-15*a*c*d^3*f-27*b*c*d^3*f+7*c^2*d^3*f-11*a*d^4*f-50*b*d^4*f-8*c*d^4*f+32*d^5*f+22*a^4*e*f+29*a^3*b*e*f+46*a^2*b^2*e*f-49*a*b^3*e*f-45*b^4*e*f+32*a^3*c*e*f-32*a^2*b*c*e*f-35*a*b^2*c*e*f-23*b^3*c*e*f-17*a^2*c^2*e*f+7*a*b*c^2*e*f+16*b^2*c^2*e*f-49*a*c^3*e*f-15*b*c^3*e*f-48*c^4*e*f-33*a^3*d*e*f+23*a^2*b*d*e*f-2*a*b^2*d*e*f+46*b^3*d*e*f-33*a^2*c*d*e*f+48*a*b*c*d*e*f-49*b^2*c*d*e*f+7*a*c^2*d*e*f-25*b*c^2*d*e*f+26*c^3*d*e*f+19*a^2*d^2*e*f+16*a*b*d^2*e*f-28*b^2*d^2*e*f-15*a*c*d^2*e*f-47*b*c*d^2*e*f-32*c^2*d^2*e*f-39*a*d^3*e*f-41*b*d^3*e*f-20*c*d^3*e*f+50*d^4*e*f+10*a^3*e^2*f-17*a^2*b*e^2*f-5*a*b^2*e^2*f+48*b^3*e^2*f-16*a^2*c*e^2*f+20*a*b*c*e^2*f-3*b^2*c*e^2*f+13*a*c^2*e^2*f+43*b*c^2*e^2*f+3*c^3*e^2*f-16*a^2*d*e^2*f-18*a*b*d*e^2*f-42*b^2*d*e^2*f+11*b*c*d*e^2*f+41*c^2*d*e^2*f+3*a*d^2*e^2*f+47*b*d^2*e^2*f-44*c*d^2*e^2*f-11*d^3*e^2*f-46*a^2*e^3*f-24*a*b*e^3*f-39*b^2*e^3*f+22*a*c*e^3*f-30*b*c*e^3*f+45*c^2*e^3*f-9*a*d*e^3*f-27*b*d*e^3*f+17*c*d*e^3*f-9*d^2*e^3*f+49*a*e^4*f-31*b*e^4*f-20*c*e^4*f-20*d*e^4*f+17*e^5*f+10*a^4*f^2-15*a^3*b*f^2-16*a^2*b^2*f^2+31*a*b^3*f^2+35*b^4*f^2-28*a^3*c*f^2+19*a^2*b*c*f^2+11*a*b^2*c*f^2+39*b^3*c*f^2+16*a^2*c^2*f^2-22*a*b*c^2*f^2-30*b^2*c^2*f^2+44*a*c^3*f^2+18*b*c^3*f^2+14*c^4*f^2-49*a^3*d*f^2+23*a^2*b*d*f^2-39*a*b^2*d*f^2-21*b^3*d*f^2-41*a^2*c*d*f^2-32*a*b*c*d*f^2+24*b^2*c*d*f^2-37*a*c^2*d*f^2+24*b*c^2*d*f^2+47*c^3*d*f^2-12*a^2*d^2*f^2+2*a*b*d^2*f^2-10*b^2*d^2*f^2-42*a*c*d^2*f^2-2*b*c*d^2*f^2-8*c^2*d^2*f^2+48*a*d^3*f^2-21*b*d^3*f^2-40*c*d^3*f^2-33*d^4*f^2+47*a^3*e*f^2-31*a^2*b*e*f^2+9*a*b^2*e*f^2+7*b^3*e*f^2+24*a^2*c*e*f^2-14*a*b*c*e*f^2-15*b^2*c*e*f^2+48*a*c^2*e*f^2-2*b*c^2*e*f^2-43*c^3*e*f^2+13*a^2*d*e*f^2+25*a*b*d*e*f^2+26*b^2*d*e*f^2+7*a*c*d*e*f^2+6*b*c*d*e*f^2+16*c^2*d*e*f^2+4*a*d^2*e*f^2+39*b*d^2*e*f^2+2*c*d^2*e*f^2-32*d^3*e*f^2+33*a^2*e^2*f^2+21*a*b*e^2*f^2+40*b^2*e^2*f^2+a*c*e^2*f^2-36*b*c*e^2*f^2+11*c^2*e^2*f^2-45*a*d*e^2*f^2-32*b*d*e^2*f^2-6*c*d*e^2*f^2+27*d^2*e^2*f^2-6*a*e^3*f^2+3*b*e^3*f^2-35*c*e^3*f^2+45*d*e^3*f^2-21*e^4*f^2-34*a^3*f^3-18*a^2*b*f^3+30*a*b^2*f^3+26*b^3*f^3+6*a^2*c*f^3-38*a*b*c*f^3+8*b^2*c*f^3+40*a*c^2*f^3+41*b*c^2*f^3+30*c^3*f^3-31*a^2*d*f^3+40*a*b*d*f^3+18*b^2*d*f^3-11*a*c*d*f^3+28*b*c*d*f^3+13*c^2*d*f^3+34*a*d^2*f^3+9*b*d^2*f^3-10*c*d^2*f^3-18*d^3*f^3+15*a^2*e*f^3-26*a*b*e*f^3-23*b^2*e*f^3-15*a*c*e*f^3+11*b*c*e*f^3-6*c^2*e*f^3+8*a*d*e*f^3+b*d*e*f^3-45*c*d*e*f^3-50*d^2*e*f^3+21*a*e^2*f^3-22*b*e^2*f^3+34*c*e^2*f^3+46*d*e^2*f^3+34*e^3*f^3+27*a^2*f^4+42*a*b*f^4-10*b^2*f^4-22*a*c*f^4-30*b*c*f^4-50*c^2*f^4-21*a*d*f^4-22*b*d*f^4+24*c*d*f^4+13*d^2*f^4-26*a*e*f^4+28*b*e*f^4+33*c*e*f^4-4*d*e*f^4-11*e^2*f^4+7*a*f^5-19*b*f^5+17*c*f^5-12*d*f^5-e*f^5+35*f^6-31*a^5*g+28*a^4*b*g+25*a^3*b^2*g+17*a^2*b^3*g-35*a*b^4*g-18*b^5*g+30*a^4*c*g+3*a^3*b*c*g+4*a^2*b^2*c*g-22*a*b^3*c*g-4*b^4*c*g+18*a^3*c^2*g+25*a^2*b*c^2*g+7*a*b^2*c^2*g-10*b^3*c^2*g+15*a^2*c^3*g+50*a*b*c^3*g+8*b^2*c^3*g+21*a*c^4*g+10*b*c^4*g-9*c^5*g+18*a^4*d*g-10*a^3*b*d*g-32*a^2*b^2*d*g-4*a*b^3*d*g+43*b^4*d*g-4*a^3*c*d*g+32*a^2*b*c*d*g+35*a*b^2*c*d*g+35*b^3*c*d*g-48*a^2*c^2*d*g+40*a*b*c^2*d*g-28*b^2*c^2*d*g-16*a*c^3*d*g-11*b*c^3*d*g-22*c^4*d*g-12*a^3*d^2*g-16*a^2*b*d^2*g+21*a*b^2*d^2*g-19*b^3*d^2*g-32*a^2*c*d^2*g+25*a*b*c*d^2*g+28*b^2*c*d^2*g+25*a*c^2*d^2*g+21*b*c^2*d^2*g-42*c^3*d^2*g-27*a^2*d^3*g-47*a*b*d^3*g-13*b^2*d^3*g+18*a*c*d^3*g+34*b*c*d^3*g-20*c^2*d^3*g+34*a*d^4*g+4*b*d^4*g+47*c*d^4*g-25*d^5*g+14*a^4*e*g+31*a^3*b*e*g-42*a^2*b^2*e*g+12*a*b^3*e*g+46*b^4*e*g-19*a^3*c*e*g+34*a^2*b*c*e*g-25*a*b^2*c*e*g-39*b^3*c*e*g+46*a^2*c^2*e*g-27*a*b*c^2*e*g-45*b^2*c^2*e*g-44*a*c^3*e*g-11*b*c^3*e*g+22*c^4*e*g+45*a^3*d*e*g+39*a^2*b*d*e*g-44*a*b^2*d*e*g-16*b^3*d*e*g+36*a^2*c*d*e*g+36*a*b*c*d*e*g-10*b^2*c*d*e*g+39*a*c^2*d*e*g-49*b*c^2*d*e*g-7*c^3*d*e*g+5*a^2*d^2*e*g+9*a*b*d^2*e*g+2*b^2*d^2*e*g+25*a*c*d^2*e*g+15*b*c*d^2*e*g-14*c^2*d^2*e*g+18*a*d^3*e*g+5*b*d^3*e*g+36*c*d^3*e*g-13*d^4*e*g-36*a^3*e^2*g-43*a^2*b*e^2*g-19*a*b^2*e^2*g+25*b^3*e^2*g-36*a^2*c*e^2*g+29*a*b*c*e^2*g-5*b^2*c*e^2*g-2*a*c^2*e^2*g-22*b*c^2*e^2*g-17*c^3*e^2*g-28*a^2*d*e^2*g-11*a*b*d*e^2*g+15*b^2*d*e^2*g+22*a*c*d*e^2*g-50*b*c*d*e^2*g-40*c^2*d*e^2*g-11*a*d^2*e^2*g+36*b*d^2*e^2*g-19*c*d^2*e^2*g-30*d^3*e^2*g+26*a^2*e^3*g-47*a*b*e^3*g-50*b^2*e^3*g+22*a*c*e^3*g-18*b*c*e^3*g+11*c^2*e^3*g-37*a*d*e^3*g+42*b*d*e^3*g-41*c*d*e^3*g-12*d^2*e^3*g+23*a*e^4*g+47*b*e^4*g+c*e^4*g-19*d*e^4*g+4*e^5*g+32*a^4*f*g+45*a^3*b*f*g-30*a^2*b^2*f*g-48*a*b^3*f*g-28*b^4*f*g-a^3*c*f*g-34*a^2*b*c*f*g+8*a*b^2*c*f*g+22*b^3*c*f*g+5*a^2*c^2*f*g-7*a*b*c^2*f*g+16*b^2*c^2*f*g+26*a*c^3*f*g-37*b*c^3*f*g+7*c^4*f*g-18*a^3*d*f*g-30*a^2*b*d*f*g-a*b^2*d*f*g+5*b^3*d*f*g-43*a^2*c*d*f*g+18*a*b*c*d*f*g-24*b^2*c*d*f*g-23*a*c^2*d*f*g-10*b*c^2*d*f*g-33*c^3*d*f*g-13*a^2*d^2*f*g-50*a*b*d^2*f*g+11*b^2*d^2*f*g-2*a*c*d^2*f*g-48*b*c*d^2*f*g+33*c^2*d^2*f*g-23*a*d^3*f*g+37*b*d^3*f*g-29*c*d^3*f*g+37*d^4*f*g+50*a^3*e*f*g-21*a^2*b*e*f*g+25*a*b^2*e*f*g+36*b^3*e*f*g+45*a^2*c*e*f*g-8*a*b*c*e*f*g+27*b^2*c*e*f*g-a*c^2*e*f*g-25*b*c^2*e*f*g-21*c^3*e*f*g+12*a^2*d*e*f*g-49*a*b*d*e*f*g-36*b^2*d*e*f*g+45*a*c*d*e*f*g-42*b*c*d*e*f*g-17*a*d^2*e*f*g+36*b*d^2*e*f*g-7*c*d^2*e*f*g+37*d^3*e*f*g-13*a^2*e^2*f*g-21*a*b*e^2*f*g-26*b^2*e^2*f*g+44*a*c*e^2*f*g-7*b*c*e^2*f*g+32*c^2*e^2*f*g-20*a*d*e^2*f*g+19*b*d*e^2*f*g+49*c*d*e^2*f*g+41*d^2*e^2*f*g-27*a*e^3*f*g+26*b*e^3*f*g+16*c*e^3*f*g-26*d*e^3*f*g+30*e^4*f*g-26*a^3*f^2*g-38*a^2*b*f^2*g+46*a*b^2*f^2*g-14*b^3*f^2*g-36*a^2*c*f^2*g+20*a*b*c*f^2*g+39*b^2*c*f^2*g+33*a*c^2*f^2*g+45*b*c^2*f^2*g-38*c^3*f^2*g+42*a^2*d*f^2*g+25*a*b*d*f^2*g+6*b^2*d*f^2*g+35*a*c*d*f^2*g-32*b*c*d*f^2*g+23*c^2*d*f^2*g+29*a*d^2*f^2*g-22*b*d^2*f^2*g+15*c*d^2*f^2*g+10*d^3*f^2*g+29*a^2*e*f^2*g+20*a*b*e*f^2*g-17*b^2*e*f^2*g+35*a*c*e*f^2*g-8*b*c*e*f^2*g+26*c^2*e*f^2*g+49*a*d*e*f^2*g+49*b*d*e*f^2*g+2*c*d*e*f^2*g-45*d^2*e*f^2*g-22*a*e^2*f^2*g-27*b*e^2*f^2*g-4*c*e^2*f^2*g-8*d*e^2*f^2*g+45*e^3*f^2*g+7*a^2*f^3*g+30*a*b*f^3*g-35*b^2*f^3*g-45*a*c*f^3*g+5*b*c*f^3*g-18*c^2*f^3*g-46*a*d*f^3*g-28*b*d*f^3*g+45*c*d*f^3*g-3*d^2*f^3*g-48*a*e*f^3*g-37*b*e*f^3*g-24*c*e*f^3*g-37*d*e*f^3*g-13*e^2*f^3*g-21*a*f^4*g+22*b*f^4*g-41*c*f^4*g+11*d*f^4*g+8*e*f^4*g+29*f^5*g-35*a^4*g^2-44*a^3*b*g^2-a^2*b^2*g^2+47*a*b^3*g^2-11*b^4*g^2+18*a^3*c*g^2-18*a^2*b*c*g^2-2*a*b^2*c*g^2-6*b^3*c*g^2+34*a^2*c^2*g^2-20*a*b*c^2*g^2+50*b^2*c^2*g^2+9*a*c^3*g^2-20*b*c^3*g^2+27*c^4*g^2-8*a^3*d*g^2+36*a^2*b*d*g^2+10*a*b^2*d*g^2+19*b^3*d*g^2+24*a^2*c*d*g^2+3*a*b*c*d*g^2+4*b^2*c*d*g^2+11*c^3*d*g^2-40*a^2*d^2*g^2+34*a*b*d^2*g^2+32*b^2*d^2*g^2+37*a*c*d^2*g^2-5*b*c*d^2*g^2+32*c^2*d^2*g^2+19*a*d^3*g^2-9*b*d^3*g^2-11*d^4*g^2-45*a^3*e*g^2+47*a^2*b*e*g^2-22*a*b^2*e*g^2-2*b^3*e*g^2+8*a^2*c*e*g^2-32*a*b*c*e*g^2+32*b^2*c*e*g^2-36*a*c^2*e*g^2+21*b*c^2*e*g^2-15*c^3*e*g^2+50*a^2*d*e*g^2-35*a*b*d*e*g^2-12*b^2*d*e*g^2-8*a*c*d*e*g^2+50*b*c*d*e*g^2+11*c^2*d*e*g^2+9*a*d^2*e*g^2-42*b*d^2*e*g^2-22*c*d^2*e*g^2-50*d^3*e*g^2-19*a^2*e^2*g^2-42*a*b*e^2*g^2-47*b^2*e^2*g^2+29*a*c*e^2*g^2+27*b*c*e^2*g^2+26*c^2*e^2*g^2-23*a*d*e^2*g^2+5*b*d*e^2*g^2-9*c*d*e^2*g^2+39*d^2*e^2*g^2+42*a*e^3*g^2+6*b*e^3*g^2-9*c*e^3*g^2-26*d*e^3*g^2-34*e^4*g^2+10*a^3*f*g^2+33*a^2*b*f*g^2+23*a*b^2*f*g^2-24*b^3*f*g^2+a^2*c*f*g^2-16*a*b*c*f*g^2-14*b^2*c*f*g^2+13*a*c^2*f*g^2+30*b*c^2*f*g^2+2*c^3*f*g^2-8*a^2*d*f*g^2-29*a*b*d*f*g^2-10*b^2*d*f*g^2-16*a*c*d*f*g^2+49*b*c*d*f*g^2+48*c^2*d*f*g^2+48*a*d^2*f*g^2+14*b*d^2*f*g^2+5*c*d^2*f*g^2+5*d^3*f*g^2-3*a^2*e*f*g^2-a*b*e*f*g^2-25*b^2*e*f*g^2-25*a*c*e*f*g^2+42*b*c*e*f*g^2+49*c^2*e*f*g^2+3*a*d*e*f*g^2-26*b*d*e*f*g^2-29*c*d*e*f*g^2-28*d^2*e*f*g^2+43*a*e^2*f*g^2+29*b*e^2*f*g^2+19*c*e^2*f*g^2-35*d*e^2*f*g^2+17*e^3*f*g^2+49*a^2*f^2*g^2-22*a*b*f^2*g^2+46*b^2*f^2*g^2+4*a*c*f^2*g^2+28*b*c*f^2*g^2-16*c^2*f^2*g^2+39*a*d*f^2*g^2+10*b*d*f^2*g^2+6*c*d*f^2*g^2-14*d^2*f^2*g^2+17*a*e*f^2*g^2-49*b*e*f^2*g^2+31*c*e*f^2*g^2+20*d*e*f^2*g^2+7*e^2*f^2*g^2+15*a*f^3*g^2+45*b*f^3*g^2-25*c*f^3*g^2-25*d*f^3*g^2+45*e*f^3*g^2+17*f^4*g^2-34*a^3*g^3+35*a^2*b*g^3-32*a*b^2*g^3+24*b^3*g^3-4*a^2*c*g^3-50*a*b*c*g^3-47*b^2*c*g^3+11*a*c^2*g^3-24*b*c^2*g^3+40*c^3*g^3+24*a^2*d*g^3+40*a*b*d*g^3+7*b^2*d*g^3-23*a*c*d*g^3+48*b*c*d*g^3-27*c^2*d*g^3-47*a*d^2*g^3+5*b*d^2*g^3+49*c*d^2*g^3-23*d^3*g^3-42*a^2*e*g^3-19*a*b*e*g^3+25*b^2*e*g^3+26*a*c*e*g^3+34*b*c*e*g^3-22*c^2*e*g^3+37*a*d*e*g^3-33*b*d*e*g^3-15*c*d*e*g^3+29*d^2*e*g^3-39*a*e^2*g^3+21*b*e^2*g^3-5*c*e^2*g^3-44*d*e^2*g^3+12*e^3*g^3-48*a^2*f*g^3+24*a*b*f*g^3-45*b^2*f*g^3-25*a*c*f*g^3+49*b*c*f*g^3+18*c^2*f*g^3-2*a*d*f*g^3+10*b*d*f*g^3-19*c*d*f*g^3+47*d^2*f*g^3+15*a*e*f*g^3+13*b*e*f*g^3-25*c*e*f*g^3+33*d*e*f*g^3+47*e^2*f*g^3-47*a*f^2*g^3-34*b*f^2*g^3+47*c*f^2*g^3-10*d*f^2*g^3-36*e*f^2*g^3-32*f^3*g^3-3*a^2*g^4+34*a*b*g^4-16*b^2*g^4+32*a*c*g^4+47*b*c*g^4-31*c^2*g^4+22*a*d*g^4-32*b*d*g^4-21*c*d*g^4+42*d^2*g^4+a*e*g^4-40*b*e*g^4+19*c*e*g^4-25*d*e*g^4+40*e^2*g^4+43*a*f*g^4-30*b*f*g^4-14*c*f*g^4-7*d*f*g^4-e*f*g^4+7*f^2*g^4+10*a*g^5+14*b*g^5+36*c*g^5-27*d*g^5+47*e*g^5-47*f*g^5-14*g^6);
     J1
     )

fortyVars = () -> (
     kk = ZZ/32003;
     R1 = kk[vars(0..39), MonomialSize=>8];
     ideal"b2de+bd2f+a2dg+abdh+ad2i+b2cj+bc2k+bcdl+a2cm+abcn+ac2o+acdp,
     b2dq+bd2r+a2ds+abdt+ad2u+b2cv+bc2w+bcdx+a2cy+abcz+ac2A+acdB,
     b2dC+bd2D+a2dE+abdF+ad2G+b2cH+bc2I+bcdJ+a2cK+abcL+ac2M+acdN"
     )

tests = {
--  "bugStas",
  "weispfenning94",
  "liu",
  "buchberger87",
  "gerdt93",
--  "lichtblau",
  "trinks",
  "eco6",
  "sym33",
  "hairer1",
  "f633",
  "katsura5",
  "katsura6",
  "katsura7",
  "katsura8",
  "cyclicn 5",
  "cyclicn 6",
  "cyclicn 7",
  -- "cyclicn 8",
  "fmtm",
  "uteshevBikker",
  "hfeSegers",
  "gonnet83",
  "f744",
  "schransTroost"
  }


end


restart
load "f5ex.m2"
doTests()
doTests1()
doTest3()
netList oo


restart
load "f5ex.m2"

makeIdeals("test-code/examples/", {"eco6"})
makeIdeals("test-code/examples/", tests)
I = liu()
J = toABC I
options GVW
GVW(J, Order => "g1")
GVW(J, Order => "Sup")
netList{statsHeader(), displayStats()}
displayit J

I = toABC trinks()
print displayit I
"test-code/examples/weispfenning94.ideal" << displayit toABC weispfenning94() << close
"test-code/examples/weispfenning94.ideal" << displayit toABC weispfenning94() << close

R = (coefficientRing ring J)[gens ring J, MonomialOrder=>{GRevLex=>numgens ring J,Position=>Down}]
J = sub(J, R)
leadTerm gb syz gens J

getInitialSyz toABC trinks()
getInitialSyz toABC buchberger87()
getInitialSyz toABC gerdt93()
getInitialSyz toABC weispfenning94()
getInitialSyz toABC eco6()


getInitialSyz toABC lichtblau()

-- read gb file:
L = lines get "test-code/src/cyclic5.gb"
posL = positions(L, s -> match("^--", s))
netList L_{posL#0+1..posL#1-1}
syzL = L_{posL#1+1..posL#2-1}
netList syzL
apply(syzL, a -> separate(":", a))
replace("^[[:space:]]+", "", o31)
replace("[[:space:]]+$", "", oo)
replace("[[:space:]]+", ",", oo)
value ("ideal\""|oo|"\"")

J = toABC ((cyclicn 5)())
R = ring J
H = readGBFile "test-code/src/cyclic5.gb"
H#"inSyz"

-----------------------------------
-- Tests for correctness ----------
-----------------------------------
restart
load "f5ex.m2"


J = toABC liu()
H = gbExample(J, StatsOnly=>true)
H = gbExample(J, Order=>0)
H = gbExample(J, Order=>0, FullReduction=>false, Verbose=>4)
H = gbExample(J, Order=>0, FullReduction=>true, Verbose=>4)
H = gbExample(J, Order=>0, Reducer => 1)
H = gbExample(J, Order=>0, Reducer => 2)
H = gbExample(J, Order=>1)
H = gbExample(J, Order=>2)
H = gbExample(J, FullReduction=>false)
H = gbExample(J, FullReduction=>true)
H = gbExample(J, FullReduction=>false, Verbose=>4)
H = gbExample(J, FullReduction=>true, Verbose=>4)

gbStats(4, J)
gbStats(3, J)
gbStats(2, J)
gbStats(1, J)
gbStats(0, J)
checkGB(J,H)
checkSyz(J,H)

J = toABC gerdt93()
H = gbExample J
checkGB(J,H)
checkSyz(J,H)

J = toABC ((cyclicn 5)())
H = gbExample J;
checkGB(J,H)
checkSyz(J,H)

gbStats(4, J)
gbStats(3, J)
gbStats(2, J)
gbStats(1, J)
gbStats(0, J)

J = toABC buchberger87()
H = gbExample J;
checkGB(J,H)
checkSyz(J,H)

J = toABC trinks()
H = gbExample J
H = gbExample(J, Reducer=>1)
checkGB(J,H)
checkSyz(J,H)

J = toABC sym33()
H = gbExample J
checkGB(J,H)
checkSyz(J,H)

J = toABC eco6()
H = gbExample J
H = gbExample (J, Reducer=>1)
H = gbExample(J, Reducer => 2)

checkGB(J,H)
checkSyz(J,H)

J = toABC hairer1()
H = gbExample J
checkGB(J,H)
checkSyz(J,H)


J = toABC katsura5()
H = gbExample J;
time H = gbExample(J, Reducer=>0)
time H = gbExample (J, Reducer=>1)
time H = gbExample(J, Reducer=>2)

checkGB(J,H)
checkSyz(J,H)

J = toABC katsura6()
H = gbExample J;
checkGB(J,H)
checkSyz(J,H)

J = toABC uteshevBikker()
H = gbExample J
checkGB(J,H)
checkSyz(J,H)

J = toABC hfeSegers()
H = gbExample J
checkGB(J,H)
checkSyz(J,H)

J = toABC weispfenning94()
H = gbExample J
checkGB(J,H)
checkSyz(J,H)

-- missing: bugStas, lichtblau, fmtm
-- katsura7, katsura8, cyclic6, cyclic7, cyclic8

-- The following take too long with all the debugging display in.
J = toABC f633()
H = gbExample J;
checkGB(J,H)
checkSyz(J,H)  -- computing syz in M2 takes too long

J = toABC f744()
H = gbExample J;
checkGB(J,H)
checkSyz(J,H)

J = toABC gonnet83()
H = gbExample J; -- takes a long time with all the debugging and stats statements!
checkGB(J,H)
checkSyz(J,H)

J = toABC schransTroost()
H = gbExample J;
time H = gbExample(J, Reducer=>0);
time H = gbExample (J, Reducer=>1);
time H = gbExample(J, Reducer=>2);
checkGB(J,H)
checkSyz(J,H)