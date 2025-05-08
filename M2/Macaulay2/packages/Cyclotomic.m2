-- -*- coding: utf-8 -*-
--  cyclotomic.m2
--
--  Copyright (C) 2009-12 Thomas Kahle <thomas-kahle@gmx.de>
--
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or (at
--  your option) any later version.
--
--  This program is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with this program; if not, write to the Free Software Foundation, Inc.,
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
--
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

newPackage(
	"Cyclotomic",
	Version => "1.0",
	Date => "February 2012",
    	Authors => {{Name => "Thomas Kahle", 
		  Email => "thomas.kahle@jpberlin.de",
		  HomePage => "http://thomas-kahle.de"}},
	Keywords => {"Algebraic Number Theory"},
	Headline => "cyclotomic fields"
    	)

export {"cyclotomicField",
        "cyclotomicPoly",
	"findRootPower",
	"joinCyclotomic"
       }

ww := getSymbol "ww"
  
-- We use memoize to get physically the same cyclotomic field again and again!
cf = (i) -> (
     Q := QQ(monoid [ww_i]);
     toField ((Q) / cyclotomicPoly (i, Q_0)))

cyclotomicField = memoize cf

--- Here is an example of how to map coefficient rings to not forget it 
-- restart
-- F = frac (QQ[a,b]/ideal(a^2-b))
-- G = frac (QQ[x,y]/ideal(x^2-y))
-- 
-- R = F[t]
-- S = G[t]
-- 
-- use R
-- I = ideal (t-a-b)
-- use S
-- J = ideal (t+x)
-- 
-- use S
-- f = map (S,R,{t,x,y^2})
-- f(I)

-- We apply this to join cyclotomic fields

joinCyclotomic = li -> (
     -- This function joins a list of ideals in a polynomial ring over a smallest common cf.  Input should consist of
     -- ideals in polynomial rings over the same variables, but with possible different cyclotomic coefficient field

     -- Find root powers:
     lc := for l in li list findRootPower ring l;
     leastcm := lcm for l in lc list if l>2 then l else 1;
     
     --Check for QQ
     if leastcm < 3 then return li;
     
     F := cyclotomicField leastcm;
     -- Here we use the assumptions that all rings have the same generators
     -- This should contain the variables of polynomials
     ge := gens ring li#0;

     S := F(monoid [ge]);
     li2 := {}; ww:=F_0; local f;
     for i in 0..#li-1 do (
	  if lc#i == 2 then (
	       -- rational coefficients: just map
	       li2 = li2 | {sub (li#i,S)};
	       )
	  else (
	       -- Was cyclotomic: need to find image of ww in new ring!
	       f = map (S, ring li#i , (gens S) |{ww^(leastcm//lc#i)});
	       li2 = li2 | { f li#i };
	       );
	  );
     li2)

cyclotomicPoly = (i,v) -> (
     -- returns the i-th cyclotomic polynomial in variable v.
     -- v must be a variable a priori
     v = value v;
     if i <= 0 then error "the input should be > 0.";
     if i==1 then return v-1 ;
     mini := v^i -1;
     -- dividing out the first cyclotomic polynomial
     -- (with result a polynomial)
     mini = (flatten entries syz matrix {{mini ,(v-1)}})#1;
     -- i is prime:
     if isPrime i then return mini / (leadCoefficient mini);
     
     -- i is not prime:
     -- find the divisors:
     for f in toList (2..i//2) do (
	  -- check for factor
	  if i%f == 0 then (
	       fac := cyclotomicPoly (f,v);
	       -- division with result in polynomial ring:
	       mini = (flatten entries syz matrix {{mini,fac}})#1;
	       )
	  );
     --make sure the leading coefficient is one.
     mini / leadCoefficient(mini))

findRootPower = R -> (
     -- Finds the power of the adjoined root of unity in the
     -- coefficient ring of R by just exponentiating.
     -- Returns '2' if the input was a polynomial ring over QQ
     r := 0;
     F := coefficientRing R;
     fieldgens := (K,F) -> if K === F then {} else for x in gens last F.baseRings list promote(x,F);
     g := fieldgens (QQ,F);
     if #g == 0 then return 2;
     if #g > 1 then error "The coefficient field has more than one generator";
     g = value (g#0);
     gg := g; -- the generator
     while not 1_F == gg do (
	  r = r+1;
	  gg = gg *g;
	  );
     if r<2 then return 2
     else r+1)

-- End of source code ---

beginDocumentation()

document { 
        Key => Cyclotomic,
        Headline => "cyclotomic fields",
        EM "Cyclotomic", " is a package for cyclotomic fields.  It is used in
        to construct extensions of the coefficient field during binomial
        primary decomposition using the package ", TO "Binomials::Binomials", "."
        }

document {
     Key => {cyclotomicField},
     Headline => "cyclotomic field construction",
     Usage => "cyclotomicField (i)",
     Inputs => {
          "i" => { "an integer, the power of the root to be adjoined."}},
     Outputs => {
          "S" => {"A cyclotomic field with $1^(1/i)$ adjoined"} },
     EXAMPLE {
          "S = cyclotomicField (5)",
	  "isField S",
	  "r = S_0",
	  "(r^9, r^10, r^11)",
          "T = S[x,y]",
     	  "I = ideal (x-r)",
	  "dim I"
          },
     Caveat => {"Strange things can happen with the reduction of the coefficients.", " In M2 v <= 1.2 dimension is off by one."},
     SeeAlso => cyclotomicPoly
     }

document {
     Key => {cyclotomicPoly},
     Headline => "Cyclotomic Polynomial",
     Usage => "cyclotomicPoly (i,v)",
     Inputs => {
          "i" => { "an integer, the power of a root of unity."},
	  "v" => { "a variable name in which the polynomial is returned."} },
     Outputs => {
          "f" => {"The minimal polynomial of the i-th root of unity."} },
     EXAMPLE {
          "R = QQ[ww]",
          "f = cyclotomicPoly (6,ww)",
          },
     SeeAlso => cyclotomicField
     }

document {
     Key => {findRootPower},
     Headline => "Find the order of the root in the coefficients of polynomial ring over a cyclotomic field",
     Usage => "findRootPower R",
     Inputs => {
          "R" => { "a polynomial ring over a cyclotomic field"}},
     Outputs => {
          "i" => {"The order of the adjoined root of unity, or 2 if the coefficient field is QQ"} },
     EXAMPLE {
          "S = cyclotomicField 5",
          "T = S[x,y]",
     	  "findRootPower T"
          },
     SeeAlso => cyclotomicField
     }

document {
     Key => {joinCyclotomic},
     Headline => "Join ideals in polynomial rings over different cyclotomic fields",
     Usage => "joinCyclotomic l",
     Inputs => {
          "li" => { "a list of ideals in polynomial rings over cyclotomic fields"}},
     Outputs => {
          "li2" => {"The list of ideals in a common ring."} },
     EXAMPLE {
	  "F = cyclotomicField 3; G = cyclotomicField 4;",
          "R = F[t]; I = ideal (t-F_0^2)",
          "S = G[t]; J = ideal (t^2-G_0)",
     	  "joinCyclotomic {I,J}"
          },
     SeeAlso => cyclotomicField
     }

TEST ///
F = cyclotomicField 3
G = cyclotomicField 4
R = F[t]
I = ideal (t-F_0^2)
S = G[t]
J = ideal (t^2-G_0)
assert (findRootPower coefficientRing ring (joinCyclotomic {I,J})#0 == 12)
///

TEST ///
-- https://github.com/Macaulay2/M2/issues/487
KK = QQ[r]/cyclotomicPoly(3,r);
S = KK[x_0..x_11];
A = matrix{
    {x_0 + r*x_1+r^2*x_2,0,0},
    {0,r*(x_0 + r*x_1+r^2*x_2),0},
    {0,0,r^2*(x_0 + r*x_1+r^2*x_2)}}
B = genericMatrix(S,x_3,3,3)
M = matrix{{A,B,0*id_(S^3)},{0*id_(S^3),r*A,r*B},{r^2*B,0*id_(S^3),r^2*A}}
det M -- used to segfault
///
