-- -*- coding: utf-8 -*-
newPackage(
  "NumericalHilbert",
  Version => "0.0", 
  Date => "July 25, 2010",
  Authors => {{Name => "Robert Krone", 
    Email => "krone@math.gatech.edu"}},
  Headline => "some local Hilbert series functions",
  DebuggingMode => true
)

export {dualBasis, dualHilbert, hilbertB, hilbertC, Point, UseDZ, STmatrix, DZmatrix}

--Generators of the dual space with all terms of degree d or less.
--Uses ST algorithm by default, but can use DZ instead if specified.
dualBasis = method(TypicalValue => Matrix, Options => {Point => {}, UseDZ => false})
dualBasis (Matrix, ZZ) := o -> (igens, d) -> (
  R := ring igens;
  epsilon := .001; --error tolerance for kernel
  M := new Matrix;
  if o.UseDZ then M = last DZmatrix(igens, d, Point => o.Point)
             else M = last STmatrix(igens, d, Point => o.Point);
  dmons := apply(1..d, i->dMonomials(R,i)); --nested list of monomials up to order d
  if M != 0 then (
    (svs, U, Vt) := SVD transpose sub(M,coefficientRing R);
    Vt = entries Vt;
    dualGens = new MutableList;
    for i from 0 to #svs-1 do
      if abs svs#i <= epsilon then dualGens#(#dualGens) = apply(Vt#i, conjugate);
    for i from #svs to (numgens target M)-1 do
      dualGens#(#dualGens) = Vt#i;
    dualGens = transpose rowReduce(matrix new List from dualGens, epsilon);
    --print (dualGens, matrix {new List from flatten dmons});
    (matrix {{1_R}}) | ((matrix {new List from flatten dmons})*sub(dualGens,R))
  )
  else (matrix {{1_R}}) | (matrix {new List from flatten dmons})
);

--List of number of generators of the dual space with lead term of degree k for k from 0 to d.
--Uses ST algorithm by default, but can use DZ instead if specified.
dualHilbert = method(TypicalValue => List, Options => {Point => {}, UseDZ => false})
dualHilbert (Matrix, ZZ) := o -> (igens, d) -> (
  epsilon := .001; --error tolerance for kernel
  mList := new Matrix;
  if o.UseDZ then mList = DZmatrix(igens, d, Point => o.Point)
             else mList = STmatrix(igens, d, Point => o.Point);
  fs := apply(mList, M->(
    svs := {};
    if M != 0 then svs = (SVD sub(M,coefficientRing R))#0;
    --print(M, svs, numgens target M, #select(svs, v->(abs v > epsilon)));
    (numgens target M) - #select(svs, v->(abs v > epsilon)) + 1
  ));
  fs = {0,1}|fs;
  apply(d+1, i->(fs#(i+1) - fs#i))
);

--Dayton-Zeng algorithm to find the matrices corresponding to the dual space
--up to degree d.
DZmatrix = method(TypicalValue => List, Options => {Point => {}})
DZmatrix (Matrix, ZZ) := o -> (igens, d) -> (
  epsilon := .001; --error tolerance for kernel
  R := ring igens;
  if o.Point != {} then igens = shift(igens, o.Point);
  dmons := apply(d+1, i->dMonomials(R,i));
  M := k -> ( --# of dual space generators with lead term of deg k or less
    p := igens;
    for m in flatten take(dmons,{1,k-1}) do p = p|(m*igens);
    (coefficients(p, Monomials => flatten take(dmons,{1,k})))#1
  );
  new List from apply(1..d, M)
)

--Stetter-Thallinger algorithm to find the matrices corresponding to the dual space
--up to degree d.
STmatrix = method(TypicalValue => List, Options => {Point => {}})
STmatrix (Matrix, ZZ) := o -> (igens, d) -> (
  epsilon := .001; --error tolerance for kernel
  R := ring igens;
  if o.Point != {} then igens = shift(igens, o.Point);
  dmons := apply(d+1, i->dMonomials(R,i));
  Ms := new MutableList;
  M := matrix {{}};
  for i from 1 to d do (
    mons := flatten take(dmons,{1,i});
    N := (coefficients(igens, Monomials => mons))#1;
    for v from 0 to #(gens R)-1 do (
      Mshift := new MutableList; --"antiderivative" of M with respect to variable v
      l := 0;
      for p from 0 to #mons-1 do (
	if (listForm mons#p)#0#0#v >= 1 and first degree mons#p > 1 then (
	  Mshift#p = (entries M)#l;
	  l = l+1;
	)
        else Mshift#p = apply(numgens source M, i->0);
      );
      --print (i,v,N,matrix new List from Mshift);
      N = N | matrix new List from Mshift;
    );
    M = gens gb N;
    Ms#(i-1) = M;
  );
  new List from Ms
)

--checks each monomial of degree d and counts ones in the monomial
--basis of the quotient space.
hilbertB = method(TypicalValue => ZZ, Options => {Point => {}})
hilbertB (Matrix, ZZ) := o -> (igens, d) -> (
  R := ring igens;
  if o.Point != {} then igens = shift(igens, o.Point);
  G := (flatten entries gens gb igens) / leadMonomial;
  #select(dMonomials(R,d), m->(#select(G, g->isDivisible(m,g)) == 0))
)

--takes alternating sum of number of all monomials, number that are
--a multiple of the lead term of each Groebner basis element, number
--in each pair-wise intersection, etc.
hilbertC = method(TypicalValue => ZZ, Options => {Point => {}})
hilbertC (Matrix, ZZ) := o -> (igens, d) -> (
  R := ring igens;
  if o.Point != {} then igens = shift(igens, o.Point);
  G := apply(flatten entries gens gb igens, g->(listForm g)#0#0); --store lead monomials as n-tuples of integers
  bin := (n,k) -> (if n >= 0 then binomial(n,k) else 0);
  listFormLCM := (a,b) -> apply(#a, i->(max {a#i,b#i}));
  recurC := (m, gIndex, coef) -> ( --for each subset S of G add or subtract # of d monomials that are divisible by lcm of S
    if gIndex == #G then
      coef*bin(d - sum m + #(gens R) - 1, #(gens R) - 1)
    else
      recurC(m, gIndex+1, coef) +
      recurC(listFormLCM(m,G#gIndex), gIndex+1, -1*coef)
  );
  newm := apply(#gens R, i->0);
  recurC(newm, 0, 1)
)

--produces a list of all monomials of degree d in the ring R in lex order
dMonomials = (R, d) -> (
  mons := new MutableHashTable;
  recurm := (m, varIndex, deg) -> (
    if varIndex == #(gens R)-1 then
      mons#(#mons) = m*(((gens R)#varIndex)^deg)
    else for k from 0 to deg do
      recurm(m*(((gens R)#varIndex)^k), varIndex+1, deg-k);
    true
  );
  recurm(1_R, 0, d);
  reverse values mons
)

isDivisible = (a, b) -> (
  dif := (listForm a)#0#0 - (listForm b)#0#0;
  #select(dif, i->(i < 0)) == 0
)

--translates the polynomials in matrix M so that Point becomes the origin
shift = (M, Point) -> (
  R := ring M;
  subs := apply(#(gens R), i->((gens R)#i => (gens R)#i + Point#i));
  sub(M, subs)
)

--performs Gaussian reduction on M but starting from the bottom right
rowReduce = (M,epsilon) -> (
  n := (numgens source M) - 1;
  m := (numgens target M) - 1;
  rindex := m;
  M = new MutableMatrix from M;
  for k from 0 to n do (
    M = new MutableMatrix from clean(epsilon,new Matrix from M);
    a := -1;
    for l from 0 to rindex do
      if (entries M)#l#(n-k) != 0 then (a = l; break);
    if a == -1 then continue;
    rowSwap(M,a,rindex);
    rowMult(M,rindex,1/((entries M)#rindex#(n-k)));
    for l from 0 to m do
      if l != rindex then rowAdd(M,l,-1*(entries M)#l#(n-k),rindex);
    rindex = rindex-1;
    --print new Matrix from M;
  );
  M = new Matrix from M
)

beginDocumentation()
document { 
  Key => hpackage,
  Headline => "some local Hilbert series functions",
  EM "hpackage", " is a basic package to be used as an example."
}
document {
  Key => hilbertA,
  Headline => "local Hilbert series of the dual space of an ideal",
  Usage => "hilbertA(igens,d)",
  Inputs => { "igens", "d" },
  Outputs => {{ "the", TT "d", "th element of the Hilbert series corresponding to the ideal with generators", TT "igens" }},
}
document {
  Key => hilbertB,
  Headline => "local Hilbert series of the quotient space of an ideal",
  Usage => "hilbertB(igens,d)",
  Inputs => { "igens", "d" },
  Outputs => {{ "the", TT "d", "th element of the Hilbert series corresponding to the ideal with generators", TT "igens" }},
}
document {
  Key => hilbertC,
  Headline => "local Hilbert series of the quotient space of an ideal",
  Usage => "hilbertC(igens,d)",
  Inputs => { "igens", "d" },
  Outputs => {{ "the", TT "d", "th element of the Hilbert series corresponding to the ideal with generators", TT "igens" }},
}

end

loadPackage "hpackage"
R = RR[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
M = matrix {{x*y,x^2-y^2,y^3}}
M = matrix {{x^2 - y}}
M = matrix {{x + y + x*y}}
STmatrix(M,4)
DZmatrix(M,4)
dualHilbert(M,4)
dualBasis(M,6)
dualHilbert(M,4, UseDZ=>true)
dualBasis(M,4, UseDZ=>true)
apply(0..4, i->hilbertB(M,i))
apply(0..4, i->hilbertC(M,i))

loadPackage "hpackage"
R = RR[x,y,z, MonomialOrder => {Weights=>{-1,-1,-1}}, Global => false]
M = matrix {{x*y+z, y*z+x, x^2-z^2}}
dualBasis(M,4)
dualHilbert(M,4)
