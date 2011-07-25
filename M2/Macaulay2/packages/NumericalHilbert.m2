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

export {dualBasis, dualHilbert, Point, DZ, ST, BM, GB1, GB2, deflation, Size, dualSpace}

DualSpace = new Type of MutableHashTable
dualSpace = method()
dualSpace Matrix := m -> (
     m = matrix entries m;
     --if not isHomogeneous m then error "expected homogeneous matrix";
     R := ring m;
     S := (coefficientRing R)[];
     new DualSpace from {"generators" => map(R^1,, map(R,S), m)}
     );

gens DualSpace := o -> V -> V#"generators";

net DualSpace := V -> net V#"generators";

degree DualSpace := V -> max flatten degrees source V#"generators";

hilbertSeries DualSpace := o -> V -> (
     l := new MutableList from ((degree V + 1):0);
     scan(flatten degrees source V#"generators", d->(l#d = l#d + 1));
     new List from l
     );

isBasis = method()
isBasis DualSpace := V -> kernel V#"generators" == 0;

DualSpace == DualSpace := (V,W) -> kernel(V#"generators" + W#"generators") == kernel(V#"generators");


--Returns a DualSpace object of generators of the dual space up to the specified degree d.
--Algorithm choices: Dayton-Zeng, Stetter-Thallinger, or Mourrain.
dualBasis = method(TypicalValue => DualSpace, Options => {Point => {}, Strategy => DZ})
dualBasis (Matrix, ZZ) := o -> (igens, d) -> (
     if      o.Strategy == DZ then dualBasisDZST(igens, d, Point => o.Point)
     else if o.Strategy == ST then dualBasisDZST(igens, d, Point => o.Point, Strategy => ST)
     else if o.Strategy == BM then dualBasisBM(igens, d, Point => o.Point)
     else error "unrecognized strategy"
     );

--Returns a list of the dimensions of the quotient space at each degree up to the specified degree d.
--Algorithm choices: Dayton-Zeng, Stetter-Thallinger find the dual space; GB1, GB2 use a Groebner Basis.
dualHilbert = method(TypicalValue => List, Options => {Point => {}, Strategy => DZ})
dualHilbert (Matrix, ZZ) := o -> (igens, d) -> (
     if      o.Strategy == DZ then dualHilbertDZST(igens, d, Point => o.Point)
     else if o.Strategy == ST then dualHilbertDZST(igens, d, Point => o.Point, Strategy => ST)
     else if o.Strategy == GB1 then apply(0..d, i->hilbertB(igens,i))
     else if o.Strategy == GB2 then apply(0..d, i->hilbertC(igens,i))
     else error "unrecognized strategy"
     );

--Generators of the dual space with all terms of degree d or less.
--Uses ST algorithm by default, but can use DZ instead if specified.
dualBasisDZST = method(TypicalValue => DualSpace, Options => {Point => {}, Strategy => DZ})
dualBasisDZST (Matrix, ZZ) := o -> (igens, d) -> (
     R := ring igens;
     epsilon := .001; --error tolerance for kernel for inexact fields
     if precision 1_R == infinity then epsilon = 0;
     M := new Matrix;
     if o.Strategy == DZ then M = transpose last DZmatrix(igens, d, Point => o.Point)
                         else M = transpose last STmatrix(igens, d, Point => o.Point);
     dmons := apply(1..d, i->dMonomials(R,i)); --nested list of monomials up to order d
     if M != 0 then (
    	  dualGens := transpose rowReduce(transpose findKernel(M, epsilon), epsilon);
	  --print(numgens source M, numgens target M);
    	  dualSpace ((matrix {{1_R}}) | ((matrix {new List from flatten dmons})*sub(dualGens,R)))
  	  )
     else dualSpace ((matrix {{1_R}}) | (matrix {new List from flatten dmons}))
     );

findKernel = (M, epsilon) -> (
     R := ring M;
     kerGens := new MutableList;
     if precision 1_R < infinity then (
       	  (svs, U, Vt) := SVD sub(M, coefficientRing R);
       	  Vt = entries Vt;
       	  for i to #Vt-1 do
	       if i > #svs-1 or abs svs#i <= epsilon then kerGens#(#kerGens) = apply(Vt#i, conjugate);
       	  transpose matrix new List from kerGens
	  ) else (
	  gens kernel sub(M, coefficientRing R)
	  )
     )

--Implementation of algorithm from 1996 paper of Bernard Mourrain.
dualBasisBM = method(TypicalValue => Matrix, Options => {Point => {}})
dualBasisBM (Matrix, ZZ) := o -> (igens, d) -> (
     R := ring igens;
     n := #gens R;
     m := #(entries igens)#0;
     epsilon := .001; --error tolerance for kernel for inexact fields
     if precision 1_R == infinity then epsilon = 0;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     betas := {}; --previously found generators
     newbetas := {1_R}; --new generators
     npairs := subsets(n,2);
     M := map(R^(m),R^0,0); --the main matrix
     E := map(R^0,R^n,0); --stores evaluation of each dual generator on each ideal generator
     bvectors := map(R^1,R^0,0);
     buildVBlock := v -> ( --function to build new blocks to add to M
 	  Vb := mutableMatrix(R,#npairs,n);
    	  for i from 0 to #npairs-1 do (
      	       Vb_(i,npairs#i#0) =  v#(npairs#i#1);
      	       Vb_(i,npairs#i#1) = -v#(npairs#i#0);
    	       );
    	  new Matrix from Vb
  	  );
     V := {{}};
     
     for e from 1 to d+1 do (
	  --print (m, M, E, bvectors, betas, newbetas);
	  s := #betas;
	  snew := #newbetas;
	  M = bvectors || M;
    	  for i from 0 to #newbetas-1 do (
	       b := newbetas#i;
      	       --get alpha vector for b
      	       alpha := apply(n, k->(
	       	    subs := matrix{apply(n, l->(if l > k then 0_R else (gens R)#l))};
		    (gens R)#k * sub(b,subs)
	       	    ));
      	       --get new A from new alpha
	       A := matrix apply(m, j->apply(alpha,a->innerProduct(a,igens_(0,j))));
	       --expand E with alpha as next row
	       E = E || matrix{alpha};
	       --expand M with Vs and new A
	       newcol := map(R^(s+snew),R^n,0) || A;
	       for v in V#i do newcol = newcol || v;
	       --print (M,newcol, numgens target M, numgens target newcol);
	       M = M | newcol;
      	       );
    	  --add newbetas to betas
	  betas = betas | newbetas;
	  s = #betas;
	  bvectors = entries transpose findKernel(M, epsilon);
    	  --find newbetas from bvectors
	  newbetas = apply(bvectors, bv->sum(#bv, i->(bv#i * E_(i//n,i%n))));
    	  --build Vs from bvectors
	  V = apply(#bvectors, i->(
	       w := apply(s, j-> apply(n,k->((bvectors#i)#(j*n + k))));
	       --print ("w",w);
	       apply(s, j->buildVBlock(w#j))
	       ));
	  if #bvectors > 0 then bvectors = matrix bvectors else break;
	  M = M || map(R^(snew*#npairs),R^(n*s),0);
  	  );
     (mons,bmatrix) := coefficients matrix {betas};
     --print(mons,bmatrix);
     bmatrix = sub(bmatrix,coefficientRing R);
     --print(numgens source M, numgens target M);
     dualSpace (mons * transpose rowReduce(transpose bmatrix,epsilon))
     );

dualBasisBMS = method(TypicalValue => Matrix, Options => {Point => {}})
dualBasisBMS (Matrix, ZZ) := o -> (igens, d) -> (
     R := ring igens;
     n := #gens R;
     m := #first entries igens;
     epsilon := .001; --error tolerance for kernel
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     ecart := max apply(first entries igens, g->(first degree g - first degree leadMonomial g)); --max ecart of generators
     betas := {}; --previously found generators
     newbetas := {1_R}; --new generators
     npairs := subsets(n,2);
     M := map(R^m,R^0,0); --the main matrix
     E := map(R^0,R^n,0); --stores evaluation of each dual generator on each ideal generator
     bvectors := map(R^1,R^0,0);
     buildVBlock := v -> ( --function to build new blocks to add to M
 	  Vb := mutableMatrix(R,#npairs,n);
    	  for i from 0 to #npairs-1 do (
      	       Vb_(i,npairs#i#0) =  v#(npairs#i#1);
      	       Vb_(i,npairs#i#1) = -v#(npairs#i#0);
    	       );
    	  new Matrix from Vb
  	  );
     V := {{}};
     
     for e from 1 to d+1 do (
	  --print (m, M, E, bvectors, betas, newbetas);
	  s := #betas;
	  snew := #newbetas;
	  M = bvectors || M;
    	  for i from 0 to #newbetas-1 do (
	       b := newbetas#i;
      	       --get alpha vector for b
      	       alpha := apply(n, k->(
	       	    subs := matrix{apply(n, l->(if l > k then 0_R else (gens R)#l))};
		    (gens R)#k * sub(b,subs)
	       	    ));
      	       --get new A from new alpha
	       A := matrix apply(m, j->apply(alpha,a->innerProduct(a,igens_(0,j))));
	       --expand E with alpha as next row
	       E = E || matrix{alpha};
	       --expand M with Vs and new A
	       newcol := map(R^(s+snew),R^n,0) || A;
	       for v in V#i do newcol = newcol || v;
	       --print (M,newcol, numgens target M, numgens target newcol);
	       M = M | newcol;
      	       );
    	  --add newbetas to betas
	  betas = betas | newbetas;
	  s = #betas;
    	  --get bvectors from kernel of M
    	  (svs, U, Vt) := SVD sub(M,coefficientRing R);
	  --print (svs,U,Vt);
    	  Vt = entries Vt;
    	  bvectors = new MutableList;
    	  for i to #Vt-1 do
	       if i > #svs-1 or abs svs#i <= epsilon then bvectors#(#bvectors) = apply(Vt#i, conjugate);
	  bvectors = new List from bvectors;
    	  --find newbetas from bvectors
	  newbetas = apply(bvectors, bv->sum(#bv, i->(bv#i * E_(i//n,i%n))));
    	  --build Vs from bvectors
	  V = apply(#bvectors, i->(
	       w := apply(s, j-> apply(n,k->((bvectors#i)#(j*n + k))));
	       --print ("w",w);
	       apply(s, j->buildVBlock(w#j))
	       ));
	  if #bvectors > 0 then bvectors = matrix bvectors else break;
	  M = M || map(R^(snew*#npairs),R^(n*s),0);
  	  );
     (mons,bmatrix) := coefficients matrix {betas};
     --print(mons,bmatrix);
     bmatrix = sub(bmatrix,coefficientRing R);
     print(numgens source M, numgens target M);
     dualSpace (mons * transpose rowReduce(transpose bmatrix,epsilon))
     );

--ST or DZ algorithms.
dualHilbertDZST = method(TypicalValue => List, Options => {Point => {}, Strategy => DZ})
dualHilbertDZST (Matrix, ZZ) := o -> (igens, d) -> (
     R := ring igens;
     epsilon := .001; --error tolerance for kernel for inexact fields
     if precision 1_R == infinity then epsilon = 0;
     mList := new Matrix;
     if      o.Strategy == DZ then mList = DZmatrix(igens, d, Point => o.Point)
     else if o.Strategy == ST then mList = STmatrix(igens, d, Point => o.Point)
     else error "unrecognized strategy";
     fs := apply(mList, M->(
	  if epsilon > 0 then (
	       svs := {};
    	       if M != 0 then svs = (SVD sub(M,coefficientRing ring igens))#0;
    	       (numgens target M) - #select(svs, v->(abs v > epsilon)) + 1
	       ) else (
	       1 + numgens kernel transpose M
	       )
  	  ));
     fs = {0,1}|fs;
     apply(d+1, i->(fs#(i+1) - fs#i))
     );

DZSmatrix = method(TypicalValue => List, Options => {Point => {}})
DZSmatrix (Matrix) := o -> (igens) -> (
     R := ring igens;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     topds := apply(igens, degree);
     M := new MutableHashTable;
     dmons := {};
     lastd := 0;
     d := 0;
     while d < lastd + (max topds) do (
     	  dmons = dmons|{dMonomials(R,d)};
    	  p := {};
	  for i from 0 to #igens-1 do
	       if d-topds#i >= 0 then p|apply(dmons#(d-topds#i), m->(m*(igens#i)));
    	  M#d = (coefficients(p, Monomials => flatten take(dmons,{0,d})))#1;
     	  d = d+1;
	  );
     {}
     );

--Dayton-Zeng algorithm to find the matrices corresponding to the dual space
--up to degree d.
DZmatrix = method(TypicalValue => List, Options => {Point => {}})
DZmatrix (Matrix, ZZ) := o -> (igens, d) -> (
     R := ring igens;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     dmons := apply(d+1, i->dMonomials(R,i));
     M := k -> ( --# of dual space generators with lead term of deg k or less
    	  p := igens;
    	  for m in flatten take(dmons,{1,k-1}) do p = p|(m*igens);
    	  (coefficients(p, Monomials => flatten take(dmons,{1,k})))#1
  	  );
     new List from apply(1..d, M)
     );

--Stetter-Thallinger algorithm to find the matrices corresponding to the dual space
--up to degree d.
STmatrix = method(TypicalValue => List, Options => {Point => {}})
STmatrix (Matrix, ZZ) := o -> (igens, d) -> (
     R := ring igens;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
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
     );

--checks each monomial of degree d and counts ones in the monomial basis of the quotient space.
hilbertB = method(TypicalValue => ZZ, Options => {Point => {}})
hilbertB (Matrix, ZZ) := o -> (igens, d) -> (
     R := ring igens;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     G := (flatten entries gens gb igens) / leadMonomial;
     #select(dMonomials(R,d), m->(#select(G, g->isDivisible(m,g)) == 0))
     );

--takes alternating sum of number of all monomials, number that are a multiple of the lead term of
--each Groebner basis element, number in each pair-wise intersection, etc.
hilbertC = method(TypicalValue => ZZ, Options => {Point => {}})
hilbertC (Matrix, ZZ) := o -> (igens, d) -> (
     R := ring igens;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
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
     );

deflation = method(TypicalValue => ZZ, Options => {Point => matrix {{}}, Size => 0})
deflation (Matrix) := o -> (igens) -> (
  R := ring igens;
  n := #(gens R);
  epsilon := .001;
  p := sub(o.Point,CC);
  m := o.Size;
  if p == 0 then p = matrix {apply(n, i->0_CC)};
  if m == 0 then m = n;
  A := jacobian igens;
  Asub := sub(A, p);
  svs := (SVD(Asub))#0;
  r := 0;
  for v in svs do
    if clean(epsilon,v) != 0 then r = r + 1;
  if r == n then return take((entries p)#0,m);
  h := randomVector(r+1);
  Br := matrix apply(n, i->randomVector(r+1));
  Bl := matrix apply(r, i->randomVector(n));
  C := (Bl*Asub*Br) || matrix{h};
  p = p | transpose solve(C, transpose matrix{(apply(r,i->0_CC))|{1_CC}});
  lambda := symbol lambda;
  R = CC[gens R,lambda_n..lambda_(n+r)];
  lvec := transpose matrix{new List from lambda_n..lambda_(n+r)};
  igens = sub(igens,R) | transpose (sub(A,R)*Br*lvec) | transpose (matrix{h}*lvec - 1);
  print transpose igens;
  deflation(igens, Point => p, Size => m)
)

--homog = f -> (
--  R := ring f;
--  S := (coefficientRing R)[t,gens R];
--  homogenize(sub(f,S),t)   
--)

dehomog = f -> (
  R := ring f;
  sub(f,R_0=>1)
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

--performs Gaussian reduction on M but starting from the bottom right
rowReduce = (M,epsilon) -> (
  R := ring M;
  n := (numgens source M) - 1;
  m := (numgens target M) - 1;
  rindex := m;
  M = new MutableMatrix from M;
  for k from 0 to n do (
    if epsilon > 0 then M = new MutableMatrix from clean(epsilon,new Matrix from M);
    a := -1;
    for l from 0 to rindex do
      if (entries M)#l#(n-k) != 0 then (a = l; break);
    if a == -1 then continue;
    rowSwap(M,a,rindex);
    rowMult(M,rindex,1_R/M_(rindex,(n-k)));
    for l from 0 to m do
      if l != rindex then rowAdd(M,l,-1*(entries M)#l#(n-k),rindex);
    rindex = rindex-1;
    --print new Matrix from M;
  );
  M = new Matrix from M
)

--generates a vector of specified length of random complex numbers with unit modulus
randomVector = (dimension) -> (
  apply(dimension, i -> exp((random 2.*pi)*ii))
)

--build a matrix from a nested list of matrices of uniform size
blockMatrix = (blist) -> (
  R := ring (blist#0#0);
  m := #blist;
  n := #(blist#0);
  a := dim target (blist#0#0);
  b := dim source (blist#0#0); 
  M := map(R^0,R^(n*b),0);
  for i from 0 to m-1 do (
    N := map(R^a,R^0,0);
    for j from 0 to n-1 do
      N = N | (blist#i#j);
    M = M || N;
  );
  M
)

--evaluation of a dual element v on a polynomial w
innerProduct = (v,w) -> (
  c := entries (coefficients matrix{{v,w}})#1;
  sum(#c,i->(c#i#0)*(c#i#1))
)

newtonsMethod = (eqns, p, n) -> (
  elist := (entries eqns)#0;
  A := entries transpose jacobian eqns;
  for i from 1 to n do (
    for j from 0 to #elist-1 do (
      val := sub(elist#j, p);
      grad := sub(A#j, p);
    );
  );
)

beginDocumentation()
document { 
  Key => NumericalHilbert,
  Headline => "some local Hilbert series functions",
  EM "NumericalHilbert", " is a basic package to be used as an example."
}

///
document {
  Key => hilbertA,
  Headline => "local Hilbert series of the dual space of an ideal",
  Usage => "hilbertA(igens,d)",
  Inputs => { "igens", "d" },
  Outputs => {{ "the", TT "d", "th element of the Hilbert series corresponding to the ideal with generators", TT "igens" }},
}
///

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

loadPackage "NumericalHilbert"
R = CC[x,y]
M = matrix {{y,x^2-y}}
deflation(M)

loadPackage "NumericalHilbert"
loadPackage ("NumericalHilbert", Reload => true)
R = RR[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
R = QQ[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
M = matrix {{x*y, x^2-y^2, y^3}}
M = matrix {{x^2-y}}
M = matrix {{x+y+x*y}}
M = matrix {{x+y+y^3}}
V = dualBasis(M,5, Strategy => BM)
V = dualBasis(M,5, Strategy => DZ)
V = dualBasis(M,5, Strategy => ST)
transpose matrix entries gens V
degree V
hilbertSeries V
isBasis V
dualHilbert(M,4, Strategy => DZ)
dualHilbert(M,4, Strategy => ST)
dualHilbert(M,4, Strategy => GB1)
dualHilbert(M,4, Strategy => GB2)

loadPackage "NumericalHilbert"
R = CC[x,y,z, MonomialOrder => {Weights=>{-1,-1,-1}}, Global => false]
M = matrix {{x*y+z, y*z+x, x^2-z^2}}
M = matrix {{z*y-x^2, y^2}}
dualBasis(M,5)
dualHilbert(M,4)
