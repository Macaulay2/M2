-- -*- coding: utf-8 -*-
newPackage(
     "NumericalHilbert",
     Version => "0.1", 
     Date => "May 11, 2012",
     Authors => {{Name => "Robert Krone", 
    	       Email => "krone@math.gatech.edu"}},
     Headline => "some local Hilbert series functions",
     DebuggingMode => true
)

export {
     dualBasis,
     dualHilbert,
     standardBasis,
     Point,
     Tolerance,
     DZ,
     --ST,
     BM,
     GB1,
     GB2,
     DZS1,
     DZS2,
     DZSmatrix,
     ProduceSB,
     deflation,
     Size,
     dualSpace
     }

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


--Default tolerance value for inexact fields (the default is 0 for exact fields)
defaultT := () -> 0.0001;

--Returns a DualSpace object of generators of the dual space up to the specified degree d.
--Algorithm choices: Dayton-Zeng, Stetter-Thallinger, or Mourrain.
dualBasis = method(TypicalValue => DualSpace, Options => {Point => {}, Strategy => DZ, Tolerance => -1.})
dualBasis (Matrix, ZZ) := o -> (igens, d) -> (
     R := ring igens;
     tol := if o.Tolerance == -1. then (if precision 1_R == infinity then 0. else defaultT()) else o.Tolerance;
     if      o.Strategy == DZ then dualBasisDZST(igens, d, tol, Point => o.Point)
     --else if o.Strategy == ST then dualBasisDZST(igens, d, tol, Point => o.Point, Strategy => ST)
     else if o.Strategy == BM then dualBasisBM(igens, d, tol, Point => o.Point)
     else error "unrecognized strategy"
     );

standardBasis = method(TypicalValue => Matrix, Options => {Point => {}, Strategy => DZ, Tolerance => -1.}) 
standardBasis (Matrix) := o -> (igens) -> (
     R := ring igens;
     tol := if o.Tolerance == -1. then (if precision 1_R == infinity then 0. else defaultT()) else o.Tolerance;
     matrix {(DZSmatrix(igens, tol, ProduceSB => true))#3}
     );

--Returns a list of the dimensions of the quotient space at each degree up to the specified degree d.
--Algorithm choices: Dayton-Zeng, Stetter-Thallinger find the dual space; GB1, GB2 use a Groebner Basis.
dualHilbert = method(TypicalValue => List, Options => {Point => {}, Strategy => DZS1, Tolerance => -1.})
dualHilbert (Matrix, ZZ) := o -> (igens, d) -> (
     R := ring igens;
     tol := if o.Tolerance == -1. then (if precision 1_R == infinity then 0. else defaultT()) else o.Tolerance;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     if      o.Strategy == DZ then dualHilbertDZST(igens, d, tol)
     --else if o.Strategy == ST then dualHilbertDZST(igens, d, tol, Strategy => ST)
     else if o.Strategy == GB1 then apply(0..d, i->hilbertB(flatten entries gens gb igens,i))
     else if o.Strategy == GB2 then apply(0..d, i->hilbertC(flatten entries gens gb igens,i))
     else if o.Strategy == DZS1 or o.Strategy == DZS2 then (
     	  sbElements := sbReduce (DZSmatrix(igens,tol))#0;
     	  if o.Strategy == DZS1 then apply(0..d, i->hilbertB(sbElements,i))
	  else                       apply(0..d, i->hilbertC(sbElements,i))
	  )
     else error "unrecognized strategy"
     );

--Returns a list of the dimensions of the quotient space at each degree up to a degree at which it coincides
--with the Hilbert polynomial.
dualHilbert (Matrix) := o -> (igens) -> (
     R := ring igens;
     n := numgens R;
     tol := if o.Tolerance == -1. then (if precision 1_R == infinity then 0. else defaultT()) else o.Tolerance;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     sbMons := sbReduce (DZSmatrix(igens,tol))#0;
     sbListForm := apply(sbMons, e -> (listForm e)#0#0); --store lead monomials as n-tuples of integers
     d := sum(n, i->max(apply(sbListForm, l->l#i)));
     if      o.Strategy == DZS1 then apply(0..d, i->hilbertB(sbMons,i))
     else if o.Strategy == DZS2 then apply(0..d, i->hilbertC(sbMons,i))
     else error "unrecognized strategy"
     );

--Generators of the dual space with all terms of degree d or less.
--Uses ST algorithm by default, but can use DZ instead if specified.
dualBasisDZST = method(TypicalValue => DualSpace, Options => {Point => {}, Strategy => DZ})
dualBasisDZST (Matrix, ZZ, RR) := o -> (igens, d, tol) -> (
     R := ring igens;
     M := new Matrix;
     if o.Strategy == DZ then M = transpose DZmatrix(igens, d, false, Point => o.Point)
                         else M = transpose STmatrix(igens, d, Point => o.Point);
     dmons := apply(d+1, i->first entries basis(i,R)); --nested list of monomials up to order d
     dualSpace parseKernel(findKernel(M, tol), dmons, tol)
     );

findKernel = (M, tol) -> (
     R := ring M;
     M = sub(M, coefficientRing R);
     kerGens := new MutableList;
     if numgens target M == 0 then return id_(source M);
     if precision 1_R < infinity then (
	  (svs, U, Vt) := SVD M;
	  cols := toList select(0..#svs-1, i->(svs#i > tol));
	  submatrix'(transpose Vt,,cols)
	  ) else (
	  gens kernel M
	  )
     );

parseKernel = (kern, dmons, tol) -> (
     R := ring first flatten dmons;
     dualGens := transpose rowReduce(transpose kern, tol);
     (matrix {new List from flatten dmons})*sub(dualGens,R)
     );

--Implementation of algorithm from 1996 paper of Bernard Mourrain.
dualBasisBM = method(TypicalValue => Matrix, Options => {Point => {}})
dualBasisBM (Matrix, ZZ, RR) := o -> (igens, d, tol) -> (
     R := ring igens;
     n := #gens R;
     m := #(entries igens)#0;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     betas := {}; --previously found generators
     newbetas := {1_R}; --new generators
     npairs := subsets(n,2);
     M := map(R^(m),R^0,0); --the main matrix
     E := map(R^0,R^n,0); --stores evaluation of each dual generator integral on each ideal generator
     bvectors := map(R^1,R^0,0); --vectors of derivative coefficients of new generators (basis of kernel of M)
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
	  bvectors = entries transpose findKernel(M, tol);
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
     dualSpace (mons * transpose rowReduce(transpose bmatrix,tol))
     );

--version of the BM algorithm with automatic stopping criterion

--ST or DZ algorithms.
dualHilbertDZST = method(TypicalValue => List, Options => {Point => {}, Strategy => DZ})
dualHilbertDZST (Matrix, ZZ, RR) := o -> (igens, d, tol) -> (
     R := ring igens;
     n := #gens R;
     genDegs := (first entries igens)/lDegree;
     cList := apply(d+1, i->bin(i + n, n));
     rList := apply(d+1, i->sum(genDegs, j->bin(i-j+n,n)));
     M := new Matrix;
     if o.Strategy == DZ then M = transpose DZmatrix(igens, d, false, Point => o.Point)
     else                     M = transpose STmatrix(igens, d, Point => o.Point);
     L := apply(d+1, i->(
	       subM := M_(toList (0..(cList#i)-1))^(toList (0..(rList#i)-1));
	       numgens source findKernel(subM, tol)
	       ));
     L = {0}|L;
     apply(d+1, i->(L#(i+1)-L#i))
     );

--Constructs Sylvester array matrix using DZ algorithm
--(for use with automatic stopping criterion)
DZSmatrix = method(TypicalValue => List, Options => {Point => {}, ProduceSB => false})
DZSmatrix (Matrix, RR) := o -> (igens, tol) -> (
     R := ring igens;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     ecart := max apply(first entries igens, g->(gDegree g - lDegree g)); --max ecart of generators
     topDegs := apply(first entries igens, gDegree);
     dmons := {};
     monGens := {};
     SBasis := {};
     finalDeg := 2*(max topDegs);
     d := 0;
     oldBasis := {};
     while d <= finalDeg do (
     	  M := DZmatrix(igens, d, true);
	  dmons = append(dmons, first entries basis(d,R));
	  kern := findKernel(transpose M, tol);
	  newBasis := first entries parseKernel(kern, dmons, tol);
	  if tol > 0 then newBasis = apply(newBasis,b->clean(tol,b));
	  newMGs := newMonomialGens(monGens, newBasis, take(dmons,{d-ecart,d}), d);
	  print(d, " newMGs: ",newMGs);
	  --print newBasis;
	  if o.ProduceSB and #newMGs > 0 then (
	       kern2 := findKernel(transpose sub(kern,R), tol);
	       iBasis := first entries parseKernel(kern2, dmons, tol);
	       if tol > 0 then iBasis = apply(iBasis,b->clean(tol,b));
	       newSBs := new List from apply(newMGs, n->
		    (first select(1,iBasis, b->(leadMonomial b == n#0)),0)
		    );
	       SBasis = SBasis|newSBs;
	       );
	  if #newMGs > 0 then finalDeg = max(finalDeg,2*d);
	  monGens = monGens|newMGs;
     	  d = d+1;
	  oldBasis = newBasis;
	  );
     print monGens;
     print SBasis;
     (
	  monGens,
	  select(oldBasis,i->(gDegree i < d-ecart)),
	  d-ecart-1,
	  sbReduce SBasis,
	  hilbertPolynomial ideal toList apply(monGens,g->g#0)
	  )
     );

newMonomialGens = (oldGens, newBasis, dmons, d) -> (
     mons := sort(flatten dmons, MonomialOrder=>Descending);
     newBasis = sort(newBasis/gLeadMonomial, MonomialOrder=>Descending);
     --print(mons,newBasis);
     newGens := {};
     i := 0;
     for m in mons do (
	  if i < #newBasis and m == newBasis#i then (i = i+1; continue);
	  if any(oldGens, g->(isDivisible(m,g#0) and gDegree m - gDegree(g#0) <= d - g#1)) then continue;
	  newGens = append(newGens, (m,d));
     	  );
     newGens
     );

--Dayton-Zeng matrix to find the the dual space up to degree d.
DZmatrix = method(TypicalValue => List, Options => {Point => {}})
DZmatrix (Matrix, ZZ, Boolean) := o -> (igens, d, syl) -> (
     R := ring igens;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     igens = first entries igens;
     genDegs := apply(igens, if syl then gDegree else lDegree);
     dmons := apply(d+1, i->first entries basis(i,R));
     p := map(R^1,R^0,0);
     for j from 0 to d do (
     	  for i from 0 to #igens-1 do (
	       newp := if j >= genDegs#i then apply(dmons#(j-genDegs#i), m->(m*(igens#i))) else {};
               if #newp > 0 then p = p|matrix {newp};
	       );
     	  );
     (coefficients(p, Monomials => flatten take(dmons,{0,d})))#1
     );

--Stetter-Thallinger algorithm to find the matrices corresponding to the dual space
--up to degree d.
STmatrix = method(TypicalValue => List, Options => {Point => {}})
STmatrix (Matrix, ZZ) := o -> (igens, d) -> (
     R := ring igens;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     dmons := apply(d+1, i->first entries basis(i,R));
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

--checks each monomial of degree d and counts ones in the monomial basis of the quotient space
hilbertB = method(TypicalValue => ZZ)
hilbertB (List, ZZ) := (sbElements, d) -> (
     R := ring first sbElements;
     G := sbElements / leadMonomial;
     #select(first entries basis(d,R), m->(#select(G, g->isDivisible(m,g)) == 0))
     );

--finds Hilbert series values combinatorially using inclusion-exclusion
hilbertC = method(TypicalValue => ZZ)
hilbertC (List, ZZ) := (sbElements, d) -> (
     R := ring first sbElements;
     n := #gens R;
     sbListForm := apply(sbElements, e -> (listForm e)#0#0); --store lead monomials as n-tuples of integers
     listFormLcm := L -> apply(n, i->max{max(apply(L,l->l#i)), 0});
     coef := s -> if even(#s) then 1 else -1;
     hsum := 0;
     for s in subsets sbListForm do
	  hsum = hsum + (coef s)*bin(d - sum listFormLcm s + n-1, n-1);
     hsum
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
     );

--returns if lead term of a is divisible by lead term of b
isDivisible = (a, b) -> (
     dif := (listForm a)#0#0 - (listForm b)#0#0;
     all(dif, i->(i >= 0))
     );

--binomial coefficient 
bin = (m,k) -> (
     if m >= 0 then binomial(m,k) else 0
     );

--lead monomial and lead monomial degree according to ordering associated with
--the ring (local) and reverse ordering (global)
lLeadMonomial = f -> leadMonomial first terms f;
gLeadMonomial = f -> leadMonomial last terms f;
lDegree = f -> first degree lLeadMonomial f;
gDegree = f -> first degree gLeadMonomial f;

--performs Gaussian reduction on M but starting from the bottom right
rowReduce = (M,epsilon) -> (
     R := ring M;
     n := (numgens source M) - 1;
     m := (numgens target M) - 1;
     rindex := m;
     M = new MutableMatrix from M;
     for k from 0 to n do (
    	  --if epsilon > 0 then M = new MutableMatrix from clean(epsilon,new Matrix from M);
    	  a := -1;
	  aval := 0;
    	  for l from 0 to rindex do
      	       if abs M_(l,n-k) > max(epsilon,aval) then (a,aval) = (l, abs M_(l,n-k));
    	  if a == -1 then continue;
    	  rowSwap(M,a,rindex);
    	  rowMult(M,rindex,1_R/M_(rindex,(n-k)));
    	  for l from 0 to m do
      	       if l != rindex then rowAdd(M,l,-1*M_(l,n-k),rindex);
    	  rindex = rindex-1;
    	  --print new Matrix from M;
  	  );
     M = new Matrix from M
     );

--auxilary function
sbReduce = L -> (
     L = L / first;
     Lgood := select(#L, i->(
     	  all(#L, j->(j == i or not isDivisible(L#i,L#j)))
	  ));
     new List from apply(Lgood, i->L#i)
     );

--generates a vector of specified length of random complex numbers with unit modulus
randomVector = (dimension) -> (
     apply(dimension, i -> exp((random 2.*pi)*ii))
     );

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
     );

--evaluation of a dual element v on a polynomial w
innerProduct = (v,w) -> (
     c := entries (coefficients matrix{{v,w}})#1;
     sum(#c,i->(c#i#0)*(c#i#1))
     );

newtonsMethod = (eqns, p, n) -> (
     elist := (entries eqns)#0;
     A := entries transpose jacobian eqns;
     for i from 1 to n do (
    	  for j from 0 to #elist-1 do (
      	       val := sub(elist#j, p);
      	       grad := sub(A#j, p);
    	       );
  	  );
     );
{*
beginDocumentation()

doc ///
     Key
     	  NumericalHilbert
     Headline
     	  functions for numerically computing local dual space and Hilbert functions
     Description
     	  Text
	       @EM "NumericalHilbert"@ gets stuff done.
///

doc ///
     Key
          DualSpace
     Description
          Text
	        a Type.
///

doc ///
     Key
          dualHilbert
     Headline
          Calculate Hilbert series of the dual of a polynomial ideal
     Usage
          dualHilbert(gns, d)
     Inputs
          gns:Matrix
	       generators of an ideal in a one-row matrix
	  d:ZZ
     Outputs
          :List
     Description
          Text
	       Calculates dimension of the dual space of an ideal at each degree from 0 to d inclusive.
	  Example
///

doc ///
     Key
          dualBasis
     Headline
          Calculate basis of the dual space of a polynomial ideal
     Usage
          dualBasis(gns, d)
     Inputs
          gns:Matrix
	       generators of an ideal in a one-row matrix
	  d:ZZ
     Outputs
          :dualSpace
	       basis of the dual space in a one-row matrix
     Description
          Text
	       Calculates a reduced basis of the truncated dual space of an ideal.  It's truncated at degree d.
	       Elements are expressed as elements of the polynomial ring of the ideal although this is an abuse of notation.
	       They are really elements of the dual ring.
	  Example
///

doc ///
     Key
          standardBasis
     Headline
          Calculate the standard basis of a polynomial ideal numerically
     Usage
          standardBasis(gns)
     Inputs
          gns:Matrix
	       generators of an ideal in a one-row matrix
	  d:ZZ
     Outputs
          :Matrix
     Description
          Text
	       Finds a standard basis of the ideal using the dual basis calculation, which is numerically stable.
	  Example
///
*}
end

needsPackage "NumericalHilbert"
R = CC[x,y]
M = matrix {{y,x^2-y}}
deflation(M)

needsPackage "NumericalHilbert"
R = CC[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
R = QQ[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
M = matrix {{ii*x}}
M = matrix {{x*y, x^2-y^2, y^3}}
M = matrix {{x^2-y}}
M = matrix {{x+y+x*y}}
M = matrix {{x+y+y^3}}
V = dualBasis(M,5, Strategy => BM)
V = dualBasis(M,5, Strategy => DZ)
--V = dualBasis(M,5, Strategy => ST)
dualHilbert(M,4, Strategy => DZ)
--dualHilbert(M,4, Strategy => ST)
dualHilbert(M,4, Strategy => GB1)
dualHilbert(M,4, Strategy => GB2)

loadPackage ("NumericalHilbert", Reload => true)
R = CC[x,y,z, MonomialOrder => {Weights=>{-1,-1,-1}}, Global => false]
M = matrix {{x*y+z, y*z+x, x^2-z^2}}
M = matrix {{z*y-x^2, y^2}}
dualBasis(M,5)
dualHilbert(M,4)

restart
loadPackage ("NumericalHilbert")
R = CC[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
R = QQ[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
R = (ZZ/101)[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
M = matrix {{x^2-x*y^2,x^3}}
M = matrix {{x*y}}
M = matrix {{x^9 - y}}
standardBasis(M)
dualHilbert(M,25, Strategy => DZS1)
DZSmatrix(M,0.001)
DZSmatrix(M,0.001,Point=>{0.000000000000000000000000001,0.0000000000000000000000000000000001})
DZSmatrix(M,0.001, ProduceSB => true)
dualHilbert(M)

-- small example
restart
loadPackage ("NumericalHilbert", Reload => true)
R = QQ[x,y,z, MonomialOrder => {Weights=>{-1,-1,-1}}, Global => false]
M = matrix {{x-y^2, y-z^2}}
RH = QQ[t,x,y,z,MonomialOrder => {Weights=>{-1,-1,-1,-1}}, Global => false]
MH = homogenize(sub(M,RH),t)	  
DH = flatten entries gens dualBasis(MH,4)
DHhat = sub(matrix{select(DH, Q->first first last listForm Q >= 1)}, {t=>1})
(
     sort flatten entries gens dualBasis(M,3)
     ) == (
     sort unique flatten entries sub(DHhat,R)
     )
