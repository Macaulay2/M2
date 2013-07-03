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
     dualInfo,
     Point,
     Tolerance,
     DZ,
     ST,
     BM,
     GB,
     dualSpace,
     ProduceSB,
     rowReduce,
     eliminatingDual,
     colonDual
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

DualSpace == DualSpace := (V,W) -> image(W#"generators") == image(V#"generators");

-----------------------------------------------------------------------------------------

--Default tolerance value for inexact fields (the default is 0 for exact fields)
defaultT := () -> 0.0001;


dualBasis = method(TypicalValue => DualSpace, Options => {Truncate => -1, Point => {}, Strategy => BM, Tolerance => -1.})
dualBasis (Matrix) := o -> (igens) -> (dualInfo(igens, Truncate=>o.Truncate, Point=>o.Point, Strategy=>o.Strategy, Tolerance=>o.Tolerance))#0;

standardBasis = method(TypicalValue => Matrix, Options => {Truncate => -1, Point => {}, Strategy => BM, Tolerance => -1.}) 
standardBasis (Matrix) := o -> (igens) -> (dualInfo(igens, Truncate=>o.Truncate, Point=>o.Point, Strategy=>o.Strategy, Tolerance=>o.Tolerance, ProduceSB=>true))#2;

dualHilbert = method(TypicalValue => List, Options => {Truncate => -1, Point => {}, Strategy => BM, Tolerance => -1.})
dualHilbert (Matrix) := o -> (igens) -> (dualInfo(igens, Truncate=>o.Truncate, Point=>o.Point, Strategy=>o.Strategy, Tolerance=>o.Tolerance))#4;

dualInfo = method(TypicalValue => Sequence, Options => {Truncate => -1, Point => {}, Strategy => BM, Tolerance => -1., ProduceSB => false})
dualInfo (Matrix) := o -> (igens) -> (
     R := ring igens;
     tol := o.Tolerance;
     deg := o.Truncate;
     if tol == -1. then (if precision 1_R == infinity then tol = 0. else tol = defaultT());
     if o.Point != {} then igens = sub(igens, matrix{gens R + apply(o.Point,p->sub(p,R))});
     print transpose igens;
     
     --outputs
     dbasis   := new Matrix;
     gcorners := new List;
     regul    := 0;
     hseries  := new Sequence;
     hpoly    := 0;
     
     --truncated strategies
     if deg != -1 then (
     	  --Macaulay matrix strategies (DZ, ST)
     	  if o.Strategy == DZ or o.Strategy == ST then (
	       M := new Matrix;
     	       if o.Strategy == DZ then M = transpose DZmatrix(igens, deg, false);
     	       if o.Strategy == ST then M = transpose STmatrix(igens, deg);
	       
     	       dmons := apply(deg+1, i->first entries basis(i,R)); --nested list of monomials up to order d
     	       dbasis = parseKernel(findKernel(M, tol), dmons, tol);
     	       
	       n := numgens R;
	       genDegs := (first entries igens)/lDegree;
     	       cList := apply(deg+1, i->bin(i + n, n));
     	       rList := apply(deg+1, i->sum(genDegs, j->bin(i-j+n,n)));
     	       L := apply(deg+1, i->(
	       		 subM := M_(toList (0..(cList#i)-1))^(toList (0..(rList#i)-1));
	       		 numgens source findKernel(subM, tol)
	       		 ));
               L = {0}|L;
     	       hseries = apply(deg+1, i->(L#(i+1)-L#i));
	       );
     	  
     	  --Mourrain algorithm (BM)
     	  if o.Strategy == BM then
	       (dbasis,hseries) = dualBasisBM(igens, deg, tol);
	       dbasis = flatten entries dbasis;
     	  );
     
     --Sylvester array strategies
     if deg == -1 then (
	  sbasis := {};
	  if o.Strategy == DZ or o.Strategy == BM then (
	       d := 0;
	       (gcorners,dbasis,d,sbasis,hpoly) = dualBasisSA(igens, tol, ProduceSB => o.ProduceSB, Strategy => o.Strategy);
	       gcorners = sbReduce gcorners;
	       );
	  if o.Strategy == GB then (
	       sbasis = first entries gens gb igens;
	       print sbasis;
	       gcorners = sbasis/lLeadMonomial;
	       );
	  regul = first degree lcm monomialIdeal gcorners;
	  hseries = apply(regul, i->hilbertC(gcorners, i));
	  if o.ProduceSB then gcorners = sbasis;
	  );
     
     (dbasis, gcorners, regul, hseries, hpoly)
     );

eliminatingDual = method(TypicalValue => List, Options => {Point => {}, Tolerance => -1.})
eliminatingDual (Matrix, ZZ, List) := o -> (igens, r, varList) -> (
     R := ring igens;
     n := numgens R;
     tol := o.Tolerance;
     if tol == -1. then (if precision 1_R == infinity then tol = 0. else tol = defaultT());
     if o.Point != {} then igens = sub(igens, matrix{gens R + apply(o.Point,p->sub(p,R))});
     lastd := -1;
     d := 0;
     dmons := {};
     eBasis := {};
     eBasisSize := 0;
     ecart := max apply(first entries igens, g->(gDegree g - lDegree g));
     wvec := new MutableList from (n:0);
     for v in varList do apply(n, i->(if v == R_i then wvec#i = -1));
     wvec = toList wvec;
     S := (coefficientRing R)[gens R, MonomialOrder => {Weights=>wvec,Weights=>n:-1}, Global => false];
     while d <= lastd + ecart + 1 do (
	  dmons = dmons | entries basis(d,R);
	  M := transpose DZmatrix(igens,d,false);
	  kern := findKernel(M,tol);
	  dualBasis := (matrix{flatten dmons})*sub(kern,R);
	  dualBasis = sub(dualBasis,S);
	  (mons, N) := coefficients dualBasis;
	  dualBasis = flatten entries parseKernel(sub(N,coefficientRing S),entries mons,tol);
	  eBasis = select(dualBasis, b->(degree(S_0, last terms b) <= r));
	  if #eBasis > eBasisSize then lastd = d;
	  eBasisSize = #eBasis;
	  d = d+1;
	  );
     apply(eBasis, b->sub(b,R))
     );

colonDual = method(TypicalValue => List)
colonDual (List, List) := (dualSpace, L) -> (
     R := ring first dualSpace;
     for l in L do (
	  (m,c) := coefficients matrix{dualSpace};
     	  m = flatten entries m;
	  M := matrix{toList(#m:0_R)};
	  for term in terms l do (
	       mdiff := apply(m, mon->(
			 d := diff(term,mon);
			 if d != 0 then d = leadMonomial d;
			 d
			 ));
	       M = M + (leadCoefficient term)*(matrix{mdiff});
	       );
	  dualSpace = flatten entries (M*c);
	  );
     dualSpace
     );

{*
initializeD = (igens, d, tol, strategy) -> (
     ecart := max apply(first entries igens, g->(gDegree g - lDegree g));
     tdeg := d;
     if d == -1 then tdeg = max(apply(first entries igens, gDegree));
     if d == -1 and strategy == BM then (
	  0
	  );
     D := new MutableHashTable from {
	  igens => igens,
	  deg => -1,
	  tol => tol;
	  targetDegree => tdeg,
	  ecart => ecart,
	  Strategy => strategy, --DZ, BM or GB
	  SA => (d == -1),
	  M => null, --the main matrix
	  kernelM => {}, --a reduced basis for the kernel of M
	  dualBasis => {},
	  colBasis => {}, --a list of the polynomials corresponding to the columns of M
	  dmons => {}, --a nested list of all monomials at each degree
	  hseries => {}, --the Hilbert series computed so far
	  gcorners => {}, --the g-corners computed so far
	  sbasis => {}, --the reduced standard basis elements computed so far
	  }
     );

advanceD = (D) -> (
     D.deg = D.deg+1;
     if D.Strategy == DZ then D.M = DZmatrix(D.igens, D.deg, D.SA);
   --if D.Strategy == ST then D.M = STmatrix(D.igens, D.deg);
     --if D.Strategy == BM then (D.M, D.colBasis) = BMmatrix(D);
     --(D.kernelM, D.dualBasis) = kern(D.M, D.colBasis, D.tol);
     if not D.SA then (
	  h' := if D.deg = 0 then 0 else D.hseries#(D.deg-1);
	  h := #D.dualBasis - h';
	  D.hseries = append(h, D.hseries);
	  )
     );
*}

--Finds kernel numerically with SVD or symbolically depending on the base field
findKernel = (M, tol) -> (
     R := ring M;
     M = sub(M, coefficientRing R);
     kerGens := new MutableList;
     if numgens target M == 0 then return id_(source M);
     if precision 1_R < infinity then (
	  (svs, U, Vt) := SVD M;
	  cols := toList select(0..#svs-1, i->(svs#i > tol));
	  submatrix'(adjointMatrix Vt,,cols)
	  ) else (
	  gens kernel M
	  )
     );

--Takes kernel coefficient matrix, and produces row reduced polynomial basis
parseKernel = (kern, dmons, tol) -> (
     R := ring first flatten dmons;
     dualGens := transpose rowReduce(transpose kern, tol);
     --print dualGens;
     --print flatten dmons;
     (matrix {new List from flatten dmons})*sub(dualGens,R)
     );

--Implementation of algorithm from 1996 paper of Bernard Mourrain.
dualBasisBM = method(TypicalValue => Matrix, Options => {Point => {}})
dualBasisBM (Matrix, ZZ, RR) := o -> (igens, d, tol) -> (
     R := ring igens;
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     hseries := new MutableList;
     betas := {}; --all previously found generators
     bpairs := {}; --most recently found generators
     M := {}; --the main matrix
     E := {}; --list of all dual basis integrals (including 1)
     
     for e from 0 to d do (
	  (M,E,bpairs) = BMmatrix(igens,M,E,bpairs,tol,false);
	  betas = betas | bpairs/last;
	  hseries#e = #bpairs;
	  if #bpairs == 0 then break;
  	  );
     
     if #betas == 0 then return (map(R^1,R^0,0), new Sequence from hseries);
     (mons,bmatrix) := coefficients matrix {betas};
     bmatrix = sub(bmatrix,coefficientRing R);
     (mons * transpose rowReduce(transpose bmatrix,tol), new Sequence from hseries)
     );

BMmatrix = (igens, M, E, bpairs, tol, homogeneous) -> (
     --print(igens,M,E,bpairs);
     R := ring igens;
     n := numgens R;
     m := numcols igens;
     snew := #bpairs;
     offset := if homogeneous then 0 else 1;
     s := (#E - offset)//n;
     npairs := subsets(n,2);
     if snew == 0 then (M,E) = (transpose sub(igens,map(R^1,R^n,0)), {1_R}) else ( --degree 0
     	  if not homogeneous then (
     	       M = M || map(R^(m + s*(1+#npairs) - numrows M),R^(#E),0);
     	       M = matrix(bpairs/first) || M;
	       ) else (
	       M = map(R^(m + s*#npairs),R^0,0);
	       E = {};
	       );
     	  for bp in bpairs do (
	       E' := apply(n, k->(
	       	    	 subs := matrix{apply(n, l->(if l > k then 0_R else (gens R)#l))};
		    	 (gens R)#k * sub(bp#1,subs)
	       	    	 ));
	       E = E | E';
	       M' := matrix apply(m, j->apply(E',a->innerProduct(a,igens_(0,j))));
	       if not homogeneous then M' = map(R^(s+snew),R^n,0) || M';
	       for j from 0 to s-1 do (
	       	    w := apply(n,k->((bp#0)#(offset + j*n + k)));
	       	    v := mutableMatrix(R,#npairs,n);
	       	    for i from 0 to #npairs-1 do (
		    	 v_(i,npairs#i#0) =  w#(npairs#i#1);
		    	 v_(i,npairs#i#1) = -w#(npairs#i#0);
		    	 );
	       	    M' = M' || new Matrix from v;
	       	    );
	       M = M | M';
	       );
	  );
     bvectors := entries transpose findKernel(M, tol);
     bpairs = apply(bvectors, bv->(bv, sum(#bv, i->(bv#i * E#i))));
     (M, E, bpairs)
     );

--Dual basis algorithm with automatic stopping criterion.
--DZ strategy uses Sylvester arrays.  BM strategy uses homogenization.
dualBasisSA = method(TypicalValue => List, Options => {Point => {}, ProduceSB => false, Strategy => BM})
dualBasisSA (Matrix, RR) := o -> (igens, tol) -> (
     R := ring igens;
     n := numgens R;
     x := symbol x;
     S := (coefficientRing R)[x_0..x_n, MonomialOrder => {Weights => (n+1):-1}, Global => false]; --projectivization of R
     homog := f -> homogenize((map(S,R,drop(gens S, 1))) f, x_0);
     dehomog := map(R,S,{1_R} | gens R);
     if o.Point != {} then igens = sub(igens, matrix{gens R + o.Point});
     ecart := max apply(first entries igens, g->(gDegree g - lDegree g)); --max ecart of generators
     topDegs := apply(first entries igens, gDegree);
     dmons := {}; --list of monomials up to degree d
     monGens := {}; --monomials generating initial ideal (g-corners)
     SBasis := {}; --standard basis elements (if ProduceSB => true)
     finalDeg := max(topDegs);
     d := 0;
     oldBasis := {}; newBasis := {};
     M := {}; E := {}; bpairs := {};
     kern := {};
     while d <= finalDeg do (
	  dmons = append(dmons, first entries sort(basis(d,R), MonomialOrder=>Descending));
	  --print last dmons;
	  if o.Strategy == BM then (
	       higens := homog igens;
	       (M,E,bpairs) = BMmatrix(higens,M,E,bpairs,tol,true);
	       betas := apply(bpairs, bp->(dehomog last bp));
	       if #betas != 0 then kern = last coefficients(matrix{betas}, Monomials=>flatten dmons)
 	       else kern = map(R^(#(flatten dmons)),R^0,0);
	       kern = sub(kern, coefficientRing R);
	       ) else (
	       M = DZmatrix(igens,d,true);
	       kern = findKernel(transpose M, tol);
	       );
	  newBasis = first entries parseKernel(kern, dmons, tol);
	  --if tol > 0 then newBasis = apply(newBasis,b->clean(10*tol,b));
	  --print transpose matrix{newBasis/gLeadMonomial, newBasis};
	  newMGs := newMonomialGens(monGens, newBasis, take(dmons,{d-ecart,d}), d);
	  if o.ProduceSB and #newMGs > 0 then (
	       kern2 := findKernel(transpose sub(kern,R), tol);
	       iBasis := first entries parseKernel(kern2, dmons, tol);
	       if tol > 0 then iBasis = apply(iBasis,b->clean(tol,b));
	       newSBs := new List from apply(newMGs, n->
		    (first select(1,iBasis, b->(leadMonomial b == n#0)),0)
		    );
	       SBasis = SBasis|newSBs;
	       );
	  monGens = monGens|newMGs;
	  if #newMGs > 0 then (
	       topLCM := max apply(subsets(#monGens,2),s->(
		         homogenizedLCMdegree(monGens#(s#0), monGens#(s#1))));
	       finalDeg = max(finalDeg,topLCM);
	       );
	  print(d,(numrows M,numcols M), #(flatten dmons), newMGs/first);
     	  d = d+1;
	  oldBasis = newBasis;
	  );
     --print monGens;
     --print SBasis;
     (
	  monGens,
	  select(oldBasis,i->(gDegree i < d-ecart)),
	  d-ecart-1,
	  sbReduce SBasis,
	  hilbertPolynomial( ideal toList apply(monGens,g->g#0), Projective=>false)
	  )
     );

homogenizedLCMdegree = (a,b) -> (
     alist := ((listForm(a#0))#0#0);
     blist := ((listForm(b#0))#0#0);
     lcmlist := apply(alist,blist, (i,j)->max(i,j));
     tdegree := max(a#1 - sum alist, b#1 - sum blist);
     sum lcmlist + tdegree
     );

newMonomialGens = (oldGens, newBasis, dmons, d) -> (
     mons := first entries sort(matrix{flatten dmons}, MonomialOrder=>Descending);
     newBasis = first entries sort(matrix{newBasis/gLeadMonomial}, MonomialOrder=>Descending);
     --print mons;
     --print newBasis;
     newGens := {};
     i := 0;
     for m in mons do (
	  while i < #newBasis and m < newBasis#i do i = i+1;
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
     (coefficients(p, Monomials => flatten dmons))#1
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
rowReduce = method(TypicalValue => Matrix)
rowReduce (Matrix, RR) := (M, tol) -> (
     --print M;
     R := ring M;
     (m,n) := (numrows M, numcols M);
     rindex := m-1;
     M = new MutableMatrix from M;
     for k from 1 to n do (
    	  --if tol > 0 then M = new MutableMatrix from clean(epsilon,new Matrix from M);
    	  (a,aval) := (-1, 0);
    	  for l from 0 to rindex do (
	       --if abs M_(l,n-k) <= tol then M_(l,n-k) = 0_R;
      	       if abs M_(l,n-k) > max(tol,aval) then (a,aval) = (l, abs M_(l,n-k));
	       );
    	  if a == -1 then continue;
    	  rowSwap(M,a,rindex);
	  c := M_(rindex,n-k);
	  --print c;
    	  for i from 0 to n-1 do M_(rindex,i) = M_(rindex,i)/c;
    	  for l from 0 to m-1 do
      	       if l != rindex then (d := M_(l,n-k); for i from 0 to n-1 do M_(l,i) = M_(l,i)-d*M_(rindex,i));
    	  rindex = rindex-1;
    	  --print (n-k,rindex,a,aval);
	  --print new Matrix from M;
  	  );
     if tol > 0 then clean(tol,new Matrix from M) else new Matrix from M
     );

--remove non-minimal standard basis elements
sbReduce = L -> (
     L = L / first;
     Lgood := select(0..#L-1, i->(
     	  all(#L, j->(j == i or not isDivisible(L#i,L#j)))
	  ));
     new List from apply(Lgood, i->L#i)
     );

--evaluation of a dual element v on a polynomial w
innerProduct = (v,w) -> (
     c := entries (coefficients matrix{{v,w}})#1;
     sum(#c,i->(c#i#0)*(c#i#1))
     );

adjointMatrix = M -> (
     M = sub(M, CC);
     M' := mutableMatrix transpose M;
     for i from 0 to (numrows M')-1 do (
     	  for j from 0 to (numcols M')-1 do M'_(i,j) = conjugate(M'_(i,j));
	  );
     matrix M'
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


restart
loadPackage "NumericalHilbert"
R = CC[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
R = QQ[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
R = (ZZ/101)[x,y, MonomialOrder => {Weights=>{-1,-1}}, Global => false]
M = matrix {{x^2-x*y^2,x^3}}
M = matrix {{x*y}}
M = matrix {{x^9 - y}}
dualInfo(M,Truncate=>8)
standardBasis(M)
dualHilbert(M,Truncate=>25)
dualBasis(M)
dualInfo(M)
dualInfo(M, Strategy=>DZ)
dualInfo(M,Point=>{0.01,0.01})

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
