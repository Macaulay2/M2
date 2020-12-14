newPackage(
	"RelativeCanonicalResolution",
Version => "1.0",
Date => "June 16, 2020",
Authors => {{Name => "Christian Bopp",
		   Email => "bopp@math.uni-sb.de",
		   HomePage => "http://www.math.uni-sb.de/ag-schreyer/index.php/people/researchers/75-christian-bopp"},
		   {Name => "Michael Hoff",
		   Email => "hahn@math.uni-sb.de",
		   HomePage => "http://www.math.uni-sb.de/ag-schreyer/index.php/people/researchers/74-michael-hahn"}
						},
Headline=> "the relative canonical resolution for g-nodal canonical curves with a fixed g^1_k",
Keywords => {"Commutative Algebra"}
	)
   
export{"getFactors", 
       "getCoordinates", 
       "canonicalMultipliers", 
       "rkSyzModules", 
--     getGlobalSections,
       "lineBundleFromPointsAndMultipliers", 
       "H0KrD",
       "h0ab",
       "balancedPartition", 
       "canCurveWithFixedScroll",
       "curveOnScroll",
       "resCurveOnScroll",
       "eagonNorthcottType",
--     liftMonomToENT,
--     liftPolyToENT,
       "liftMatrixToEN",
       "getScrollDegrees",
       "getCoxDegrees",
       "bettiENtype",
       "iteratedCone"}

       
       
---------------------------------------------------------------
---------------------------------------------------------------
-- Part 1
-- some usefull functions which will be needed to construct
-- the canonical model, as well as the model on the scroll
---------------------------------------------------------------
---------------------------------------------------------------


-- Computation of the g-nodal k-gonal canonical curve such that 
-- we know the 2x(deg scroll) matrix that defines the scroll, 
-- as described in Christian Bopp's masters thesis:


-- computes list L1 of linearfactors of a polynomial 
-- and list L2 of quadric factors of a polynomial and gives back L1,L2

getFactors = method()
getFactors (RingElement) := (f) -> (
     S := ring f;
     L1 := {};
     L2 := {};
     facs := apply(toList(factor f), fac -> fac#0);
     if (f == 0_S) then (L1,L2) = ({},{}) else      
     (L1,L2) = (select(facs,fac -> degree fac == {1}), select(facs, fac -> degree fac == {2}) );
     --List of linear and quadratic factors
     L1,L2
     );

undocumented { getFactors, (getFactors,RingElement) } 
 
 
 -- computes the vanishing loci of a linear polynomial in two variables
 
getCoordinates = method()
getCoordinates (RingElement) := (l) -> ( 
     S := ring l;
     if rank source (coefficients(l))#0 == 2 then
     matrix{{-(coefficients l)_1_(1,0), (coefficients l)_1_(0,0)}}
     else if sub((coefficients(l))#0,S) - map(S^1,S^1,(entries vars S)_0#0) == 0 then matrix{{0,1}}
     else if sub((coefficients(l))#0,S) - map(S^1,S^1,(entries vars S)_0#1) == 0 then matrix {{1,0}}	  
     ); 
 
undocumented { getCoordinates, (getCoordinates,RingElement) } 


-- computes the canonical multipliers
-- this function is identical to one from the M2 package "NodalCurves.m2"

canonicalMultipliers = method()
canonicalMultipliers (Matrix,Matrix) := (P,Q) -> (
     -- P and  Q two 2xg matrices representing 2g distinct points of P^1
     S := ring P;
     if dim S != 2 then error "not a Polynomialring in two variables";
     g := numcols P; 
     quadrics := apply(g, i -> (det(P_{i}|(transpose vars S)))*(det(Q_{i}|(transpose vars S))));
     sections := (apply(g, i -> product(g, j -> if i == j then 1_S else quadrics_j)));
     -- a basis of the canonical series as subseries of S_{2g-2} of the nodal curve
     A := transpose matrix(apply(g, i -> {sub(sections_i, transpose P_{i}), sub(sections_i, transpose Q_{i})}));
     A);  
 
 
 -- rank of the i-th syzygy module on the scroll 
 
rkSyzModules = method() 
rkSyzModules (ZZ,ZZ) := (pos,k) -> (
    if (pos == 0 or pos == k-2) then 1  
    else(
    pos*(k-2-pos)/(k-1)*binomial(k,pos+1)));



-- computes the global sections of a linebundle of degree d 
-- from the normalized multipliers (i.e. multipliers of the form (a_i,1)) 
-- and points

lineBundleFromPointsAndMultipliers = method()
lineBundleFromPointsAndMultipliers (List,Matrix,Matrix,ZZ) := (ms,P,Q,k) -> (
     S := ring P;
     B := flatten entries basis(k,S);
     M := matrix apply(#ms,i -> 
	        apply(k+1,j -> sub(B_j,transpose(P_{i}))-ms_i*sub(B_j,transpose(Q_{i}))));
     basis(k,S)*syz M
     );

     
-- computes H^0(OO_C,K-rD)
-- takes list PQ of points P and Q since M2 can't handle more 
-- than 4 arguments in a function

H0KrD = method()
H0KrD (Sequence,List,ZZ,ZZ) := (PQ,multL,r,k) -> (
     (P,Q) := (PQ_0,PQ_1);
     g := numcols P;
     kk := coefficientRing (ring P);
     multK0 := sub(canonicalMultipliers(P,Q),kk);
     multK := apply(g,i->multK0_(0,i)/multK0_(1,i));
     multKrD := apply(g,i->multK_i/(multL_i^r));
     lineBundleFromPointsAndMultipliers(multKrD,P,Q,2*g-2-r*k)
     );
 
undocumented { H0KrD, (H0KrD,Sequence,List,ZZ,ZZ) }

 
-- computes dim H^0(PP(E), aH+bR), where E is of balanced type
-- Input: twists a,b,  genus g, gonality k
-- Output: dim H^0(PP(E), aH+bR)

h0ab = method()
h0ab (ZZ,ZZ,ZZ,ZZ) := (a,b,g,k) -> (
    if (a < 0) then error("negative twist of H");
    e := balancedPartition(k-1,g-k+1);
    if(a*min(e)+b+1 < 0) then error("b too negative");
    sum(e)*binomial(a+k-2,k-1)+(b+1)*binomial(a+k-2,k-2)
    ); 

undocumented { h0ab, (h0ab,ZZ,ZZ,ZZ,ZZ) }
 
 
-- compute a balanced partition (usual n=dim Scroll and m=deg Scroll)

balancedPartition = method()
balancedPartition (ZZ,ZZ) := (n,m) -> ( -- m items list of lenth n
      k := m%n;
      r := m//n;
      apply(k,i -> r+1)|apply(n-k,i -> r));
 
 
---------------------------------------------------------------
---------------------------------------------------------------
-- Part 2
-- construction of g-nodal k-gonal canonical curves as well
-- as the relative canonical resolution of such curves
---------------------------------------------------------------
---------------------------------------------------------------
  
  
 -- computes a g-nodal k-gonal canonical curve canonical curve such that 
 -- the curve automatically lies on a normalized scroll of balanced type   
 -- Input: genus g, gonality k and integer n determing the characteristic p=nexPrime(n)
 -- Output: canonical curve which lies on a normalized scroll of balanced type 
 
  canCurveWithFixedScroll = method()
  canCurveWithFixedScroll (ZZ,ZZ,ZZ) := (g,k,n) -> (
      p := nextPrime(n);
      kk := ZZ/p;
      x := getSymbol "x";
      S := kk[x_0,x_1];
      (L,L1) := ({},{});
      PHI := matrix map(S^1,S^1,0_S);
      while (#flatten entries PHI < g) do (
      pts := random(apply(p,i -> matrix{{i,1}})|{matrix{{1,0}}});
     while (#L < g or #(unique flatten L) < #(flatten L)) do (  
             (L,L1) = ({},{}); 
	     f := random(S^1, S^{2:-k});
	     j := 0;
	  while (#L < g and j < p)  do ( if #(L1 = (getFactors(sub(det(f||sub(pts_j,S)),S)))#0) >= 2 then L= L|{L1};	    
			           j=j+1;  
				   );
			                              );
     	  P := transpose matrix apply(L,l -> (flatten entries getCoordinates l_0));
          Q := transpose matrix apply(L,l -> (flatten entries getCoordinates l_1));	
	  -- multiplieres of the degree k line bundle
	  multL := apply(g,i -> sub(sub(f_(0,0),transpose(P_{i})),kk)/sub(sub(f_(0,0),transpose(Q_{i})),kk));
	  -- we compute the scroll:n  
	  PQ := (P,Q);
          j = 0;
          while rank source H0KrD(PQ,multL,j,k) > 0 do(j = j+1);
          eDual := apply(j,i -> rank source H0KrD(PQ,multL,i,k) - rank source H0KrD(PQ,multL,i+1,k));
          e := apply(k-1,i -> #select(eDual,e0->e0 >= i+1)-1);--the type of the scroll  
	  if (max(e)-min(e)>1) then error("scroll not balanced");
	  --
	  PHI0 := apply(e,n -> (
          H0KnD1 := H0KrD(PQ,multL,n,k);
          phi := H0KnD1*random(kk^(rank source H0KnD1),kk^1);
              s := apply(n+1,i -> (
                    (phi*f_(0,0)^(n-i)*f_(0,1)^i))_(0,0))));
          PHI = matrix{flatten PHI0};      );
	  -- The ideal of C in PP^{g-1}  
	  t := getSymbol "t";   
          T := kk[t_0..t_(g-1)];
          phi := map(S,T,PHI);
          Ican := saturate ker phi;
	  Ican ); 

	  
-- given a k gonal canonical curve C which lies on a normalized scroll of balanced type, 
-- we compute the ideal of IC in the scroll
-- Input: Ican in PP^{g-1}, genus g of Ican, gonality k of Ican
-- Output: Ican in X

  curveOnScroll = method()
  curveOnScroll (Ideal,ZZ,ZZ) := (Ican,g,k) -> (
      T := ring Ican;
      kk := coefficientRing T;
      e := balancedPartition(k-1,g-k+1);
      degs := {apply(0..#e-1, i -> {1,max e -e_i}), 2:{0,1}};
      pp := getSymbol "pp";
      v := getSymbol "v";
      w := getSymbol "w";
      Rcox := kk[pp_0..pp_(k-2),v,w,Degrees=>degs];
      PSI := matrix{flatten apply(#e,i->flatten entries (basis({0,e_i},Rcox)*(vars Rcox)_(0,i)))};
      psi := map(Rcox,T,PSI);
      Scroll := ideal mingens ker psi;  
      if (Ican+Scroll != Ican) then error("curve does not lie on the normalized scroll");
      T' := T/Scroll;
      psi' := map(Rcox,T',PSI);
      J := psi'(ideal mingens sub(Ican,T'));
      J1 := saturate(J,ideal basis({0,1},Rcox)); --ideal of Ican in X
      if (k == 3) then ( Jcan := J1) else(
      Jcan = ideal select(flatten entries gens J1,f -> (degree f)_0 == 2);--picking correct degrees
      Jcan ));
 
 
-- computes the resolution on the scroll by picking at eah step syzygies in correct twists of H
-- Input: ideal of canonical curve on normalized scroll and the gonality k
-- Output: the resolution on the scroll


resCurveOnScroll = method()
resCurveOnScroll (Ideal,ZZ,ZZ) := (Jcan,g,lengthRes) -> (
      Rcox := ring Jcan;
      part1 := rank source basis({1,0},Rcox);
      part2 := rank source basis({1,1},Rcox)-2*part1;
      k := part1+part2+1;
      f := g-k+1;
    -- computation of the degreeLimit
      e := balancedPartition(k-1,g-k+1);
      degLimit := toList(getCoxDegrees({k,f-2},e));  
      degsH := toList(3..(k-2))|{k};--twists of H
    resX := {gens Jcan};
    scan(lengthRes, i -> (
                    M0 := syz(resX_i,DegreeLimit => degLimit);
                    cols := toList(0..rank source M0-1);
                    M := M0_(select(cols,j -> ((degrees source M0)_j)_0 == degsH_i));
		    if (rank source M < rkSyzModules(2+i,k) ) then error("DegreeLimit too low");
                    resX = resX|{M}));
    chainComplex resX        
     );   
    
resCurveOnScroll (Ideal,ZZ) := (Jcan,g) -> ( 
    Rcox := ring Jcan;
    part1 := rank source basis({1,0},Rcox);
    part2 := rank source basis({1,1},Rcox)-2*part1;
    k := part1+part2+1;
    f := g-k+1;
  -- computation of the degreeLimit
    e := balancedPartition(k-1,g-k+1);
    degLimit := toList(getCoxDegrees({k,f-2},e));  
    degsH := toList(3..(k-2))|{k}; -- twists of H
    lengthRes := floor((k-4)/2); -- half of the resolution
    resX := {gens Jcan};
    scan(lengthRes, i -> (
                    M0 := syz(resX_i,DegreeLimit => degLimit);
                    cols := toList(0..rank source M0-1);
                    M := M0_(select(cols, j -> ((degrees source M0)_j)_0 == degsH_i));
		    if (rank source M < rkSyzModules(2+i,k) ) then error("DegreeLimit too low");
                    resX = resX|{M}));
    chainComplex resX        
     );

---------------------------------------------------------------
---------------------------------------------------------------
-- Part 3
-- Eagon-Northcott type complexes and iterated mapping cone
---------------------------------------------------------------
---------------------------------------------------------------

-- Given the degree (a_0,a_1) in the Cox ring, 
-- we compute the degree on the scroll.

getScrollDegrees = method()
getScrollDegrees (List,List) := (a,e) -> (a_0,a_1-e_0*a_0) 


-- Given the degree (a,b) on the scroll, 
-- we compute the degrees (a',b') in the Cox ring, such that HH^0(aH+bR) correspons to basis({a',b'},S).

getCoxDegrees = method()
getCoxDegrees (List,List) := (a,e) -> (a_0,a_0*e_0+a_1)


-- computes the bettinumbers and twists of anEagon-Northcott type complex

bettiENtype = method()
bettiENtype (ZZ,ZZ,ZZ,ZZ) := (b,a,f,mult) -> ( 
     -- EN-type res of OO_PP(E)(a*H+b*R)^mult, were PP(E) is a scroll of degree f
     L := {};
     for j from 0 to b do L = L|{(mult*binomial(f,j)*(b-j+1),a-j)};
     for j from b+1 to f do L = L|{(mult*binomial(f,j+1)*(j-b),a-j-1)};
     L
     ) 
undocumented { bettiENtype, (bettiENtype,ZZ,ZZ,ZZ,ZZ) }


-- Computes the Eagon-Northcott type resolution. 
-- The function is based on the Eagon-Northcott function by Greg Smith.
-- Input: twist b of the Ruling and matrix Phi defining the scroll, thus defining the differentials
-- Output: a chaincomplex, the Eagon-Northcott type complex

eagonNorthcottType = method()
eagonNorthcottType (Matrix,ZZ) := (Phi,b) -> (
     R := ring Phi;
     f := rank source Phi;
     g := rank target Phi;
     B0 := apply(toList(0..b), i -> {i, flatten table(subsets(f,i), compositions(g,b-i), (p,q) -> {p,q})});
     B1 := apply(toList(b+1..f-g+2), i -> {i, flatten table(subsets(f,g+i-1), compositions(g,i-1-b), (p,q) -> {p,q})});
     B := hashTable(B0|B1);
     d := {};
     scan(1..b, i -> (
	       d0 := matrix_R table(B#(i-1),B#i,
		    (p,q) -> if not isSubset(p#0,q#0) then 0_R
		            else (
			    vec := p#1 - q#1;
			      if any(vec, e -> e < 0 or e > 1) then 0_R 
			      else (
			       s := first select(toList(0..#q#0-1),l -> not member(q#0#l, p#0));
			       t := first select(toList(0..g-1), l -> vec#l == 1);
			       (-1)^(s+1)*Phi_(t,q#0#s))));
		d = d|{d0};));
      d0 := matrix_R table(B#b,B#(b+1),
	   (p,q) -> if not isSubset(p#0,q#0) then 0_R 
	           else (
		    ind := sort toList(set(q#0)-set(p#0));
		    s := select(toList(0..#q#0-1),l -> not member(q#0#l, p#0));
     	            (-1)^(sum s-1)*det(Phi_(ind))));
      d = d|{d0};	
      scan(b+2..f-g+2, i -> (
		d0 := matrix_R table(B#(i-1),B#i,
		     (p,q) -> if not isSubset(p#0,q#0) then 0_R
		             else (
			     vec := q#1 - p#1;
			       if any(vec, e -> e < 0 or e > 1) then 0_R 
			       else (
			        s := first select(toList(0..#q#0-1),l -> not member(q#0#l, p#0));
			        t := first select(toList(0..g-1), l -> vec#l == 1);
			        (-1)^(s+1)*Phi_(t,q#0#s))));
		d = d|{d0};));  
     chainComplex d)


-- lifts a monomial to the Eagon-Nortcott-type resolution
-- Input: A=1*1 matrix whose entry is a monomial representing multiplication map between two line bundles on scroll,  
--  e=partition of the scroll.
-- Output: induced map between the first modules in the Eagon-Northcott type resolution of these line bundles.

liftMonomToENT = method()
liftMonomToENT (Matrix,List) := (A,e) -> (	 
     Rcox := ring A;
     kk := coefficientRing Rcox;
     z := getSymbol"z";
     T2 := kk[flatten apply(#e, j -> apply(e_j+1, i -> z_(j,i)))];
     Phi0 := {flatten apply(#e, j -> toList(z_(j,0)..z_(j,e_j-1))),
     	    flatten apply(#e, j -> toList(z_(j,1)..z_(j,e_j)))};
     Phi := matrix apply(#Phi0, i -> 
	          apply(#(Phi0_i), j -> Phi0_i_j_T2));	
     Scroll := minors(2,Phi);
     s := (matrix basis({0,1},Rcox))_(0,0);
     t := (matrix basis({0,1},Rcox))_(0,1);
     PSI := matrix {flatten apply(#e, i-> flatten entries (basis({0,e_i},Rcox)*(vars Rcox)_(0,i)))}; 
     psi := map(Rcox,T2,PSI);
     degsA := apply(flatten degrees A, d -> -d);
     degtarget := getScrollDegrees(degsA_0,e);
     degsource := getScrollDegrees(degsA_1,e);
     comp1 := compositions(2,degsource_1);
     comp2 := compositions(2,degtarget_1);
     b0 := matrix{apply(#comp1, i -> (-1)^(i+1)*t^(comp1_i_0)*s^(comp1_i_1))};
     b1 := matrix{apply(#comp2,i -> (-1)^(i+1)*t^(comp2_i_0)*s^(comp2_i_1))};
     Amul := A*b0;
     Amul' := contract(transpose b1,Amul);
     Amul' = leadTerm Amul';
     base' := matrix{unique flatten entries((matrix (basis(degtarget_0-degsource_0,T2)))%Scroll)};
     base := psi base';
     M := matrix apply(rank target Amul', i -> 
	           apply(rank source Amul', j -> 
		       (sub(contract(base,Amul'_(i,j)),kk)*transpose base')_(0,0)));
     map(T2^{rank target Amul':degtarget_0}, T2^{rank source Amul':degsource_0},M)
     )

undocumented { liftMonomToENT, (liftMonomToENT,Matrix,List) }


-- lifts a polynomial to the Eagon-Nortcott-type resolution
-- Input: A=1*1 matrix whose entry is a polynomial representing multiplication map between two line bundles on scroll,  
--       e=partition of the scroll.
-- Output: induced map between the first modules in the Eagon-Northcott type resolution of these line bundles.

liftPolyToENT = method()
liftPolyToENT (Matrix,List) := (A,e) -> (
    kk := coefficientRing ring A;
    z := getSymbol"z";
    T2 := kk[flatten apply(#e, j -> apply(e_j+1, i -> z_(j,i)))];
  sum apply(terms(A_(0,0)), a -> sub(liftMonomToENT(map(target A, source A,a),e),T2))
       	    )

undocumented { liftPolyToENT, (liftPolyToENT,Matrix,List) }

-- lifts a polynomial to the Eagon-Nortcott-type resolution
-- Input: A=matrix representing map between two vector bundles on scroll,  
--       e=partition of the scroll.
-- Output: induced map between the first modules in the Eagon-Northcott type resolution of these vector bundles.

liftMatrixToEN = method()
liftMatrixToEN (Matrix,List) := (A,e) -> (
    kk := coefficientRing ring A;
    z := getSymbol"z";
    T2 := kk[flatten apply(#e, j -> apply(e_j+1, i -> z_(j,i)))];
matrix apply(rank target A, i -> 
             apply(rank source A, j -> sub(liftPolyToENT(submatrix(A,{i},{j}),e),T2)))
       	    )  
 
 
-- Input: resX=resolution on the scroll, e=scroll type
-- Output: the iterated mapping cone

iteratedCone = method()
iteratedCone (ChainComplex,List) := (resX,e) -> ( 
   k := length(e)+1;
   g := sum(e)+k-1;
   kk := coefficientRing ring resX_0;
 -- we use T2 here in order to make sorting easier
   z := getSymbol"z";
   T2 := kk[flatten apply(#e,j->apply(e_j+1,i->z_(j,i)))];
   Phi0 := {flatten apply(#e,j->toList(z_(j,0)..z_(j,e_j-1))),
     	    flatten apply(#e,j->toList(z_(j,1)..z_(j,e_j)))};
   Phi := matrix apply(#Phi0,i->
	              apply(#(Phi0_i),j->Phi0_i_j_T2));	
   lresX := length resX;
 -- computation of the ENtype complexes involved in the iterated MC construction
   Clist := {};
   C := apply(lresX+1, i -> (
	        Clist := {};
                degs := degrees resX_i;
                for j from 0 to (rank resX_i -1) do (
                Clist = Clist|{eagonNorthcottType(Phi,-(getScrollDegrees(degs_j,e))_1)};
	                                            ); 
                (directSum Clist)**T2^{1:-(getScrollDegrees(degs_0,e))_0})
	                );                        
 -- the lift of the first horizontal maps
   mapA := apply(lresX, i -> sub(liftMatrixToEN(resX.dd_(i+1),e),T2));		  
 -- building the iterate mapping cone:    
   Cone := C_(lresX);
   for i from 1 to lresX do (
       Cone = cone (extend(C_(lresX-i),Cone,mapA_(lresX-i)) );
                            );
   t := getSymbol"t";			
   T := kk[t_0..t_(g-1)];
   Bchange := map(T,T2,vars T);
   Bchange(Cone)
);
 
----------------------------------------------------------------------

beginDocumentation()

document { 
  Key => RelativeCanonicalResolution,
  Headline => "construction of relative canonical resolutions and Eagon-Northcott type complexes",
  "This package provides functions that construct g-nodal canonical curves with a degree k line bundle, which lie on a normalized scroll.
   It furthermore contains functions that compute the so-called relative canonical resolution. 
   The construction of such canonical curves is based on the Macaulay2 package",  
   HREF("https://www.math.uni-sb.de/ag/schreyer/index.php/computeralgebra"," kGonalNodalCurves. "), 
   "This package can be seen as an upgrade to the ", 
   HREF("https://www.math.uni-sb.de/ag/schreyer/index.php/computeralgebra"," kGonalNodalCurves ")," package.",
   PARA{},
   "We also provide functions to compute (possibly non-minimal) free resolutions of such curves by an iterated 
   mapping cone construction, as described in Schreyer's article", 
   HREF("http://link.springer.com/article/10.1007%2FBF01458587?LI=true"," Syzygies of Canonical Curves and Special Linear Series"),". ",  	 
   PARA{},
   SUBSECTION "Construction of relative canonical resolutions",  
   UL{   TO canCurveWithFixedScroll,
	 TO curveOnScroll,
	 TO resCurveOnScroll
      },
   PARA{},
   SUBSECTION "Iterated mapping cones and Eagon-Nortcott type complexes",  
   UL{   TO eagonNorthcottType,
	 TO liftMatrixToEN,
	 TO iteratedCone
      },  
  Caveat => {"This package requires Macaulay2 Version 1.11 or newer."} 
   }


doc ///
  Key 
    canCurveWithFixedScroll
    (canCurveWithFixedScroll,ZZ,ZZ,ZZ) 
  Headline 
    Computes a g-nodal canonical curve with a degree k line bundle on a normalized scroll
  Usage
    Ican=canCurveWithFixedScroll(g,k,n)
  Inputs
    g: ZZ
       the genus of the curve
    k: ZZ
       the degree of the line bundle on C
    n: ZZ
       integer defining the characteristic p ($\ge n$) of the ground field        
  Outputs
    ICan: Ideal
          ideal of the canonical curve        
  Description
     Text
       Computes the ideal of a g-nodal canonical curve with a degree k<g line bundle,
        which lies on a normalized scroll. The construction of such curves is based on the 
	Macaulay2 package  @HREF("http://www.math.uni-sb.de/ag/schreyer/index.php/people/researchers/75-christian-bopp","kGonalNodalCurves")@

     Example
         (g,k,n) = (8,5,1000);
         Ican = canCurveWithFixedScroll(g,k,n);
	 genus Ican, degree Ican, dim Ican
	 betti res(Ican, DegreeLimit => 1)
	 Phi = matrix{{t_0,t_2,t_4,t_6},{t_1,t_3,t_5,t_7}}
	 Iscroll = minors(2,Phi);
	 Ican + Iscroll == Ican         
  SeeAlso
    curveOnScroll       
///

doc ///
  Key 
    curveOnScroll
    (curveOnScroll,Ideal,ZZ,ZZ) 
  Headline 
    Computes the ideal of a canonical curve on a normalized scroll in terms of generators of the scroll
  Usage
    J=curveOnScroll(Ican,g,k)
  Inputs
    Ican: Ideal
          ideal of a genus g canonical curve with a degree k line bundle on a normalized scroll
    g: ZZ
       the genus
    k: ZZ
       the degree of the line bundle on C       
  Outputs
    Jcan: Ideal
          ideal of the canonical curve in terms of generators of the scroll       
  Description
     Text
       Given the ideal of a canonical curve on a normalized scroll, the function computes the ideal
       of the curve in terms of generators on the scroll.
     Example
        (g,k,n) = (8,5,1000);
	Ican = canCurveWithFixedScroll(g,k,n);
	Jcan = curveOnScroll(Ican,g,k);
	betti Jcan             
  SeeAlso
    canCurveWithFixedScroll
    resCurveOnScroll      
///

doc ///
  Key 
    resCurveOnScroll
--    (resCurveOnScroll,Ideal,ZZ,ZZ,ZZ)
    (resCurveOnScroll, Ideal,ZZ,ZZ)
    (resCurveOnScroll,Ideal,ZZ) 
  Headline 
    Computes the relative canonical resolution
  Usage
--    resX=resCurveOnScroll(Jcan,g,l,d)
    resX=resCurveOnScroll(Jcan,g,l)
    resX=resCurveOnScroll(Jcan,g)
  Inputs
    Jcan: Ideal
          ideal of a genus g curve with a degree k line bundle in terms of generators on the scroll
    g: ZZ
       the genus of C
    l: ZZ
       the length limit of the resolution  
    d: ZZ
       defining a degree limit        
  Outputs
    resX: ChainComplex
          the relative canonical resolution       
  Description
     Text
       Computes the relative canonical resolution by picking syzygies of correct degree in each step 
       until the length limit is reached.  
           
     Example
        (g,k,n) = (8,5,1000);
	Ican = canCurveWithFixedScroll(g,k,n);
	Jcan = curveOnScroll(Ican,g,k);
	resX = resCurveOnScroll(Jcan,g);
	betti resX
  SeeAlso
    curveOnScroll       
///

doc ///
  Key 
    rkSyzModules
    (rkSyzModules,ZZ,ZZ) 
  Headline 
    Computes the rank of the i-th module in the relative canonical resolution
  Usage
    rk=rkSyzModules(i,k)
  Inputs
    i: ZZ
       the position
    k: ZZ
       the degree of the line bundle on C        
  Outputs
    rk: ZZ
          the rank of the i-th module in the relative canonical resolution      
  Description
     Text
       Computes the rank of the i-th module in the relative canonical resolution 
       
     Example
        k = 5;
        rks = apply(k-1, i -> rkSyzModules(i,k))            
       
///

doc ///
  Key 
    eagonNorthcottType
    (eagonNorthcottType,Matrix,ZZ) 
  Headline 
    Computes the Eagon-Northcott type resolution
  Usage
    resX=resCurveOnScroll(Phi,b)
  Inputs
    Phi: Matrix
          matrix whose maximal minors define the scroll
    b: ZZ
       the twist     
  Outputs
    ENresolution: ChainComplex
                  the Eagon-Northcott type resolution     
  Description
     Text
       Computes the Eagon-Northcott type resolution associated to the matrix Phi defining the scroll
       and an integer b defining the twist $O_{P(E)}(bR)$. The way the function works is similar to 
       the way the "Eagon-Northcott" function by Greg Smith works.  
           
     Example
	R = ZZ/12347[x_0..x_7]
        Phi = matrix{{x_0..x_3},{x_4..x_7}}
        betti(eagonNorthcottType(Phi,0))
        betti(eagonNorthcottType(Phi,1))
  SeeAlso
    liftMatrixToEN
    iteratedCone       
///

doc ///
  Key 
    liftMatrixToEN
    (liftMatrixToEN,Matrix,List) 
  Headline 
    Lifts a matrix between bundles on the scroll to the associated Eagon-Northcott type complexes
  Usage
    A=liftMatrixToEN(Psi,e)
  Inputs
    Psi: Matrix
          matrix that defines map between vector bundles on the scroll.
    e: List
       the type of the scroll $P(E)$   
  Outputs
    A: Matrix
       the induced map     
  Description
     Text
       Given a map between vector bundles F and G on a normalized scroll of type e, the function computes
       the induced map between the first modules in the Eagon-Northcott type resolution of F and G.  
           
     Example
	(g,k,n) = (8,5,1000)
	e = balancedPartition(k-1,g-k+1)
	Ican = canCurveWithFixedScroll(g,k,n);
	Jcan = curveOnScroll(Ican,g,k);
	betti(resX = resCurveOnScroll(Jcan,g,2))
	betti(liftMatrixToEN(resX.dd_1,e))
	betti(liftMatrixToEN(resX.dd_2,e))
	betti(liftMatrixToEN(resX.dd_3,e))
  SeeAlso
    eagonNorthcottType
    iteratedCone     
///

doc ///
  Key 
    iteratedCone
    (iteratedCone,ChainComplex,List) 
  Headline 
    Computes a (possibly non-minimal) resolution of C in P^{g-1} starting from the relative canonical resolution of C in P(E)
  Usage
    resC=iteratedCone(resX,e)
  Inputs
    resX: ChainComplex
          the relative canonical resolution
    e: List
       the type of the scroll $P(E)$   
  Outputs
    resC: ChainComplex
       	  the resolution of C obtained by an iterated mapping cone     
  Description
     Text
       Given the relative canonical resolution of C on a normalized scroll $P(E)$, the function computes
       a (possibly non-minimal) free resolution of C in $P^{g-1}$ by an iterated mapping cone construction.
       For gonality k=3,4 the iterated mapping cone is always minimal. In these cases "iteratedCone" is much 
       faster (for $g >9$) than computing the resolution via the @TO res@ command. 
           
     Example
	(g,k,n) = (8,5,1000)
	e = balancedPartition(k-1,g-k+1)
	Ican = canCurveWithFixedScroll(g,k,n);
	betti res(Ican,DegreeLimit=>1)
	Jcan = curveOnScroll(Ican,g,k);
	betti(resX = resCurveOnScroll(Jcan,g,2))
	betti(resC = iteratedCone(resX,e))
  SeeAlso
    eagonNorthcottType
    liftMatrixToEN      
///

doc ///
  Key 
    balancedPartition
    (balancedPartition,ZZ,ZZ) 
  Headline 
    Computes balanced partition of n of length d
  Usage
    e=balancedPartition(d,n)
  Inputs
    d: ZZ
       length of the partition
    n: ZZ
       the natural number we want to partition    
  Outputs
    e: List
       the balanced partition     
  Description
     Text
       Computes a balanced partition of an integer n of length d  
           
     Example
	(d,n) = (4,7);
	e = balancedPartition(d,n)
       
///

doc ///
  Key  
    lineBundleFromPointsAndMultipliers
    (lineBundleFromPointsAndMultipliers, List, Matrix, Matrix, ZZ)
  Headline 
    Computes basis of a line bundle from the 2g points P_i, Q_i and the multipliers 
  Usage
    lineBundleFromPointsAndMultipliers(multL,P,Q,k)
  Inputs
       multL: List
          containing the normalized multipliers a_i (normalized means that b_i=1)
       P: Matrix
          coordinate matrix of g points in P^1
       Q: Matrix
          coordinate matrix of g points in P^1
       k: ZZ
          defining the degree of the line bundle	  	  
  Outputs
       f : Matrix
	  a basis of sections of the line bundle defined by the points 
	  P_i, Q_i and the multipliers          
  Description
     Text
       If C is a g-nodal canonical curve with normalization 
       $\nu:\  P^1 \to P^{g-1}$ then a line bundle L of degree k on C is given by
       $\nu^*(O_{P^1}(k))\cong L$ and gluing data 
       $\frac{b_j}{a_j}:O_{P^1}\otimes kk(P_j)\to O_{P^1}\otimes kk(Q_j)$.
       Given 2g points P_i, Q_i and the multipliers (a_i,b_i) we can compute a basis
       of sections of L as a kernel of the matrix $A=(A)_{ij}$ with
       $A_{ij}=b_iB_j(P_i)-a_iB_j(Q_i)$ where 
       $B_j:P^1\to kk,\  (p_0:p_1)\to p_0^{k-j}p_1^j$.

  SeeAlso
    canonicalMultipliers
    	      
///

doc ///
  Key  
    canonicalMultipliers
    (canonicalMultipliers,  Matrix, Matrix)
  Headline 
    Computes the canonical multipliers of a rational curves with nodes 
  Usage
    canonicalMultipliers(P,Q)
  Inputs
       P: Matrix
          coordinate matrix of g points in P^1
       Q: Matrix
          coordinate matrix of g points in P^1
  Outputs
       A: Matrix
	  A of multipliers          
  Description
     Text
       This function is identical to the function "canonicalMultipliers" from
       the Macaulay2 package  @ HREF("http://www.math.uni-sb.de/ag/schreyer/home/NodalCurves.m2","NodalCurves") @. 
       Given g pairs of points P_i, Q_i, on P^1 
       computes the canonical series of the corresponding nodal curve of 
       genus g and determines the g ratios A_i of the glueing data for the canonical bundle
       (note that A_i depends on the choice of the
       homogeneous coordinates of the point P_i and  Q_i).
  
       Step 1.We compute g quadrics q_i 
       $q_i\ :=det(P_i\ |\ (x_0,x_1)^t)\ *\ det(Q_i\ |\ (x_0,x_1)^t)$ and
       a basis of $H^0(C,\omega_C)$ by
       $\{s_i\ :=\prod^g_{j\neq i,j=1}q_i\ |\ i=1,...,g \}$.
       
       Step 2. We compute the multipliers
       $a_i=s_i(P_i)$ and $b_i=s_i(Q_i)$
///

doc ///
  Key  
    getCoxDegrees
    (getCoxDegrees,  List, List)
  Headline 
    Computes the degree of a polynomial in the Cox ring corresponding to a section of a bundle on the scroll 
  Usage
    getCoxDegrees(ab,e)
  Inputs
       ab: List
           containing the twist $a$ of the hyperplane class and $b$ of the ruling
       e: List
          the type of the scroll
  Outputs
       ab': List
	    containg the bidegree in the coxring         
  Description
     Text
       Computes integers a' and b' such that $H^0(O_{P(E)}(aH+bR))$ corresponds to basis($\{a',b'\}$,S), where S is the Cox ring of the scroll $P(E)$.
  SeeAlso
    getScrollDegrees   

///

doc ///
  Key  
    getScrollDegrees
    (getScrollDegrees,  List, List)
  Headline 
    Computes the degree of a section of a bundle on the scroll ring corresponding to a polynomial in the Cox ring 
  Usage
    getScrollDegrees(ab',e)
  Inputs
       ab': List
           containing the bidegree of polynomial in the Cox ring
       e: List
          the type of the scroll
  Outputs
       ab: List
	   containing the bidegree on the scroll        
  Description
     Text
       Computes integers a and b such that $H^0(O_{P(E)}(aH+bR))$ corresponds to basis($\{a',b'\}$,Rcox), where Rcox is the Cox ring of the scroll $P(E)$.
  SeeAlso
    getCoxDegrees  
///


TEST ///
(g,k,n) = (8,5,1000);
Ican = canCurveWithFixedScroll(g,k,n);
assert((genus Ican, degree Ican, dim Ican) == (g,2*g-2,2))
betti res(Ican, DegreeLimit => 1)
Phi = matrix{{t_0,t_2,t_4,t_6},{t_1,t_3,t_5,t_7}}
Iscroll = minors(2,Phi);
assert(Ican + Iscroll == Ican)
D = Ican + ideal(Phi^{0});
assert((degree D, dim D) == (k,1))
///

TEST ///
(g,k,n) = (8,5,1000)
e = balancedPartition(k-1,g-k+1)
assert(e=={1,1,1,1})
Ican = canCurveWithFixedScroll(g,k,n);
betti res(Ican,DegreeLimit=>1)
Jcan = curveOnScroll(Ican,g,k);
betti(resX = resCurveOnScroll(Jcan,g,2))
betti(resC = iteratedCone(resX,e))
assert(rank matrix basis(5,resC_4) == 6)
///

TEST ///
R = ZZ/12347[x_0..x_7]
Phi = matrix{{x_0..x_3},{x_4..x_7}}
betti(eagonNorthcottType(Phi,0))
betti(eagonNorthcottType(Phi,1))
///

end;

---------------------------------------------

restart;
uninstallPackage"RelativeCanonicalResolution"
installPackage"RelativeCanonicalResolution"
check "RelativeCanonicalResolution"
