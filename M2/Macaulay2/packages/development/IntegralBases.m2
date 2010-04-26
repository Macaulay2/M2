needsPackage "UPolynomials"
needsPackage "FractionalIdeals"
needsPackage "Puiseux"

newPackage(
        "IntegralBases",
        Version => "0.1", 
        Date => "7 Aug 2009",
        Authors => {{Name => "Mike Stillman", 
                  Email => "mike@math.cornell.edu", 
                  HomePage => "http://www.math.cornell.edu/~mike"}},
        Headline => "integral bases for plane curves, a la van Hoeij, and Trager",
        DebuggingMode => true
        )

needsPackage "UPolynomials"
needsPackage "FractionalIdeals"
needsPackage "Puiseux"
needsPackage "MatrixNormalForms"

export {
     integralBasis,
     vanHoeij,
     Trager,
     principalPart,
     makeEquations,
     findPuiseuxSeries,
     puiseuxTruncations,
     findBranches,
     findTruncations,
     displayTruncations,
     traceRadical,
     trager
     }

--------------------------------------------
-- Local basis, using van Hoeij algorithm --
--------------------------------------------
-- Assumptions and input:
--  1. F(x,y) in R = kk[x,y], where x = R_0 is the indep variable, and y = R_1 is the extension variable
--  2.  F is monic of degree n in y
--  3.  x^2 divides discriminant(F,y), and also F is singular at some point above x=0.
-- Output:
--  V, a list encoding the (local) integral basis
-- Step 1. Puisuex
--   Compute Ps, the list of truncated Puiseux parametrizations, over x=0.
--   These are truncated to the degrees mentioned in Mark vH paper (possibly can do better)
-- Step 2.
--   Initialize V
--   Initialize W, a list of same size as V.
--     W_i is a list of the truncated t-coeffs of V_i
--   loop1:
--     compute truncations of y*V_-1, append to V, W.
--     concatenate W to make a matrix (over kk), find syzygy
--     if no syzygies, loop1
--     if there is a syzygy:
--       mult syz by a row vector coming from elements in V
--       change last entry in V by using this element
--       recompute truncated t-coeffs of this element
--
-- begin
--   V = {{0,1_R}}
--   W = apply(V, v -> computeTruncationVector(Ps, v))
--   nextstep = NextElement -- or ThisElement, or Done
--   while true do (
--     if nextstep === NextElement then (
--       if #V === n then return V;
--       v = y*V_-1;
--       w = computeTruncationVector(Ps, v);
--       z := findSyzygy W;
--       nextstep = if z == 0 then NextElement else ThisElement;
--       )
--     else if done === ThisElement then (
--       -- replace last element of V with v (but with one higher degree x)
--       -- and recompute truncation
--       ));
--   V
-- end.


vecSpaceMap = (K,A) -> (
     -- returns a function: f : A --> K^n, where A is isom to K^n
     -- several cases here.
     if K === A
     then (a) -> {a}
     else (
	  if not isField A then error "expected a field";
	  A1 = coefficientRing A;
	  if coefficientRing A1 === K then (
	       monoms := flatten entries basis A1; -- could give an error
	       << "making map using " << monoms << endl;
	       f := (a) -> (
		    b := coefficient(1_A1, a);
		    flatten entries lift(last coefficients(b, Monomials=>monoms), K));
	       f
	       )
	  else (
	       null
	       )
	  )
     )

tcoeffs = (p,R) -> (
     -- returns a function (deg,v) --> list of t-coeffs of v/(x(t)^deg) evaluated at p
     (xt, yt) := p;
     d := first degree xt;
     K := coefficientRing ring xt;
     A := K(monoid [gens ring xt]); -- a regular poly ring
     t := A_0;
     toA := map(A,ring xt,{t});
     (xt,yt) = (toA xt, toA yt);
     monoms := for i from 0 to d-1 list t^i;
     F := map(A, R, {toA p#0,toA p#1}); -- R should have two variables, first x, then y
     f := vecSpaceMap(coefficientRing R, K);
     --<< " monoms = " << monoms << endl;
     --<< " F      = " << F << endl;
     (deg,v) -> (
	  v1 := (F v) % t^((deg+1)*d);
	  if v1 % xt^deg != 0 then error "warning -- my logic is not right here";
	  v2 := v1 // xt^deg;
	  L := flatten entries sub(last coefficients(v2, Monomials=>monoms), K);
	  flatten(L/f)
     ))
computeTruncation = (Ps,deg, v) -> flatten apply(Ps, p -> p(deg,v))
appendToBasis = (Ps,V,W,deg,v) -> (
     w := computeTruncation(Ps,deg,v);
     (append(V,{deg,v}), append(W,w))
     )
nextElement = (Ps,V,W,y) -> appendToBasis(Ps,V,W,V#-1#0,y * V#-1#1)
thisElement = (Ps,V,W,z) -> appendToBasis(Ps,drop(V,-1), drop(W,-1), V#-1#0 + 1, z)
findSyzygy = (x) -> (V,W) -> (
     z := syz transpose matrix W;
     if z == 0 
       then null 
       else (
	    topdeg := V#-1#0;
	    Vx := matrix{apply(V, p -> x^(topdeg - p#0) * p#1)};
	    (Vx * z)_(0,0)
	    )
     )
localBasis = (Ps, n, x, y) -> (
     R := ring x;
     Ps = for p in Ps list tcoeffs(p,R);
     local z;
     V = {{0, 1_R}};
     W = {computeTruncation(Ps, 0, 1_R)};
     for i from 0 to n-1 do (
	  while (z = (findSyzygy x)(V,W)) =!= null do
	       (V,W) = thisElement(Ps,V,W,z);
	  if i === n-1 then break;
	  (V,W) = nextElement(Ps,V,W,y);
	  );
     V
     )

--------------------------------------------
principalPart = (P, f, truncdegree) -> (
     -- returns the list of coefficients in t of f(x(t),y(t))/fdenom(x(t),y(t)) 
     -- up to, but not including deg_t x(t).
     R := ring f;
     (xt,yt) := P;
     S := ring xt;
     S1 := S/S_0^truncdegree;
     xt = sub(xt,S1);
     yt = sub(yt,S1);
     f' := sub(f, {R_0 => xt, R_1 => yt});
     lift(f',S))

makeEquations = (Ps, base, denomdegree) -> (
     -- First make the matrices to split the principal part into coefficients
     R := ring base_0;
     KK := coefficientRing R;
     Ms := apply(Ps, P -> (
	       d := first degree P_0;
	       d = d * denomdegree;
	       t := (ring P_0)_0;
	       M := matrix {apply(toList(0..d-1), i -> t^i)};
	       (d, M) -- d is the truncation degree, M is t^0..t^(d-1) as a matrix
	       ));
     -- Now loop through each element of base, and create the matrix
     -- whose rows are the coefficients of the principal part of base_i
     -- at each of the parametrizations P.
     transpose matrix apply(base, f -> flatten apply(#Ps, i -> (
		    ft := principalPart(Ps#i,f,Ms#i#0);
		    cfs := (coefficients(ft, Monomials => Ms#i#1))_1;
		    cfs = lift(cfs,KK);
		    flatten entries cfs)))
     )

findPuiseuxSeries = method()
findPuiseuxSeries(RingElement) := (F) -> (
     R := ring F;
     Px := (coefficientRing R)[(gens R)_0];
     ds := disc(F, R_1);
     ds = apply(ds, d -> sub(d#0,Px));
     as := apply(ds, adjoinRoot);
     Rs := apply(as, a -> if ring a === coefficientRing R then R else (ring a)[gens R]);
     Fs := apply(#ds, i -> (S := Rs#i; sub(F, {R_0 => S_0 + as#i, R_1 => S_1})));
     << "warning: still need to set the truncation degrees correctly" << endl;
     hashTable apply(#Fs, i -> (ds#i,as#i) => (
	  P := puiseux(Fs#i,10);
	  join apply(P, (xt,yt) -> (xt+sub(as#i,ring xt),yt))
	  ))
     )

puiseuxTruncations = method()
puiseuxTruncations RingElement := (F) -> (
     -- Compute the Puiseux tree
     -- and the degrees
     -- and then make the Puiseux series to that degree
     -- returns a list of {(x(t),y(t)), Ni, Inti}
     PT := puiseuxTree F;
     L := findVanHoeijWeights PT;
     B := branches F; -- TODO: compute this from PT!! -- but: these are in the same order as L
       -- and should have the same length as L
     if #L =!= #B then error "my logic is somehow wrong";
     truncationDegrees := apply(L, f -> floor(f#"Info"#"Multiplicity" * f.cache#"Ni")+1);
     apply(#B, i -> (
	       b := B#i;
	       f := L#i;
	       {f.cache#"Ni", f.cache#"Int", truncationDegrees#i, series extend(b, truncationDegrees#i)}
	       ))
     )

findBranches = method()
findBranches(RingElement) := (F) -> (
     R := ring F;
     Px := (coefficientRing R)[(gens R)_0];
     ds := disc(F, R_1);
     ds = apply(ds, d -> sub(d#0,Px));
     as := apply(ds, adjoinRoot);
     Rs := apply(as, a -> if ring a === coefficientRing R then R else (ring a)[gens R]);
     Fs := apply(#ds, i -> (S := Rs#i; sub(F, {R_0 => S_0 + as#i, R_1 => S_1})));
     hashTable apply(#Fs, i -> (ds#i,as#i) => (
	  P := branches(Fs#i);
	  << netList P << endl;
	  apply(P, p -> (series p#0, p#1))
	  ))
     )
findBranches(RingElement) := (F) -> (
     R := ring F;
     ds := disc(F, R_1);
     ds = ds/first;
     Fs := apply(ds, d -> center(F,d));
     hashTable apply(#Fs, i -> (ds#i,Fs#i#1) => (
	  P := branches(Fs#i#0);
	  apply(P, p -> (series p#0, p#1))
	  ))
     )

findTruncations = method()
findTruncations(RingElement) := (F) -> (
     R := ring F;
     ds := disc(F, R_1);
     ds = ds/first;
     Fs := apply(ds, d -> center(F,d));
     hashTable apply(#Fs, i -> ds#i => Fs#i#1 => (
	  puiseuxTruncations(Fs#i#0)
	  ))
     )

displayTruncations = method()
displayTruncations HashTable := (H) -> (
     L := apply(pairs H, (k,v) -> (k, (v#0, flatten apply(v#1, x -> drop(x,3)))));
     L = apply(L, (k,v) -> (k, v#0 => netList apply(v#1, x -> (
			 R := ring x#1;
			 K := coefficientRing R;
			 if isPolynomialRing K then K = coefficientRing K;
			 {VerticalList flatten entries gens ideal K, x})
		      )));
--     L := hashTable apply(pairs H, (k,v) -> (k,(v#0 => netList v#1)));
     print netList L;
     )

-- van Hoeij algorithm
-- step 1: computing the factors of the discriminant, roots and Puiseux series
-- step 2: Creation of (and solving) the equations in van Hoiej
-- step 3: van Hoeij

-- Trager algorithm
-- step 1: computing the trace and mult map matrices
-- step 2: Hermite normal form
-- step 3: p-trace radical

needsPackage "TraceForm"

improveDenoms = (Ds, F) -> (
     D := product(Ds, fn -> fn#0 ^(fn#1));
     D = D//F;
     (factor D)//toList/toList
     )

selectdisc = (L) -> L#0#0
selectdisc = (L) -> L/first//product

trager1 = (omega, Ms, dx) -> (
     n := numRows omega;
     KA := ring omega;
     A := KA.baseRings#-1;
     M := dx*id_(A^n) | sub(omega,A);
     << "place a" << endl << flush;
     print toString M;
     H := hermiteNF(M, ChangeMatrix=>false);
     << "place b" << endl << flush;
     H = submatrix(H, 0..n-1);
     H = transpose sub(H,KA);
     -- columns of dx * H^-1 are the A-generators of rad(dx)
     -- 1/dx * H is the change of basis matrix from vs to gens or rad(dx).
     -- Now compute Hom(rad(dx),rad(dx)).
     Hinv := dx * H^-1; -- columns are the A1-basis of the radical of dx.
     -- First, compute the mult maps for each A1-gen of rad(dx).
     Msnewbasis := apply(entries transpose sub(Hinv,A), rs -> (
	  sum apply(#rs, i -> rs#i * Ms#i)
	  ));
     << "place c" << endl << flush;
     MM := matrix{apply(Msnewbasis, m -> transpose(1/dx * H * m))};
     --MM = lift(MM,A); -- for some reason thi sfails now!
     MM = sub(MM,A);
     << "place d" << endl << flush;     
     L := hermiteNF(MM, ChangeMatrix => false);
     << "place e" << endl << flush;     
     L = transpose submatrix(L, 0..n-1);
     (L, (sub(L,KA))^-1)
     --(H, Hinv, Msnewbasis, MM, L)
     )

trager = method(Options => {Verbosity => 0})
trager Ring := opts -> (R) -> (
     -- R should be of the form: K[ys]/I, where
     -- KK = frac(kk[x]).
     (vs, omega, Ms) := traceFormAll R;
     n := #vs;
     KA := coefficientRing R;
     A := KA.baseRings#-1;
     D := sub(det omega, A);
     Ds := (factor D)//toList/toList/(x -> if x#1 >= 2 then {x#0,floor(x#1/2)} else null);
     Ds = select(Ds, x -> x =!= null);
     << "disc: " << Ds << endl;
     S := id_(KA^n);
     L := Linv := null;
     detL := null;
     while (
	 if #Ds === 0 then break;
	 dx := selectdisc(Ds);
	 << "about to trager1" << endl;
	 (L, Linv) = trager1(omega, Ms, dx);
	 << "done with trager1" << endl;
	 detL = det L;
	 << "det L: " << detL << endl;
	 not isUnit(detL))
       do (
	 omega = (transpose Linv) * omega * Linv;
	 Ds = improveDenoms(Ds, detL);
	 Ms0 := apply(entries transpose Linv, rs -> sum apply(#rs, i -> rs#i * Ms#i));
	 Ms = apply(Ms0, m -> L * m * Linv);
	 << "new sqroot of disc: " << Ds << endl;
	 -- change Ds
	 S = S * Linv;
	 << "at end of loop, L = " << L << " and S = " << S << endl;
	 );
     matrix{vs} * S
     )

--------------------------------------------------

beginDocumentation()

doc ///
Key
  IntegralBases
Headline
  computing integral bases of plane curves
Description
  Text
  Example
Caveat
SeeAlso
///


TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

end
doc ///
Key
Headline
Inputs
Outputs
Consequences
Description
  Text
  Example
Caveat
SeeAlso
///

restart
loadPackage "IntegralBases"

TEST ///
  R = QQ[x,y]
  F = y^4-y^2+x^3+x^4
  H = findTruncations F
  displayTruncations H

  H#x#1
Ps = apply(puiseuxTruncations F, last)
syz matrix makeEquations(Ps, {1_R,y,y^2,y^3}, 1)
syz matrix makeEquations(Ps, {x*1_R,x*y,x*y^2,y^3,y^4}, 2)
syz matrix makeEquations(Ps, {1_R,y,y^2,y^3,y^4}, 2)

  Ps = apply(H#(first keys H)#1, last)
  
  B = ring Ps_0_1
  K = coefficientRing coefficientRing B
  R1 = first flattenRing(K (monoid B))
  Y = Ps_0_1
  sub(Y, R1)  
  Y^2
  use R1
  coefficients(sub(Y^2,R1), Variables=>{b,t})
  syz matrix makeEquations(Ps, {1_R,y,y^2,y^3}, 1)
///

TEST ///
  R = QQ[x,y]
  F = x^3*y^2 + x*y^4 + x^2*y^4 + y^7 + x^12*y + x^15 + y^9
  H = findTruncations F
  displayTruncations H

  netList(Ps = puiseuxTruncations F)
  time Ps = apply(Ps, last)
  time localBasis(Ps, degree_y F, x, y)
  netList oo
  
  use R
  G = sub(F, {x=>y,y=>x})
  H = findTruncations G
  displayTruncations H
///

TEST ///
  -- simple Duval example
  R = QQ[x,y]
  F = (x^2+y^2)^3 - 4*x^2*y^2
  H = findTruncations F
  displayTruncations H

  time Ps = apply(puiseuxTruncations F, last)
  netList time localBasis(Ps, degree_y F, x, y)
  eliminate(ideal F + ideal jacobian ideal F, {y}) -- (x^6)
///

TEST ///
  -- Duval example
  R = QQ[x,y]
  F = poly"y16-4y12x6-4y11x8+y10x10+6y8x12+8y7x14+14y6x16+4y5x18+y4(x20-4x18)-4y3x20+y2x22+x24"
  H = findTruncations F
  displayTruncations H 

  time Ps = apply(puiseuxTruncations F, last)
  netList time localBasis(Ps, degree_y F, x, y)
  eliminate(ideal F + ideal jacobian ideal F, {y}) --

///

TEST ///
  -- Leonard example
  R = QQ[x,y]
  F = (y^2-y-x/3)^3-y*x^4*(y^2-y-x/3)-x^11
  H = findTruncations F
  displayTruncations H 
  degree_y F -- 6
  factor discriminant(F,y)  -- x^27 * sqfree
  eliminate(ideal F + ideal jacobian ideal F, {y}) -- (x^9)
  Ps = puiseuxTruncations F
  netList oo
  time Ps = apply(Ps, last)
  netList time localBasis(Ps, degree_y F, x, y)
///

TEST ///
  --vanHoeij1 -- this one is BAD!!
  R = QQ[x,y]
  F = poly"y10+(-2494x2+474)y8+(84366+2042158x4-660492x2)y6
           +(128361096x4-4790216x2+6697080-761328152x6)y4
	   +(-12024807786x4-506101284x2+15052058268x6+202172841+134266087241x8)y2
	   +34263110700x4-228715574724x6+5431439286x2+201803238-9127158539954x10-3212722859346x8"
  H = findTruncations F
  displayTruncations H 

  use R
  G = sub(F, {x=>y,y=>x})
  H = findTruncations G
  displayTruncations H

  degree_y F -- 10
  factor discriminant(F,y)  -- x^8 * other stuff
  -- 2*(ramif 3) + 4 * (ramif 1) places over (x).
  -- 8 = 2*delta + 4, implies delta = 2
  -- max floor(Int_i) = 1
  -- so expect (0,...,0,1,1) (and get that)
  eliminate(ideal F + ideal jacobian ideal F, {y}) -- (x) * other stuff
    -- this tells us that sing locus contains x * (29x^2+3)^2
    -- this is in the conductor
  Ps = puiseuxTruncations F
  netList oo
  time Ps = apply(Ps, last)
  netList time localBasis(Ps, degree_y F, x, y)
//

TEST ///
  --vanHoeij2
  R = QQ[x,y]
  F = poly"y20+y13x+x4y5+x3(x+1)2"
  H = findTruncations F
  displayTruncations H 

  use R
  G = sub(F, {x=>y,y=>x})
  H = findTruncations G
  displayTruncations H
//

TEST ///
  --vanHoeij3
  R = QQ[x,y]
  F = poly"y30+y13x+x4y5+x3(x+1)2"
  H = findTruncations F
  displayTruncations H 

  use R
  G = sub(F, {x=>y,y=>x})
  H = findTruncations G
  displayTruncations H
//

TEST ///
  --vanHoeij4
  R = QQ[x,y]
  F = poly"y40+y13x+x4y5+x3(x+1)2"
  H = findTruncations F
  displayTruncations H 

  use R
  G = sub(F, {x=>y,y=>x})
  H = findTruncations G
  displayTruncations H
//

TEST ///
  --boehm3, also the example used in M2 aug 2009 meeting
  R = QQ[x,y]
  F = y^5+2*x*y^2+2*x*y^3+x^2*y-4*x^3*y+2*x^5
  H = findTruncations F
  displayTruncations H 

  use R
  G = sub(F, {x=>y,y=>x})
  H = findTruncations G
  displayTruncations H


  degree_y F -- 5
  factor discriminant(F,y)  -- x^8 * (x+1)^2 * (x^2-x+1)^2*sqfree
  -- 8 = 2*n + (3-1) ==> n=3
  -- max floor(Int_i) = 2
  -- so expect (0,0,1,2) (and get that)
  eliminate(ideal F + ideal jacobian ideal F, {y}) -- (x^6+x^3)
  Ps = puiseuxTruncations F
  netList oo
  time Ps = apply(Ps, last)
  netList time localBasis(Ps, degree_y F, x, y)
//

TEST ///
  R = QQ[x,y]
  F = y^3-7*x^5-8*x^7
  H = findTruncations F
  displayTruncations H 

  use R
  G = sub(F, {x=>y,y=>x})
  H = findTruncations G
  displayTruncations H
//

TEST ///
  --boehm4, over QQ
  R = QQ[x,y]
  F = 25*x^8+184*x^7*y+518*x^6*y^2+720*x^5*y^3+576*x^4*y^4+282*x^3*y^5+84*x^2*y^6+
        14*x*y^7+y^8+244*x^7+1326*x^6*y+2646*x^5*y^2+2706*x^4*y^3+1590*x^3*y^4+
	546*x^2*y^5+102*x*y^6+8*y^7+854*x^6+3252*x^5*y+4770*x^4*y^2+
	3582*x^3*y^3+1476*x^2*y^4+318*x*y^5+28*y^6+1338*x^5+3740*x^4*y+
	4030*x^3*y^2+2124*x^2*y^3+550*x*y^4+56*y^5+1101*x^4+
	2264*x^3*y+1716*x^2*y^2+570*x*y^3+70*y^4+508*x^3+738*x^2*y+
	354*x*y^2+56*y^3+132*x^2+122*x*y+28*y^2+18*x+8*y+1
  H = findTruncations F
  displayTruncations H 

  use R
  G = sub(F, {x=>y,y=>x})
  H = findTruncations G
  displayTruncations H
//

TEST ///
  --boehm5, over QQ
  R = QQ[x,y]
  F = 25*x^8+184*x^7*y+518*x^6*y^2+720*x^5*y^3+576*x^4*y^4+282*x^3*y^5+84*x^2*y^6+14*x*y^7+y^8+244*x^7+1326*x^6*y+2646*x^5*y^2+2706*x^4*y^3+1590*x^3*y^4+546*x^2*y^5+102*x*y^6+8*y^7+854*x^6+3252*x^5*y+4770*x^4*y^2+3582*x^3*y^3+1476*x^2*y^4+318*x*y^5+28*y^6+1338*x^5+3740*x^4*y+4030*x^3*y^2+2124*x^2*y^3+550*x*y^4+56*y^5+1101*x^4+2264*x^3*y+1716*x^2*y^2+570*x*y^3+70*y^4+508*x^3+738*x^2*y+354*x*y^2+56*y^3+132*x^2+122*x*y+28*y^2+18*x+8*y+1

  H = findTruncations F
  displayTruncations H 

  use R
  G = sub(F, {x=>y,y=>x})
  H = findTruncations G
  displayTruncations H
///

TEST ///
--singular-sakai1
  R = QQ[x,y] -- genus 0 4 nodes and 6 cusps
  F = poly "(x2+y2-1)3 +27x2y2"

  H = findTruncations F
  displayTruncations H 

  use R
  G = sub(F, {x=>y,y=>x})
  H = findTruncations G
  displayTruncations H
///

TEST ///
--singular-sakai2
  R = QQ[x,y] -- genus 0
  F = poly "(x-y2)2 - yx3"

  H = findTruncations F
  displayTruncations H 
///

TEST ///
  --singular-sakai5
  R = QQ[x,y]
  F = poly "55x8+66y2x9+837x2y6-75y4x2-70y6-97y7x2"
  F = sub(F, {x => y+x})
  time   H = findTruncations F
  displayTruncations H 
///


---------------------
-- A first example --
---------------------
-- This example doesn
-- ZZZ
--kk = ZZ/32003
kk = QQ
--P = kk[x]
R = kk[x,y]
F = y^5+2*x*y^2+2*x*y^3+x^2*y-4*x^3*y+2*x^5
H = findTruncations F
hashTable apply(pairs H, (k,v) -> (k,(v#0 => netList v#1)))
ds = disc(F,y)

netList puiseuxTruncations F

(G,a) = center(F, x+1)
puiseuxTruncations G

use ring F
(G,a) = center(F, x^2-x+1)
netList puiseuxTruncations G

ds = ds/first
--dx = disc(F,x)
--ds = apply(ds, d -> sub(d#0,P))
--as = apply(ds, adjoinRoot)
puiseux(F, 10, Center => ds#0)
puiseux(F, 10, Center => ds#1)
puiseux(F, 5, Center => ds#2)
netList oo

netList apply(pairs findBranches F, (a,b) -> prepend(a#0, b/first))

netList puiseux(F, 10)
(branches F)/first
netList oo
Rs = apply(as, a -> if ring a === coefficientRing R then R else (ring a)[gens R])
Fs = apply(#ds, i -> (S := Rs#i; sub(F, {R_0 => S_0 + as#i, R_1 => S_1})))
netList puiseux(Fs#0,10)
netList (P = puiseux(Fs#1,10)) -- seems nasty, this one does.
P/(x -> ring x_1)
puiseux(Fs#2,10) -- doesn't work

puiseuxTruncations F
Ps = oo/last
syz matrix makeEquations(Ps, {1_R,y,y^2,y^3}, 1)
syz matrix makeEquations(Ps, {x*1_R,x*y,x*y^2,y^3,y^4}, 2)
syz matrix makeEquations(Ps, {1_R,y,y^2,y^3}, 1)

-- how does Fractional Ideals do on this one?
S = kk[y,x,MonomialOrder=>{1,1}]
A = S/(sub(F,S))
time integralClosureHypersurface A

--------------------
-- A good example --
--------------------
restart
needsPackage "IntegralBases"
P = QQ[x]
R = QQ[x,y]
F = y^4-y^2+x^3+x^4

H = findTruncations F
hashTable apply(pairs H, (k,v) -> (k,(v#0 => netList v#1)))

disc(F,y)
netList puiseuxTruncations F

use ring F
(G,a) = center(F,4*x^4+4*x^3-1)
netList puiseuxTruncations G



netList apply(pairs findBranches F, (a,b) -> prepend(a#0, b/first))

ds = disc(F,y)
dx = disc(F,x)
ds = apply(ds, d -> sub(d#0,P))
as = apply(ds, adjoinRoot)
Rs = apply(as, a -> if ring a === coefficientRing R then R else (ring a)[gens R])
Fs = apply(#ds, i -> (S := Rs#i; sub(F, {R_0 => S_0 + as#i, R_1 => S_1})))
netList (P1 = puiseux(Fs#0,10))
netList (P2 = puiseux(Fs#1,10)) -- seems nasty, this one does.
P2/(x -> ring x_1)
P2a = apply(P2, (xt,yt) -> (xt+sub(as#1,ring xt),yt))
testPuiseux(P2a_0,F,15)
(xt,yt) = P2a_0
yt^3-yt -- something seems wrong here...


(branches F)/first//netList
(branches F)/first/series
-- 



S = QQ[y,x,MonomialOrder=>{1,1}]
A = S/(sub(F,S))
integralClosureHypersurface A

-- Example
restart
needsPackage "IntegralBases"
R = QQ[x,y]
F = y^4-y^2+x^3+x^4

H = findTruncations F
hashTable apply(pairs H, (k,v) -> (k,(v#0 => netList v#1)))


puiseuxTruncations F
Ps = oo/last
syz matrix makeEquations(Ps, {1_R,y,y^2,y^3}, 1)

Ps = findBranches F
series (values Ps)_0_0_0
series (values Ps)_1_0_0

Ps = findPuiseuxSeries F
keys Ps
syz matrix makeEquations(Ps#((keys Ps)_0), {1_R,y,y^2,y^3}, 1)
syz matrix makeEquations(Ps#((keys Ps)_1), {1_R,y,y^2,y^3}, 1)
syz matrix makeEquations(Ps#((keys Ps)_2), {1_R,y,y^2,y^3}, 1)


-- switch x and y
restart
needsPackage "IntegralBases"
R = QQ[x,y]
F = x^4-x^2+y^3+y^4
Ps = findPuiseuxSeries F
syz matrix makeEquations(Ps#((keys Ps)_0), {1_R,y,y^2,y^3}, 1)
syz matrix makeEquations(Ps#((keys Ps)_1), {1_R,y,y^2,y^3}, 1)
syz matrix makeEquations(Ps#((keys Ps)_2), {1_R,y,y^2,y^3}, 1)

S = QQ[y,x,MonomialOrder=>{1,1}]
A = S/(sub(F,S))
integralClosureHypersurface A

-- Example for M2 meeting, Aug 2009, MSRI
restart
loadPackage "IntegralBases"
kk = QQ
R = kk[x,y]
F = y^5+2*x*y^2+2*x*y^3+x^2*y-4*x^3*y+2*x^5

H = findTruncations F
hashTable apply(pairs H, (k,v) -> (k,(v#0 => netList v#1)))

disc(F,y)

puiseux(F, 10)


S = QQ[y,x,MonomialOrder=>{1,1}]
A = S/(sub(F,S))
integralClosureHypersurface A -- generated over A by one fraction with denom. x^2(x^3+1).

kk = QQ
R = kk[x,y,z]
F = z*(x^2+y^2)-x^2+y^2
F = (z+x+y)*(x^2+y^2)-x^2+y^2
F = (z+x+y)*(x^2+y^2)^2-x^2+y^2
F = (z+x+y)*(x^3+y^3)^2-x^3+y^3
A = R/F
integralClosure A
icFractions A

R = kk[y,x,z,MonomialOrder=>{1,2}]
F = sub(F,R)
F = sub(F, {y => x+y, z => z-x})
A = R/F
integralClosureHypersurface A
disc(F,y)

-- Trager algorithm
-- 
restart
debug loadPackage "IntegralBases"
loadPackage "MatrixNormalForms"
loadPackage "TraceForm"

A1 = QQ[x]
B1 = frac A1
R = QQ[x,y]
F = y^4 - y^2 + (x^3 + x^4)

D = sub(discriminant(F,y), A1)
Ds = select((factor D)//toList/toList, x -> x#1 > 1)

C1 = B1[y]
D1 = C1/sub(F,C1)
trager D1


(m,p,q) := (9,2,9); -- q =2..9 -- modifying these gives other good examples
A1 = QQ[x]
B1 = frac A1
S = B1[y]
R = S/(y^m - x^p*(x - 1)^q)

trager R

R = QQ[x,y]/(y^m - x^p*(x - 1)^q)
time integralClosure R
icFractions R
ideal ooo
see ideal gens gb oo


--leonard1
S = QQ[x,y]
ideal((y^2-y-x/3)^3-y*x^4*(y^2-y-x/3)-x^11)
R = S/oo
time integralClosure oo
icFractions R

A1 = QQ[x]
B1 = frac A1
S = B1[y]
R = S/ideal((y^2-y-x/3)^3-y*x^4*(y^2-y-x/3)-x^11)
time trager R
ideal oo
see oo

--vanHoeij1
restart
debug loadPackage "IntegralBases"
loadPackage "MatrixNormalForms"
loadPackage "TraceForm"
A1 = QQ[x]
B1 = frac A1
S = B1[y]
I = ideal"y10+(-2494x2+474)y8+(84366+2042158x4-660492x2)y6
           +(128361096x4-4790216x2+6697080-761328152x6)y4
	   +(-12024807786x4-506101284x2+15052058268x6+202172841+134266087241x8)y2
	   +34263110700x4-228715574724x6+5431439286x2+201803238-9127158539954x10-3212722859346x8"
R = S/I
time trager R  -- crash!!
