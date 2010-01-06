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
     traceRadical,
     trager
     }

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

-- ZZZ
kk = ZZ/32003
kk = QQ
P = kk[x]
R = kk[x,y]
F = y^5+2*x*y^2+2*x*y^3+x^2*y-4*x^3*y+2*x^5
ds = disc(F,y)
dx = disc(F,x)
ds = apply(ds, d -> sub(d#0,P))
as = apply(ds, adjoinRoot)
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
integralClosureHypersurface A

--------------------
-- A good example --
--------------------
restart
needsPackage "IntegralBases"
P = QQ[x]
R = QQ[x,y]
F = y^4-y^2+x^3+x^4
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


-- 



S = QQ[y,x,MonomialOrder=>{1,1}]
A = S/(sub(F,S))
integralClosureHypersurface A

-- Example
restart
needsPackage "IntegralBases"
R = QQ[x,y]
F = y^4-y^2+x^3+x^4
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
