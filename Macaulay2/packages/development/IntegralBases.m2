needsPackage "UPolynomials"
needsPackage "FractionalIdeals"
needsPackage "Puiseux"

newPackage(
        "IntegralBases",
        Version => "0.1", 
        Date => "27 July 2009",
        Authors => {{Name => "Mike Stillman", 
                  Email => "mike@math.cornell.edu", 
                  HomePage => "http://www.math.cornell.edu/~mike"}},
        Headline => "integral bases for plane curves, a la van Hoeij, and Trager",
        DebuggingMode => true
        )

needsPackage "UPolynomials"
needsPackage "FractionalIdeals"
needsPackage "Puiseux"

export {
     integralBasis,
     vanHoeij,
     Trager,
     principalPart,
     makeEquations,
     findPuiseuxSeries
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

-- van Hoeij algorithm
-- step 1: computing the factors of the discriminant, roots and Puiseux series
-- step 2: Creation of (and solving) the equations in van Hoiej
-- step 3: van Hoeij

-- Trager algorithm
-- step 1: computing the trace and mult map matrices
-- step 2: Hermite normal form
-- step 3: p-trace radical

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

