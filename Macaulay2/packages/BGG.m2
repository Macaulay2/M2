-- 
newPackage(
	"BGG",
    	Version => "1.0", 
    	Date => "October 4, 2005",
    	Authors => {
	     {Name => "David Eisenbud", Email => "sorin@math.sunysb.edu", HomePage => "http://www.math.sunysb.edu/~sorin/"},
	     {Name => "Wolfram Decker", Email => "ggsmith@mast.queensu.ca", HomePage => "http://www.mast.queensu.ca/~ggsmith"},	-- oops -- who's the author???
	     {Name => "Frank Schreyer", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike"}
	     },
    	Headline => "Bernstein-Gelfand-Gelfand correspondence",
    	DebuggingMode => true
    	)

export {symExt,bgg,tateResolution,sheafCohomology,sortedBasis,beilinson1,beilinson,U}

symExt = (m,E) ->(
     ev := map(E,ring m,vars E);
     mt := transpose jacobian m;
     jn := gens kernel mt;
     q  := vars(ring m)**id_(target m);
     ans:= transpose ev(q*jn);
     --now correct the degrees:
     map(E^{(rank target ans):1}, E^{(rank source ans):0}, 
         ans));

bgg = (i,M,E) ->(
     S :=ring(M);
     numvarsE := rank source vars E;
     ev:=map(E,S,vars E);
     f0:=basis(i,M);
     f1:=basis(i+1,M);
     g :=((vars S)**f0)//f1;
     b:=(ev g)*((transpose vars E)**(ev source f0));
     --correct the degrees (which are otherwise
     --wrong in the transpose)
     map(E^{(rank target b):i+1},E^{(rank source b):i}, b));

tateResolution = (m,E,loDeg,hiDeg)->(
     M := coker m;
     reg := regularity M;
     bnd := max(reg+1,hiDeg-1);
     mt  := presentation truncate(bnd,M);
     o   := symExt(mt,E);
     --adjust degrees, since symExt forgets them
     ofixed   :=  map(E^{(rank target o):bnd+1},
                E^{(rank source o):bnd},
                o);
     res(coker ofixed, LengthLimit=>max(1,bnd-loDeg+1)));

sheafCohomology = (m,E,loDeg,hiDeg)->(
     T := tateResolution(m,E,loDeg,hiDeg);
     k := length T;
     d := k-hiDeg+loDeg;
     if d > 0 then 
        chainComplex apply(d+1 .. k, i->T.dd_(i))
     else T);

sortedBasis = (i,E) -> (
     m := basis(i,E);
     p := sortColumns(m,MonomialOrder=>Descending);
     m_p);

beilinson1=(e,dege,i,S)->(
     E := ring e;
     mi := if i < 0 or i >= numgens E then map(E^1, E^0, 0)
           else if i === 0 then id_(E^1)
           else sortedBasis(i+1,E);
     r := i - dege;
     mr := if r < 0 or r >= numgens E then map(E^1, E^0, 0)
           else sortedBasis(r+1,E);
     s = numgens source mr;
     if i === 0 and r === 0 then
          substitute(map(E^1,E^1,{{e}}),S)
     else if i>0 and r === i then substitute(e*id_(E^s),S)
     else if i > 0 and r === 0 then
          (vars S) * substitute(contract(diff(e,mi),transpose mr),S)
     else substitute(contract(diff(e,mi), transpose mr),S));

beilinson = (o,S) -> (
     coldegs := degrees source o;
     rowdegs := degrees target o;
     mats = table(numgens target o, numgens source o,
              (r,c) -> (
                   rdeg = first rowdegs#r;
                   cdeg = first coldegs#c;
                   overS = beilinson1(o_(r,c),cdeg-rdeg,cdeg,S);
                   -- overS = substitute(overE,S);
                   map(U(rdeg,S),U(cdeg,S),overS)));
     if #mats === 0 then matrix(S,{{}})
     else matrix(mats));

U = (i,S) -> (
     if i < 0 or i >= numgens S then S^0
     else if i === 0 then S^1
     else cokernel koszul(i+2,vars S) ** S^{i});

-- To implement
beginDocumentation()

document { Key => BGG,
     Headline => "BGG (Bernstein-Gelfand-Gelfand) correspondence",
     EM "BGG", " is a package for using the Eisenbud-Floystad-Schreyer
     explicit form of the BGG correspondence."
     }

///
load "BGG.m2"
S=ZZ/32003[x_0..x_2];
E=ZZ/32003[e_0..e_2,SkewCommutative=>true];
M=coker matrix{{x_0^2, x_1^2}};
m=presentation truncate(regularity M,M);
symExt(m,E)
M=cokernel matrix{{x_0^2, x_1^2, x_2^2}};
bgg(1,M,E)
m = matrix{{x_0,x_1}};
regularity coker m
T = tateResolution(m,E,-2,4)
betti T
T.dd_1
S=ZZ/32003[x_0..x_3];
E=ZZ/32003[e_0..e_3,SkewCommutative=>true];
m=koszul(3,vars S);
regularity coker m
betti tateResolution(m,E,-6,2)
betti sheafCohomology(m,E,-6,2)
M=sheaf coker m;
HH^1(M(>=0))
S = ZZ/32003[x_0..x_2];
U1 = coker koszul(3,vars S) ** S^{1};
k2 = koszul(2,vars S)
alpha = map(U1 ++ U1, S^{-1}, transpose{{0,-1,0,1,0,0}});
alphad = map(S^1, U1 ++ U1, matrix{{0,1,0,0,0,1}} * (k2 ++ k2));
F = prune homology(alphad, alpha);
betti  F
S=ZZ/32003[x_0..x_3];
E=ZZ/32003[e_0..e_3,SkewCommutative=>true];
koszul(2,vars S)
sortedBasis(2,E)
beilinson1(e_1,1,3,S)
beilinson1(e_1,1,2,S)
beilinson1(e_1,1,1,S)
S=ZZ/32003[x_0..x_2];
E = ZZ/32003[e_0..e_2,SkewCommutative=>true];
alphad = map(E^1,E^{-1,-1},{{e_1,e_2}})
alpha = map(E^{-1,-1},E^{-2},{{e_1},{e_2}})
alphad=beilinson(alphad,S);
alpha=beilinson(alpha,S);
F = prune homology(alphad,alpha);
betti  F
S = ZZ/32003[x_0..x_4];
E = ZZ/32003[e_0..e_4,SkewCommutative=>true];
beta=map(E^1,E^{-2,-1},{{e_0*e_2+e_1*e_3,-e_4}})
alpha=map(E^{-2,-1},E^{-3},{{e_4},{e_0*e_2+e_1*e_3}})
beta=beilinson(beta,S);
alpha=beilinson(alpha,S);
G = prune homology(beta,alpha);
betti res G
foursect = random(S^4, S^10) * presentation G;
IX = trim minors(4,foursect);
codim IX
degree IX
codim singularLocus IX
alphad = matrix{{e_4*e_1, e_2*e_3},{e_0*e_2, e_3*e_4},
                {e_1*e_3, e_4*e_0},{e_2*e_4, e_0*e_1},
                {e_3*e_0, e_1*e_2}};
alphad=map(E^5,E^{-2,-2},alphad)
alpha=syz alphad
alphad=beilinson(alphad,S);
alpha=beilinson(alpha,S);
FHM = prune homology(alphad,alpha);
betti res FHM
regularity FHM
betti sheafCohomology(presentation FHM,E,-6,6)
sect =  map(S^1,S^15,0) | random(S^1, S^4);
mapcone = sect || transpose presentation FHM;
fmapcone = res coker mapcone;
IX =  trim ideal fmapcone.dd_2;
codim IX
degree IX
codim singularLocus IX
///

end

randomMap = (F,G) -> (
     R := ring F;
     H := Hom(F,G);
     Hdeg0 := basis(0,H);
     randomf := Hdeg0 * random(source Hdeg0, R^1);
     homomorphism randomf)

randomVanishingIdeal = (F,G) -> (
     randomf := randomMap(F,G);
     presentIX := presentation cokernel randomf;
     sz := syz transpose presentIX;
     if numgens source sz =!= 1 then
       << "warning: expected syzygy to be a (twisted) ideal" << endl;
     trim ideal sz)

loadPackage "BGG"
KK=ZZ/32003
E=KK[e_0..e_4,SkewCommutative=>true];
S=KK[x_0..x_4]; 

-- veronese surface in P^4
F = random(E^{-1},E^{-4,-4,-4})
G = beilinson(F,S)
Z = syz transpose presentation cokernel G
J = trim ideal Z
res J
betti oo

-- rat.d7.g4.new
F = random(E^{-1,0},E^{-4,-4,-4,-4})
G = beilinson(F,S)
Z = syz transpose presentation cokernel G
J = trim ideal Z
res J
betti oo
degree J
genera comodule J

-- rat.d8.g5.new
F = random(E^{-1,-1,0,0,0,0,0},E^{-2,-2})
G = beilinson(F,S)
Z = syz transpose presentation cokernel G
J = trim ideal Z
res J
betti oo
degree J
genera comodule J

-- rat.d10.g9.quart1.new
F = random(E^{-1,-1,0},E^{-3,-3})
G = beilinson(F,S)
Z = syz transpose presentation cokernel G
J = trim ideal Z
res J
betti oo
degree J
genera comodule J

-- K3 surface
F = random(E^{-2,-2,-2},E^{-3,-3,-3,-3,-4})
G = beilinson(F,S)
Z = syz transpose presentation cokernel G;
J = trim ideal Z;
res J
betti oo
degree J
genera comodule J
J = randomVanishingIdeal((U(3,S))^4 ++ U(4,S), (U(2,S))^3)
betti J
res J

-- k3.d8.g6
F = S^{-2,-1,-1}
G = U(1,S)
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J
--       total: 1 8 11 5 1
--           0: 1 .  . . .
--           1: . .  . . .
--           2: . .  . . .
--           3: . 8 11 5 1

-- k3.d9.g8
F = U(4,S) ++ U(3,S)
G = S^6
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J
--       total: 1 6 6 1
--           0: 1 . . .
4--           1: . . . .
--           2: . . . .
--           3: . 6 6 1

-- k3.d11.g12
F = S^{-1} ++ (U(3,S))^3
G = (U(2,S))^2 ++ S^2
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- k3.d12.g14
F = S^{-1} ++ (U(3,S))^4
G = (U(2,S))^3
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- ell.d7.g6
F = S^{-2,-2}
G = S^{-1,-1,1}
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- ell.d8.g7
F = S^{-1,-1}
G = S^{0,1,1}
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- ell.d9.g7
F = S^{-1} ++ (U(2,S))^2
G = (U(1,S))^3 ++ S^2
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- ell.d10.g10
F = S^{-1,-1} ++ U(3,S)
G = U(1,S) ++ S^3
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- ell.d11.g12
F = S^{-1,-1} ++ (U(3,S))^2
G = U(2,S) ++ U(1,S) ++ S^1
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

-- HP of U
S = ZZ/32003[a..e]
P0 = hilbertPolynomial(S^1,Projective=>true)
P1 = hilbertPolynomial(U(1,S) ** S^{-4},Projective=>true)
P2 = hilbertPolynomial(U(2,S) ** S^{-4},Projective=>true)
P3 = hilbertPolynomial(U(3,S) ** S^{-4},Projective=>true)
P4 = hilbertPolynomial(U(4,S) ** S^{-4},Projective=>true)

P0+3*P4-P1
P0-3*P2+4*P3+P4

matrix{{17,27,19,5}}
LLL syz oo
ker ooo
LLL gens oo

-P1+P2-2*P4
-P1-2*P2+4*P3-P4
-2*P1+3*P2-3*P3+2*P4

-- doesn't work (a=b=0, d=11, g=11)
F = U(1,S) ++ (U(2,S))^2 ++ U(4,S)
G = (U(3,S))^4
J = randomVanishingIdeal(F,G);
res J
betti res J
degree J
genera J

gg = (a,b) -> 
  {-1,-2,4,-1} + a * {-1, 1, 0, -2} + b * {-2, 3, -3, 2}
gg(1,1) -- {-4,2,1,-1}
randomVanishingIdeal((U(2,S))^2 ++ U(3,S), (U(1,S))^4 ++ U(4,S))
gg(1,2) -- {-6, 5, -2, 1}
randomVanishingIdeal((U(2,S))^5 ++ U(4,S), (U(1,S))^6 ++ (U(3,S))^2)
random(E^{-1,-1,-1,-1,-1,-1,-3},E^{-2,-2,-2,-2,-2,-4})
beilinson(oo,S)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages NAMEOFPACKAGE=BGG install-one"
-- End:

