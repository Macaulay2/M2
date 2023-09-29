newPackage(
    "SheafMaps",
    Version => "0.1",
    Date => "September 27, 2023",
    Authors => {
	{Name => "Keller VandeBogert", Email => "kvandebo@nd.edi", HomePage => "todo"}},
    Headline => "methods for working with morphisms of sheaves",
    Keywords => {"algebraic geometry"},
    PackageExports => {"Truncations"},
    PackageImports => {"NormalToricVarieties"},
    DebuggingMode => true
    )

export {"sheafMap",
    "SheafMap",
    "isLiftable",
    "nlift",
    "SaturationMap",
    "TorsionFree",
    "GlobalSectionLimit"}

SheafMap = new Type of HashTable


SheafMap.synonym = "Morphism of Sheaves"

source SheafMap := CoherentSheaf => f -> f.source
target SheafMap := CoherentSheaf => f -> f.target
variety SheafMap := ProjectiveVariety => f -> variety source f
degree SheafMap := ZZ => f -> f.degree
matrix SheafMap := Matrix => opts -> f -> f.map



map(CoherentSheaf,CoherentSheaf,Matrix) := SheafMap => opts -> (G,F,phi) -> (
    if variety G =!= variety F then error "Expected sheaves over the same variety";
    if instance(variety F,NormalToricVariety) then error "Maps of sheaves not yet implemented on norma toric varieties";
    deg := if opts.Degree === null then 
        min flatten degrees source phi
    else opts.Degree;
    new SheafMap from {
        symbol source => F,
        symbol target => G,
        symbol degree => deg,
        symbol map => phi,
        symbol cache => new CacheTable
        }
    )

map(CoherentSheaf,CoherentSheaf,Matrix,ZZ) := SheafMap => opts -> (G,F,phi,d) -> (
    newPhi := inducedMap(module G,target phi)*phi;
    map(G,F,newPhi,Degree => d)
    ) 
     

sheafMap = method();
sheafMap(Matrix) := SheafMap => phi -> (
    map(sheaf target phi, sheaf source phi, phi)
    )

sheafMap(Matrix,ZZ) := SheafMap => (phi,d) -> (
    map(sheaf target phi, sheaf source phi, truncate(d,phi),d)
    )


SheafMap * SheafMap := SheafMap => (phi,psi) -> (
    d := degree phi;
    e := degree psi;
    if d >= e then map(target phi,source psi,(matrix phi)*truncate(d,matrix psi))
    else map(target phi, source psi,truncate(d,matrix phi)*truncate(d,matrix psi),d)
    )

lineOnTop := (s) -> concatenate(width s : "-") || s



expression SheafMap := Expression => f -> (
    d := degree f;
    s := (f.map);
    if s == 0 then 
        new ZeroExpression from {0}
    else new VerticalList from 
        RowExpression {MapExpression { target f, source f, s }}
    )

net SheafMap := Net => f -> (
    if f.map == 0 then net "0"
    else if f.cache.?map then 
    stack horizontalJoin ( net target f, " <--",
		            lineOnTop(net (f.cache.map)),
		            "-- ", net source f
                    ) 
    else
                stack horizontalJoin ( net target f, " <--",
		            lineOnTop(net (f.map)),
		            "-- ", net source f
                    )
     )
 
 --general method: checks whether a map phi is in the image of
 --the map Hom(eta,target phi)
isLiftable = method();
isLiftable(Matrix,Matrix) := (phi,eta) -> (
    newPhi := homomorphism'(phi);
    deta := Hom(eta,target phi);
    ((image newPhi)/coker deta) == 0
    )

--checks whether a sheaf map represented by a map
--phi : M(\geq e) --> N can be factored through
--a smaller truncation of the module M
isLiftable(SheafMap,ZZ) := (shphi,d) -> (
    phi := matrix shphi;
    M := module source shphi;
    eta := inducedMap(truncate(d,M),source phi);
    isLiftable(phi,eta)
    )

--if phi is in the image of Hom(eta,target phi), this code
--computes the actual lift
nlift = method();
nlift(Matrix,Matrix) := Matrix => (phi,eta) -> (
    newPhi := homomorphism'(phi);
    homomorphism(newPhi//Hom(eta,target phi))
    )

--lifts a sheaf map shphi represented by a module map
--phi : M(\geq d) --> N to a map M(\geq e) --> N that represents
--the same morphism of sheaves, if possible
--WARNING: this method does not actually verify if the lift is possible
nlift(SheafMap,ZZ) := SheafMap => (shphi,e) -> (
    d := degree shphi;
    phi := matrix shphi;
    M := module source shphi;
    eta := inducedMap(truncate(e,M),source phi);
    sheafMap(nlift(phi,eta),e)
    )

--lifts a sheaf map shphi represented by a module map
--phi : M(\geq d) --> N to a map M(\geq e) --> N that represents
--the same morphism of sheaves, for the smallest possible value of e
--WARNING: this method does not actually verify if the lift is possible
nlift(SheafMap) := SheafMap => shphi -> (
    d := degree shphi;
    M := module source shphi;
    m := min flatten degrees M;
    while isLiftable(shphi,d-1) and d > m do d = d-1;
    nlift(shphi,d)
    )

-*lift(Matrix,Matrix) := Matrix => opts -> (phi,eta) -> (
    newPhi := homomorphism'(phi);
    newPhi//Hom(eta,target phi)
    )*- 

ker SheafMap := CoherentSheaf => opts -> phi -> (sheaf ker matrix phi)

image SheafMap := CoherentSheaf => phi -> (sheaf image matrix phi)

coimage SheafMap := CoherentSheaf => phi -> (sheaf coimage matrix phi)

coker SheafMap := CoherentSheaf => phi -> (sheaf coker matrix phi)

CoherentSheaf#id = F -> (map(F,F,id_(module F)))

--this needs to be improved: there are some subtleties to discuss
inducedMap(CoherentSheaf,CoherentSheaf) := SheafMap => opts -> (G,F) -> (map(G,F,inducedMap(module G,module F)))

components SheafMap := List => phi -> (
    if phi.cache.?components then phi.cache.components else {phi}
    )

--WARNING: the current direct sum of sheaves does not cache components
SheafMap.directSum = args -> (
    assert(#args>0);
    X := variety args#0;
    if not all(args,phi -> variety phi === X) then error "Expected maps of sheaves over the same variety";
    DS := map(directSum apply(args,phi -> target phi),directSum apply(args,phi -> source phi),directSum apply(args,phi -> matrix phi));
    DS.cache.components = toList args;
    DS
    )

directSum SheafMap := SheafMap => phi -> (directSum(1:phi))

SheafMap ++ SheafMap := SheafMap => (phi,psi) -> directSum(phi,psi)

--this tensor command is a little too naive at the moment
tensor(SheafMap,SheafMap) := SheafMap => (phi,psi) -> (
    --m := max(degree phi, degree psi);
    map((target phi)**(target psi),(source phi)**(source psi),(matrix phi)**(matrix psi))
    )

SheafMap ** SheafMap := SheafMap => (phi,psi) -> tensor(phi,psi)
SheafMap ** CoherentSheaf := SheafMap => (phi,F) -> phi**id_F
CoherentSheaf ** SheafMap := SheafMap => (F,phi) -> id_F**phi 
SheafMap ** SheafOfRings := SheafMap => (phi,O) -> phi**(O^1)
SheafOfRings ** SheafMap := SheafMap => (O,phi) -> (O^1)**phi

--twist notation
SheafMap (ZZ) := SheafMap => (phi,d) -> phi**OO_(variety phi)^1(d)

sdual = method();
sdual(SheafMap) := SheafMap => phi -> map(dual source phi, dual target phi, dual matrix phi)

--this code refused to work, must have to do with overloading dual
--dual(SheafMap) := SheafMap => opts -> phi -> map(dual source phi, dual target phi, dual matrix phi)

sheafHom(SheafMap,SheafMap) := SheafMap => (phi,psi) -> (sdual phi)**psi
sheafHom(SheafMap,CoherentSheaf) := SheafMap => (phi,F) -> sheafHom(phi,id_F)
sheafHom(CoherentSheaf,SheafMap) := SheafMap => (F,phi) -> sheafHom(id_F,phi)
sheafHom(SheafMap,SheafOfRings) := SheafMap => (phi,O) -> sheafHom(phi,O^1)
sheafHom(SheafOfRings,SheafMap) := SheafMap => (O,phi) -> sheafHom(O^1,phi)




--Some questions:
--Should prune automatically use the nlift command to find the 
--simplest possible representative? I think this fits with the intent of prune,
--but maybe the user should be the one to decide whether they want the
--simplest representative

--this is code that Devlin wrote based on some discussions
--that he and I had regarding computing the maps on cohomology
mapOnExt=method();
mapOnExt(ZZ,CoherentSheaf,SheafMap) :=  (m,F,A)->(
e:=0;
          if not instance(variety F,ProjectiveVariety)
          then error "expected sheaves on a projective variety";
          M := module F;
          N1 := module source A;
          N2 := module target A;
          R := ring M;
          if not isAffineRing R
          then error "expected sheaves on a variety over a field";
          f := presentation R;
          S := ring f;
          n := numgens S -1;
--Is N1 the right one to use in the next lines here, vs N2?
          l := min(dim N1, m);
          P := resolution(cokernel lift(presentation N1,S) ** cokernel f);
          p := length P;
          if p < n-l then(print p; E := Ext^m(truncate(0,M), matrix A))
--truncating M at 0 here seems wrong and gives the wrong results
          else (
                    a := max apply(n-l..p,j -> (max degrees P_j)#0-j);
                    r := a-e-m+1;
                    E = basis(0,Ext^m(truncate(r,M), matrix A))))

globalSectionsModule = (G,bound) -> (
     -- compute global sections
     if degreeLength ring G =!= 1 then error "expected degree length 1";
     M := module G;
     A := ring G;
     --M = cokernel presentation M;
     S := saturate image map(M,A^0,0);
     if S != 0 then M = M/S;
     G.module.cache.TorsionFree = M;
     F := presentation A;
     R := ring F;
     N := cokernel lift(presentation M,R) ** cokernel F;
     r := numgens R;
     wR := R^{-r};
     p := 0;
     if bound < infinity and pdim N >= r-1 then (
	  E1 := Ext^(r-1)(N,wR);
	  p = (
	       if dim E1 <= 0
	       then max degreeList E1 - min degreeList E1 + 1
	       else 1 - first min degrees E1 - bound
	       );
	  if p === infinity then error "global sections module not finitely generated, can't compute it all";
	  if p > 0 then M = Hom(image matrix {apply(numgens A, j -> A_j^p)}, M);
	  );
      G.module.cache.GlobalSectionLimit = max(0,p);
      minimalPresentation M)

degreeList = (M) -> (
     if dim M > 0 then error "expected module of finite length";
     H := poincare M;
     T := (ring H)_0;
     H = H // (1-T)^(numgens ring M);
     exponents H / first)



cohomology(ZZ,SumOfTwists) :=  Module => opts -> (i,S) -> (
     F := S#0;
     M := module F;
     R := ring F;
     if not isAffineRing R then error "expected coherent sheaf over a variety over a field";
     b := first S#1;
     if i === 0 then (
	 H := globalSectionsModule(F,b);
	 p := F.module.cache.GlobalSectionLimit;
	 iota := inverse H.cache.pruningMap;
	 N := F.module.cache.TorsionFree;
	 quot := inducedMap(N, M);
	 F.module.cache.SaturationMap = if p == 0 then iota*quot
	 else (
	     homMap := Hom(inducedMap(R^1,image matrix {apply(numgens R, j -> R_j^p)}),N);
	     iota * homMap * inducedMap(Hom(R^1, N), N) * quot);
	 H
	 )
     else HH^(i+1)(M,Degree => b)
     )


minimalPresentation CoherentSheaf := prune CoherentSheaf := opts -> F -> sheaf HH^0 F(>=0)

minimalPresentation SheafMap := 
prune SheafMap := SheafMap => opts -> f -> (
    F := source f;
    G := target f;
    prune F; --these are pruned just to build cache data
    prune G;
    tfF := F.module.cache.TorsionFree;
    tfG := G.module.cache.TorsionFree;
    f' := inducedMap(tfG,tfF,matrix f);
    p := max(F.module.cache.GlobalSectionLimit,G.module.cache.GlobalSectionLimit);
    B := ideal vars ring module F;
    Bp := module B^[p];
    nlift sheafMap prune Hom(Bp,f'))



beginDocumentation()

doc ///
    Key
        SheafMaps
    Headline
        a package for computing with morphisms of sheaves
    Description
        Text
            ToDo
    Subnodes
        sheafMap
///

TEST ///
S = QQ[x_1..x_3];
X = Proj S;
phi1 = vars S
G = sheaf target phi1
F = sheaf source phi1
shphi = map(G,F,phi1)
peek shphi
assert(source shphi === OO_X^3(-1))
assert(target shphi === OO_X^1)
assert(degree shphi ===1)
phi = truncate(3,phi1);
shphi2 = map(G,F,phi,3)
assert(source shphi2 === OO_X^3(-1))
assert(target shphi2 === OO_X^1)
assert(degree shphi2 === 3)
///

TEST ///
S = QQ[x_1..x_3];
X = Proj S;
phi = vars S;
psi = (transpose phi)**S^{1:-2}
shphi = sheafMap(phi)
assert(matrix entries shphi.map == matrix {{x_1, x_2, x_3}})
shpsi = sheafMap(psi)
shphi*shpsi
shphi3 = sheafMap(phi,3)
shphi3*shpsi

///

end--

uninstallPackage "SheafMaps"
restart
debug installPackage "SheafMaps"
check SheafMaps


S = QQ[x_1..x_3];
X = Proj S;
phi1 = vars S
G = sheaf target phi1
F = sheaf source phi1
shphi = map(G,F,phi1)
peek shphi
assert(source shphi === OO_X^3(-1))
assert(target shphi === OO_X^1)
assert(degree shphi ===1)

S = QQ[x_1..x_3];
X = Proj S;
phi = vars S;
psi = (transpose phi)**S^{1:-2}
shphi = sheafMap(phi)
assert(matrix entries shphi.map == matrix {{x_1, x_2, x_3}})
shpsi = sheafMap(psi)
shphi*shpsi
shphi3 = sheafMap(phi,3)
shphi3*shpsi
phii = matrix oo
eta = inducedMap(truncate(2,source phii),truncate(3,source phii))
shphi**shpsi --this output seems to be a problem: should probably truncate 
             --to a common degree, first
sheafHom(shphi,OO_X^3) --this looks like the correct output
mapOnExt(1,OO_X^1,shphi**OO_X(-3))
hphi = prune mapOnExt(2,OO_X^1,shphi**OO_X(-3)) --I think this is correct actually
target hphi
source hphi
ZERO = sheaf coker vars S
prune ZERO
F = sheaf truncate(4,S^1)
prune F
oo.module.cache.pruningMap
S = QQ[x_1..x_4];
F = sheaf(comodule (ideal(x_1,x_2)*ideal(x_3,x_4)))
pF = prune F
sMap = F.module.cache.SaturationMap
pF.module.cache.pruningMap
peek F.module.cache
shsMap = sheafMap(sMap,4)
nlift shsMap
F = sheaf comodule (ideal(x_1,x_2)*ideal(x_3,x_4))
prune F
Omega = sheaf ker vars S;
pOmega = prune Omega
sMap = Omega.module.cache.SaturationMap
assert(source sMap === module Omega)
assert(target sMap === module pOmega)
peek Omega.module.cache 
sMap = Omega.module.cache.SaturationMap
source sMap
target sMap
F = sheaf comodule (ideal(x_1,x_2,x_3)*ideal(x_3,x_4))
pF = prune F
sMap = F.module.cache.SaturationMap
assert(source sMap === module F)
assert(target sMap ===  module pF)
target pMap
F = sheaf S^{5}/((ideal(x_1,x_2)*ideal(x_3,x_4)))
pF = prune F
pMap = pF.module.cache.pruningMap
F = sheaf S^{5}/((ideal(x_1,x_2,x_3)*ideal(x_3,x_4)))
pF = prune F
sMap = F.module.cache.SaturationMap
shsMap = sheafMap sMap
nlift shsMap

--TODO: check if things have already been pruned
S = QQ[x_1..x_3];
X = Proj S
f = sheafMap(truncate(2,vars S))

prune f

K = ker vars S
f = sheafMap dual wedgeProduct(1,1,K)
prune f

cohomology(ZZ,SheafMap) := Matrix => opts -> (p,f) -> (if p==0 then basis(0,matrix prune f) else error "not yet"
    )
