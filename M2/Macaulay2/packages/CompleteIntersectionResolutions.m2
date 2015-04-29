newPackage(
              "CompleteIntersectionResolutions",
              Version => "0.8", 
              Date => "April 2015",
              Authors => {{Name => "David Eisenbud", 
                        Email => "de@msri.org", 
                        HomePage => "http://www.msri.org/~de"}},
              Headline => "Analyzing Resolutions over a Complete Intersection",
	      PackageExports => {"BGG"},
              DebuggingMode => true --should be false when submitted
              )
	  export{
          --some utilities
--	   submoduleByDegrees,	   
--    	   toArray,
	   "splittings",
	   "S2",
	   "hf",
	   "isQuasiRegular",
	   "makeModule",
	   "makeModule1",	   
   	   "isLinear",
	   "cosyzygyRes",	  	   
	   	   --things related to Ext over a complete intersection
	   "ExtModule", 
	   "evenExtModule", 
	   "oddExtModule",
	   "ExtModuleData",
	   "highSyzygy",
  	   "Optimism", -- optional arg for highSyzygy etc	   
	       --tools used to construct the matrix factorization and 
	       --things related to it
	   "makeT",
	   "koszulExtension",
	   "matrixFactorization",
	   "Check", -- optional arg for matrixFactorization
	       --scripts to unpack the info in a matrix factorization
	   "BRanks",
	   "ARanks",
	   "bMaps",
	   "dMaps",
	   "psiMaps",
	   "hMaps",
	   "mfBound",
	   "finiteBettiNumbers",
           "infiniteBettiNumbers",
	   "makeFiniteResolution",	   
	          --some families of examples
	   "twoMonomials",
	   "sumTwoMonomials",
    	    	    --modules over the exterior algebra
	   "makeHomotopies",
	   "makeHomotopies1",
	   "extVsCohomology",
   	   "exteriorTorModule",
	    "exteriorExtModule",	   
	   "TateResolution",
	   "BGGL",	   
	   "freeExteriorSummand",
	   --the inverse problem: represent a module as an Ext_R(M,k)
	   "moduleAsExt",
	   "hfModuleAsExt",
	   "complexity",
	   "stableHom",
	   "mapToHomomorphism",
	   "isStablyTrivial"
	   }

stableHom = method()
stableHom(Module, Module) := (M,N)->(
    --returns the map from Hom(M,N) to the stable Hom
    H := Hom(M,N);
    if isFreeModule M then return map((ring M)^0, H, 0);
    p := map(N, cover N, 1);
    map(coker Hom(M,p), Hom(M,N), 1))

mapToHomomorphism = method()
mapToHomomorphism Matrix := f ->(
    S := ring f;
    M := source f;
    N := target f;
    F := cover M;
    p := map(M, F, 1);
    Fd := dual F;
    one := reshape(Fd**F,S^1, id_F);
    map(Hom(M,N), S^1, Hom(M,f)*((Hom(F,p)//Hom(p,M)))*one)
	    )

isStablyTrivial = method()
isStablyTrivial Matrix := f ->(
   -- f: M \to N is given.
   -- represent f as an element of Hom, that is, as a map (ring M)^1 \to Hom(M,N) 
   --then apply stableHom.
   f1 := mapToHomomorphism f;
   (stableHom(source f, target f)*f1) == 0)

///
restart
uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"
viewHelp CompleteIntersectionResolutions
loadPackage("CompleteIntersectionResolutions", Reload=>true)
S = ZZ/101[a,b,c]
M = S^2/ideal"a,b"++S^3/ideal"b,c"
N = coker random (S^{0,1}, S^{-1})
Hom(M,M)
g = mapToHomomorphism id_M
homomorphism g
stableHom(M,M)
isStablyTrivial id_M
isStablyTrivial(map(M, cover M, 1))
///

   

isQuasiRegular = method()
isQuasiRegular(Matrix, Module) := (ff,E) ->(
    len := rank source ff;
    if len > dim E then return false;
    T := null;
    for i from 0 to len-1 do (
        T = ker(ff_{i}**id_(E/(ideal(ff_{0..i-1})*E)));
	if dim T > 0 then return false
	    else if i==len-1 then return true
	))
isQuasiRegular(List, Module) := (fList, E) ->isQuasiRegular(matrix{fList}, E)
isQuasiRegular(Sequence, Module) := (fseq, E) ->isQuasiRegular(toList fseq, E)


hf=method()
hf(Sequence, Module) := (range,P) -> (
       apply(toList range,i->hilbertFunction(i, P)))
hf(List, Module) := (range,P) -> (
       apply(range,i->hilbertFunction(i, P)))

submoduleByDegrees = method()
submoduleByDegrees(Module,ZZ):= (A,n)->(
     F := cover A;
     L := flatten degrees F;
     L1:= positions(L,d->d<=n);
     image (inducedMap(A,cover A)*F_L1)
     )
{*
submatrixByDegrees = method()
submatrixByDegrees(Matrix, List, List) := (f,D,E)->(
     --D,E are lists of degrees for rows and cols, respectively
     Ltarget := flatten degrees target f;
     Lsource := flatten degrees source f;
     Lt:= toList select(toList(0..(rank target f)-1),i->member(Ltarget_i, D));
     Ls:= toList select(toList(0..(rank source f)-1),i->member(Lsource_i, E));
     map(target((target f)^Lt),source((source f)_Ls), f_Ls^Lt)
     )
*}
toArray = method()
toArray List := L -> splice [toSequence L]
toArray ZZ := n->[n]

transpose Module := M -> coker transpose presentation M
    --this is Auslander's transpose functor
    


ExtModule = method()
ExtModule Module := M -> (
     --If M is a module over a complete intersection R
     --of codim c, the script returns   
     --Ext^*(M,(ring M)^1/(ideal vars ring M))
     --graded in POSITIVE degrees
     --as a module over the polynomial ring kk[X_1..X_(codim R)],
     --where the vars have degree 2
     R := ring M;
     kk := coefficientRing R;
     kkk := (ring M)^1/(ideal vars ring M);
     E := Ext(M,kkk);
     TE := ring E;
     c := numgens source presentation R;
     X := local X;
     T := kk[X_0..X_(c-1), Degrees => toList(c:{2})];
     v := map(T,
	  ring E, 
	  vars T | matrix{toList ((numgens R):0_T)}, 
	  DegreeMap => i -> {-first i} );
     prune coker v presentation E)

  
evenExtModule = method()
evenExtModule Module := M -> (
     --If M is a module over a complete intersection R
     --of codim c, the script returns 
     --Ext^(even)(M,(ring M)^1/(ideal vars ring M))
     --as a module generated in degree 0
     --over the polynomial ring kk[X_1..X_(codim R)],
     --where the vars have degree 1
     E := ExtModule M;
     P := positions(flatten degrees E, even);
     Ee:=prune image (E_P);
     T := ring E;
     kk:= coefficientRing T;
     X := symbol X;
     T1 := kk[X_0..X_(numgens T -1)];
     v1 := map(T1, T, vars T1, DegreeMap => i->{(first i)//2});
     coker v1 presentation Ee
     )


oddExtModule = method()
oddExtModule Module := M -> (
     --If M is a module over a complete intersection R
     --of codim c, the script returns 
     --Ext^(odd)(M,(ring M)^1/(ideal vars ring M))
     --as a module generated in degree 0
     --over the polynomial ring kk[X_1..X_(codim R)],
     --where the vars have degree 1
     E := ExtModule M;
     P := positions(flatten degrees E, odd);
     Eo:=prune image (E_P);
     T := ring E;
     kk:= coefficientRing T;
     X := symbol X;
     T1 := kk[X_0..X_(numgens T -1)];
     v1 := map(T1, T,vars T1, DegreeMap => i->{(first i)//2});
     coker v1 presentation Eo
     )

    

makeT = method()
makeT(Matrix, ChainComplex,ZZ) := (F,G,i) ->(
     {*
     If F is an m x 1 matrix and
     G is a resolution of a module at least up to the i-th step,
     over R = S/(ideal F), 
     of codim c this returns a list of the c ci-operators
     G_i \to G_{i-2}
     corresponding to the entries of F.
     *}
     c := numcols F;
     degsF := flatten((degrees F)_1);
     R := ring G;
     S := ring F;
     d0 := sub(G.dd_i, S);
     d1 := sub(G.dd_(i-1), S);
     Gtar := target d1;
     Gsour := source d0;
     d2 := d1*d0;
     utemp := local utemp;
     u := apply(c,i ->(
	     utemp = map(S^{-degsF_i}**Gtar, Gsour, d2//((target d2)**F_{i}));
	     d2 = d2 - utemp**F_{i};
	     utemp));
     --check: is d1*d0 = sum F_{i}*u_i 
     if d1*d0 != map(Gtar, Gsour, sum(c, i-> u_i**F_{i})) then 
                  error{"doesn't add up"};
     ret := map(R,S);
     apply(u, u1 -> ret u1)
     )

isSurjCIOperator = method()
isSurjCIOperator(Matrix, ChainComplex, ZZ) := (F,G,i) ->(
     {*
     Assuming that G is a resolution over a complete intersection
     S/ideal F with
     F = matrix{{f1, f2, ...}}
     returns "true" iff the operator G_i -> G_(i-2)
     "corresponding to f1" is surjective.
     *}
     v := (makeT(F,G,i))_0;
     0 == coker v
     )

isSurjCIOperator(Matrix, ChainComplex) := (F,G) ->(
     {*
     Assuming that G is a resolution over a complete intersection
     S/ideal F with
     F = matrix{{f1, f2, ...}}
     returns the smallest integer i
     so that the operator 
     G_j -> G_(j-2)
     "corresponding to f1" 
     is surjective for all i\leq j\leq length G.
     Question: is it enough to check this up to the regularity of 
     Ext?
     *}
     r := length G;     
     if not isSurjCIOperator(F,G,r) then return -1;
     for j from 0 to r-2 do
     	  if not isSurjCIOperator(F,G,r-j) then return r-j+1;
     2
     )

///
restart
loadPackage "CompleteIntersectionResolutions"
kk= ZZ/101
S = kk[x,y,z]
F = matrix"x3,y3"
R = S/ideal F;
M = coker random(R^{0,-1}, R^{-2,-4,-5});
G = res(M, LengthLimit =>3)
isSurjCIOperator(F,G)
G = res(M, LengthLimit =>4)
isSurjCIOperator(F,G)
G = res(M, LengthLimit =>10)
isSurjCIOperator(F,G)
///

splittings = method()
splittings (Matrix, Matrix) := (a,b) -> (
     {*
     Assuming that (a,b) are the maps of a right exact
     sequence 
              a      b
     0--> A ----> B ----> C ----> 0 
     
     with B, C free,
--     the script produces a list {tau,sigma}
     the script produces a list {sigma, tau)
     sigma: B --> A a splitting of a and
     with tau: C --> B a splitting of b;
     that is
     a*sigma+tau*b = 1_B
     sigma*a = 1_A
     b*tau = 1_C
     *}
     if not isFreeModule source b then error("source b not free");
     if not isFreeModule target b then error("target b not free");
     (tau,remtau) := quotientRemainder(id_(target b),b);
     if remtau !=0 then error("second map not splittable");
     (sigma,remsigma) := quotientRemainder(id_(source b) - (tau*b),a);
     if remsigma !=0 then error("first map not splittable");
     {map(source a, target a, sigma), map(source b, target b,tau)}
     )

///
restart
loadPackage "CompleteIntersectionResolutions"
kk= ZZ/101
S = kk[x,y,z]
t = random(S^{2:-1,2:-2}, S^{3:-1,4:-2})
t = id_(S^2)
betti t
isSurjective t
ss = splittings(syz t, t)
ss/betti

(A,B) = (syz t, t)
spl = splittings(A,B)
sigma = spl_0; tau=spl_1;
     assert(A*sigma+tau*B == id_(source B));
     assert(sigma*tau==0);
     assert(B*tau == id_(target B));
     assert(sigma*A == id_(source A));
///


cosyzygyRes = method()
cosyzygyRes (ZZ,Module) := (p,M)-> (
    --returns a p+1-step resolution F of the 
    --p-th cosyzygy of M (so F.dd_p is the presentation
    --matrix of M.) 
    --This is zero if the module
    --is annihilated by a nonzerodivisor. Makes most sense for
    --an MCM over a Gorenstein ring.
    E:=res (transpose M, LengthLimit => p+1);
    chainComplex apply(p+1, j->transpose E.dd_(p+1-j))
    )
	     
cosyzygyRes Module := M -> cosyzygyRes(2,M)

matrixFactorization = method(Options=>{Check => false})
matrixFactorization(Matrix, Module) := opts -> (ff, M) -> (
    --Inputs:
    --ff = {{f1,..,fc}} is a 1 x c matrix 
    --whose entries are a sufficiently 
    --general regular sequence in S.
    --R#c := S/(ideal ff).
    --M an R#c-module that is a high syzygy over R#c.
    --
    --If opts#check == true (the default value) then various
    --tests are performed along the way.
    
    --Outputs: 
    --d: a triangular map of direct-sum modules,
    --the matrix factorization differential.
    --
    --h: a hashTable where the h#p are maps of direct sum modules.
    --the partial homotopies.
    --
    --Description:
    --Atar#p = (target BS#1++..++target BS#p) 
    --Asour#p = (source BS#1++..++source BS#p), and
    --
    --d: Atar#c <-- Asour#c
    --and h#p: Asour#p <--- Atar#p over S.
    --The map
    --d is a special upper triangular 
    --lifting to S of the presentation matrix
    --of M over R#c.
    --
    --The map h#p is a homotopy for ff#p on the restriction
    --dpartial#p: Atar#p <-- Asour#p of d, over the ring R#(p-1),
    --so dpartial#p * h#p = ff#p mod (ff#1..ff#(p-1).
    --
    --In addition, h#p * dpartial#p induces f#p on B1#p.
    --
    --Notation:
    --B1#i is the i-th matrix (ie, complex) 
    --of the matrix factorization tower,
    --regarded as a map over R#(i-1);
    --A#(p-1) is the matrix over R#p obtained inductively
    --as the induced map on the complex
    --ker A1#(p) -->> B1#(p), where A1#p is A#p lifted to R#(p-1).
    --inc#(p,0): source A#(p-1) \to source A#p -- inclusion
    --inc'#(p,0): splits inc#(p,0)
    --inc#(p,1) and inc'#(p,1): same for targets
    --proj#(p,0):source A1#p -->> source B1#p
    --proj'#(p,0):its splitting
    --proj#(p,1), proj'#(p,1): same for targets.
    
--Initialize local variables
    spl:= null; -- a dummy variable for splittings
    h := new MutableHashTable;
    A := new MutableHashTable;
    A1 := new MutableHashTable;
    --A1#p is A#p substituteed into R#(p-1)
    B1 := new MutableHashTable;
    --B1#p would be B#p over R#(p-1) (there is no B)
    BS := new MutableHashTable; --same over S
    dpartial := new MutableHashTable;    
    psi:= new MutableHashTable;--psi#p: B1#p-->target A#(p-1)
    psiS:= new MutableHashTable;--psi#p: B1#p-->target A#(p-1)    
    inc := new MutableHashTable; --the #p versison are over R#(p-1)
    inc' := new MutableHashTable;    
    inc'S := new MutableHashTable;        
    proj := new MutableHashTable; 
    projS := new MutableHashTable;     
    proj' := new MutableHashTable;
    E := null; -- cosyzygy complex over R#p
    E1 := new MutableHashTable;
    --E1#i will be E.dd_i substituted into R#(p-1)
    
--Substance begins HERE.
    fail := false; --flag to escape if a CI op is not surjective    
    --Put the regular sequence and the factor rings into hash tables:
    --ci#i is the i-th element; R#i is codim i.
    c := numcols ff;
    S := ring ff;
    ci := hashTable apply(toList(1..c), 
	 p->{p,ff_{p-1}});--values are 1x1 matrices
    degs := hashTable apply(toList(1..c), 
	p->{p,(degree ci#p_0_0)_0});--values are ZZ
    R := hashTable apply(toList(0..c), 
	p->(if p==0 then {0,S}
	    else {p,S/ideal apply(toList(1..p), j->ci#(j))}));

--MAIN LOOP: work from p = c down to p = 1, creating the B1#p etc
    A#c = presentation M; --initialize
scan(reverse toList(1..c), p->(
    E = cosyzygyRes(2, coker A#p);	
    --sub into R#(p-1)
    A1#p = substitute (A#p, R#(p-1));
    scan(toList(1..3), i->E1#i = sub(E.dd_i,R#(p-1)));
    --define the ci operators proj#(p,j), A1#c --> B#c
    --and their kernels inc#(p,j) over R#(c-1).
    scan(2, j->(
	proj#(p,j) = map(R#(p-1)^{ -degs#p}**target E1#(j+1),
	                 source E1#(j+2),
			 E1#(j+1)*E1#(j+2)//((target E1#(j+1)**ci#p)));
        inc#(p,j) = syz proj#(p,j)
	));
    --if one of the proj#(p,j) is not surjective then
    --set fail = true and break from loop
    scan(2,j->
	if not isSurjective proj#(p,j) then(
	   << "CI operator not surjective at level codim " << c << endl;
	   << "on example M = coker "  << endl;
	   <<toString presentation M <<endl;
	   fail = true;
	   break;
	 ));
    if fail == true then break;
    --make the splittings to/from A1#p, over R#(p-1)
    scan(2, j-> (
         spl :=splittings(inc#(p,j),proj#(p,j));
         inc'#(p,j) = spl_0;
         proj'#(p,j) = spl_1));
   --make B1#p, A#(p-1), and
   --the map psi#p: source B1#p -> target A1#(p-1)
         B1#p = proj#(p,0)*A1#p*proj'#(p,1); -- B#p over R#(p-1)
         A#(p-1) = inc'#(p,0)*A1#p*inc#(p,1);
         psi#p = inc'#(p,0)*A1#p*proj'#(p,1);
));
--END OF MAIN LOOP
--Now put together the maps for output. All the work is done except
--for the creation of the homotopies.
    if fail == true then return("cannot complete MF");
    --lift all the relevant maps to S
    scan(toList(1..c), p-> (
	    BS#p = substitute(B1#p, S);
	    psiS#(p)= substitute(psi#p, S);
	    scan(2, j->(
	    projS#(p,j)= substitute(proj#(p,j), S);
	    inc'S#(p,j)= substitute(inc'#(p,j), S)
	        ))
	    ));
    --make psi(q,p):  BS#(q,0) <-- BS#(p,1) (note direction!)
    scan(toList(1..c), p->scan(toList(1..c), q->(
	    if q>p then psi#(q,p) = map(target BS#q,source BS#p, 0)
	    else if q == p then psi#(q,p) = BS#p
	    --if q< p then psi#(q,p) is a composition of
	    --a projection and a sequence of inclusions.
 	    else if q<p then( 
	     spl = psiS#p;
	     scan(reverse toList(q+1..p-1), j -> 
		 spl = inc'S#(j,0)*spl);
	     psi#(q,p) = projS#(q,0)*spl
	     )
    	    )));
    --construct the triangular differential d:Asour --> Atar, 
    --first as a list of lists of matrices
    Atar := directSum(apply(toList(1..c), p->target BS#p));
    Asour := directSum(apply(toList(1..c), p->source BS#p));    
    LL := apply(toList(1..c),
	       q->apply(toList(1..c), 
	       p->psi#(q,p)));
    d := map(Atar, Asour, matrix LL);

    --make homotopies h#p for ci#p on A1#p.
    --BUG: tensoring with R#(p-1) destroys the cache of components
    --of a direct sum, so
    --define dpartial#p over S, to be 
    --the restriction of d to the first p summands.
    scan(toList(1..c), p->(
    dpartial#p = map(
        target Atar^(toArray toList(0..p-1)),
        source Asour_(toArray toList(0..p-1)),
        Atar^(toArray toList(0..p-1))*
        d*
        Asour_(toArray toList(0..p-1)));
	       
    h#p = map(source dpartial#p, 
        S^{ -degs#p}**target dpartial#p,
        substitute(
        (R#(p-1)**(target dpartial#p**ci#p))//
                        (R#(p-1)**dpartial#p),
		   S));

--optionally check that dpartial and h have the right relationship
   if opts#Check==true then(
   if not isHomogeneous h#p 
         then error "homotopy not homogeneous";
   if 0 != R#(p-1)**dpartial#p*h#p - 
      R#(p-1)**(target dpartial#p)**ci#p
         then error "homotopy not good";
   if 0!= R#(p-1)**(target h#p)^[p-1]*h#p*dpartial#p- 
                 R#(p-1)**(target h#p)^[p-1]**ci#p
            then error "homotopy on B not good";   
                           )
    	));

--H = flatten apply(#h, p->(source d)h#p
--(source d)_[0,1]*H#2

Hhash := hashTable pairs h;
Hlist := apply(keys Hhash, i-> Hhash#i);
htar := target last Hlist;
Hlist1 := apply(#Hlist, m -> htar_(toArray toList(0..m))*Hlist_m);
h = Hlist1_0;
scan(#Hlist1 -1, i-> h=h|Hlist1_(i+1));
h = map(htar, directSum (Hlist1/source), h);

{d,h}
)

BRanks = method()
BRanks List := MF -> (
      B0 := (target MF_0).cache.components;
      B1 := (source MF_0).cache.components;
      apply(#B0, i-> {rank B0_i, rank B1_i}
      ))

ARanks = method()
ARanks List := MF -> (
      --list of pairs {rank A_0(p), rank A_1(p)} (=partial sums of the BRanks)
      B := BRanks MF;
      A := {B_0};
      scan(#B-1, i-> A = A|{B_(i+1)+last A});
      A)

--routines for taking apart d:
bMaps = method()
bMaps List := MF -> (
    	d := MF_0;
        apply(#BRanks MF, i-> (
	(target d)^[i]*d*(source d)_[i]))
        )

dMaps = method()
dMaps List := MF -> (
        d := MF_0;
        apply(#BRanks MF, i-> (
        (target d)^(toArray toList(0..i))*d*(source d)_(toArray toList(0..i))))
        )

psiMaps = method()
psiMaps List := MF -> (
        --psiMaps_p is the map B_1(p+1) -- A_0(p)
        d := MF_0;
        apply(#BRanks MF-1, i-> (
        (target d)^(toArray toList(0..i))*d*(source d)_(toArray {i+1})))
        )

hMaps = method()
hMaps List := mf-> (
    h := mf_1;
    apply(#(source h).cache.components, 
    p -> (target h)^(toArray toList(0..p))*h*(source h)_[p])
)
--Hlist == hMaps

ExtModuleData = method()
ExtModuleData Module := M -> (
     --Suppose that M is a module over a complete intersection R
     --of codim c, so that 
     --E := ExtModule M 
     --is a module generated in degrees >=0 
     --over a polynomial ring T 
     --generated in degree 2, and
     --E0 := evenExtModule M and 
     --E1 := oddExtModule M
     --are modules generated in degree >= 0
     -- over a polynomial ring T' with generators 
     --in degree 1.
     --
     --The script returns 
     --{E0,E1,reg0,reg1}
     --where regi = regularity Ei
     --and prints a message if reg0 != reg1 
     --If we set r = max(2*reg0, 1+2*reg1),
     --and F is a resolution of M, then 
     --coker F.dd_(r+1)
     --is the first szygy module of M such that
     --regularity evenExtModule M =0 AND
     --regularity oddExtModule M =0 
     --We have been using regularity ExtModule M 
     --as a substitute for r,
     --but that's not always the same.
     E := ExtModule M;
     P0 := positions(flatten degrees E, even);     
     P1 := positions(flatten degrees E, odd);
     E0':=prune image (E_P0);
     E1':=prune image (E_P1);     
     T' := ring E;
     kk:= coefficientRing T';
     X := symbol X;
     T := kk[X_0..X_(numgens T' -1)];
     v1 := map(T, T' ,vars T, DegreeMap => i->{(first i)//2});
     E0 := coker v1 presentation E0';
     E1 := coker v1 presentation E1';
     r0 := max(0, regularity E0);
     r1 := max(0, regularity E1);
     --I've temporarily commented out the following because
     --of the bug in Ext (12/29/12)
     if abs(r0-r1)>1 then (
	 <<"regularities of even and odd Ext modules differ by more than 1" <<endl;
	 <<"module with presentation matrix" <<endl;
	 <<toString presentation M);
     {E0,E1,r0,r1}
     )
///
restart
uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"
viewHelp ExtModuleData
viewHelp "CompleteIntersectionResolutions"

--something weird here: (it's a bug in Ext, I think.
--reported to Mike and Dan on April 9, 2013.
  The routine ExtModuleData also provides the
  regularity of each of these.
 Example
--in this case oddExtModule returns 0.
  c = 2
  S = ZZ/101[x_1..x_c, a_(1,1)..a_(c,c)];
  X = matrix{{x_1..x_c}}
  ff = X*map(source X,,genericMatrix(S,a_(1,1),c,c))
  R = S/ideal ff;
  betti res coker vars R
  evenExtModule coker vars R 
  oddExtModule coker vars R
  ExtModuleData coker vars R
--but not in this case:
  S = ZZ/101[s,t,u,v]  
  ff = matrix"su+tv"
  R = S/ideal ff
  betti res coker vars R
  ExtModuleData coker vars R
 
///


    
mfBound = method()
mfBound Module := M0 ->( 
    --gives (conjectural) bound for which map in the resolution
    --of M0 will have cokernel a high syzygy
E := ExtModuleData M0;
1+max(2*E_2, 1+2*E_3)
)

highSyzygy = method(Options=>{Optimism => 0})
highSyzygy Module := opts -> M0 ->(
    --with increment => 0 (the default) this gives our conjectural
    --bound, which is best possible.
    -- But if that's not good enough, use Optimism=>-1 etc
    len := mfBound M0-opts#Optimism;
    F := res(M0, LengthLimit => len);
    coker F.dd_len)

///%%

restart
loadPackage ("CompleteIntersectionResolutions", Reload=>true)
--viewHelp matrixFactorization
--Example 0
S = kk[a,b]
ff = matrix"ab"
R = S/ideal ff
M0 = R^1/ideal"a,b"
ExtModuleData M0
regularity ExtModule M0 -- 2
len = 2
F = res(M0, LengthLimit =>len)
--
MF = matrixFactorization(ff,coker F.dd_len)
MF = matrixFactorization(ff,coker F.dd_len,Check=>true)
MF = matrixFactorization(ff,highSyzygy M0)
betti MF_0
betti MF_1#1
MF_0*MF_1#1
BRanks MF

--Example 0a
S = kk[a,b,c]
ff = matrix"ac-b2"
R = S/ideal ff
m = matrix"a,b;b,c"
betti m
M0 = coker m
MF = matrixFactorization(ff,highSyzygy M0)
BRanks MF

--Example1
S = kk[a,b,u,v]
ff = matrix"au,bv"
R = S/ideal ff
M0 = R^1/ideal"a,b"
MF = matrixFactorization(ff,highSyzygy M0)
BRanks MF

--Example2
S = kk[a,b]
ff = (vars S)^[3]
R = S/ideal ff;
M0=R^1/ideal"ab" 
MF = matrixFactorization (ff, highSyzygy M0)
BRanks MF

--Example3
S = kk[a,b,c]
ff = matrix"a3,b3,c3"
betti ff
ff1 = ff*random(S^{3: -3}, S^{3: -3})
R = S/ideal ff; 
M0= R^1/ideal"ab"
MF = matrixFactorization (ff1, highSyzygy M0)
netList BRanks MF

--Example4
S = ZZ/101[a,b,c,d]
mm= ideal vars S
ff = (matrix"a3,b3,c3,d3")
ff1 = ff*random(source ff, source ff);
R = S/(ideal ff);
M0 = coker map(R^1, R^{-2,-3}, matrix"a2,bcd")
MF = matrixFactorization(ff1,highSyzygy M0);
netList BRanks MF

--Formerly bad example. Now seems fine
S = ZZ/32003[x_0..x_2]
f = matrix{{x_0^5, x_1^5, x_2^5}}
ff = f*random(source f, source f)
R = S/ideal f
m1 = {x_0^2*x_2^4, x_0*x_1^4*x_2}
M0 = R^1/ideal(m1_0+m1_1);
MF = matrixFactorization(ff, highSyzygy M0);
netList BRanks MF
///

finiteBettiNumbers = method()
finiteBettiNumbers List := MF -> (
    --MF should be the output of  matrixFactorization
    B := BRanks MF;
    c := #B;
     sourceRanks := B/last;
     targetRanks := B/first;
     apply(c+1, j->
           sum(1..c, 
	     i-> (targetRanks_(i-1)*binomial(i-1,j)+
		  sourceRanks_(i-1)*binomial(i-1,j-1))
	     ))
     )

infiniteBettiNumbers = method()
infiniteBettiNumbers (List,ZZ) := (MF,len) -> (
    --MF should be the output of  matrixFactorization
    B := BRanks MF;
    c := #B;
     sourceRanks := B/last;
     targetRanks := B/first;
     apply(len+1, j->
	 if j%2 ==0 then
           sum(1..c, 
	     i-> (targetRanks_(i-1)*binomial(c-i+j//2,c-i)))
	 else
           sum(1..c, 
	     i-> (sourceRanks_(i-1)*binomial(c-i+(j-1)//2,c-i)))
	     )
     )
     
///
restart
loadPackage("CompleteIntersectionResolutions", Reload => true)
uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"
viewHelp finiteBettiNumbers
///
    
--The following functions are used in makeHomotopies
expo = method()
expo(ZZ,ZZ) := (n,d) ->(
     --the next three lines define a function that returns
     --a list of all lists of n non-neg ints adding up to d.
     x:=local x;
     T := ZZ/2[x_0..x_(n-1)];
     flatten((flatten entries basis(d, T^1))/exponents)
     )

lessThan = (L1,L2) -> (
     --returns true if L1<L2 in the termwise partial order
     for i from 0 to #L1-1 do if L1#i>L2#i then return false;
     if L1==L2 then return false
     else return true)
    

expo(ZZ,List):= (n,L) ->(
     --returns the list of all elements of expo(n,d) that
     --are <L in the termwise partial order
     d := sum L;
     LL := flatten(for i from 0 to d-1 list expo(n,i));
     select(LL, M->lessThan(M,L))
     )

makeHomotopies = method()
makeHomotopies (Matrix, ChainComplex) := (f,F) ->
     makeHomotopies(f,F, max F)


     
makeHomotopies (Matrix, ChainComplex, ZZ) := (f,F,d) ->(
     --given a 1 x lenf matrix f and a chain complex 
     -- F_min <-...,
     --the script attempts to make a family of higher homotopies
     --on F for the elements of f.
     --The output is a hash table {{i,J}=>s), where
     --J is a list of non-negative integers, of length = ncols f
     --and s is a map F_i->F_(i+2|J|-1) satisfying the conditions
     --s_0 = d
     -- s_0s_{i}+s_{i}s_0 = f_i
     -- and, for each index list I with |I|<=d,
     -- sum s_J s_K = 0, when the sum is over all J+K = I
     H := new MutableHashTable;
     minF := min F;
     maxF := max F;
     if d>max F then d=maxF;
     flist := flatten entries f;
     lenf := #flist;
     e0 := (expo(lenf,0))_0;
     for i from minF to d+1 do H#{e0,i} = F.dd_i;

     e1 := expo(lenf,1);
     scan(#flist, j->H#{e1_j,minF-1}= map(F_minF, F_(minF-1), 0));
     for i from minF to d do
	       scan(#flist,
	       j->H#{e1_j,i}= (-H#{e1_j,i-1}*H#{e0,i}+flist_j*id_(F_i))//H#{e0,i+1}
	       );
     for k from 2 to d do(
	  e := expo(lenf,k);
	  apply(e, L ->(
	    k := sum L;
	    H#{L,minF-1}= map(F_(minF+2*k-2),F_(minF-1),0);
	    for i from minF to d-2*k+1 do
	      H#{L,i} = sum(expo(lenf,L), 
		 M->(H#{L-M,i+2*sum(M)-1}*H#{M,i}))//H#{e0,i+2*k-1};
	    )));
     hashTable pairs H
     )

makeHomotopies1 = method()
makeHomotopies1 (Matrix, ChainComplex) := (f,F) ->(
     makeHomotopies1 (f,F, length F))

makeHomotopies1 (Matrix, ChainComplex, ZZ) := (f,F,d) ->(
     --given a 1 x lenf matrix f and a chain complex 
     -- F_min <-...,
     --the script attempts to make a family of first homotopies
     --on F for the elements of f.
     --The output is a hash table {{J,i}=>s), where
     --J is an integer 0<= J < lenf, 
     --and s is a map F_i->F_(i+1) satisfying the conditions
     -- ds_{i}+s_{i}d = f_i
     H := new MutableHashTable;
     minF := min F;
     maxF := max F;
     if d>max F then d=maxF;
     flist := flatten entries f;
     rem := 0; -- no error yet.
     h := null;
     scan(#flist, j->H#{j,minF-1}= map(F_minF, F_(minF-1), 0));
     for i from minF to d do
	       scan(#flist, j->(
	       (h,rem) = 
	          quotientRemainder(-H#{j,i-1}*F.dd_i+flist_j, --*id_(F_i),
		                   F.dd_(i+1));
	       if rem != 0 then (
		     <<"homotopy " <<{j,i} <<" doesn't exist."<<endl;
		     --error()
		     );
	       H#{j,i} = h;    
	       ));
     hashTable pairs H
     )

///
restart
loadPackage("CompleteIntersectionResolutions", Reload =>true)
kk=ZZ/101
S = kk[a,b,c]
F = res ideal vars S  
f = matrix{{a,b,c}}
H = makeHomotopies1(f,F)
homot = makeHomotopies(f,F,2)
homot = makeHomotopies(f,F,1)
peek homot
netList select(keys homot, k->homot#k!=0)


f = matrix{{a^3,b^4}}
F= res (ideal vars S)
F = res ideal"a4,b2"
H = makeHomotopies1(f,F,3)
netList select(keys H, k->H#k!=0)
///


exteriorTorModule = method()
exteriorTorModule(Matrix, ChainComplex) := (f,F) -> (
     --Write Tor_S(M,k) as a module over Tor(S/(f),k) = \wedge V:
     --f is a matrix with entries that are homotopic to zero on F
     --Typically, F is a resolution of a module annihilated by
     --the entries of f.
     H := makeHomotopies1(f,F);
     flist := flatten entries f;
     lenf := #flist;
     k := coefficientRing ring F;
     T := toList apply(min F..max F,i->sub(F_i,k));
     Hk := hashTable apply(keys H, h-> (h, sub(H#h,k)));
     --Hk(j,i) is the homotopy for f_j from F_i**k to F_(i+1)**k,
     --defined for i from min F to max F-1.
     e := symbol e;
     E := k[e_0..e_(numcols f -1), SkewCommutative => true];
     TE :=makeModule(E,T,Hk);
     TE
)	  

exteriorExtModule = method()
exteriorExtModule(Matrix, ChainComplex) := (f,F) -> (
    --dual of exteriorTorModule
    TE := exteriorTorModule(f,F);
    Hom(TE, (ring TE)^1))

makeModule = method()
makeModule(Ring,List,HashTable) := (R,T,Hk) -> (
     --T is a list of free modules F_i over over
     --k =  coefficientRing R.
     --H is a HashTable with pairs of the form {j,i} => phi,
     --where phi: F_i\to F_(i+1).
     --The script takes R**\sum F_i as a graded R-module, 
     --where the j-th generator of R acts as H#{j,i}.
     k := coefficientRing R;
     pro := map(R,k);
     RT := apply(#T, i->pro(T_i)**R^{ -i});
     P := directSum RT;
     RHk := hashTable apply(keys Hk,ke ->(
	       i := last ke;
	       j := first ke;
	       (ke, map(RT_(i+1), RT_i**R^{ -1},pro Hk#{j,i})))
	       );
     M := P/sum apply(keys Hk,ke ->(
	       i := last ke;
	       j := first ke;
	       image(P_[i+1]*RHk#{j,i}-R_j*P_[i])));
     --prune M
     M
     )


exteriorTorModule(Matrix,Ring,Module,Module) := (ff,E,M,N) ->(
    --ff is a regular sequence in S that annihilates M and N
    --E is an exterior algebra on numcols ff generators
    --over S/J, where J is an ideal containing ideal ff
    --that annihilates Tor^S(M,N)
    --the script returns Tor^S(M,N) as an E-module.
    S := ring M;
    F := res M;
    complete F;
    H := for i from 0 to max F list HH_i(F**N);
    h := makeHomotopies1(ff,F);
    deglist := apply(flatten entries ff, g->degree g);
    e := hashTable apply(keys h, k -> {k, 
    map(H_(1+k_1), S^{ -deglist_(k_0)}**H_(k_1), 
	(matrix (h#k**N)//(generators H_(1+k_1)))*(generators H_(k_1)))}
    );
(H,e))
{*
error();
    M1 := makeModule1(E,H,e);
    --why does "prune M1" kill the grading??
    f := presentation M1;
    g := complement f;
    M2 := cokernel modulo(g, f);
--    (M2, M1)
    M2
     )
*}

 
makeModule1 = method()
makeModule1(Ring, List, HashTable) := (E,T,e)->(
    --in this version:
    --E is an algebra over R, generated by elements e_i, either commutative or
    --skew commutative, of bidegree{1,0};
    -- T is a list {T_0..T_m} of E-modules, which will become the homogeneous components
    -- of the output module,
    --e is a HashTable of homogeneous maps
    --e#{j,i}: T_i -> T_(i+1)
    --satisfying the commutative/skew-commutative conditions satisfied by the e_j.
    --Output is M = \oplus_i T_i
    --as an E-module.
    --this is obtained as the cokernel of the obvious map
    -- oplus_(i=1)^m T_(i-1)**E^{{-1,0}} --> oplus_(i=0)^m T_i
    
     m := 1+max(0,max ((keys e)/last)); -- there are m+1 modules T_0..T_m
     if m == 1 then return E**T_0; -- handles case of only 1 module
     flength := #unique apply(keys e, k->k_0); -- number of E-variables involved
     --promote e#{j,i} to a bi-homogeneous map eE#{j,i}: $T_i(-i) \to T_{i+1}(-i-1)$ over E.
     eE := hashTable apply(keys e, ke ->(
       	       (ke,map(E^{{ -ke_1-1, 0}}**(E**T_(ke_1+1)), 
		       E^{{ -ke_1-1, 0}}**(E**T_(ke_1)), 
		       (E**e#ke)))
	       ));
     fir := new Array from 0..m-1;
     las := new Array from 1..m;
     P := directSum apply(m+1, i-> if i==0 then 
	          T_0**E else target eE#{0,i-1});
--     directSum0 := L -> if #L==0 then L_0 else directSum L;
     Q := apply(flength, j-> 
	  directSum apply(m, i-> source(eE#{j,i})));
--     P := T_0**E++directSum apply(m, i->target eE#{0,i}); -- puts T_i in first-degree i
     --P_[1] is the inclusion of T_1++..++T_m     
     f := apply(flength, j -> 
	 map(P, Q_j, 
	 P_las*directSum apply(m, i->eE#{j,i})
		 ));
     g := apply(flength, j ->
	  map(P,Q_j, 
	      P_fir*(E^{{1,0}}**(E_j**id_(Q_j)))
		  ));
--     g := apply(flength, j ->map(P,Q_j, E_j**id_(Q_j)||matrix map(T_0**E,Q_j,0)));

     M := P/sum(apply (flength, j->image(f_j-g_j)));
     error();
     --prune M
     M
     )
  
///
--1-var
restart
loadPackage( "CompleteIntersectionResolutions", Reload => true)
--viewHelp CompleteIntersectionResolutions
S = ZZ/101[a,b,c]
E = S[x]
T = {S^1,S^1}
e = hashTable{({0,0},map(T_1,T_0,1))}
X = makeModule1(E,T,e)
isHomogeneous X
prune X


kk=ZZ/101
S = kk[a,b,c]

f = matrix{{a^2,b^2}}
R = S/ideal f
red = map(R,S)
F = res (ideal vars R, LengthLimit => 7)
complete F
M = apply(7, i-> coker F.dd_(i+1));
MS = M/(Mi -> pushForward(red, Mi));
E = R[x,y, SkewCommutative=>true]
(H,e) = exteriorTorModule(f, E, MS_4, MS_4);
M1 = makeModule1(E,H,e);


apply(keys e, ke ->(
       	       (ke,map(E^{{ -ke_1-1, 0}}**(E**H_(ke_1+1)), 
		       E^{{ -ke_1-1, 0}}**(E**H_(ke_1)), 
		       (E**e#ke)))
	       ))

H = {cokernel matrix {{0, 0, 0, 0, 0, c, b, a, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, c, b, a, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, c, b, a}}, cokernel matrix {{0, 0, c, b, a, 0, 0, 0,
       0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, c, b, a, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, c, b, a, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, c, b, a, 0, 0, 0}, {0, 0,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, c, b, a}}, cokernel matrix {{c, b, a, 0, 0, 0}, {0, 0, 0, c, b, a}}, 0}

e = new HashTable from {{1, -1} => matrix {{}, {}, {}}, {1, 0} => matrix {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}}, {1, 1} => matrix {{0, 0, 0, 0, 0}, {-1, 0, 0, 0, 0}}, {1, 2} =>
       matrix {}, {0, -1} => matrix {{}, {}, {}}, {0, 0} => matrix {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}}, {0, 1} => matrix {{0, 0, 0, 0, 0}, {0, 0, 0, 1, 0}}, {0, 2} => matrix {}}
e
H/(h->betti presentation h)
makeModule1(E,H,e)

exteriorTorModule(f, E, MS_0, S^{1}**MS_1)




for i from 0 to 7 do
    for j from i to 3 do
        print betti res exteriorTorModule(f, E, MS_i, MS_j)


Pt = directSum(apply(3, j-> F_(j+1)))
viewHelp directSum
viewHelp Array
new Array from 0..2
[1..2]
PtFirst = Pt_
///




TEST///
kk=ZZ/101
R = kk[a,b]
E=R[f,g, SkewCommutative=>true]
///

isLinear = method()
isLinear(Matrix) := phi ->(
     L := (flatten entries phi)/degree;
     flag := true;
     scan(flatten L, ell-> if ell>1 then (flag = false;break));
     flag)


freeExteriorSummand = method()
freeExteriorSummand(Module) := M -> (
     --M should be a module over an exterior algebra E.
     --script finds a basis of M/(ann_M soc E).
     E := ring M;
     mm := ideal vars E;
     soc := (ideal 0_E):mm;
     nongens := (0_E*M):soc;
     freegens := (basis (M/nongens))//inducedMap(M/nongens,M)
     )
///
restart
uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"
viewHelp "CompleteIntersectionResolutions"
///
///
restart
loadPackage( "CompleteIntersectionResolutions", Reload => true)
kk= ZZ/101
E = kk[e,f,g, SkewCommutative => true]
M = E^1++module ideal vars E++E^{-1}
freeExteriorSummand M


kk=ZZ/101
S = kk[a,b,c]
F = res (ideal vars S)
f = matrix{{a,b,c}}
H = makeHomotopies1(f,F)
tor = exteriorTorModule(f,F)

betti tor
betti prune tor

f = gens ideal"a3,b3,c3"
R = S/ideal f
M = R^1/(ideal"abc,a2b")
betti (FF =res( M, LengthLimit =>8))
p = map(R,S)
MS = prune pushForward(p, coker FF.dd_3)
betti(F = res MS)
T = exteriorTorModule(f,F);
betti T
betti (PT = prune T)
phi = presentation PT
isLinear phi

M= coker random(R^2, R^{3:-1})
betti (FF =res( M, LengthLimit =>10))
p = map(R,S)
MS = prune pushForward(p, coker FF.dd_4);
--the pruned presentation of tor starts with FF.dd_5
betti(F = res MS)
T = exteriorTorModule(f,F);
betti T
betti (PT = prune T)
phi = presentation PT;
isLinear phi
submatrixByDegrees(phi,{0},{2})

S = kk[a,b,c,d]
f = (vars S)^[3]
R = S/ideal f
p = map(R,S)
--M= coker random(R^2, R^{3:-1}) -- too hard!
M = R^1/ideal"ab+cd+b2+d2,abc"
M = coker random(R^1, R^{-2})
time betti (FF =res( M, LengthLimit =>6))
MS = prune pushForward(p, coker FF.dd_4);
--the pruned presentation of tor starts with FF.dd_5
betti(F = res MS)
time T = exteriorTorModule(f,F);
betti T
betti (PT = prune T)
phi = presentation PT;
isLinear phi
submatrixByDegrees(phi,{0},{2})
betti res PT
S = kk[a,b,c,d]
f = (vars S)^[4]
R = S/ideal f
p = map(R,S)
--M= coker random(R^2, R^{3:-1}) -- too hard!
M = R^1/ideal"a2b+bc2,ac+b2,c3d3"
--M = coker random(R^1, R^{-2})
time betti (FF =res( M, LengthLimit =>6))
MS = prune pushForward(p, coker FF.dd_4);
--the pruned presentation of tor starts with FF.dd_5
betti(F = res MS)
time T = exteriorTorModule(f,F);
betti T
betti (PT = prune T)
phi = presentation PT;
isLinear phi
submatrixByDegrees(phi,{0},{2})

///

--load "HomMatrixModule.m2" -- still necessary in 1.5?


S2 = method()
S2(ZZ,Module) := Matrix => (b,M)-> (
     --returns a map M --> M', where M' = \oplus_d>=b H^0(\tilde M).
     --the map is equal to the S2-ification AT LEAST in degrees >=b.
     S := ring M;
     r:= regularity M;
     if b>r+1 then return id_(truncate(b,M));
     tbasis := basis(r+1-b,S^1); --(vars S)^[r-b];
     t := map(S^1, module ideal tbasis, tbasis);
     s:=Hom(t,M)
     --could truncate source and target; but if we do it with
     --the following line then we get subquotients AND AN ERROR!
--     inducedMap(truncate(b,target s),truncate(b,source s),s)
     )

///
restart
loadPackage "CompleteIntersectionResolutions"
S = kk[a,b,c]
M = truncate(3,S^1)
f = inducedMap(S^1,M)
hf(0..10,coker S2(0,M))
betti S2(1,M)


S = kk[a,b,c,d]
M = S^1/intersect(ideal"a,b,c", ideal"b,c,d",ideal"c,d,a",ideal"d,a,b")
prune source S2(0,M)
prune target S2(0,M)
hf(-5..5,coker S2(-5,M))


S = kk[a,b,c,d]
M = truncate(3,S)
betti S2(0,M)
betti S2(1,M)
M = S^1/intersect(ideal"a,b,c", ideal"b,c,d",ideal"c,d,a",ideal"d,a,b")
S2(0,M)

S = kk[a,b]
M = coker map(S^2, S^{2:-1}, matrix"a,b;0,0")
betti S2(-1,M) -- this is wrong (also with 0)

S=kk[a,b,c]
I = ideal (vars S)^[3]
f = map(S^1,module I,(vars S)^[3])
matrix f
///



TateResolution = method()
TateResolution(Module,ZZ,ZZ) := (M,lower,upper) ->(
    d := transpose (res(M, LengthLimit => upper)).dd_upper;
    betti res (coker d, LengthLimit =>upper+lower)
    )
TateResolution(Module,ZZ) := (M,b) -> TateResolution(M,b,b)
TateResolution(Module) := M-> TateResolution(M,5,5)
------------
--special purpose code
--

----- look for small examples
--This code searches for pairs of monomials of degree d
-- in the given ring (mod the c d-th powers of the variables) 
--of complexity c (that is,
--dim Ext(R/(m1, m2),k)=c), and tallies the sequence of B ranks
--for a high syzygy of R/(m1,m2).

--Conclusion: in every one of these cases, the sequences
--{rank target B(i)} and {rank source B(i)} are *strictly increasing
--for i = 2..4 (and weakly increasing between i = 1,2.)
--also, the multiplicity is always 8, 12 or 17.
twoMonomials = method(Options => {Optimism => 0})
twoMonomials(ZZ,ZZ) := opts-> (c,d)->(
Blist := null;
M0:=null;
MF:=null;
B:= null;
x := symbol x;
S := ZZ/101[x_0..x_(c-1)];
f := map(S^1, S^{c: -d}, (i,j) -> x_j^d);
ff := f*random(source f, source f);
R := S/ideal f;
L := flatten entries basis R;
for e from 2 to c*(d-1) do(
L1 := select(L, m -> (degree m)_0 == e);
--make all pairs
pL1 :=(unique flatten apply(L1, i-> apply(L1, j-> set(i,j))))/toList;
time Blist = apply(pL1, m -> (
	M0 = R^1/ideal m;
	--<< m << endl;
    	MF = matrixFactorization(ff, highSyzygy(opts, M0));
	B = BRanks MF;
	scan(c-1, j-> (
		if last B_(j+1)-last(B_j)<0 then(
		print m;
		error();
		)));
	if B_0 != {0,0} then
	{B,toList m}
	else null
	)
    );
Blist = select(Blist, i-> i=!=null);
<< e <<endl;
<< tally(Blist/(k->k_0))<<endl;<<endl;
<<flush
)
)


--sumtwoMonomials(c,d)
--tallies the sequences of B-ranks that occur for sums of pairs of 
--monomials in R = S/(d-th powers of the variables), with
--full complexity (=c); that is,
--for an appropriate syzygy M of 
--M0 = R/(two monomials of the same degree)
sumTwoMonomials = method()
sumTwoMonomials(ZZ,ZZ) := (c,d) ->(
Blist := null;
M0:=null;
MF:=null;
B:= null;
x := symbol x;
S := ZZ/32003[x_0..x_(c-1)];
f := map(S^1, S^{c: -d}, (i,j) -> x_j^d);
ff := f*random(source f, source f);
R := S/ideal f;
L := flatten entries basis R;
for e from 2 to c*(d-1) do(
--make all pairs
L1 := select(L, m -> (degree m)_0 == e);
pL1 :=(unique flatten apply(L1, i-> apply(L1, j-> set(i,j))))/toList;
ppL1 := select(pL1, j->#j == 2);
time Blist = apply(ppL1, m -> (
	M0 = R^1/ideal(m_0+m_1);	
	--<< m << endl;
    	MF = matrixFactorization(ff, highSyzygy M0);
	B = BRanks MF;
	scan(c-1, j-> (
		if last B_(j+1)-last(B_j)<0 then(
		print m;
		error("example of decreasing source ranks");
		)));
	if B_0 != {0,0} then
	{B,toList m}
	else null
	)
    );

Blist = select(Blist, i-> i=!=null);
<< e <<endl;
<< tally(Blist/(k->k_0))<<endl;<<endl;
)
)


--helper routines for moduleAsExt

hfModuleAsExt = method()
hfModuleAsExt(ZZ, Module,ZZ) := (m,M,n) ->(
    --compute what should be the total Betti numbers
    --in the resolution of the module whose ext module
    --is the  tensor product of the 
    --exterior algebra on numgens ring M generators
    --with M. Give m values (starting from 
    --numgens ring M + reg M).
    rr := ring M;
    reg := regularity M;
    MM := minimalPresentation truncate(reg, M)**rr^{reg};
    h := apply(m+n+1, j->hilbertFunction(j,MM));
    n1 := if even n then n else n+1;
    apply(n1//2+1..m,i-> if even(i) then
	    sum(n1//2+1, j->binomial(n,2*j)*h_(i//2-j))
	         else
	    sum(n1//2, j->binomial(n,2*j+1)*h_((i-1)//2-j)))
    )

insertT = method()
insertT (Matrix, List) := (phi,Ti) -> (
    	--phi is a matrix of linear forms in vars x_1..x_c.
    	--Ti is a list of lists, each a
	--1-rowed matrix of scalars over R
	--corresonding to one of the x_i.
	--replace each entry of phi by the 
	--appropriate linear combination of these matrices 
	--and the output is a matrix of scalars over R.
	R := ring Ti_0;
	v := vars ring phi;
	n := numgens ring phi;
	L := entries phi; -- list of lists of lin forms in Ops
        matrix apply(#L, i -> 
	    apply(#L_i,
		j-> sum(n, k->(sub(diff(v, L_i_j),R))_{k}**Ti_k)))
	);

moduleAsExt = method()
moduleAsExt(Module,Ring) := (M,R) ->(
    --caveat: if the CI defining R has gens of varying degrees, then
    --the result of this routine would not be homogeneous. The script will
    --return an error. However, we could deal with the local case -- saved for the future.
    --the problem is that the CI operators have different internal
    --degrees, so when we replace a matrix of linear forms over Ops with the 
    --CI operators over R, it may not be possible to make the degrees consistent.
    n := numgens R;
    rr := ring M;
    c := numgens rr;
    ff := presentation R;
    -- get degree d of first entry of the 
    -- complete intersection defining rr
    d := (degrees source ff)_0_0; 
    scan(flatten (degrees ff)_1, 
        e -> if e !=d then 
	error"all degrees of the CI need to be equal");
    reg := regularity M;
    --truncate M at the regularity to get MM
    MM := minimalPresentation truncate(1+reg, M)**rr^{reg};
    F := res MM;
    m := length F;
    --now prepare the CI operators as maps of resolutions
    K := res(coker vars R, LengthLimit => m+1);
    T1 := apply (m, i->makeT(ff, K, 2+i));
    T := apply (m, i->apply(c, j-> 
	    map(K_i,
		R^{d}**K_(i+2), 
		T1_i_j)));
    --T_i_j: is the matrix of of the CI map K_(2+i) \to K_i
    --corresponding to ff_j.
    V := apply(m+1, i->R^(rank F_i));
    tar := directSum apply(m+1, i->(
	    R^{ d*(m-i)}**V_i**K_(m-i)));
    sour := directSum apply(m+1, i->(
	    R^{ d*(m-i)}**V_i**K_(m-i+1)));
    --the part of the differential made from the differential of K
    d1 := sum apply(m+1, i-> 
	(tar_[i])*(R^{ d*(m-i)}**V_i**K.dd_(m-i+1))*(sour^[i]));
    --the part made by substituting T's for vars of rr
    phi := symbol phi;	
    psi := symbol psi;
    gamma := symbol gamma;    
    gamma1 := symbol gamma1;    
    d2 := sum apply(m, i->(
	    phi = transpose (F.dd_(i+1));
	    gamma1 = insertT (phi,T_(m-i-1));
	    gamma = map(R^{d*(m-i-1)}**V_(i+1)**(K_(m-i-1)),
		    R^{d*(m-i)}**V_i**(K_(m-i+1)),
		    gamma1);
	    psi = gamma;
    tar_[i+1] * psi * sour^[i]));
    assert isHomogeneous(d1+d2);
    prune coker(d1+d2))

///
restart
loadPackage ("CompleteIntersectionResolutions", Reload=>true)
n=3;c=3;
  kk = ZZ/101;
  S = kk[a_0..a_(n-1)]
  ff = ((vars S)_{0..c-1})^[3];--matrix{{a^3, b^3}};
  R = S/ideal ff;
--  Ops = kk[x_1,x_2,x_3,Degrees =>{3:{2}}]
  Ops = kk[x_1..x_c]

  MM = Ops^1/ideal(x_1)    
  MM = Ops^1/ideal(x_1^2)  
  MM = Ops^1/ideal(x_1^3)    
  MM = Ops^1
  MM = Ops^1/ideal(x_2,x_3)
  MM = Ops^1/(x_1*ideal(x_2,x_3))
  MM = Ops^1/ideal(x_1,x_2,x_3^5)
  N =  moduleAsExt(MM,R)
  betti res( N, LengthLimit => 10)
  hfModuleAsExt(10,MM,n)
  ///
  
koszulExtension = method()
koszulExtension(ChainComplex,ChainComplex,Matrix,Matrix) := (FF, BB, psi1, ff) ->(
    --with BB a two-term complex B_1-->B_0 and FF a resolution
    --of a module annihilated by ideal ff, and
    --psi1: B_1-->F_0,
    --the script produces the extension map
    --KK(ff)**B[1] --> F
    --and returns the cone on this map.
    S := ring ff;
    KK := koszul ff;
    --first make the Koszul extension from the complex BB1: FF_0 --> 0
    BB1 := chainComplex map(S^0,FF_0,0);
    phi11 := map (FF_0, (KK**BB1)_1,id_(FF_0)|map(FF_0,KK_1**BB1_0,0));
    psi11 := extend(FF, KK**BB1[1], id_(FF_0));
    --then compose with KK tensored with the map BB[1] --> BB1[1]
    phi := map(BB1[1],BB[1],i-> (if i ==-1 then 0;if i==0 then psi1));
    psi := psi11*(KK**phi);
    cone psi
    )

makeFiniteResolution = method()
makeFiniteResolution(List,Matrix) := (MF,ff) -> (
    S := ring MF_0;
    B := bMaps MF;
    psi := psiMaps MF;
    c' := complexity MF; -- the complexity
    c := rank source ff; -- the codim
    R := S/ideal(ff_{0..(c-c'-1)});
    toR := map(R,S);
      --ring over which the finite resolution first occurs.
    A := chainComplex toR B_(c-c');
    scan(c'-1, p -> 
     A = koszulExtension(
      A,chainComplex toR B_(c-c'+p+1), toR psi_(c-c'+p), toR ff_{(c-c')..(c-c'+p)}));
    scan(length A-1, i-> if( prune HH_(i+1) A) != 0 then error"A not acyclic");
    A
    )

complexity = method()
--complexity of a module over a CI ring
complexity Module := M-> dim evenExtModule M
complexity List := mf -> (
    br := BRanks mf;
    L := select(br, pair->pair_0!=0);
    #L)
///
restart
loadPackage ("CompleteIntersectionResolutions", Reload=>true)
toArray = method()
toArray List := L -> splice [toSequence L]
toArray ZZ := n->[n]

S = ZZ/101[a,b,c]
ff = matrix"a3,b3"
R = S/ideal ff
M = highSyzygy (R^1/ideal vars R)
mf = matrixFactorization (ff, M)
G = makeFiniteResolution(mf,ff)
F = res pushForward(map(R,S),M)
G.dd_1
F.dd_1
G.dd_2
F.dd_2

///


-----------------------------
--------Tests-----------
--------------------------------
{*
	   splittings,
	   --things related to complete intersection resolutions
	   ExtModule, 
	   evenExtModule, 
	   oddExtModule,
	   ExtModuleData,
	   makeT,
--	   isSurjCIOperator,
--	   splitResolution,
--	   decomposeResolution,
	   cosyzygyRes,	  
	   matrixFactorization,
	   Check, -- optional arg for matrixFactorization
	   BRanks,
	   ARanks,
	   bMaps,
	   dMaps,
	   psiMaps,
	   hMaps,
	   mfBound,
	   highSyzygy,
	   Optimism, -- optional arg for highSyzygy etc
	   finiteBettiNumbers,
           infiniteBettiNumbers,
	   makeHomotopies,
	   makeHomotopies1,
           exteriorTorModule,
           exteriorExtModule,	   
	   TateResolution,
	   makeModule,
	   isLinear,
	   freeExteriorSummand,
*	   S2,
	   twoMonomials,
	   sumTwoMonomials,
	   moduleAsExt,
	   hfModuleAsExt,
	   koszulExtension,
	   makeFiniteResolution,	   
	   complexity,
	   extVsCohomology,
	   BGGL,
	   hf,
	   isQuasiRegular
*}
TEST ///--of S2
S = ZZ/101[a,b,c];
M = S^1/intersect(ideal"a,b", ideal"b,c",ideal"c,a");
--assert( (hf(-7..1,coker S2(-5,M))) === (0, 3, 3, 3, 3, 3, 3, 2, 0))
assert( (betti prune S2(-5,M)) === new BettiTally from {(0,{-6},-6) => 3, (1,{0},0) => 1} )
///


TEST/// --test of splittings
setRandomSeed 100
kk= ZZ/101
S = kk[x,y,z]
t = random(S^{2:-1,2:-2}, S^{3:-1,4:-2})
(A,B) = (syz t, t);
ss = splittings(A,B);
sigma = ss_0;
tau = ss_1;
     assert(A*sigma+tau*B == id_(source B));
     assert(sigma*tau==0);
     assert(B*tau == id_(target B));
     assert(sigma*A == id_(source A));
(a,b) = (transpose ss_0, transpose ss_1);
ss=splittings(a,b);
sigma = ss_0;
tau = ss_1;
     assert(a*sigma+tau*b == id_(source b));
     assert(sigma*tau==0);
     assert(b*tau == id_(target b));
     assert(sigma*a == id_(source a));
///

///
--setRandomSeed 100
S = ZZ/101[a,b,c]
ff = (vars S)^[3]
R = S/ideal ff; 
M= R^1/ideal"ab"
FF = res cosyzygyRes(3,M)
betti FF
betti (cosyzygyRes(3,coker FF.dd_2))

///

///TEST -- of ExtModule, evenExtModule, oddExtModule, ExtModuleData
  kk = ZZ/101
  S = kk[a,b,c]
  R = S/ideal"a2,b3,c4"
  M = R^1/ideal"a,b,c"
assert ((rank ExtModule M ==8) and  (isFreeModule ExtModule M))
assert (rank evenExtModule M == 4)
assert (rank oddExtModule M == 4)
E = ExtModuleData M
assert (rank E_0 == 4 and rank E_1 == 4 and E_2==1 and E_3==1)
///

///TEST
     S = ZZ/101[x,y,z];
     ff = matrix"x3,y3,z3";
     R = S/ideal ff;
     M = coker matrix"x,y,z;y,z,x";
     betti (F = res M)
assert( (makeT(ff,F,3)) === {map(R^{{-4},{-4},{-4}},R^{{-4},{-4},{-4},{-4},{-4},{-4}},{{0, 0, 0, 0, 1,
      --------------------------------------------------------------------------------------------------------
      0}, {0, 0, 0, -1, 0, 0}, {0, 0, 0, 0, 0,
      --------------------------------------------------------------------------------------------------------
      1}}),map(R^{{-4},{-4},{-4}},R^{{-4},{-4},{-4},{-4},{-4},{-4}},{{0, 1, 0, 0, 0, 0}, {1, 0, 0, 0, 0, 0},
      --------------------------------------------------------------------------------------------------------
      {0, 0, 1, 0, 0, 0}}),map(R^{{-4},{-4},{-4}},R^{{-4},{-4},{-4},{-4},{-4},{-4}},{{0, -1, 0, 0, -1, 0},
      --------------------------------------------------------------------------------------------------------
      {-1, 0, 0, 1, 0, 0}, {0, 0, -1, 0, 0, -1}})} );     
///

///TEST
  setRandomSeed 0
  S = ZZ/101[a,b,c,d]
  ff1 = matrix"a3,b3,c3,d3"
  ff =ff1*random(source ff1, source ff1)
  R = S/ideal ff
  M = highSyzygy (R^1/ideal"a2b2")
  assert(complexity M==2)
  mf = matrixFactorization (ff, M)
  assert(complexity mf ==2)
  assert(BRanks mf == {{0, 0}, {0, 0}, {2, 2}, {1, 2}})
  G = makeFiniteResolution(mf,ff);
  F = res prune pushForward(map(R,R1),M);
  assert(betti F ==  betti G)
///

///
restart
loadPackage"CompleteIntersectionResolutions"
check "CompleteIntersectionResolutions"

uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"

///

///
restart
E = ZZ/101[a,b,c,d, SkewCommutative => true]
P = E^1/ideal(a*b,c)
betti res P
hf(0..3, P)
S = ZZ/101[x,y,z,w]
betti BGGL(P,S)

///
BGGL = (P,S) ->(
    --given an exterior module P, 
    --returns the linear complex L(P)
    E := ring P;
    fake := map(S,E,vars S);
    B := basis P;
    dlist := flatten degrees source B;
    dmin := min dlist;
    dmax := max dlist;
    Blist := apply(toList(dmin..dmax), i-> basis(i,P));
    maplist1 := apply(dmax-dmin,j->
	fake(((Blist_j)**vars E)//Blist_(j+1))*(sub(source Blist_j,S)** 
	         transpose vars S
		 )
	);
    maplist := apply(dmax-dmin, i-> 
	map(S^{rank source Blist_(i+1):i+1},
	    S^{rank source Blist_i:i},
	   maplist1_i));
    (chainComplex reverse maplist)[dmax]**S^{dmin}
    )

extVsCohomology = method()
extVsCohomology(Matrix, Module) := (ff,N) ->(
    --N is an R=S/(ff)-module
    --M is a high syzy of N
    --compares the coho tables of the even and odd parts of Ext(M,k)
    --with the tate resolution of Ext_S(M,k) as a module
    --over the exterior alg.
    S:= ring ff;
    p := map(ring N,S);
    M := N; --highSyzygy N;
    MS := pushForward(p,M);
    Ee := evenExtModule M;
    Eo := oddExtModule M;
    exter := ring Ee;
    E := exteriorExtModule(ff,res MS);
    T := exteriorTorModule(ff,res MS);
TE := (betti (S^{-5})[6])**TateResolution(E,5,5);
TEe := (cohomologyTable(presentation (Ee), ring E,-5,5));	
TEo:= cohomologyTable(presentation (Eo), ring E,-5,5);
    <<"Tate Resolution of Ext_S(M,k) as exterior module:"<<endl;
    <<"Note that maps go left to right"<<endl;
    
    <<TE<<endl;
        <<"---"<<endl;
    <<"Cohomology table of evenExtModule M:"<<endl;
    <<TEe<<endl;
        <<"---"<<endl;
    <<"Cohomology table of oddExtModule M:"<<endl;	
    <<TEo<<endl;
    (E,T))

doc ///
   Key
    extVsCohomology
    (extVsCohomology, Matrix, Module)
   Headline
    compares Ext_S(M,k) as exterior module with coh table of sheaf Ext_R(M,k)
   Usage
    (E,T) = extVsCohomology(ff,N)
   Inputs
    ff:Matrix
     regular sequence in a regular ring S
    N:Module
     graded module over R = S/ideal(ff) (usually a high syzygy)
   Outputs
    E:Module
    T:Module
     Ext and Tor as exterior modules
   Description
    Text
     Given a matrix ff containing a regular sequence in a polynomial ring S over k,
     set R = S/(ideal ff). If N is a graded R-module, and M is the module N regarded
     as an S-module, the script returns E = Ext_S(M,k) and T = Tor^S(M,k)
     as modules over an exterior algebra.
     
     The script prints the Tate resolution of E; and the cohomology table of the sheaf associated
     to Ext_R(N,k) over the ring of CI operators, which is a polynomial ring over k on c variables.
     
     The output can be used to (sometimes) check whether
     the submodule of Ext_S(M,k) generated in degree 0 splits (as an exterior module
    Example
     S = ZZ/101[a,b,c]
     ff = matrix "a2,b2,c2"
     R = S/(ideal ff)
     N = highSyzygy(R^1/ideal(a*b,c))
     E = extVsCohomology(ff,highSyzygy N);
   SeeAlso
    
///

-----------------------------
--------Documentation-----------
--------------------------------
--
--<<docTemplate

{*
restart
loadPackage ("CompleteIntersectionResolutions", Reload=>true)
uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"
check "CompleteIntersectionResolutions"
viewHelp CompleteIntersectionResolutions
viewHelp makeFiniteResolution
viewHelp complexity
*}

doc ///
Key 
 makeFiniteResolution
 (makeFiniteResolution, List, Matrix)
Headline
 finite resolution of a matrix factorization module M
Usage
 A = makeFiniteResolution(mf,ff)
Inputs
 mf:List
   output of matrixFactorization
 ff:Matrix
   the regular sequence used for the matrixFactorization computation
Outputs
 A:ChainComplex
   A is the minimal finite resolution of M over R.
Description
 Text
  Suppose that f_1..f_c is a homogeneous regular sequence 
  of forms of the same degree in a 
  polynomial ring S and M is a high syzygy module over
  S/(f_1,..,f_c) = R(c), and mf = (d,h) is the output
  of matrixFactorization(M,ff). If the complexity of M
  is c', then M has a finite free resolution over
  R = S/(f_1,..,f_{(c-c')}) (and, more generally, has
  complexity c-d over S/(f_1,..,f_{(c-d)}) for d>=c').

  The complex A is the minimal finite free resolution 
  of M over A, constructed as an iterated Koszul extension,
  made from the maps in bMaps mf and psiMaps mf, as described
  in Eisenbud-Peeva.
 Example
  setRandomSeed 0
  S = ZZ/101[a,b,c];
  ff = matrix"a3,b3";
  R = S/ideal ff;
  M = highSyzygy (R^1/ideal vars R);
  mf = matrixFactorization (ff, M)
  G = makeFiniteResolution(mf,ff)
  F = res pushForward(map(R,S),M)
  G.dd_1
  F.dd_1
  G.dd_2
  F.dd_2
 Text
  If the complexity of M is not maximal, then the finite
  resolution takes place over an intermediate complete
  intersection:
 Example
  S = ZZ/101[a,b,c,d]
  ff1 = matrix"a3,b3,c3,d3"
  ff =ff1*random(source ff1, source ff1)
  R = S/ideal ff
  M = highSyzygy (R^1/ideal"a2b2")
  complexity M
  mf = matrixFactorization (ff, M)
  complexity mf
  BRanks mf
  G = makeFiniteResolution(mf,ff);
  codim ring G
  R1 = ring G
  F = res prune pushForward(map(R,R1),M)
  betti F
  betti G

SeeAlso
 matrixFactorization
 bMaps
 psiMaps
 complexity
///

doc ///
Key
 complexity
 (complexity, Module)
 (complexity, List)
Headline
 complexity of a module over a complete intersection
Usage
 c = complexity M
 c = complexity mf
Inputs
 M:Module
  module over a complete intersection
 mf:List
  output of a matrix factorization computation
Outputs
 c:ZZ
  1+dimension of Ext(M,k) over the ring of CI operators
Description
 Text
  The minimal resolution of a module over a complete intersection has betti numbers
  that grow as a polynomial of degree at most equal to the codimension-1.
  The complexity is one more than the degree of this polynomial.
 Example
  S = ZZ/101[a,b,c,d];
  ff1 = matrix"a3,b3,c3,d3";
  ff =ff1*random(source ff1, source ff1);
  R = S/ideal ff;
  M = highSyzygy (R^1/ideal"a2b2");
  complexity M
  mf = matrixFactorization (ff, M)
  complexity mf
  betti res (R^1/ideal"a2b2", LengthLimit=>10)
SeeAlso
 matrixFactorization
 makeFiniteResolution
     ///


doc ///
Key 
 koszulExtension
 (koszulExtension, ChainComplex,ChainComplex,Matrix,Matrix)
Headline 
 creates the Koszul extension complex of a map
Usage 
 MM = koszulExtension(FF,BB,psi1,ff)
Inputs 
 FF:ChainComplex
  resolution over S
 BB:ChainComplex
  two-term complex BB_1-->BB_0
 psi1:Matrix
  from BB_1 to FF_0
 ff:Matrix
  regular sequence annihilating the module resolved by FF
Outputs
 MM:ChainComplex
  the mapping cone of the induced map B[-1]\otimes KK(ff) to W extending psi
Description
 Text
  Implements the construction in the paper
  "Matrix Factorizations in Higher Codimension"
  by Eisenbud and Peeva.  
 Example
    
SeeAlso
 makeFiniteResolution
///

doc///
Key
  CompleteIntersectionResolutions
Headline 
  "Resolution over a Complete Intersection"
Description 
 Text
  The resolution of a module over a hypersurface ring 
  (graded or local) is always periodic of period at most 2 (Eisenbud, "Homological Algebra Over A Complete Intersection",
  Trans. Am. Math. Soc. 260 (1980) 35--64),
  but the asymptotic structure of minimal resolutions over a 
  complete intersection is a topic of active research. 
  This package contains code that helps analyze (graded) examples. 
  
  Most of our routines work with a polynomial ring S and a complete
  intersection R = S/(ideal ff), where 
  $$
  ff = matrix\{\{f_1,\dots,f_c\}\}
  $$
  is a 1-rowed
  matrix whose entries are (sufficiently general) generators
  of a complete intersection ideal. (Unless
  the complete intersection is homogeneous 
  and generated by elements of a single degree,
  it may not be possible to choose sufficiently general homogeneous elements, 
  even when the ideal of the complete intersection is homogeneous.)

  The routines fall into several groups:
  
  1) One group centers around the construction of the
  matrix factorizations for high syzygies of a module N,
  introduced in the
  paper
  "Matrix Factorizations in Higher Codimension"
  by Eisenbud and Peeva. The routine ``mfBound'' determines
  which syzygy to take.
  The routine matrixFactorization constructs
  the higher matrix factorization 
  of a module over R defined by Eisenbud and Peeva in 
  The ranks of the stack of matrices b_p that are used 
  in the construction of the matrix factorization, and the various matrices
  themselves, are obtained from the routines BRanks, ARanks, bMaps, dMaps, psiMaps, hMaps
  (the notation is explained in the paper). 
  
  
  Here is an example of a matrix factorization in codimension 2:
 Example
  setRandomSeed 0
  c = 2;
  S = ZZ/101[x_1..x_c, a_(1,1)..a_(c,c)];
  X = matrix{{x_1..x_c}};
  ff = X*map(source X, , genericMatrix(S,a_(1,1),c,c));
  R = S/ideal ff;
  mbound = mfBound coker (R**X)
  F = res(coker (R**X) , LengthLimit =>mbound);
  M = coker F.dd_(mbound);
  MF = matrixFactorization(ff,M)
  netList BRanks MF
  netList ARanks MF
  netList bMaps MF
  netList dMaps MF
  netList psiMaps MF
  netList hMaps MF
 Text
  The routines infiniteBettiNumbers and finiteBettiNumbers compute the Betti numbers of
  M over R and over S from the BRanks.   The minimal free
  resolution of M as a module over R/(f_1..f_s), where
  s=c-complexity M, is reconstructed (in a special form)
  from the matrix factorization MF by the routine
  makeFiniteResolution(MF, ff).
 Example
  betti res M
  infiniteBettiNumbers(MF,7)
  betti res pushForward(map(R,S),M)
  finiteBettiNumbers MF  
  G = makeFiniteResolution (MF,ff)
  G' = res(pushForward(map(R,S),M))
 Text
  The group of routines ExtModule, evenExtModule, oddExtmodule,
  extModuleData (which call the routine
  Ext(M,N) of Avramov-Grayson) are useful for analyzing the
  module Ext_R(M,k). TateResolution returns the betti table
  of a specified part of the Tate resolution of a 
  maximal Cohen-Macaulay module M
  first calling the routine cosysyzy.
  
  The routines moduleAsExt and hfModuleAsExt give
  partial converse constructions (following Avramov-Jorgensen)
  
  The routines twoMonomials and sumTwoMonomials provide some
  interesting examples.
  
  The routine makeT constructs CI operators on a resolution
  over a complete intersection, while the routine makeHomotopies
  constructs a set of higher homotopies on the resolution
  of a module M for a sequence of 
  elements in the annihilator of M(makeHomotopies1 constructs
  just the ordinare ``first'' homotopies).

  The routine exteriorTorModule constructs the module $Tor^S(M,k)$
  as a module over the exterior algebra $\wedge(k^n)$.
  
  The routine S2 takes a graded module M and returns the map
  $$
  M -> \oplus_{-p}^\infty H^0(sheaf M(p)).
  $$
  
  In all examples I know (as of 4/14/2013), if $M$ is a module
  over the complete intersection R and $E$ is
  the stable Ext module ``stable Ext(M,k)'',  the natural map from E to the 
  S2-ification of E is surjective (that is, the first local
  cohomology of a truncation of E is 0 in the range of the
  truncation.

///



{*
restart
uninstallPackage("CompleteIntersectionResolutions")
installPackage("CompleteIntersectionResolutions")
viewHelp CompleteIntersectionResolutions

loadPackage("CompleteIntersectionResolutions", Reload=>true)
*}


doc ///
Key
 moduleAsExt
 (moduleAsExt, Module, Ring)
Headline
 Find a module with given asymptotic resolution
Usage
 M = moduleAsExt(MM,R)
Inputs
 M:Module
  module over polynomial ring with c variables
 R:Ring
  (graded) complete intersection ring of codimension c, embedding dimension n
Outputs
 N:Module
  module over R such that Ext_R(N,k) = M\otimes \wedge(k^n) in large homological degree.
Description
 Text
  The routine ``moduleAsExt'' is a partial inverse to the 
  routine ExtModule, computed following ideas of Avramov and Jorgensen: 
  given a module
  E over a polynomial ring k[x_1..x_c], it provides a module
  N over a specified polynomial ring in n variables such that
  Ext(N,k) agrees with $E'=E\otimes \wedge(k^n)$ after truncation.
  Here the grading on E is taken to be even, while $\wedge(k^n)$
  has generators in degree 1. The routine hfModuleAsExt computes
  the resulting hilbert function for E'. This uses ideas of
  Avramov and Jorgensen. Note that the module Ext(N,k) (truncated)
  will automatically be free over the exterior algebra $\wedge(k^n)$
  generated by Ext^1(k,k); not a typical Ext module.
  
  More precisely:
 
  Suppose that $R = k[a_1,\dots, a_n]/(f_1,\dots,f_c)$ let
  $KK = k[x_1,\dots,x_c]$, and let $\Lambda = \wedge k^n$.
  $E = KK\otimes\Lambda$, so that the minimal
  $R$-free resolution of $k$ has underlying module $R\otimes E^*$,
  where $E^*$ is the graded vector space dual of $E$.
  
  Let MM be the result of truncating M at its regularity and shifting it
  so that it is generated in degree 0. Let $F$ be a $KK$-free resolution of $MM$,
  and write $F_i = KK\otimes V_i.$
  Since linear forms over $KK$ correspond to CI operators of degree -2 on the
  resolution G of k over R, we may form a map
  $$
  d_1+d_2: \sum_{i=0}^m G_{i+1}\otimes V_{m-i}^* \to \sum_{i=0}^m G_i\otimes V_{m-i}^*
  $$
  where $d_1$ is the direct sum of the differentials $(G_{i+1}\to G_i)\otimes V_i^*$
  and $d_2$ is the direct sum of the maps $\phi_i$ defined from the differentials of $F$
  by substituting CI operators for linear forms,
  $\phi_i: G_{i+1}\otimes V_i \to G_{i-1}\otimes V_{i-1}$.
  The script returns the module N that is the cokernel of $d_1+d_2$. 
  
  The module $Ext_R(N,k)$ agrees, after a few steps, with the module derived from
  $MM$ by tensoring it with $\Lambda$, that is, with the moduleß
  $$
  MM' = \sum_j (MM'(j)\otimes \Lambda_j)
  $$
  so that $MM'_p = (MM_p\otimes Lambda_0) \oplus (MM_{p-1}\otimes Lambda_1) \oplus\cdots$.
  
  The function hfModuleAsExt computes the Hilbert function of MM' numerically
  from that of MM.
 Example
  kk = ZZ/101;
  S = kk[a,b,c];
  ff = matrix{{a^4, b^4,c^4}};
  R = S/ideal ff;
  Ops = kk[x_1,x_2,x_3];
  MM = Ops^1/(x_1*ideal(x_2^2,x_3));
  N = moduleAsExt(MM,R);
  betti res( N, LengthLimit => 10)
  hfModuleAsExt(12,MM,3)
Caveat
  The elements f_1..f_c must be homogeneous of the same degree.
  The script could be rewritten to accomodate different degrees,
  but only by going to the local category
SeeAlso
  ExtModule
  evenExtModule
  oddExtModule
  ExtModuleData
  hfModuleAsExt  

       ///

doc ///
Key
 hfModuleAsExt
 (hfModuleAsExt, ZZ, Module, ZZ)
Headline
 predict betti numbers of moduleAsExt(M,R)
Usage
 seq = hfModuleAsExt(numValues,M,numgensR)
Inputs
 numValues:ZZ
          number of values to compute
 M:Module
  module over the ring of operators
 numgensR:ZZ
  number of generators of the target ring
Outputs
 seq:Sequence
  sequence of numValues integers, the expected total Betti numbers
Description
 Text
  Given a module M over the ring of operators $k[x_1..x_c]$, the
  call
  $N = moduleAsExt(M,R)$
  produces a module N over the ring R whose ext module is
  the exterior algebra on n=numgensR generators tensored with M.
  This script computes numValues values of the 
  Hilbert function of 
  $$
  M \otimes \wedge k^n,
  $$
  which should be equal to the total betti numbers of N.
 Example
  kk = ZZ/101;
  S = kk[a,b,c];
  ff = matrix{{a^4, b^4,c^4}};
  R = S/ideal ff;
  Ops = kk[x_1,x_2,x_3];
  MM = Ops^1/(x_1*ideal(x_2^2,x_3));
  N = moduleAsExt(MM,R);
  betti res( N, LengthLimit => 10)
  hfModuleAsExt(12,MM,3)
SeeAlso
 moduleAsExt
///



doc ///
   Key 
    Optimism
   Headline
    Option to highSyzygy
   Usage
    highSyzygy(M, Optimism => 1)
   Inputs
    Optimism:ZZ
   Description
    Text
     If highSyzygy(M) chooses the p-th syzygy,
     then highSyzygy(M,Optimism=>r) chooses the (p-r)-th syzygy.
     (Positive Optimism chooses a lower "high" syzygy, negative
     Optimism a higher "high" syzygy.
    Example
   Caveat
    Are there cases when positive Optimism is justified?
   SeeAlso
    mfBound
    highSyzygy
///



doc ///
   Key 
    Check
   Headline
    Option for matrixFactorization
   Usage
    matrixFactorization(ff,m,Check => true)
   Inputs
    Check:Boolean
   Description
    Text
     Makes matrixFactorization perform various checks as it computes.
   SeeAlso
    matrixFactorization
 ///


doc ///
   Key 
    highSyzygy
    (highSyzygy, Module)
    [highSyzygy, Optimism]
   Headline
    Returns a syzygy module one beyond the regularity of Ext(M,k)
   Usage
    M = highSyzygy M0
   Inputs
    M0:Module
     over a complete intersection ring
   Outputs
    M:Module
     a syzygy module of M0
   Description
    Text
     A "high syzygy" over a complete intersection is one
     such that general ci-operators have split kernels
     when applied recursively on cosyzygy chains of 
     previous kernels.
     
     ASSUMING our
     conjecture that Ext surjects onto its S2-ification, this
     should be the first syzygy beyond the regularity of
     the even and odd Ext modules Ext(M,k). This position is
     computed by the script mfBound.
     
     If p = mfBound M0, then highSyzygy M0
     returns the p-th syzygy of M0.
     (if F is a resolution of M this is the cokernel 
     of F.dd_p). Optimism => r as optional
     argument, highSyzygy(M0,Optimism=>r)
     returns the (p-r)-th syzygy. The script is
     useful with matrixFactorization(ff, highSyzygy M0).
    Example
     setRandomSeed 100
     S = ZZ/101[x,y,z]
     f = matrix"x3,y3+x3,z3+x3+y3"
     ff = f*random(source f, source f)
     R = S/ideal f
     M0 = R^1/ideal"x2z2,xyz"
     betti res (M0, LengthLimit => 7)
     mfBound M0
     M = betti res highSyzygy M0
     netList BRanks matrixFactorization(ff, highSyzygy M0)
    Text
     In this case as in all others we have examined, 
     greater "Optimism" is not 
     justified:
    Example
     matrixFactorization(ff, highSyzygy(M0, Optimism=>1));
   Caveat
    A bug in the total Ext script means that the oddExtModule
    is sometimes zero, and this can cause a wrong value to be
    returned. 
   SeeAlso
    evenExtModule
    oddExtModule
    mfBound
    matrixFactorization
///




doc ///
   Key
    mfBound
    (mfBound, Module)
   Headline
    determines how high a syzygy to take for "matrixFactorization"
   Usage
    p = mfBound M
   Inputs
    M:Module
     over a complete intersection
   Outputs
    p:ZZ
   Description
    Text
     If p = mfBound M, then the p-th syzygy of M,
     which is computed by highSyzygy(M), 
     should (this is a conjecture) 
     be a "high Syzygy" in the sense required
     for matrixFactorization. In examples, the estimate
     seems sharp (except when M is already a high syzygy).
    
     The actual formula used is:
    
     mfBound M = 1+max(2*r_{even}, 1+2*r_{odd})
    
     where r_{even} = regularity evenExtModule M and
     r_{odd} = regularity oddExtModule M. Here
     evenExtModule M is the even degree part of
     Ext(M, (residue class field)).
   SeeAlso
    highSyzygy
    evenExtModule
    oddExtModule
    matrixFactorization
///



doc ///
   Key
    ExtModuleData
    (ExtModuleData, Module)
   Headline
    Even and odd Ext modules and their regularity
   Usage
    L = ExtModuleData M
   Inputs
    M:Module
     Module over a complete intersection S
   Outputs
    L:List
     L = \{evenExtModule, oddExtModule, reg0, reg1\}
   Description
    Text
     Suppose that M is a module over a complete intersection R
     so that 
     
     E := ExtModule M 
     
     is a module generated in degrees >=0 
     over a polynomial ring T' 
     generated in degree 2, and
     
     E0 := evenExtModule M and 
     E1 := oddExtModule M
     
     are modules generated in degree >= 0
     over a polynomial ring T with generators 
     in degree 1.
     
     The script returns 
     
     L = \{E0,E1, regularity E0, regularity E1\}
     
     and prints a message if |reg0-reg1|>1.
     
     If we set r = max(2*reg0, 1+2*reg1),
     and F is a resolution of M, then 
     coker F.dd_{(r+1)}
     is the first szygy module of M such that
     regularity evenExtModule M =0 AND
     regularity oddExtModule M =0 
     
     We have been using regularity ExtModule M 
     as a substitute for r,
     but that's not always the same.
    Example
     setRandomSeed 100
     S = ZZ/101[a,b,c,d];
     f = map(S^1, S^4, (i,j) -> S_j^3)
     R = S/ideal f;
     M = R^1/ideal"ab2+cd2";     
     betti (F = res(M, LengthLimit => 5))
     E = ExtModuleData M;
     E_2     
     E_3          
     r = max(2*E_2,2*E_3+1)
     Er = ExtModuleData coker F.dd_r;
     regularity Er_0
     regularity Er_1
     regularity evenExtModule(coker F.dd_(r-1))
     ff = f*random(source f, source f);
     matrixFactorization(ff, coker F.dd_(r+1));
    Text
     This succeeds, but we would get an error from
     
     matrixFactorization(ff, coker F.dd_r)
     
     because one of the CI operators would not be surjective.
   Caveat
     ExtModule creates a ring inside the script, so if it's run
     twice you get modules over different rings. This should be
     changed.
   SeeAlso
    ExtModule
    evenExtModule
    oddExtModule
///

{*
doc ///
   Key
    submatrixByDegrees
    (submatrixByDegrees, Matrix, List, List)
   Headline
    submatrix of elements with given row and col degrees
   Usage
    m1 = submatrixByDegrees(m,rowDegList,colDegList)
   Inputs
    m:Matrix
      map between graded FREE modules
    rowDegList:List
      list of integers, desired row (target) degrees
    colDegList:List
      list of integers, desired column (source) degrees
   Outputs
    m1:Matrix
      submatrix of m
   Description
    Text
    Example
     S = ZZ/2[a,b,c,d];
     setRandomSeed 0
     m = random(S^{2,4,6},S^{ -1,3});
     betti m
     m1 = submatrixByDegrees(m,{3},{4});
     betti m1
     m1 = submatrixByDegrees(m,{ -2,-4},{1});
     betti m1
///
*}
{*
document{Key =>  hf,
     Headline => "Hilbert function in a range",
     Usage => "using hilbertFunction(ZZ,Module),
     hf returns a Sequence or List
     of the values of the Hilbert function of the Module
     at the integer arguments specified by the Sequence or List."}
*}

doc ///
   Key
    isLinear
    (isLinear, Matrix)
   Headline
    check whether matrix entries have degree 1
   Usage
    b = isLinear M
   Inputs
    M:Matrix
   Outputs
    b:Boolean
   Description
    Text
     Note that a linear matrix, in this sense, can still
     have different target degrees (in which case the
     cokernel decomposes into a direct sum by generator
     degree.)
///

doc ///
   Key
    TateResolution
    (TateResolution, Module, ZZ,ZZ)
    (TateResolution, Module, ZZ)
    (TateResolution, Module)
   Headline
    TateResolution of a module over an exterior algebra
   Usage
    F = TateResolution(M,lower,upper)
   Inputs
    M:Module
    lower:ZZ
    upper:ZZ
          lower and upper bounds for the resolution
   Outputs
    F:ChainComplex
   Description
    Text
     Forms an interval, lower..upper, 
     of a doubly infinite free resolution of 
     a a Cohen-Macaulay
     module over a Gorenstein ring, such as
     any module over an exterior algebra (actually,
     any module over any ring.)
    Example
     E = ZZ/101[a,b,c, SkewCommutative=>true]
     M = coker map(E^2, E^{-1}, matrix"ab;bc")
     presentation M
     TateResolution(M,2,7) 
///

doc ///
   Key
    makeT
    (makeT,Matrix, ChainComplex,ZZ)
--    (makeT,Matrix, ChainComplex,Matrix, ZZ)    
   Headline
    make the CI operators on a complex
   Usage
    T = makeT(ff,F,i)
    T = makeT(ff,F,t0,i)
   Inputs
    ff:Matrix
      1xc matrix whose entries are a complete intersection in S
    F:ChainComplex
      over S/ideal ff
    t0:Matrix
      CI-operator on F for ff_0 to be preserved
    i:ZZ
      define CI operators from F_i \to F_{i-2}
   Outputs
    L:List
      of CI operators F_i \to F_{i-2} corresponding to entries of ff
   Description
    Text
     substitute matrices of two differentials of F into S = ring ff,
     compose them, and divide by entries of ff, in order.
     If the second Matrix argument t0 is present, use it
     as the first CI operator. 
     
     The degrees of the targets of the T_j are
     changed by the degrees of the f_j to make the T_j
     homogeneneous.
    Example
     S = ZZ/101[x,y,z];
     ff = matrix"x3,y3,z3";
     R = S/ideal ff;
     M = coker matrix"x,y,z;y,z,x";
     betti (F = res M)
     T = makeT(ff,F,3);
     netList T
     isHomogeneous T_2
   Caveat
    Script assumes that ring F == (ring ff)/(ideal ff).
    It might be more useful to return the operators as matrices
    over S rather than over R, since this is what we'd need
    for things like matrixFactorization (where this process
    currently done on the fly, not calling makeT)
///

{*doc///
Key
 isSurjCIOperator
 (isSurjCIOperator, Matrix, ChainComplex)
 (isSurjCIOperator, Matrix, ChainComplex, ZZ) 
Headline
 Checks whether a CI operator is surjective
Usage
 i = isSurjCIOperator(ff,F)
Inputs
 ff:Matrix
    1xc matrix containing a regular sequence
 FF:ChainComplex 
    over S/ideal ff
Outputs
  i:ZZ
    point from which CI operator corresponding to ff_0 is surjective.
///
*}

doc ///
        Key 
	 ExtModule
	 (ExtModule, Module)
        Headline 
	 Ext^*(M,k) over a complete intersection as module over CI operator ring
        Usage
	 E = ExtModule M
        Inputs
	 M:Module
	   over a complete intersection ring
        Outputs
	 E:Module
	   over a polynomial ring with gens in even degree
        Description

         Text
	  Uses code of Avramov-Grayson described in Macaulay2 book
         Example
	  kk= ZZ/101
	  S = kk[x,y,z]
	  I1 = ideal "x3y"
	  R1 = S/I1
	  M1 = R1^1/ideal(x^2)
	  betti res (M1, LengthLimit =>5)
	  E = ExtModule M1
	  apply(toList(0..10), i->hilbertFunction(i, E))
	  Eeven = evenExtModule(M1)
	  apply(toList(0..5), i->hilbertFunction(i, Eeven))
	  Eodd = oddExtModule(M1)
	  apply(toList(0..5), i->hilbertFunction(i, Eodd))
	  --
	  use S
	  I2 = ideal"x3,yz"
	  R2 = S/I2
	  M2 = R2^1/ideal"x2,y,z"
	  betti res (M2, LengthLimit =>10)	  
	  E = ExtModule M2
	  apply(toList(0..10), i->hilbertFunction(i, E))
	  Eeven = evenExtModule M2
	  apply(toList(0..5), i->hilbertFunction(i, Eeven))
	  Eodd = oddExtModule M2
	  apply(toList(0..5), i->hilbertFunction(i, Eodd))
        SeeAlso 
	  evenExtModule 
	  oddExtModule
///
doc ///
        Key 
	 evenExtModule
	 (evenExtModule, Module)
        Headline 
	 even part of Ext^*(M,k) over a complete intersection as module over CI operator ring
        Usage
	 E = evenExtModule M
        Inputs
	 M:Module
	   over a complete intersection ring
        Outputs
	 E:Module
	   over a polynomial ring with gens in degree 1
        Description
         Text
	  Extracts the even degree part from ExtModule M
         Example
	  kk= ZZ/101
	  S = kk[x,y,z]
	  I2 = ideal"x3,yz"
	  R2 = S/I2
	  M2 = R2^1/ideal"x2,y,z"
	  betti res (M2, LengthLimit =>10)	  
	  E = ExtModule M2
	  apply(toList(0..10), i->hilbertFunction(i, E))
	  Eeven = evenExtModule M2
	  apply(toList(0..5), i->hilbertFunction(i, Eeven))
        SeeAlso 
	  ExtModule 
	  oddExtModule

     ///
doc ///
        Key 
	 oddExtModule
	 (oddExtModule, Module)
        Headline 
	 odd part of Ext^*(M,k) over a complete intersection as module over CI operator ring
        Usage
	 E = oddExtModule M
        Inputs
	 M:Module
	   over a complete intersection ring
        Outputs
	 E:Module
	   over a polynomial ring with gens in degree 1
        Description
         Text
	  Extracts the odd degree part from ExtModule M
         Example
	  kk= ZZ/101
	  S = kk[x,y,z]
	  I2 = ideal"x3,yz"
	  R2 = S/I2
	  M2 = R2^1/ideal"x2,y,z"
	  betti res (M2, LengthLimit =>10)	  
	  E = ExtModule M2
	  apply(toList(0..10), i->hilbertFunction(i, E))
	  Eodd = oddExtModule M2
	  apply(toList(0..5), i->hilbertFunction(i, Eodd))
        SeeAlso 
	  ExtModule 
	  evenExtModule
     ///


doc ///
Key
 makeHomotopies
 (makeHomotopies,Matrix,ChainComplex,ZZ)
 (makeHomotopies,Matrix,ChainComplex)
Headline
 returns a system of higher homotopies
Usage
 H = makeHomotopies(f,F,d)
Inputs
 f:Matrix
   1xn matrix of elements of S
 F:ChainComplex
   admitting homotopies for the entries of f
 d:ZZ
   how far back to compute the homotopies (defaults to length of F)
Outputs
 H:HashTable
   gives the higher homotopy from F_i corresponding to a monomial with exponent
   vector L as the value $H#\{L,i\}$
Description
 Text
  Given a $1\times n$ matrix f, and a chain complex F,
  the script attempts to make a family of higher homotopies
  on F for the elements of f, in the sense described, for
  example, in Eisenbud "Enriched Free Resolutions and Change
  of Rings".
  
  The output is a hash table with entries of the form $\{J,i\}=>s$, 
  where
  J is a list of non-negative integers, of length n
  and $H\#\{J,i\}: F_i->F_{i+2|J|-1}$ are maps satisfying 
  the conditions
  $$
  H\#\{e0,i\} = d;
  $$
  and
  $$
  H#\{e0,i+1\}*H#\{e,i\}+H#\{e,i-1\}H#\{e0,i\} = f_i,
  $$ 
  where $e0 = \{0,\dots,0\}$ and $e$ is the index of degree 1
  with a 1 in the $i$-th place;
  and, for each index list I with |I|<=d,
  $$
  sum_{J<I} H#\{I\setminus J, \} H#\{J, \} = 0.
  $$
 Example
  kk=ZZ/101
  S = kk[a,b,c,d]
  F = res ideal vars S  
  f = matrix{{a,b,c}}
  homot = makeHomotopies(f,F,2)
 Text
  In this case the higher homotopies are 0:
 Example
  L = sort select(keys homot, k->(homot#k!=0 and sum(k_0)>1))
 Text
  On the other hand, if
  we take a complete intersection and something
  contained in it in a more complicated situation,
  the program gives nonzero higher homotopies:
 Example
  kk= ZZ/32003;
  S = kk[a,b,c,d];
  M = S^1/(ideal"a2,b2,c2,d2");
  F = res M
  setRandomSeed 0
  f = random(S^1,S^{2:-5});
  homot = makeHomotopies(f,F,5)
 Text
  We can see that all 6 potential higher homotopies are nontrivial:
 Example
  L = sort select(keys homot, k->(homot#k!=0 and sum(k_0)>1))
  #L
  netList L
 Text
  For example we have:
 Example
  homot#(L_0)
 Text
  But all the homotopies are minimal in this case:
 Example
  k1 = S^1/(ideal vars S);
  select(keys homot,k->(k1**homot#k)!=0)
SeeAlso
 makeHomotopies1
///


doc ///
Key
 makeHomotopies1
 (makeHomotopies1, Matrix,ChainComplex,ZZ)
 (makeHomotopies1, Matrix,ChainComplex) 
Headline
 returns a system of first homotopies
Usage
 H = makeHomotopies1(f,F,d)
Inputs
 f:Matrix
   1xn matrix of elements of S
 F:ChainComplex
   admitting homotopies for the entries of f
 d:ZZ
   how far back to compute the homotopies (defaults to length of F)
Outputs
 H:HashTable
   gives the homotopy from F_i corresponding to f_j
   as the value $H#\{j,i\}$
Description
 Text
   Same as makeHomotopies, but only computes the ordinary 
   homotopies, not the higher ones. Used in exteriorTorModule
SeeAlso
 makeHomotopies
 exteriorTorModule
///

doc ///
Key
 makeModule
 (makeModule, Ring,List,HashTable)
Headline
 realize a free module with (anti)-commuting operators as a module
Usage
 M =  makeModule(R,T,Hk)
     --T is a list of free modules F_i over over
     --k =  coefficientRing R.
     --H is a HashTable with pairs of the form {j,i} => phi,
     --where phi: F_i\to F_(i+1).
     --The script takes R**\sum F_i as a graded R-module, 
     --where the j-th generator of R acts as H#{j,i}.

Inputs
 R:Ring
  The ring over which the module will be defined
 T:List
  A List of free modules over the coefficient ring of R, the components of the new module
 Hk:HashTable
  The value Hk#{j,i} specifies the action of the j-th variable
  as a map T_i --> T_(i+1)
Outputs
 M:Module 
  Module over R (not a minimal presentation
Description
 Text
  Used in exteriorTorModule
SeeAlso
 exteriorTorModule
///

doc ///
Key
 S2
 (S2,ZZ,Module)
Headline
 Universal map to a module satisfying Serre's condition S2
Usage
 f = S2(b,M)
Inputs
 b:ZZ
   degree bound to which to carry the computation
 M:Module
Outputs
 f:Matrix
   defining a map M-->M' that agrees with the
   S2-ification of M in degrees $\geq b$
Description
 Text
  If M is a graded module over a ring S, then the S2-ification
  of M is \sum_{d \in ZZ} H^0((sheaf M)(d)), which may be computed
  as lim_{d->\infty} Hom(S/I_d,M), where I_d is any sequence
  of ideals contained in higher and higher powers of S_+.
  There is a natural restriction map 
  f: M = Hom(S,M) \to Hom(I_d,M).
  We compute all this using the ideals 
  I_d generated by the d-th powers
  of the variables in S.
  
  Since the result may not be finitely generated (this happens
  if and only if M has an associated prime of dimension 1), we
  compute only up to a specified degree bound b. For the result to
  be correct down to degree b, it is sufficient to compute
  Hom(I,M)
  where I \subset (S_+)^{r-b}.
 Example
  kk=ZZ/101
  S = kk[a,b,c,d]
  M = truncate(3,S^1)
  betti S2(0,M)
  betti S2(1,M)
  M = S^1/intersect(ideal"a,b,c", ideal"b,c,d",ideal"c,d,a",ideal"d,a,b")
  prune source S2(0,M)
  prune target S2(0,M)
SeeAlso
  "IntegralClosure"
  "makeS2"
  "BGG"
  "cohomology"
  "HH^ZZ SumOfTwists"
Caveat
 Text
  S2-ification is related to computing cohomology and to 
  computing integral closure; there are scripts in
  those packages that produce an S2-ification, but one takes
  a ring as argument and the other doesn't produce the 
  comparison map.
///

doc///
Key
  splittings
  (splittings, Matrix, Matrix)
Headline
  compute the splittings of a split right exact sequence
Usage
  x = splittings(a,b)
Inputs
  a:Matrix
    maps into the kernel of b
  b:Matrix
    representing a surjection from target a to a free module
Outputs
  L:List
    L = \{sigma,tau\}, splittings of a,b respectively
Description
 Text
     Assuming that (a,b) are the maps of a right exact
     sequence 
     
     $0\to A\to B\to C \to 0$
     
     with B, C free,
     the script produces a pair of maps sigma, tau
     with $tau: C \to B$ a splitting of b and
     $sigma: B \to A$ a splitting of a; that is,
     
     $a*sigma+tau*b = 1_B$
     
     $sigma*a = 1_A$
     
     $b*tau = 1_C$
 Example
   kk= ZZ/101
   S = kk[x,y,z]
   setRandomSeed 0
   t = random(S^{2:-1,2:-2}, S^{3:-1,4:-2})
   ss = splittings(syz t, t)
   ss/betti
///


     doc ///
        Key 
	 toArray
	 (toArray, List)
	 (toArray, ZZ)
        Headline
	 makes an array from a List or from a single integer
	Usage
	 arr = toArray L
	 arr = toArray n
	Inputs
	 L:List
	 n:ZZ
	Outputs
	 arr:Array
     ///
     
doc ///
Key
 matrixFactorization
 (matrixFactorization, Matrix, Module)
 [matrixFactorization, Check]
Headline
 Maps in a higher codimension matrix factorization
Usage
 MF = matrixFactorization(ff,M)
Inputs
 ff:Matrix
   a sufficiently general regular sequence in a ring S
 M:Module
   a high syzygy over S/ideal ff 
Outputs
 MF:List
    \{d,h\}, where d:A_1 \to A_0 and h is a hashTable of ``partial homotopies''
Description
 Text
  The input module M should be a ``high syzygy'' over
  R = S/ideal ff.  In all example we
  know it suffices for Ext^{even}_R(M,k) and Ext^{odd}_R(M,k) to have negative regularity
  over the ring of CI operators (regraded with variables of degree 1).
  
  If the CI operator at some stage of the induction is NOT surjective,
  then the script returns a String containing the presentation matrix
  of M. This condition can be caught by testing whether the
  returned value is a String or a List.
  
  When the optional input Check==true (the default is Check==false), 
  the properties in the definition of Matrix Factorization are verified

  If the CI operators at each stage are surjective (that is, if
  M is really a high syzygy), then:
  
  The output is a list   
  \{d,h\}. 
  
  The map d is a special lifting to S of a presentation of
  M over R. To explain the contents, we introduce some notation
  (from our paper ****):

  R(i) = S/(ff_0,..,ff_{i-1}). Here 0<= i <= c, and R = R(c)
  and S = R(0).
  
  B(i) = the matrix (over S) representing d_i: B_1(i) \to B_0(i)
  
  d(i): A_1(i) \to A_0(i) the restriction of d = d(c).
  where A(i) = \oplus_{i=1}^p B(i)
  
  The object h is a hashTable. 
  The map h#p:target A(p) \to source A(p)
  is a homotopy for ff#p on the restriction
  d(p): over the ring R#(p-1) = S/(ff#1..ff#(p-1),
  so d(p) * h#p = ff#p mod (ff#1..ff#(p-1).
  
  In addition, h#p * d(p) induces ff#p on B1#p 
  mod (ff#1..ff#(p-1).
  
  Here is a simple codim 2 example:
 Example
  setRandomSeed 0
  kk = ZZ/101
  S = kk[a,b,u,v]
  ff = matrix"au,bv"
  R = S/ideal ff
  M0 = R^1/ideal"a,b"
  M = highSyzygy M0
  MF = matrixFactorization(ff,M);
  netList BRanks MF
  netList bMaps MF
  betti res(M, LengthLimit => 7)
  infiniteBettiNumbers (MF,7)
  betti res pushForward(map(R,S),M)
  finiteBettiNumbers MF
SeeAlso
  finiteBettiNumbers
  infiniteBettiNumbers
  highSyzygy
  bMaps
  BRanks
///

///
restart
uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"
viewHelp matrixFactorization
///

doc///
Key
 finiteBettiNumbers
 (finiteBettiNumbers, List)
Headline
 betti numbers of finite resolution computed from a matrix factorization
Usage
 L = finiteBettiNumbers MF
Inputs
 MF:List
   List of HashTables as computed by "matrixFactorization"
Outputs
 L:List
   List of betti numbers
Description
 Text
  Uses the ranks of the B matrices in a matrix factorization
  for a module M over S/(f_1,..,f_c) to compute the betti numbers
  of the minimal resolution of M over S, which is the sum
  of the Koszul complexes K(f_1..f_{j-1}) tensored with B(j)
 Example
  setRandomSeed 0
  kk = ZZ/101
  S = kk[a,b,u,v]
  ff = matrix"au,bv"
  R = S/ideal ff
  M0 = R^1/ideal"a,b"
  F = res(M0, LengthLimit =>3)
  M = coker F.dd_3;
  MF = matrixFactorization(ff,M);
  betti res pushForward(map(R,S),M)
  finiteBettiNumbers MF
  infiniteBettiNumbers(MF,5)
  betti res (M, LengthLimit => 5)
SeeAlso
  matrixFactorization
  infiniteBettiNumbers
///

doc ///
Key
 infiniteBettiNumbers
 (infiniteBettiNumbers, List, ZZ)
Headline
 betti numbers of finite resolution computed from a matrix factorization
Usage
 L = finiteBettiNumbers (MF,len)
Inputs
 MF:List
   List of HashTables as computed by "matrixFactorization"
 len:ZZ
   length of betti number sequence to produce
Outputs
 L:List
   List of betti numbers
Description
 Text
  Uses the ranks of the B matrices in a matrix factorization
  for a module M over S/(f_1,..,f_c) to compute the betti numbers
  of the minimal resolution of M over R, which is the sum
  of the divided power algebras on c-j+1 variables tensored with B(j).
 Example
  setRandomSeed 0
  kk = ZZ/101
  S = kk[a,b,u,v]
  ff = matrix"au,bv"
  R = S/ideal ff
  M0 = R^1/ideal"a,b"
  F = res(M0, LengthLimit =>3)
  M = coker F.dd_3;
  MF = matrixFactorization(ff,M);
  betti res pushForward(map(R,S),M)
  finiteBettiNumbers MF
  infiniteBettiNumbers(MF,5)
  betti res (M, LengthLimit => 5)
SeeAlso
  matrixFactorization
  finiteBettiNumbers
///

{*doc ///
Key
 BMaps
 (BMaps, List)
Headline
 The stack of "B" maps making up a matrix factorization
Usage
 L = BMaps MF
Inputs
 MF:List
   List of HashTables as computed by "matrixFactorization"
Outputs
 L:List
   List of matrices
Description
 Text
 Example
  S = kk[a,b,u,v]
  ff = matrix"au,bv"
  R = S/ideal ff
  M0 = R^1/ideal"a,b"
  F = res(M0, LengthLimit =>3)
  M = coker F.dd_3;
  MF = matrixFactorization(ff,M);
  BMaps MF
  netList ((BMaps MF)/betti)
SeeAlso
  matrixFactorization
  finiteBettiNumbers
  infiniteBettiNumbers
///
*}
doc///
Key
  exteriorTorModule
  (exteriorTorModule, Matrix, ChainComplex)
  (exteriorTorModule,Matrix,Ring,Module,Module)  
Headline
  Homology of a complex **k as a module over an exterior algebra
Usage
  T = exteriorTorModule(f,F)
  T = exteriorTorModule(f,E,M,N)
Inputs
  f:Matrix
    1 x c, entries must be homotopic to 0 on F
  F:ChainComplex
    A minimal complex, typically a resolution of a module annihilated by ideal f
  E:Ring
   an exterior algebra over S/J, where J is an ideal containing ideal f
  M:Module
   S-module annihilated by ideal f
  N:Module
   S-module annihilated by ideal f so that ann M + ann N contains J
Outputs
  T:Module
    Tor^S(M,N) as a Module over E
Description
 Text
  Suppose that F is a minimal 
  complex over a ring S with residue field k, and that
  multiplication by the elements of f are null-homotopic on F.
  The script exteriorTorModule(f,F)
  calls makeHomotopies1 to compute 
  homotopies for multiplication by the f_i on F.
  This makes F a homotopy associative, homotopy commutative graded module
  over the Koszul complex of f. Thus F**k is a module over \Lambda(k^{length f}).
  
  When M,N are S-modules and J\subset (ann M)+(ann N), so that Tor^S((M,N) is a 
  homologically graded (S/J)-module, then the same considerations make
  Tor^S(M,N) into an E-module, where E is the exterior algebra over S/J on c variables,
  and this E-module is the output of exteriorTorModule(f,E,M,N).

  The scripts call makeModule (respectively makeModule1) 
  to compute a (non-minimal) presentation of this module.
  
  From the description by matrix factorizations we see that
  when M is a high syzygy and F is its resolution,
  then the presentation of Tor(M,S^1/mm) always has generators
  in degrees 0,1, corresponding to the targets and sources of the
  stack of maps B(i). We CONJECTURE that the relations are all in degrees 1,2,
  and that the resolution is componentwise linear in a suitable sense.
  In the following example, these facts are verified. The Tor module does NOT 
  split into the direct sum of the submodules generated in degrees 0 and 1, however.
  
 Example
  kk = ZZ/101
  S = kk[a,b,c]
  f = matrix"a4,b4,c4"
  R = S/ideal f
  p = map(R,S)
  M = coker map(R^2, R^{3:-1}, {{a,b,c},{b,c,a}})			       
  betti (FF =res( M, LengthLimit =>6))
  MS = prune pushForward(p, coker FF.dd_6);
  betti(F = res MS)
  T = exteriorTorModule(f,F);
  betti T
  betti res (PT = prune T)
  ann PT
  PT0 = image (inducedMap(PT,cover PT)* ((cover PT)_{0..12}));
  PT1 = image (inducedMap(PT,cover PT)* ((cover PT)_{13..30}));
  betti res prune PT0
  betti res prune PT1
  betti res prune PT
SeeAlso
  makeModule
///


doc///
Key
  exteriorExtModule
  (exteriorExtModule, Matrix, ChainComplex)
Headline
  Homology of Hom(complex,k) as a module over an exterior algebra
Usage
  E = exteriorExtModule(f,F)
Inputs
  f:Matrix
    1 x c, entries must be homotopic to 0 on F
  F:ChainComplex
    A minimal complex, typically a resolution of a module annihilated by ideal f
Outputs
  E:Module
    Module over an exterior algebra with variables corresponding to elements of f
Description
 Text
  Suppose that F is a minimal 
  complex over a ring S with residue field k, and that
  multiplication by the elements of f are null-homotopic on F.
  This script calls makeHomotopies1 to compute 
  homotopies for multiplication by the f_i on F.
  This makes F a homotopy associative, homotopy commutative graded module
  over the Koszul complex of f. Thus Hom(F,k) is a module over \Lambda(k^{length f}).
  
  The script prints a part of a Tate resolution of E, and returns E,
  which is computed as the dual of exteriorExtModule(f,F).
  
  From the description by matrix factorizations we see that
  when M is a high syzygy and F is its resolution,
  then the presentation of Tor(M,S^1/mm) always has generators
  in degrees 0,1, corresponding to the targets and sources of the
  stack of maps B(i). We CONJECTURE that the relations are all in degrees 1,2,
  and that the resolution is componentwise linear in a suitable sense.
  
 Example
  kk = ZZ/101
  S = kk[a,b,c]
  f = matrix"a4,b4,c4"
  R = S/ideal f
  p = map(R,S)
  M = coker map(R^2, R^{3:-1}, {{a,b,c},{b,c,a}})			       
  betti (FF =res( M, LengthLimit =>6))
  MS = prune pushForward(p, coker FF.dd_6);
  betti(F = res MS)
  T = exteriorTorModule(f,F);
  E = exteriorExtModule(f,F);
  betti res (PE = prune E)
  betti res (PT = prune T)  
SeeAlso
  exteriorTorModule
  makeModule
///
     doc ///
        Key
	  freeExteriorSummand
	  (freeExteriorSummand,Module)
        Headline
	  find the free summands of a module over an exterior algebra
        Usage
	  F = freeExteriorSummand M
        Inputs
	  M:Module
	    over an exterior algebra
        Outputs
	  F:Matrix
            Map from a free module to M. Image is the largest free summand
	Description
	 Text
         Example
   	    kk= ZZ/101
	    E = kk[e,f,g, SkewCommutative => true]
	    M = E^1++module ideal vars E++E^{-1}
	    freeExteriorSummand M
     ///
     
{*       doc ///
          Key
	   submoduleByDegrees
	   (submoduleByDegrees,Module,ZZ)
          Headline
	   submodule generated by elements of bounded degree
          Usage
	   N = submoduleByDegrees(M,n)
          Inputs
	   M:Module
	   L:ZZ
	     n bound for generators
          Outputs
	   N:Module
	     submodule generated by elements of degree <=n
          Description
           Text
	    For testing componentwise linearity, the module should
	    be truncated in degree n
           Example
	    S = ZZ/101[a,b]
            setRandomSeed 0
	    M = coker random(S^{1,0,-1},S^{-2,-3});
	    prune submoduleByDegrees(M,-1)
	    N = submoduleByDegrees(M,0)
	    betti res prune N
	    betti res truncate(0, prune N)
	  Caveat
	   Text
	    Output is not pruned or truncated
       ///
*}

doc ///
   Key
    cosyzygyRes
    (cosyzygyRes, ZZ, Module)
    (cosyzygyRes, Module)
   Headline
    cosyzygy chain of a Cohen-Macaulay module over a Gorenstein ring
   Usage
    F = cosyzygyRes(len, M)
   Inputs
    len:ZZ
        how long a chain of cosyzygies
    M:Module
      Should be a CM module over a Gorenstein ring
   Outputs
    F:ChainComplex
      last map is presentation of M
   Description
    Text
     the script returns the dual of the complex F obtained by
     resolving the cokernel of the transpose of 
     the presentation of M
     for len steps. Thus M is the len-th syzygy of the module
     resolved by F. When the second argument len is omitted, 
     the value defaults to len = 2.
    Example
     S = ZZ/101[a,b,c];
     R = S/ideal"a3,b3,c3";
     M = module ideal vars R;
     betti presentation M
     betti (F = cosyzygyRes(3,M))
     cosyzygyRes M
///


     doc ///
        Key 
	 BRanks
	 (BRanks, List)
        Headline 
	 ranks of the modules B_i(d) in a matrixFactorization
        Usage 
	 br = BRanks MF
        Inputs 
	 MF:List
	  output of a matrixFactorization computation
        Outputs
	 br: List
	  list of pairs {rank B_(d), rank B_0(d)
        Description
         Example
	  c = 2
	  S = ZZ/32003[x_0..x_(c-1),a_(0,0)..a_(c-1,c-1)];
	  A = genericMatrix(S,a_(0,0),c,c);
	  f = matrix{{x_0..x_(c-1)}}*map(S^{c:-1},S^{c:-2},A)
	  R = S/ideal f;
	  kR = R^1/ideal(x_0..x_(c-1))
	  MF = matrixFactorization(f,highSyzygy kR)
	  netList BRanks MF
	  netList dMaps MF
	  netList bMaps MF
	  netList psiMaps MF
	SeeAlso
	  matrixFactorization
	  bMaps
	  dMaps
	  psiMaps
	  hMaps
     ///

     doc ///
        Key 
	 ARanks
	 (ARanks, List)
        Headline 
	 ranks of the modules A_i(d) in a matrixFactorization
        Usage 
	 AR = ARanks MF
        Inputs 
	 MF:List
	  output of a matrixFactorization computation
        Outputs
	 AR: List
	  list of pairs {rank A_1(p), rank A_0(p)}
        Description
         Text
	  See example in the description of BRanks
        SeeAlso
	  matrixFactorization
	  BRanks
	  bMaps
	  dMaps
	  psiMaps
	  hMaps
     ///


     doc ///
        Key 
	 bMaps
	 (bMaps, List)
        Headline 
	 list the maps  d_p:B_1(p)-->B_0(p) in a matrixFactorization
        Usage 
	 bmaps = bMaps mf
        Inputs 
	 mf:List
	  output of a matrixFactorization computation
        Outputs
	 bmaps: List
	  list of matrices $d_p: B_1(p)\to B_0(p)$
        Description
	 Text
	  See the documentation for matrixFactorization for an example.
        SeeAlso
	 matrixFactorization
	 BRanks
	 dMaps
	 psiMaps
	 hMaps
     ///
     doc ///
        Key 
	 dMaps
	 (dMaps, List)
        Headline 
	 list the maps  d(p):A_1(p)--> A_0(p) in a matrixFactorization
        Usage 
	 amaps = dMaps mf
        Inputs 
	 mf:List
	  output of a matrixFactorization computation
        Outputs
	 amaps: List
	  list maps $d_p: B_1(p)\to B_0(p)$
        Description
	 Text
	  See the documentation for matrixFactorization for an example.
        SeeAlso
	 matrixFactorization
	 BRanks
	 bMaps
	 psiMaps
	 hMaps
     ///

     doc ///
        Key 
	 hMaps
	 (hMaps, List)
        Headline 
	 list the maps  h(p): A_0(p)--> A_1(p) in a matrixFactorization
        Usage 
	 hMaps = hMaps mf
        Inputs 
	 mf:List
	  output of a matrixFactorization computation
        Outputs
	 hMaps: List
	  list matrices $h_p: A_0(p)\to A_1(p)$
        Description
	 Text
	  See the documentation for matrixFactorization for an example.
        SeeAlso
	 matrixFactorization
	 dMaps
	 BRanks
	 bMaps
	 psiMaps
     ///

     doc ///
        Key 
	 psiMaps
	 (psiMaps, List)
        Headline 
	 list the maps  psi(p): B_1(p) --> A_0(p-1) in a matrixFactorization
        Usage 
	 psmaps = psiMaps mf
        Inputs 
	 mf:List
	  output of a matrixFactorization computation
        Outputs
	 psmaps: List
	  list matrices $d_p: B_1(p)\to A_0(p-1)$
        Description
	 Text
	  See the documentation for matrixFactorization for an example.
        SeeAlso
	 matrixFactorization
	 BRanks
	 bMaps
	 dMaps
	 hMaps
     ///

     doc ///
        Key
	 sumTwoMonomials
	 (sumTwoMonomials, ZZ,ZZ)
        Headline
	 tally the sequences of BRanks for certain examples
        Usage
	 sumTwoMonomials(c,d)
        Inputs
	 c:ZZ
	   codimension in which to work
	 d:ZZ
	   degree of the monomials to take
        Outputs
	 T:Tally
        Description
         Text
	  tallies the sequences of B-ranks that occur for sums of pairs of 
	  monomials in R = S/(d-th powers of the variables), with
	  full complexity (=c); that is,
	  for an appropriate syzygy M of 
	  M0 = R/(m1+m2)
	  where m1 and m2 are monomials of the same degree.
         Example
	  sumTwoMonomials(2,3)
        SeeAlso
	 twoMonomials
	///
	
     doc ///
        Key
	 twoMonomials
	 (twoMonomials, ZZ,ZZ)
	 [twoMonomials,Optimism]
        Headline
	 tally the sequences of BRanks for certain examples
        Usage
	 T = TwoMonomials(c,d)
        Inputs
	 c:ZZ
	  codimension in which to work
	 d:ZZ
	  degree of the monomials to take
        Outputs
	 T:Tally
        Description
         Text
	  tallies the sequences of B-ranks that occur for 
	  ideals generated by pairs of 
	  monomials in R = S/(d-th powers of the variables), with
	  full complexity (=c); that is,
	  for an appropriate syzygy M of 
	  M0 = R/(m1, m2)
	  where m1 and m2 are monomials of the same degree.
         Example
	  twoMonomials(2,3)
        SeeAlso
	 twoMonomials
     ///
--docTemplate

{*
doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
 Item
Description
 Text
 Code
 Pre
 Example
 CannedExample
Subnodes
Caveat
SeeAlso
///
*}
doc ///
Key
 BGGL
Headline
 Exterior module to linear complex
Usage
 L = BGGL(P,S)
Inputs
 P:Module
  module over an exterior algebra
 S:Ring
  polynomial ring on the same number of vars
Outputs
 L:ChainComplex
  linear chain complex over S corresponding to P
Description
 Text
  Implements the left adjoint BGG functor. The more primitive
  function SymExt does this only for linearly presented exterior
  modules.
 Example
  E = ZZ/101[a,b,c,d, SkewCommutative => true]
  P = E^1/ideal(a*b,c)
  betti res P
  hf(0..3, P)
  S = ZZ/101[x,y,z,w]
  betti BGGL(P,S)
Caveat
 This script really belongs in the BGG package.
///
doc ///
   Key 
    hf
    (hf, Sequence, Module)
    (hf, List, Module)
   Headline 
    Computes the hilbert function in a range of degrees
   Usage
    H = hf(s,P)
   Inputs
    s:Sequence
     or List
    P:Module
     graded module
   Outputs
    H:List
///

doc ///
   Key
    isQuasiRegular
    (isQuasiRegular, Matrix, Module)
    (isQuasiRegular, Sequence, Module)
    (isQuasiRegular, List, Module)
   Headline
    tests a matrix or sequence or list for quasi-regularity on a module
   Usage
    t = isQuasiRegular(ff,M)
   Inputs
    ff:Matrix
    ff:List
    ff:Sequence
    M:Module
   Outputs
    t:Boolean
   Description
    Text
     ff is quasi-regular if the length of ff is <= dim M and the annihilator of ff_i on
     M/(ff_0..ff_(i-1))M has finite length for all i=0..(length ff)-1.
    Example
     kk=ZZ/101;
     S = kk[a,b,c];
     E = S^1/ideal"ab"++S^1/ideal vars S;
     f1 =matrix"a";
     f2 =matrix"a+b,c";
     f3 = matrix"a+b";
     f4 = matrix"a+b, a2+b";
     isQuasiRegular(f1,E)
     isQuasiRegular(f2,E)
     isQuasiRegular(f3,E)
     isQuasiRegular(f4,E)
///

doc ///
   Key
    stableHom
    (stableHom, Module, Module)
   Headline
    map from Hom(M,N) to the stable Hom module
   Usage
    p = stableHom(M,N)
   Inputs
    M:Module
    N:Module
   Outputs
    p:Matrix
     projection from Hom(M,N) to the stable Hom
   Description
    Text
     The stable Hom is Hom(M,N)/T where T is the submodule of homomorphisms
     that factor through a free cover of N (or, equivalently, through any projective)
   SeeAlso
    isStablyTrivial
///
doc ///
   Key
    isStablyTrivial
    (isStablyTrivial, Matrix)
   Headline
    returns true if the map goes to 0 under stableHom
   Usage
    b = isStablyTrivial f
   Inputs
    f:Matrix
     map M to N
   Outputs
    b:Boolean
     true iff f factors through a projective
   SeeAlso
    stableHom
///
doc ///
   Key
    mapToHomomorphism
    (mapToHomomorphism, Matrix)
   Headline
    converts a map f:M\to N to a map S^1 \to Hom(M,N), where M,N are S-modules
   Usage
    g = mapToHomomorphism f
   Inputs
    f:Matrix
     map f: M \to N, S-modules
   Outputs
    g:Matrix
     map g: S^1 \to Hom(M,N)
   Description
    Text
     This is the inverse of the function homomorphism.
     Thus if M,N are S-modules and g:S^1 \to Hom(M,N), then
     f = homomorphism g produces f:M\to N
     and 
     g = mapToHomomorphism f.
     
     The function is used in the script isStablyTrivial
   SeeAlso
    Hom
    homomorphism
    isStablyTrivial
///
end--


-- Noncommutativity of CI operators ---

-- A possible obstruction to the commutativity of the CI operators in codim c
-- is the non-triviality of the map
-- M_(k+4) --> M_k \otimes \wedge^2(S^c)
--in the stable category of maximal Cohen-Macaulay modules.

--In the following example, the map is non-trivial...but it is stably trivial.
--(note that in this case, with c = 3, two of the three alternating products are
--actually equal to 0, so we test only the third.)

--Do we really need all of stable triviality??

restart
loadPackage ("CompleteIntersectionResolutions", Reload=>true)
kk = ZZ/101
S = kk[a,b,c]
ff = matrix"a2,b2,c2"
R = S/ideal ff
M = R^1/ideal"a,bc"
m = 10
F = res(M, LengthLimit => m)
syzygies = apply(1..m, i->coker F.dd_i);
for k from 2 to 5 do(
t1 = makeT(ff,F,k+4);
t2 = makeT(ff,F,k+2);
T2Components = flatten for i from 0 to 1 list(
    for j from i+1 to 2 list map(F_k, F_(k+4), t2_i*t1_j-t2_j*t1_i));
print isStablyTrivial map(syzygies_k, syzygies_(k+4),T2Components_2)
)


-- Experiment with exteriorExtModule
restart
uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"
--viewHelp TateResolution
--viewHelp exteriorExtModule
--P2
S = ZZ/32003[a,b,c]
ff =matrix"a4,b4,c4"
R = S/ideal ff

N = coker vars R
(E,T) = extVsCohomology(ff,highSyzygy N);
P = coker ((cosyzygyRes(3,E)).dd_1);
hf(-3..5,P)
L = BGGL (P,S)
betti L

T1 = coker (res T).dd_4;

D = directSum decomposeByDegree T1;
betti res D

--isIsomorphic(D,T1) --too hard on such big modules.


N = R^1/(ideal vars R)^2
--extVsCohomology(ff, N);
syz1M = coker syz presentation highSyzygy N;
syz2M = coker syz presentation syz1M;
E =  extVsCohomology(ff,highSyzygy N);
E =  extVsCohomology(ff,syz1M);
E =  extVsCohomology(ff,syz2M);


N = coker random(R^{0}, R^{ -2,-3})
T = extVsCohomology(ff,N);
T = extVsCohomology(ff,highSyzygy N);

--the following is a nonsplit example. The pattern is different:
--the top row of the odd ext is everywhere too big by 3. 
--is there an exact triangle 
--of which we are seeing just two of 3 terms?
N = coker map(R^2,R^{3: -1}, matrix"a,b,c;b,c,a")
(E,T) = extVsCohomology(ff,highSyzygy N);
betti (Tr =res T)
betti (Tdr =  res (coker transpose Tr.dd_4, LengthLimit =>10))
p = transpose Tdr.dd_4
Lp = decomposeByDegree coker p;
isSurjection(directSum Lp, coker p)
isIsomorphic(directSum Lp, coker p)
betti res E

syz1M = coker syz presentation highSyzygy N;
syz2M = coker syz presentation syz1M;

(E,T) =  extVsCohomology(ff,highSyzygy N);
(E,T) =  extVsCohomology(ff,syz1M);
E =  extVsCohomology(ff,syz2M);

hf(0..5, ExtModule(highSyzygy N))

-- P3
S = ZZ/101[a,b,c,d]
ff =matrix"a4,b4,c4,d4"
R = S/ideal ff

N = coker map(R^{0}, R^{ -2,-3,-4},matrix"a2+b2,c3+d3,abcd")
E = extVsCohomology(ff,highSyzygy N);
betti res (Hom(E,(ring E)^1))

S = ZZ/101[a,b,c,d]
ff =matrix"a2,b2,c2,d2"
R = S/ideal ff
N = coker map(R^{0,0}, R^{4:-1},matrix"a,b,c,d;b,c,d,a")
E = extVsCohomology(ff,highSyzygy N);


--script says even, odd regs differ by >1!
S = ZZ/101[a,b,c,d]
ff =matrix"a4,b4,c4,d4"
R = S/ideal ff
N = coker random(R^{0,1}, R^{ -1,-2,-3,-4})
M = highSyzygy N -- 


restart
uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"
viewHelp CompleteIntersectionResolutions
  c = 2;
  S = ZZ/101[x_1..x_c, a_(1,1)..a_(c,c)];
  X = matrix{{x_1..x_c}};
  ff = X*map(source X, , genericMatrix(S,a_(1,1),c,c));
  R = S/ideal ff;
  MF = matrixFactorization(ff,highSyzygy coker (R**X))

  c = 3;
  S = ZZ/101[x_1..x_c, a_(1,1)..a_(c,c)];
  X = matrix{{x_1..x_c}};
  ff = X*map(source X, , genericMatrix(S,a_(1,1),c,c));
  R = S/ideal ff;
  MF = matrixFactorization(ff,highSyzygy coker (R**X))



    
