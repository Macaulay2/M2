newPackage(
              "CompleteIntersectionResolutions",
              Version => "1.0", 
              Date => "April 7, 2016",
              Authors => {{Name => "David Eisenbud", 
                        Email => "de@msri.org", 
                        HomePage => "http://www.msri.org/~de"}},
              Headline => "Analyzing Resolutions over a Complete Intersection",
              DebuggingMode => false, --should be false when submitted
	      PackageExports => {"MCMApproximations","BGG"} 
	      )
	  export{
	--things related to Ext over a complete intersection
	   "ExtModule", 
	   "evenExtModule", 
	   "OutRing",
	   "oddExtModule",
	   "ExtModuleData",
	--tools used to construct the "higher matrix factorization"
	--of a high syzygy
	   "matrixFactorization",
	   "Check", -- optional arg for matrixFactorization
	   "highSyzygy",
  	   "Optimism", -- optional arg for highSyzygy etc	   
	   "makeT",
	   "koszulExtension",
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
	--Routines that make other resolutions
	   "Shamash",
	   "layeredResolution",
	   "makeFiniteResolution",	   
	   "makeFiniteResolutionCodim2",	   	   
	   "TateResolution",
	--modules over the exterior algebra
   	   "exteriorTorModule",
	   "exteriorExtModule",	   
	   "makeHomotopies",
	   "makeHomotopies1",
    	   "makeHomotopiesOnHomology",
	   "exteriorHomologyModule",
	   "BGGL",	   
	   "extVsCohomology",
	   "freeExteriorSummand",
       --Representing a module as Ext_R(M,k)
	   "moduleAsExt",
	   "hfModuleAsExt",
	   "complexity",
	--some routines to test particular conjectures
	    "regularitySequence",
	    "extIsOnePolynomial",
	--some families of examples
	   "twoMonomials",
	   "sumTwoMonomials",
         --some utilities
	   "splittings",
	   "S2",
	   "hf",
	   "isQuasiRegular",
	   "makeModule",	   
   	   "isLinear",
	   "cosyzygyRes",	  	   
	   "stableHom",
	   "isStablyTrivial",
	   "dualWithComponents",
	   "HomWithComponents",
	   "tensorWithComponents",
	   "toArray"
	   }

{*
restart
loadPackage("CompleteIntersectionResolutions", Reload=>true)

     S = ZZ/101[x,y,z]
     R = S/ideal"x3,y3"
     M = R^1/ideal(x,y,z)
     ff = matrix{{z^3}}
     --
     m = random(R^3, R^{3:-1})
     ff = matrix{{det m}}
     M = coker m     
     R1 = R/ideal ff
     F = res M
     FF = Shamash(ff,F,5)
     betti FF

    H =  makeHomotopies(ff,F)

     GG = Shamash(R1,F,4)
     betti FF
     betti GG
     ring GG
     apply(length GG, i->prune HH_i FF)
*}

regularitySequence = method()
regularitySequence(List, Module) := (R,M) ->(
    --R = complete intersection list R_(i+1) = R_i/f_(i+1), i= 0..c.
    --M = module over R_c
    --returns the list of pairs {reg evenExtModule M_i, reg oddExtModule M_i}
    --where M_i is the MCM approximation of M over R_i
    if M == 0 then return{- infinity, {}, - infinity, {}};
    em := null;
    om := null;
    c := length R-1;
    (MList,kkk,p) := setupModules(R,M);
    MM := apply(c+1, j->source approximation(pushForward(p_c_j, M),Total =>false));
    MM = select(MM, m-> not isFreeModule m);
    <<"reg even ext, soc degs even ext, reg odd ext, soc degs odd ext"<<endl<<endl;
    scan(reverse MM, m-> (
	    em = evenExtModule m;
	    om = oddExtModule m;
     <<{regularity em, socleDegrees em, regularity om, socleDegrees om})
     <<endl);
    )

Shamash = method()
Shamash(Matrix, ChainComplex,ZZ) := (ff, F, len) ->(
    --Given a 1 x 1 matrix ff over a ring R and a chain complex F
    --admitting a homotopy for ff_0, produce the Shamash complex
    -- F as a chain complex Fbar over Rbar = R/ideal ff.
    R := ring ff;
    deg := (degrees source ff)_0_0;
    H :=  makeHomotopies(ff,F);
    --simplify the notation for the map from F_j to F_i
    d := (i,j) -> if (even(i-j) or (i-j<(-1)) or (F_i==0) or (F_j==0)) 
                     then map(F_i,F_j,0) else 
                          map(F_i,F_j, H#{{(1+i-j)//2},j});
--    error();
        --make the modules
    G := apply(1+len, i->directSum(
	         if even i then apply(1+i//2, j->R^{ -deg*((i-2*j)//2)}**F_(2*j))
	     	 else apply((i+1)//2, j->R^{ -deg*(i-(2*j+1))//2}**F_(1+2*j))));
    --make maps G_(i) to G_(i-1)
    D := i-> if even i then
    	   --if i is even then G_i = F_0++..++F_i, a total of i//2 terms, and G_(i-1) = F_1++.. has i//2-1 terms
           map(G_(i-1),G_i,matrix apply(i//2, p-> apply(1+i//2, q-> d(2*p+1,2*q)))) else
           map(G_(i-1),G_i,matrix apply(1+(i-1)//2, p-> apply(1+i//2, q-> d(2*p,2*q+1))));
    Rbar := ring ff/ideal ff;
    chainComplex apply(len, i-> Rbar**D(i+1))
)

Shamash(Ring, ChainComplex,ZZ) := (Rbar, F, len) ->(
    P := map(Rbar,ring F);
    ff := gens ker P;
    if numcols ff != 1 then error"given ring must be quotient of ring of complex by one element";
    FF := Shamash(ff, F,len);
    P = map(Rbar, ring FF, vars Rbar);
    P FF
)    

layeredResolution = method()
--version that produces the finite layered resolution
layeredResolution(Matrix, Module) := (ff, M) ->(
    --ff is a 1 x c matrix over a Gorenstein ring S
    --M is an S-module annihilated by I = ideal ff.
    --returns a pair (L,aug), where aug: L_0 \to M is the augmentation.
    --Here L_0 = L'_0 ++ B_0, and L' is the resolution of M', the 
    --MCM approximation of M over R' = S/(ideal ff'), and ff' = ff_{0..(c-2)}.
    L := null;
    cod := numcols ff;
    if cod <=1 then return (L = res M, map(M,L_0,id_(L_0)));
    S := ring ff;
    R := S/(ideal ff);
    ff' := ff_{0..cod-2};
    R' := S/(ideal ff');
    p:= map(R,R');
    q := map(R',S);
        
    MR := prune R**M;
    MR' := prune(R'**M);
    (alpha, beta) := approximation MR';
    B0 := source beta;
    M' := source alpha;
--    assert(M' == prune M');

    gamma := map(MR', M'++B0, (alpha)|beta);
    BB1 := ker gamma;
    B1 := minimalPresentation BB1;
--    assert(isFreeModule B1);
    psib :=  inducedMap(M' ++ B0, BB1)*(B1.cache.pruningMap);
    psi := psib^[0];
    b := psib^[1];
--    assert(source psi == B1 and source b == B1);
--    assert(target psi == M' and target b == B0);
    M'S := pushForward(q,M');
    bS := substitute(b,S);
    B0S := target bS;
    B1S := source bS;    
    KK := koszul(ff');
    B := chainComplex{bS};
    
    (L',aug') := layeredResolution(ff', M'S);
    assert(target aug' == M'S);
    psiS0 := map(M'S, B1S, sub(matrix psi,S));
    psiS := psiS0//aug';
    Psi1 := extend(L',B[1],matrix psiS);
    Psi2 := Psi1**KK;
    Psi := extend(L',L'**KK, id_(L'_0))*Psi2;
    L = cone Psi; -- L', the target of Psi, is the first summand, so this is L_0==L'_0++B_0
    assert(L_0 == L'_0 ++ B_0);
    m := (sub((matrix alpha),S)*matrix aug') |sub(matrix beta,S);
    aug := map(M,L'_0++B_0,m);
--Check exactness
--    scan(length L -1, s->assert( HH_(s+1) L == 0));
    (L,aug)
    )



layeredResolution(Matrix, Module, ZZ) := (ff, M, len) ->(
    --ff is a 1 x c matrix over a Gorenstein ring S and ff' = ff_{0..(c-2)}, ff'' = ff_{c-1}.
    --R = S/ideal ff
    --R' = S/ideal ff'
    --NOTE R =!= R'/ideal ff''; we need to use a map to go between them.
    --M is an MCM R-module.
    --The script returns a pair (L,aug), where L is the first len steps of an R-free resolution of M 
    --and aug: L_0 \to M is the augmentation.
    -- Let
    --        B_1 --> B_0 ++ M' --> M
    -- be the MCM approximation of M over R'.
    -- If L' is the layered R'-free resolution of M', then
    --     L_0 = R\otimes (L'_0 ++ B_0),
    --     L_1 = R\otimes (L'_1 ++ B_1).
    -- and L is the Shamash construction applied to the box complex.
    -- The resolution is returned over the ring R.
    cod := numcols ff;
    R := ring M;
    S := if cod >0 then ring ff else R;
    StoR := map(R,S);
    MS := pushForward(StoR, M);
    
    if cod == 0 then (
    	L := res(M,LengthLimit => len);
    	return (L, map(M, L_0, id_(L_0))));
    ff' := ff_{0..cod-2};
    R' := S/ideal ff';
    ff'' := R'** ff_{cod-1};
--    R1 := R'/ideal ff'';
    
    R'toR := map(R,R');
--    MR := R**M;
    MR':= pushForward(R'toR,M);
    (alpha, beta) := approximation MR';
    B0 := source beta;
    M' := source alpha;
    gamma := map(MR', M'++B0, (alpha|beta));
    BB1 := ker gamma;
    B1 := minimalPresentation BB1;
    psib :=  inducedMap(M' ++ B0, BB1)*(B1.cache.pruningMap);
    psi := psib^[0];
    b := psib^[1];
--error();
(L',aug') := layeredResolution(ff',M',len);
--L' := res(M', LengthLimit=> len);
--    aug' = map(M', L'_0, id_(L'_0));
assert(ring L' === R');
assert(ring aug' === R');
assert(ring b === R');
    B := chainComplex {b};
    Psi := extend(L', B[1], matrix(psi//aug'));
    box := cone Psi;
    L =  Shamash(R, box, len);    
    aug := map(M, L_0, 
          R'toR matrix( 
	      map(MR',M'++B0, (alpha|beta))*map(M'++ B0, box_0, aug'++id_(B0))));
    (L, aug)
    )


dualWithComponents = method()
dualWithComponents Module := M -> (
   if not isDirectSum M then return dual M else (
	directSum(components M/dualWithComponents)))

tensorWithComponents = method()
tensorWithComponents(Module, Module) := (M,N) ->(
   if not isDirectSum M and not isDirectSum N then return M**N else
      directSum flatten apply(components M, m->apply(components N, n->(
	       tensorWithComponents(m,n)))))

HomWithComponents = method()
HomWithComponents (Module, Module) :=  (M,N) ->(
   if not isDirectSum M and not isDirectSum N then return Hom(M,N) else
      directSum flatten apply(components M, m->apply(components N, n->(
	       HomWithComponents(m,n)))))

stableHom = method()
stableHom(Module, Module) := (M,N)->(
    --returns the map from Hom(M,N) to the stable Hom
    H := Hom(M,N);
    if isFreeModule M then return map((ring M)^0, H, 0);
    p := map(N, cover N, 1);
    map(coker Hom(M,p), Hom(M,N), 1))

isStablyTrivial = method()
isStablyTrivial Matrix := f ->(
   -- f: M \to N is given.
   -- represent f as an element of Hom, that is, as a map (ring M)^1 \to Hom(M,N) 
   --then apply stableHom.
   f1 := homomorphism' f;
   (stableHom(source f, target f)*f1) == 0)


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

  
evenExtModule = method(Options =>{OutRing => 0})
evenExtModule Module := opts -> M -> (
     --If M is a module over a complete intersection R
     --of codim c, the script returns 
     --Ext^(even)(M,(ring M)^1/(ideal vars ring M))
     --as a module generated in degree 0
     --over the polynomial ring kk[X_1..X_(codim R)],
     --where the vars have degree 1
     --unless the option Outring => outring is given, with outring being
     --a polynomial ring with numGens ring E, in chich case this ring is used.
     E := ExtModule M;
     P := positions(flatten degrees E, even);
     Ee:=prune image (E_P);
     T := ring E;
     if class opts#OutRing === PolynomialRing then T1 := opts#OutRing else
     (
     kk:= coefficientRing T;
     X := symbol X;
     T1 = kk[X_0..X_(numgens T -1)]
     );
     v1 := map(T1, T, vars T1, DegreeMap => i->{(first i)//2});
     coker v1 presentation Ee
     )


oddExtModule = method(Options =>{OutRing => 0})
oddExtModule Module := opts -> M -> (
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
     if class opts#OutRing === PolynomialRing then T1 := opts#OutRing else
     (
     kk:= coefficientRing T;
     X := symbol X;
     T1 = kk[X_0..X_(numgens T -1)]
     );
     v1 := map(T1, T,vars T1, DegreeMap => i->{(first i)//2});
     coker v1 presentation Eo
     )


makeT = method()
makeT(Matrix, ChainComplex,ZZ) := (ff,F,i) ->(
     {*
     If ff is an c x 1 matrix and
     F is a chain complex
     over R = S/(ideal ff), 
     of codim c this returns a list of the c ci-operators
     F_i \to F_{i-2}
     corresponding to the entries of ff.
     *}
     c := numcols ff;
     degsff := flatten((degrees ff)_1);
     R := ring F;
     S := ring ff;
     complete F;
     minF := min F;
     d0 := sub(F.dd_i, S);
     d1 := sub(F.dd_(i-1), S);
     Ftar := target d1;
     Fsour := source d0;
     d2 := d1*d0;
     T := (d2//(ff**Ftar));
     I := id_(source ff);
     u := apply(c, j-> (I^{j}**Ftar)*T);
     --check: is d1*d0 = sum ff_{i}*u_i 
     if d1*d0 != map(Ftar, Fsour, sum(c, i-> u_i**ff_{i})) then 
                  error{"doesn't add up"};
     ret := map(R,S);
     apply(u, u1 -> ret u1)
     )

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
    --h: a map, the sum of the
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
    if fail == true then error("cannot complete MF");
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
--        S^{ -degs#p}**target dpartial#p,
        tensorWithComponents(S^{ -degs#p},target dpartial#p),
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
--error();
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
    --makes a list of the components of h, preserving the direct sum decompositions
    --of the sources and targets of the components.
    h := mf_1;
    apply(#components source h,
        p -> (
          map(directSum ((components target h)_(toList(0..p))),
              directSum(((components source h)/components)_p),
	      --(target h)^(toArray toList(0..p))*h*((source h)_[p]))))
              h_[p]^(toArray toList(0..p)))))
    	       )
-- the commented line was necessary when we had ^[ ] being frobeniusPower in our init.m2

///  
restart
loadPackage("CompleteIntersectionResolutions", Reload =>true)
setRandomSeed 0
kk=ZZ/101
S = kk[a,b]
ff = matrix"a4,b4"
R = S/ideal ff
N = coker vars R
M = highSyzygy N
mf = matrixFactorization(ff, M)
(hMaps mf)_0
(hMaps mf)_1
///

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
--	 <<"module with presentation matrix" <<endl;
	 <<toString presentation M);
     {E0,E1,r0,r1}
     )
    
mfBound = method()
mfBound Module := M0 ->( 
    --gives (conjectural) bound for which syzygy
    --of M0 will be a high syzygy
E := ExtModuleData M0;
max(2*E_2, 1+2*E_3)
)

highSyzygy = method(Options=>{Optimism => 0})
highSyzygy Module := opts -> M0 ->(
    --with increment => 0 (the default) this gives our conjectural
    --bound, which is best possible.
    -- But if that's not good enough, use Optimism=>-1 etc
    len := 1+mfBound M0-opts#Optimism;
    F := res(M0, LengthLimit => len);
    coker F.dd_len)


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






--the following version makes inhomgeneous maps!
makeHomotopies = method()
makeHomotopies (Matrix, ChainComplex) := (f,F) ->
     makeHomotopies(f,F, max F)
makeHomotopies(Matrix, ChainComplex, ZZ) := (f,F,d) ->(
     --given a 1 x lenf matrix f and a chain complex 
     -- F_min <-...,
     --the script attempts to make a family of higher homotopies
     --on F for the elements of f.
     --The output is a hash table {{J,i}=>s), where     
     --J is a list of non-negative integers, of length = ncols f
     --and s is a map F_i->F_(i+2|J|-1) satisfying the conditions
     --s_0 = differential of F
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
     --hashTable pairs H
     S := ring f;
     degs := apply(flist, fi -> degree fi); -- list of degrees (each is a list)
     hashTable apply(keys H, k->
     {k, map(F_(k_1+2*sum (k_0)-1), 
	     tensorWithComponents( S^(-sum(#k_0,i->(k_0)_i*degs_i)),F_(k_1)), 
				         H#k)})
     )
///
restart
notify=true
uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"
check"CompleteIntersectionResolutions"
loadPackage("CompleteIntersectionResolutions", Reload =>true)
S = kk[a,b,c]
ff = matrix"a4,b4,c4"
R = S/ideal ff
N = coker vars R
MR=highSyzygy N
M = pushForward(map(R,S), MR);
F = res M
H = makeHomotopies1(ff, F)
H = makeHomotopies(ff, F)

scan(keys H, k-> if not isHomogeneous H#k then print k)

///


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
		     error());
	       H#{j,i} = h;    
	       ));
     S := ring f;
     degs := apply(flist, fi -> degree fi); -- list of degrees (each is a list)
     hashTable apply(keys H, k->
     {k, map(F_(k_1+1), 
	     tensorWithComponents(S^(-degs_(k_0)),F_(k_1)), 
				         H#k)})
--     hashTable pairs H
     )


makeHomotopiesOnHomology = method()
makeHomotopiesOnHomology (Matrix, ChainComplex) := (ff,C)->(
    --returns a pair (H,h) whose first element is the hashTable of homology of C
    --and whose second element is the hashTable of 1-step homotopies for ff
    h0 := makeHomotopies1(ff,C);
    homDegs := sort unique ((keys h0)/(k->k_1));
    m := max homDegs;
    H := hashTable({{m+1, HH_(m+1) C}} | apply(homDegs, i->{i, HH_i C}));
    h := hashTable apply(keys h0, k->{k,
	  map(H#(k_1+1), H#(k_1), 
         (matrix h0#k//(generators H#(k_1+1)) *generators H#(k_1)))
     	 });
     (H,h)
     )


exteriorHomologyModule = method()
exteriorHomologyModule(Matrix,ChainComplex) := (ff, C) ->(
    {*
    Assuming that the elements of the 1xc matrix ff are null-homotopic
    on C, the script returns their direct sum as a module over 
    a new ring, consisting of ring C with c exterior variables adjoined.
    *}
--Construct the homology of C and the action of the homotopies on it
   (H,h) := makeHomotopiesOnHomology(ff,C);
--now make a ring "like ring C" but with some exterior variables.
    S :=ring C;
    kk := coefficientRing S;
    numS := numgens S;
    J := ideal S;
    X := symbol X;
    e := symbol e;
    n := numgens S;
    Sdegs := apply((degrees vars S)_1, i->{i_0,0});
    c := rank source ff;
    Edegs := apply((degrees ff)_1, i->{i_0,1});
    SE0 := kk[X_0..X_(n-1),e_0..e_(c-1), 
	SkewCommutative=>toList(numS..numS+c-1),
	Degrees =>Sdegs|Edegs];
    S0 := (ring presentation S);
    bringJ := map(SE0, S0, {X_0..X_(n-1)},DegreeMap=>i->{i_0,0});
    SE := SE0/bringJ(J);
    E := (vars SE)_{numS..numS+c-1};
    F := apply(c, j->source E_{j});
--bring H,h over to SE with appropriate degrees (second degree = homological degree)
    toSE := map(SE, S, toList(X_0..X_(numS-1)), DegreeMap=>i->{i_0,0});    
    HE := hashTable apply(keys H, k->
	{k, SE^{{0,-k}}**subquotient(toSE generators H#k, toSE relations H#k)});
    hE := hashTable (apply(keys h, hk-> {hk, map(HE#(hk_1+1), HE#(hk_1), toSE matrix h#hk)}));

    phi := hashTable apply(keys h, k -> 
	{k,  map(HE#(1+k_1), 
		 F_(k_0)**HE#(k_1), 
		 hE#k)
	 }
                         );
   makeModule(HE,E,phi)
)

makeModule = method()
makeModule(HashTable, Matrix, HashTable) := (T,E,phi) ->(
    -- in this version:
    -- RE is a bigraded ring
    -- E: \oplus RE^{d_i} \to RE^1 is a matrix of c variables from RE
    -- T is a hashTable of m pairs {i, t_i}, where the t_i are RE-modules
    -- phi is a hash-table of maps phi#{j,i}: t_i**F_j\to t_(i+1)
    -- where F_j = source (E_j = matrix {{e_j}})
    -- such that the maps 
    --              p#{j,i} = (E_j || -phi#{j,i}): t_i**F_j \to t_i++t_(i+1),
    -- are homogeneous.
    -- The script returns M = \oplus_i T_i
    -- as an RE-module,
    -- computed as the quotient of P := \oplus T_i
    -- obtained by factoring out the sum of the images of the maps p#{j,i}
    -- In our application, (T#i)_1 has second degree i.
    sourceKeys := (sort unique (keys phi/(k->k_1)));
    targetKeys := (sort unique (keys phi/(k->1+k_1)));
    if length sourceKeys ==0  then return T#(min sourceKeys); -- handles case of only 1 module    
    Pkeys := {min targetKeys-1}|targetKeys;
    m := length Pkeys;
    c := rank source E;
    P := directSum apply(Pkeys , i -> T#i);
    fir := new Array from 0..m-2;
    las := new Array from 1..m-1;
    F := apply(c, j-> source E_{j});
    Q := apply(c, j-> directSum apply(sourceKeys, i-> F_j**T#i));
    f := apply(c, j -> 
	 map(P, Q_j, 
	 P_las * directSum apply(sourceKeys, i->phi#{j,i})
		 ));
    g := apply(c, j ->
	  map(P,Q_j, 
              P_fir*directSum apply(sourceKeys, i->E_{j}**T#i)
		  ));
    M := P/sum(apply (c, j->image(f_j-g_j)));
    M
     )

exteriorTorModule = method()


exteriorTorModule(Matrix, Module) := (f,M) -> (
     --Write Tor_S(M,k) as a module over Tor(S/(f),k) = \wedge V:
     --f is a matrix with entries that are homotopic to zero on F
     --Typically, F is a resolution of a module annihilated by
     --the entries of f.
     S := ring M;
     n := numgens S;
     k := coefficientRing S;     
     F := complete res M;
     H := makeHomotopies1(f,F);
     e := symbol e;
     E := k[e_0..e_(numcols f -1), SkewCommutative => true];
     red := map(E,S, {n:0},DegreeMap=>d->{0});
     --problem: the following indexes T starting with 0. But we shouldn't need it!
     T := hashTable apply(toList(0..max F),i->{i,E^{ -i}**red F_i}); 
     goodkeys := select(keys H, k->k_1>=0);
     Hk := hashTable apply(goodkeys, h-> (h, red H#h));
     --Hk(j,i) is the homotopy for f_j from F_i**k to F_(i+1)**k,
     --defined for i from 0 to max F-1.
     TE :=makeModule(T, vars E, Hk);
     TE
)

exteriorTorModule(Matrix,Module,Module) := (ff,M,N) ->(
    --M,N are modules in a ring S;
    --ff is a sequence of elements in S that annihilate M and N;
    --The script defines a new ring
    --SE =kk[X_0..X_(n-1), e_0..e_c] mod the defining relations of S,
    --with with degree e_j = degree f_j.
    --which is is an exterior algebra over S on c:=numcols ff generators e_0..e_(c-1),
    --
    --the script returns Tor^S(M,N) as an SE-module with structure induced
    --by the homotopies of the resolution of the first factor; note that
    --this is NOT symmetric in the two factors.
    --NOTE:
    --h0#{i,j} is the homotopy for f_i starting from the j-th step of the resolution.    
    Mres := res M;
    complete Mres;
    exteriorHomologyModule(ff, Mres**N)
    )

exteriorExtModule = method()
exteriorExtModule(Matrix, Module) := (f,M) -> (
    --dual of exteriorTorModule
    E := exteriorTorModule(f, M);
    omega := (ring E)^{numgens ring E};
    Hom(E,omega))

exteriorExtModule(Matrix, Module, Module) := (ff, M,N)->(
    --M,N are modules in a ring S;
    --ff is a sequence of elements in S that annihilate M and N;
    --The script defines a new ring
    --SE =kk[X_0..X_(n-1), e_0..e_c] mod the defining relations of S,
    --with with degree e_j = degree f_j.
    --which is is an exterior algebra over S on c:=numcols ff generators e_0..e_(c-1),
    --
    --the script returns Ext_S(M,N) as an SE-module with structure induced
    --by the homotopies of the resolution of the first factor; note that
    --this is NOT symmetric in the two factors.
    --NOTE:
    --h0#{i,j} is the homotopy for f_i starting from the j-th step of the resolution.    
    Mres := complete res M;
    exteriorHomologyModule(ff, Hom(Mres,N))
    )




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

S2 = method()
S2(ZZ,Module) := Matrix => (b,M)-> (
     --returns a map M --> M', where M' = \oplus_{d>=b} H^0(\tilde M).
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


TateResolution = method()
TateResolution(Module,ZZ,ZZ) := (M,low,high) ->(
         d := transpose ((res(M, LengthLimit => high)).dd_high);
	 F := res (coker d, LengthLimit =>(high-low+2));
	 complete F;
         T := (chainComplex reverse apply(high-low+1, j->transpose (F.dd_j)))[-low];
	 T
         )
TateResolution(Module,ZZ) := (M,b) -> TateResolution(M,b,b)
TateResolution(Module) := M-> TateResolution(M,-5,5)

{*Old version returned a betti table; now use 
betti TateResolution
instead.

TateResolution0 = method()
TateResolution0(Module,ZZ,ZZ) := (M,lower,upper) ->(
    d := transpose (res(M, LengthLimit => upper)).dd_upper;
    betti res (coker d, LengthLimit =>upper+lower)
    )
*}
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

makeFiniteResolutionCodim2 = method()
makeFiniteResolutionCodim2(List,Matrix) := (MF,ff) -> (
    --given a codim 2 matrix factorization, makes all the maps
    --that are relevant, as in 4.2.3 of Eisenbud-Peeva 
    --"Minimal Free Resolutions and Higher Matrix Factorizations"
    c := rank source ff; -- the codim
    if c !=2 then error"requires a codim 2 complete intersection";
    S := ring MF_0;
    bb := bMaps MF;
    ps := psiMaps MF;
    h := hMaps MF;    
    c' := complexity MF; -- the complexity
    if c'<2  then return {MF_0,MF_1};

    --from here on c=c'=2, and we build the maps over S
    --write Bsp for B_s(p)
    B01 := target bb_0;
    B02 := target bb_1;
    B11 := source bb_0;
    B12 := source bb_1;
    f1 := (entries ff)_0_0 ;-- first elt of the reg seq
    f2 := (entries ff)_0_1 ;-- second elt of the reg seq    
    deg1 := (degree f1)_0 ;
    B02' := S^{ -deg1}**B02;
    B12' := S^{ -deg1}**B12;    
    --next two lines are really cosmetic
    B13 := B02'; 
    B23 := B12';
    F0 := B01++B02;
    F1 := directSum{B11,B12,B13};
    F2 := B23;
    hh0 := h_0;
    hh1' := h_1;
    d1 := map(F0,F1,(bb_0 | ps_0 | map(B01,B02',0)) || (map(B02,B11,0)| bb_1 | f1*map(B02,B02',1)));
    d2 := map(F1,F2, map(B11,B12', h_0*ps_0) || -f1*map(B12,B12',1) || (S^{ -deg1}**bb_1));
    F := chainComplex{d1,d2};
    homot := makeHomotopies1(ff, F);
    --note that the following are various components of homotopies related to f2!
    hf1 := homot#{1,0};
    hf2 := homot#{1,1};    
    hh1 := hf1^[0,1];
    vv1 := homot#{1,1};
    vv := vv1_[0];
    out := hashTable{"resolution" => F,
	"partial" => bb_0,
	"b" => bb_1,
	"mu" => hh0,
	"h1" => hh1,
	"h1'" =>hh1',
	"alpha" => hh1_[0]^[0],
	"tau" => hh1_[1]^[0],
	"sigma" => hh1_[1]^[1],
    	"u" => -hf1_[0]^[2],
	"v" => vv,
	"psi" => ps_0,
    	"X" => -hf1_[1]^[2],
	"Y" => hf2_[1]
	};
    --make sure the formula for the f1 homotopy works:
    hconst := {
	    map(F_1,S^{ -deg1}**F_0,
	    (map(B11,B01,out#"alpha")    | map(B11,B02, out#"tau")) ||
	    (map(B12,B01,out#"v"*out#"mu") | map(B12,B02, out#"sigma"))||
	    (map(B13,B01,-out#"u")       | map(B13,B02, -out#"X"))
	    ),
	    map(S^{deg1}**F_2, F_1, out#"v" | out#"Y" | out#"sigma")
	    };
    assert(F.dd_1*hconst_0 == map(F0, S^{ -deg1}**F_0, f2*id_(F_0)));
    assert(hconst_0*F.dd_1+F.dd_2*hconst_1 == f2*id_(F_1));
    assert(hconst_1*F.dd_2 == map(S^{deg1}**F_2, F_2, f2*id_(F_2)));
    if hh1 !=hh1' then 
           <<"homotopy for first f1 (which is ff_0) 
	   had component mapping to e_1**B_0(2)."<<endl;
    out
    )


complexity = method()
--complexity of a module over a CI ring
complexity Module := M-> dim evenExtModule M
complexity List := mf -> (
    br := BRanks mf;
    L := select(br, pair->pair_0!=0);
    #L)

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
    E := exteriorExtModule(ff,MS);
    T := exteriorTorModule(ff,MS);
TE := (betti (S^{-5})[6])**betti TateResolution(E,-5,5);
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

extIsOnePolynomial = method()
extIsOnePolynomial Module := M ->(
    Ee := evenExtModule M;
    Eo := oddExtModule M;
    pe := hilbertPolynomial(Ee, Projective => false);
    po := hilbertPolynomial(Eo, Projective => false);
    z := local z;
    U := QQ[z];
    V := ring pe;
    s := map(U,V,{U_0});
    pe = s pe;
    po = s po;
    H := sub(pe, {z =>z/2});
    (H, H == sub(po, {z =>z/2-1/2}))
    )

///
restart
--notify=true
uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"
loadPackage("CompleteIntersectionResolutions", Reload=>true)
check "CompleteIntersectionResolutions"
///


-----------------------------
--------Documentation-----------documentation--DOCUMENT
--------------------------------

--<<docTemplate
{*
restart
loadPackage ("CompleteIntersectionResolutions", Reload=>true)
uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"
viewHelp "CompleteIntersectionResolutions"
check "CompleteIntersectionResolutions"
*}

beginDocumentation()

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
  
  Most of our routines for constructing resolutions over complete intersections
  work with a polynomial ring S and a complete
  intersection R = S/(ideal ff), where 
  $$
  ff = matrix\{\{f_1,\dots,f_c\}\}
  $$
  is a 1-rowed
  matrix whose entries are (sufficiently general) generators
  of a complete intersection ideal, usually all of the same degree.
 Text
  Most of this package is related to the notions introduced and studied in the
  Springer Lecture Notes, ``Minimal Free Resolutions over Complete Intersections''
  by David Eisenbud and Irena Peeva (2016).

  The routines fall into several groups:
  --  
 Text
  @SUBSECTION "Routines to analyze Ext_R(M,k) as a module over the ring of operators"@
 Text
  @UL {
  {TO "ExtModule"},
  {TO "evenExtModule"},
  {TO "oddExtModule"},
  {TO "ExtModuleData"},
  {TO "complexity"}
  }@
 Text
  @SUBSECTION "Representing a module as Ext_R(M,k)"@
 Text
  @UL {
  {TO "moduleAsExt"},
  {TO "hfModuleAsExt"}
  }@
 Text
  @SUBSECTION "Routines Related to Particular Conjectures"@
 Text
  @UL {
  {TO "regularitySequence"},
  {TO "extIsOnePolynomial"}
  }@
 Text
  @SUBSECTION "Routines that make resolutions of various kinds"@
 Text
  @UL {
  {TO "TateResolution"},
  {TO "Shamash"},
  {TO "layeredResolution"},
  {TO "makeFiniteResolution"},
  {TO "makeFiniteResolutionCodim2"}
  }@
 Text
  @SUBSECTION "Tools for construction of higher matrix factorizations"@
 Text
  @UL {
  {TO "matrixFactorization"},
  {TO "highSyzygy"},
  {TO "koszulExtension"},
  {TO "makeT"}
  }@
 Text
  @SUBSECTION "Tools to unpack the info in higher matrix factorizations"@
 Text
  @UL {
  {TO "BRanks"},
  {TO "ARanks"},
  {TO "bMaps"},
  {TO "dMaps"},
  {TO "psiMaps"},
  {TO "hMaps"},
  {TO "mfBound"},
  {TO "finiteBettiNumbers"},
  {TO "infiniteBettiNumbers"}
  }@
 Text
  @SUBSECTION "Ext_S(M,k), Tor^S(M,k), homology and linear resolutions as modules over the exterior algebra"@
 Text
  @UL {
  {TO "exteriorExtModule"},
  {TO "exteriorTorModule"},
  {TO "exteriorHomologyModule"},
  {TO "BGGL"},
  {TO "extVsCohomology"},
  {TO "freeExteriorSummand"}
  }@
 Text
  @SUBSECTION "Routines for general module theory"@
 Text
  @UL {
  {TO "S2"},
  {TO "isQuasiRegular"},
  {TO "stableHom"},
  {TO "isStablyTrivial"},  
  {TO "makeModule"},  
  {TO "isLinear"}
  }@
 Text
  @SUBSECTION "Utilities"@
 Text
  @UL {
  {TO "cosyzygyRes"},
  {TO "dualWithComponents"},
  {TO "HomWithComponents"},
  {TO "tensorWithComponents"},  
  {TO "toArray"}
  }@
 Text
  @SUBSECTION "Some families of Examples"@
 Text
  @UL {
  {TO "twoMonomials"},
  {TO "sumTwoMonomials"}
  }@
 
 Text  
  The construction of the
  matrix factorizations for high syzygies of a module N,
  introduced in the
  paper
  "Matrix Factorizations in Higher Codimension"
  by Eisenbud and Peeva. The routine ``mfBound'' determines
  which syzygy to take.
  The routine matrixFactorization constructs
  the higher matrix factorization 
  of a module over R defined by Eisenbud and Peeva in the 2016 Springer Lecture Notes
  ``Minimal Free Resolutions over Complete Intersections''.
  The ranks of the stack of matrices b_p that are used 
  in the construction of the matrix factorization, and the various matrices
  themselves, are obtained from the routines BRanks, ARanks, bMaps, dMaps, psiMaps, hMaps
  (the notation is explained in the Lecture Notes). 
  
  Here is an example of a matrix factorization in codimension 2:
 Example
  setRandomSeed 0
  c = 2;
  S = ZZ/101[x_1..x_c, a_(1,1)..a_(c,c)];
  X = matrix{{x_1..x_c}};
  ff = X*map(source X, , genericMatrix(S,a_(1,1),c,c));
  R = S/ideal ff;
  mbound = mfBound coker (R**X)
  F = res(coker (R**X) , LengthLimit =>mbound+1);
  M = coker F.dd_(mbound+1);
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
  module Ext_R(M,k). TateResolution returns 
  a specified part of the Tate resolution of a 
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
  
Caveat
  Unless the complete intersection is homogeneous 
  AND generated by elements of a single degree,
  it may not be possible to choose sufficiently general HOMOGENEOUS generators
  for some of our construction routines to work, 
  even when the ideal of the complete intersection is homogeneous, so our examples
  in the routines for are primarily using complete intersections of equal degree.
  The theory takes place in the local case, however, where this is not a problem.
///


doc///
   Key
    regularitySequence
    (regularitySequence, List,Module)
   Headline
    regularity of Ext modules for a sequence of MCM approximations
   Usage
    L = regularitySequence (R,M)

   Inputs
    R:List
     list of rings R_i = S/(f_0..f_{(i-1)}), complete intersections
    M:Module
     module over R_c where c = length R - 1.
   Outputs
    L:List
     List of pairs {regularity evenExtModule M_i, regularity oddExtModule M_i)
   Description
    Text
     Computes the non-free parts M_i of the MCM approximation to M over R_i, 
     stopping when M_i becomes free, and
     returns the list whose elements are the pairs of regularities, starting
     with M_{(c-1)}
     Note that the first pair is for the 
    Example
     c = 3;d=2
     R = setupRings(c,d);
     Rc = R_c
     M = coker matrix{{Rc_0,Rc_1,Rc_2},{Rc_1,Rc_2,Rc_0}}
     regularitySequence(R,M)
   SeeAlso
    approximation
    auslanderInvariant
///


doc ///
   Key
    dualWithComponents
    (dualWithComponents, Module)
   Headline
    dual module preserving direct sum information
   Usage
    N = dualWithComponents M
   Inputs
    M:Module
   Outputs
    N:Module
   Description
    Text
     If M is a direct sum module (isDirectSum M == true) then
     N is the direct sum of the duals of the components (and this is done recursively).
     This SHOULD be built into dual M, but isn't as of M2, v. 1.7
   SeeAlso
    HomWithComponents
    tensorWithComponents
///
doc ///
   Key
    HomWithComponents
    (HomWithComponents, Module, Module)
   Headline
    computes Hom, preserving direct sum information
   Usage
    H = Hom(M,N)
   Inputs
    M:Module
    N:Module
   Outputs
    H:Module
   Description
    Text
     If M and/or N are direct sum modules (isDirectSum M == true) then
     H is the direct sum of the Homs between the components.
     This SHOULD be built into Hom(M,N), but isn't as of M2, v. 1.7
   SeeAlso
    tensorWithComponents
    dualWithComponents
///
doc ///
   Key
    tensorWithComponents
    (tensorWithComponents, Module, Module)
   Headline
    forms the tensor product, preserving direct sum information
   Usage
    T = tensor(M,N)
   Inputs
    M:Module
    N:Module
   Outputs
    T:Module
   Description
    Text
     If M and/or N are direct sum modules (isDirectSum M == true) then
     T is the direct sum of the tensor products between the components.
     This SHOULD be built into M**N, but isn't as of M2, v. 1.7
   SeeAlso
    HomWithComponents
    dualWithComponents
///


doc///
   Key
    makeHomotopiesOnHomology
    (makeHomotopiesOnHomology, Matrix, ChainComplex)
   Headline
    Homology of a complex as exterior module
   Usage
    (H,h) = makeHomotopiesOnHomology(ff, C)
   Inputs
    ff:Matrix
     matrix of elements homotopic to 0 on C
    C:ChainComplex
   Outputs
    H:HashTable
     Homology of C, indexed by places in the C
    h:HashTable
     homotopies for elements of f on the homology of C
   Description
    Text
     The script calls makeHomotopies1 to produce homotopies for the ff_i on C,
     and then computes their action on the Homology of C.
   SeeAlso
    exteriorTorModule
    exteriorExtModule
///
doc ///
   Key
    makeModule
    (makeModule, HashTable, Matrix, HashTable)
   Headline
    makes a Module out of a collection of modules and maps
   Usage
    M = makeModule(H,E,phi)
   Inputs
    H:HashTable
     graded components that are modules, to make into as single module
    E:Matrix
     Matrix of variables whose action will defined
    phi:HashTable
     maps between the graded components that will be the action of the variables in E
   Outputs
    M:Module
     graded modules whose components are given by H
   Description
    Text
     The Hashtable H should have consecutive integer keys i_0..i_0, say, with values
     H#i that are modules over a ring SE whose variables include the elements of E.
     E: \oplus SE^{d_i} \to SE^1 is a matrix of c variables from SE
     H is a hashTable of m pairs {i, t_i}, where the t_i are RE-modules, 
     and the i are consecutive integer.
     phi is a hash-table of homogeneous maps phi#{j,i}: H#i**F_j\to H#(i+1)
     where F_j = source (E_{j} = matrix {{e_j}}).
     Thus the maps
     p#{j,i} = (E_j || -phi#{j,i}): t_i**F_j \to t_i++t_{(i+1)},
     are homogeneous.
     The script returns M = \oplus_i T_
     as an SE-module,
     computed as the quotient of P := \oplus T_i
     obtained by factoring out the sum of the images of the maps p#{j,i}
           
     The Hashtable phi has keys of the form {j,i} where j runs from 0 to c-1, 
     i and i+1 are keys of H, 
     and phi#{j,i} is the map from (source E_{i})**H#i to H#(i+1) that will be
     identified with the action of E_{j}.
     
     The script is used in both the singly graded case, for example in
     exteriorTorModule(ff,M)
     and in the bigraded case, for example in
     exteriorTorModule(ff,M,N).
     
     In the following we use makeModule to construct by hand
     a free module of rank 1 over the exterior algebra on x,y,
     starting with the construction of a module over a bihomogeneous ring.
    Example
     SE = ZZ/101[a,b,c,x,y,Degrees=>toList(3:{1,0})|toList(2:{1,1}), SkewCommutative=>{x,y}]
     RE = SE/ideal"a2,b2,c2"
     T = hashTable {{0,RE^1},{1,RE^{2:{ -1,-1}}}, {2,RE^{{ -2,-2}}}}
     E = matrix{{x,y}}
     F=apply(2, j-> source E_{j})
     phi = hashTable{ {{0,0}, map(T#1, F_0**T#0, T#1_{0})},{{1,0}, map(T#1, F_1**T#0, T#1_{1})},{{0,1}, map(T#2, F_0**T#1, T#1^{1})}, {{1,1}, -map(T#2, F_1**T#1, T#1^{0})}}
     apply(keys phi, k->isHomogeneous phi#k)
     X = makeModule(T,E,phi)
     isHomogeneous X
     q = map(ZZ/101[x,y, SkewCommutative => true, DegreeMap => d->{d_1}], ring X, {3:0,x,y})
     prune coker q presentation X
   SeeAlso
    exteriorHomologyModule
    exteriorTorModule
    exteriorExtModule
///


doc ///
   Key
    exteriorHomologyModule
    (exteriorHomologyModule, Matrix, ChainComplex)
   Headline
    Make the homology of a complex into a module over an exterior algebra
   Usage
    M = exteriorHomologyModule(ff, C)
   Inputs
    ff:Matrix
     Matrix of elements that are homotopic to 0 on C
    C:ChainComplex
   Outputs
    M:Module
   Description
    Text
     Assuming that the elements of the 1xc matrix ff are null-homotopic
     on C, the script returns the direct sum of the homology of C as a module over 
     a new ring, consisting of ring C with c exterior variables adjoined.
     The script is the main component of exteriorTorModule
   SeeAlso
    exteriorTorModule
    makeHomotopiesOnHomology
///


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
    highSyzygy
    exteriorExtModule
///

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
    makeFiniteResolutionCodim2
    (makeFiniteResolutionCodim2, List, Matrix)
   Headline
    Maps associated to the finite resolution of a high syzygy module in codim 2
   Usage
    maps = makeFiniteResolutionCodim2(mf,ff)
   Inputs
    mf:List
     matrix factorization
    ff:Matrix
     regular sequence
   Outputs
    maps:HashTable
     many maps
   Description
    Text
     Given a codim 2 matrix factorization, makes all the components of 
     the differential and of the homotopies
     that are relevant to the finite resolution, as in 4.2.3 of Eisenbud-Peeva 
     "Minimal Free Resolutions and Higher Matrix Factorizations"
    Example
     kk=ZZ/101
     S = kk[a,b]
     ff = matrix"a4,b4"
     R = S/ideal ff
     N = R^1/ideal"a2, ab, b3"
     N = coker vars R
     M = highSyzygy N
     MS = pushForward(map(R,S),M)
     mf = matrixFactorization(ff, M)
     G = makeFiniteResolutionCodim2(mf, ff)
     F = G#"resolution"
   SeeAlso
    makeFiniteResolution
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
SeeAlso
 makeFiniteResolution
///




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
     
     If p = mfBound M0, then highSyzygy M0
     returns the p-th syzygy of M0.
     (if F is a resolution of M this is the cokernel 
     of F.dd_{p+1}). Optimism => r as optional
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
     justified, and thus
     matrixFactorization(ff, highSyzygy(M0, Optimism=>1));
     would produce an error.
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
    
     mfBound M = max(2*r_{even}, 1+2*r_{odd})
    
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

     The regularities of the even and odd Ext modules *can* differ by more than 1.
     An example can be produced with     
     setRandomSeed 0
     S = ZZ/101[a,b,c,d]
     ff =matrix"a4,b4,c4,d4"
     R = S/ideal ff
     N = coker random(R^{0,1}, R^{ -1,-2,-3,-4}) --gives reg Ext^even = 4, reg Ext^odd = 3
     L = ExtModuleData N;
     but takes some time to compute.

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
     TateResolution(M,-2,7) 
   Caveat
    In a previous version of this script,
    this command returned a betti table; now use 
    "betti TateResolution" instead.
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
    OutRing
   Headline
    Option allowing specification of the ring over which the output is defined
   SeeAlso
    evenExtModule
    oddExtModule
///

doc ///
        Key 
	 evenExtModule
	 (evenExtModule, Module)
	 [evenExtModule, OutRing]
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
	  If the optional argument OutRing => T is given, and class T === PolynomialRing,
	  then the output will be a module over T.
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
	  OutRing

     ///
doc ///
        Key 
	 oddExtModule
	 (oddExtModule, Module)
	 [oddExtModule,OutRing]
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
	  Extracts the odd degree part from ExtModule M.
	  If the optional argument OutRing => T is given, and class T === PolynomialRing,
	  then the output will be a module over T.
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
          OutRing
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
  
  To make the maps homogeneous, $H\#\{J,i\}$ is actually a map from
  a an appropriate negative twist of F to a shift of S.
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
  as lim_{d->\infty} Hom(I_d,M), where I_d is any sequence
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
 Text
  At one time DE hoped that, if M were a module over the complete intersection R with
  residue field k,
  then the natural map from "complete" Ext module "(widehat Ext)_R(M,k)"
  to the the S2-ification of Ext_R(M,k) would be surjective; equivalently, if
  N were a sufficiently negative syzygy of M, then the first local cohomology module
  of Ext_R(M,k) would be zero. This is false, as shown by the following example:
 Example
  needsPackage "CompleteIntersectionResolutions"
  S = ZZ/101[x_0..x_2];
  ff = apply(3, i->x_i^2);
  R = S/ideal ff;
  M = cokernel matrix {{x_0, x_1*x_2}};
  b = 5;
  Mb = prune syzygyModule(-b,M);
  E = prune evenExtModule Mb;
  S2map = S2(0,E);
  SE = prune target S2map;
  extra = prune coker S2map;
  KE = prune ker S2map;
  betti res(Mb, LengthLimit => 10)
  apply (5, i-> hilbertFunction(i, KE))
  apply (5, i-> hilbertFunction(i, E))
  apply (5, i-> hilbertFunction(i, SE))
  apply (5, i-> hilbertFunction(i, extra))
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
    \{d,h\}, where d:A_1 \to A_0 and h: \oplus A_0(p) \to A_1
    is the direct sum of partial homotopies.
Description
 Text
  The input module M should be a ``high syzygy'' over
  R = S/ideal ff.  In all example we
  know it suffices for Ext^{even}_R(M,k) and Ext^{odd}_R(M,k) to have negative regularity
  over the ring of CI operators (regraded with variables of degree 1).
  
  If the CI operator at some stage of the induction is NOT surjective,
  then the script returns an error.
  
  When the optional input Check==true (the default is Check==false), 
  the properties in the definition of Matrix Factorization are verified

  If the CI operators at each stage are surjective (that is, if
  M is really a high syzygy), then:
  
  The output is a list of maps \{d,h\} whose sources and targets are direct sums.
  
  The map d is a special lifting to S of a presentation of
  M over R. To explain the contents, we introduce some notation
  (from our paper ****):

  R(i) = S/(ff_0,..,ff_{i-1}). Here 0<= i <= c, and R = R(c)
  and S = R(0).
  
  B(i) = the matrix (over S) representing d_i: B_1(i) \to B_0(i)
  
  d(i): A_1(i) \to A_0(i) the restriction of d = d(c).
  where A(i) = \oplus_{i=1}^p B(i)
  
  
  The map h is a direct sum of maps target d(p) \to source d(p)
  that are  homotopies for ff_p on the restriction
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


doc///
Key
  exteriorTorModule
  (exteriorTorModule, Matrix, Module)
  (exteriorTorModule,Matrix,Module,Module)  
Headline
  Tor as a module over an exterior algebra or bigraded algebra
Usage
  T = exteriorTorModule(f,F)
  T = exteriorTorModule(f,M,N)
Inputs
  f:Matrix
    1 x c, entries must be homotopic to 0 on F
  M:Module
   S-module annihilated by ideal f
  N:Module
   S-module annihilated by ideal f
Outputs
  T:Module
    Tor^S(M,N) as a Module over an exterior algebra
Description
 Text
  If M,N are S-modules annihilated by the elements of the matrix
  ff = (f_1..f_c), and k is the residue field of S, then
  the script exteriorTorModule(f,M)
  returns Tor^S(M, k) as a module over an exterior
  algebra k<e_1,...,e_c>, where the e_i have degree 1, while 
  exteriorTorModule(f,M,N)
  returns Tor^S(M,N) as a module over a bigraded ring SE = S<e_1,..,e_c>,
  where the e_i have degrees {d_i,1}, where d_i is the degree of f_i.
  The module structure, in either case, is defined by the homotopies
  for the f_i on the resolution of M, computed by the script
  makeHomotopies1.
  
  The scripts call makeModule
  to compute a (non-minimal) presentation of this module.
  
  From the description by matrix factorizations and the paper
  *** of Eisenbud, Peeva and Schreyer it follows that
  when M is a high syzygy and F is its resolution,
  then the presentation of Tor(M,S^1/mm) always has generators
  in degrees 0,1, corresponding to the targets and sources of the
  stack of maps B(i), and that the resolution is componentwise linear in a suitable sense.
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
  T = exteriorTorModule(f,MS);
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
  (exteriorExtModule, Matrix, Module)
  (exteriorExtModule,Matrix,Module,Module)
Headline
  Ext(M,k) or Ext(M,N) as a module over an exterior algebra
Usage
  E = exteriorExtModule(f,M)
Inputs
  f:Matrix
    1 x c, entries must be homotopic to 0 on F
  M:Module
    annihilated by the elements of ff
  N:Module
    annihilated by the elements of ff
Outputs
  E:Module
    Module over an exterior algebra with variables corresponding to elements of f
Description
 Text
  If M,N are S-modules annihilated by the elements of the matrix
  ff = (f_1..f_c), and k is the residue field of S, then
  the script exteriorExtModule(f,M)
  returns Ext_S(M, k) as a module over an exterior
  algebra E = k<e_1,...,e_c>, where the e_i have degree 1. 
  It is computed as the E-dual of exteriorTorModule.
  
  The script
  exteriorTorModule(f,M,N)
  returns Ext_S(M,N) as a module over a bigraded ring SE = S<e_1,..,e_c>,
  where the e_i have degrees {d_i,1}, where d_i is the degree of f_i.
  The module structure, in either case, is defined by the homotopies
  for the f_i on the resolution of M, computed by the script
  makeHomotopies1.The script calls makeModule
  to compute a (non-minimal) presentation of this module.
 Example
  kk = ZZ/101
  S = kk[a,b,c]
  f = matrix"a4,b4,c4"
  R = S/ideal f
  p = map(R,S)
  M = coker map(R^2, R^{3:-1}, {{a,b,c},{b,c,a}})			       
  betti (FF =res( M, LengthLimit =>6))
  MS = prune pushForward(p, coker FF.dd_6);
  resFld := pushForward(p, coker vars R);
  T = exteriorTorModule(f,MS);
  E = exteriorExtModule(f,MS);
  hf(-4..0,E)
  betti res MS
  betti res (PE = prune E)
  betti res (PT = prune T)
  
  E1 = prune exteriorExtModule(f, MS, resFld);
  ring E1
  exRing = kk[e_0,e_1,e_2, SkewCommutative =>true]
 Text
  We can also construct the exteriorExtModule as a bigraded module,
  over a ring SE that has both polynomial variables like S and exterior
  variables like E. The polynomial variables have degrees {1,0}. The
  exterior variables have degrees {deg ff_i, 1}. 
 Example
  E1 = prune exteriorExtModule(f, MS, resFld);
  ring E1
  exRing = kk[e_0,e_1,e_2, SkewCommutative =>true]
 Text
  To see that this is really the same module, with a more complex grading,
  we can bring it over to a pure exterior algebra. Note that the necessary map of rings
  must contain a DegreeMap option. In general we could only take the degrees of
  the generators of the exterior algebra to be the gcd of  the deg ff_i ; in the
  example above this is 1.
 Example
  q = map(exRing, ring E1, {3:0,e_0,e_1,e_2}, DegreeMap => d -> {d_1})
  E2 = coker q presentation E1;
  hf(-5..5,E2) == hf(-5..5,E)
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
     resolved by F. When the first argument len is omitted, 
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
	  list of pairs {rank B_1(d), rank B_0(d)}
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
	  list matrices $h_p: A_0(p)\to A_1(p)$. The sources and targets of these
	  maps have the components B_s(p).
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
     M/(ff_0..ff_{(i-1))}M has finite length for all i=0..(length ff)-1.
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
   Description
    Text
     A possible obstruction to the commutativity of the CI operators in codim c,
     even assymptotically,
     would be the non-triviality of the map
     M_{(k+4)} --> M_k \otimes \wedge^2(S^c)
     in the stable category of maximal Cohen-Macaulay modules.
    
     In thw following example, studied in the paper ***** of
     Eisenbud, Peeva and Schreyer,
     the map is non-trivial...but it is stably trivial.
     The same goes for higher values of k (which take longer to compute).
     (note that in this case, with c = 3, two of the three alternating products are
     actually equal to 0, so we test only the third.)

     Note that T is well-defined up to homotopy; so T^2 is well-defined
     mod mm^2. 
    Example
     kk = ZZ/101
     S = kk[a,b,c]
     ff = matrix"a2,b2,c2"
     R = S/ideal ff
     M = R^1/ideal"a,bc"
     k = 1
     m = k+5
     F = res(M, LengthLimit => m)
     syzygies = apply(1..m, i->coker F.dd_i);
     t1 = makeT(ff,F,k+4);
     t2 = makeT(ff,F,k+2);
     T2Components = flatten for i from 0 to 1 list(for j from i+1 to 2 list map(F_k, F_(k+4), t2_i*t1_j-t2_j*t1_i));
     g = map(syzygies_k, syzygies_(k+4), T2Components_2)
     isStablyTrivial g
   SeeAlso
    stableHom
///

doc ///
   Key
    Shamash
    (Shamash, Matrix, ChainComplex, ZZ)
    (Shamash, Ring, ChainComplex, ZZ)
   Headline
    Computes the Shamash Complex
   Usage
    FF = Shamash(ff,F,len)
    FF = Shamash(Rbar,F,len)
   Inputs
    ff:Matrix
     1 x 1 Matrix over ring F.
    Rbar:Ring
     ring F mod ideal ff
    F:ChainComplex
     starting from F_0, defined over the same ring as ff
    len: ZZ
   Outputs
    FF:ChainComplex
     chain complex over (ring F)/(ideal ff)
   Description
    Text
     Let R = ring F = ring ff, and Rbar = R/(ideal ff).
     The complex F should admit a system of higher homotopies for the entry of ff,
     returned by the call makeHomotopies(ff,F).
     
     The complex FF has terms 
     
     FF_{2*i} = F_0 ++ F_2 ++ .. ++ F_i
     
     FF_{2*i+1} = F_1 ++ F_3 ++..++F_{2*i+1}
     
     and maps made from the higher homotopies
     In the form Shamash(Rbar,F,len) the complex F is moved over to the ring Rbar.
    Example
     S = ZZ/101[x,y,z]
     R = S/ideal"x3,y3"
     M = R^1/ideal(x,y,z)
     ff = matrix{{z^3}}
     R1 = R/ideal ff
     F = res M
     betti F
     FF = Shamash(ff,F,4)
     GG = Shamash(R1,F,4)
     betti FF
     betti GG
     ring GG
     apply(length GG, i->prune HH_i FF)
   Caveat
    F is assumed to be a homological complex starting from F_0.
    The matrix ff must be 1x1.
   SeeAlso
    makeHomotopies
///

doc ///
   Key
    layeredResolution
    (layeredResolution, Matrix, Module)
    (layeredResolution, Matrix, Module, ZZ)    
   Headline
    layered finite and infinite layered resolutions of CM modules
   Usage
    (FF, aug) = layeredResolution(ff,M)
    (FF, aug) = layeredResolution(ff,M,len)    
   Inputs
    ff:Matrix
     1 x c matrix whose entries are a regular sequence in the Gorenstein ring S
    M:Module
     MCM module over R, represented as an S-module in the first case and as an R-module in the second
    len:ZZ
     length of the segment of the resolution to be computed over R, in the second form.
   Outputs
    FF:ChainComplex
     resolution of M over S in the first case; length len segment of the resolution over R in the second.
   Description
    Text
     The resolutions computed are those described in the paper "Layered Resolutions of Cohen-Macaulay modules"
     by Eisenbud and Peeva. They are both minimal when M is a suffiently high syzygy of a module N.
     Here is an example computing 5 terms of an infinite resolution:
    Example
     S = ZZ/101[a,b,c]
     ff = matrix"a3, b3, c3" 
     R = S/ideal ff
     M = syzygyModule(2,coker vars R)
     (FF, aug) = layeredResolution(ff,M,5)
     betti FF
     betti res(M, LengthLimit=>5)
     C = chainComplex flatten {{aug} |apply(4, i-> FF.dd_(i+1))}
     apply(5, j-> prune HH_j C == 0)
    Text
     And one computing the whole finite resolution:
    Example
     MS = pushForward(map(R,S), M);
     (GG, aug) = layeredResolution(ff,MS)
     betti GG
     betti res MS
     C = chainComplex flatten {{aug} |apply(length GG -1, i-> GG.dd_(i+1))}    
     apply(length GG +1 , j-> prune HH_j C == 0)     
///

doc ///
   Key
    extIsOnePolynomial
    (extIsOnePolynomial, Module)
   Headline
    check whether the Hilbert function of Ext(M,k) is one polynomial
   Usage
    (p,t) = extIsOnePolynomial M
   Inputs
    M:Module
     module over a complete intersection
   Outputs
    p:RingElement
     p(z)=pe(z/2), where pe is the Hilbert poly of Ext^{even}(M,k)
    t:Boolean
     true if the even and odd polynomials match to form one polynomial
   Description
    Text
     Computes the Hilbert polynomials pe(z), po(z) of evenExtModule and oddExtModule.
     It returns pe(z/2), and compares to see whethe this is equal to po(z/2-1/2).
     Avramov, Seceleanu and Zheng have proven that if the ideal of quadratic leading
     forms of a complete intersection of codimension c generate an ideal of codimension
     at least c-1, then the betti numbers of any module grow, eventually, as a 
     single polynomial (instead of requiring separate polynomials for even and 
     odd terms.) This script checks the result in the homogeneous case (in which
     case the condition is necessary and sufficient.)
    Example
     R1=ZZ/101[a,b,c]/ideal(a^2,b^2,c^5)
     R2=ZZ/101[a,b,c]/ideal(a^3,b^3)     
     extIsOnePolynomial coker random(R1^{0,1},R1^{3:-1})
     extIsOnePolynomial coker random(R2^{0,1},R2^{3:-1})
   SeeAlso
    evenExtModule
    oddExtModule
///

------TESTs------
TEST/// -- tests of the "with components" functions
S = ZZ/101[a,b]
M = S^{1,2}
N = S^{3,5}
M' = (S^{1}++S^{2})
N' = S^{3}++S^{5}

H = Hom(M,N) 
T = M**N
D = dual M
H' = HomWithComponents (M',N')
T' = tensorWithComponents(M',N')
D' = dualWithComponents M'
assert( H == H' and T == T' and D == D')
assert(HomWithComponents(M',N') == tensorWithComponents(dualWithComponents M', N'))
assert(components HomWithComponents(M',N') == components tensorWithComponents(dualWithComponents M', N'))
M = S^{1,2}/ideal(a^2)
M' = S^{1}/ideal(a^2)++S^{2}/ideal(a^2)
M == M'
(T = M**N) == M'**N'
assert(T == tensorWithComponents(M',N'))
M= S^0
M'=S^0++S^0
assert(M**M == tensorWithComponents (M',M'))
///
TEST///
setRandomSeed 0
R1=ZZ/101[a,b,c]/ideal(a^2,b^2,c^5)
R2=ZZ/101[a,b,c]/ideal(a^3,b^3)     

(p,t) = extIsOnePolynomial coker random(R1^{0,1},R1^{3:-1});
assert(t === true)
z = (ring p)_0
assert(p ===(1/2)*z^2-(1/2)*z+3)

(p,t) = extIsOnePolynomial coker random(R2^{0,1},R2^{3:-1});
z = (ring p)_0
assert(t === false)
assert(p ===(3*z - 2))
///

TEST///
setRandomSeed 100
c = 2
d = 2
R = setupRings(c,d)
(M,k,p) = setupModules(R,coker vars R_c);
regularitySequence(R,coker vars R_c)
///

///TEST
S1 = ZZ/101[a,b,c]
len = 4
--codim 0
ff = matrix{{}}
M = S1^1
(FF, aug) = layeredResolution(ff,M,len)
betti FF == betti res(M, LengthLimit=>len)
--codim 1
use S1
ff = matrix"a3" 
R1 = S1/ideal ff
M = syzygyModule(3,coker vars R1)
(FF, aug) = layeredResolution(ff,M,len)
betti FF == betti res(M, LengthLimit=>len)
--codim 2
use S1
ff = matrix"a3, b3" 
R1 = S1/ideal ff
M = syzygyModule(2,coker vars R1)
(FF, aug) = layeredResolution(ff,M,len)
assert(betti FF == betti res(M, LengthLimit=>len))
--codim 3
use S1
len = 5
ff = matrix"a3, b3, c3" 
R1 = S1/ideal ff
M = syzygyModule(2,coker vars R1)
(FF, aug) = layeredResolution(ff,M,len)
assert(betti FF == betti res(M, LengthLimit=>len))
C = chainComplex flatten {{aug} |apply(len-1, i-> FF.dd_(i+1))}
scan(len, j-> assert(prune HH_j C == 0))
///

TEST///
S = ZZ/101[a,b,c]
ff1 = matrix"a3,b3,c3"
setRandomSeed 0
ff = ff1*random(source ff1, source ff1)
R = S/(ideal ff)
M = coker matrix {{R_0,R_1,R_2},{R_1,R_2,R_0}}
F = res coker vars R
F0 = res (M, LengthLimit =>3)
makeT(ff, F0, 4)
min F0
--generateAssertions"makeT(ff, F0, 2)"
assert( (makeT(ff, F0, 2)) === {map((R)^{{-3},{-3}},(R)^{{-3},{-3},{-3},{-3},{-3}},{{39, 0, 0, -26, 0},
      --------------------------------------------------------------------------------------------------------
      {0, 39, 0, 0, 26}}),map((R)^{{-3},{-3}},(R)^{{-3},{-3},{-3},{-3},{-3}},{{33, 0, 0, 31, 0}, {0, 33, 0, 0,
      --------------------------------------------------------------------------------------------------------
      -31}}),map((R)^{{-3},{-3}},(R)^{{-3},{-3},{-3},{-3},{-3}},{{8, 0, 0, -31, 0}, {0, 8, 0, 0, 31}})} );
///

///TEST
S = ZZ/101[x,y,z]
R = S/ideal"x3,y3"
M = R^1/ideal(x,y,z)
F = res M
ff = matrix{{z^3}}
FF = Shamash(ff,F,4)
scan(length FF -1, i->assert(0==(HH_(i+1)FF)))
Rbar = R/ideal(z^3)
assert (Shamash(Rbar,F,4) == (map(Rbar,ring FF))FF)
///

TEST///
kk=ZZ/101
S = kk[a,b]
ff = matrix"a4,b4"
R = S/ideal ff
N = coker vars R
M = highSyzygy N
mf = matrixFactorization(ff, M)

h = (hMaps mf)_1
h' = map(target h, source h,
    matrix apply (#components source h, i->(apply(#components target h, j-> h_[j]^[i])))
)
assert(h == h')

h = (hMaps mf)_0
h' = map(target h, source h,
    matrix apply (#components source h, i->(apply(#components target h, j-> h_[j]^[i])))
)
assert(h == h')
///

TEST///
S = ZZ/101[a,b,c];
ff = matrix"a3,b3";
R = S/ideal ff;
M = highSyzygy (R^1/ideal vars R)
mf = matrixFactorization (ff, M)
G = makeFiniteResolution(mf,ff)
F = res pushForward(map(R,S),M)
assert(betti G == betti F)
///

TEST///
  kk = ZZ/101;
  S = kk[a,b,c];
  ff = matrix{{a^4, b^4,c^4}};
  R = S/ideal ff;
  Ops = kk[x_1,x_2,x_3];
  MM = Ops^1/(x_1*ideal(x_2^2,x_3));
  N = moduleAsExt(MM,R);
 assert( hf(-7..6, N) == {0, 18, 48, 108, 177, 222, 254, 227, 180, 128, 66, 33, 11, 0})
  assert(hfModuleAsExt(12,MM,3) == (23, 25, 27, 29, 31, 33, 35, 37, 39, 41));
///

TEST///
--Example 0
kk = ZZ/101
S = kk[a,b]
ff = matrix"ab"
R = S/ideal ff
M0 = R^1/ideal"a,b"
assert(2==regularity ExtModule M0)
len = 2
F = res(M0, LengthLimit =>len)
MF = matrixFactorization(ff, coker F.dd_len, Check=>true)
use S
assert( (MF) === {map((S)^{{-1},{-1}},(S)^{{-2},{-2}},{{b, 0}, {0,a}}),map((S)^{{-2},{-2}},(S)^{{-3},{-3}},{{a, 0}, {0, b}})} );
assert(BRanks MF=={{2,2}})
///

TEST///
--Example 0a
kk = ZZ/101
S = kk[a,b,c]
ff = matrix"ac-b2"
R = S/ideal ff
m = matrix"a,b;b,c"
betti m
M0 = coker m
MF = matrixFactorization(ff,highSyzygy M0)
assert(toString MF == "{matrix {{b, -c}, {-a, b}}, matrix {{-b, -c}, {-a, -b}}}")
BRanks MF=={{2,2}}
///

TEST///
--Example1
kk = ZZ/101
S = kk[a,b,u,v]
ff = matrix"au,bv"
R = S/ideal ff
M0 = R^1/ideal"a,b"
MF = matrixFactorization(ff,highSyzygy M0)
toString MF == "{matrix {{0, u, v, 0}, {-a, b, 0, 0}, {0, 0, -a, b}}, matrix {{b, -u, 0, 0, 0}, {a, 0, 0, v, 0}, {0, 0, b, -u, 0}, {0, 0, a, 0, v}}}"
assert(BRanks MF =={{2,2}, {1,2}})
///


TEST///--Example2
kk = ZZ/101
S = kk[a,b]
ff = matrix{{a^3,b^3}}
R = S/ideal ff;
M0=R^1/ideal"ab" 
MF = matrixFactorization (ff, highSyzygy M0)
assert(BRanks MF == {{2, 2}, {1, 2}})
///

TEST///
--Example3
kk = ZZ/101
S = kk[a,b,c]
ff = matrix"a3,b3,c3"
betti ff
ff1 = ff*random(S^{3: -3}, S^{3: -3})
R = S/ideal ff; 
M0= R^1/ideal"ab"
use S
MF = matrixFactorization (ff1, highSyzygy M0)
assert(BRanks MF == {{0, 0}, {2, 2}, {1, 2}})
///

TEST///
--Example4
S = ZZ/101[a,b,c,d]
mm= ideal vars S
ff = (matrix"a3,b3,c3,d3")
ff1 = ff*random(source ff, source ff);
R = S/(ideal ff);
M0 = coker map(R^1, R^{-2,-3}, matrix"a2,bcd")
MF = matrixFactorization(ff1,highSyzygy M0);
assert(BRanks MF=={{8, 8}, {8, 12}, {9, 16}, {11, 21}});
///

TEST///
--Formerly bad example. Now seems fine
S = ZZ/32003[x_0..x_2]
f = matrix{{x_0^5, x_1^5, x_2^5}}
ff = f*random(source f, source f)
R = S/ideal f
m1 = {x_0^2*x_2^4, x_0*x_1^4*x_2}
M0 = R^1/ideal(m1_0+m1_1);
MF = matrixFactorization(ff, highSyzygy M0);
assert(BRanks MF=={{6, 6}, {5, 7}, {5, 8}})
///


TEST///
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

TEST///
     S = ZZ/101[a,b,c];
     M = S^2/ideal"a,b"++S^3/ideal"b,c";
     N = coker random (S^{0,1}, S^{-1});
     g = homomorphism' id_M;
     assert(id_M == homomorphism g)
     assert(isStablyTrivial id_M == false)
     assert(isStablyTrivial(map(M, cover M, 1))==true)
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

TEST///
kk=ZZ/101;
S = kk[a,b,c];
ff = matrix{{a^2,b^2}};
R = S/ideal ff;
red = map(R,S);
F = res (ideal (vars R)_{0..2}, LengthLimit => 3);
MS2 = pushForward(red, coker F.dd_3);
MS1 = pushForward(red, coker F.dd_2);


T = exteriorTorModule(ff, MS2, MS1);
assert(apply(10, d->rank source basis({d,1}, T))==
    apply(10, d->rank source basis(d, Tor_1(MS2, MS1))));

T1 = exteriorTorModule(ff, MS2);
assert(apply(10, d->rank source basis(d, T1))==
    apply(10, d->rank source basis(Tor_d(MS2,coker vars ring MS2))))

Ex1 = exteriorHomologyModule(ff, (coker vars ring ff)**dual res MS2);
assert(apply(10, d->rank source basis(-d, Ex1))=={0, 0, 5, 7, 1, 3, 0, 0, 0, 0});
///

TEST///
kk=ZZ/101
S = kk[a,b,c]
extring = kk[e_0,e_1, Degrees =>{2:1}, SkewCommutative =>true]
simplify = t ->(
toe := map(extring, ring t, {3:0, e_0,e_1}, DegreeMap => i->{i_1});
toe presentation t)

ff = matrix{{a^2,b^2}}
R = S/ideal ff
red = map(R,S)
F = complete res (ideal (vars R)_{0..2}, LengthLimit => 7)
M = apply(7, i-> coker F.dd_(i+1));
MS = M/(Mi -> pushForward(red, Mi));

C = (complete res MS_1)**MS_0;
T0 = apply(7, i -> exteriorTorModule(ff,MS_i));
T1 = apply(7, i -> exteriorTorModule(ff, MS_i, coker vars S));
T2 = apply(7, i -> exteriorHomologyModule(ff, (complete res MS_i)**coker vars S));
assert( (apply(T0, t->isHomogeneous t)) === {true,true,true,true,true,true,true} );
       assert( (apply(T1, t->isHomogeneous t)) === {true,true,true,true,true,true,true} );
       assert( (apply(T2, t->isHomogeneous t)) === {true,true,true,true,true,true,true} );

assert(apply (7, i->betti prune coker simplify T1_i == betti prune  presentation  T0_i)==
    {true,true,true,true,true,true,true} );
assert(apply (7, i-> betti prune coker simplify T2_i == betti prune  presentation  T0_i)==
{true,true,true,true,true,true,true} );
///

TEST///
SE = ZZ/101[a,b,c,x,y,Degrees=>toList(3:{1,0})|toList(2:{1,1}), SkewCommutative=>{x,y}]
RE = SE/ideal"a2,b2,c2"
T = hashTable {{0,RE^1},{1,RE^{2:{ -1,-1}}}, {2,RE^{{ -2,-2}}}}
E = matrix{{x,y}}
F=apply(2, j-> source E_{j})
phi = hashTable{
    {{0,0}, map(T#1, F_0**T#0, T#1_{0})},
    {{1,0}, map(T#1, F_1**T#0, T#1_{1})},
    {{0,1}, map(T#2, F_0**T#1, T#1^{1})},
    {{1,1}, -map(T#2, F_1**T#1, T#1^{0})}
    }
assert(apply(keys phi, k->isHomogeneous phi#k) == {true, true, true, true});

X = makeModule(T,E,phi);
assert(isHomogeneous X==true);

q = map(ZZ/101[x,y, SkewCommutative => true, DegreeMap => d->{d_1}], ring X, {3:0,x,y})

E = prune coker q presentation X;
assert(isFreeModule E);
assert(rank E==1);
///

///TEST
R = ZZ/101[a,b,c]/ideal"a3,b3,c3"
M = R^1/ideal"ab,ac,bc"
U = ZZ/101[A,B,C]
Ee = evenExtModule(M, OutRing => U)
Eo = oddExtModule(M, OutRing => U)
assert(ring Ee === U)
assert(ring Eo === U)
///

end--

--notify=true
uninstallPackage "CompleteIntersectionResolutions"
restart
installPackage "CompleteIntersectionResolutions"
check "CompleteIntersectionResolutions"

viewHelp CompleteIntersectionResolutions

