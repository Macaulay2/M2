newPackage(
        "EagonResolution",
        Version => "1.0", 
        Date => "September 23, 2020",
        Authors => {{Name => "David Eisenbud", 
                  Email => "de@msri.org", 
                  HomePage => "http://www.msri.org/~de"},
	          {Name => "Mike Stillman", 
                  Email => "mike@math.cornell.edu", 
                  HomePage => "http://pi.math.cornell.edu/~mike"}},
        Headline => "Compute the Eagon Resolution of the residue field",
        DebuggingMode => false
        )


export {
    "eagon", 
    "eagonResolution", -- use res EagonData as a synonym
    "golodBetti",   -- the betti table "as if" the module is Golod.
    "verticalStrand", --make a vertical strand of the Eagon complex
    "horizontalStrand", --make a vertical strand of the Eagon complex    
    "eagonBeta", -- the maps responsible for any non-minimality.
    "picture", -- compressed display of labeled matrices; various options
    "mapComponent",
--Symbols:
    "EagonData", -- the result of the eagon computation
    "Transpose", -- option for picture
    "DisplayBlocks", --option for picture
    "Display", -- option for picture and for eagonBeta
    "CompressBeta" --option for eagon
    }
protect EagonLength

--EagonData--
EagonData = new Type of HashTable
net EagonData := E -> net ("EagonData in <ring>.cache computed to length "| toString(E.EagonLength))

--verticalStrand--
verticalStrand = method()
verticalStrand (EagonData, ZZ) := ChainComplex =>  (E,n) -> (
    	   Kemax :=select(keys E, k -> (k_0 === "dVert" and E#k !=0));
           maxn := max apply(Kemax, k->k_1);
	   if n>maxn then error "That vertical strand wasn't defined";
    	   Ke := select(keys E, k -> (k_0 === 0 and k_1 === n and E#k !=0));
	   b := max (Ke/(k-> k_2));
           chainComplex(apply(b, i-> E#{"dVert", n,i+1})))
       
--horizontalStrand--       
horizontalStrand = method()
horizontalStrand (EagonData, ZZ) := ChainComplex =>  (E,i) -> (
    	   Kemax :=select(keys E, k -> (k_0 === "dHor" and E#k !=0));
           maxi := max apply(Kemax, k->k_2);
	   if i>maxi then error "That horizontal strand wasn't defined";
    	   Ke := select(keys E, k -> (k_0 === 0 and k_2 === i and E#k !=0));
	   b := max (Ke/(k-> k_1));
           chainComplex(apply(b-1, n -> E#{"dHor", n+1,i})))
    
///
restart
loadPackage("EagonResolution", Reload => true)
///

--tensoring a list of modules together:
tensorL = (R,L) -> (if L === {} then return R^1;
                              if #L === 1 then return L_0;
			      L_0**tensorL(R,drop(L,1)))
--golodBetti--			  
golodBetti = method()
golodBetti (ChainComplex, ChainComplex, ZZ) := BettiTally => (F,G,b) ->(
    --F,G finite free complexes (resolutions) over a ring S.
    --Compute the Betti table of what should be the Eagon resolution of 
    --the module resolved by G over the ring resolved by F
    --up to step b.
    symbs := apply(b+1, n->eagonSymbols(n,0));
    mods := apply(symbs, s -> 
	directSum apply(#s, 
	    i-> G_(s_i_0)**tensorL(ring F, apply(s_i_1, j->F_(j)))));
   betti chainComplex apply(b,i->map(mods_i,mods_(i+1),0))
   )

golodBetti (Module,ZZ) := BettiTally => (M,b) ->(
    --case where M is a module over a factor ring R = S/I,
    --MS is the same module over S
    --F = res I
    --K = res MS
    R := ring M;
    p := presentation R;
    S := ring p;
    phi1 := substitute(presentation M, S);
    phi := phi1 | target phi1 ** p;
    MS := prune coker phi;
    K := res MS;
    F := res coker p;
    golodBetti(F,K,b)
    )

///
restart
loadPackage("EagonResolution", Reload =>true)
S = ZZ/101[x,y,z]
R = S/ideal(x^2,y^3)
M = coker random(R^2, R^{-2,-2,-3})
b = 5
golodBetti(M,5)
///


homologyCover = method()
homologyCover(ChainComplex,ZZ) := Matrix => (C,i) ->(
    --map from a free module to C_i giving a basis of HH_i C.
    --Note that HH_i C must be of finite length, or there's an error.
    B := basis HH_i C;
    (gens target B)*matrix B)

homologyCover(ZZ, ChainComplex) := List => (b,C) -> 
    apply(b, i-> homologyCover(C,i))

homologyCover(ChainComplex) := List => C -> homologyCover(1+length C, C)

homologyIsomorphism = method()
homologyIsomorphism(Module, ChainComplex, ZZ) := Matrix => (M,C,i) ->(
    --If M is isomorphic to HH_i C then the matrix returned is a map
    -- target presentation M --> C_i 
    --inducing the isomorphism.
    --else the function throws and error.
    Hi := HH_i C;
    H := prune Hi;
    p := H.cache.pruningMap; -- iso from H to HH_i C
    f := degreeZeroSurjection(M,H);
    if f === null then error "non-iso modules that should be iso";
--    g := degreeZeroSurjection(H, M); -- this is just a check; get rid of it eventually
    map(C_i,HH_i C, gens Hi)*p*f
)

labeler = (L,F) -> directSum(1:(L=>F));

tensorWithComponents = method()
tensorWithComponents(Module, Module, Function) := Module => (F, G, combineIndices) -> (
    if F == 0 or G == 0 then return (ring F)^0;
    (compsF, indicesF) := componentsAndIndices F;
    (compsG, indicesG) := componentsAndIndices G;
    comps := flatten for f from 0 to #compsF-1 list (
        for g from 0 to #compsG-1 list (
            newindex := if indicesF#f === null or indicesG#g === null
	       then null else combineIndices(indicesF#f, indicesG#g);
            newindex => directSum(1:(newindex=>(compsF#f ** compsG#g)))
            )
        );
    if #comps == 0 then (ring F)^0 else directSum comps
    )
tensorWithComponents(Module, Module) := Module => (F, G) -> tensorWithComponents(F, G, (a,b) -> a|b)
tensorWithComponents(Matrix, Module, Function) := Matrix => (phi, G, combineIndices) -> (
                          src :=  tensorWithComponents(source phi, G, combineIndices);
                          tar :=  tensorWithComponents(target phi, G, combineIndices);			  
			  map(tar,src,phi**G))

eTensor = method()
eTensor(Module,Module) := Module => (F, G) -> tensorWithComponents(F, G, (a,b) ->(a#0+b#0,a#1|b#1))
eTensor(Matrix,Module) := Matrix => (phi,G) -> tensorWithComponents(phi, G, (a,b) ->(a#0+b#0,a#1|b#1))


trimWithLabel = method()
trimWithLabel ZZ := ZZ => n -> n
trimWithLabel Symbol := Symbol => s -> s

trimWithLabel Module := Module => M ->(
    if M == 0 then return (ring M)^0;
    ci := componentsAndIndices M;
    pci := positions(ci_0,M -> M!=0);
    if #pci ===0 then return (ring M)^0;
    if #pci === 1 then 
        labeler(ci_1_pci_0, ci_0_pci_0) 
    else
        directSum apply(pci, i->labeler(ci_1_i,ci_0_i))
)

trimWithLabel Matrix := Matrix => f ->(
    S := source f;
    T := target f;
    S':= trimWithLabel S;
    T':= trimWithLabel T;
    map(T',T,id_T')*f*map(S,S',id_S)
    )

trimWithLabel HashTable := HashTable => E -> hashTable apply(keys E, k-> (k,trimWithLabel E#k))

trimWithLabel ChainComplex := ChainComplex => F -> chainComplex apply(
                              length F, i-> trimWithLabel(F.dd_(i+1))
			      )

///
restart
loadPackage("EagonResolution", Reload =>true)
S = ZZ/101[x,y,z]
R = S/ideal"x2,y2,z2"
E = eagon(R,5)
E
eagonResolution E

resolution E
res E
eagon(R,4)
eagon(R,-1)
///

--eagon--
eagon = method(Options => {CompressBeta => true, Verbose => false})
eagon(Ring, ZZ) := EagonData => o -> (R,b) ->(
    --compute the Eagon configuration up to and including column b; and thus
    --also the "Eagon Resolution"
    --Y^b_0 \to...\to Y^1_0 \to Y^0_0. 

    --Let X_i be the free module R**H_i(K), where K is the Koszul complex on the variables of R.
        --We count X_i as having homological degree i+1.
    --The module Y^n_i = Eagon#{0,n,i} is described in Gulliksen-Levin as:
    --Y^0 = koszul vars R
    --Y^(n+1)_0 = Y^n_1; and 
    --for i>0, Y^(n+1)_i = Y^n_(i+1) ++ Y^n_0**X_i

    --Note that Y^n_i == 0 for i>1+length koszul vars R, so we will carry computations out to that length.

    --Each Y^n is a complex whose i-th homology is H_i(Y^n) = H_0(Y^n)^0**X_i (proved in Gulliksen-Levin).
    --assuming that the differential of Y^n and the maps Y^n --> Y^(n-1) are known
    --To construct the differential of Y^(n+1) and the map Y^(n+1) \to Y^n, 
    --this isomorphism must be made explicit.

    if b == -1 then (
	remove(R.cache,symbol EagonData);  
	<<"EagonData removed from "<<R<<".cache"<<endl;
	return null);    
    
    if R.cache.?EagonData  and R.cache.EagonData.EagonLength >= b then return R.cache.EagonData;
    
    Eagon := new MutableHashTable;        

    g := numgens R;
    K0 := koszul vars R;
    Eagon#"numgens" = g;

    --now label the modules in the Koszul complex    
    K := chainComplex(for i from 1 to length K0 list 
	map(labeler((i-1,{}), K0_(i-1)),
	    labeler((i,{}), K0_(i)),
	    K0.dd_i));
    
    homologyCover' := (K,i) -> (
	--Returns the map from X(i) to K_i
        phi := homologyCover(K,i); 
        Xi := labeler((0,{i}),source phi); 
        map(K_i, Xi, phi)
        );
    
    ebasis := memoize homologyCover';
    X := i -> if i<=g and (s := source ebasis(K,i))!=0 then  
             labeler((0,{i}),s) else R^0; -- X(i) is the X_i of Gulliksen-Levin.
    --we made it a function so that it would be available for all integers i.
        pd := 0; while X(pd)!=0 do pd = pd+1; pd = pd-1; -- max i such that X(i)!=0
    	Eagon#"pd" = pd;
    
    --first make the free modules Y^n_i = Eagon#{0,n,i}. 
    --The maps Y^(n+1)_j \to Y^n_j will be Eagon#{"dHor",n+1,j}
    west := "dHor";
    --The differential verticaldiff of Y^n is the sum of maps eagon#{"dVert",n,i} and eagon#{"NW",n,i}.
    north := "dVert";
    eagonBeta := "eagonBeta";

    --Make the free modules Eagon#{0,n,i}. 
    --two special cases:
    for i from 0 to g+1 do (
	Eagon#{0,0,i} = K_i;-- print Eagon#{0,0,i}.cache.components);
      for n from 0 to b do(
           Eagon#{0,n,g+2} = directSum(1:R^0);-- R^0++R^0; 
       	                 ));
    -- cases:
    for n from 1 to b do (
    for i from 0 to g+1 do(	
       if i == 0 then (
	   Eagon#{0,n,i} = Eagon#{0,n-1,1} ;
	)
        else (
        Eagon#{0,n,i} = Eagon#{0,n-1,i+1}++eTensor(Eagon#{0,n-1,0},X(i));
	     )
    ));

    --Now make the northward maps; the maps of the complexes Y^n = E#{0,n,*}
    --Note that the highest term in Y^n is in place b-n, so the top interesting homology is H_(b-n-1)
    --initialize:
    for i from 0 to g+2 do Eagon#{north,0,i} = K.dd_i;
	       
--Make the maps for n=1:
    --three special cases:
    Eagon#{north, 1, g+2} = map(Eagon#{0,1,g+1}, Eagon#{0,1,g+2},0);
    Eagon#{west,1,0} = Eagon#{north, 0,1};

    Eagon#{north,1,1} = (Eagon#{north,0,2})*(Eagon#{0,1,1})^[0] +
	                    ebasis(K,1)*(Eagon#{0,1,1})^[1];
    Eagon#{eagonBeta,1,1} = ebasis(K,1);			    
    Eagon#{west,1,1} = K.dd_2 | ebasis(K,1);
    
    for i from 2 to g+1 do(
	Eagon#{north,1,i} = (Eagon#{0,1,i-1})_[0]*
	                    (
	                    (Eagon#{north,0,i+1})*(Eagon#{0,1,i})^[0] +
	                    ebasis(K,i)*(Eagon#{0,1,i})^[1]
			    );
        Eagon#{eagonBeta,1,i} = ebasis(K,i);			    
	Eagon#{west,1,i} = K.dd_(i+1) | ebasis(K,i);
			);
    		
--now the induction, assuming that the Y^m have been defined for m<n:
    for n from 2 to b do(
       Eagon#{north, n, g+2} = map(Eagon#{0,n,g+1}, Eagon#{0,n,g+2},0);
       Eagon#{west,n,0} = Eagon#{north, n-1,1};
       Eagon#{eagonBeta,n,0} = Eagon#{eagonBeta, n-1,1};       
    	    	    
    for i from 1 to g+1 do(

--for eagonBeta:
        toLift := -(if #components Eagon#{0,n-2,i} ===1 then 
	   id_(Eagon#{0,n-2,i}) else Eagon#{0,n-2,i}_[0])*
             Eagon#{eagonBeta,n-1,i}*
             eTensor(Eagon#{north, n-2,1},X(i));
        M := Eagon#{north,n-2,i+1};
     
--idea: when o.CompresBeta == true, then 
--try the initial segments of blocks until lifting becomes possible; then
--set M = that lifting.  We
--know that a lifting is eventually possible.

     ind := indices flattenBlocks source M;
     if o.CompressBeta == true and ind =!=null then(
       numInd := #ind;
       if numInd != 0 then scan(numInd, p-> (
		 M' := extractBlocks(M,toList(0..p));
		 if toLift % M' == 0 then(
		     (M = M';
		      if o.Verbose == true then 
		         <<"Used "<<p+1<<" of "<<numInd<<" blocks of eagonBeta "<<(n,i)<<endl;
		      break)))));

     Eagon#{eagonBeta,n,i} = toLift//M;
 

     Eagon#{west,n,i} = Eagon#{0,n-1,i}_[0]*Eagon#{eagonBeta,n,i}*(Eagon#{0,n,i})^[1]+
	                   Eagon#{0,n-1,i}_[1]* (Eagon#{west,n-1,0}**X(i))  *(Eagon#{0,n,i})^[1]+
	                   (if Eagon#?{west,n-1,i+1} then 
			            Eagon#{0,n-1,i}_[0]*Eagon#{west,n-1,i+1}*Eagon#{0,n,i}^[0] 
				                     else 0);

     if i == 1 then 
         Eagon#{north,n,i} = -- special case because Y^n_0 is not a tensor product with Y^(n-1)_0
	                    (
	                    (Eagon#{north,n-1,i+1})*((Eagon#{0,n,i})^[0])+
	                    Eagon#{0,n-1,i}_[0]*Eagon#{eagonBeta,n,i}*(Eagon#{0,n,i})^[1]+
			    Eagon#{0,n-1,i}_[1]*(Eagon#{north, n-2,1}**X(i))*(Eagon#{0,n,i}^[1])
			    )
     else
         Eagon#{north,n,i} =
	    Eagon#{0,n,i-1}_[0]*
	                    (
	                    (Eagon#{north,n-1,i+1})*((Eagon#{0,n,i})^[0])+
	                    Eagon#{0,n-1,i}_[0]*Eagon#{eagonBeta,n,i}*(Eagon#{0,n,i})^[1]+
			    Eagon#{0,n-1,i}_[1]*(Eagon#{north, n-2,1}**X(i))*(Eagon#{0,n,i}^[1])
			    );
  ));

  H := trimWithLabel hashTable prepend((symbol EagonLength, b), pairs Eagon);
  E :=new EagonData from H;

  R.cache.EagonData = E;
  E
)

--eagonBeta--
eagonBeta = method(Options => {Display => "picture", Verbose => false})
--There are no maps eagonBeta(E,0) or eagonBeta(E,1); the display starts with eagonBeta(E,2).

eagonBeta(EagonData, ZZ) := o -> (E,n) -> (
         if o.Display == "picture" then 
             picture(E#{"eagonBeta",n,0},Verbose => o.Verbose)
         else if o.Display == "DisplayBlocks" then 
	     displayBlocks E#{"eagonBeta",n,0} 
         else 
	     E#{"eagonBeta",n,0}
         )

eagonBeta EagonData := List => o-> E -> (
    b := max apply(select(keys E, k-> k_0 === 0 and k_2 === 0), k->k_1);
    netList apply(b-1, n -> eagonBeta(E,n+2,Display => o.Display, Verbose => o.Verbose))
    )

extractBlocks = method()
extractBlocks(Matrix, List) := Matrix => (phi, L) -> (
    --the map map(target phi, source phi, phi_[L]*(source phi)^[L], where L is a list
    --of integers respresenting blocks in source phi.
    phi1 := flattenBlocks phi;
    src := source phi1;
    ind := indices src;
    sum(L, i->phi1_[ind_i]*src^[ind_i])
    )

--eagonResolution--
eagonResolution = method()
eagonResolution EagonData := ChainComplex => E ->(
    b := max apply( select(keys E, k-> k_0 === 0 and k_2 === 0), k->k_1);
    chainComplex(apply(b,n->E#{"dHor",n+1,0}))
    )    

eagonResolution(Ring,ZZ) := ChainComplex => (R,b) ->(
    eagonResolution eagon(R,b)
    )

resolution EagonData := opts->E -> eagonResolution E
--res--
--resolution--

///
restart
loadPackage("EagonResolution", Reload=>true)
S = ZZ/101[a,b,c]
I = ideal"a3,b3,c3"
R = S/I
E = eagon(R,6)
eagonResolution E -- works
--resolution EagonData := E -> eagonResolution E
resolution E -- fails
eagonResolution E
res R.cache.EagonData
eagonResolution E
///

eagonSymbols = method()
eagonSymbols(ZZ,ZZ) := List => (n,i) ->(
    --symbol of the module Y^n_i, as a list of pairs, defined inductively from n-1,i+1 and n-1,0
    --assumes large number of vars and pd.
    if n === 0 then return {(i,{})};
    if i === 0 then return eagonSymbols(n-1,1);
    e' := eagonSymbols (n-1,0);
    e'1 := apply (e', L -> L_1|{i});
    eagonSymbols(n-1,i+1)|apply (#e', j-> (e'_j_0,e'1_j))
    )

componentsAndIndices = (F) -> (
    if not F.cache.?components then (
        -- F has no components
        ({F}, {null})
        )
    else if #F.cache.components == 1 then (
        if F.cache.?indices then 
            ({F}, F.cache.indices)
        else 
            ({F}, {null})
        )
    else (
        a := for f in F.cache.components list componentsAndIndices f;
        (flatten(a/first), flatten(a/last))
        )
    )

flattenBlocks = method()
flattenBlocks Module := (F) -> (
    if not isFreeModule F then error "expected a free module";
    (comps, inds) := componentsAndIndices F;
    compsLabelled := for i from 0 to #comps-1 list (
        inds#i => comps#i
        );
    directSum compsLabelled
    )

flattenBlocks Matrix := (M) -> (
    F := flattenBlocks target M;
    G := flattenBlocks source M;
    map(F,G,matrix M)
    )

--displayBlocks--
displayBlocks = method()
displayBlocks Matrix := (M1) -> (
    M := flattenBlocks M1;
    src := select(indices source M, i-> i =!= null);
    tar := select(indices target M, i-> i =!= null);
    netList (prepend(
        prepend("", src),
        for t in tar list prepend(t, for s in src list (
                mts := M^[t]_[s];
                h := if mts == 0 then "." else if (numrows mts == numcols mts and mts == 1) then "1" else net mts
                ))
        ), Alignment=>Center)
    )
displayBlocks ChainComplex := List => C -> apply(length C, i -> displayBlocks(C.dd_(i+1)))
displayBlocks EagonData := List => E -> displayBlocks eagonResolution E

    
--pictureList--
pictureList = method(Options => {Verbose => false, Display => "picture", Transpose => false})
pictureList Matrix := o -> (M1) -> (
    M := flattenBlocks M1;
    src := indices source M;
    tar := indices target M;
    kkk := ring M/(ideal gens ring M);
    L := prepend(
        prepend("", src),
        for t in tar list prepend(t, for s in src list (
                mts := M^[t]_[s];
		nums := toString(numrows mts,numcols mts);
		cont := ideal M^[t]_[s];
		if o.Verbose == false then (
		  h := if mts == 0 then 
                          "." 
                       else if (numrows mts == numcols mts and mts == 1) then 
                          "id" 
                       else if cont == ideal(1_(ring mts)) then
		          toString numrows M^[t]_[s]|","|toString rank(kkk**M^[t]_[s]) 
                       else "*"
                  )
                else (
		  h = if mts == 0 then 
                          nums|" ." 
                      else if (numrows mts == numcols mts and mts == 1) then 
                          nums|" id" 
                      else if cont == ideal(1_(ring mts)) then 
                          nums|","|toString rank(kkk**M^[t]_[s]) 
                      else "*"
                  )
         )));
    if o.Transpose then transpose L else L
    )

pictureList ChainComplex := List => o -> C -> apply(length C, i-> pictureList(C.dd_(i+1), o))
pictureList EagonData := List => o -> E -> pictureList (eagonResolution E, o)

--picture--
picture = method(Options => options pictureList)
picture Matrix := o -> (M1) -> (
    if o.Display === "DisplayBlocks" then return displayBlocks M1;
    netList (pictureList(M1,o), Alignment => Center)
    )  
picture ChainComplex := Net => o -> C -> netList apply(length C, i-> picture(C.dd_(i+1), o))
picture EagonData := Net => o -> E -> picture (eagonResolution E, o)

--mapComponent--
mapComponent = method()
mapComponent(Matrix, Sequence, Sequence) := Matrix => (M,tar,src) -> (
    --Matrix should be one with labeled components, such as produced by
    --E = eagon(R,n)
    --M = E#{"dVert",4,1}
    --or
    --M = (eagonResolution(R,n)).dd_4
    M1 := flattenBlocks M;
    --use "member" and "componentsAndIndices" to check reasonableness? or try evaluating and catch error.
    try (M2 := M1^[tar]_[src]) then 
       M2
    else (
       <<endl<<"*** bad source or target symbol; use `picture M1' to check ***"<<endl<<endl; 
       error()
       )
    )

degreeZeroSurjection = method()
degreeZeroSurjection(Module,Module) := Matrix => (A,B) -> ( -- null if no surjection
    --creates a random degree 0 map f:A --> B and tests surjectivity. returns the map if true, else null.
    A' := prune A;
    pruningMapA := A'.cache.pruningMap; --from A' to A
    B' := prune B;
    pruningMapB := B'.cache.pruningMap;   
    H := Hom(A',B');
    B0 := basis(0,H); -- this seems to be total degree 0 in case of degreeLength>1
    f := homomorphism(B0*random(source B0, (ring B0)^1));
    t := coker f == 0;
    if t then pruningMapB * f * (pruningMapA)^-1 else null
)

beginDocumentation()

-*
restart
debug loadPackage("EagonResolution", Reload => true)
uninstallPackage "EagonResolution"
restart
installPackage "EagonResolution"
check "EagonResolution"
viewHelp EagonResolution
*-

--docEagonResolution
doc ///
Key
  EagonResolution
Headline
 Construct the Eagon double complex, which contains a resolution of the residue field
Description
  Text
   This package implements Eagon's algorithm for producing a not-necessarily minimal resolution of
   the residue field of a ring R = S/I where S is a polynomial ring and I is an ideal.
   The resolution constructed is minimal if and only if R is Golod. The resolution
   constructed is sometimes called the Golod or Shamash or Eagon resolution.
   
   This resolution was described, in the special case where it is minimal, by
   E.S. Golod: Homology of some local rings, Uspekhi Mat. Nauk 33 (1978), no. 5(203), 177–178.
   A general construction was described by Jack Shamash:
   The Poincaré series of a local ring II, J. Algebra 17 (1971), 1–18
   and, perhaps around the same time, by Jack Eagon.
   Eagon's construction, superficially different than Shamash'
   was not published by him, but is described in Ch. 4 of the notes
   by Gulliksen and Levin: Homology of local rings,
   Queen's Paper in Pure and Applied Mathematics, No. 20 Queen's University, Kingston, Ont. 1969.  
   
   To get a glimpse of the construction, consider the first steps. Let 
   K be the Koszul complex of S, which is the minimal S-free resolution
   of the residue field k. If numgens S = n, this begins 
   
   K_1 = S^n -> K_0 = S -> k.
   
   Let F be the mimimal S-free resolution of R.
   by the right-exactness of the tensor product, the complex
   
   R**K_1 -> R**K_0 -> k 
   
   is a presentation of k, and of course R**K_2 maps to the kernel of
   R**K_1 -> R**K_0. But there are new elements of the kernel, obtained by
   writing the generators of I, which correspond to the generators of F_1,
   in terms of the generators of the maximal ideal. Thus we must add a map
   R**F_1 -> R**K_1, and it is easy to show that the resulting complex
   
   R**F_1 ++ R**K_2 -> R**K_1 -> R**K_0 -> k
   
   is exact. There are three important points to note:
   
   1) F_0 does not occur
   
   2) F_1 occurs in homological degree 2

   3) There is a map F_1 -> K_1 that must be introduced and that does not
      come from either the complex F nor the complex K.
      
   Eagon showed how this complex can be continued to a resolution.
   The underlying graded
   module of the complex is K ** T(F'), where F' is the complex F, shifted by
   1 in homological degree so that F_i is in homological degree i+1, and truncated
   by dropping F_0; and T(F') denotes the tensor algebra on the graded module F'.

   The differentials of the complex come from the differentials in the Koszul
   complex and various maps identifying the homology, at successive stages of the 
   construction, with tensor products of modules already constructed.
   These are also the ingredients of
   the "Massey products" from topology, used by Golod to construct the complex
   in the special case where there are ``trivial Massey products'', and the resolution is therefore minimal.
   
   The command  @TO eagon@ produces a type of hashTable called an @TO EagonData@, defined in the package.
   It contains all the data produced in
   Eagon's construction of the resolution: a double complex Y^n_i, and some internal 
   maps. The vertical differental is called dVert: Y^n_i -> Y^n_{i+1} and the horizontal
   differential is dHor: Y^n_i -> Y^{n-1}_i. 

   Thus for example if $R$ is a factor ring of a polynomial ring S, then
   
   E = eagon(R,5)
   @TO eagonResolution@ E

   or simply 

   res EE

   produces the first 5 steps of a 
   (not necessarily minimal) R-free resolution of the residue field of R.  The function picture gives
   alternate ways of viewing the innards of the resolution.
   
   
  Example
   S = ZZ/101[a,b,c]
   I = ideal(a,b,c)*ideal(b,c)
   R = S/I
   E = eagon(R,5)
   F = eagonResolution E
   assert(F == res E)
  Text
   As stated above, F = K\otimes T(F'), and one can see the maps between 
   each pair of summands. We label the summand 
   K_i**F_{j_1}**..**F_{j_m} with the symbol (i,\{j_1,..,j_m\}), and we can write out
   the differentials in block form with the function picture, 
   with the option Display => "DisplayBlocks", including the labels:
  Example
   F.dd_3
   picture(F.dd_3, Display => "DisplayBlocks")
  Text
   Since the matrices can be very large, it is sometimes better to know just whether
   a given block is zero or not, and this can be obtained with the function @TO picture@,
   with the default option Display => "picture".
  Example   
   picture F.dd_3
   picture (F, Verbose => true)
   picture (F, Verbose => true, Transpose => true)
SeeAlso
   eagon
   picture
   eagonResolution
   DisplayBlocks
   Transpose

///

--docEagonData
doc ///
   Key
    EagonData
   Headline
    HashTable storing output of eagon
   Usage
    E = eagon(R,b)
   Inputs
    R:Ring
    b:ZZ
   Outputs
    E:EagonData
   Description
    Text
     The command E = eagon(R,b) puts the EagonData E in R.cache so that E== R.cache.EagonData,
     and causes the message "EagonData in <ring>.cache computed to length b"
    Example
     R = ZZ/101[x,y]/ideal"x2,xy,y2"
     E = eagon(R,3)
     E === R.cache.EagonData
   SeeAlso
    eagon
///

--doceagon
doc///
   Key 
    eagon
    (eagon, Ring, ZZ)
    [eagon,CompressBeta]
    [eagon,Verbose]
   Headline 
    compute the Eagon double complex
   Usage
    E = eagon(R,b)
   Inputs
    R:Ring
    b:ZZ
     how far to carry the computation; -1 means "delete EagonData"
   Outputs
    E:EagonData
   Description
    Text
     eagon(R,b) computes the first b columns of the Eagon double complex Y^*_* of R, 
     and caches them in a HashTable of class EagonData in of R.cache.EagonData. 
     (The command eagon(R,-1) removes this.) 
     
     Folowing
     Gulliksen-Levin we think of Y^n_* as the n-th column, and Y^*_i as the i-th row. The columns
     Y^n are not acyclic.
     The i-th row is a resolution of the i-th module of boundaries in the Koszul complex K
     of the variables of R; in particular, the
     "Eagon Resolution" is the 0-th row,
     
     Y^b_0 \to...\to Y^1_0 \to Y^0_0. 
     
     Let X_i be the free module R**H_i(K), which is also the R**F_i, where F is a minimal free
     resolution of R as a module over the polynomial ring on the same set of variables.
     
     We count X_i as having homological degree i+1.
     With this convention, Y^*_0 has the form K\otimes T(F'), where T denotes the tensor algebra
     and F' is the F_1++F_2++... .
     
     The module Y^n_i = Eagon#{0,n,i} is described in Gulliksen-Levin as:
     Y^0 = koszul vars R
     Y^{n+1}_0 = Y^n_1; and 
     for i>0, Y^{n+1}_i = Y^n_{i+1} ++ Y^n_0**X_i

     Note that Y^n_i == 0 for i>1+length koszul vars R - n, 
     
     The i-th homology of Y^n_* is H_i(Y^n) = H_0(Y^n_*)**X_i (proved in Gulliksen-Levin). Part of the
     inductive construction will be a map inducing this isomorphism
     
     alpha^n_i = eagonBeta^n_i + dHor^n_0**1: Y^n_0**X_i \to Y^{n-1}_{i+1} ++ Y^{n-1}_0**X_i = Y^n
     
     
     Assume that the differential of Y^n and the maps dVert^n and alpha^n are known. We take
     
     dHor^{n+1}_0: Y^{n+1}_0 = Y^n_1 -> Y^n_0 to be dVert^n_1. 
     
     The remaining horizontal differentials dHor^{n+1}_i: Y^{n+1} \to Y^n have source and target as follows:
     
     Y^{n+1}_i = Y^n_{i+1} ++ Y^n_0**X_i -> Y^n_i = Y^{n-1}_{i+1} ++ Y^{n-1}_0**X_i.
     
     We take dHor^{n+1}_i to be the sum of two maps: 
     
     dVert^n_{i+1}  Y^n_{i+1} -> Y^n_i ++ Y^{n-1}_0**X_i.
     
     and alpha^{n+1}_i = eagonBeta^{n+1}_i + dHor^n_0**1:  Y^n_0**X_i \to  Y^n_i ++ Y^{n-1}_0**X(i).
     
     It remains to define eagonBeta^{n+1}_i; we take this to be
     the negative of
     
     a lifting along the map from Y^{n+1}_{i-1} \subset Y^n_i to Y^n_{i-1} of the composite
     
     dVert^{n+1}_{i-1} *  (dHor^n_0 ** X_i): Y^n_0**X_i -> Y^{n-1}_0.
     
    Example
     S = ZZ/101[a,b,c]
     I = ideal(a,b)*ideal"a3,b3,c3"
     R = S/I
     needsPackage "DGAlgebras"; isGolod R
     E = eagon(R,6)
    Text
     We can see the vertical and horizontal strands, and the eagonBeta maps
    Example
     verticalStrand(E,3)
     horizontalStrand(E,2)
     horizontalStrand (E,0)
     F = eagonResolution E     
     eagonBeta E     
    Text
     With the default option CompressBeta => true, only a subset of the components of Y^{n+1}_{i-1} are used.
     To see the effect of CompressBeta => true, consider:
    Example     
     eagon(R,-1)
     E = eagon(R,6, Verbose =>true)
     eagon(R,-1)
     En = eagon(R,6,CompressBeta => false)
     eagonBeta (E,4), eagonBeta(E,5)
     eagonBeta (En,4), eagonBeta(En,5)
    Text
     There are also ways to investigate the components of dVert, dHor, and eagonBeta; see 
     @TO picture@, @TO DisplayBlocks@, and @TO mapComponent@.
   SeeAlso
    verticalStrand
    horizontalStrand
///

--doceagonResolution
doc ///
   Key
    eagonResolution
    (eagonResolution, Ring, ZZ)
    (eagonResolution, EagonData)
   Headline
    computes a resolution of the residue field
   Usage
    F = eagonResolution(R,n)
    F = eagonResolution E
   Inputs
    R:Ring
     factor ring of a polynomial ring
    n:ZZ
     number of maps to compute
    E:EagonData
     computed by eagon(R,n)
   Outputs
    F:ChainComplex
     possibly non-minimal R-free resolution of R/(ideal vars R)    
   Description
    Text
     computes the Eagon resolution
    Example
     S = ZZ/101[a,b,c]
     I = ideal(a,b,c)*ideal(b,c)
     R = S/I
     eagonResolution(R,5)
   SeeAlso
     eagon
///

--docpicture
doc ///
   Key
    picture
    (picture, Matrix)
    (picture, ChainComplex)    
    (picture, EagonData)        
    [picture, Display]
    [picture, Verbose]
    [picture, Transpose]    
   Headline
    information about components of a labeled Matrix or ChainComplex
   Usage
    N = picture M
    L = picture C
    L = picture E
   Inputs
    M:Matrix
    C:ChainComplex
    E:EagonData
     produced by eagon; picture E is equivalent to picture @TO eagonResolution@ E and to picture res E
   Outputs
    N:Net
    L:List
     List of Nets, one for each map in the complex
   Description
    Text
     The free modules that are the sources and targets of the matrices defined in the EagonData eagon(R,b) 
     generally have many components. These can be analyzed with the functions
     picture, and @TO mapComponent@. Each summand of one of these free modules has
     a label of the form (i, \{u_1..u_s\}) representing the tensor product K_i ** X_{u_1}**..**X_{u_s},
     where 0\leq i \leq numvars R and 1\leq u_t \leq projective dimension over S of R.
     Thus a block is identified by a pair of such symbols in the order target, source.

     For any labeled matrix M, picture M (with the default option Display => "picture") 
     prints a net showing which blocks of the matrix are 
     0 (represented by .); or
     nonzero and in the maximal ideal, represented by *; or
     contain a unit entry, represented by a pair of numbers, which are
     the rank of the target of the matrix and the rank of the matrix tensored with the residue field
     (the "nonminimal part").
     
     Options:
     The default option is Display => "picture".
     With the option Display=>"DisplayBlocks", picture prints the matrices in each block.
     With any other assignment such as Display =>"", Display prints the whole matrix, without showing 
     the block structure.
     With the option Verbose => true, picture prints (numrows,numcols) for each block.
     With the option Transpose => true, picture prints the data for the transposed matrix 
     (possibly useful if there are many columns in the matrix).
     
     Applied to a complex of labeled matrices such as that produced by (res, EagonData) or applied to 
     an instance of EagonData, 
     picture prints a netList of the pictures of the maps in the complex

    Example
     S = ZZ/101[a,b,c]
     I = ideal(a,b)*ideal(a,b,c)
     R = S/I
     E = eagon(R,4);
     picture E
     picture E#{"eagonBeta",3,0}
     picture E
     picture verticalStrand(E,1)

   SeeAlso
    eagon
    "eagonBeta"
    eagonResolution
    DisplayBlocks
    mapComponent
///


--docdisplayBlocks
doc ///
   Key
    DisplayBlocks
   Headline
    Display => "DisplayBlocks" option for picture
   Usage
    N = picture(M, Display => "DisplayBlocks")
   Inputs
    M:Matrix
   Outputs
    N:Net
     prints a "picture" -- a net -- showing information about the blocks
   Description
    Text
     The free modules that are the sources and targets of the matrices defined in the EagonData eagon(R,b) 
     generally have many components. These can be analyzed with the functions
     @TO picture@, and @TO mapComponent@. Each summand of one of these free modules has
     a label of the form (i, {u_1..u_s}) representing the tensor product K_i ** X_{u_1}**..**X_{u_s},
     where 0\leq i \leq numvars R and 1\leq u_t \leq projective dimension R.
     Thus a block is identified by a pair of such symbols, representing source and target.
     
     Display => "picture"; with this option, @TO picture@ does not actually print
     the entries of the matrices. But picture(M, Display => "DisplayBlocks") prints a net 
     with the matrices themselves.
    Example
     S = ZZ/101[a,b,c]
     I = ideal(a,b)*ideal(a,b,c)
     R = S/I
     E = eagon(R,4);
     C = horizontalStrand(E,0)
     picture C
     picture(C, Display => "DisplayBlocks")
   SeeAlso
    eagon
    eagonResolution
    picture
    mapComponent
    horizontalStrand
    verticalStrand
///


--doceagonBeta
doc///
   Key
    eagonBeta
    (eagonBeta,EagonData)
    (eagonBeta,EagonData,ZZ)
    [eagonBeta, Display]
    [eagonBeta, Verbose]
   Headline
    print the eagonBeta maps in the Eagon resolution
   Usage
    N = eagonBeta E
    N = eagonBeta(E,n)
   Inputs
    E:EagonData
     created by eagon(R,b)
    n:ZZ
     which eagonBeta to show
   Outputs
    N:Net
     either a "Display" display (with Display => "picture", the default) or a "displayBlocks" display
     with Display => "DisplayBlocks" or a plain matrix if Display => <anything else>.
     With Verbose => true, the display includes (rank target eagonBeta,rank source eagonBeta)
   Description
    Text
     The eagonBeta maps are the components of the Eagon resolution,
     starting from the 2nd differential that may or may not be minimal, 
     and are therefore most interesting. With the default option 
     
     Display => "picture"
     
     the pictures (which blocks are 0,nonzero, nonminimal) are shown; or
     the displayBlocks output
     with Display => "DisplayBlocks" or a plain matrix if Display => <any other string>.
     
     In the notes of Gulliksen-Levin it is proven that R is Golod if and only if the maps eagonBeta can be
     taken with values in the Koszul complex; thus in particular, if R is Golod, then 
     there are no "new" eagonBetas after eagonBeta(E,numgens R+1). Since R is Golod iff all the eagonBeta matrices have
     all entries in the maximal ideal, this proves in particular that R is Golod if and only if
     the Betti numbers of the resolution of coker vars R agree up to the step numgens R with 
     the Betti numbers of the Eagon resolution.
    Example
     S = ZZ/101[a,b,c,d]
     I = ideal(a,b,c)*ideal(a,b,c,d)
     I = ideal"a3,b3,c3"
     R = S/I
     E = eagon(R,4);
     eagonBeta(E,4)
     eagonBeta(E,4,Display => "DisplayBlocks")
     eagonBeta(E,4,Display => "")     
     eagonBeta E
   SeeAlso
    eagon
    picture
    DisplayBlocks
///

--dochorizontalStrand
doc///
   Key
    horizontalStrand
    (horizontalStrand, EagonData, ZZ)
   Headline
    extracts one horizontal strand from an Eagon double complex
   Usage
    F = horizontalStrand(E,i)
   Inputs
    E:EagonData
     produced by eagon(R,b)
    i:ZZ
     which strand
   Outputs
    F:ChainComplex
     beginning of the free resolution of the i-th boundary module of the Koszul complex
   Description
    Text
     The 0-th strand is a possibly non-minimal resolution of the residuce field.
     More generally, the i-th strand resolves the i-th boundary module in the Koszul complex of R. These
     resolutions are
     all minimal iff R is Golod.
    Example
     S = ZZ/101[x,y,z]
     R = S/((ideal(x,y))^2+ideal(z^3))
     E = eagon(R,5);
     F = horizontalStrand(E,2)
     picture F
   SeeAlso
    verticalStrand
    eagon
    picture
///

--docverticalStrand
doc ///
   Key
    verticalStrand
    (verticalStrand, EagonData, ZZ)
   Headline
    extracts one vertical strand from an Eagon double complex
   Usage
    F = verticalStrand(E,i)
   Inputs
    E:EagonData
     produced by eagon(R,b)
    i:ZZ
     which strand
   Outputs
    F:ChainComplex
     beginning of the free resolution of the i-th boundary module of the Koszul complex
   Description
    Text
     The 0-th vertical strand is the Koszul complex of R. The vertical strands are
     never resolutions unless R is regular. The key lemma in Eagon's treatment identifies
     the i-th homology H_i of the n-th vertical strand with H_0**X_i.
    Example
     S = ZZ/101[x,y,z]
     R = S/((ideal(x,y))^2+ideal(z^3))
     E = eagon(R,5);
     F = verticalStrand(E,3)
     picture F
   SeeAlso
    horizontalStrand
    eagon
    picture
///

--docmapComponent
doc ///
   Key
    mapComponent
    (mapComponent, Matrix, Sequence, Sequence)
   Headline
    extract a single component from a labeled map
   Usage
    N = mapComponent(M,tar,src)
   Inputs
    M:Matrix
     labeled map from eagon(R,b)
    tar:Sequence
     symbol of a free module components of the Eagon resolution
    src:Sequence
     symbol of a free module components of the Eagon resolution
   Outputs
    N:Matrix
   Description
    Text
     The source and target of a a map in the Eagon double complex, such as
     dVert, dHor, and eagonBeta, are direct sums of tensor products of the form
     K_i**X_{u_1}**..**X_{u_s} where K_i is a term of the Koszul complex and X_i
     is a term of the S-free resolution of R, all tensored with R.
     This tensor product is represented by a symbol that is a two element Sequence
     
     (i, \{u_1..u_s\})

     The block structure of the matrix, together with the source and
     target Sequences, can be seen from 
     picture M.
     
     The function mapComponent returns a single block.
    Example
     S = ZZ/101[a,b,c,d,e]
     R = S/(ideal(e^2,d*e^4)+(ideal"ab,ac")^2) --a non-Golod ring, generators in different degrees
     E = eagon (R,5);
     picture E#{"dHor",3,0}
     mapComponent(E#{"dHor",3,0}, (0,{1}),(1,{1}))
     picture E#{"dVert",3,1}
     mapComponent(E#{"dVert",3,1}, (0,{2}),(0,{1,1})) 
     picture E#{"eagonBeta",3,1}
     mapComponent(E#{"eagonBeta",3,1}, (0,{2}),(0,{1,1})) 
   SeeAlso
    picture
    DisplayBlocks
    eagon
///

--docgolodBetti
doc ///
   Key
    golodBetti
    (golodBetti, ChainComplex, ChainComplex, ZZ)
    (golodBetti, Module, ZZ)    
   Headline
    list the ranks of the free modules in the resolution of a Golod module
   Usage
    B = golodBetti(F,K,b)
    B = golodBetti(M,b)    
   Inputs
    F:ChainComplex
     resolution, typcally of (R = S/I)^1 over S
    K:ChainComplex
     resolution, typically of an R-module M over S
    M:Module
     R-module
    b:ZZ
     homological degree to which to carry the computation
   Outputs
    B:BettiTally
     This would be betti table of the free res of M over R, if M were a Golod module over R
   Description
    Text
     Let S be a standard graded polynomial ring. A module M over R = S/I is Golod if
     the resolution H of M has maximal betti numbers given the
     betti numbers of the S-free resolutions F of R and K of M. This resolution, H,
     has underlying graded module H = R**K**T(F'), where F' is the truncated resolution
     F_1 <- F_2... and T(F') is the tensor algebra.
     
     Since the component modules of H are given, the computation only requires the computation of
     the minimal S-free resolution of M, and then is purely numeric;
     the differentials in the R-free resolution of M are not computed.
     
     In case M = coker vars R, the result is the Betti table of the Golod-Shamash-Eagon 
     resolution of the residue field.
     
     We say that M is a Golod module (over R) if the ranks of the free modules in a minimal R-free resolution
     of M are equal to the numbers produced by golodBetti. Theorems of Levin and Lescot assert that if
     R has a Golod module, then R is a Golod ring; and that if R is Golod, then the d-th syzygy
     of any R-module M is Golod for all d greater than or equal to the projective dimension
     of M as an S-module (more generally, the co-depth of M) (Avramov, 6 lectures, 5.3.2).
     
    Example
     S = ZZ/101[a,b,c]
     I = (ideal(a,b,c^2))^2
     F = res(S^1/I)
     K = res coker vars S
     R = S/I
     E = eagon(R,6);
     golodBetti(F,K,6)
     betti res (coker vars R, LengthLimit => 6)
     betti eagonResolution E     
   SeeAlso
    eagon
    eagonResolution
///
-*
--doceagonSymbols
doc ///
   Key
    eagonSymbols
    (eagonSymbols, ZZ, ZZ)
   Headline
    symbols of the components of a module in the Eagon double complex    
   Usage
    L = eagonSymbols(n,i)
   Inputs
    n:ZZ
     column indix
    i:ZZ
     row index
   Outputs
    L:List
     list of symbols
   Description
    Text
     Each module in the Eagon double complex is a direct sum of tensor products of
     a component of the Koszul complex and a list of modules X_i. Such
     a tensor product is represented by a symbol that is a two element Sequence
     of the form
     
     (i, {u_1..u_s})
     
     representing K_i ** X_{u_1} ** .. ** X_{u_s}. 
     
     The function eagonSymbols(n,i) produces the list of symbols of the summands
     of the module Y^n_i in the n-th column, i-th row of the double complex.
     This is done arithmetically, that is,  without computing the double complex.
    Example
     L = eagonSymbols(3,2)
    Text
     This is the list of symbols associated to the source of (for example)
     the 2-nd differential in the 3-rd vertical strand of the double complex.
    Example
     S = ZZ/101[a,b,c,d,e]
     R = S/(ideal vars S)^2 --a Golod ring
     E = eagon(R,3);
     V = verticalStrand(E,3)
     picture (V.dd_2)
    Text
     Compare the top row with
    Example
     L
   SeeAlso
    eagon
    picture
    DisplayBlocks
    mapComponent
///
*-

--docDisplay
doc ///
   Key
    Display
   Headline
    Option for eagonBeta, default is "picture"
   Usage
    eagonBeta(E,Display => "picture")
   Inputs
    E:EagonData
   Description
    Text
     if Display=>"picture" then @TO picture@ is invoked; if Display =>"DisplayBlocks" 
     then a net with the matrices (the "blocks") is produced.
    Example
     R = ZZ/101[x,y,z]/ideal"x3,y3,z3"
     E = eagon(R,5);
     eagonBeta(E,3)
     eagonBeta(E,3,Display =>"DisplayBlocks")
   SeeAlso
    eagon
///
--docCompressBeta
doc ///
   Key
    CompressBeta
   Headline
    CompressBeta is an option for eagon, default is true
   Usage
    E = eagon(R,b,CompressBeta =>bool1, Verbose =>bool2)
   Inputs
    R:Ring
    b:ZZ
    bool1: Boolean
    bool2: Boolean
   Outputs
    E:EagonData
   Description
    Text
     eagonBeta(E,i) := E#{"eagonBeta",n,0} 
     is defined by lifting another map along the map dVert^n_i. If
     CompressBeta => true, the default, then the lifting uses
     the smallest initial subsequence of the blocks of source dVert as possible,
     and thus the columns of picture eagonBeta(E,i) will have fewer nonzero entries.
     if Verbose =>true, then data about the usage is printed.
    Example
     R = ZZ/101[x,y,z]/ideal"x3,y3,z3"
     E = eagon(R,4,CompressBeta =>true, Verbose =>true);
   SeeAlso
    eagon
    eagonBeta
///
doc ///
   Key
    Transpose
   Headline
    Transpose => false, default option for picture
   Usage
    N = picture(M, Transpose => b)
   Inputs
    M:Matrix
     labeled matrix
    b:Boolean
   Outputs
    N: Net
   Description
    Text
     With the option Transpose => true, picture prints the picture of the transposed matrix; when 
     the matrix has many more columns than rows this makes it easier to read.
    Example
     S = ZZ/101[a,b]
     R = S/ideal"a2,b2"
     E = eagon(R,3)
     picture res E
     picture(res E, Transpose => true)
   SeeAlso
    DisplayBlocks
    Verbose
///



doc ///
   Key
    (net, EagonData)
   Headline
    prints the homological degree to which the EagonData has been computed
   Usage
    s = net E
   Inputs
    E:EagonData
   Outputs
    s:Net
   Description
    Text
     prints the homological degree to which the EagonData has been computed
    Example
     R = ZZ/101[a,b]/ideal"a2,b2"
     E = eagon(R,4)
     net E
   SeeAlso
    EagonData
///
doc ///
   Key
    (resolution, EagonData)
   Headline
    outputs the resolution that is the 0th row of the Eagon double complex
   Usage
    C = resolution E
   Inputs
    E:EagonData
   Outputs
    C:ChainComplex
   Description
    Text
     This command is equivalent to @TO (eagonResolution, EagonData)@.
    Example
     R = ZZ/101[a,b]/ideal"a2,b2"
     E = eagon(R,4)
     C = resolution E
     picture C
   SeeAlso
    eagon
    EagonData
    eagonResolution
    picture
///

----TESTS
-*
restart
uninstallPackage "EagonResolution"
installPackage "EagonResolution"
check EagonResolution
viewHelp EagonResolution
loadPackage("EagonResolution", Reload => true)
*-

TEST///
     S = ZZ/101[a,b,c]
     I = ideal(a,b)*ideal"a3,b3,c3"
     R = S/I
     E = eagon(R,6,CompressBeta =>true);
     En = eagon(R,6,CompressBeta => false);
     F = eagonResolution E
     Fn= eagonResolution En
     assert(F.dd^2 == 0)
     assert(Fn.dd^2 == 0)
     assert(all(5, i->prune HH_(i+1) F == 0))
     assert(all(5, i->prune HH_(i+1) Fn == 0))
///

TEST///
debug EagonResolution
S = ZZ/101[a,b,c]/ideal(b^2,c^2) -- complete intersection
B = 3
E = eagon(S,B);
F = eagonResolution E
M = F.dd_3
M' = extractBlocks(M,{0,2})
M'' = map(target M', source M', matrix {{c, 0, -b, 0, 0, 0, 0, 0}, {-b, 0, 0, -c, 0, 0, 0, 0}, {a, 0, 0, 0, 0, -c, b, 0}, {0, 0, a, 0, b,
      0, c, 0}, {0, 0, 0, a, 0, b, 0, c}})
assert(M' == M'')
///

TEST///
S = ZZ/101[a,b,c]
R = S/(ideal"ab,ac")^2 --a simple Golod ring on which to try this
b = 6
E = eagon(R,b);
F = eagonResolution E
G = res (coker vars R, LengthLimit => b)
assert(betti F == betti G)
assert(F.dd^2== 0)
assert(all(b-1, i-> prune (HH_(i+1) F) == 0))
///

TEST///
debug EagonResolution
S = ZZ/101[a,b,c,d,e]
A = S^1
B = S^2
A' = labeler((0,{1}),A)
indices A'
B' = labeler((0,{2}),B)
assert(indices eTensor(A',B') == {(0, {1, 2})})
assert(indices (T = eTensor(B',A')) == {(0, {2, 1})})
assert(indices trimWithLabel T ==  {(0, {2, 1})})
///

TEST/// -- test of eagon
S = ZZ/101[a,b,c]
R = S/(ideal"ab,ac")^2 --a simple Golod ring on which to try this
bound = 6
E = eagon(R,bound);
Y = apply(bound, n-> verticalStrand(E,n))
assert(all(#Y, n->((Y_n).dd)^2 == 0))
assert all(#Y, n->isHomogeneous Y_n)
F = eagonResolution(R,bound)
F = eagonResolution E
assert isHomogeneous F
assert all(bound-1,i-> prune HH_(i+1) F == 0)
assert(betti res(coker vars R,LengthLimit => bound) == betti F)

S = ZZ/101[a,b,c,d,e]
R = S/(ideal(e^2,d*e^4)+(ideal"ab,ac")^2) --a non-Golod ring, generators in different degrees
E = eagon(R,5);
Y = apply(5, n -> verticalStrand(E,n));
assert(all(#Y, n->((Y_n).dd)^2 == 0))
assert all(#Y, n->isHomogeneous Y_n)
F = eagonResolution(R,5)
assert isHomogeneous F
assert all(4,i-> prune HH_(i+1) F == 0)
///


TEST///
debug needsPackage "EagonResolution"
assert(eagonSymbols(1,2) == {(3, {}), (0, {2})})
assert (eagonSymbols(2,1) == eagonSymbols(3,0))
assert(eagonSymbols(1,3) == {(4, {}), (0, {3})})
assert(eagonSymbols(2,2) == {(4, {}), (0, {3}), (1, {2})})
assert(eagonSymbols(2,0) == {(2, {}), (0, {1})})
assert(eagonSymbols(3,1) == {(4, {}), (0, {3}), (1, {2}), (2, {1}), (0, {1, 1})})
assert(eagonSymbols(3,2) == {(5, {}), (0, {4}), (1, {3}), (2, {2}), (0, {1, 2})})
///

///
restart
loadPackage("EagonResolution",Reload => true)
needsPackage "DGAlgebras"
///
TEST///
S = ZZ/101[x,y,z]
I = trim(ideal(x,y)*ideal"x,y2,z")
R = S/I
F = res I
G = res coker vars S
b = 6
H = res(coker vars R,LengthLimit =>6)
E = eagon(R,b);
assert(betti eagonResolution E == betti H)
assert(golodBetti(F,G,b) == betti H)
assert (golodBetti (coker vars R,b) == betti H)
///


-- TODO: PLACE INTO M2 CORE
--SOME USEFUL INTERNAL FUNCTIONS
--    "compositions"
--    "isDegreeZeroSurjection",
--    "isIsomorphic"
-*
compositions(ZZ,ZZ,ZZ) := (nparts, k, maxelem) -> (
    -- nparts is the number of terms
    -- k is the sum of the elements
    -- each element is between 0 and maxelem.
     compositionn := (n,k) -> (
	  if n===0 or k < 0 then {}
	  else if k===0 then {toList(n:0)}
	  else (
          set1 := apply(compositionn(n-1,k), s -> s | {0});
          set2 := apply(compositionn(n,k-1), s -> s + (toList(n-1:0) | {1}));
          set2 = select(set2, s -> s#(n-1) <= maxelem);
          join(set1, set2)
          )
      );
     compositionn = memoize compositionn;
     result := compositionn(nparts,k);
     compositionn = null;
     result
     );

isDegreeZeroSurjection := method()
isDegreeZeroSurjection(Module,Module) := Boolean => (A,B) -> (
    --tests a random degree 0 map f:A --> B to see whether its a surjection, 
    --and returns the answer. If "true" and  o.Verbose == true then returns f.
    H := Hom(A,B);
    B0 := basis(0,H); -- this seems to be total degree 0 in case of degreeLength>1
    f := homomorphism(B0*random(source B0, (ring B0)^1));
    coker f == 0
)

-- the following function works for modules in the graded case.
-- and produces (only) graded isomorphisms.
isIsomorphic = method()
isIsomorphic(Module,Module) := (A,B) -> (
    --tests random degree 0 maps A->B, B->A and returns true
    --if both are surjective.
    if not(isHomogeneous A and isHomogeneous B) then 
	  error"not implemented for inhomogeneous modules";
    Ap := prune A;
    Bp := prune B;
    dA := set flatten degrees source gens Ap;
    dB := set flatten degrees source gens Bp;
    if dA =!= dB then return false;
    degreeZeroSurjection(Ap,Bp) =!= null and degreeZeroSurjection(Bp,Ap) =!= null
    )
*-

TEST/// --test of degreeZeroSurjection
debug needsPackage "EagonResolution"--degreeZeroSurjection is not exported.
S = ZZ/101[a,b]
A = S^{1,0}; B = S^{0,1};B1 = B/((ideal a)*B)
f = degreeZeroSurjection(A,B)
assert(source f == A)
assert(target f == B)
assert(coker f == 0)

f = degreeZeroSurjection(A,B1)
assert(source f == A)
assert(target f == B1)
assert(coker f == 0)

f = degreeZeroSurjection(B1,A)
assert(f===null)
///

end--
------------------------------------
restart
uninstallPackage "EagonResolution"
restart
installPackage "EagonResolution"
check EagonResolution

viewHelp EagonResolution
