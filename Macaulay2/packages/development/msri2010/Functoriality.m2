
newPackage(
	  "Functoriality",
	  Version => "0.1",
	  Date => "January 8, 2010",
	  Authors => {{Name => "Chris Cunningham",  Email => "cjc258@cornell.edu"},{Name => "Jason McCullough", Email => "jason.g.mccullough@gmail.com"}},
          Headline => "Functoriality",
	  DebuggingMode => true
	  )
     
export {connectingHomomorphism,cylinder,connectingExt,connectingTor,horseshoe,toHom,Prune}

horseshoe = method()
horseshoe(Matrix,Matrix) := (f,g) -> (
    -- This resolves source f and target g, then builds the horseshoe
    -- resolution of target f = source g.
    if target f != source g then error "Expected composable maps of modules.";
    if kernel f != 0 then error "Expected first map injective.";
    if cokernel g != 0 then error "Expected second map surjective.";
    if image f != kernel g then error "Expected short exact sequence of modules.";
    C := res source f;
    D := new ChainComplex;
    D.ring = ring f;
    E := res target g;
    complete C;
    complete E;
    spots := F -> select(keys F, i -> class i === ZZ);
    scan(toList(set spots C + set spots E), i -> D#i = C_i ++ E_i);
    complete D;
    complete C.dd;
    complete E.dd;
    F := map(D,C,i -> id_(C_i) || map(E_i,C_i,0));
    G := map(E,D,i -> map(E_i,C_i,0) | id_(E_i));
     
    -- this should be the i=0 map that would go at the end of the resolution,a map to target f.
    D0 := map(target f,D_0,matrix f | (inducedMap(target g,E_0) // g));
    K := kernel D0;
     
    -- The matrix we want is in block form like this:
    -- 
    -- C L
    -- 0 E  
    --
    -- But mathematically extracting L and E separately is awkward. So
    -- instead the map is written here as (C || 0) | (L || E)
    -- where L || E is the map found by lifting a map that is to the kernel of the previous.  
    scan(keys(set spots C.dd + set spots E.dd),   
       i ->
       -- Here is the map from kernel D_(i-1) to E_(i-1):
       (P := map(E_(i-1),source gens K,G_(i-1)*gens K),
	P = E.dd_i // P,
	P = gens K * P,
        D.dd#i = map(D_(i-1),D_i, (C.dd_i    || map(E_(i-1),C_i,0)) | P),
	K = kernel D.dd#i)); 
    (D,F,G)
    )


connectingHomomorphism = method(Options => {Prune => false})
connectingHomomorphism(ChainComplexMap,ChainComplexMap) := ChainComplexMap => opts -> (f,g) -> (
     if target f != source g then error "expected composable maps of chain complexes";
     if ker g != image f then error "expected exact sequence of chain complexes";
     if ker f != 0 then error "expected first chain complex map injective";  
     if coker g != 0 then error "expected second chain complex map surjective";
     --- Here is the canonical map from target g to cone g
     beta := map(cone g, target g, i -> id_(target g)_i || map((source g)_(i-1),(target g)_i,0));   
     --- And here is the degree 1 map from source f to cone g.
     alpha := map(cone g, source f, i -> map((target g)_(i+1),(source f)_i,0) || f_i, Degree=>1);  
     --- When we take homology of both maps,
     Hbeta := HH beta;
     Halpha := HH alpha;
     --- then compose one with the inverse of the other,
     Halphainv := map(source Halpha, target Halpha, i -> inverse Halpha_(i-1), Degree=>-1);
     --- We have the connecting homomorphism.
     if opts.Prune then return prune (Halphainv*Hbeta);
     Halphainv * Hbeta
     )

connectingHomomorphism(ZZ,ChainComplexMap,ChainComplexMap) := Matrix => opts -> (n,f,g) -> (
     if target f != source g then error "expected composable maps of chain complexes";
     if ker g != image f then error "expected exact sequence of chain complexes";
     if ker f != 0 then error "expected first chain complex map injective";  
     if coker g != 0 then error "expected second chain complex map surjective";
     --- Here is the canonical map from target g to cone g
     beta := map(cone g, target g, i -> id_(target g)_i || map((source g)_(i-1),(target g)_i,0));   
     --- And here is the degree 1 map from source f to cone g.
     alpha := map(cone g, source f, i -> map((target g)_(i+1),(source f)_i,0) || f_i, Degree=>1);  
     --- Here we only care about one spot in the homology of both maps. 

     if opts.Prune then return prune ((inverse HH_(n-1) alpha) * HH_n beta);
     (inverse HH_(n-1) alpha) * HH_n beta
     )

coneInjectTarget = method()
coneInjectTarget(ChainComplexMap) := ChainComplexMap => (f) -> (
      C := source f;
      D := target f;
      E := cone f;
      map(E,D,k->E_k_[0])
      )

coneInjectSource = method()
coneInjectSource(ChainComplexMap) := ChainComplexMap => (f) -> (
      C := source f;
      D := target f;
      E := cone f;
      map(E,C[-1],k->E_k_[1])
      )
 
coneProjectTarget = method()
coneProjectTarget(ChainComplexMap) := ChainComplexMap => (f) -> (
      C := source f;
      D := target f;
      E := cone f;
      map(D,E,k->E_k^[0])
      )
  
coneProjectSource = method()
coneProjectSource(ChainComplexMap) := ChainComplexMap => (f) -> (
      C := source f;
      D := target f;
      E := cone f;
      map(C[-1],E,k->E_k^[1])
      )

connectingExt = method(TypicalValue => ChainComplexMap)
connectingExt(Module,Matrix,Matrix) := ChainComplexMap => (N,f,g) -> (
     D := res N;
     connectingHomomorphism(Hom(D,f),Hom(D,g),Prune => true)
     ) 
connectingExt(Matrix,Matrix,Module) := ChainComplexMap => (f,g,N) -> (
     (D,F,G) := horseshoe(f,g);
     connectingHomomorphism(Hom(G,N),Hom(F,N), Prune => true)
     ) 
connectingExt(ZZ,Module,Matrix,Matrix) := ChainComplexMap => (n,N,f,g) -> (
     D := res N;
     connectingHomomorphism(n,Hom(D,f),Hom(D,g), Prune => true)
     ) 
connectingExt(ZZ,Matrix,Matrix,Module) := ChainComplexMap => (n,f,g,N) -> (
     (D,F,G) := horseshoe(f,g);
     connectingHomomorphism(n,Hom(G,N),Hom(F,N), Prune => true)
     ) 
connectingTor = method(TypicalValue => ChainComplexMap)
connectingTor(Module,Matrix,Matrix) := ChainComplexMap => (N,f,g) -> (
     (D,F,G) := horseshoe(f,g);
     connectingHomomorphism(N ** F,N ** G)
     ) 
connectingTor(Matrix,Matrix,Module) := ChainComplexMap => (f,g,N) -> (
     (D,F,G) := horseshoe(f,g);
     connectingHomomorphism(F ** N,G ** N)
     ) 
connectingTor(ZZ,Module,Matrix,Matrix) := ChainComplexMap => (n,N,f,g) -> (
     (D,F,G) := horseshoe(f,g);
     connectingHomomorphism(n,N ** F,N ** G)
     ) 
connectingTor(ZZ,Matrix,Matrix,Module) := ChainComplexMap => (n,f,g,N) -> (
     (D,F,G) := horseshoe(f,g);
     connectingHomomorphism(n,F ** N,G ** N)
     ) 
     

Hom(ChainComplex,Matrix) := ChainComplexMap => (D,f) -> (
     map(Hom(D,target f),Hom(D,source f),i -> Hom(D_-i,f))
     )

Hom(Matrix,ChainComplex) := ChainComplexMap => (f,D) -> (
     map(Hom(source f,D),Hom(target f,D),i -> Hom(f,D_i))
     )

Hom(Module, Matrix) := Matrix => (N,f) -> (
    -- previously, for free modules only, dual N ** f
    F1 := (dual target presentation N) ** matrix f;
    --F2 := F1 * gens Hom(N,source f);
    --map(Hom(N,target f),Hom(N,source f), F2 // gens Hom(N,target f))
    inducedMap(Hom(N,target f),Hom(N,source f),F1)
    )

Hom(Matrix,Module) := Matrix => (f,N) -> (
    -- previously, for free modules only, transpose f ** N.
    -- Old try: map(Hom(source f,N),Hom(target f,N), transpose matrix f ** N)
    F1 := (transpose matrix f) ** target presentation N;
    --F2 := F1 * gens Hom(target f,N);
    --map(Hom(source f,N),Hom(target f,N),F2 // gens Hom(source f,N)) 
    inducedMap(Hom(source f,N),Hom(target f,N),F1)
    )

Hom(Matrix,Matrix) := Matrix => (f,g) -> (
    A := source f;
    B := target f;
    C := source g;
    D := target g;
    -- This means we are given maps f: A -> B and g: C -> D.
    -- So we need a map Hom(B,C) -> Hom(A,D).
    inducedMap(Hom(A,D),Hom(B,C),(transpose matrix f) ** matrix g))

Ext(ZZ, Module, Module) := Module => (i,M,N) -> (
     R := ring M;
     if not isCommutative R then error "'Ext' not implemented yet for noncommutative rings.";
     if R =!= ring N then error "expected modules over the same ring";
     E := R^0;
     if i < 0 then E = R^0
     else 
     if i === 0 then E = Hom(M,N)     
     else (
          C := resolution(M,LengthLimit=>i+1);
          b := C.dd;
          complete b;
          if b#?i then (
               if b#?(i+1) 
               then E = prune homology(Hom(b_(i+1),N), Hom(b_i,N))
               else E = prune cokernel Hom(b_i,N))
          else (
               if b#?(i+1) 
               then E = prune kernel Hom(b_(i+1),N)
               else E = prune Hom(C_i,N)));
     E.cache.Ext = {i,M,N};
     E
     );

-- Computes the map of Tor modules Tor_i(f,N)
-- Returns the map (Matrix) Tor_i(source f,N) --> Tor_i(target f,N)
Tor(ZZ, Matrix, Module) := Matrix => (i,f,N) -> (
	if ring source f != ring N then error "expected the same ring";
	R := ring N;
	if not isCommutative R then error "'Tor' not implemented yet for noncommutative rings";
	if i < 0 then map(R^0,R^0,0)
	else if i === 0 then  f ** N
	else (
	     F := resolution(f,LengthLimit=>i+1);
	     C := source F;
	     D := target F;
	     tC := C ** N;
	     tD := D ** N;	     
	     tf := F_i ** N;
	     inducedMap(HH_i(tD), HH_i(tC), tf)
	     )
	)
   

-- Computes the map of Tor modules Tor_i(N,f)
-- Returns the map (Matrix) Tor_i(N, source f) --> Tor_i(N, target f)
Tor(ZZ, Module, Matrix) := Matrix => (i,N,f) -> (
	if ring source f != ring N then error "expected the same ring";
	R := ring N;
	if not isCommutative R then error "'Tor' not implemented yet for noncommutative rings";
	if i < 0 then map(R^0,R^0,0)
	else if i === 0 then  N ** f
	else (
	     F := resolution(f,LengthLimit=>i+1);
	     C := source F;
	     D := target F;
	     tC := N ** C;
	     tD := N ** D;	     
	     tf := N ** F_i;
	     inducedMap(HH_i(tD), HH_i(tC), tf)
	     )
	)


toHom = method()
toHom(Matrix) := f -> (
     -- This takes a map f: M --> N and gives an
     -- element of Hom(M,N), in the form of a map
     -- R^1 --> Hom(M,N).
     M := source f;
     N := target f;
     R := ring f;
     -- The ambient module of Hom(M,N) is dual M_0 ** N_0
     -- where M_0 and N_0 are free modules on generators 
     -- of M and N. Given a generator m_i of M and n_j of
     -- N, the ring element for m_i* ** n_j is in the (j,i)
     -- slot of the matrix f: M --> N.  
     -- So we need f_0 || f_1 || ...
     g := mingle entries f;
     inducedMap(Hom(M,N),R^1,transpose matrix(R,{g}))
     )

cylinder = method(TypicalValue => ChainComplex)
cylinder ChainComplexMap := ChainComplex => f -> (
     if f.degree =!= 0 then error "expected a map of chain complexes of degree zero";
     C := source f;
     D := target f;
     E := new ChainComplex;
     E.ring = ring f;
     complete C;
     complete D;
     spots := F -> select(keys F, i -> class i === ZZ);
     scan(toList(set spots C /( i -> i+1 ) + set spots D), i -> E#i = C_i ++ D_i ++ C_(i-1));
            complete C.dd;
            complete D.dd;
            bump := x -> apply(x, i -> i+1);
            scan(keys(set bump spots C.dd + set spots D.dd + set bump spots f),
                 i -> E.dd#i =                  
		 C.dd_i              |  map(C_(i-1),D_i,0)       |   id_(C_(i-1))  ||
		 map(D_(i-1),C_i,0)  |  D.dd_i                   |      f_(i-1)    ||
                 map(C_(i-2),C_i,0)  |  map(C_(i-2),D_i,0)       |   - C.dd_(i-1)
                 );
            E)
       
beginDocumentation()
needsPackage "SimpleDoc";

doc ///
  Key
    Functoriality
  Headline
    A package to make Hom, Ext, Tor, and some other things properly functorial.
///
doc ///
  Key
    connectingHomomorphism
    (connectingHomomorphism,ChainComplexMap,ChainComplexMap)
  Headline
    The connecting homomorphism for a short exact sequence of chain complexes.
  Usage
    d = connectingHomomorphism(f,g)
  Inputs
    f:ChainComplexMap  
      with kernel 0 and image f = kernel g
    g:ChainComplexMap
      with cokernel 0 and image f = kernel g
  Outputs
    d:ChainComplexMap
      the connecting homomorphism, a degree -1 map from (HH target g) to (HH source f).
  Description
   Text
     Computes the connecting homomorphism for a short exact sequence of chain complexes.
   Example
     R = QQ[x]
     A = chainComplex{map(R^1,R^0,0)}
     B = chainComplex{map(R^1,R^1,x)}
     C = chainComplex{map(R^0,R^1,0)}
     F1 = map(B,A,i -> (if i==0 then 1 else 0))
     F2 = map(C,B,i -> (if i==1 then 1 else 0))
     connectingHomomorphism(F1,F2)
///
doc ///
  Key
    (connectingHomomorphism,ZZ,ChainComplexMap,ChainComplexMap)
  Headline
    A single degree of the connecting homomorphism for a short exact sequence of chain complexes.
  Usage
    d = connectingHomomorphism_n(f,g)
  Inputs
    n:ZZ
    f:ChainComplexMap  
      with kernel 0 and image f = kernel g
    g:ChainComplexMap
      with cokernel 0 and image f = kernel g
  Outputs
    d:Matrix
      One piece of the connecting homomorphism, a map from (HH_n target g) to (HH_(n-1) source f).
  Description
   Text
     This is used to avoid calculating the entire chain complex map.
  SeeAlso
    (connectingHomomorphism,ChainComplexMap,ChainComplexMap)
///
doc ///
  Key
    cylinder
    (cylinder,ChainComplexMap)
  Headline
    The mapping cylinder for a chain complex map. 
  Usage
    C = cylinder f 
  Inputs
    f:ChainComplexMap
  Outputs
    C:ChainComplex
  Description
   Text
     Computes the mapping cylinder of a chain complex map. 
     Here we have F, the inclusion of the circle S^1 into D^2. The mapping cylinder is contractible.
   Example
     R = ZZ
     A = chainComplex(matrix{{1,-1},{-1,1}})
     B = chainComplex(matrix{{1,-1},{-1,1}},matrix{{1},{1}})
     F = map(B,A,i -> matrix{{1,0},{0,1}})
     cylinder F
     prune HH cylinder F == gradedModule(ZZ^1)
  SeeAlso
    (cone,ChainComplexMap)
///
doc ///
  Key
    horseshoe
    (horseshoe,Matrix,Matrix)
  Headline
    Computes a horseshoe resolution given a short exact sequence of modules. 
  Usage
    (D,F,G) = horseshoe(f,g) 
  Inputs
    f:Matrix
      with kernel 0 and image f = kernel g
    g:Matrix
      with cokernel 0 and image f = kernel g
  Outputs
    D:ChainComplex
      containing a resolution D of target f = source g, and two maps F,G forming a short exact sequence of chain complexes res f -> D -> res g.
    F:ChainComplexMap
      from res source f to D
    G:ChainComplexMap
      from D to res target g, forming a short exact sequence of chain complexes.
  Description
   Text
     Takes a short exact sequence of modules and returns a short exact sequence of chain complexes which are resolutions of those modules. It uses the res command to find resolutions of two of the modules, then computes a resolution for the third that will fit inbetween.
   Example
     S = QQ[x,y]
     I = ideal{x^3,x*y}
     J = ideal{x^3,x*y,y^3}
     f = map(comodule I,comodule (I:y^3),y^3)
     g = map(comodule J,comodule I,1)
     (D,F,G) = horseshoe(f,g)
     prune HH_0 D == comodule I
     image F == kernel G
   Text
     Using this function, we can construct the long exact sequences in Ext and Tor; for example:
   Example
     k = comodule ideal gens S
     F1 = HH Hom(F,k)
     G1 = HH Hom(G,k)
     H1 = connectingHomomorphism(Hom(G,k),Hom(F,k))
     image G1 == kernel F1
     image H1 == kernel G1
     image F1 == kernel H1     
   Text
     The functions connectingExt and connnectingTor utilize horseshoe for the above purpose. 
///
doc ///
  Key
    connectingExt
  Headline
    Computes the connecting homomorphism on Ext.
///
doc ///
  Key
    (connectingExt,Matrix,Matrix,Module)
    (connectingExt,ZZ,Matrix,Matrix,Module)
  Headline
    Computes the connecting homomorphism on Ext.
  Usage
    F = connectingExt(f,g,N)
  Inputs
    f:Matrix
    g:Matrix
      with f and g forming a short exact sequence of modules.
    N:Module
  Outputs 
    F:ChainComplexMap
      the connecting map to Ext^*(target g,N) from Ext^*(source f,N)
  Description
   Text
     This function uses connectingHomomorphism and horseshoe to produce the connecting homomorphism on Ext.
   Example
     S = QQ[x,y]
     I = ideal{x^3,x*y}
     J = ideal{x^3,x*y,y^3}
     f = map(comodule I,comodule (I:y^3),y^3)
     g = map(comodule J,comodule I,1)
     k = comodule ideal gens S
     connectingExt(f,g,k)
   Text
     We can also pick out one degree to avoid calculating all the others.
   Example
     connectingExt_-1(f,g,k)
///
doc ///
  Key
    (connectingExt,Module,Matrix,Matrix)
    (connectingExt,ZZ,Module,Matrix,Matrix)
  Headline
    Computes the connecting homomorphism on Ext.
  Usage
    F = connectingExt(N,f,g)
  Inputs
    N:Module
    f:Matrix
    g:Matrix
      with f and g forming a short exact sequence of modules.
  Outputs 
    F:ChainComplexMap
      the connecting map from Ext^*(N,target g) to Ext^*(N,source f)
  Description
   Text
     This function uses connectingHomomorphism and horseshoe to produce the connecting homomorphism on Ext.
   Example
     S = QQ[x,y]
     M = comodule ideal"x2,xy,y2"
     k = comodule ideal"x,y"
     N = comodule ideal"x,y2"
     f = map(M,k,x)
     g = map(N,M,1)     
     connectingExt(k,f,g)
   Text
     We can also pick out one degree to avoid calculating all the others.
   Example
     connectingExt_-1(k,f,g)
///
doc ///
  Key
    Prune
    [connectingHomomorphism,Prune]
  Headline
    an option for connectingHomomorphism that prunes the map before returning it.
///
doc ///
  Key
    connectingTor
  Headline
    Computes the connecting homomorphism on Tor.
///
doc ///
  Key
    (connectingTor,Matrix,Matrix,Module)
    (connectingTor,Module,Matrix,Matrix)
    (connectingTor,ZZ,Matrix,Matrix,Module)
    (connectingTor,ZZ,Module,Matrix,Matrix)
  Headline
    Computes the connecting homomorphism on Tor.
  Usage
    F = connectingTor(f,g,N)
  Inputs
    f:Matrix
    g:Matrix
      with f and g forming a short exact sequence of modules.
    N:Module
  Outputs 
    F:ChainComplexMap
      the connecting map from Tor(target g,N) to Tor(source f,N), or from Tor(N,target g) to Tor(N,source f).
  Description
   Text
     This function uses connectingHomomorphism and horseshoe to produce the connecting homomorphism in the long exact sequence of Tor.
   Example
     S = QQ[x,y]
     I = ideal{x^3,x*y}
     J = ideal{x^3,x*y,y^3}
     f = map(comodule I,comodule (I:y^3),y^3)
     g = map(comodule J,comodule I,1)
     k = comodule ideal gens S
     connectingTor(f,g,k)
   Text
     We can also avoid some computation by choosing one degree to calculate.
   Example
     connectingTor_1(f,g,k)
///       
doc ///
  Key
    toHom
    (toHom,Matrix)
  Headline
    Given a map, get the element of the relevant Hom module.
  Usage
    g = toHom(f)
  Inputs
    f:Matrix
      say from modules N <- M.  
  Outputs
    g:Matrix
      an element of Hom(M,N), in the form of a map Hom(M,N) <- R^1.  
  Description
   Text
     This is the same example as in the documentation for homomorphism(Matrix):    
   Example
     R = QQ[x,y,z]/(y^2-x^3)
     f = map(R^1,module ideal(x,y),{{y,x^2}})
     g = toHom(f)
   Text
     toHom acts as an inverse to the function homomorphism.
   Example
     homomorphism g == f
  SeeAlso
    homomorphism
///        

restart
loadPackage"Functoriality"
R = ZZ/3[x_1..x_5]
m = ideal vars R
N = comodule m
M = comodule m^2
L = m/m^2
f = inducedMap(N,M)
e = inducedMap(M,L)
P = N
time connectingTor_1(P,e,f);

time (D,F,G) = horseshoe(e,f);
time F2 = P ** F;
time G2 = P ** G;
time connectingHomomorphism(1,F2,G2)


