newPackage(
"MCMApproximations",
Version => "1.1",
Date => "April 3, 2013, revised August 21, 2017",
Authors => {{Name => "David Eisenbud",
Email => "de@msri.org",
HomePage => "http://www.msri.org/~de"}},
Headline => "MCM approximations and complete intersections",
Keywords => {"Commutative Algebra"},
DebuggingMode => false
)

export {
    "approximation", 
    "coApproximation",
    "approximationSequence",
    "coApproximationSequence",
    "Total", -- option for approximation
    "CoDepth", -- option for approximation
--   "approx", --synonym for approximation
    "auslanderInvariant",
    "profondeur", -- should be depth, but that's taken
    "syzygyModule",
    "socleDegrees",
    "setupRings",
    "Characteristic", -- option for setupRings
    "Randomize", -- option for setupRings
    "setupModules"
    }

-* test: The following code crashes M2 v 8.2
S = ZZ/101[a]
R = S/ideal(a^2)
res (coker vars R, LengthLimit => 0)
*-

depth Module := M-> profondeur M

socleDegrees = method()
socleDegrees Module := M ->(
    R := ring M;
    k := coefficientRing R;
    if not isField k then error"coefficient ring not a field";
     flatten degrees target basis Hom(coker vars R,M)    
    )

syzygyModule = method(Options=>{CoDepth => -1})
syzygyModule(ZZ,Module) := opts -> (k,M) -> (
    if k === 0 then return M;
    F := null;

    if k>0 then (
	F = res(M, LengthLimit => k+1);
	return coker F.dd_(k+1));
    
    if k<0 then (
	n := numgens ring M;
	if opts.CoDepth == 0 then
	n = 1 else
	if opts.CoDepth >0 then
	n = opts.CoDepth;
	F  = res(M, LengthLimit => n);
	M1 := image dual F.dd_(n);
	G := res(M1, LengthLimit => -k+n);
    return image dual G.dd_(-k+n));
    )

profondeur = method()
profondeur(Ideal, Module) := (I,M) ->(
    --requires R to be an affine ring (eg NOT ZZ[x])
    R := ring M;
    d := max(1,dim M); -- d=0 causes a crash
    if not isCommutative R then error"profondeur undefined for noncommutative rings";
    F := M**dual res (R^1/I, LengthLimit => d);
    i := 0;    
    while HH_i F == 0 do i=i-1;
    -i)

profondeur Module := M  ->(
    --profondeur of a module with respect to the max ideal, via finite proj dim
    --gives error if the ultimate coefficient ring of R = ring M is not a field.
    R := ring M;
    if not isCommutative R then error"profondeur undefined for noncommutative rings";
    (S,F) := flattenRing R;
    if not isField coefficientRing S then error"input must be a module over an affine ring";
    S0 := ring presentation S;
    r := F*map(S,S0); 
    MM := pushForward(r,M);
    numgens S0 - pdim MM)

profondeur Ring := R -> profondeur R^1


--MCM approximation 
approximatione = method(Options =>{CoDepth => -1})
approximatione(ZZ,Module) := opts -> (n,M) ->(
    --returns the map to M from the
    --dual of the n-th syz of the n-th syzy of Mp = prune M
    --if n' were 1 or 2 then, without introducing Mp 
    --the source M' of the map returned might not be homogeneous.
    if n == 0 then n = 1;
    Mp := prune M;
    p := Mp.cache.pruningMap; -- the iso Mp -->M
    if isFreeModule Mp then return p;
    F := res(Mp, LengthLimit =>n);
    if F.dd_n == 0 then return map(M,(ring M)^0,0); -- in this case the n-th syz is 0
    G := res(coker transpose F.dd_n, LengthLimit =>n);
    F' := chainComplex reverse apply(n, j-> transpose F.dd_(j+1));
    phi := extend(G, F', id_(G_0));
    M' := coker transpose G.dd_n;
    map(M, M',(matrix p)*transpose phi_n)
    )

approximatione Module := opts -> M ->(
    --returns the map from the essential MCM approximation
    n := max(3,1+dim ring M); -- if n were 1 or 2 we might get nonminimal presentations
    if opts.CoDepth == 0 then n = 1;
    if opts.CoDepth > 0 then n = opts.CoDepth;
    approximatione(n, M)-- correctly gives 0 if M has finite pd (necessarily < 1+dim ring M)
    )

coSyzygyChain = method()
coSyzygyChain(ZZ, Module) := (n,M) ->(
    --assuming M is MCM, the script returns
    --produces dual G of the  resolution of the dual of the second syzygy of M for n+1 steps,
    --adjusted so that M == image G.dd_0. Thus the map G.dd_(-1) is the universal map
    --of M into a free module, etc.
    F := res(M,LengthLimit => 1);
    G := res(coker dual F.dd_1, LengthLimit => n+1);
    H := (dual G) [-1];
    H)
    
coApproximation = method(Options =>{Total => true, CoDepth=>-1})
coApproximation Module := opts -> M -> (
    p := presentation M;
    F0 := target p;
    (phi,psi) := approximation(M, Total => opts.Total,CoDepth => opts.CoDepth);
    M' := source (phi|psi);--the total MCM approximation.
    if isFreeModule M' then return map(M,M,1);
    q := matrix (phi | psi);
    r := p//q;
    r0 := id_F0//q;
    k := syz q;
    K := source k;
    G := coSyzygyChain(1, M');
    sour := F0 ++ K;
    tar := G_(-1);
    N := coker(G.dd_0*(r | k));
    map(N,M, G.dd_0*r0)
    )
coApproximationSequence = M -> (
    S:=ring M;
    alpha := coApproximation M;
    N := coker alpha;
    beta := inducedMap(N,target alpha);
    chainComplex {map(S^0,N,0),beta,alpha,map(source alpha,S^0,0)})

approximation = method(Options =>{CoDepth=>-1, Total =>true})
approximation Module := opts -> M->(
    --returns the list {phi, psi} where:
    --phi is the map from the essential MCM approximation
    --psi is the minimal map from a free module necessary to make
    -- alpha = (phi | psi) an epimorphism
    phi := approximatione(M,CoDepth=>opts.CoDepth);
    if opts.Total != true then return phi;
    psi := null;
    N := coker phi;
    N1 := prune N;
    if N1 == 0 then (
	psi = map(M,(ring M)^0, 0);
        return (phi, psi));
    MtoN := map(N,M, id_(cover M));
    a := N1.cache.pruningMap;
    psi1 := (matrix a)//matrix(MtoN);
--the following line added 170615
    psi = map(M, source psi1, psi1);
    (phi, psi)
    )

approximationSequence = M->(
    (alpha,beta) := approximation M;
    S := ring M;
    tot := (alpha|beta);
    N := kernel tot;
    gamma := inducedMap(source tot,N);
    chainComplex {map(S^0,M,0), tot,gamma,map(N,S^0,0)}
    )

auslanderInvariant = method(Options =>{CoDepth => -1})
auslanderInvariant Module := opts->M-> (
    --number of free summands in the MCM approximation
    if isFreeModule M then return numgens M;
    phi := approximation(M, CoDepth => opts.CoDepth, Total=>false);
    numgens prune coker phi)

setupRings = method(Options =>{Characteristic => 101, Randomize =>true})

setupRings(ZZ,ZZ) := opts -> (c,d)->(
    x := local x;
    p := opts.Characteristic;
    S := ZZ/p[x_0..x_(c-1)];
    ff := matrix{apply(c, i->S_i^d)};
    if opts.Randomize===true then ff = ff*random(source ff, source ff);
    {S}|apply(c, j->(S/ideal(ff_{0..j})))
    )

setupRings(Matrix) := opts -> (ff)->(
     S := ring ff;
     c := numcols ff;
     if opts.Randomize===true then ff = ff*random(source ff, source ff);
     {S}|apply(c, j->(S/ideal(ff_{0..j})))
     )


setupModules = method()
setupModules(List,Module) := (R,M)->(
    --R_i is a ci of codim i in a ring S
    --returns (MM,kk,p) where
    --MM,kk are lists whose i-components are the module M and residue field k, but over R_i
    --p_i_j is the projection from R_j to R_i (c >= i >= j >= 0)
    --M is a a module over R_c.
    c := length R-1;
    kk :=apply(c+1, i-> coker vars R_i);
    p := apply(c+1, i->apply(i+1, j->map(R_i,R_j)));
    MM := apply(c+1, j->prune pushForward(p_c_j, M));
    (MM,kk,p))


-----DOCUMENTATION---Documentation---documentation
beginDocumentation()

doc ///
Key
  MCMApproximations
Headline
  Maximal Cohen-Macaulay Approximations and Complete Intersections
Description
  Text
   Maximal Cohen-Macaulay approximations were introduced by Auslander and Buchweitz
   [The homological theory of maximal Cohen-Macaulay (MCM) approximations, 
   Colloque en l'honneur de Pierre Samuel (Orsay, 1987)
   Soc. Math. France (N.S.)} No. {\bf 38}, (1989), 5 - 37.] 
   In the context of a local Gorenstein ring R, the theory simplifies a little
   and can be expressed as follows. Let M be an R-module. 
   
   1) There is a unique
   maximal Cohen-Macaulay R-module M' and a short exact "approximation sequence"
   0\to N' \to M' \to  M \to 0
   such that N has finite projective dimension; 
   the module M, together with the surjection,
   is the MCM approximation of M.
   
   2) Dually, there is a unique short exact "co-approximation sequence"
   0\to M \to N'' \to M'' \to 0
   such that N'' has finite projective dimension and M'' is a maximal Cohen-Macaulay module,
   the MCM co-approximation.
   
   These sequences are easy to compute. Let
   d = 1+ depth R - depth M. Write M'_0 for the d-th cosyzygy of the 
   d-th syzygy module of M, and \alpha: M'\to M the induced map. The module M'
   (or the map (M'\to M) is called the 
   essential MCM approximation of M. Since d >= 2, the module M' has no free summand.
   Let B_0 be a minimal free module mapping
   onto M/(image M'_0), and lift the surjection to a map
   \beta: B_0 \to M. The map
   (\alpha, \beta): M'_0 \oplus B_0 --> M
   is the MCM approximation, and N is its kernel. 
   
   The routine
   approximation M 
   returns the pair (\alpha, \beta).
   
   Further, if M'' is the (d+1)st cosyzygy of the d-th syzygy of M
   then there is a short exact sequence
   0\to M' \to F \to M'' \to 0
   with F free. Pushing this sequence forward along the map \alpha: M' \to M
   gives the coapproximation sequence
   0\to M \to N''\to M'' \to 0.
   
   The routine coApproximation M returns the map M --> N''.
   Here is an example of a simple approximation sequence, 
   exhibited by the betti tables of its 3 middle terms:
   
   The Betti table of the module M is at the top, and one sees that it is NOT MCM (the resolution is not periodic
   at the beginning) and not of finite projective dimension (the length of the given part of
   of the -- actually infinite -- resolution is already longer than the dimension of the ring.
   
   Next comes the betti table of the  MCM module that approximates M (we see that its resolution is
   periodic from the beginning). 

   Finally we see a module of finite projective dimension (in this case 1).
  Example
   S = ZZ/101[a,b,c]
   R = S/ideal(a^3+b^3+c^3)
   M = coker random(R^2, R^{4:-1});
   Ea = approximationSequence M;
   netList apply({1,2,3}, i-> betti res Ea_i)
  Text
   Here is a similar display for the co-approximation sequence. Here
   the Betti table of M is at the bottom, the Betti table of the module of finite projective dimension
   is in the middle, and that of the MCM module is at the top (
  Example
   Ec = coApproximationSequence M;
   netList apply(5, i-> betti res prune Ec_i)   
///


///
restart
loadPackage("MCMApproximations", Reload=>true)
///

doc ///
   Key
    socleDegrees
    (socleDegrees, Module)
   Headline
    lists the degrees of the socle generators
   Usage
    L = socleDegrees M
   Inputs
    M:Module
   Outputs
    L:List
   Description
    Text
     L is the list of socle degrees of M, with multiplicities. Thus
     L = {} if the socle is 0.
    Example
     R = ZZ/101[x,y,z]
     M0 = R^1/(ideal(x,y,z)*ideal (x,y));
     M1 = coker random(R^{1,2}, R^{0,-1,-2}); -- dim 1
     M2 = coker random(R^{1,2}, R^{0,-1,-2,-4}); -- dim 0
   ///

doc ///
   Key
    CoDepth
   Headline
    Option for syzygyModule(-k,M,CoDepth => m)
   Description
    Text
     Allows the user to specify a number m (which must be at least CoDepth M),
     for more efficient computation.
   Caveat
    Does not check that the CoDepth value is correct.
   SeeAlso
    syzygyModule
///

doc ///
   Key
    syzygyModule
    (syzygyModule, ZZ, Module)
    [syzygyModule, CoDepth]
   Headline
    Produces the k-th syzygy module (k \in ZZ)
   Usage
    N = syzygyModule(k,M)
   Inputs
    k:ZZ
     which syzygy
    M:Module
   Outputs
    N:Module
   Description
    Text
     If k==0 then the N=M. If k>0 then the syzygy module is computed from the 
     resolution. If k<0 then the program returns the dual of the (n-k)-th syzygy
     of the dual of the k-th syzygy, where n is one more than Codepth if that
     option is specified, and else n is the number of variables of ring M. 
     Of course the resulting N is 0 if ring M is regular, and otherwise correct
     only if ring M is Gorenstein. In the Gorenstein case, syzygyModule(-k, syzygyModule(k, M))
     -is the non-free part of the source of the MCM approximation of M.
    Example
     R = setupRings(4,3);
     M = coker vars R_2;
     betti res M
     betti syzygyModule(2,M)
     betti (N2 = syzygyModule(-2,M))
     betti res N2
     betti syzygyModule(-2,M,CoDepth=>2)
   Caveat
    ring M must be Gorenstein, and the program does not check
   SeeAlso
    setupRings
///
doc ///
   Key
    profondeur
    (profondeur,Ideal,Module)
    (profondeur, Module)
    (profondeur, Ring)
   Headline
    computes the profondeur with respect to an ideal
   Usage
    m = profondeur (I,M)
   Inputs
    I:Ideal
    M:Module
    R:Ring
   Outputs
    m:ZZ
   Description
    Text
     When the ideal I is not specified, the maximal ideal is used, and the 
     computation is done using the Auslander-Buchsbaum formula.
///

doc ///
   Key
    coApproximation
    (coApproximation, Module)
    [coApproximation, CoDepth]
    [coApproximation, Total]
   Headline
    Maximal Cohen-Macaulay co-approximation of M
   Usage
    a = coApproximation M
   Inputs
    M:Module
   Outputs
    a:Matrix
   Description
    Text
     If R is a Gorenstein ring, and M is a finitely generated R-module, then, according
     to the theory of Auslander and Buchweitz (a good exposition is in Ding's Thesis) 
     there are unique exact sequences
     $$0\to K \to M' \to M\to 0$$
     and 
     $$0\to M \to N\to M''\to 0$$
     such that K and N are of finite projective dimension, M' and M'' are 
     maximal Cohen-Macaulay, and 
     M'' has no free summands. 
     The call
     
      approximation M  
      
      returns the map $M'\to M$, while the call
      
      coApproximation M
      
      returns the map $M\to N$.
      
      Since the script coApproximation begins by computing the approximation, it may
      shorten the computation if the user knows the depth of M in advance, specified
      with the option Depth => d.
    Example
     setRandomSeed 100
     c = 3;d=3;
     S = setupRings(c,d);
     R = S_c; -- complete intersection, codim = c
     R' = S_(c-1); --codim c-1
     Mc = coker vars R;
     (M,k,p) = setupModules(S,Mc); --M_(c-1) is Mc as an R_(c-1)-module
     ca = coApproximation M_(c-1); 
     M'' = coker ca;
     N = target ca
     profondeur M'' == dim ring M'' -- an MCM module
     M'' == source approximation(M'', Total=>false) -- no free summands
     2 == length res(N, LengthLimit =>10) -- projective dimension <\infty
   SeeAlso
    setupRings
    setupModules
    profondeur
    approximation
    syzygyModule
///
doc ///
   Key
    Total
   Headline
    option for approximation
   Usage
    approximation(M, Total =>t)
   Inputs
    M:Module
    t:Boolean
   Description
    Text
     If t != true then return only the map from the non-free part of the MCM approximation
     Otherwise, return the pair of maps that defines the MCM approximation.
     Default is t ==true.
   SeeAlso
    approximation
    auslanderInvariant
    CoDepth
///


-*
doc ///
   Key
    approx
   Headline
    synonym for approximation
   SeeAlso
    approximation
///
*-
doc ///
   Key
    approximation
    (approximation, Module)
    [approximation, Total]
    [approximation, CoDepth]
   Headline
    returns pair of components of the map from the MCM approximation
   Usage
    (phi,psi) = approximation M
   Inputs
    M:Module
   Outputs
    phi:Matrix
     map from the nonfree component
    psi:Matrix
     map from the free component
   Description
    Text
     If R is a local or standard graded
     Gorenstein ring, and M is a finitely generated R-module, then, according
     to the theory of Auslander and Buchweitz (a good exposition is in Ding's Thesis) 
     there are unique exact sequences
     $$0\to K \to M' \to M\to 0$$
     and 
     $$0\to M \to N\to M''\to 0$$
     such that K and N are of finite projective dimension, M' and M'' are 
     maximal Cohen-Macaulay, and 
     M'' has no free summands. Thus, for example, the projective
     dimension of K is one less than the CoDepth of M.)
 
     The call
     
      coApproximation M  
      
      returns the map $M\to N$, while the call
      
      approximation M
      
      returns the pair (phi,psi), which define the map $M'\to M$.
      Here phi is the "essential MCM approximation" from the biggest summand M'0 of 
      M' that has no free summands, and psi is the map from the free summand M'1.
     
     The module M'0 is computed as syzygyModule(-k, syzygyModule(k,M)) for any k >= CoDepth M,
     and the map $M'0 \to M$ is induced by the comparison map of resolutions. 
     
     The rank t of the free summand M'1 is called the Auslander Invariant of M,
     and is returned by the call auslanderInvariant M.
     
     The CoDepth of M can be provided as an option to speed computation.
     
     If Total => false, then just the map phi is returned.
    Example
     R = ZZ/101[a,b]/ideal(a^2)
     k = coker vars R
     approximation k
     M = image vars R
     approximation M
     approximation(M, Total=>false)
     approximation(M, CoDepth => 0)
   SeeAlso
    syzygyModule
    auslanderInvariant
///

doc ///
   Key
    approximationSequence
   Headline
    Short exact sequence of the MCM approximation
   Usage
    E = approximationSequence M
   Inputs
    M:Module
   Outputs
    E:ChainComplex
   Description
    Text
     The approximation sequence of a module M over a Gorenstein ring
     is the versal short exact sequence
     $$0\to P \to M' \to M \to 0$$
     where M' is a maximal Cohen-Macaulay module and P is a module of finite projective
     dimension, as defined by Auslander and Buchweitz.
    Example
     S = ZZ/101[a,b]/ideal(a^3+b^3)
     R = S/ideal(a*b)
     M = R^1/(ideal vars R)^2
     approximationSequence M
   SeeAlso
    coApproximationSequence
///
doc ///
   Key
    coApproximationSequence
   Headline
    Short exact sequence of the MCM coapproximation
   Usage
    E = coApproximationSequence M
   Inputs
    M:Module
   Outputs
    E:ChainComplex
   Description
    Text
     The  coapproximation sequence of a module M over a Gorenstein ring
     is the versal short exact sequence
     $$0\to M \to P \to M' \to 0$$
     where M' is a maximal Cohen-Macaulay module and P is a module of finite projective
     dimension, as defined by Auslander and Buchweitz.
    Example
     S = ZZ/101[a,b]/ideal(a^3+b^3)
     R = S/ideal(a*b)
     M = R^1/(ideal vars R)^2
     coApproximationSequence M
   SeeAlso
    approximationSequence
///



doc ///
   Key
    auslanderInvariant
    (auslanderInvariant, Module)
    [auslanderInvariant, CoDepth]
   Headline
    measures failure of surjectivity of the essential MCM approximation
   Usage
    a = auslanderInvariant M
   Inputs
    M:Module
   Outputs
    a:ZZ
   Description
    Text
     If R is a Gorenstein local ring and M is an R-module, then
     the essential MCM approximation is a map phi: M'-->M, where 
     M' is an MCM R-module, obtained as the k-th cosyzygy of the k-th syzygy of M,
     where k >= the co-depth of M. The Auslander invariant is the number of 
     generators of coker phi. Thus if R is regular the Auslander invariant is
     just the minimal number of generators of M, and if M is already an MCM module
     with no free summands then the Auslander invariant is 0.
     
     Ding showed that if R is a hypersurface ring, then
     auslanderInvariant (R^1)/((ideal vars R)^i) is zero precisely for i<multiplicity R.
     
     Experimentally, it looks as if for a complete intersection the power is the 
     a-invariant plus 1, but NOT for the codim 3 Pfaffian example.
    Example
     R = ZZ/101[a..d]/ideal"a3"
     apply(5, i -> auslanderInvariant ((R^1)/(ideal(vars R))^(i+1)))
     R = ZZ/101[a..d]/ideal"a3,b4"
     apply(6, i -> auslanderInvariant ((R^1)/(ideal(vars R))^(i+1)))
     S = ZZ/101[a,b,c]
     N = matrix{{0,a,0,0,c},
	     	{0,0,b,c,0},
		{0,0,0,a,0},
		{0,0,0,0,b},
		{0,0,0,0,0}}
     M = N-transpose N
     J = pfaffians(4,M)
     R = S/J
     I = ideal vars R
     scan(5, i->print auslanderInvariant ((R^1)/(I^i)))
   SeeAlso
    approximation
///
doc ///
   Key
    Characteristic
   Headline
    Option for setupRings(c,d,Characteristic=>q)
   Description
    Text
     Allows the user to specify the characteristic of the rings to be defined.
   SeeAlso
    setupRings
    Randomize
    setupModules
///
doc ///
   Key
    Randomize
   Headline
    Option for setupRings(c,d,Characteristic=>q, Randomize=>false)
   Description
    Text
     Defaults to true. When = true, replaces the regular sequence of
     d-th powers with a regular sequence of random linear combinations.
   SeeAlso
    setupRings
    Characteristic
    setupModules
///

doc ///
   Key
    setupRings
    (setupRings, ZZ, ZZ)
    (setupRings, Matrix)
    [setupRings, Characteristic]
    [setupRings, Randomize]
   Headline
    Sets up a complete intersection for experiments
   Usage
    R = setupRings(c,d)
    R = setupRings(ff)    
   Inputs
    c:ZZ
     desired codimension
    d:ZZ
     degree of homogeneous generators
    ff:Matrix
     a regular sequence
   Outputs
    R:List
     List of rings R_0..R_c with R_i = S/(f_0..f_(i-1))
   Description
    Text
     Makes a complete intersection f_0..f_{c-1} = x_0^d..x_{c-1}^d
     or, when Random=>true (the default), random linear combinations of these,
     in the polynomial ring ZZ/p[x_0..x_{c-1}], where p can be set by the optional 
     argument Characteristic=>p. By default, p = 101.
    Example
     netList setupRings(2,2)
     netList setupRings(2,2,Characteristic=>5)
   SeeAlso
    setupModules
///
    --R_i is a ci of codim i in a ring S
    --returns (MM,kk,p) where
    --MM,kk are lists whose i-components are the module M and residue field k, but over R_i
    --p_i_j is the projection from R_j to R_i (c >= i >= j >= 0)

doc ///
   Key
    setupModules
    (setupModules, List, Module)
   Headline
    Creates a list of modules and maps over complete intersection for experiments
   Usage
    (MM, kk, p) = setupModules(R,M)
   Inputs
    R:List
     of complete intersections R_i = S/(f_0..f_(i-1))
    M:Module
     over the ring R_{c-1} where c = length R.
   Outputs
    MM:List
     of c+1 modules M_i over R_i
    kk:List
     of residue class modules k_i of R_i
    p:List
     of maps, p_i_j: R_j to R_i the projection
   Description
    Text
     This is useful for setting up an experiment. For example, we conjecture
     that the regularity of Ext_{R_i}(M_i,k_i) is a non-decreasing function of i.
     Here ring M = R_{c-1} and  M_i = pushForward(p_{(c-1)}_i, M).
    Example
     needsPackage "CompleteIntersectionResolutions" -- for "evenExtModule"
     R =setupRings(3,2);--codims 0..3, degrees = 2
     MM0 = coker random(R_3^2, R_3^{3: -1});
     (M,kkk,p) = setupModules(R,MM0);
     apply(3, j->regularity evenExtModule M_(j+1))
   SeeAlso
    setupRings
///

-----TESTS
TEST///
   setRandomSeed 0
   T = setupRings(3,3)
   R = T_3
   M = coker random(R^2, R^{3: -2});
   (MM,kk,p) = setupModules(T, M)
   (a,b) = approximation MM_1 -- MM_1 is M as a module over the ring of codim 1
   M' = source a
   assert(length res pushForward(p_1_0,M') == 1) -- an MCM module
   assert isFreeModule source b -- free module
///


TEST///
     setRandomSeed 100;
     R = setupRings(2,2);
     M = syzygyModule_2 coker vars R_2;
     N = syzygyModule_2 syzygyModule(-2,M);
     assert(betti M == betti N)
     N = prune syzygyModule(-2,syzygyModule(2,M),CoDepth =>0);
     assert(betti M == betti N)

     R = setupRings(2,2, Characteristic=>5, Randomize=>false);
     M = syzygyModule_2 coker vars R_2;
     N = syzygyModule_2 syzygyModule(-2,M);
     assert(betti M == betti N)
     N = prune syzygyModule(-2,syzygyModule(2,M),CoDepth =>0);
     assert(betti M == betti N)
///
TEST///
setRandomSeed()
R = ZZ/101[a,b,c,d,e]/(ideal(a,b)*ideal(c,d))
assert(profondeur R == 2)    
assert(profondeur(ideal(a,d,e), R^1) == 2)
assert(profondeur R^1 == 2)
/// 
TEST///setRandomSeed 100
c = 3;d=3;
S = setupRings(c,d)
R = S_c
Mc = coker vars R
(M,k,p) = setupModules(S,Mc)
M_(c-1)
ca = coApproximation M_(c-1)
M'' = coker ca
N = target ca
assert(profondeur M'' == dim ring M'') -- an MCM module
assert(betti res prune M'' == betti res source approximation(prune M'', Total=>false)) -- no free summands
assert(2 == length res(N, LengthLimit =>10)) -- projective dimension <\infty
///
///TEST
setRandomSeed 100
assert( (approximation M) === (map(image map((R)^1,(R)^{{-1},{-1}},{{a, b}}),cokernel map((R)^{{-1},{-1}},(R)^{{-2},{-2}},{{-a, b}, {0, a}}),{{-1, 0}, {0, 1}}),map(image map((R)^1,(R)^{{-1},{-1}},{{a, b}}),(R)^0,0)) );
assert( (approximation(M, Total=>false)) === map(image map((R)^1,(R)^{{-1},{-1}},{{a,b}}),cokernel map((R)^{{-1},{-1}},(R)^{{-2},{-2}},{{-a, b}, {0, a}}),{{-1, 0}, {0, 1}}) );
assert( (approximation(M, CoDepth => 0)) === (map(image map((R)^1,(R)^{{-1},{-1}},{{a,b}}),cokernel map((R)^{{-1},{-1}},(R)^{{-2},{-2}},{{a, -b}, {0, a}}),{{1, 0}, {0,1}}),map(image map((R)^1,(R)^{{-1},{-1}},{{a, b}}),(R)^0,0)) );
///
TEST///
setRandomSeed 100
c=3;d=2;
R = setupRings(c,d);
(M,k,p) = setupModules(R,coker vars R_c);
assert(numcols  matrix p_c_c === 3 )
///
TEST///
kk = ZZ/101
R = kk[x,y,z]
assert(3==profondeur R)
assert (2 == profondeur(ideal(x,y), R^1))
assert(0 == profondeur coker vars R)
assert (0 == profondeur(ideal(x,y), coker vars R))
R = ZZ/101[a..f]
I = minors(2,genericSymmetricMatrix(R,a,3))
assert (profondeur(R/I) ==3)
assert(profondeur(R/I^2) == 0)
mm = ideal vars (R/I)
assert(profondeur(mm, (R/I)^1)== 3)
///
TEST///
S = ZZ/101[a,b,c]
R = S/ideal"a3,b3,c3"
use S
R' = S/ideal"a3,b3"
M = coker vars R
assert( (pushForward(map(R,R'),M)) === cokernel map((R')^1,(R')^{{-1},{-1},{-1}},{{c, b, a}}) );
use S
assert( (pushForward(map(R,S), M)) === cokernel map((S)^1,(S)^{{-1},{-1},{-1}},{{c, b, a}}) );
///
TEST///
setRandomSeed()
c = 3
R = setupRings(c,3)
M = syzygyModule(1,coker vars R_c)
(MM,kk,p) = setupModules(R,M);
auslanderInvariant syzygyModule_2 MM_1
assert (1 ==auslanderInvariant syzygyModule_2 MM_1)
(0 ==auslanderInvariant kk_2)
assert(p_1_0 === map(R_1,R_0))
///

TEST///
setRandomSeed()
S = ZZ/101[a,b,c]
R = S/ideal"a3,b3,c3"
use S
R' = S/ideal"a3,b3"
M = coker vars R
(phi,psi) = approximation(pushForward(map(R,R'),ker syz presentation M))

assert(presentation source phi == map(R'^{6:-4,-3},,matrix {{0, 0, -b^2, 0, 0, 0, -c, 0, a}, {0, a^2, 0, -c, 0, 0, 0, 0, -b}, {0, 0, a^2, 0, -c, 0, 0, -b, 0}, 
	{0, 0, 0, a, 0, 0, b, 0, 0}, {0, 0, 0, 0, -a, b, 0, 0, 0}, {-b^2, 0, 0, 0, 0, c, 0, a, 0}, {0, 0, 0, 0, b^2, 0, a^2, 0, 0}}
))
assert( (prune source psi) === (R')^{{-4},{-4},{-4}} )
assert(isSurjective(phi|psi)===true)
assert( (prune ker (phi|psi)) === (R')^{{-5},{-5},{-5},{-6},{-6},{-6}} );
///

TEST///
needsPackage "CompleteIntersectionResolutions"
S = ZZ/101[a,b,c]
ff = matrix"a3, b3,c3" 
len = 5
cod = numcols ff
I = ideal ff
R = S/I
q = map(R,S)
M0= coker random(R^2, R^{4:-1});
M = pushForward(q,syzygyModule(4,M0));
L = (layeredResolution(ff,M))_0;
assert(betti L == betti res M)
///

TEST///
S = ZZ/101[a,b,c]/ideal(a^3)
M = module(ideal(a,b,c));
Ea = approximationSequence M;
Ec = coApproximationSequence M;
assert(isFreeModule prune Ea_3 ===true)
assert(length res prune Ec_2 == 1)
///

end--
restart
loadPackage("MCMApproximations", Reload=>true)

uninstallPackage"MCMApproximations"
restart
installPackage"MCMApproximations"
check "MCMApproximations"
viewHelp MCMApproximations


uninstallPackage "CompleteIntersectionResolutions"
restart
installPackage "CompleteIntersectionResolutions"
check "CompleteIntersectionResolutions"

approximation(MR')
