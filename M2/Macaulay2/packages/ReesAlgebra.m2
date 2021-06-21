-------------------------------------------------------------------------
-- PURPOSE : Compute the rees algebra of a module as it is defined in the 
--           paper "What is the Rees algebra of a module?" by Craig Huneke, 
--           David Eisenbud and Bernde Ulrich.
--           Also to compute many of the structures that require a Rees 
--           algebra, including 
-- analyticSpread
-- specialFiber
-- idealIntegralClosure
-- distinguished -- distinguished subvarieties of  a variety 
--                  (components of the support of the normal cone)
-- PROGRAMMERs : Rees algebra code written by David Eisenbud,
--               Amelia Taylor, Sorin Popescu, and students (see the JSAG description)
-- UPDATE HISTORY : created 27 October 2006 
-- 	     	    updated 29 June 2008
--                  updated 19-21 July 2017 (Berkeley M2 Workgroup)
--                  updated November 2017
--
---------------------------------------------------------------------------
newPackage(
	"ReesAlgebra",
    	Version => "2.3", 
    	Date => "November 2019",
    	Authors => {{
		  Name => "David Eisenbud",
		  Email => "de@msri.org"},
	     {Name => "Amelia Taylor",
   	     Email => "originalbrickhouse@gmail.com"},
             {Name => "Sorin Popescu",
	      Email => "sorin@math.sunysb.edu"},
	     {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu"}},  
    	DebuggingMode => false,
    	Headline => "Rees algebras",
	Keywords => {"Commutative Algebra"},
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "The ReesAlgebra package in Macaulay2",
	     "acceptance date" => "21 May 2018",
	     "published article URI" => "https://msp.org/jsag/2018/8-1/p05.xhtml",
	     "published article DOI" => "10.2140/jsag.2018.8.49",
	     "published code URI" => "https://msp.org/jsag/2018/8-1/jsag-v8-n1-x05-ReesAlgebra.m2",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/ReesAlgebra.m2",
	     "release at publication" => "0ccfca1d3d08d13ed0da78435b2106209fcee1b1",	    -- git commit number in hex
	     "version at publication" => "2.2",
	     "volume number" => "8",
	     "volume URI" => "https://msp.org/jsag/2018/8-1/"
	     }
	)
-*
restart
uninstallPackage "ReesAlgebra"
restart
installPackage  "ReesAlgebra"
viewHelp ReesAlgebra
check "ReesAlgebra"
*-

export{
  "analyticSpread", 
  "distinguished",
  "intersectInP",
  "isLinearType", 
  "minimalReduction",
  "isReduction",
  "multiplicity",
  "normalCone", 
  "reductionNumber",
  "reesIdeal",
  "reesAlgebra",
  "specialFiberIdeal",
  "specialFiber",
  "symmetricKernel", 
  "versalEmbedding",
  "whichGm",
  "Tries",
  "jacobianDual",
  "symmetricAlgebraIdeal",
  "expectedReesIdeal",
  "PlaneCurveSingularities",
  --synonyms
  "associatedGradedRing" => "normalCone",
  "reesAlgebraIdeal" => "reesIdeal",
  "Trim" -- option in reesIdeal
  }


symmetricAlgebraIdeal = method(Options =>
    {             VariableBaseName => "w"
     })
symmetricAlgebraIdeal Module := Ideal => o -> M -> (
    ideal presentation symmetricAlgebra(M, o))
symmetricAlgebraIdeal Ideal := Ideal => o -> M -> (
    ideal presentation symmetricAlgebra(module M, o))


    
symmetricKernel = method(Options=>{Variable => "w"})
symmetricKernel(Matrix) := Ideal => o -> (f) -> (
     if rank source f == 0 then return trim ideal(0_(ring f));
     w := o.Variable;
     if instance(w,String) then w = getSymbol w;
     S := symmetricAlgebra(source f, VariableBaseName => w);
     T := symmetricAlgebra target f;
     trim ker symmetricAlgebra(T,S,f))

versalEmbedding = method()
versalEmbedding(Ideal) :=
versalEmbedding(Module) := Matrix => (M) -> (
     if (class M) === Ideal then M = module M;
     UE := transpose syz transpose presentation M;
     map(target UE, M, UE)
     )

fixupw = w -> if instance(w,String) then getSymbol w else w

reesIdeal = method(
    Options => {
	  Jacobian =>false,
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  Variable => "w",
	  Trim => true
	  }
      )

--the following uses a versal embedding
reesIdeal(Module) := Ideal => o -> M -> (
     if o.Trim == true then P := presentation minimalPresentation M else P = presentation M;
     UE := transpose syz transpose P;
     symmetricKernel(UE,Variable => fixupw o.Variable)
     )

--in the case of ideals we don't need a versal embedding; any embedding in the ring will do.
reesIdeal(Ideal) := Ideal => o-> (J) -> (
    if o.Trim == true then J' := mingens J else J' = gens J;
     symmetricKernel(J', Variable => fixupw o.Variable)
     )

-- the following method, usually faster,
-- needs a user-provided non-zerodivisor a such that M[a^{-1}] is of linear type.

reesIdeal(Module,RingElement) := Ideal => o-> (I,I0) ->(
    if o.Trim == true then I' := trim I else I' = I;
    K' := if o.Jacobian == true then expectedReesIdeal I' else(
    K' = symmetricAlgebraIdeal I';
    R := ring K';
    IR := substitute(I0, R);
    trim saturate(K',IR)
    )
    )

reesIdeal(Ideal, RingElement) := Ideal => o -> (I,a) -> (
     if o.Trim == true then I' := trim I else I' = I;
     reesIdeal(module I', a, Trim => o.Trim)
     )

reesAlgebra = method (TypicalValue=>Ring,
            Options => {Jacobian => false,
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  Variable => "w"
	  }
    )
-- accepts a Module, Ideal, or pair (depending on the method) and
-- returns the quotient ring isomorphic to the Rees Algebra rather
-- than just the defining ideal as in reesIdeal. 

reesAlgebra Ideal :=
reesAlgebra Module := o-> M ->  quotient reesIdeal(M, o)

reesAlgebra(Ideal, RingElement) :=
reesAlgebra(Module, RingElement) := o->(M,a)-> quotient reesIdeal(M,a,o)
       
isLinearType=method(TypicalValue =>Boolean,
        Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null--,
	  --Variable => "w"
	  }
)

isLinearType(Ideal):=
isLinearType(Module):= o-> N->(
     if class N === Ideal then N = module N;
     M := prune N;
     I := reesIdeal (M,o);
     S := ring I;
     P := promote(presentation M, S);
     J := ideal((vars S) * P);
     ((gens I) % J) == 0)

isLinearType(Ideal, RingElement):=
isLinearType(Module, RingElement):= o-> (N,a)->(
     if class N === Ideal then N = module N;
     M := prune N;
     I := reesIdeal(M,a,o);
     S := ring I;
     P := promote(presentation M, S);
     J := ideal((vars S) * P);
     ((gens I) % J) == 0)

normalCone = method(TypicalValue => Ring, 
    	    Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  Variable => "w"
	  }
)
normalCone(Ideal) := o -> I -> (
     RI := reesAlgebra(I,o);
     RI/promote(I,RI)
     )

normalCone(Ideal, RingElement) := o -> (I,a) -> (
     RI := reesAlgebra(I,a,o);
     RI/promote(I,RI)     
     )

multiplicity = method(
    	    Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  Variable => "w"
	  }
)

multiplicity(Ideal) := ZZ => o -> I ->  (
     RI := normalCone (I,o);
     J := ideal RI;
     J1 := first flattenRing J;
     S1 := newRing(ring J1, Degrees=>{numgens ring J1 : 1});
     degree substitute(J1,S1)
     )

multiplicity(Ideal,RingElement) := ZZ => o -> (I,a) ->  (
     RI := normalCone (I,a,o);
     J := ideal RI;
     J1 := first flattenRing J;
     S1 := newRing(ring J1, Degrees=>{numgens ring J1 : 1});
     degree substitute(J1,S1)
     )


isEquigenerated = A -> (
    if isHomogeneous A and 
    all(A_*, a->degree a == degree(A_*_0)) then true else false)


specialFiberIdeal=method(TypicalValue=>Ideal, 
        Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  Variable => "w",
	  Jacobian =>false,
	  Trim => true
	  }
      )
specialFiberIdeal(Ideal):= o-> I ->(
if isEquigenerated I then(
    kk := ultimate(coefficientRing, ring I);
    Z := symbol Z;
    ker map(ring I, kk[Z_0..Z_(numgens I -1)], gens I)) else
specialFiberIdeal (module I, o))


specialFiberIdeal(Module):= o->i->(
     Reesi:= reesIdeal(i, o);     
     S := ring Reesi;
     kk := ultimate(coefficientRing, S);
     T := kk(monoid [gens S]);
     minimalpres := map(T,S);
     trim minimalpres Reesi
     )
 
specialFiberIdeal(Ideal, RingElement):= o->(i,i0) ->(
if isEquigenerated i then return(
    kk := ultimate(coefficientRing, ring i);
    w := symbol w;
    ker map(ring i, kk[w_0..w_(numgens i -1)], gens i));
specialFiberIdeal(module i, i0))

specialFiberIdeal(Module,RingElement):= o->(i,a)->(
     Reesi:= reesIdeal(i, o);     
     S := ring Reesi;
     kk := ultimate(coefficientRing, S);
     T := kk[gens S];
     minimalpres := map(T,S);
     trim minimalpres Reesi
     )


--The following returns a ring with just the new vars.
--The order of the generators is supposed to be the same as the order
--of the given generators of I.
specialFiber=method(TypicalValue=>Ring, 
            Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  Variable => "w",
	  Jacobian => false,
	  Trim => true
	  }
      )

specialFiber(Ideal):= 
specialFiber(Module):= o->i->(
     spIdeal := specialFiberIdeal(i,o);
     (ring spIdeal)/spIdeal
     )

specialFiber(Ideal, RingElement):= 
specialFiber(Module, RingElement):= o->(i,a)->(
     spIdeal := specialFiberIdeal(i,a,o);
     (ring spIdeal)/spIdeal
     )

isReduction=method(TypicalValue=>Boolean, 
       Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  Variable => "w"
	  }
    )

--test whether the SECOND arg is a reduction of the FIRST arg
isReduction(Module,Module):= 
isReduction(Ideal,Ideal):= o->(I,J)->(
     if isSubset(J, I) then (
    	     I' := trim I;
	     Sfib:= specialFiber(I', o);
	     Ifib:=ideal presentation Sfib;
	     kk := coefficientRing Sfib;
	     M := sub(gens J // gens I', kk);
	     M = promote(M, Sfib);
	     L :=(vars Sfib)*M;
	     0===dim ideal L)
     else false)

isReduction(Module,Module,RingElement):= 
isReduction(Ideal,Ideal,RingElement):= o->(I,J,a)->(
     if isSubset(J, I) then (
	     Sfib :=specialFiber(I, a, o);
	     Ifib:= ideal presentation Sfib;
	     kk := coefficientRing Sfib;
	     M := sub(gens J // gens I, kk);
	     M = promote(M, Sfib);
	     L :=(vars Sfib)*M; 
	     0===dim ideal L)
     else false)

analyticSpread = method(
           Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null--,
	  --Variable => "w"
	  }
      )

analyticSpread(Ideal) := 
analyticSpread(Module) := ZZ => o->(M) -> dim specialFiberIdeal(M,o)

analyticSpread(Ideal,RingElement) :=
analyticSpread(Module,RingElement) := ZZ => o->(M,a) -> dim specialFiberIdeal(M,a,o)
 

distinguished = method(Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  Variable => "w"
	  }
)

distinguished(RingMap, Ideal) := o -> (f,I) ->(
--f: S -> R,  I\subset S, J\subset R, f(I)\subset J:
    S := source f;
    R := target f;
    NI := normalCone (I,o);
    NJ := normalCone(f I,o);
    K := ker map(NJ,NI,(vars NJ));
    L := decompose K;
    M := apply(L,P->(Pcomponent := K:(saturate(K,P))));
    --the P-primary component. The multiplicity is
    --computed as (degree M_i)/(degree L_i)
    prune NI;
    mp := NI.minimalPresentationMap;
    apply(#L, i -> {(degree mp(M_i))/(degree mp(L_i)),kernel(map(NI/L_i, S/I))})
    )

distinguished(Ideal,Ideal) := o -> (I,J) -> (
    --I,J ideals in the same ring S
    S := ring I;
    f := map(S/J,S);
    distinguished(f,I)
)

distinguished(Ideal) := o -> I -> (
    S := ring I;
    f := map(S,S);
    distinguished(f,I)
)

intersectInP = method(Options=>{
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  Variable => "w"
	  })

intersectInP(Ideal,Ideal) := o->(I,J) ->(
    --I,J in a polynomial ring; intersection done with the diagonal, then pulled back
    P := ring I;
    kk := coefficientRing P;
    n := numgens P;
    if P =!=ring J then error"requires two ideals in the same ring";
    if not isPolynomialRing P and isField kk then 
         error" ring should be a polynomial ring over a field";
    X:=symbol X;
    Y:=symbol Y;
    PP := kk[X_0..X_(n-1),Y_0..Y_(n-1)];
    diag := ideal apply(n, i-> X_i-Y_i);
    toP := map(P,PP/diag,vars P | vars P);
    inX := map(PP,P,apply(n,i->X_i));
    inY := map(PP,P,apply(n,i->Y_i));
    II := inX I + inY J;
    L := distinguished(diag,II);
    apply(L, l-> {l_0, trim toP l_1})
)


rand = method()
rand(Ideal, ZZ, ZZ) := (I,s,d) ->
    --s elements of degree d chosen at random from I
    ideal ((gens I)*random(source gens I, (ring I)^{s:-d}))

rand(Ideal, ZZ) := (I,s) ->(
    --without the third argument d, the function takes
    --random linear combinations of the generators, without
    --regard for the degrees, thus sometimes inhomogeneous.
    kk := ultimate(coefficientRing, ring I);
    choose1 := I -> sum(I_*, i-> random(kk)*i);
    ideal apply(s, i-> choose1 I))

rand(Module, ZZ) := (M,s) ->(
    --random linear combinations of the generators, without
    --regard for the degrees, thus sometimes inhomogeneous.
    kk := ultimate(coefficientRing, ring M);
    choose1 := M -> sum(M_*, i-> random(kk)*i);
    map(M,(ring M)^s, matrix apply(s, i-> choose1 M))
    )

minimalReduction = method(
                   Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null,
	  --Variable => "w",
	  Tries => 20
	  }
      )

minimalReduction Ideal := Ideal => o -> i -> (
     S:=ring i;
     ell := analyticSpread(i,
	  DegreeLimit => o.DegreeLimit,
	  BasisElementLimit => o.BasisElementLimit,
	  PairLimit => o.PairLimit,
	  MinimalGenerators => true,
	  Strategy => o.Strategy
	 ); -- the list is necessary because isReduction doesn't know about "Tries"
     J:=null;
     for b from 1 to o.Tries do(
     J = rand(i, ell);
     if isReduction(i,J,
	  DegreeLimit => o.DegreeLimit,
	  BasisElementLimit => o.BasisElementLimit,
	  PairLimit => o.PairLimit,
	  MinimalGenerators => true,
	  Strategy => o.Strategy
	  )
      then  return J);
     <<o.Tries <<" iterations were not enough to randomly find a minimal reduction"; endl;
     error("not random enough")
          )


reductionNumber = method()
reductionNumber (Ideal,Ideal) := (i,j) -> (
     rN:=0;
     I := (gens i)%j; -- will be a power of i
     if isHomogeneous j then (
     	  while I!=0 do (
    	       j = trim(i*j);
	       I = (gens trim (i*ideal I))%j;
     	       rN =rN+1))
     else(
	  M:= ideal vars ring i; -- we're pretending to be in a local ring
     	  while I!=0 do (
    	       j = trim(i*j+M*ideal I);
	       I = (gens trim (i*ideal I))%j;
     	       rN =rN+1));
     rN)

whichGm = method()
whichGm Ideal := i -> (
     --This *probabilistic* procedure returns the largest number m for which the ideal i satsifies
     --the condition
     --
     --G_m: i_P is generated by <= codim P elements for all P with codim P < m.
     --
     f:=presentation module i;
     S:=ring f;
     if f==0 then "infinity" else(
     q:=rank target f;
     maxSource := (max degrees source f)_0;
     minTarget := (min degrees target f)_0;
     randomMinor := (f,t)->(
	  if t<=0 then ideal(1_S) else
	  if t >min(rank source f, rank target f) then ideal(0_S) else
	   ideal det (random(S^{t:-minTarget},target f)*f*random(source f, S^{t:-maxSource})));
     d:=dim ring i;
     m:=codim i;
     j:=i+randomMinor(f,q-m);
          while m<d+1 and codim j > m do (
	       m=m+1;
	       j=j+randomMinor(f, q-m));
     if m<=d then m else "infinity"))
 
------------------------------------------------------------------
 
jacobianDual = method(Options=>{Variable => "w"})

jacobianDual Matrix := o-> phi ->(
 S := ring phi;
 X := vars S;
 ST := symmetricAlgebra(target phi, VariableBaseName => fixupw o.Variable);
 (vars ST * promote(phi, ST))//promote(X,ST)
          )

jacobianDual(Matrix,Matrix, Matrix) := o -> (phi,X,T) -> (
    --Suppose that T is a 1 x m matrix of variables in the ring ST = R[T_0..T_(m-1)],
    --and phi is a matrix over ST that is defined over the subring R.
    --Suppose also that  X is a 1 x n matrix defined over ST whose
    --entries generate ideal containing the entries of the matrix phi.
    --the routine returns a matrix psi over ST such that 
    --T phi = X psi.
    --Thus psi is a Jacobian dual of phi with respect to X.
    if numcols T != numrows phi then error"if phi has m rows then T must have m cols.";
    psi := (T * phi)//X;
    --check that this worked:
    if not T*phi == X*psi then error"requires 
         ideal flatten entries matrix phi subset ideal flatten entries X";
    psi
    )

expectedReesIdeal = method()

expectedReesIdeal Ideal := I -> expectedReesIdeal module I
expectedReesIdeal Module := Ideal => I -> (
    S := ring I;
    I1 := symmetricAlgebraIdeal I;    
    S1 := ring I1;    
    if numgens I < numgens S then return I1;
    X := promote(vars ring I, S1);
    jImat := jacobianDual (presentation I, X, vars S1);
    I2 := minors(numrows jImat,jImat);
    trim(I1+I2)
    )

beginDocumentation()

///
restart
uninstallPackage "ReesAlgebra"
restart
installPackage  "ReesAlgebra"
viewHelp ReesAlgebra
check "ReesAlgebra"
///


doc ///
  Key
    ReesAlgebra
  Headline
    Compute Rees algebras and their invariants
  Description
    Text
     The Rees Algebra of an ideal is the
     commutative algebra analogue of the blow up in algebraic
     geometry. (In fact, the  ``Rees Algebra''
     is sometimes called the ``blowup algebra''.) 
     A great deal of modern
     commutative algebra is devoted to studying them.
     
     Classically the Rees algebra appeared as the bihomogeneous coordinate
     ring of the blowup of a projective variety along a subvariety or
     subscheme, used for resolution of singularities. 
     Though this is computationally slow on interesting examples,
     we illustrate with some elementary cases of resolution of plane curve
     singularities in @TO PlaneCurveSingularities@.

     The Rees algebra was 
     studied in the commutative algebra context (in the case where M is an ideal of a ring R), 
     by David Rees in
     a famous paper,
     
     {\em On a problem of Zariski}, Illinois J. Math. (1958) 145-149).
 
     In fact, 
     Rees mainly studied the ring 
     $R[It,t^{-1}]$, now also called the `extended Rees
     Algebra' of I. 

     The original goal of this package, first written around 2002,
     was to compute the Rees
     algebra of a module as it is defined in the paper {\em What is the
     Rees algebra of a module?}, by Craig Huneke, David Eisenbud and Bernd
     Ulrich, Proc. Am. Math. Soc. 131, 701-708, 2002.
     It has since expanded to include routines
     for computing many of the invariants of an ideal or module
     defined in terms of Rees algebras.
     
     The Rees algebra, or more precisely the associated graded ring, which
     we compute as a biproduct, plays a central role in modern intersection
     theory: it is the basis of the Fulton-MacPherson definition of the
     intersection product in the Chow ring. We illustrate this in
     @TO distinguished@ and @TO intersectInP@.

     The Rees algebra of a module M is defined 
     by a certain ideal in the symmetric
     algebra $Sym(M)$ of $M$, or, as in this package, 
     by an ideal in the symmetric algebra of any
     free module $F$ that maps onto $M$. 
     When $\phi: M \to G$ is the {\em versal embedding}
     of $M$, then, by the definition of Huneke-Eisenbud-Ulrich,
     the {\em Rees ideal of M} is the kernel of $Sym(\phi)$. Thus the
     Rees Algebra of M is the image of $Sym(\phi)$.
     
     In most cases the kernel of the 
     $Sym(\phi)$ is the same for any embedding $\phi$ of
     $M$ into a free module:
     
     {\bf Theorem (Eisenbud-Huneke-Ulrich, Thms 0.2 and 1.4):} Let R be a Noetherian ring
     and let $M$ be a finitely generated R-module. Let $\phi: M \to G$
     be a versal map of $M$ to a free module. Assume that $\phi$ is an inclusion, and let
     $\psi: M \to G'$ be any inclusion. If $R$ is torsion-free over $\ZZ$
     or $R$ is unmixed and generically Gorenstein, or $M$ is free locally
     at each associated prime of $R$, or $G=R$, then the kernel of $Sym(\phi)$ and the
     kernel of $Sym(\psi)$ are equal.
          
     It follows that in the good cases above the Rees 
     ideal is equal to the saturation
     of the defining ideal of symmetric 
     algebra of $M$ with respect to any
     element f of the ground ring such 
     that $M[f^{-1}]$ is free, or is simply {\em of linear type},
     meaning that $Sym(\phi)$ is a monomorphism. This is the case,
     for example, when M is an ideal and $M[f^{-1}]$ is generated
     by a regular sequence.
     This fact often leads to 
     a faster computation than computing the
     kernel of $Sym(\phi)$ directly.
       
     Here is an example of the pathological case of
     inclusions $\phi: M \to G$ and $\psi: M \to G'$ where $ker(\phi) \neq ker(\psi)$.
     In the following, any finite characteristic would work as well.
    Example
     p = 5;
     R = ZZ/p[x,y,z]/(ideal(x^p,y^p)+(ideal(x,y,z))^(p+1));
     M = module ideal(z);
    Text
     It is easy to check that M \cong R^1/(x,y,z)^p.
     We write iota: M\to R^1 for the embedding as an ideal
     and psi for the embedding M \to R^2 sending z to (x,y).
    Example
     iota = map(R^1,M,matrix{{z}});
     psi = map(R^2,M,matrix{{x},{y}});
    Text
     Finally, a versal embedding is M \to R^3,
     sending z to (x,y,z):
    Example
     phi = versalEmbedding(M);
    Text
     We now compute the kernels of the three maps
     on symmetric algebras:
    Example
     Iiota = symmetricKernel iota;
     Ipsi = symmetricKernel psi;
     Iphi = symmetricKernel phi;
    Text 
     and check that the ones corresponding to phi and iota
     are equal, whereas the ones corresponding to psi and phi
     are not:
    Example
     Iiota == Iphi    
     Ipsi == Iphi
    Text
     In fact, they differ in degree p:
    Example
     numcols basis(p,Iphi) 
     numcols basis(p,Ipsi)
  SeeAlso
   PlaneCurveSingularities
   distinguished
   intersectInP
///

doc ///
   Key
    symmetricAlgebraIdeal
    (symmetricAlgebraIdeal,Ideal)
    (symmetricAlgebraIdeal,Module)
    [symmetricAlgebraIdeal,VariableBaseName]
   Headline
    Ideal of the symmetric algebra of an ideal or module
   Usage
    I = symmetricAlgebraIdeal J
   Inputs
    I:Ideal
    I: Module
   Outputs
    J:Ideal
   Description
    Text
     Uses the built-in  function @TO symmetricAlgebra@. The function returns J an ideal in a
     new ring, with generators corresponding to those of th eideal or module I. The name
     of the new generators may be set, for example to T, with the form
     
     symmetricAlgebraIdeal(J, VariableBaseName =>"T")
   SeeAlso
    reesIdeal
///

-- viewHelp symmetricAlgebra

doc ///
  Key
     symmetricKernel
     (symmetricKernel,Matrix)
  Headline
     Compute the Rees ring of the image of a matrix  
  Usage
     I = symmetricKernel f
  Inputs
    f:Matrix
  Outputs
    :Ideal
      the defining ideal of the image of $Sym(f)$
  Description
   Text
     Given a map between free modules $f: F \to G$ this function computes the
     kernel of the induced map of symmetric algebras, $Sym(f): Sym(F) \to
     Sym(G)$ as an ideal in $Sym(F)$.  When $f$ defines a versal embedding
     of $Im f$ then by the definition
     of Huneke-Eisenbud-Ulrich) this is equal to the defining ideal of the Rees
     algebra of the module Im f, the Rees ideal of M.
     
     When $M$ is an ideal (and in general in characteristic 0) then, by a 
     theorem of Eisenbud-Huneke-Ulrich, 
     any embedding of M into a free module may be used,
     and it follows that the Rees ideal is equal to the saturation
     of the defining ideal of symmetric algebra of M with respect to any
     element f of the ground ring such that M[f^{-1}] is free. And this
     often gives a faster computation.
     
     Most users will prefer to use one of the front 
     end commands @TO reesAlgebra@, @TO reesIdeal@ to compute the ideal.
   Example
     R = QQ[a..e]
     J = monomialCurveIdeal(R, {1,2,3})
     symmetricKernel (gens J)
   Text
     Let I be the ideal returned and let S be the ring it lives in 
    (also printed). The ring S/I is isomorphic to 
    the Rees algebra R[Jt].  We can get the same information
    above using {\tt reesIdeal(J)}, see @TO reesIdeal@. Note that the degree length
    of S is one more than the degree length of R; the old variables
    now have first degree 0, while the new variables have first degree 1.
   Example
     S = ring oo;
     (monoid S).Options.Degrees
   Text
     The function {\tt symmetricKernel} can also be computed over a quotient ring.
   Example
     R = QQ[x,y,z]/ideal(x*y^2-z^9)
     J = ideal(x,y,z)
     symmetricKernel(gens J)
   Text
     The many ways of working with this function allows the system 
     to compute both the classic Rees algebra of an ideal over a ring 
     (polynomial or quotient) and to compute the the Rees algebra of a 
     module or ideal using a versal embedding as described in the paper 
     of Eisenbud, Huneke and Ulrich.  It also allows different ways of 
     setting up the quotient ring.
  SeeAlso
     reesIdeal
     reesAlgebra
     versalEmbedding
///


doc ///
  Key
    Trim  
  Headline
    Choose whether to trim (or find minimal generators) for the ideal or module.
  Usage
    reesIdeal(..., Trim => true)
  Description
   Text
    Note that when Trim=>true, the generators used will be the ones (and in the order) M2 likes,
    possibly not the original ones.
  SeeAlso
   reesIdeal
   reesAlgebra
   specialFiberIdeal
   specialFiber
   expectedReesIdeal
///

///
  Description
    Text
      When searching for a minimal reduction of an ideal over a field with
      a small number of elements, random choices of generators are often
      not good enough. This option controls how many times the routine
      will try new random choices before giving up and reporting an error.
    Example
      setRandomSeed(314159268)
      kk=ZZ/2
      S = kk[a,b,c,d];
      I = monomialCurveIdeal(S, {1,3,4});
      minimalReduction(I, Tries=>30);
///
      
doc ///
  Key
    [minimalReduction, Tries]
    Tries
  Headline
    Set the number of random tries to compute a minimal reduction
  Usage
    minimalReduction(..., Tries => 20)
  Description
    Text
      When searching for a minimal reduction of an ideal over a field with
      a small number of elements, random choices of generators are often
      not good enough. This option controls how many times the routine
      will try new random choices before giving up and reporting an error.
    Example
      setRandomSeed(314159268)
      kk=ZZ/2
      S = kk[a,b,c,d];
      I = monomialCurveIdeal(S, {1,3,4});
      minimalReduction(I, Tries=>30);
///

doc ///
  Key
    versalEmbedding
    (versalEmbedding,Ideal)
    (versalEmbedding,Module)
  Headline
    Compute a versal embedding
  Usage
    u = versalEmbedding M
  Inputs
    M:Module
      or @ofClass Ideal@
  Outputs
    u:Matrix
      a matrix that induces a versal embedding of the R-module M
      into a free R-module.
  Description
   Text
      For any module M over a Noetherian ring R there is a map $u: M \to H$
      that is versal for maps from M to free modules; that is,
      such that any map from M to a free module factors through u. Such a map
      may be constructed by choosing a set of s generators for Hom(M,R), and using
      them as the components of a map $u: M \to H := R^s$.
      
      (NOTE: In the paper of  Eisenbud, Huneke and Ulrich
      cited below, the versal map is described with the 
      term ``universal'', which is misleading, since the induced map
      from H is generally not unique.)
      
      Suppose that $M$ has a free presentation $F \to G$, and let $u1$ be the
      map $u1: G\to H$ induced by composing $u$ with the surjection $p: G \to
      M$.  By definition, the Rees algebra of $M$ is the image of the induced
      map $Sym(u1): Sym(G)\to Sym(H)$, and thus can be computed with
      symmetricKernel(u1).  The map u is computed from the dual of the first
      syzygy map of the dual of the presentation of $M$.

      We first give a simple example looking at the syzygy matrix of the cube of
      the maximial ideal of a polynomial ring.
   Example
      S = ZZ/101[x,y,z];
      FF=res ((ideal vars S)^3);
      M=coker (FF.dd_2)
      versalEmbedding M
   Text
      A more complicated example.
   Example
      x = symbol x;
      R=QQ[x_1..x_8];
      m1=genericMatrix(R,x_1,2,2); m2=genericMatrix(R,x_5,2,2);
      m=m1*m2
      d1=minors(2,m1); d2=minors(2,m2);
      M=matrix{{0,d1_0,m_(0,0),m_(0,1)},	   {0,0,m_(1,0),m_(1,1)},	   {0,0,0,d2_0},	   {0,0,0,0}}
      M=M-(transpose M);
      N= coker(res coker transpose M).dd_2
      versalEmbedding(N)
   Text
      Here is an example from the paper "What is the Rees Algebra of a
      Module" by David Eisenbud, Craig Huneke and Bernd Ulrich,
      Proc. Am. Math. Soc. 131, 701-708, 2002.  The example shows that one
      cannot, in general, define the Rees algebra of a module by using *any*
      embedding of that module, even when the module is isomorphic to an ideal;
      this is the reason for using the map provided by the routine
      versalEmbedding. Note that the same paper shows that such problems do
      not arise when the ring is torsion-free as a ZZ-module, or when one takes
      the natural embedding of the ideal into the ring.
   Example
      p = 3;
      S = ZZ/p[x,y,z];
      R = S/((ideal(x^p,y^p))+(ideal(x,y,z))^(p+1))
      I = module ideal(z)
   Text
      As a module (or ideal), $Hom(I,R^1)$ is minimally generated by 3 elements,
      and thus a versal embedding of $I$ into a free module is into $R^3$.
   Example
      betti Hom(I,R^1)
      ui = versalEmbedding I
   Text
      it is injective:
   Example
      kernel ui
   Text
      It is easy to make two other embeddings of $I$ into free modules. One is
      the natural inclusion of $I$ into $R$ as an ideal:
   Example
      inci = map(R^1,I,matrix{{z}})
      kernel inci
   Text
      Another is the map defined by multiplication by x and y. 
   Example
      gi = map(R^2, I, matrix{{x},{y}})
      kernel gi
   Text
      We can compose $ui, inci$ and $gi$ with a surjection $R\to i$ to get maps
      $u:R^1 \to R^3, inc: R^1 \to R^1$ and $g:R^1 \to R^2$ having image $i$.
   Example
      u= map(R^3,R^{-1},ui)
      inc=map(R^1, R^{-1}, matrix{{z}})
      g=map(R^2, R^{-1}, matrix{{x},{y}})
   Text
      We now form the symmetric kernels of these maps and compare them.  Note
      that since symmetricKernel defines a new ring, we must bring them to the
      same ring to make the comparison.  First the map u, which would be used
      by reesIdeal:
   Example
      A=symmetricKernel u
   Text
      Next the inclusion:
   Example
      B1=symmetricKernel inc
      B=(map(ring A, ring B1)) B1
   Text
      Finallly, the map g1:
   Example
      C1 = symmetricKernel g
      C=(map(ring A, ring C1)) C1
   Text
      The following test yields ``true'', as implied by the theorem of
      Eisenbud, Huneke and Ulrich.
   Example
      A==B
   Text
      But the following yields ``false'', showing that one must take care
      in general, which inclusion one uses.
   Example
      A==C
  SeeAlso
      reesIdeal
      reesAlgebra
      symmetricKernel
///


doc ///
  Key
    reesIdeal
    (reesIdeal,Ideal)
    (reesIdeal,Module)
    (reesIdeal,Ideal, RingElement)
    (reesIdeal,Module, RingElement)
    [reesIdeal,Jacobian]
    [reesIdeal,Trim]    
  Headline
    Compute the defining ideal of the Rees Algebra
  Usage
    reesIdeal M
    reesIdeal(M,f)
  Inputs
    M:Module
      or @ofClass Ideal@ of a quotient polynomial ring $R$
    f:RingElement
      any non-zerodivisor in ideal or the first Fitting ideal of the module.  Optional
  Outputs
    :Ideal
      defining the Rees algebra of M
  Description
    Text
      This routine gives the user a choice between two methods for finding the
      defining ideal of the Rees algebra of an ideal or module $M$ over a ring
      $R$: The command {\tt reesIdeal(M)}
      computes a versal embedding $g: M\to G$ and a surjection $f: F\to M$
      and returns the result of symmetricKernel(gf). 
      
      When M is an ideal (the usual case) or in characteristic 0, the same
      ideal can be computed by an alternate method that is often faster.
      If the
      user knows a non-zerodivisor $a\in{} R$ such that $M[a^{-1}$ is a free
      module (for example, when M is an ideal, any non-zerodivisor $a \in{} M$ 
      then it is often much faster to compute
        {\tt reesIdeal(M,a)}
      which computes the saturation of the defining ideal of the symmetric algebra
      of M with respect to a. This
      gives the correct answer even under the slightly weaker hypothesis that
      $M[a^{-1}]$ is {\em of linear type}. (See also @TO isLinearType@.)

   Example
      kk = ZZ/101;
      S=kk[x_0..x_4];
      i= trim monomialCurveIdeal(S,{2,3,5,6})
      time V1 = reesIdeal i; 
      time V2 = reesIdeal(i,i_0); 
   Text
     The following example shows how we handle degrees
   Example
      S=kk[a,b,c]
      m=matrix{{a,0},{b,a},{0,b}}
      i=minors(2,m)
      time I1 = reesIdeal i;
      time I2 = reesIdeal(i,i_0);
      transpose gens I1
      transpose gens I2      
   Text
      {\bf Investigating plane curve singularities:} 
      
      Proj of the Rees algebra of I \subset{} R
      is the blowup of I in spec R. Thus the Rees algebra is a basic construction in
      resolution of singularities. Here we work out a simple case:
   Example
      R = ZZ/32003[x,y]
      I = ideal(x,y)
      cusp = ideal(x^2-y^3)
      RI = reesIdeal(I)
      S = ring RI
      totalTransform = substitute(cusp, S) + RI
      D = decompose totalTransform -- the components are the strict transform of the cuspidal curve and the exceptional curve 
      totalTransform = first flattenRing totalTransform
      L = primaryDecomposition totalTransform 
      apply(L, i -> (degree i)/(degree radical i))
   Text
      The total transform of the cusp contains the exceptional divisor with
      multiplicity two.  The strict transform of the cusp is a smooth curve but
      is tangent to the exceptional divisor
   Example
      use ring L_0
      singular = ideal(singularLocus(L_0));
      SL = saturate(singular, ideal(x,y));
      saturate(SL, ideal(w_0,w_1))
   Text
      This shows that the strict transform is smooth.
  SeeAlso
    symmetricKernel
    reesAlgebra
///

doc ///
  Key
    reesAlgebra
    (reesAlgebra,Ideal)
    (reesAlgebra, Module)
    (reesAlgebra,Ideal, RingElement)
    (reesAlgebra,Module, RingElement)
  Headline
    Compute the defining ideal of the Rees Algebra
  Usage
    A = reesAlgebra M
    A = reesAlgebra(M,f)
  Inputs
    M:Module
      or @ofClass Ideal@ of a quotient polynomial ring $R$
    f:RingElement
      any non-zerodivisor in ideal or the first Fitting ideal of the module.  Optional
  Outputs
    A:Ring
      defining the Rees algebra of M
  Description
    Text
      If $M$ is an ideal or module over a ring $R$, and $F\to M$ is a
      surjection from a free module, then reesAlgebra(M) returns the ring
      $Sym(F)/J$, where $J = reesIdeal(M)$.  
      
      In the following example, we find the Rees Algebra of a monomial curve
      singularity.  We also demonstrate the use of @TO reesIdeal@, @TO symmetricKernel@,
      @TO isLinearType@, @TO normalCone@, @TO associatedGradedRing@, @TO specialFiberIdeal@.
    Example
      S = QQ[x_0..x_3]
      i = monomialCurveIdeal(S,{3,7,8})      
      I = reesIdeal i;
      reesIdeal(i, Variable=>v)
      I=reesIdeal(i,i_0);
      (J=symmetricKernel gens i);
      isLinearType(i,i_0)
      isLinearType i
      reesAlgebra (i,i_0)
      trim ideal normalCone (i, i_0)
      trim ideal associatedGradedRing (i,i_0)
      trim specialFiberIdeal (i,i_0)
  SeeAlso
    reesIdeal
    symmetricKernel
///


doc ///
  Key
    isLinearType
    (isLinearType, Module)
    (isLinearType, Ideal) 
    (isLinearType,Module, RingElement)
    (isLinearType, Ideal, RingElement)
    
  Headline
     Determine whether module has linear type
  Usage
     isLinearType M
     isLinearType(M,f)
  Inputs
     M:Module
       or @ofClass Ideal@
     f:RingElement
      any non-zero divisor modulo the ideal or module.  Optional
  Outputs
     :Boolean
       true if M is of linear type, false otherwise
  Description
   Text
     A module or ideal $M$ is said to be ``of linear type'' if the natural map
     from the symmetric algebra of $M$ to the Rees algebra of $M$ is an
     isomorphism. It is known, for example, that any complete intersection
     ideal is of linear type.

     This routine computes the @TO reesIdeal@ of M.  Giving the element f
     computes the @TO reesIdeal@ in a different manner, which is sometimes
     faster, sometimes slower.  
   Example
      S = QQ[x_0..x_4]
      i = monomialCurveIdeal(S,{2,3,5,6})
      isLinearType i
      isLinearType(i, i_0)
      I = reesIdeal i
      select(I_*, f -> first degree f > 1)
   Example
      S = ZZ/101[x,y,z]
      for p from 1 to 5 do print isLinearType (ideal vars S)^p
  SeeAlso
    reesIdeal
    monomialCurveIdeal
///

doc ///
  Key
    isReduction
    (isReduction, Ideal, Ideal)
    (isReduction, Ideal, Ideal, RingElement)
    (isReduction, Module, Module)
    (isReduction, Module, Module, RingElement)
    
  Headline
     Determine whether an ideal is a reduction
  Usage
     t=isReduction(I,J)
     t=isReduction(I,J,f)
  Inputs
     I:Ideal
     J:Ideal
     f:RingElement
       an optional element, which is a non-zerodivisor modulo M and the ring of M
  Outputs
     t:Boolean
       true if J is a reduction of I, false otherwise
  Description
   Text
    For an ideal $I$, a subideal $J$ of $I$ is said to be a {\bf reduction}
    of $I$ if there exists a nonnegative integer n such that 
    $JI^{n}=I^{n+1}$.

    This function returns true if $J$ is a reduction of $I$ and returns false
    if $J$ is not a subideal of $I$ or $J$ is a subideal but not a reduction of $I$.  
   Example
    S = ZZ/5[x,y]
    I = ideal(x^3,x*y,y^4)
    J = ideal(x*y, x^3+y^4)
    isReduction(I,J)
    isReduction(J,I)
    isReduction(I,I)
    g = I_0
    isReduction(I,J,g)
    isReduction(J,I,g)
    isReduction(I,I,g)

  SeeAlso
    minimalReduction
    reductionNumber
///


doc ///
  Key
    normalCone
    (normalCone, Ideal)
    (normalCone, Ideal, RingElement)
    
  Headline
    The normal cone of a subscheme
  Usage
    normalCone I
    normalCone(I,f)
  Inputs
    I:Ideal
    f:RingElement
      optional argument, if given it should be a non-zero divisor in the ideal I
  Outputs
    :Ring
      the ring $R[It] \otimes{} R/I$ of the normal cone of $I$
  Description
    Text
      The normal cone of an ideal $I\subset{} R$ is the ring $R/I \oplus{} I/I^2
      \oplus \ldots$, also called the associated graded ring of $R$ with
      respect to $I$.  If $S$ is the Rees algebra of $I$, then this ring is
      isomorphic to $S/IS$, which is how it is computed here.
  SeeAlso
    reesAlgebra
    associatedGradedRing
    normalCone
///




doc ///
  Key
    multiplicity
    (multiplicity, Ideal)
    (multiplicity, Ideal, RingElement)
    
  Headline
     Compute the Hilbert-Samuel multiplicity of an ideal
  Usage
     multiplicity I
     multiplicity(I,f)
  Inputs
    I:Ideal
    f:RingElement
      optional argument, if given it should be a non-zero divisor in the ideal I
  Outputs
    :ZZ
      the normalized leading coefficient of the Hilbert-Samuel polynomial of $I$
  Description
   Text
     Given an ideal $I\subset{} R$, ``multiplicity I'' returns the degree of the
     normal cone of $I$.  When $R/I$ has finite length this is the sum of the
     Samuel multiplicities of $I$ at the various localizations of $R$. When $I$
     is generated by a complete intersection, this is the length of the ring
     $R/I$ but in general it is greater. For example,
   Example
     R=ZZ/101[x,y]
     I = ideal(x^3, x^2*y, y^3)
     multiplicity I
     degree I
  Caveat
     The normal cone is computed using the Rees algebra, thus may be slow.
  SeeAlso
///

doc ///
  Key
    specialFiberIdeal
    (specialFiberIdeal, Module)
    (specialFiberIdeal, Ideal)
    (specialFiberIdeal, Module, RingElement)
    (specialFiberIdeal, Ideal, RingElement)
    [specialFiberIdeal, Jacobian]
    [specialFiberIdeal, Trim]
  Headline
     Special fiber of a blowup     
  Usage
     specialFiberIdeal M
     specialFiberIdeal(M,f)
  Inputs
     M:Module
       or @ofClass Ideal@
     f:RingElement
      a non-zerodivisor such that $M[f^{-1}]$ is a free module when $M$ is a module, an element in $M$ when $M$ is an ideal
  Outputs
     :Ideal
  Description
   Text
     Let $M$ be an $R = k[x_1,\ldots,x_n]/J$-module (for example an ideal), and
     let $mm=ideal vars R = (x_1,\ldots,x_n)$, and suppose that $M$ is a
     homomorphic image of the free module $F$. Let $T$ be the Rees algebra of
     $M$. The call specialFiberIdeal(M) returns the ideal $J\subset{} Sym(F)$
     such that $Sym(F)/J \cong{} T/mm*T$; that is, $specialFiberIdeal(M) =
     reesIdeal(M)+mm*Sym(F).$

     The name derives from the fact that $Proj(T/mm*T)$ is the special fiber of
     the blowup of $Spec R$ along the subscheme defined by $I$.
     
     With the default Trim => true, the computation begins by computing minimal generators,
     which may result in a change of generators of M
   Example
     R=QQ[a..h]
     M=matrix{{a,b,c,d},{e,f,g,h}}
     analyticSpread minors(2,M)
     specialFiberIdeal minors(2,M)
   Text
     If M is an n x n+1 matrix in n variables, and all generators have the 
     same degree d, with ell = n as expected,
     then the special fiber is a rational hypersurface of degree $D := d^n$, and
     the reduction number is D-1.
   Example
     n = 2
     x = symbol x
     S = ZZ/32003[x_1..x_n]
     M = matrix{{x_1,x_2,0},{0,x_1,x_2}}
     I = minors(n,M)
     specialFiber(I,I_0)  
  Caveat
     Special fiber is here defined to be the fiber of the blowup over the
     subvariety defined by the vars of the original ring. Note that if the
     original ring is a tower ring, this might not be the fiber over the
     closed point! To get the closed
     fiber, flatten the base ring first. 

  SeeAlso
     reesIdeal
///

doc ///
  Key
    specialFiber
    (specialFiber, Module)
    (specialFiber, Ideal)
    (specialFiber, Module, RingElement)
    (specialFiber, Ideal, RingElement)
    [specialFiber, Jacobian]
    [specialFiber, Trim]    
  Headline
     Special fiber of a blowup     
  Usage
     specialFiber M
     specialFiber(M,f)
  Inputs
     M:Module
       or @ofClass Ideal@
     f:RingElement
       an optional element, which is a non-zerodivisor such that $M[f^{-1}]$ is a free module when $M$ is a module, an element in $M$ when $M$ is an ideal
  Outputs
     :Ring
  Description
   Text
     Let $M$ be an $R = k[x_1,\ldots,x_n]/J$-module (for example an ideal), and
     let $mm=ideal vars R = (x_1,\ldots,x_n)$, and suppose that $M$ is a
     homomorphic image of the free module $F$ with $m+1$ generators. Let $T$ be the Rees algebra of
     $M$. The call specialFiber(M) returns the ideal $J\subset{} k[w_0,\dots,w_m]$
     such that $k[w_0,\dots,w_m]/J \cong{} T/mm*T$; that is, $specialFiber(M) =
     reesIdeal(M)+mm*Sym(F)$. This routine differs from @TO specialFiberIdeal@ in that
     the ambient ring of the output ideal is $k[w_0,\dots,w_m]$ rather than
     $R[w_0,\dots,w_m]$. 
     The coefficient ring $k$ used is always the 
     @TO ultimate@ @TO2 {coefficientRing, "coefficient ring"} @ of $R$.
     
     The name derives from the fact that $Proj(T/mm*T)$ is the special fiber of
     the blowup of $Spec R$ along the subscheme defined by $I$.

     With the default Trim => true, the computation begins by computing minimal generators,
     which may result in a change of generators of M
   Example
     R=QQ[a..h]
     M=matrix{{a,b,c,d},{e,f,g,h}}
     analyticSpread minors(2,M)
     specialFiber minors(2,M)
  SeeAlso
     reesIdeal
     specialFiberIdeal
///

doc ///
  Key
    analyticSpread
    (analyticSpread, Module)
    (analyticSpread, Ideal)
    (analyticSpread, Module, RingElement)
    (analyticSpread, Ideal, RingElement)
  Headline
     Compute the analytic spread of a module or ideal
  Usage
     analyticSpread M
     analyticSpread(M,f)
  Inputs
     M:Module
       or @ofClass Ideal@
     f:RingElement
       an optional element, which is a non-zerodivisor such that $M[f^{-1}]$ is a free module when $M$ is a module, an element in $M$ when $M$ is an ideal
  Outputs
     :ZZ
       the analytic spread of a module or an ideal $M$
  Description
   Text
     The analytic spread of a module is the dimension of its special fiber
     ring.  When $I$ is an ideal (and more generally, with the right
     definitions) the analytic spread of $I$ is the smallest number of
     generators of an ideal $J$ such that $I$ is integral over $J$. See for
     example the book Integral closure of ideals, rings, and modules. London
     Mathematical Society Lecture Note Series, 336. Cambridge University Press,
     Cambridge, 2006, by Craig Huneke and Irena Swanson.
   Example
     R=QQ[a..h]
     M=matrix{{a,b,c,d},{e,f,g,h}}
     analyticSpread minors(2,M)
     specialFiberIdeal minors(2,M)
     R=QQ[a,b,c,d]
     M=matrix{{a,b,c,d},{b,c,d,a}}
     analyticSpread minors(2,M)
     specialFiberIdeal minors(2,M)
  SeeAlso
     specialFiberIdeal
     reesIdeal
///

doc ///
  Key
    distinguished
    (distinguished, RingMap, Ideal)
    (distinguished, Ideal, Ideal)
    (distinguished, Ideal)    
  Headline
     Compute the distinguished subvarieties of a pullback, intersection or cone
  Usage
     L = distinguished(f,I)
     L = distinguished(I,J)
     L = distinguished(I)     
  Inputs
    f:RingMap
    I:Ideal
    J:Ideal
  Outputs
    L:List
  Description
   Text
     Suppose that f:S\to R is a map of rings, and I is an ideal of S.
     Let K be the kernel of the map of associated graded rings
     gr_I(S) \to gr_(fI)R.

     The distinguished primes p_i in S/I are the intersections of the
     minimal primes P_i over K with S/I \subset{} gr_IS, that is,
     the minimal primes of the support in R/I of the normal cone of f(I).
     The multiplicity associated
     with p_i is by definition the the multiplicities of P_i in the primary
     decomposition of K.
     
     Distinguished subvarieties and their multiplicity
     (defined by the distinguished primes, usually
     in the global case of a quasi-projective variety and its sheaf of rings) 
     play a central role in the Fulton-MacPherson
     construction of refined intersection products. See William Fulton, Intersection Theory,
     Section 6.1 for the geometric context and the general case, and the explanation
     in the article Rees Algebras in JSAG (submitted).
     
     This application is illustrated in the code for @TO intersectInP@.
     
     We allow the special cases
     
     {\tt distinguished(I,J) := distinguished(f,I)}, with f:S\to S/J the projection
     
     and
     
     {\tt distinguished(I) := distinguished(f,I)}, with f:S\to S the identity.
          
     which computes the distinguished primes in the support of the normal cone gr_IS
     itself.
     An interesting application is given in the paper

     ``A geometric effective Nullstellensatz,''
     Invent. Math. 137 (1999), no. 2, 427--448 by
     Ein and Lazarsfeld.
     
     Here is an example showing that associated primes need not be distinguished primes:
   Example
     R = ZZ/101[a,b]
     I = ideal(a^2, a*b)
     ass I 	 
   Text
     There is one minimal associated prime (a thick line in $P^3$) and one
     embedded prime.
   Example
     distinguished I
     intersectInP(I,I)
  SeeAlso
   intersectInP
   saturate
///

doc ///
   Key
    intersectInP
    (intersectInP, Ideal, Ideal)
   Headline
    Compute distinguished varieties for an intersection in A^n or P^n
   Usage
    L = intersectInP(I,J)
   Inputs
    I:Ideal
     of a polynomial ring P over a field
    J:Ideal
     of the same ring
   Outputs
    L:List
   Description
    Text
     This function applies the technology of @TO distinguished @ to compute
     the distinguished subvarieties, with their multiplicities, for an intersection
     in affine or projective space. The function @TO distinguished @ is actually applied
     to the diagonal ideal in P**P and the ideal I**P + P**I, and the answer is
     pulled back to P.
    Example
     kk = ZZ/101
     P = kk[x,y]
     I = ideal"x2-y";J=ideal y
     intersectInP(I,J)
     I = ideal"x4+y3+1"
     intersectInP(I,J)
     I = ideal"x2y";J=ideal"xy2"
     intersectInP(I,J)
     intersectInP(I,I)
    Text
     Note that in the last two cases, which are improper intersections of
     two cubics, the total multiplicity is 9 = 3*3. But this is not always the case
     (in the actual definition of the intersection product, the multiplicity is
     multiplied by the class of a certain cycle supported on the distinguished subvariety).
    Example
     I = ideal"y-x2"
     intersectInP(I,I)
   Caveat
   SeeAlso
    distinguished
///

doc ///
   Key
    [intersectInP,BasisElementLimit]
    [intersectInP,DegreeLimit]
    [intersectInP,MinimalGenerators]
    [intersectInP,PairLimit]
    [intersectInP,Strategy]
    [intersectInP,Variable]
    [multiplicity,Variable]
   Headline
    Option for intersectInP
   Description
    Text
     see the options for @TO saturate@.
   SeeAlso
    intersectInP
    distinguished
    saturate
///



doc ///
  Key
    minimalReduction
    (minimalReduction, Ideal)
    
  Headline
    Find a minimal reduction of an ideal
  Usage
    J = minimalReduction I
  Inputs
    I:Ideal
  Outputs
    :Ideal
      A minimal reduction of I (defined below)
  Description
    Text
      {\tt minimalReduction} takes an ideal I that is homogeneous or inhomogeneous
      (in the latter case the ideal is to be regarded as an ideal in the 
      localization of the polynomial ring at the origin.). It returns an ideal $J$
      contained in $I$, with a minimal number of generators 
      such that $I$ is integrally dependent on $J$. This minimal number is called
      the analyticSpread of $I$.
     
      This routine is probabilistic: $J$ is computed as the ideal generated by the
      right number of random linear combinations of the generators of $I$. However, the
      routine checks rigorously that the output ideal is a reduction, and tries
      probabilistically again if it is not. If it cannot find a minimal reduction after
      a certain number of tries, it returns an error. The number of tries defaults
      to 20, but can be set with the optional argument @TO Tries@.

      To say that $I$ is integrally dependent on $J$ means that
      $JI^k = I^{k+1}$ for some non-negative integer $k$.  The smallest $k$ with this
      property is called the reduction number of $I$, and can be computed
      with @TO reductionNumber@ i.

      See the book 
      Huneke, Craig; Swanson, Irena: Integral closure of ideals, rings, and modules. 
      London Mathematical Society Lecture Note Series, 336. Cambridge University Press, Cambridge, 2006.
      for further information.
    Example
      kk = ZZ/101;
      S = kk[a..c];
      m = ideal vars S;
      i = (ideal"a,b")*m+ideal"c3"
      analyticSpread i
      minimalReduction i
    Text
      Note that this is inhomogeneous--
      it is generated by 3 random linear combinations
      of the generators of i.
      There is no homogeneous ideal with just 3 generators
      on which i is integrally dependent.
    Example
      f = gens i
      for a from 0 to 3 do(jhom:=ideal (f*random(source f, S^{3-a:-2,a:-3})); print(i^6 == (i^5)*jhom))
  Caveat
     It is possible that the ideal returned is not a minimal reduction,
     due to the probabilistic nature of the routine.  This will be addressed in a future version
     of the package.  The larger the size of the base field, the less likely this is to happen.
  SeeAlso
    analyticSpread
    reductionNumber
    whichGm
///

doc ///
  Key
    reductionNumber
    (reductionNumber, Ideal, Ideal)
  Headline
    Reduction number of one ideal with respect to another
  Usage
    k = reductionNumber(I,J)
  Inputs
    I:Ideal
    J:Ideal
  Outputs
    :ZZ
      the reduction number of $I$ (defined below)
  Description
    Text
      The function {\tt reductionNumber} takes a pair of ideals $I,J$, homogeneous or inhomogeneous
      (in the latter case $I$ and $J$ are to be regarded as ideals in the 
      localization of the polynomial ring at the origin.).
      The ideal $J$ must be a reduction of $I$ (that is, $J\subset{} I$
      and $I$ is integrally dependent on $J$. This condition is checked by
      the function @TO isReduction@. It returns the smallest integer $k$ such that
      $JI^k = I^{k+1}$.
      
      For further information, see the book:
      Huneke, Craig; Swanson, Irena: Integral closure of ideals, rings, and modules, 
      London Mathematical Society Lecture Note Series, 336. Cambridge University Press, 
      Cambridge, 2006.
    Example
      setRandomSeed()
      kk = ZZ/32003;
      S = kk[a..c];
      m = ideal vars S;
      i = (ideal"a,b")*m+ideal"c3"
      analyticSpread i
      j=minimalReduction i
      reductionNumber (i,j)
  Caveat
     It is possible for the routine to not finish in reasonable time, due to the
     probabilistic nature of the routine.  What happens is that 
     the routine @TO minimalReduction@ occasionally, but rarely, returns an ideal
     which is not a minimal reduction.  In this case, the routine goes into an infinite loop.
     This will be addressed in a future version
     of the package.  In the meantime, simply interrupt the routine, and restart the
     computation.
  SeeAlso
    analyticSpread
    minimalReduction
    whichGm
///

doc ///
  Key
    whichGm
    (whichGm, Ideal)
  Headline
    Largest Gm satisfied by an ideal
  Usage
    whichGm I
  Inputs
    I:Ideal
  Outputs
    :ZZ
      what it does
  Description
    Text
      An ideal $I$ in a ring $S$ is said to satisfy the condition $G_m$ if, for every prime ideal $P$ of
      codimension $0<k<m$, the ideal $I_P$ in $S_P$ can be generated by at most $k$ elements. 
      
      The command {\tt whichGm I}
      returns the largest $m$ such that $I$ satisfies $G_m$, or infinity if $I$ satisfies $G_m$ 
      for every $m$.

      This condition arises frequently in work of Vasconcelos and Ulrich and their schools
      on Rees algebras and powers of ideals. See for example
      Morey, Susan; Ulrich, Bernd:
      Rees algebras of ideals with low codimension.  
      Proc. Amer. Math. Soc.  124  (1996),  no. 12, 3653--3661.
    Example
      kk=ZZ/101;
      S=kk[a..c];
      m=ideal vars S
      i=(ideal"a,b")*m+ideal"c3"
      whichGm i
  SeeAlso
    analyticSpread
    minimalReduction
    reductionNumber
///

doc ///
   Key
    jacobianDual
    (jacobianDual, Matrix)
    (jacobianDual, Matrix, Matrix, Matrix)
   Headline
    Computes the 'jacobian dual', part of a method of finding generators for Rees Algebra ideals
   Usage
    psi = jacobianDual phi
    psi = jacobianDual(phi, X, T)
   Inputs
    phi:Matrix
     presentation matrix of an ideal
    X:Matrix
     row matrix generating an ideal that contains the entries of phi
    T:Matrix
     row matrix of variables that will be generators of the Rees algebra of I
   Outputs
    psi:Matrix
     the `Jacobian Dual', which satisfies $T*phi = X*psi$
   Description
    Text
     Let I be an ideal of R and let phi be the presentation matrix of I as a module.
     The symmetric algebra of I has the form 
     
     $Sym_R(I) = R[T_0..T_m]/ideal(T*phi)$
     
     where the T_i correspond to the generators of I. If $X = matrix\{\{x_1..x_n\}\}$,
     with x_i \in{} R, and ideal X contains the entries of the matrix phi, then there is 
     a matrix psi defined over R[T_0..T_m], called the Jacobian Dual of phi with respect to X,
     such that $T*phi = X*psi$. (the matrix psi is generally
     not unique; Macaulay2 computes it using Groebner division with remainder.)
     
     In the form {\tt psi = jacobianDual phi},
     a new ring ST := S[T_0..T_m] is created, and the vector X is set to the variables
     of R. The result is returned as a matrix over ST. 
     To do the computation in a ring previously defined computed, 
     use the form {\tt psi = jacobianDual(phi, X,T)};
     in this case, the matrices phi, X, T should all be defined over the
     same ring ST, 
     the matrix T should be a row of variables of ST, and
     the matrix phi should have entries in a subring not involving the entries of T.
      
     If I is an ideal of grade >=1 and ideal X contains a nonzerodivisor of R
     (which will be automatic if I has finite projective dimension) then
     ideal X has grade >= 1 on the Rees algebra. Since ideal(T*phi) is contained in the
     defining ideal of the Rees algebra, the vector X is annihilated by the matrix
     psi when regarded over the Rees algebra. If also the number of relations of I
     is >= the number of generators of I, this implies that the maximal minors of
     psi annihilate  the x_i as elements of the Rees algebra, and thus that the maximal
     minors of psi are inside the ideal of the Rees algebra. In very favorable circumstances,
     one may even have the equality reesIdeal I = ideal(T*phi)+ideal minors(psi): For example:
     
     Theorem (S. Morey and B. Ulrich, Rees Algebras of Ideals with Low Codimension, Proc. Am. Math.
     Soc. 124 (1996) 3653--3661):
     Let R be a local Gorenstein ring with infinite residue field, let I be a perfect ideal
     of grade 2 with n generators, and let phi be the presentation matrix of I. Let
     ell = ell(I) be the analytic spread. Suppose that
     I satisfies the condition G_{ell} or, equivalently, that the n-p sized minors of phi 
     have codimension >p for 1<= p < ell. The following conditions are equivalent:
     
     1) reesAlgebra I is Cohen-Macaulay and I_(n-ell)(phi) = I_1(phi)^{n-ell}
     2) reductionNumber I < ell and I_(n+1-ell)(phi) = I_1(phi)^{n+1-ell}
     3) reesIdeal I = symmetricAlgebraIdeal I + minors(n, jacobianDual phi)
     
     We start with the presentation matrix phi of an (n+1)-generator perfect ideal
     Such that the first row consists of the n
     variables of the ring, and the rest of whose rows are reasonably general (in this
     case random quadrics):
    Example
     setRandomSeed 0
     n=3;
     kk = ZZ/101;
     S = kk[a_0..a_(n-2)];
     phi' = map(S^(n),S^(n-1), (i,j) -> if i == 0 then a_j else random(2,S));
     I = minors(n-1,phi');
     betti (F = res I)
     phi = F.dd_2;
     jphi = jacobianDual phi
    Text
     We first compute the analytic spread ell and the reduction number r
    Example
     ell = analyticSpread I
     r = reductionNumber(I, minimalReduction I)
    Text
     Now we can check the condition G_{ell}, first probabilistically
    Example
     whichGm I >= ell
    Text
     and now deterministically
    Example
     apply(toList(1..ell-1), p-> {p+1, codim minors(n-p, phi)})
    Text
     We now check the three equivalent conditions of the Morey-Ulrich Theorem.
     Note that since ell = n-1 in this case, the second part of conditions
     1,2 is vacuously satisfied, and since r<ell,
     the conditions must all be satisfied.
     We first check that reesAlgebra I is Cohen-Macaulay:
    Example
     reesI = reesIdeal I;
     codim reesI
     betti res reesI
    Text
     Finally, we wish to see that reesIdeal I is generated by the ideal 
     of the symmetric algebra together with the jacobian dual:
    Example
     psi = jacobianDual phi
    Text
     We now compute the ideal J of the symmetric algebra; the call symmetricAlgebra I
     would return the ideal over a different ring, so we do it by hand:
    Example
     ST = ring psi
     T = vars ST
     J = ideal(T*promote(phi, ST))
     betti res J
     J1 = minors(ell, psi)
     betti (G = res trim (J+J1))
     betti res reesIdeal I
    Text
     The name Jacobian Dual comes from the case where phi is a matrix of linear forms
     the x_i are the variables of R, and the generators of I are forms, all of the same degree D;
     in this case Euler's formula sum(df_i/dx_j*xj) = Df can be used to express the
     entries of psi in terms of the derivatives of the entries of phi, at least when
     the degrees of the columns of phi are nonzero in the coefficient field.
     
     Explicitly, let x_1,...,x_n be the variables of R, and let phi be a presentation matrix for I.
     Since all the f_i have the same degree, if follows that,
     for each j, the entries phi_(i,j) will all have the same degree, say D_j = deg phi_(i,j).
     Let ST be the polynomial ring R[T_0..T_m], where the T_i correspond to f_i, and let
     X=matrix{{x_1,...,x_n}}, and T=matrix{{T_0,...,T_m}} be row matrices over ST.
     In this case, by Euler's formula, we may take
     
     psi_{k,j}=(1/D_j)*sum_i(d phi_{i,j}/d x_k*T_i),
   Caveat
     The division with
     remainder step is usually fast, but if this
     ever becomes a bottleneck it would be possible to test for the degree condition and
     use Euler's formula in the case where it applies.
   SeeAlso
    whichGm
    expectedReesIdeal
    reesAlgebra
    reesAlgebraIdeal
    reesIdeal
    specialFiberIdeal
///
doc ///
  Key
    [symmetricKernel, Variable]
    [reesIdeal, Variable]
    [reesAlgebra, Variable]
    [associatedGradedRing, Variable]
    [specialFiberIdeal, Variable]
    [specialFiber, Variable]
    [distinguished, Variable]
    [isReduction, Variable]
    [jacobianDual, Variable]

  Headline
    Choose name for variables in the created ring
  Usage
    symmetricKernel(...,Variable=>w)
    reesIdeal(...,Variable=>w)
    reesAlgebra(...,Variable=>w)
    specialFiberIdeal(...,Variable=>w)
    specialFiber(...,Variable=>w)    
    distinguished(...,Variable=>w)
    isReduction(...,Variable=>w)
    jacobianDual(...,Variable=>w)

  Description
    Text
      Each of these functions creates a new ring of the form R[w_0,\ldots, w_r]
      or R[w_0,\ldots, w_r]/J, where R is the ring of the input ideal or module
      (except for @TO specialFiber@, which creates a ring $K[w_0,\ldots, w_r]$, 
      where $K$ is the ultimate coefficient ring of the input ideal or module.)
      This option allows the user to change the names of the new variables in this ring.
      The default variable is w.
    Example
      R = QQ[x,y,z]/ideal(x*y^2-z^9)
      J = ideal(x,y,z)
      I = reesIdeal(J, Variable => p)
    Text
      To lift the result to an ideal in a flattened ring, use @TO flattenRing@:
    Example
      describe ring I
      I1 = first flattenRing I
      describe ring oo      
    Text
      Note that the rings of I and I1 both have bigradings. Use @TO newRing@ to
      make a new ring with different degrees.
    Example
      S = newRing(ring I1, Degrees=>{numgens ring I1:1})
      describe S
      I2 = sub(I1,vars S)
      res I2
  SeeAlso
    flattenRing
    newRing
    substitute
///

doc ///
   Key
    [reesIdeal, Strategy]
    [reesAlgebra,Strategy]
    [isLinearType,Strategy]
    [isReduction, Strategy]    	  
    [normalCone, Strategy]    	  
    [multiplicity, Strategy]    	  
    [specialFiberIdeal, Strategy]    	  
    [specialFiber, Strategy]    	  
    [analyticSpread, Strategy]    	  
    [distinguished,Strategy]
    [minimalReduction, Strategy]    	  
   Headline
    Choose a strategy for the saturation step
   Usage
    reesIdeal(...,Strategy => X)
   Description
    Text
     where X is is one of @TO Iterate@, @TO Linear@, @TO Bayer@, @TO Eliminate@.
     These are described in the documentation node for @TO saturate@.
     
     The Rees algebra S(M) of a submodule M of a free module (most importantly,
     an ideal in the ring), is equal to the symmetric algebra Sym_k(M) mod torsion.
     computing this torsion is the slow link in most of the programs in this
     package. The fastest way to compute it is usually by saturating the ideal
     defining the symmetric algebra with respect
     to an element in that ideal.
   SeeAlso
    reesIdeal
    reesAlgebra
    isLinearType
    isReduction
    normalCone
    multiplicity
    specialFiberIdeal
    specialFiber
    analyticSpread
    distinguished
    minimalReduction
    saturate
///
doc ///
   Key
    [reesIdeal, PairLimit]
    [minimalReduction, PairLimit]
    [distinguished,PairLimit]
    [analyticSpread, PairLimit]
    [specialFiber, PairLimit]
    [specialFiberIdeal, PairLimit]
    [multiplicity, PairLimit]
    [normalCone, PairLimit]
    [isReduction, PairLimit]
    [isLinearType,PairLimit]
    [reesAlgebra,PairLimit]
   Headline
    Bound the number of s-pairs considered in the saturation step
   Usage
    reesIdeal(...,PairLimit => X)
   Description
    Text
     Here X is a positive integer. Each of these functions computes the Rees
     Algebra using a saturation step, and the optional argument causes the saturation
     process to stop after that number of s-pairs is found.
     This is described in the documentation node for @TO saturate@.
   SeeAlso
    reesIdeal
    reesAlgebra
    isLinearType
    isReduction
    normalCone
    multiplicity
    specialFiberIdeal
    specialFiber
    analyticSpread
    distinguished
    minimalReduction
    saturate
///
doc ///
   Key
    [reesIdeal, MinimalGenerators]
    [minimalReduction, MinimalGenerators]
    [distinguished,MinimalGenerators]
    [analyticSpread, MinimalGenerators]
    [specialFiber, MinimalGenerators]
    [specialFiberIdeal, MinimalGenerators]
    [multiplicity, MinimalGenerators]
    [normalCone, MinimalGenerators]
    [isReduction, MinimalGenerators]
    [isLinearType,MinimalGenerators]
    [reesAlgebra,MinimalGenerators]
   Headline
    Whether the saturation step returns minimal generators
   Usage
    reesIdeal(...,MinimalGenerators => X)
   Description
    Text
     Here X is of type boolean. Each of these functions involves the
     computation of a Rees algebra, which may involve a saturation step.
     This optional argument determines whether or not 
     the output of the saturation step will be forced to have a minmimal generating set.
     This is described in the documentation node for @TO saturate@.
   SeeAlso
    reesIdeal
    reesAlgebra
    isLinearType
    isReduction
    normalCone
    multiplicity
    specialFiberIdeal
    specialFiber
    analyticSpread
    distinguished
    minimalReduction
    saturate
///
doc ///
   Key
    [minimalReduction, BasisElementLimit]
    [reesIdeal, BasisElementLimit]
    [distinguished,BasisElementLimit]
    [analyticSpread, BasisElementLimit]
    [specialFiber, BasisElementLimit]
    [multiplicity, BasisElementLimit]
    [normalCone, BasisElementLimit]
    [isReduction, BasisElementLimit]
    [isLinearType,BasisElementLimit]
    [reesAlgebra,BasisElementLimit]
    [specialFiberIdeal, BasisElementLimit]
   Headline
    Bound the number of Groebner basis elements to compute in the saturation step
   Usage
    reesIdeal(...,BasisElementLimit => X)
   Description
    Text
     Here X is a positive integer. Each of these functions computes the Rees
     Algebra using a saturation step, and the optional argument causes the saturation
     process to stop after that number of s-pairs is found.
     This is described in the documentation node for @TO saturate@.
   SeeAlso
    reesIdeal
    reesAlgebra
    isLinearType
    isReduction
    normalCone
    multiplicity
    specialFiberIdeal
    specialFiber
    analyticSpread
    distinguished
    minimalReduction
    saturate
///
doc ///
   Key
    [reesIdeal, DegreeLimit]
    [minimalReduction, DegreeLimit]
    [distinguished,DegreeLimit]
    [analyticSpread, DegreeLimit]
    [specialFiber, DegreeLimit]
    [normalCone, DegreeLimit]
    [multiplicity, DegreeLimit]
    [isReduction, DegreeLimit]
    [isLinearType,DegreeLimit]
    [reesAlgebra,DegreeLimit]
    [specialFiberIdeal, DegreeLimit]
   Headline
    Bound the degrees considered in the saturation step. Defaults to infinity
   Usage
    reesIdeal(...,DegreeLimit => X)
   Description
    Text
     where X is a non-negative integer. Stop computation at degree X.
     This is described in the documentation node for @TO saturate@.
     Here X is a positive integer. Each of these functions computes the Rees
     Algebra using a saturation step, and the optional argument causes the saturation
     process to stop after that number of s-pairs is found.
     This is described in the documentation node for @TO saturate@.
   SeeAlso
    reesIdeal
    reesAlgebra
    isLinearType
    isReduction
    normalCone
    multiplicity
    specialFiberIdeal
    specialFiber
    analyticSpread
    distinguished
    minimalReduction
    saturate
///


doc ///
   Key
    expectedReesIdeal
    (expectedReesIdeal, Ideal)
    (expectedReesIdeal, Module)    
   Headline
    symmetric algebra ideal plus jacobian dual
   Usage
    J = expectedReesIdeal M
   Inputs
    M:Ideal
    M:Module
   Outputs
    J:Ideal
   Description
    Text
     Let M be an R-module with g generators and free presentation phi: R^h \to R^g. The symmetric algebra of M
     can be written as R[T_1,\dots,T_g]/J, where J is the ideal generated by the entries of the 1 x h matrix
     T*m, where T = (T_1..T_g). If the entries of m are all contained in an ideal (X_1..X_n) (for example, when
     m is a minimal presentation and the X_i generate the maximal ideal, there is a matrix psi: R[Z]^h \to R[Z]^n
     such that T*phi = X*psi. Under reasonable hypotheses (eg when R is a domain) the relation
     X*psi = 0 in the Rees algebra implies that the n x n minors of psi are 0. Thus these minors lie in the ideal
     defining the Rees algebra. The expectedReesIdeal is the sum of the ideals (T*phi) and the ideal of nxn minors of psi.
     Under particularly good circumstances this sum is known to be equal to the ideal of the Rees algrebra. More generally,
     it may speed computations of @TO reesIdeal@ to start with this sum rather than with the ideal T*phi, as in the following
     example. (This can be turned off with the Jacobian=>false option.)
    
     The term 'Expected Rees Ideal' for the sum of 
     of the ideal of the symmetric algebra of I with
     the ideal of maximal minors of the Jacobian dual matrix of a presentation of I
     is derived from the paper 
     "Rees Algebras of Ideals of Low Codimension", Proc. Am. Math. Soc. 1996
     of Colley and Ulrich. Building on the paper 
     "Ideals with Expected Reduction Number", Am. J. Math 1996,
     they prove that this ideal is in fact equal to the 
     ideal of the Rees algebra of I when I is a codimension 2 perfect ideal whose
     Hilbert-Burch matrix has a special form. See @TO jacobianDual@ for an example.
    Example
     setRandomSeed 0
     n = 5
     S = ZZ/101[x_0..x_(n-2)];
     M1 = random(S^(n-1),S^{n-1:-2});
     M = M1||vars S
     I = minors(n-1, M);
     time rI = expectedReesIdeal I; -- n= 5 case takes < 1 sec.
     --time rrI = reesIdeal(I,I_0); -- n = 5 case ~20 sec
     --time rrrI = reesIdeal I; -- n = 4 case > 1 minute; I didn't wait to see!
     --assert(rI == (map(ring rI, ring rrI, vars ring rI)) rrI)
     kk = ZZ/101;
     S = kk[x,y,z];
     m = random(S^3, S^{4:-2});
     I = minors(3,m);
     time reesIdeal (I, I_0);
     time reesIdeal (I, I_0, Jacobian =>false);
   SeeAlso
    symmetricAlgebraIdeal
    jacobianDual
///

///     


///

///
  uninstallPackage "ReesAlgebra"
  restart
  installPackage "ReesAlgebra"
  check "ReesAlgebra"
  viewHelp PlaneCurveSingularities

restart
loadPackage("ReesAlgebra", Reload=>true)
///

doc ///
   Key
    PlaneCurveSingularities
   Headline
    Using the Rees Algebra to resolve plane curve singularities
   Description
    Text
     The Rees Algebra of an ideal I appeared classically as
     the bihomogeneous coordinate ring of the
     blow up of the ideal I, used in resolution of singularities. Though the general
     case is still out of reach, we illustrate with some simple examples of plane
     curve singularities. 
     
     First the cusp in the affine plane
    Example
     R = ZZ/32003[x,y]
     cusp = ideal(x^2-y^3)
     mm = radical ideal singularLocus cusp
    Text
     The cusp is singular at the maximal ideal (x,y), so we blow that up,
     and examine the ``total transform'', that is, the ideal generated by the 
     x^2-y^3 in the Rees algebra. 
    Example
     B = first flattenRing reesAlgebra(mm)
    Text
     Application of {\tt first flattenRing} serves to make B a quotient of the polynomial
     ring T in 4 variables; otherwise it would be a quotient of R[w_0,w_1], which
     Macaulay2 treats as a polynomial ring in 2 variables, and the calculation of
     the singular locus later on would be wrong.
    Example
     vars B
     proj = map(B,R,{x,y})
     totalTransform = proj cusp
     D = decompose totalTransform 
     D/codim
    Text
     We see that the reduced preimage consists of two codimension 1 components,
     the `exceptional divisor', which is the pullback of the point we blew up, (x,y),
     and the `strict transform'.
     The two components meet in a double point in the 2 dimensional variety
     B \subset{} A^2\times P^1. We have to saturate with respect to the irrelevant
     ideal to understand what's going on.
    Example
     irrelB = ideal(B_0,B_1)
     doublePoint = saturate(D_0+D_1, irrelB)
     codim doublePoint
     degree doublePoint
    Text
     We can see the multiplicities of these components by comparing their degrees to the
     degrees of the reduced components
    Example
     divisors = primaryDecomposition totalTransform
     strictTransform = divisors_0
     exceptional = divisors_1
     divisors/(i-> degree i/degree radical i)
    Text
     That is, the exceptional component occurs with multiplicity 2 (in general we'd 
     get the exceptional component with multiplicity equal to the multiplicity of the
     singular point we blew up.)
     
     We next investigate the singularity of the strict transform. We want to see
     it as a curve in P^1 x A^2, that is, as an ideal of  T = kk[w_0,w_1,x,y]
    Example
     T = ring ideal B
     irrelT  = ideal(w_0,w_1)
     sing = saturate(ideal singularLocus strictTransform, irrelT)
    Text
     We see that the singular locus of the strict transform is empty; that is, the curve is smooth.

     We could have made the computation in B as well:
    Example
     jacobianMatrix = diff(vars B, transpose gens strictTransform)
     codim strictTransform
     jacobianIdeal  = strictTransform+ minors(1,jacobianMatrix)
     sing1 = saturate(jacobianIdeal, irrelB)

    Text
     Next we look at the desingularization of a tacnode; it will take two blowups.
    Example
     R = ZZ/32003[x,y]
     tacnode = ideal(x^2-y^4)
     sing = ideal singularLocus tacnode
     mm = radical sing
     B1 = first flattenRing reesAlgebra mm
     proj1 = map(B1,R,{x,y})
     irrelB1 = ideal(w_0,w_1)
     totalTransform1 = proj1 tacnode
     netList (D1 = decompose totalTransform1)
     strictTransform1 = saturate(totalTransform1,proj1 mm )
    Text
     Here proj1 mm is the ideal of the exceptional divisor. 
     The strict transform is, by definition, obtained by saturating it away,
     The strict transform of the tacnode is not yet smooth: it consists of
     two smooth branches, meeting transversely at a point:
    Example
     strictTransform1 == intersect(D1_1,D1_2)
     degree (D1_1+D1_2)
    Text
     We compute the singular point of the strict transform:
    Example
     mm1 = sub(radical ideal singularLocus strictTransform1, B1)
    Text
     ...and blow up B1, getting a variety in P^2 x P^1 x A^2
    Example
     B2 = first flattenRing reesAlgebra(mm1, Variable => p)
     vars B2
     proj2 = map(B2,B1,{w_0,w_1,x,y})
     irrelB2 = ideal(p_0,p_1,p_2)
     irrelTot = (proj2 irrelB1) *irrelB2
     totalTransform2 = saturate(proj2 proj1 tacnode, irrelTot)
     exceptional2 = saturate(proj2 proj1 mm, irrelTot)
     netList(D2 = decompose totalTransform2)
     netList(E2 = decompose exceptional2)
     strictTransform2 = saturate(totalTransform2, exceptional2)
    Text 
     We compute the singular locus once again:
    Example
     time sing2 = ideal singularLocus strictTransform2;
     saturate(sing2, sub(irrelTot, ring sing2))
    Text
     The answer, {\tt ideal 1} shows that the second blowup desingularizes the tacnode.
    Text
     It is not necessary to repeatedly blow up closed points: there is always a 
     single ideal that can be blown up to desingularize 
     (Hartshorne, Algebraic Geometry,Thm II.7.17).
     In this case, blowing-up (x,y^2) desingularizes the tacnode x^2-y^4 in a single step.
    Example
     R = ZZ/32003[x,y];
     tacnode = ideal(x^2-y^4);
     mm = ideal(x,y^2);
     B = first flattenRing reesAlgebra mm;
     irrelB = ideal(w_0,w_1);
     proj = map(B,R,{x,y});
     totalTransform = proj tacnode
     netList (D = decompose totalTransform)
     exceptional = proj mm
     strictTransform = saturate(totalTransform, exceptional);
     netList decompose strictTransform
     sing0 = sub(ideal singularLocus strictTransform, B);
     sing = saturate(sing0,irrelB)
    Text
     So this single blowup is already nonsingular.
 ///

///
  uninstallPackage "ReesAlgebra"
  restart
  installPackage "ReesAlgebra"
  check "ReesAlgebra"
  viewHelp ReesAlgebra
///

-----TESTS-----
TEST///
--TEST for jacobianDual
setRandomSeed 0
     d=2
     S = ZZ/101[a_0..a_(d-1)]
     kk = ZZ/101
     mlin = transpose vars S
     mquad = random(S^d, S^{-1,-4,d-2:-2})
     Irand = minors(d,mlin|mquad)
     X = vars S
     phi = syz gens Irand;
     psi = jacobianDual phi

     T = symbol T
     ST = kk[T_0..T_d, x_0..x_(d-1)] 
     X = matrix{toList(x_0..x_(d-1))}
     Ts = matrix{{T_0,T_1..T_d}}
     phi1 = (map(ST,S,X)) phi
     psi1 = jacobianDual(phi1, X, Ts)
     f = map(ST, ring psi, vars ST)
     assert(f psi - psi1 == 0)
     m = matrix {{-15*T_1-8*T_2, T_0*x_0^3+14*T_0*x_0*x_1^2-24*T_0*x_1^3+18*T_2},
      {T_0*x_0^3-16*T_0*x_0^2*x_1+2*T_0*x_0*x_1^2+32*T_0*x_1^3+45*T_1+40*T_2,
      -11*T_0*x_1^3-11*T_1+43*T_2}}
     f psi - m
///

TEST///
--test for expectedReesIdeal
     setRandomSeed 0
     n = 3
     S = ZZ/101[x_0..x_(n-2)];
     M1 = random(S^(n-1),S^{n-1:-2});
     M = M1||vars S
     I = minors(n-1, M);
     time rI = expectedReesIdeal I
     time rrI = reesIdeal I;
     time rrI = reesIdeal(I,I_0); -- ~20 sec
     assert(betti rrI == betti rI)     
///

///
restart
uninstallPackage "ReesAlgebra"
installPackage "ReesAlgebra"
check "ReesAlgebra"
///

TEST///
--TEST for versalEmbedding
p=3
S=ZZ/p[x,y,z]
R=S/((ideal(x^p,y^p))+(ideal(x,y,z))^(p+1))
i=module ideal(z)
ui=versalEmbedding i
assert(kernel ui == ideal(0_R))
inci=map(R^1,i,matrix{{z}})
assert(kernel inci == 0)
gi=map(R^2, i, matrix{{x},{y}})
assert(kernel gi == 0)
u= map(R^3,R^{-1},ui)
inc=map(R^1, R^{-1}, matrix{{z}})
g=map(R^2, R^{-1}, matrix{{x},{y}})
A=symmetricKernel u
B1=symmetricKernel inc
B=(map(ring A, ring B1)) B1
C1 = symmetricKernel g
C=(map(ring A, ring C1)) C1
assert((A==B)==true)
assert((A==C)==false)
///

--- A very basic tests of reesIdeal - a few more after this. 
TEST///
S=ZZ/101[x,y]
i=ideal"x5,y5, x3y2"
V1 = reesIdeal(i)
use ring V1
assert(V1 == ideal(x^2*w_1-y^2*w_2,y*w_1^2-x*w_0*w_2,x^3*w_0-y^3*w_1,x*w_1^3-y*w_0*w_2^2,w_1^5-w_0^2*w_2^3))
V2 = reesIdeal(i,i_0)
use ring V2
assert(V2 == ideal(x^2*w_1-y^2*w_2,y*w_1^2-x*w_0*w_2,x^3*w_0-y^3*w_1,x*w_1^3-y*w_0*w_2^2,w_1^5-w_0^2*w_2^3))
///

-- 3 very simple tests.  The first tests just reesIdeal, the second
-- just reesAlgebra and the third tests both. 
TEST///
S = ZZ/101[x,y]
M = module ideal(x,y)
V = reesIdeal M
use ring V
assert(V == ideal (-w_0*y+w_1*x))
use S
M = module (ideal(x,y))^2
R = reesAlgebra M
assert(numgens R + numgens coefficientRing R == 5)
use ambient R
assert(ideal R == ideal (-w_1*y+w_2*x, -w_0*y + w_1*x, w_1^2 - w_0*w_2))
use S
M = module (ideal (x,y))^3
V = reesIdeal M
use ring V
assert(V == ideal (-w_2*y+w_3*x,-w_1*y+w_2*x,-w_0*y+w_1*x,w_2^2-w_1*w_3,w_1*w_2-w_0*w_3,w_1^2-w_0*w_2))
R = reesAlgebra M
assert(numgens R + numgens coefficientRing R == 6)
use ambient R
assert(ideal R == ideal (-w_2*y+w_3*x,-w_1*y+w_2*x,-w_0*y+w_1*x,w_2^2-w_1*w_3,w_1*w_2-w_0*w_3,w_1^2-w_0*w_2))
///

--- Checking that the two methods for getting a Rees Ideal yields the
--- same answer.  This is now an example as well. 
TEST///
x = symbol x
S=ZZ/101[x_0..x_4]
i=monomialCurveIdeal(S,{4,5,6,7})
M1 = gens gb reesIdeal i; 
M2 = gens gb reesIdeal(i,i_0);
M1 = substitute(M1, ring M2);
assert(M2 == M1)
///

///
restart
loadPackage ("ReesAlgebra", Reload =>true)
S=ZZ/101[x_0..x_4]
i=monomialCurveIdeal(S,{5,8,9,11})
time M1 = gens gb reesIdeal i; 
time M2 = gens gb reesIdeal(i,i_0);
time M3 = gens gb reesIdeal(i,i_0, Strategy => Bayer);
time M4 = gens gb reesIdeal(i, Strategy => Bayer);
M1 = substitute(M1, ring M2);
M4 = substitute(M4, ring M2);
assert(M2 == M1)
assert(M2 == M4)

///


--- Testing analyticSpread
TEST ///
R=QQ[a,b,c,d,e,f]
M=matrix{{a,c,e},{b,d,f}}
assert(analyticSpread image M == 3)
///

---Testing specialFiberIdeal
TEST///
R=ZZ/23[a,b,c,d]
msq=ideal(a^2, a*b, b^2,a*c,b*c, c^2,a*d, b*d, c*d, d^2)
sfi=specialFiberIdeal(msq)
S=ring sfi
T=ZZ/23[S_0,S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9]
M=matrix{{S_0,S_1,S_3,S_6},{S_1,S_2,S_4,S_7},{S_3,S_4,S_5,S_8},{S_6,S_7,S_8,S_9}}
i=minors(2,M)
assert(sfi == i)
///


---Testing minimalReduction, isReduction, reductionNumber
TEST///
S = ZZ/5[x,y]
I = ideal(x^3,x*y,y^4)
J = ideal(x*y, x^3+y^4)
assert(isReduction(I,J)==true)
assert(isReduction(J,I)==false)
K= minimalReduction I
assert(reductionNumber(I,J)==1)
assert(isReduction(I,K)==true)
assert(reductionNumber(I,K)==1)
///
--testing multiplicity
TEST///
R=ZZ/101[x,y]
I = ideal(x^3, x^2*y, y^3)
assert(multiplicity I==9)
R = ZZ/101[x,y]/ideal(x^3-y^3)
I = ideal(x^2,y^2)
assert(multiplicity I==6)
///

--Testing which Gm
TEST///
kk=ZZ/101;
S=kk[a..c];
m=ideal vars S
i=(ideal"a,b")*m+ideal"c3"
assert(whichGm i==3)
///

TEST///
--Test for isLinearType
S = ZZ/101[x,y]
M = module ideal(x,y)
E = {true, false, false, false, false}
assert({true, false, false, false, false} == 
    for p from 1 to 5 list(isLinearType (ideal vars S)^p))
///

TEST///
--Associated Graded ring and Normal Cone very basic test
R=ZZ/23[x]
I=ideal(x)
A=associatedGradedRing I
S=ring ideal A
assert(dim S==2)
assert(codim A==1)
N=normalCone I
s=ring ideal N
assert(dim s==2)
assert(codim N==1)
///


TEST///
--Test for distinguished
R=ZZ/101[x,y,u,v]
I=ideal(x^2, x*y*u^2+2*x*y*u*v+x*y*v^2,y^2)
p = map(R/I,R)
assert(distinguished I == {{2, p ideal(x,y)}})
///

TEST///
kk = ZZ/101
S = kk[x,y]
I = ideal"x2y";J=ideal"xy2"
assert(intersectInP(I,J) == {{5, ideal (y, x)}, {2, ideal y}, {2, ideal x}})
I = ideal"y-x2";J=ideal y
assert(intersectInP(I,J) == {{2, ideal (y, x)}})
///

TEST///
--Test for symmetricAlgebraIdeal
R=ZZ/101[x,y,u,v]
I=ideal vars R
J = symmetricAlgebraIdeal I
S = ring J
m = promote(vars R,S)||vars S
assert(J == minors(2,m))
///

end--
restart
uninstallPackage "ReesAlgebra"
restart
installPackage "ReesAlgebra"
check "ReesAlgebra"
viewHelp ReesAlgebra

----

