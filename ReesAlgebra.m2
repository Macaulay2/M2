--------------------------------------------------------------------------
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
-- PROGRAMMERs : Rees algebra code written by David Eisenbud and
--               Amelia Taylor with some assistance from Sorin Popescu. 
-- UPDATE HISTORY : created 27 October 2006 
-- 	     	    updated 29 June 2008, and later
--
-- Missing documentation and most examples are now at the end of the file
-- waiting to be included in the documentation -- more fixes to come
---------------------------------------------------------------------------
newPackage(
	"ReesAlgebra",
    	Version => "1.1", 
    	Date => "October 30, 2009",
    	Authors => {{
		  Name => "David Eisenbud",
		  Email => "de@msri.org"},
	     {Name => "Amelia Taylor",
	     HomePage => "http://faculty1.coloradocollege.edu/~ataylor/",
   	     Email => "amelia.taylor@coloradocollege.edu"},
             {Name => "Sorin Popescu",
	      Email => "sorin@math.sunysb.edu"},
	 {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu"}},  
    	Headline => "Rees algebras",
    	DebuggingMode => false,
	Reload => true  
    	)

export{
  "analyticSpread", 
  "associatedGradedRing", 
  "distinguished",
  "distinguishedAndMult",
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
  "universalEmbedding",
  "whichGm",
  "Tries"
  }

-- Comment : The definition of Rees algebra used in this package is 
-- R(M) = Sym(M)/(intersection over g of L_g) where the intersection 
-- ranges over all maps from M to free R-modules and L_g is the kernel 
-- of Sym(g) (where Sym(g): Sym(M) -> Sym(R)).

-- For computation the key is that R(M) = R(f) where f is any map from 
-- from M to a free module F such that the dual map F^* -> M^* is surjective
-- and R(f) is the image of Sym(f).

 
-- PURPOSE : Compute the rees ring of the image of a 
--           a matrix regarded as over a quotient ring.  
-- INPUT : 'f' a matrix and 'I' and an ideal over a polynomial ring OR 
--         a module over a polynomial or quotient ring. 
-- OUTPUT : an Ideal defining the Rees algebra of the image of f regarded 
--          as a matrix over the ring of I mod I if f is a versal embedding 
--          For the module or if f is not a versal embedding it naively computes 
--          the defining ideal of the Rees algebra and the output may be correct 
--          and it may be nonsense.
-- COMMENT : If I is the zero ideal and f is the generators of an ideal then 
--           the ideal is this is the usual
--           defining ideal of the usual Rees Algebra. Otherwise f 
--           corresponds to the versal embedding as defined in Eisenbud, 
--           Huneke, and Ulrich.  Also the Module version just sets up 
--           the input to use symmetric Kernel on a Module.  See "OUTPUT" 
--           for caution.

--- Assumes we have a homogeneous (multi) map

symmetricKernel = method(Options=>{Variable => "w"})
symmetricKernel(Matrix) := Ideal => o -> (f) -> (
     if rank source f == 0 then return trim ideal(0_(ring f));
     w := o.Variable;
     if instance(w,String) then w = getSymbol w;
     S := symmetricAlgebra(source f, VariableBaseName => w);
     T := symmetricAlgebra target f;
     trim ker symmetricAlgebra(T,S,f))

-- PURPOSE: Computes the universal embedding of the 
--          image of f, or of M or of J over a quotient ring.  
-- INPUT : 'M' a matrix and 'I' an ideal defined over a polynomial ring.
-- OUTPUT : a map that is a versal embedding of the image of M over the 
--          ring of M mod I.  
-- COMMENT : The purpose is to compute a universal embedding to be used in 
--           symmetricKernel in order to compute a Rees Algebra in the most 
--           general case possible at this time as defined in Eisenbud, Huneke 
--           and Ulrich. 

universalEmbedding = method()
universalEmbedding(Ideal) :=
universalEmbedding(Module) := Matrix => (M) -> (
     if (class M) === Ideal then M = module M;
     UE := transpose syz transpose presentation M;
     map(target UE, M, UE)
     )

-- PURPOSE : Front end for computing the defining ideal of the Rees algebra 
--           of a module, or an ideal defined over a polynomial ring or a 
--           quotient ring.
-- INPUT : 'M' a module OR
--         'J' an ideal 
-- Options : The computation requires additional variables.  The user 
--           can use Variable => to specify the letter used for the 
--           new indexed variable.  The default is the letter w.  The 
--           default algorithm is symmetricKernel, but in the case of 
--           an ideal over a polynomial ring the user might want to use 
--           the algorithm in reesSaturate specified by Strategy => Saturate.
-- OUTPUT : an Ideal defining the Rees algebra of the module M or of the ideal J.
-- COMMENT : Uses proposition 1.3 in Eisenbud, Huneke, Ulrich and computes 
--           the rees algebra of a versal embedding of the 
--           Module regardless of the ring and for an ideal over a quotient ring. 
--           In the case of an ideal over a polynomial ring the process is slightly 
--           streamlined, skipping the unneccessary versal computation as in that 
--           case the inclusion map is a versal map.

reesIdeal = method(Options => {Variable => "w"})

fixupw = w -> if instance(w,String) then getSymbol w else w

reesIdeal(Module) := Ideal => o -> M -> (
     P := presentation M;
     UE := transpose syz transpose P;
     symmetricKernel(UE,Variable => fixupw o.Variable)
     )

reesIdeal(Ideal) := Ideal => o-> (J) -> (
     symmetricKernel(gens J, Variable => fixupw o.Variable)
     )

---- needs user-provided non-zerodivisor. 

reesIdeal (Module, RingElement) := Ideal =>
reesIdeal (Module, RingElement) := Ideal => o -> (M,a) -> (
     R := ring M;
     if R =!= ring a 
       then error("Expected Module and Element over the same ring");   
     P := presentation M;
     S := symmetricAlgebra(target P, VariableBaseName => fixupw o.Variable);
     I := ideal(vars S * promote(P,S));
     saturate(I,promote(a,S)))
reesIdeal(Ideal, RingElement) := Ideal => o -> (I,a) -> (
     reesIdeal(module I, a)
     )

reesAlgebra = method (TypicalValue=>Ring,Options=>{Variable => "w"})
-- accepts a Module, Ideal, or pair (depending on the method) and
-- returns the quotient ring isomorphic to the Rees Algebra rather
-- than just the defining ideal as in reesIdeal. 

reesAlgebra Ideal := 
reesAlgebra Module := o-> M ->  quotient reesIdeal(M, o)
--     R:=ring M;
--     reesIM := reesIdeal M;
--     reesAM := (ring reesIM)/reesIM;
--     --A:= map(reesAM, R);
--     --(reesAM,A)
--     reesAM
--     )

reesAlgebra(Ideal, RingElement) := 
reesAlgebra(Module, RingElement) := o->(M,a)-> quotient reesIdeal(M,a,o)
--     R:=ring M;
--     reesIM := reesIdeal(M,a);
--     reesAM := (ring reesIM)/reesIM;
--     --A:= map(reesAM, R);
--     --(reesAM,A)
--     reesAM
--     )
       
isLinearType=method(TypicalValue =>Boolean)

isLinearType(Ideal):=
isLinearType(Module):= M->(
     if class M === Ideal then M = module M;
     I := reesIdeal M;
     S := ring I;
     P := promote(presentation M, S);
     J := ideal((vars S) * P);
     ((gens I) % J) == 0)

isLinearType(Ideal, RingElement):=
isLinearType(Module, RingElement):= (M,a)->(
     if class M === Ideal then M = module M;
     I := reesIdeal(M,a);
     S := ring I;
     P := promote(presentation M, S);
     J := ideal((vars S) * P);
     ((gens I) % J) == 0)

normalCone = method(TypicalValue => Ring, Options => {Variable => "w"})
normalCone(Ideal) := o -> I -> (
     RI := reesAlgebra(I);
     RI/promote(I,RI)
     )
normalCone(Ideal, RingElement) := o -> (I,a) -> (
     RI := reesAlgebra(I,a);
     RI/promote(I,RI)     
     )
     
associatedGradedRing= method(TypicalValue => Ring, Options => {Variable => "w"})
associatedGradedRing (Ideal) := o -> I -> normalCone(I)
associatedGradedRing (Ideal, RingElement) := o -> (I,a) -> normalCone(I,a)
     

-- PURPOSE : Compute the multipicity e_I(M) and e(I) = e_I(R), 
--           the normalized leading coefficient of the corresponding 
--           associated graded ring.  
-- INPUT : 'I' an Ideal, for e(I) or 'I' and 'M' for e_I(M)
-- OUTPUT : the Hilbert-Samuel multiplicity
-- COMMENT : The associated graded ring is computed using the Rees algebra.
-- WARNING : Computing a quotient like R[It]/IR[It] requires a 
--           Groebner basis computation and thus can quickly take all of your
--           memory and time (most likely memory).   

multiplicity = method()
multiplicity(Ideal) := ZZ => I ->  (
     RI := normalCone I;
     J := ideal RI;
     J1 := first flattenRing J;
     S1 := newRing(ring J1, Degrees=>{numgens ring J1 : 1});
     degree substitute(J1,S1)
     )
multiplicity(Ideal,RingElement) := ZZ => (I,a) ->  (
     RI := normalCone (I,a);
     J := ideal RI;
     J1 := first flattenRing J;
     S1 := newRing(ring J1, Degrees=>{numgens ring J1 : 1});
     degree substitute(J1,S1)
     )

///
restart
load "ReesAlgebra.m2"

kk=ZZ/101
S=kk[x,y]
I = ideal(x^2, y^3)
assert (multiplicity I == 6)
use S
I=ideal(x^2, x*y+y^3)
assert(multiplicity I == 6)
use S
I = ideal(x^2+x^3)
assert(multiplicity I == 3)
normalCone I
use S
isLinearType I

use S
I = ideal((x+1)^2, (x+1)*(y+1)+(y+1)^3)
multiplicity I

use S
I = ideal"x3, x2y,y3"
multiplicity I
degree(S^1/I)

///

--Special fiber is here defined to be the fiber of the blowup over the
--subvariety defined by the vars of the original ring. Note that if the
--original ring is a tower ring, this might not be the fiber over the
--closed point! ***put this into the documentation!*** to get the closed
--fiber, flatten the base ring first. 

specialFiberIdeal=method(TypicalValue=>Ideal, Options=>{Variable=>"w"})

specialFiberIdeal(Ideal):= 
specialFiberIdeal(Module):= o->i->(
     Reesi:= reesIdeal(i, Variable=>fixupw o.Variable);
     trim(Reesi + promote(ideal vars ring i, ring Reesi))
     )
 
specialFiberIdeal(Ideal, RingElement):=
specialFiberIdeal(Module,RingElement):= o->(i,a)->(
     Reesi:= reesIdeal(i, a, Variable=>fixupw o.Variable);
     trim(Reesi + promote(ideal vars ring i, ring Reesi))     
     )

---------The following returns a ring with just the new vars.
--The order of the generators is supposed to be the same as the order
--of the given generators of I.
specialFiber=method(TypicalValue=>Ring, Options=>{Variable=>"w"})

specialFiber(Ideal):= 
specialFiber(Module):= o->i->(
     Reesi:= reesIdeal(i, Variable=>fixupw o.Variable);
     S := ring Reesi;
     --is the coefficient ring of Reesi automatically flattened? NO
     kk := ultimate(coefficientRing, S);
     T := kk[gens S];
     minimalpres := map(T,S);
     T/(minimalpres Reesi)
     )
 
specialFiber(Ideal, RingElement):=
specialFiber(Module,RingElement):= o->(i,a)->(
     Reesi:= reesIdeal(i, a, Variable=>fixupw o.Variable);
     S := ring Reesi;
     --is the coefficient ring of Reesi automatically flattened? NO
     kk := ultimate(coefficientRing, S);
     T := kk[gens S];
     minimalpres := map(T,S);
     T/(minimalpres Reesi)
     )

isReduction=method(TypicalValue=>Boolean, Options=>{Variable=>"w"})

--tests whether the SECOND arg is a reduction of the FIRST arg

isReduction(Module,Module):= 
isReduction(Ideal,Ideal):= o->(I,J)->(
     if isSubset(J, I) then (
	     Sfib:= specialFiber(I, Variable=>fixupw o.Variable);
	     Ifib:=ideal presentation Sfib;
	     kk := coefficientRing Sfib;
	     M := sub(gens J // gens I, kk);
	     M = promote(M, Sfib);
	     L :=(vars Sfib)*M;
	     0===dim ideal L)
     else false)

isReduction(Module,Module,RingElement):= 
isReduction(Ideal,Ideal,RingElement):= o->(I,J,a)->(
     if isSubset(J, I) then (
	     Sfib :=specialFiber(I, a, Variable=>fixupw o.Variable); 
	     Ifib:= ideal presentation Sfib;
	     kk := coefficientRing Sfib;
	     M := sub(gens J // gens I, kk);
	     M = promote(M, Sfib);
	     L :=(vars Sfib)*M; 
	     0===dim ideal L)
     else false)


///
####
restart
uninstallPackage "ReesAlgebra"
installPackage ("ReesAlgebra", FileName => "~/gitRepos/Workshop-2017-Berkeley/ReesAlgebras/ReesAlgebra.m2")


peek loadedFiles
S = kk[s,t][x,y,z]
I = ideal"x2, xy+y4"
--II=reesIdeal I
SF = specialFiber I
ideal presentation SF
J = minimalReduction I
(gens J) ==(gens I)*(gens J//gens I)
isReduction(I, J, I_0)

analyticSpread I

time tally for n from 1 to 100 list isReduction(I, minimalReduction I, I_0)

setRandomSeed(314159268)
scan({2,3,5,7,11,101,32003}, p ->(
	  kk=ZZ/p;
	  S = kk[a,b,c,d];
	  I = monomialCurveIdeal(S, {1,3,4});
	  T:=tally for n from 1 to 100 list isReduction(I, minimalReduction I);
	  print (p, T#false, T#true))
)	  

///

-- PURPOSE : Analytic spread of a module as defined in M2 by a matrix, 
--           a module or ideal over a quotient ring R/I.
-- INPUT : 'M' a module OR
--         'J' an ideal  
-- Options : The ring R can be a quotient ring, or, the user can define 
--           f, M or J over a polynomial ring R and an ideal I can be given 
--           as the option Strategy and the special fiber is then computed 
--           over the quotient ring R/I.
-- OUTPUT : The analytic spread of f/M/or J over over the ring R, or if 
--          the option is given, R/I.
analyticSpread = method()

analyticSpread(Ideal) := 
analyticSpread(Module) := ZZ => (M) -> dim specialFiberIdeal(M)

analyticSpread(Ideal,RingElement) :=
analyticSpread(Module,RingElement) := ZZ => (M,a) -> dim specialFiberIdeal(M,a)
 
----- distinguished and Mult still does not work!!!!!
   
--We can use this to compute the distinguished subvarieties of
--a variety (components of the support of the normal cone).
--In the following routine i must be an ideal of a polynomial ring, not a 
--quotient ring.

-- PURPOSE : Compute the distinguised subvarieties of a variety 
--           (components of the support of the normal cone).
-- INPUT : 'i' an ideal over a polynomial ring. 
-- OUTPUT : the components of the support of the normal cone of V(i).
-- COMMENT : I have a note stating that "right now" this computation 
--           requires a polynomial ring over a finite field - written 
--           in 2000/2001.  I have no idea why.  I suspect that at the 
--           time decompose required this.  But I think it is not necessary 
--           now. 

distinguished = method(Options => {Variable => "w"})
distinguished(Ideal) := List => o -> i -> (
     R:=ring i;
     reesAi := reesAlgebra (i,Variable=>fixupw o.Variable);
     A := map(reesAi,R);
     (T,B) := flattenRing reesAi;
     L:=decompose substitute(i,T);
     apply(L, p->kernel(map(T/p, T)*B*A))
     )

distinguished(Ideal,RingElement) := List => o -> (i,a) -> (
     R:=ring i;
     reesAi := reesAlgebra (i,a,Variable=>fixupw o.Variable);
     A := map(reesAi,R);
     (T,B) := flattenRing reesAi;
     L:=decompose substitute(i,T);
     apply(L, p->kernel(map(T/p, T)*B*A))
     )
     
-- PURPOSE : Compute the distinguised subvarieties of a variety  
--           (components of the support of the normal cone) WITH their 
--           multiplicities.
-- INPUT : 'i' an ideal over a polynomial ring. 
-- OUTPUT : ideals that are the components of the support of the normal 
--          cone of V(i) and integers that are their corresponding 
--          multiplicities.
-- CAVEAT: R must be a polynomial ring.


///

restart
installPackage "ReesAlgebra"
loadPackage "ReesAlgebra"
T=ZZ/101[c,d]
D = 4
P = product(D, i -> random(1,T))
R = ZZ/101[a,b,c,d]
I = ideal(a^2, a*b*(substitute(P,R)), b^2)
ass I -- there is one minimal associated prime (a thick line in PP^3) and D embedded primes (points on the line) 
primaryDecomposition I
distinguished I -- only the minimal prime is a distinguished component


K = distinguishedAndMult(I) -- get multiplicity 2 
J=intersect apply(K, i-> i_1^(i_0)) -- checks the Geometric Nullstellensatz on Ein-Lazarsfeld
(gens J)% (gb I)

R=ZZ/32003[x,y,z]
I=intersect(ideal(x),(ideal(x,y))^2, (ideal(x,y,z))^3)
ass I
decompose top intersect(ideal(x), ideal(y,z))

distinguished  I
K = distinguishedAndMult I
intersect apply(K, i-> i_1^(i_0)) 

viewHelp top


///

distinguishedAndMult = method(Options => {Variable => "w"})
distinguishedAndMult(Ideal) := List => o -> i -> (
    R:=ring i;
    ReesI := reesIdeal( i, Variable => fixupw o.Variable);
    (S,toFlatS) := flattenRing ring ReesI;
     I:=(toFlatS ReesI)+substitute(i,S);
     L:=decompose I;
     apply(L,P->(Pcomponent := I:(saturate(I,P)); 
	       --the P-primary component. The multiplicity is
	       --computed as (degree Pcomponent)/(degree P)
       	  {(degree Pcomponent)/(degree P), kernel(map(S/P, R))})))

distinguishedAndMult(Ideal,RingElement) := List => o -> (i,a) -> (
    R:=ring i;
    ReesI := reesIdeal( i,a, Variable => fixupw o.Variable);
    (S,toFlatS) := flattenRing ring ReesI;
     I:=(toFlatS ReesI)+substitute(i,S);
     L:=decompose I;
     apply(L,P->(Pcomponent := I:(saturate(I,P)); 
	       --the P-primary component. The multiplicity is
	       --computed as (degree Pcomponent)/(degree P)
       	  {(degree Pcomponent)/(degree P), kernel(map(S/P, R))})))
 


minimalReduction = method(Options=>{Tries => 20})
minimalReduction Ideal := Ideal => o -> i -> (
     --if the following is true, then we can compute the minimal reduction homogeneously
     h := isHomogeneous i and (d:=min degrees i) == max degrees i;
     S:=ring i;
     ell:=analyticSpread i;
     
     J:=null;
     --o.Tries defaults to 20
     for b from 1 to o.Tries do(
     
     if h then
          J = ideal ((gens i)*random(source gens i, (ring i)^{ell:-d})) 
     else 
     	  J = ideal (map(S^1, S^(rank source gens i), gens i)*random(S^(rank source gens i), S^ell));
	  
     if isReduction(i,J) then (
--	  if b>1 then print b; 
	  return J));

     {* << bound *} -- incorrect code commented out
     <<" iterations were not enough to randomly find a minimal reduction"; endl;
     error("not random enough")
          )

///
restart
load "ReesAlgebra.m2"

kk=ZZ/2
S = kk[a,b,c,d];
I = monomialCurveIdeal(S, {1,3,4});

I = ideal random(S^4, S^{5:-2})
J = minimalReduction I

time reductionNumber(I,J)
time reductionNumber(I,J, "1")

time for n from 1 to 100 do(m=m+1; minimalReduction I)

scan({2,3,5,7,11,101,32003}, p ->(
	  kk=ZZ/p;
	  S = kk[a,b,c,d];
	  I = monomialCurveIdeal(S, {1,3,4});
	  T:=tally for n from 1 to 100 list isReduction(I, minimalReduction I);
	  print (p, T#false, T#true))
)	  

///

reductionNumber = method()

///
--The following seems to  be much slower!
reductionNumber (Ideal, Ideal, String) := (i,j,s) -> (
     Ifib := specialFiber(I);
     Pfib := (ring Ifib); -- ambient ring of the fiber
     n := numgens Pfib;
     kk := coefficientRing Pfib;
     M := sub(gens J // gens I, kk);
     M = promote(M, Pfib);
     L := (vars Pfib)*M; 
     regularity (Pfib^1/ideal(leadTerm(Ifib+ideal L)))
	  )
///
reductionNumber (Ideal,Ideal) := (i,j) -> (
     I:=i; -- will be a power of i
     M:= ideal vars ring i; -- we're pretending to be in a local ring
     rN:=0;
     if isHomogeneous j then (
     	  while not ((gens I)%j)==0 do (
	       I = trim (i*I);
	       j= trim(i*j);
     	       rN =rN+1))
     else(
     	  while not ((gens I)%(j+M*I))==0 do (
	       I = trim(i*I);
	       j= i*j;
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
 
jacobianDual = method()
jacobianDual Matrix := phi ->(
    t := numrows phi;
    T := symbol T;
    S := ring phi;
    ST := S[T_0..T_(t-1)];
    X := vars ring phi;
    Ts :=  vars ST;
    jacobianDual(phi,X,Ts)
    )

jacobianDual(Matrix,Matrix, Matrix) := (phi,X,T) -> (
    --If phi is an m x n matrix over R, then T = matrix{{T_1..T_m}}
    -- should be a 1 x m over variables over R[T_1..T_m].
    --ideal X \subset R should contain the entries of the matrix phi.
    --the routine returns a matrix psi over ring T such that 
    --T phi = X psi.
    --Thus psi is a Jacobian dual of phi.
    ST := ring T;
    toST := map(ST,ring phi);--,gens ring phi|toList(numcols T:0_ST));
    if numcols T != numrows phi then error"if phi has m rows then T must have m cols.";
    XST := X;
    if ring XST =!= ST then XST = toST X;
    phiS := phi;
    if ring phi =!= ST then phiST := toST phi;
    psi := (T * phiST)//XST;
    --check that this worked:
    assert(T*phiST == XST*psi);
    psi
    )
///

restart
uninstallPackage "ReesAlgebra"
installPackage "ReesAlgebra"
--viewHelp reesAlgebra

kk = ZZ/101
d = 3
S = kk[x_0..x_(d-1)]
mlin = transpose vars S
mquad = random(S^d, S^{-1,-4,d-2:-2})
Irand = minors(d,mlin|mquad)
X = vars S
phi = syz gens Irand;
psi = jacobianDual phi


--ST = kk[x_0..x_(d-1), T_0..T_3] -- or
ST = S[T_0..T_3]
Ts = matrix{{T_0,T_1,T_2,T_3}}
STS = map(ST,ring psi,toList(T_0..T_3)|toList(x_0..x_(d-1)))
psi1 = jacobianDual(phi, X, Ts)
(STS psi) - psi1 == 0
///

beginDocumentation()
debug SimpleDoc

doc ///
   Key
    jacobianDual    
   Headline
    computes the ``jacobian dual'', part of a method of finding generators for Rees Algebra ideals
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
     the `Jacobian Dual' ; satisfies T*phi = X*psi
   Description
    Text
     Let I be an ideal of R and let phi be the presentation matrix of I as a module.
     The symmetric algebra of I has the form 
     Sym_R(I) = R[T_0..T_m]/ideal(T*phi)
     where the T_i correspond to the generators of I. If X = matrix{{x_1..x_n}},
     with x_i \in R, and ideal X contains the entries of the matrix phi, then there is 
     a matrix psi, called the Jacobian Dual of phi with respect to X,
     defined over R[T_0..T_m] such that T*phi = X*psi (the matrix psi is generally
     not unique; Macaulay2 computes it using Groebner division with remainer.
 
     The name Jacobian Dual comes from the case where phi is a matrix of linear forms
     the x_i are the variables of R, and the generators of I are forms, all of the same degree D;
     in this case Euler's formula sum(df_i/dx_j*xj) = Df can be used to express the
     entries of psi in terms of the derivatives of the entries of phi, at least when
     D is nonzero in the coefficient field.  The division with
     remainder is usually fast, but if this
     ever becomes a bottleneck it would be possible to test for the degree condition and
     use Euler's formula.
     
     If I is an ideal of grade >=1 and ideal X contains a nonzerodivisor of R
     (which will be automatic if I has finite projective dimension) then
     ideal X has grade >= 1 on the Rees algebra. Since ideal(T*phi) is contained in the
     defining ideal of the Rees algebra, the vector X is annihilated by the matrix
     psi when regarded over the Rees algebra. If also the number of relations of I
     is >= the number of generators of I, this implies that the maximal minors of
     psi annihilate  the x_i as elements of the Rees algebra, and thus that the maximal
     minors of psi are inside the ideal of the Rees algebra. In very favorable circumstances,
     one may even have 
     
     reesIdeal I = ideal(T*phi)+ideal minors(psi).
     
    Example
     d=3
     S = ZZ/101[a_0..a_(d-1)]
   SeeAlso
    reesAlgebra
    specialFiberIdeal
///

 
beginDocumentation()
debug SimpleDoc

doc ///
  Key
    ReesAlgebra
  Headline
    Compute Rees algebra
  Description
    Text
       The goal of this package is to provide commands to compute the Rees
       algebra of a module as it is defined in the paper {\em What is the
       Rees algebra of a module?}, by Craig Huneke, David Eisenbud and Bernd
       Ulrich, Proc. Am. Math. Soc. 131, 701--708, 2002.
       It also includes functions for computing many of the structures
       that require a Rees algebra.  The included functions are listed
       below. Examples of the use of each of the functions are included with
       their documentation.
///

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
      defining the Rees ring of {\tt f}
  Description
   Text
     Given a map between free modules $f: F \to G$ this function computes the
     kernel of the induced map of symmetric algebras, $Sym(f): Sym(F) \to
     Sym(G)$ as an ideal in $Sym(F)$.  When $f$ defines the universal embedding
     of $Im f$, or when $G$ is the ground ring, then (by results in the paper
     of Huneke-Eisenbud-Ulrich) this is equal to the defining ideal of the Rees
     algebra of the module Im f.

     This function is the workhorse of all/most of the Rees algebra 
     functions in the package.  Most users will prefer to use one of the front 
     end commands @TO reesAlgebra@, @TO reesIdeal@ and others.
   Example
     R = QQ[a..e]
     J = monomialCurveIdeal(R, {1,2,3})
     symmetricKernel (gens J)
   Text
     Let {\tt I} be the ideal returned and let {\tt S} be the ring it lives in 
    (also printed), then {\tt S/I} is isomorphic to 
    the Rees algebra {\tt R[Jt]}.  We can get the same information
    above using {\tt reesIdeal(J)}, see @TO reesIdeal@.  {\bf The following is no
    longer correct!}.  Also 
    note that {\tt S} is multigraded allowing Macaulay2 to correctly 
    see that the variables of {\tt R} now live in degree 0 and the new variables 
    needed to describe {\tt R[Jt]} as a {\tt k}-algebra are in degree 1.
   Example
     S = ring oo;
     (monoid S).Options.Degrees
   Text
     {\tt symmetricKernel} can also be computed over a quotient ring.
   Example
     R = QQ[x,y,z]/ideal(x*y^2-z^9)
     J = ideal(x,y,z)
     symmetricKernel(gens J)
   Text
     The many ways of working with this function allows the system 
     to compute both the classic Rees algebra of an ideal over a ring 
     (polynomial or quotient) and to compute the the Rees algebra of a 
     module or ideal using a universal embedding as described in the paper 
     of Eisenbud, Huneke and Ulrich.  It also allows different ways of 
     setting up the quotient ring.
  SeeAlso
     reesIdeal
     reesAlgebra
     universalEmbedding
///

doc ///
  Key
    [symmetricKernel, Variable]
    [reesIdeal, Variable]
    [reesAlgebra, Variable]
    [normalCone, Variable]
    [associatedGradedRing, Variable]
    [specialFiberIdeal, Variable]
    [specialFiber, Variable]
    [distinguished, Variable]
    [distinguishedAndMult, Variable]
    [isReduction, Variable]
  Headline
    Choose name for variables in the created ring
  Usage
    symmetricKernel(...,Variable=>w)
    reesIdeal(...,Variable=>w)
    reesAlgebra(...,Variable=>w)
    normalCone(...,Variable=>w)
    associatedGradedRing(...,Variable=>w)
    specialFiberIdeal(...,Variable=>w)
    specialFiber(...,Variable=>w)    
    distinguished(...,Variable=>w)
    distinguishedAndMult(...,Variable=>w)
    isReduction(...,Variable=>w)
  Description
    Text
      Each of these functions creates a new ring of the form R[w_0, \ldots, w_r]
      or R[w_0, \ldots, w_r]/J, where R is the ring of the input ideal or module
      (except for @TO specialFiber@, which creates a ring $K[w_0, \ldots, w_r]$, 
      where $K$ is the ultimate coefficient ring of the input ideal or module.)
      This option allows the user to change the names of the new variables in this ring.
      The default variable is {\tt w}.
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
    universalEmbedding
    (universalEmbedding,Ideal)
    (universalEmbedding,Module)
  Headline
    Compute the universal embedding
  Usage
    u = universalEmbedding M
  Inputs
    M:Module
      or @ofClass Ideal@
  Outputs
    u:Matrix
      defining the universal embedding of the {\tt R}-module {\tt M}
      into a free {\tt R}-module.
  Description
   Text
      Suppose that M has free presentation $F\to G$.  universalEmbedding
      provides the universal map from the input module M into a free module H
      over the same ring, written as a map $u:M \to H$, such that any map from
      $M$ to a free $R$-module, factors uniquely through $u$.  Let $u1$ be the
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
      universalEmbedding M
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
      universalEmbedding(N)
   Text

      Here is an example from the paper "What is the Rees Algebra of a
      Module" by David Eisenbud, Craig Huneke and Bernd Ulrich,
      Proc. Am. Math. Soc. 131, 701--708, 2002.  The example shows that one
      cannot, in general, define the Rees algebra of a module by using *any*
      embedding of that module, even when the module is isomorphic to an ideal;
      this is the reason for using the map provided by the routine
      universalEmbedding. Note that the same paper shows that such problems do
      not arise when the ring is torsion-free as a ZZ-module, or when one takes
      the natural embedding of the ideal into the ring.
   Example
      p = 3;
      S = ZZ/p[x,y,z];
      R = S/((ideal(x^p,y^p))+(ideal(x,y,z))^(p+1))
      I = module ideal(z)
   Text
      As a module (or ideal), $Hom(I,R^1)$ is minimally generated by 3 elements,
      and thus the universal embedding of $I$ into a free module is into $R^3$.
   Example
      betti Hom(I,R^1)
      ui = universalEmbedding I
   Text
      it is injective:
   Example
      kernel ui
   Text
      It is easy to make two other embeddings of $I$ into free modules. First,
      the natural inclusion of $I$ into $R$ as an ideal is
   Example
      inci = map(R^1,I,matrix{{z}})
      kernel inci
   Text
      and second, the map defined by multiplication by x and y. 
   Example
      gi = map(R^2, I, matrix{{x},{y}})
      kernel gi
   Text
      We can compose ui, inci and gi with a surjection R--> i to get maps
      u:R^1 --> R^3, inc: R^1 --> R^1 and g:R^1 --> R^2 having image i
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
    (reesIdeal, Module)
    (reesIdeal,Ideal, RingElement)
    (reesIdeal,Module, RingElement)
  Headline
    compute the defining ideal of the Rees Algebra
  Usage
    reesIdeal M
    reesIdeal(M,f)
  Inputs
    M:Module
      or @ofClass Ideal@ of a quotient polynomial ring $R$
    f:RingElement
      any non-zero divisor modulo the ideal or module.  Optional
  Outputs
    :Ideal
      defining the Rees algebra of M
  Description
    Text
      The Rees algebra of a module $M$ over a ring $R$ is here defined,
      following the paper What is the Rees algebra of a module? David Eisenbud,
      Craig Huneke and Bernd Ulrich, Proc. Amer. Math. Soc. 131 (2003)
      701--708, as follows: If $h:F\to M$ is a surjection from a free module,
      and $g: M\to G$ is the universal map to a free module, then the Rees
      algebra of $M$ is the image of the induced map of $Sym(gh): Sym(F)\to
      Sym(G)$, and thus can be computed with symmetricKernel(gh). The paper
      above proves that if $M$ is isomorphic to an ideal with inclusion $g:
      M\to R$ (or, in characteristic zero but not in characteristic $>0$ if $M$
      is a submodule of a free module and $g': M\to G$) is any injection), then
      the Rees algebra is equal to the image of $g'h$, so it is unnecessary to
      compute the universal embedding.

      This package gives the user a choice between two methods for finding the
      defining ideal of the Rees algebra of an ideal or module $M$ over a ring
      $R$: The call

        {\tt reesIdeal(M)}

      computes the universal embedding $g: M\to G$ and a surjection $f: F\to M$
      and returns the result of symmetricKernel(gf). On the other hand, if the
      user knows an non-zerodivisor $a\in R$ such that $M[a^{-1}$ is a free
      module (this is the case, for example, if $a \in M\subset R$ and $a$ is a
      non-zerodivisor), then it is often much faster to call

        {\tt reesIdeal(M,a)}

      which finds a surjection $f: F\to M$ and returns $(J:a^{\infty}) \subset
      Sym(F)$, the saturation of the ideal $J:=(ker f)Sym(F)$. Note that this
      gives the correct answer even under the slightly weaker hypothesis that
      $M[a^{-1}]$ is ``of linear type''. (See also @TO isLinearType@.)

      {\bf Historical Background}: The Rees Algebra of an ideal is the basic
      commutative algebra analogue of the blow up operation in algebraic
      geometry. It has many applications, and a great deal of modern work in
      commutative algebra has been devoted to it.  The term ``Rees Algebra'' (of
      an ideal $I$ in a ring $R$, say) is used here to refer to the ring
      $R[It]\subset R[t]$ which is sometimes called the ``blowup algebra''
      instead. (The origin of the name may be traced to a paper by David Rees
      ({\em On a problem of Zariski}, Illinois J. Math. (1958) 145-149), where Rees
      used the ring $R[It,t^{-1}]$, now also called the ``extended Rees
      Algebra.'')
   Example
      kk = ZZ/101;
      S=kk[x_0..x_4];
      i=monomialCurveIdeal(S,{2,3,5,6})
      time V1 = reesIdeal i; 
      time V2 = reesIdeal(i,i_0); 
   Text
      This example is particularly interesting upon a bit more
      exploration.
   Example
      numgens V1
      numgens V2
   Text
      The difference is striking and, at least in part, explains the
      difference in computing time.  Furthermore, if we compute a Grobner
      basis for both and compare the two matrices, we see that we indeed got
      the same ideal.
   Example
      M1 = gens gb V1;
      M2 = gens gb V2;
      use ring M2
      M1 = substitute(M1, ring M2);
      M1 == M2
      numgens source M2
   Text
      Another example illustrates the power and usage of the code.  We
      also show the output in this example.  While a bit messy, the
      user can see how we handle the degrees in both cases.  
   Example
      S=kk[a,b,c]
      m=matrix{{a,0},{b,a},{0,b}}
      i=minors(2,m)
      time reesIdeal i
      res i
      m=random(S^3,S^{4:-1})
      i=minors(3,m)
      time I=reesIdeal (i,i_0);
      transpose gens I
      i=minors(2,m);
      time I=reesIdeal (i,i_0);
   Text
      {\bf Investigating plane curve singularities}
   Example
      R = ZZ/32003[x,y,z]
      I = ideal(x,y)
      cusp = ideal(x^2*z-y^3)
      RI = reesIdeal(I)
      S = ring RI
      totalTransform = substitute(cusp, S) + RI
      D = decompose totalTransform -- the components are the proper transform of the cuspidal curve and the exceptional curve 
      totalTransform = first flattenRing totalTransform
      L = primaryDecomposition totalTransform 
      apply(L, i -> (degree i)/(degree radical i))
   Text
      The total transform of the cusp contains the exceptional with
      multiplicity two.  The proper transform of the cusp is a smooth curve but
      is tangent to the exceptional curve.
   Example
      use ring L_0
      singular = ideal(singularLocus(L_0));
      SL = saturate(singular, ideal(x,y,z));
      saturate(SL, ideal(w_0,w_1)) -- we get 1 so it is smooth.
  Caveat
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
    compute the defining ideal of the Rees Algebra
  Usage
    reesAlgebra M
    reesAlgebra(M,f)
  Inputs
    M:Module
      or @ofClass Ideal@ of a quotient polynomial ring $R$
    f:RingElement
      any non-zero divisor modulo the ideal or module.  Optional
  Outputs
    :Ring
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
      S = QQ[x_0..x_4]
      i = monomialCurveIdeal(S,{2,3,5,6})
      time I = reesIdeal i;
      reesIdeal(i, Variable=>v)
      time I=reesIdeal(i,i_0);
      time (J=symmetricKernel gens i);
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
     is a module of linear type
  Usage
     isLinearType M
     isLinearType(M,f)
  Inputs
     M:Module
       or @ofClass Ideal@
     f:RingElement
       an optional element, which is a non-zerodivisor modulo {\tt M} and the ring of {\tt M}
  Outputs
     :Boolean
       true if {\tt M} is of linear type, false otherwise
  Description
   Text
     A module or ideal $M$ is said to be ``of linear type'' if the natural map
     from the symmetric algebra of $M$ to the Rees algebra of $M$ is an
     isomorphism. It is known, for example, that any complete intersection
     ideal is of linear type.

     This routine computes the @TO reesIdeal@ of {\tt M}.  Giving the element {\tt
     f} computes the @TO reesIdeal@ in a different manner, which is sometimes
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
     is a reduction
  Usage
     t=isReduction(I,J)
     t=isReduction(I,J,f)
  Inputs
     I:Ideal
     J:Ideal
     f:RingElement
       an optional element, which is a non-zerodivisor modulo {\tt J} 
       which is a member of {\tt I}.
  Outputs
     t:Boolean
       true if {\tt J} is a reduction of {\tt I}, false otherwise
  Description
   Text
    For an ideal $I$, a subideal $J$ of $I$ is said to be a {\bf reduction}
    of $I$ if there exists a nonnegative integer {\tt n} such that 
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
    the normal cone of a subscheme
  Usage
    normalCone I
    normalCone(I,f)
  Inputs
    I:Ideal
    f:RingElement
      optional argument, if given it should be a non-zero divisor in the ideal {\tt I}
  Outputs
    :Ring
      the ring $R[It] \otimes R/I$ of the normal cone of $I$
  Description
    Text
      The normal cone of an ideal $I\subset R$ is the ring $R/I \oplus I/I^2
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
    associatedGradedRing
    (associatedGradedRing, Ideal)
    (associatedGradedRing, Ideal, RingElement)
  Headline
    the associated graded ring of an ideal
  Usage
    associatedGradedRing I
    associatedGradedRing(I,f)
  Inputs
    I:Ideal
    f:RingElement
      optional argument, if given it should be a non-zero divisor in the ideal {\tt I}
  Outputs
    :Ring
      the associated graded ring $R[It] \otimes R/I$
  Description
   Text
    associatedGradedRing is a synonym for @TO normalCone@.
  SeeAlso
    reesAlgebra
    normalCone
    "tangentCone"
///



doc ///
  Key
    multiplicity
    (multiplicity, Ideal)
    (multiplicity, Ideal, RingElement)
  Headline
     compute the Hilbert-Samuel multiplicity of an ideal
  Usage
     multiplicity I
     multiplicity(I,f)
  Inputs
    I:Ideal
    f:RingElement
      optional argument, if given it should be a non-zero divisor in the ideal {\tt I}
  Outputs
    :ZZ
      the normalized leading coefficient of the Hilbert-Samuel polynomial of $I$
  Description
   Text
     Given an ideal $I\subset R$, ``multiplicity I'' returns the degree of the
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
  SeeAlso
///

doc ///
  Key
    specialFiberIdeal
    (specialFiberIdeal, Module)
    (specialFiberIdeal, Ideal)
    (specialFiberIdeal, Module, RingElement)
    (specialFiberIdeal, Ideal, RingElement)
  Headline
     special fiber of a blowup     
  Usage
     specialFiberIdeal M
     specialFiberIdeal(M,f)
  Inputs
     M:Module
       or @ofClass Ideal@
     f:RingElement
       an optional element, which is a non-zerodivisor such that $M[f^{-1}]$ is a free module when $M$ is a module, an element in $M$ when $M$ is an ideal
  Outputs
     :Ideal
  Description
   Text
     Let $M$ be an $R = k[x_1,\ldots,x_n]/J$-module (for example an ideal), and
     let $mm=ideal vars R = (x_1,\ldots,x_n)$, and suppose that $M$ is a
     homomorphic image of the free module $F$. Let $T$ be the Rees algebra of
     $M$. The call specialFiberIdeal(M) returns the ideal $J\subset Sym(F)$
     such that $Sym(F)/J \cong T/mm*T$; that is, specialFiberIdeal(M) =
     reesIdeal(M)+mm*Sym(F).

     The name derives from the fact that $Proj(T/mm*T)$ is the special fiber of
     the blowup of $Spec R$ along the subscheme defined by $I$.
   Example
     R=QQ[a..h]
     M=matrix{{a,b,c,d},{e,f,g,h}}
     analyticSpread minors(2,M)
     specialFiberIdeal minors(2,M)
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
  Headline
     special fiber of a blowup     
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
     $M$. The call specialFiber(M) returns the ideal $J\subset k[w_0,\dots,w_m]$
     such that $k[w_0,\dots,w_m]/J \cong T/mm*T$; that is, specialFiber(M) =
     reesIdeal(M)+mm*Sym(F). This routine differs from @TO specialFiberIdeal@ in that
     the ambient ring of the output ideal is $k[w_0,\dots,w_m]$ rather than
     $R[w_0,\dots,w_m]$. The coefficient ring $k$ used is always the @TO ultimate@
     @TO coefficientRing@ of $R$.
     
     The name derives from the fact that $Proj(T/mm*T)$ is the special fiber of
     the blowup of $Spec R$ along the subscheme defined by $I$.
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
     compute the analytic spread of a module or ideal
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
    (distinguished, Ideal)
    (distinguished, Ideal, RingElement)
  Headline
     compute the distinguished subvarieties of a scheme
  Usage
     distinguished I
     distinguished(I,f)
  Inputs
    I:Ideal
    f:RingElement
      optional argument, if given it should be a non-zero divisor in the ideal {\tt I}
  Outputs
    :List
  Description
   Text
     Let $I\subset R$ be an ideal in a ring $R$, the image of a free $R$-module
     $F$. Let $ReesI$ be the Rees algebra of $I$.  Certain of the minimal
     primes of $I$ are distinguished from the point of view of intersection
     theory: These are the ones that correspond to primes $P\subset ReesI$
     minimal among those containing $I*ReesI$---in other words, the isolated
     components of the support of the normal cone of $I$. The prime $p$
     corresponding to $P$ is simply the kernel of the the induced map $R \to
     ReesI/P$.

     Each of these primes comes equipped with a multiplicity, which may be
     computed as the ratio $degree(ReesI/P)/degree(R/p)$.

     For these matters and their significance, see section 6.1 of the book
     ``Intersection Theory,'' by William Fulton, and the references there,
     along with the paper

     ``A geometric effective Nullstellensatz.''
     Invent. Math. 137 (1999), no. 2, 427--448 by
     Ein and Lazarsfeld.

   Example
     T = ZZ/101[c,d];
     D = 4;
     P = product(D, i -> random(1,T))
     R = ZZ/101[a,b,c,d]
     I = ideal(a^2, a*b*(substitute(P,R)), b^2)
   Text
     There is one minimal associated prime (a thick line in $P^3$) and $D$
     embedded primes (points on the line).
   Example
     ass I 
     primaryDecomposition I
   Text
     Only the minimal prime is a distinguished component, and it has multiplicity 2.
   Example
     distinguished(I)
     K = distinguishedAndMult(I)
  SeeAlso
    distinguishedAndMult
///

doc ///
  Key
    distinguishedAndMult
    (distinguishedAndMult, Ideal)
    (distinguishedAndMult, Ideal, RingElement)
  Headline
     compute the distinguished subvarieties of a scheme along with their multiplicities
  Usage
     distinguishedAndMult I
     distinguishedAndMult(I,f)
  Inputs
    I:Ideal
    f:RingElement
      optional argument, if given it should be a non-zero divisor in the ideal {\tt I}
  Outputs
    :List
      of pairs ${d,P}$, where $d$ is the multiplicity of $I$ along the prime ideal $P$,
      where $P$ is the ideal of a distinguished subvariety of the normal cone of $I$
  Description
   Text
     See @TO distinguished@ for the definition of distinguished subvarieties, and for an example.
  SeeAlso
    distinguished
///

doc ///
  Key
    minimalReduction
    (minimalReduction, Ideal)
  Headline
    minimal reduction of an ideal
  Usage
    J = minimalReduction I
  Inputs
    I:Ideal
  Outputs
    :Ideal
      A minimal reduction of I (defined below)
  Description
    Text
      {\tt minimalReduction} takes an ideal {\tt I} that is homogeneous or inhomogeneous
      (in the latter case the ideal is to be regarded as an ideal in the 
      localization of the polynomial ring at the origin.). It returns an ideal $J$
      contained in $I$, with a minimal number of generators (= analyticSpread $I$),
      such that $I$ is integrally dependent on $J$. 
     
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
      it's generated by 3=analyticSpread i random linear combinations
      of the generators of i.
      There is apparently no homogeneous ideal with just 3 generators
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
    reduction number of one ideal with respect to another
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
      reductionNumber takes a pair of ideals $I,J$, homogeneous or inhomogeneous
      (in the latter case the ideal is to be regarded as an ideal in the 
      localization of the polynomial ring at the origin.).
      The ideal $J$ must be a reduction of $I$ (that is, $J\subset I$
      and $I$ is integrally dependent on $J$. This condition is checked by
      the function @TO isReduction@. It returns the smallest integer $k$ such that
      $JI^k = I^{k+1}$.
      
      See the book 
      Huneke, Craig; Swanson, Irena: Integral closure of ideals, rings, and modules. 
      London Mathematical Society Lecture Note Series, 336. Cambridge University Press, Cambridge, 2006.
      for further information.
    Example
      setRandomSeed()
      kk = ZZ/101;
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
    largest Gm satisfied by an ideal
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
      
      The call {\tt whichGm I}
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

TEST///
--TEST for universalEmbedding
p=3
S=ZZ/p[x,y,z]
R=S/((ideal(x^p,y^p))+(ideal(x,y,z))^(p+1))
i=module ideal(z)
ui=universalEmbedding i
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
assert(V1 == ideal(-w_0*y^2+w_2*x^2,w_0*w_1*x-w_2^2*y,w_1*x^3-w_2*y^3,-w_0^2*w_1*y+w_2^3*x,w_0^3*w_1^2-w_2^5))
V2 = reesIdeal(i,i_0)
use ring V2
assert(V2 == ideal(-w_0*y^2+w_2*x^2,w_0*w_1*x-w_2^2*y,w_1*x^3-w_2*y^3,-w_0^2*w_1*y+w_2^3*x,w_0^3*w_1^2-w_2^5))
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
i=monomialCurveIdeal(S,{2,3,5,6})
time M1 = gens gb reesIdeal i; 
time M2 = gens gb reesIdeal(i,i_0);
use ring M2
M1 = substitute(M1, ring M2);
assert(M2 == M1)
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
j=i+ideal(a,b,c,d)
assert(sfi==j)
sf=specialFiber(msq)
isf= ideal sf
S=ring isf
T=ZZ/23[S_0,S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9]
M=matrix{{S_0,S_1,S_3,S_6},{S_1,S_2,S_4,S_7},{S_3,S_4,S_5,S_8},{S_6,S_7,S_8,S_9}}
i=minors(2,M)
ideal sf ==i
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
--For isLinearType
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

end

restart
path=prepend("Users/Whitney/Workshop-2017-Berkeley/ReesAlgebras",path)
uninstallPackage "ReesAlgebra"
installPackage "ReesAlgebra"
loadPackage( "ReesAlgebra", Reload=>true)
check "ReesAlgebra"
installPackage(ReesAlgebra, IgnoreExampleErrors=>true)

end


--NOTE Oct 5
-- I think we should combine the examples section for 
--symmetricKernel, reesIdeal, reesAlgebra, isLinearType, normalCone, 
--specialFiberIdeal. (These are called GROUP1 below).

--Also together:
distinguished, distinguishedAndMult

--separately
--universalEmbedding, multiplicity, analyticSpread








---- AT:  Anything Listed as a TEST below here is not really a TEST
---- yet since it either has no asserts or it does not have enough and
---- is more of an example.  Some of these examples are from a very
---- long time ago.  

--Examples for the ReesAlgebra package.                                                                                                                                                                                       

restart
load "ReesAlgebra.m2"
kk=ZZ/101
x = symbol x

--Example 1: a monomial ideal in 4-space.
S=kk[x_0..x_4]
i=monomialCurveIdeal(S,{2,3,5,6})
assert(isLinearType(i,i_0) == false)
assert(isLinearType i == false)
ring i
use S
-- reesAlgebra (i,i_0)
-- presentation normalCone (i, i_0)
presentation associatedGradedRing (i,i_0)
specialFiberIdeal (i,i_0)

--Example 2: determinantal ideals
restart
loadPackage "ReesAlgebra"
kk=ZZ/101
S=kk[x,y]
m=random(S^3,S^{4:-1})
i=minors(2,m);
time I=reesIdeal (i,i_0); -- .04 sec
time I=reesIdeal(i);
betti res i
betti(I, Weights=>{1,1})

--For symmetricKernel

S = ZZ/101[x_1,x_2, Degrees => {{1,1}, {1,-3}}]
I = ideal(x_1^4*x_2^3)
f = matrix{{x_1,x_2, 0, 0, 0}, {0, 0 , x_1^2, x_1*x_2, x_2^2}}
F = map(S^{{-2, 1}, {2, 2}}, S^{{-3, 0},{ -3, 4},{0,0}, {0, 4}, {0,8}}, f)
R = S/I
M = (image F)**R
symmetricKernel F
degrees ring oo



{* 
A reference we might NOT put in:
T. Roemer,  "Homological Properties of Bigraded Modules"
Roemer, Tim(D-ESSN)
Homological properties of bigraded algebras. (English summary) 
Illinois J. Math. 45 (2001), no. 4, 1361--1376. 
 Thm 5.3
shows that if i is and ideal in the polynomial ring,
generated in degree d (and maybe i is 
primary to the maximal ideal) then
  reg(I^j) = jd + b for m>-=j0
where j0 is the max degree in the "new variables" of
a bigeneric initial ideal of reesIdeal(i)
(bigeneric means we allow general changes of coords in
the new vars alone and in the old vars alone.)

Eisenbud and Ulrich have shown that there is a similar bound
in terms of the regularity with respect to the variables y
(graded with the degrees of the generators of i). This is proven
only in the case of ideals generated in a single degree and
primary to the maximal ideal. 

Research Problem: what's the situation in general?
*}

///
--For isLinearType
restart
loadPackage "ReesAlgebra"
S = ZZ/101[x,y]
M = module ideal(x,y)
E = {true, false, false, false, false}
assert({true, false, false, false, false} == 
    for p from 1 to 5 list(isLinearType (ideal vars S)^p))
///


///
--For reesAlgebra (combine with reesIdeal?)
restart
loadPackage "ReesAlgebra"
S = ZZ/101[x,y]
M = (ideal vars S)^2
reesAlgebra(M,S_0)
reesAlgebra(M)
reesIdeal M
///

TEST///
restart
loadPackage "ReesAlgebra"
kk=ZZ/101
R=kk[x,y]
i=(ideal vars R)^2
reesAlgebra i
reesIdeal i
specialFiberIdeal i
assert (isLinearType i==false)

normalCone i

restart
loadPackage "ReesAlgebra"
kk=ZZ/101
R=kk[x,y]
i = ideal(x^2,y^2)
-- need to reset ring
i = ideal(x+y^2)
multiplicity i 

R = ZZ/101[x,y]/ideal(x^3-y^3)
I = ideal(x^2,y^2)
multiplicity I

///

///
restart
loadPackage "ReesAlgebra"
kk=ZZ/101
R=kk[x,y]
i=(ideal vars R)^2
reesAlgebra i
reesIdeal i
specialFiberIdeal i
///

///
--Examples for reesIdeal

--Using reesIdeal to understand blowups and resolution of plane curve singularities:

restart
load "ReesAlgebra.m2"
R = ZZ/32003[x,y,z]
I = ideal(x,y)
cusp = ideal(x^2*z-y^3)
RI = reesIdeal(I)
S = ring RI
totalTransform = substitute(cusp, S) + RI
D = decompose totalTransform -- the components are the proper transform of the cuspidal curve and the exceptional curve 
L = primaryDecomposition totalTransform 
apply(L, i -> (degree i)/(degree radical i))
-- the total transform of the cusp contains the exceptional with multiplicity two 
-- the proper transform of the cusp is a smooth curve but tangent to the exceptional curve 
singular = ideal(singularLocus(L_0));
SL = saturate(singular, ideal(x,y,z));
saturate(SL, ideal(w_1,w_2)) -- we get 1 so it is smooth.

--another example with blowing up:
tacnode = ideal(x^2*z^2-x^4-y^4)
RI = reesIdeal(I)
S = ring RI
totalTransform = substitute(tacnode, S) + RI
D = decompose totalTransform -- the components are the proper transform of the cuspidal curve and the exceptional curve 
L = primaryDecomposition totalTransform 
apply(L, i -> (degree i)/(degree radical i))
-- the total transform of the tacnode contains the exceptional with multiplicity two 
-- the proper transform of the tacnode is not yet smooth.  We compute the singular point of the proper 
-- transform and blow up again. 
singular = ideal(singularLocus(L_0));
SL = saturate(singular, ideal(x,y,z));
J = saturate(SL, ideal(w_1,w_2)) -- we get 1 so it is smooth.
RJ = reesIdeal(J,Variable => v)
SJ = ring RJ
totalTransform = substitute(L_0, SJ) + RJ
D = decompose totalTransform -- the components are the proper transform of the cuspidal curve and the exceptional curve 
L = primaryDecomposition totalTransform 
(degree L_1)/(degree radical L_1) -- multiplicity of the second exceptional curve is 1
-- the second blow-up desingularizes the tacnode. 
singular = ideal(singularLocus(L_0));
SL = saturate(singular, ideal(x,y,z));
J = saturate(SL, ideal(w_1,w_2))
J2 = saturate(J, ideal(v_1,v_2, v_3))


-- however blowing-up (x^2,y) desingularlizes the tacnode x^2-y^4 in a single step.
R = ZZ/32003[x,y,z]
I = ideal(x^2,y)
tacnode = ideal(x^2*z^2-y^4)
RI = reesIdeal(I)
S = ring RI
totalTransform = substitute(tacnode, S) + RI
D = decompose totalTransform -- the components are the proper transform of the cuspidal curve and the exceptional curve 
L = primaryDecomposition totalTransform 
singular = ideal(singularLocus(D_0));
SL = saturate(singular, ideal(x,y,z));
saturate(SL, ideal(w_1,w_2)) -- we get 1 so it is smooth.


-- two singularities (x^2+y^2-3x*z)^2 -4*x^2*(2z-x)*z -- blowup both singularities
R = ZZ/32003[x,y,z]
curve = ideal((x^2+y^2-3*x*z)^2 -4*x^2*(2*z-x)*z)
sings = radical saturate(ideal(singularLocus(curve)), ideal (vars R))
decompose sings -- there is a tacnode at (0:0:1) and a cusp at (1:0:1) 
-- we blow up P2 at both points. 
RI = reesIdeal(sings) 
S = ring RI
totalTransform = substitute(curve, S) + RI
D = decompose totalTransform -- the components are the proper transform of the curve and two exceptional curves
singular = ideal(singularLocus(D_0));
SL = saturate(singular, ideal(x,y,z));
J = saturate(SL, ideal(w_1,w_2))

-- we resolved the cusp, but need a second blow-up to resolve the tacnode (at a point on the exceptional divisor). 
 
RJ = reesIdeal(J, Variable => v)
SJ = ring RJ
totalTransform = substitute(D_0, SJ) + RJ
D = decompose totalTransform -- the components are the next proper transform and the new exceptional curve.
-- the second blow-up desingularizes the original curve.
singular = ideal(singularLocus(D_0));
SL = saturate(singular, ideal(x,y,z));
J = saturate(SL, ideal(w_1,w_2))
J2 = saturate(J, ideal(v_1,v_2, v_3))

///

///
--- Example of non-distinguished components to test distinguished code.
restart 
loadPackage "ReesAlgebra"
T=ZZ/101[c,d]
D = 4
P = product(D, i -> random(1,T))
R = ZZ/101[a,b,c,d]
I = ideal(a^2, a*b*(substitute(P,R)), b^2)
ass I -- there is one minimal associated prime (a thick line in PP^3) and D embedded primes (points on the line) 
primaryDecomposition I
distinguished(I) -- only the minimal prime is a distinguished component
K = distinguishedAndMult(I) -- get multiplicity 2 
///

-----------
restart 
load "randomIdeal.m2"
loadPackage "ReesAlgebra"

T=ZZ/101[a,b,c,d]
line=ideal"a,b"
L={5,5,5,5}

--point=(ideal(a,b,c))^2
point = randomMonomialIdeal(L,T)
i=intersect(line,point)

for t from 1 to 100 do(
     L={random(7), random(7), random(7), random(7), random(7)};
          point = randomMonomialIdeal(L,T);
     I=specialFiberIdeal point;
     if length (D= decompose I) > 1 then (
	  print toString point;
	  print D;
	  print)
     )
i=ideal(b^2*c^3,a^2,a*c^3,b^5*c,b^4*c)
I=specialFiberIdeal i

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=ReesAlgebra RemakeAllDocumentation=true IgnoreExampleErrors=false"
-- End:
