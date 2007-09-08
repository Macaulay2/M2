---------------------------------------------------------------------------
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
-- PROGRAMMERs : Rees algebra code written by David Eisenbud and edited and 
--               maintained by Amelia Taylor.  Ideal integral closure 
--		 code written and maintained by Amelia Taylor
-- UPDATE HISTORY : 27 October 2006
--
-- Missing documentation and most examples are now at the end of the file
-- waiting to be included in the documentation -- more fixes to come
---------------------------------------------------------------------------
newPackage(
	"ReesAlgebra",
    	Version => "0.1", 
    	Date => "October 27, 2006",
    	Authors => {{
		  Name => "David Eisenbud",
		  Email => "de@msri.org"},
	     {Name => "Amelia Taylor",
	     HomePage => "http://faculty1.coloradocollege.edu/~ataylor/",
   	     Email => "amelia.taylor@coloradocollege.edu"},
             {Name => "Sorin Popescu",
	      Email => "sorin@math.sunysb.edu"}},  
    	Headline => "Rees algebras",
    	DebuggingMode => true
    	)

export{ symmetricKernel, universalEmbedding, reesAlgebra, reesIdeal, distinguished, 
     distinguishedAndMult, specialFiber, analyticSpread, isLinearType, 
     idealIntegralClosure, multiplicity}




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
w := global w;
symmetricKernel = method(Options=>{Variable => null})
symmetricKernel(Matrix,Ideal) := Ideal => o -> (f, I) -> (
     -- first four lines set up constructing key rings.
     S := ring f;
     if (monoid S).Options.DegreeRank =!= 1 then (
	  -- foldedDegs := apply((monoid S).Options.Degrees, i -> sum(i));
	  -- R := (coefficientRing(S))(monoid[gens S, Degrees => foldedDegs, MonomialOrder => GRevLex]);
	  -- Caveat: folded degrees is just a hack till towers of multigraded rings will work. Fix me!
     	  R := (coefficientRing(S))(monoid[gens S, DegreeRank => 1, MonomialOrder => GRevLex]);
	  G := map(R,S);
	  f = G(f);
	  I = G(I);
	  )
     else (R = S);
	  kk := coefficientRing(R);           
     	  oldvarlist := flatten entries vars R;   
     	  nR := rank source vars R;
     	  -- Set up the key map from R to R/I.
     	  mtar := -(min flatten degrees target f)+1;
     	  msource := -(min flatten degrees source f)+1;
     	  ntarf := rank target f;
     	  nsouf := rank source f;
--	  error "debug me";
     	  tardeglist :=  degrees source vars R | degrees target (f**R^{-mtar});
	  Rtar1 := kk(monoid [oldvarlist,Y_1..Y_(ntarf),Degrees=>tardeglist]);
     	  F := map(Rtar1, R);
     	  Rtar = Rtar1/F(I);
     	  oldvars := (vars Rtar)_{0..nR-1};
     	  RtoRtar := map(Rtar, R, oldvars);
     	  -- g builds key elements y_j*f_i to make the map R[y_j*f_i] to R/I
     	  -- desired answer is the kernel of this map given as i.
     	  g := oldvars|((vars Rtar)_{nR..(nR+ntarf-1)})*(RtoRtar(f**R^{-mtar}));  
     	  if o.Variable === null then (
     	       Rsource := kk(monoid [oldvarlist, w_0..w_(nsouf-1), Degrees=>degrees source g]))
     	  else (Rsource = kk(monoid [oldvarlist, (o.Variable)_0..(o.Variable)_(nsouf-1),
	       		 Degrees=>degrees source g]));
     	  i := ideal mingens ker map(Rtar, Rsource, g);
     	  -- Degrees are set to desired multidegree of the rees algebra.
     	  newdegs1 := apply(degrees source oldvars, i -> append(i,0));
     	  newdegs2 := apply(degrees source f, i -> append(i, 1));
      	  if o.Variable === null then (
     	       Ranswer := kk(monoid [oldvarlist, w_0..w_(nsouf-1),
	       		 MonomialOrder => { #oldvarlist, nsouf},
	       		 Degrees=>join(newdegs1,newdegs2)]))
     	  else (Ranswer = kk(monoid [oldvarlist, (o.Variable)_0..(o.Variable)_(nsouf-1),
	       		 MonomialOrder => { #oldvarlist, nsouf},
	       		 Degrees=>join(newdegs1,newdegs2)]));
     	  (map(Ranswer, Rsource))(i)
     )


symmetricKernel(Module) := Ideal => o-> (M) ->(
     S := ring M;
     I := ideal S;
     R := ring I;
     symmetricKernel(lift(presentation M, R), I)
     )    	      	   
symmetricKernel(Ideal) := Ideal => o -> (J) -> (
     symmetricKernel(coker gens J))
     
     
-- PURPOSE: Core code for the universal embedding of the image of f**R/I.
--          For more information, see below.
universalEmbeddingCore = (f,I) ->(
     R := ring f;
     S := R/I;
     F := map(S, R);
     fbar := F(f);
     fres := res(image fbar, LengthLimit=>2);
     ftres := res(image transpose fres.dd_1, LengthLimit=>2);
     fnew := transpose ftres.dd_1;
     lift(fnew, R)
     )

-- PURPOSE: Front end code for the universal (or versal) embedding of the 
--          image of f, or of M or of J over a quotient ring.  
-- INPUT : 'M' a matrix and 'I' an ideal defined over a polynomial ring.
-- OUTPUT : a map that is a versal embedding of the image of M over the 
--          ring of M mod I.  
-- COMMENT : The purpose is to compute a versal embedding to be used in 
--           symmetricKernel in order to compute a Rees Algebra in the most 
--           general case possible at this time as defined in Eisenbud, Huneke 
--           and Ulrich. 
universalEmbedding = method()
universalEmbedding(Matrix,Ideal) := Matrix => (M, I) -> universalEmbeddingCore(M,I)


-- PURPOSE :  Simply compute the rees ring of an ideal over a polynomial ring.
-- INPUT : 'J' an Ideal and 'a' a RingElement. 
-- OUTPUT : an Ideal defining the Rees algebra of an ideal saturated using the 
--          ring element given.   
-- COMMENT : Can be used independently or as a strategy for reesIdeal and reesAlgebra.  
--           Goal is for this to be only a local routine for reesIdeal and 
--           reesAlgebra as soon as we can find a way to enter the ring 
--           element for that strategy.

reesSaturate = method(Options => {Variable => null})     
reesSaturate (Ideal, RingElement)  := Ideal => o -> (J, a) -> (
     R := ring J;
     j := syz gens J;
     oldvarlist := flatten entries vars R;
     numnew := numgens J;
     newdegs1 := apply(degrees source oldvars, i -> append(i,0));
     newdegs2 := apply(numnew, i -> flatten{degree(I_i),1});
     if o.Variable === null then (
     	  Ranswer := (coefficientRing R)(monoid [oldvarlist, w_0..w_(numnew-1),
	       MonomialOrder => { #oldvarlist, numnew},
	       Degrees=>join(newdegs1,newdegs2)]))
     else (Ranswer = (coefficientRing R)(monoid [oldvarlist, (o.Variable)_0..(o.Variable)_(numnew-1),
	       MonomialOrder => { #oldvarlist, numnew},
	       Degrees=>join(newdegs1,newdegs2)])); 
     F := map(Ranswer, R); -- uses that oldvars are first.
     symm := ideal(matrix{{Ranswer_n..Ranswer_(numgens R + numnew -1)}}*F(j));
     saturate(symm,F(a))
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
reesIdeal = method(Options=>{Variable => null, Strategy => null})
reesIdeal(Module) := Ideal => o -> (M) -> (
     S := ring M;
     I := ideal S;
     R := ring I;
     symmetricKernel(universalEmbedding(lift(presentation M, R),I), I, 
	  Variable => o.Variable)
	  )
reesIdeal(Ideal) := Ideal => o-> (J) -> (
     S := ring J;
     if instance(S, PolynomialRing) == true then(
     	  if Strategy == Saturate then (
	       reesSaturate(J,J_0, Variable => o.Variable))
     	  else symmetricKernel(gens J, ideal(0_S), Variable => o.Variable))
     else (
	  I := ideal S;
     	  symmetricKernel(universalEmbedding(lift(gens J, ring I), I), I, Variable => o.Variable)
	  )
     )

-- PURPOSE : Front end for computing the defining Rees algebra 
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
-- OUTPUT : A quotient ring isomorphic to the Rees Algebra.
-- COMMENT : This is IDENTICAL to reesIdeal, but returns the quotient ring 
--           rather than the ideal.  
--           Uses proposition 1.3 in Eisenbud, Huneke, Ulrich and computes 
--           the rees algebra of a versal embedding of the 
--           Module regardless of the ring and for an ideal over a quotient ring. 
--           In the case of an ideal over a polynomial ring the process is slightly 
--           streamlined, skipping the unneccessary versal computation as in that 
--           case the inclusion map is a versal map.
     
reesAlgebra = method(Options=>{Variable => null, Strategy => null})
reesAlgebra(Module) := QuotientRing => o -> (M) -> (
     J := reesIdeal(M, Variable => o.Variable);
     (ring J)/J)
reesAlgebra(Ideal) := QuotientRing => o -> (J) -> (
     if Strategy == Saturate then (
	  J = reesSaturate(J,J_0, Variable => o.Variable))
     else (J = reesIdeal(J, Variable => o.Variable));
     (ring J)/J
     )
          
          
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
distinguished = method(Options => {Variable => null})
distinguished(Ideal) := List => o -> i -> (
     R := ring i;
     J := reesIdeal(i, Variable => o.Variable); -- the ideal of the Rees algebra
     oldDegs := (monoid (ring J)).Options.Degrees;
     newDegs := apply(oldDegs, i -> first entries (matrix{i}*matrix{{1},{1}}));
     S := (coefficientRing (ring J))(monoid[gens (ring J), Degrees => newDegs, MonomialOrder => GRevLex]);
     mapToSfromRees:= map(S, (ring J));
     mapToSfromR := map(S, R);
     L := decompose (mapToSfromR(i)+mapToSfromRees(J));                 
     apply(L,P->kernel(map(S/P, R)))
     )

-- PURPOSE : Compute the distinguised subvarieties of a variety  
--           (components of the support of the normal cone) WITH their 
--           multiplicities.
-- INPUT : 'i' an ideal over a polynomial ring. 
-- OUTPUT : ideals that are the components of the support of the normal 
--          cone of V(i) and integers that are their corresponding 
--          multiplicities.
-- COMMENT : I have a note stating that "right now" this computation 
--           requires a polynomial ring over a finite field - written 
--           in 2000/2001.  I have no idea why.  I suspect that at the 
--           time decompose required this.  But I think it is not necessary 
--           now. 
distinguishedAndMult = method(Options => {Variable => null})
distinguishedAndMult(Ideal) := List => o -> i -> (
     R := ring i;
     J := reesIdeal(i, Variable => o.Variable); -- the ideal of the Rees algebra
     oldDegs := (monoid (ring J)).Options.Degrees;
     newDegs := apply(oldDegs, i -> first entries (matrix{i}*matrix{{1},{1}}));
     S := (coefficientRing (ring J))(monoid[gens (ring J), Degrees => newDegs, MonomialOrder => GRevLex]);
     mapToSfromRees:= map(S, (ring J));
     mapToSfromR := map(S, R);
     I := mapToSfromR(i)+mapToSfromRees(J);    
     Itop := top I; -- we only need the irreducibles of codim 1 mod J.
     L := decompose (I); -- find its irred components
     apply(L,P->(Pcomponent := Itop:(saturate(Itop,P)); 
	       --the P-primary component. The multiplicity is
	       --computed as (degree Pcomponent)/(degree P)
       	  {(degree Pcomponent)/(degree P), kernel(map(S/P, R))})))


-- PURPOSE : Core code of the special fiber of the rees algebra 
--           over a quotient ring. For use, see front end. 
specialFiberCore = (f,V) -> (
     J := reesAlgebra(f, Variable => V);
     R := ring f;
     S := ring J;
     nR := (rank source vars S) - (rank source vars R);
     fiberdeglist := take(degrees S, nR);
     if V === null then Rfiber := coefficientRing(R)[w_1..w_nR, Degrees=>fiberdeglist]
     else Rfiber = coefficientRing(R)[V_1..V_nR, Degrees=>fiberdeglist];
     specialize := map(Rfiber,ring J);
     trim (specialize J))

-- PURPOSE : The special fiber of the rees algebra over a quotient ring.
-- INPUT : 'f' a map or 
--         'M' a module or 
--         'J' an ideal over a ring R.
-- Options : The ring R can be a quotient ring, or, the user can define 
--           f, M or J over a polynomial ring R and an ideal I can be given 
--           as the option Strategy and the special fiber is then computed 
--           over the quotient ring R/I.
-- OUTPUT : an ideal defining the special fiber of the rees algebra over 
--          a quotient ring, in a NEW polynomial ring. 
specialFiber = method(Options=>{Variable => null})
specialFiber(Module) := Ideal => o-> (M) -> specialFiberCore(M, o.Variable)
specialFiber(Ideal) :=  Ideal => o -> (J) -> specialFiberCore(J, o.Variable)

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
analyticSpread(Module) := ZZ => (M) -> dim specialFiber(M)
analyticSpread(Ideal) := ZZ => (J) ->  dim specialFiber(J)
 
-- PURPOSE : Core code for isLinearType.  For more information, see below.
isLinearTypeCore = (f) -> ( 
     K := reesAlgebra(f);
     T := ring K;
     oldvars := substitute(vars ring I, T);
     newvars := compress ((vars T)%oldvars);
     test := compress contract(newvars,contract(newvars, gens K));
     (rank source test)==0
     )

-- PURPOSE : Test if a module is of linear type.
-- INPUT : 'M' a module OR
--         'J' an ideal 
-- Options : The ring R can be a quotient ring, or, the user can define 
--           f, M or J over a polynomial ring R and an ideal I can be given 
--           as the option Strategy and the special fiber is then computed 
--           over the quotient ring R/I.
-- OUTPUT : The boolean true if the rees algebra of the image of f, M, 
--         or J is equal to its symmetric algebra and false otherwise.
-- COMMENT : Tests whether the rees relations are linear forms in the 
--           new variables.
isLinearType = method(Options=>{Strategy => null})
isLinearType(Module) := Boolean => o -> (M) -> isLinearTypeCore(M)
isLinearType(Ideal) := Boolean => o -> (J) -> isLinearTypeCore(J)


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
multiplicity(Ideal) := ZZ => (I) -> (  
     R := ring I;
     J1 := reesAlgebra(I);  --defining ideal of Rees algebra
     SJ1 := ring J1;
     S := coefficientRing(SJ1)[gens SJ1];
	       --Degrees => apply(degrees SJ1, i -> first i)]);
     F := map(S, SJ1);
     BS := S/F(J1);  -- The Rees algebra
     G := map(BS, R);
     -- coker gens G(I) is the associated graded ring.
     degree coker gens G(I)
     )

multiplicity(Module,Ideal) := ZZ => (M, I) -> (  
     R := ring M;
     J1 := reesAlgebra(I);  --defining ideal of Rees algebra
     SJ1 := ring J1;
     S := coefficientRing(SJ1)[gens SJ1];
	       --Degrees => apply(degrees SJ1, i -> first i)]);
     F := map(S, SJ1);
     BS := S/F(J1);  -- the Rees algebra
     G := map(BS, R);
     AssGI := minimalPresentation(BS/G(I)); -- the associated graded ring.
     H := map(AssGI, R);
     degree (H**M)
     )

-- PURPOSE : Compute the integral closure of an ideal.
-- INPUT : 'I' and ideal.
-- OUTPUT: an ideal, the integral closure of I.
-- COMMENT : Computed by finding the degree 1 piece of the integral 
--           closure of the Rees algebra R[It].
-- WARNING:  The Rees algebra involves as many new variables as you 
--           have generators of I.  Thus taking the integral closure of such
--           a ring can take up all of your memory and time (most likely time!).
idealIntegralClosure = method()
idealIntegralClosure(Ideal) := Ideal => (I) -> (
     R := ring I;
     n1 := numgens R;
     J1 := reesAlgebra(I);  --defining ideal of Rees algebra
     R2 := ring J1;
     n2 := numgens R2;
     NewVars := take(gens R2,{n1,n2-1});
     S := R2/J1;  -- the rees algebra.
     Sfrac1 := first entries (ICfractionsLong S);  -- The slow part is this! 
     Sfrac2 := apply(#Sfrac1-n1, i-> Sfrac1#i);
     toLift := select(Sfrac2, i-> (degree i)#1 == 1);
     VarImage := flatten append (first entries gens I,gens R);
     LiftMap := map(R,R2,VarImage);
     NewNums := apply(toLift, i-> LiftMap(substitute(numerator i, R2)));
     NewDenoms := apply(toLift, i-> LiftMap(substitute(denominator i, R2)));
     NewGens := apply(#toLift, i-> substitute((NewNums#i)/(NewDenoms#i),R));
     ideal mingens(I + ideal(NewGens)) 
     )

beginDocumentation()

document {
     Key => ReesAlgebra,
     Headline => "compute Rees algebras and integral closure 
     of ideals",
     " The goal of this package is to provide commands to compute the 
     Rees algebra of a module as it is defined in the paper ", EM "What is 
     the Rees algebra of a module?", " by Craig Huneke, David Eisenbud and 
     Bernd Ulrich. It also includes functions for computing many of 
     the structures that require a Rees algebra.  The included functions are 
     listed below. Examples of the use of each of the functions are included 
     with their documentation."
     }

document {
     Key => symmetricKernel,
     Headline => "compute the defining ideal of the rees algebra for a 
     ma",
     Usage => "symmetricKernel(f, I)",
     Inputs => {"f" => {ofClass Matrix}},
     Outputs => {{ofClass Ideal, "defining the Rees ring of 
	       the ", ofClass Matrix, TT "f"}},
	       
     PARA{}, "This function is the workhorse of all/most of the Rees algebra 
     functions.  Most users will prefer to use one of the front 
     end commands ", TO "reesAlgebra", ".",
     
     EXAMPLE {
	  "R = QQ[a..e]",
	  "J = monomialCurveIdeal(R, {1,2,3,4})",
	  (
	       stderr << "--warning: non-functional example code commented out" << endl;
	       "symmetricKernel -- (gens J)"
	       )
     },
    
    "Let the ideal returned be ", TT "I", " and the ring it lives in 
    (also printed) ", TT "S", ", then ", TT "S/I", " is isomorphic to 
    the Rees algebra ", TT "R[Jt]",  "We can get the same information 
    using ", TT "reesAlgebra(J)", ", see ", TO "reesAlgebra", ".  Also 
    note that ", TT "S", " is multigraded allowing Macaulay2 to correctly 
    see that the variables of R now live in degree 0 and the new variables 
    needed to describe ", TT "R[Jt]", "as a k-algebra are in degree 1.",
    
    PARA{ TT "symmetricKernel", " can also be computed over a quotient 
    ring by either initially defining the ring ", TT "R", " as a 
    quotient ring, or by giving the quotient ideal as an optional argument."},
    
    EXAMPLE { 
     	  "R = QQ[x,y,z]/ideal(x*y^2-z^9)",
	  "J = ideal(x,y,z)",
	  (
	       stderr << "--warning: non-functional example code commented out" << endl;
	       "symmetricKernel -- (gens J)"
	       )
	  },
     " or ",
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "I = ideal(x*y^2-z^9)",
	  "J = ideal(x,y,z)",
	  (
	       stderr << "--warning: non-functional example code commented out" << endl;
	       "symmetricKernel -- (gens J)"
	       )
	  },
     "These many ways of working with the function allows the system 
     to compute both the classic Rees algebra of an ideal over a ring 
     (polynomial or quotient) and to compute the the Rees algebra of a 
     module or ideal using a universal embedding as described in the paper 
     of Eisenbud, Huneke and Ulrich.  It also allows different ways of 
     setting up the quotient ring.",
     SeeAlso => {reesAlgebra, universalEmbedding},
     }


document {
     Key => (symmetricKernel,Matrix,Ideal),
     }

document {
     Key => (symmetricKernel,Module),
	  }

document {
     Key => [symmetricKernel, Variable],
     Headline=> "symmetricKernel introduces new variables and the option 
     Variable allows the user to specify a variable name for this purpose, 
     the default variable is", TT  "w", "but the default value of the option 
     is null."     
     }

document { 
     Key => {universalEmbedding, (universalEmbedding,Matrix, Ideal)},
     Headline => "Compute the universal embedding",
     Usage =>  "universalEmbedding(M,I)", 
     Inputs => {"M" => {ofClass Matrix, " in ", ofClass PolynomialRing}, 
	  "I" => {ofClass Ideal, " in ", ofClass PolynomialRing}},
     Outputs => {{ofClass Matrix, "defining the universal embedding 
	       of the module given over a quotient ring defined by ", TT "I",
	       " into a free module over the polynomial ring for ", TT "I",
	       " where ", TT "M", " is the lift of a presentation of the module to 
	       the polynomial ring"}},
      PARA{}, "The main purpose of this function is to compute the embedding 
     needed to compute the ReesAlgebra of a module following ", EM "What 
     is the Rees algebra of a module?", " written by Eisenbud, Huneke, and 
     Ulrich ", ". The function is incorporated in ", TO "reesAlgebra", " but the 
     interested user can use this function to see the map or use it for 
     something else. ", 
     EXAMPLE { 
	  "R=QQ[x_1..x_8];",
	  "m1=genericMatrix(R,x_1,2,2); m2=genericMatrix(R,x_5,2,2);",
	  "m=m1*m2",
	  "i= ideal flatten m",
	  "d1=minors(2,m1); d2=minors(2,m2);",
	  "j=i+d1+d2",
	  "M=matrix{{0,d1_0,m_(0,0),m_(0,1)},
               {0,0,m_(1,0),m_(1,1)},
	       {0,0,0,d2_0},
	       {0,0,0,0}}",
	  "M=M-(transpose M)",
	  "N=transpose (res coker transpose M).dd_2",
	  (
	       stderr << "--warning: non-functional example code commented out" << endl;
	       "uN=universalEmbedding -- (N)"
	       )
	  }
     }


document {
     Key => reesAlgebra, 
     Headline => "compute the Rees algebra"
     }

document {
     Key => [reesAlgebra, Variable],
     Headline=> "rees introduces new variables and the option 
     Variable allows the user to specify a variable name for this purpose, 
     the default is", TT  "w"     
     }

document {
     Key => [reesAlgebra, Strategy],
     Headline=> "rees introduces new variables and the option 
     Variable allows the user to specify a variable name for this purpose, 
     the default is", TT  "w"     
     }


document {
     Key => (reesAlgebra,Module), 
     Headline => "compute the Rees algebra of a module over a quotient ring",
     Usage =>  "reesAlgebra(M)",
     Inputs => {"M"},
     Outputs => {{" defining the Rees algebra of  
	       the ", ofClass Module, " ", TT "M"}},
     "Stuff."
     }

document { 
     Key => (reesAlgebra,Ideal),
     Headline => "compute the Rees algebra of an ideal over a quotient ring",
     Usage =>  "reesAlgebra(J)",
     Inputs =>  {"J"},
     Outputs => {{" defining the Rees algebra of 
	       the ", ofClass Ideal, " ", TT "J"}},
     "Stuff."
     }

document {
     Key => {distinguished, (distinguished,Ideal)},
     Headline => "computes the distinguished subvarieties of a scheme",
     Usage => "distinguished I" ,
     Inputs =>  {"I" => {ofClass Ideal, " in ", ofClass PolynomialRing}},
     Outputs => {{ofClass List, " of prime ideals defining the components 
	  of the support of the normal cone over ", TT "I"}},
     "Stuff."
     }

document {
     Key => [distinguished, Variable],
     Headline=> "distinguished introduces new variables and the option 
     Variable allows the user to specify a variable name for this purpose, 
     the default is", TT  "w"     
     }

document {
     Key => {distinguishedAndMult, (distinguishedAndMult,Ideal)},
     Headline => "compute the distinguished subvarieties of a variety along 
     with their multiplicities",
     Usage => "distinguishedAndMult I" ,
     Inputs => {"I" => {ofClass Ideal, " in ", ofClass PolynomialRing}},
     Outputs => {{ofClass List, " of pairs where the first entry 
	       is the multiplicity of the second entry which is one 
	       of the ideals defining a component of the support of 
	       the normal cone over ", TT "I"}},
     "Stuff."
     }

document {
     Key => [distinguishedAndMult, Variable],
     Headline=> "distinguishedAndMult introduces new variables and the option 
     Variable allows the user to specify a variable name for this purpose, 
     the default is", TT  "w"     
     }

document {
     Key => specialFiber, 
     Headline => "compute the special fiber"
     }

document {
     Key => [specialFiber, Variable],
     Headline=> "specialFiber introduces new variables and the option 
     Variable allows the user to specify a variable name for this purpose, 
     the default is", TT  "w"     
     }

document { 
     Key => (specialFiber,Module), 
     Headline => "compute the special fiber of the image of a matrix over a", 
     "a quotient ring",
     Usage =>  "specialFiber(M)",
     Inputs =>  {"M"},
     Outputs => {{"defining the special fiber of ", TT "M"}},
     "Stuff."
     }

document { 
     Key => (specialFiber,Ideal),
     Headline => "compute the special fiber of the image of a matrix over 
     a quotient ring",
     Usage =>  "specialFiber(J)",
     Inputs =>  {"J"},
     Outputs => {{"defining the special fiber of ", TT "J"}},
     "Stuff."
     }

document {
     Key => analyticSpread, 
     Headline => "compute the analytic spread"
     }

document {
     Key => (analyticSpread,Module), 
     Headline => "compute the analytic spread of a module over a 
     quotient ring",
     Usage => "analyticSpread(M)",
     Inputs => {"M"},
     Outputs => {{"the dimension of the special fiber of ", TT "M"}},
               "Stuff."
     }	   

document {
     Key => (analyticSpread,Ideal),
     Headline => "compute the analytic spread of an ideal over a 
     quotient ring",
     Usage => "analyticSpread(J)",
     Inputs =>  {"J"},
     Outputs => {{"the dimension of the 
	       special fiber of the ideal ", TT "J"}},
     "Stuff."
     }

document {
     Key => isLinearType, 
     Headline => "determine if a module is of linear type"
     }

document {
     Key => (isLinearType,Module), 
     Headline => "determine if the image of a matrix is of linear type",
     Usage =>  "isLinearType(M)",
     Inputs =>  {"M"},
     Outputs => {{"true if the module is of linear 
	  type and false otherwise."}},
     "Stuff."
     }

document {
     Key => (isLinearType,Ideal),
     Headline => "determine if the image of a matrix is of linear type",
     Usage =>  "isLinearType(J)",
     Inputs =>  {"J"},
     Outputs => {{"true if the ideal is of linear 
	  type and false otherwise."}},
     "Stuff."
     }


document {
     Key => {idealIntegralClosure, (idealIntegralClosure,Ideal)},
     Headline => "compute the integral closure of an ideal",
     Usage =>  " idealIntegralClosure(I)",
     Inputs =>  {"I" => {ofClass Ideal, " in ", ofClass PolynomialRing}},
     Outputs => {{ofClass Ideal, " that is the integral closure of ", TT "I"}},
     "Computed as the degree 1 piece of 
	  the integral closure of the Rees algebra of ", TT "I", "."
     }

document {
     Key => multiplicity, 
     Headline => "compute the multiplicity of an ideal or module"
     }

document {
     Key => (multiplicity,Ideal),
     Headline => "compute the Hilbert-Samuel multiplicty of an ideal",
     Usage =>  "multiplicity I",
     Inputs =>  {"I"},
     Outputs => {{"  that is the normalized leading 
	  coefficient of the associated graded ring of ", TT "R", 
	  " with respect to ", TT "I"}},
     "Stuff."
     }

document {
     Key => (multiplicity,Module,Ideal),
     Headline => "compute the Hilbert-Samule multiplicity of a module with 
     respect to an ideal",
     Usage =>  "multiplicity(M,I)",
     Inputs =>  {"M", "I"},
     Outputs => {{" that is the normalized leading coefficient of 
	       the associated graded module of ", TT "M", " with 
	       respect to ", TT "I"}},
     "Stuff."
     }


-- symmetricKernel, rees, reesClassic, analyticSpread, 
-- distinguishedAndMult, specialFiber, distinguished, universalEmbedding, 
-- idealIntegralClosure, isLinearType, multiplicity

TEST ///
R=QQ[a,b,c,d,e,f]
M=matrix{{a,c,e},{b,d,f}}
analyticSpread image M
///


{*
restart
loadPackage "ReesAlgebraNew"
R=QQ[a..e]
j=monomialCurveIdeal(R, {1,2,3,5})
IS = symmetricKernel(j)
time L = reesAlgebra(j)
M = coker gens j
IM = reesAlgebra(M)
IR= time rees(j)
betti gens IR
degrees source vars ring IR
specialFiber(j, Strategy => I)
analyticSpread(j, Strategy => I)
----
restart
loadPackage "ReesAlgebraNew"
--kk=ZZ/32003
R=QQ[x_1..x_8]
m1=genericMatrix(R,x_1,2,2)
m2=genericMatrix(R,x_5,2,2)
m=m1*m2
flatten m
i= ideal flatten m
d1=minors(2,m1)
d2=minors(2,m2)
j=i+d1+d2
codim j
d1_0
m_(0,0)
M=matrix{{0,d1_0,m_(0,0),m_(0,1)},
         {0,0,m_(1,0),m_(1,1)},
	 {0,0,0,d2_0},
	 {0,0,0,0}}
M=M-(transpose M)
minors(4,M)

I=ideal(0_R)
N=transpose (res coker transpose M).dd_2

uN=universalEmbedding(N)
symmetricKernel(uN)
IR=rees(N)

SIR= specialFiber(N)
*}

{*
fu=universalEmbedding(I,f)   -- f = ????
betti symmetricKernel(I,fu)
betti symmetricKernel(I,f)
*}

{*
restart
loadPackage "ReesAlgebraNew"
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
saturate(SL, ideal(w_0,w_1)) -- we get 1 so it is smooth.
degree(D_0+D_1)/(degree radical(D_0+D_1))
*}

{*
restart
loadPackage "ReesAlgebraNew"
R = ZZ/32003[x,y,z]
I = ideal(x,y)
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
J = saturate(SL, ideal(w_0,w_1)) -- we get 1 so it is smooth.
RJ = reesIdeal(J,Variable => v)
SJ = ring RJ
totalTransform = substitute(L_0, SJ) + RJ
D = decompose totalTransform -- the components are the proper transform of the cuspidal curve and the exceptional curve 
L = primaryDecomposition totalTransform 
(degree L_1)/(degree radical L_1) -- multiplicity of the second exceptional curve is 1
-- the second blow-up desingularizes the tacnode. 
singular = ideal(singularLocus(L_0));
SL = saturate(singular, ideal(x,y,z));
J = saturate(SL, ideal(w_0,w_1))
J2 = saturate(J, ideal(v_0,v_1, v_2))


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
saturate(SL, ideal(w_0,w_1)) -- we get 1 so it is smooth.


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
J = saturate(SL, ideal(w_0,w_1))

-- we resolved the cusp, but need a second blow-up to resolve the tacnode (at a point on the exceptional divisor). 
 
RJ = reesIdeal(J, Variable => v)
SJ = ring RJ
totalTransform = substitute(D_0, SJ) + RJ
D = decompose totalTransform -- the components are the next proper transform and the new exceptional curve.
-- the second blow-up desingularizes the original curve.
singular = ideal(singularLocus(D_0));
SL = saturate(singular, ideal(x,y,z));
J = saturate(SL, ideal(w_0,w_1))
J2 = saturate(J, ideal(v_0,v_1, v_2))

*}

{*
--- Example of non-distinguished components to test distinguished code.
T=ZZ/101[c,d]
D = 4
P = product(D, i -> random(1,T))
R = ZZ/101[a,b,c,d]
I = ideal(a^2, a*b*(substitute(P,R)), b^2)
ass I -- there is one minimal associated prime (a thick line in PP^3) and D embedded primes (points on the line) 
primaryDecomposition I
distinguished(I) -- only the minimal prime is a distinguished component
K = distinguishedAndMult(I) -- get multiplicity 2 
intersect apply(K, i-> i_1^(i_0)) -- checks the Geometric Nullstellensatz on Ein-Lazarsfeld

*}

{*
R=ZZ/32003[x,y,z]
I=intersect(ideal(x),(ideal(x,y))^2, (ideal(x,y,z))^3)
ass I
distinguished I
K = distinguishedAndMult I
intersect apply(K, i-> i_1^(i_0)) 
*}



{*
-- Check multiplicities of the distinguished components versus the effective Nullstellenstaz

n = 5
S = ZZ/101[u,v]
R = ZZ/101[x_0..x_3]
f=map(S, R, matrix {{u^n, u^2, u*v,v}})
I = kernel f
*}

end
installPackage "ReesAlgebra"
