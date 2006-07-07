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
-- UPDATE HISTORY : 14 June 2006
---------------------------------------------------------------------------
newPackage(
	"ReesAlgebra",
    	Version => "1.0", 
    	Date => "June 14, 2006",
    	Authors => {{
		  Name => "David Eisenbud", 
		  HomePage => "",
		  Email => "de@msri.org"}},
    	Headline => "Rees algebras",
    	DebuggingMode => true
    	)

--	     {Name => "Amelia Taylor",
--	     HomePage => "http://www.stolaf.edu/people/ataylor",
--   	     Email => "ataylor@stolaf.edu"}

export{symmetricKernel, universalEmbedding, rees, reesClassic, distinguished, 
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
-- INPUT : 'f' a matrix over a ring R
-- Options : The ring R can be a quotient ring, or, the user can define 
--           f over a polynomial ring R and an ideal I can be given 
--           as the option Strategy and the special fiber is then computed 
--           over the quotient ring R/I.
-- OUTPUT : an Ideal defining the Rees algebra of the image of f regarded 
--          as a matrix over the ring R, or if option is given, R/I.
w := local w;
symmetricKernel = method(Options=>{Strategy => null,Variable => null})
symmetricKernel(Matrix) := Ideal => o -> (f) -> (
     -- first four lines set up constructing key rings.
     R := ring f;
     F := f;
     kk := coefficientRing(R);      
     oldvarlist := flatten entries vars R;   
     nR := rank source vars R;
     -- Set up the key map from R to R/I.
     mtar := -(min flatten degrees target f)+1;
     msource := -(min flatten degrees source f)+1;
     ntarf := rank target f;
     nsouf := rank source f;
     tardeglist := degrees target (f**R^{-mtar}) | degrees source vars R;
     Rtar1 := kk[Y_1..Y_(ntarf),oldvarlist,Degrees=>tardeglist];
     if o.Strategy === null and class R === PolynomialRing then Rtar := Rtar1
     else (if o.Strategy =!= null then (
	       I := o.Strategy;
     	       Rtar = Rtar1/substitute(I, Rtar1))
	  else (Rtar = Rtar1/substitute(ideal ring f, Rtar1));
	  );
     oldvars := (vars Rtar)_{0..nR-1};
     RtoRtar := map(Rtar, R, oldvars);
     -- g builds key elements y_j*f_i to make the map R[y_j*f_i] to R/I
     -- desired answer is the kernel of this map given as i.  
     g := ((vars Rtar)_{nR..(nR+ntarf-1)})*(RtoRtar(f**R^{-mtar}))|oldvars;
 --     << "--map" << g << endl;
      if o.Variable === null then (
     	   Rsource := kk[w_1..w_(nsouf),oldvarlist,
	  Degrees=>degrees source g])
     else (
	  Rsource = kk[(o.Variable)_1..(o.Variable)_(nsouf),oldvarlist,
	  Degrees=>degrees source g]);
     i := ideal mingens ker map(Rtar, Rsource, g);
--     << "--Rsource" << Rsource << endl;
--     << "--Rtar" << Rtar << endl;
     -- Degrees are set to desired multidegree of the rees algebra.
     newdegs1 := apply(degrees source oldvars, i -> append(i,0));
     newdegs2 := apply(degrees source f, i -> append(i, 1));
     if o.Variable === null then (
     	  Ranswer := kk[w_1..w_(nsouf),oldvarlist, 
	       MonomialOrder => {nsouf, #oldvarlist},
	       Degrees=>join(newdegs2,newdegs1)])
     else (Ranswer = kk[(o.Variable)_1..(o.Variable)_(nsouf),oldvarlist, 
	       MonomialOrder => {nsouf, #oldvarlist},
	       Degrees=>join(newdegs2,newdegs1)]);
     substitute(i, Ranswer)
     )
     
-- PURPOSE: Core code for the universal embedding of the image of f**R/I.
--          For more information, see below.
universalEmbeddingCore = (I,f) ->(
     if I =!= null then (
	  R := ring f; 
	  Rbar := R/I; 
	  fbar := substitute(f, Rbar)
	  )
     else fbar = f;
     fres := res(image fbar, LengthLimit=>2);
     ftres := res(image transpose fres.dd_1, LengthLimit=>2);
     if I === null then transpose ftres.dd_1
     else (fnew := transpose ftres.dd_1;
	  substitute(fnew, R))
     )

-- PURPOSE: Front end code for the universal (or versal) embedding of the 
--          image of f, or of M or of J over a quotient ring.  
-- INPUT : 'f' a matrix OR
--         'M' a module OR
--         'J' an ideal 
-- Options : The ring R can be a quotient ring, or, the user can define 
--           f, M or J over a polynomial ring R and an ideal I can be given 
--           as the option Strategy and the special fiber is then computed 
--           over the quotient ring R/I.
-- OUTPUT : a map that is a versal embedding of the image of f, M or J 
--          over the ring R, or if option is given, R/I.
-- COMMENT : The purpose is to compute a versal embedding to be used in 
--           symmetricKernel in order to compute a Rees Algebra. This allows 
--           as input any algebraic object we might want a rees algebra for.
universalEmbedding = method(Options=>{Strategy => null})
universalEmbedding(Matrix) := Matrix => o -> (f) -> universalEmbeddingCore(o.Strategy, f)
universalEmbedding(Module) := Matrix => o -> (M) -> universalEmbeddingCore(o.Strategy, presentation M)
universalEmbedding(Ideal) := Matrix => o -> (J) -> universalEmbeddingCore(o.Strategy, gens J)


-- PURPOSE : Front end for computing the Rees ring of a module, the image 
--           of a matrix, or an ideal each regarded as being in/over a 
--           quotient ring.
-- INPUT : 'f' a matrix OR
--         'M' a module OR
--         'J' an ideal 
-- Options : The ring R can be a quotient ring, or, the user can define 
--           f, M or J over a polynomial ring R and an ideal I can be given 
--           as the option Strategy and the special fiber is then computed 
--           over the quotient ring R/I.
-- OUTPUT : an Ideal defining the Rees algebra of the image of f, of M, 
--          or of J each considered as being in/over over the ring R, 
--          or if option is given, R/I..
-- COMMENT : Uses proposition 1.3 in Eisenbud, Huneke, Ulrich and computes 
--           the rees algebra of a versal embedding of the 
--           Matrix/Module/ideal over R/I.
rees = method(Options=>{Strategy => null,Variable => null})
rees(Matrix) := Ideal => o -> (f) -> (
     if o.Strategy === null then symmetricKernel(universalEmbedding(f),Variable => o.Variable)
     else (I := o.Strategy;
	  symmetricKernel(universalEmbedding(f,Strategy => I), Strategy => I, Variable => o.Variable)
	  )
     )
rees(Module) := Ideal => o -> (M) -> (
     if o.Strategy === null then symmetricKernel(universalEmbedding(M), Variable => o.Variable)
     else (I := o.Strategy;
	  symmetricKernel(universalEmbedding(M,Strategy => I), Strategy => I, Variable => o.Variable)
	  )
     )
 rees(Ideal) := Ideal => o-> (J) -> (
      if o.Strategy === null then symmetricKernel(universalEmbedding(J), Variable => o.Variable)
      else (I := o.Strategy;
	   symmetricKernel(universalEmbedding(J,Strategy => I), Strategy => I, Variable => o.Variable)
	  )
     )

-- PURPOSE : Quick computation of the basic computation of the Rees ring of 
--           an ideal.
-- INPUT : 'J' an ideal.
-- OUTPUT : Defining ideal for the Rees ring R[Jt] where R is the ring of J.
-- COMMENT : in the computations above, this corresponds to the special case 
--           where I is 0, so that we are still over R and the versal map, 
--           in this case is given by gens J. 
--           The point is that for large ideals, skipping the versal 
--           computation can save important time. 
reesClassic = method(Options => {Variable => null})
reesClassic(Ideal) := Ideal => o -> J -> symmetricKernel(gens J,Variable => o.Variable)

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
     J := reesClassic(i, Variable => o.Variable); -- the ideal of the Rees algebra
     II := substitute(i, ring J);
     L := decompose (II+J);                 
     apply(L,P->kernel(map((ring J)/P, R))) 
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
     J := reesClassic(i, Variable => o.Variable); -- the ideal of the Rees algebra
     I := J+substitute(i, ring J); -- the ideal of the normal cone
     Itop := top I; -- we only need the irreducibles of codim 1 mod J.
     L := decompose (I); -- find its irred components
     apply(L,P->(Pcomponent := Itop:(saturate(Itop,P)); 
	       --the P-primary component. The multiplicity is
	       --(degree Pcomponent)/(degree P)
       	  {(degree Pcomponent)/(degree P), kernel(map((ring P)/P, R))})))

-- PURPOSE : Core code of the special fiber of the rees algebra 
--           over a quotient ring. For use, see front end. 
specialFiberCore = (I,f,V) -> (
     J := rees(f, Strategy => I,Variable => V);
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
specialFiber = method(Options=>{Strategy => null, Variable => null})
specialFiber(Matrix) := o -> (f) -> specialFiberCore(o.Strategy, f,o.Variable)
specialFiber(Module) := o-> (M) -> specialFiberCore(o.Strategy, presentation M,o.Variable)
specialFiber(Ideal) :=  o -> (J) -> specialFiberCore(o.Strategy, gens J,o.Variable)

-- PURPOSE : Analytic spread of a module as defined in M2 by a matrix, 
--           a module or ideal over a quotient ring R/I.
-- INPUT : 'f' a matrix OR
--         'M' a module OR
--         'J' an ideal  
-- Options : The ring R can be a quotient ring, or, the user can define 
--           f, M or J over a polynomial ring R and an ideal I can be given 
--           as the option Strategy and the special fiber is then computed 
--           over the quotient ring R/I.
-- OUTPUT : The analytic spread of f/M/or J over over the ring R, or if 
--          the option is given, R/I.
analyticSpread = method(Options=>{Strategy => null})
analyticSpread(Matrix) := ZZ => o -> (f) -> (
     if o.Strategy === null then dim specialFiber(f)
     else (I := o.Strategy;
	  dim specialFiber(f, Strategy => I))
     )
analyticSpread(Module) := ZZ => o -> (M) -> (
     if o.Strategy === null then dim specialFiber(M)
     else (I := o.Strategy;
	  dim specialFiber(M, Strategy => I))
     )

analyticSpread(Ideal) := ZZ => o -> (J) -> (
     if o.Strategy === null then dim specialFiber(J)
     else (I := o.Strategy;
	  dim specialFiber(J, Strategy => I))
     )
 
-- PURPOSE : Core code for isLinearType.  For more information, see below.
isLinearTypeCore = (I,f) -> ( 
     K := rees(f,Strategy => I);
     T := ring K;
     oldvars := substitute(vars ring I, T);
     newvars := compress ((vars T)%oldvars);
     test := compress contract(newvars,contract(newvars, gens K));
     (rank source test)==0
     )

-- PURPOSE : Test if a module is of linear type.
-- INPUT : 'f' a matrix OR
--         'M' a module OR
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
isLinearType(Matrix) := Boolean => o -> (f) -> isLinearTypeCore(o.Strategy, f)
isLinearType(Module) := Boolean => o -> (M) -> isLinearTypeCore(o.Strategy, presentation M)
isLinearType(Ideal) := Boolean => o -> (J) -> isLinearTypeCore(o.Strategy, gens J)


-- Last updated May 4, 2005

-- PURPOSE : Compute the multipicity of I, the normalized leading 
--           coefficient of the hilbert polynomial of the associated 
--           graded ring with respect to I.
-- INPUT : 'I' an Ideal.
-- OUTPUT : 
-- COMMENT : Done as classically as possible.  
-- WARNING : Computing a quotient like R[It]/IR[It] requires a 
--           Groebner basis computation and thus can quickly take all of your
--           memory and time (most likely memory).   
multiplicity = method()
multiplicity(Ideal) := ZZ => (I) -> (  
     R := ring I;
     J1 := reesClassic(I);  --defining ideal of Rees algebra
     degree coker gens J1
   --- 17 June 2006 multigraded degree is not implemented 
   --- according to the documentation.  So what am I getting???
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
     J1 := reesClassic(I);  --defining ideal of Rees algebra
     R2 := ring J1;
     n2 := numgens R2;
     NewVars := take(gens R2,{0,n2-n1-1});
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
     Headline => "for rees algebra and integral closure of ideals",
     " Stuff"
     }

-- OUTPUT : an Ideal defining the Rees algebra of the image of f regarded 
--          as a matrix over R/I.
document {
     Key => {symmetricKernel, (symmetricKernel,Matrix)},
     Headline => "compute the rees ring for a matrix f over a quotient ring",
     Usage => "symmetricKernel(f)",
     Inputs => {"f"},
     Outputs => {{"an ",  ofClass Ideal, " defining the rees ring of 
	       the ", ofClass Matrix, TT "f"}},
     "Stuff"
     }

document {
     Key => [symmetricKernel, Strategy],
     Headline=> "Allows the user to define the matrix, ideal, or module over 
     a polynomial ring and then set an ideal to compute the symmetric kernel 
     over a quotient ring defined by Strategy"
     }

document {
     Key => [symmetricKernel, Variable],
     Headline=> "symmetricKernel introduces new variables and the option 
     Variable allows the user to specify a variable name for this purpose, 
     the default is", TT  "w"     
     }

document { 
     Key => universalEmbedding,
     Headline => "Compute the universal embedding" 
     }

document {
     Key => [universalEmbedding, Strategy],
     Headline=> "Allows the user to define the matrix, ideal, or module over 
     a polynomial ring and then set an ideal to compute a the univeral 
     embedding over a quotient ring defined by Strategy"
     }

document {
     Key => (universalEmbedding,Matrix),
     Headline => "compute the universal embedding of a matrix",
     Usage =>  "universalEmbedding(f)", 
     Inputs => {"f" => {ofClass Matrix}},
     Outputs => {{"a ", ofClass Matrix, " defining the universal embedding 
	       of  the image of ", ofClass Matrix, TT "f", "into a free 
	       module over the ring of ", TT "f"}},
     	       "Stuff."
     }

document {
     Key => (universalEmbedding,Module),
     Headline => "compute the universal embedding of a matrix",
     Usage =>  "universalEmbedding(f)", 
     Inputs => {"M" => {ofClass Module}},
     Outputs => {{"a ", ofClass Matrix, " defining the universal embedding 
	       of the ", ofClass Module, TT "M", "into a free module over 
	       the ring of ", TT "M"}},
     	       "Stuff."
     }

document {
     Key => (universalEmbedding,Ideal),
     Headline => "compute the universal embedding of an ideal",
     Usage =>  "universalEmbedding(I,J)", 
     Inputs => {"J" => {ofClass Ideal}},
     Outputs => {{"a ", ofClass Matrix, " defining the universal embedding 
	       of the ", ofClass Ideal, TT "J", "into a free module over 
	       the ring of ", TT "J"}},
     	       "Stuff."
     }

document {
     Key => rees, 
     Headline => "compute the rees algebra"
     }

document {
     Key => [rees,Strategy],
     Headline=> "Allows the user to define the matrix, ideal, or module over 
     a polynomial ring and then set an ideal to compute the rees algebra 
     over a quotient ring defined by Strategy"
     }
document {
     Key => [rees, Variable],
     Headline=> "rees introduces new variables and the option 
     Variable allows the user to specify a variable name for this purpose, 
     the default is", TT  "w"     
     }


document {
     Key => (rees,Matrix), 
     Headline => "compute the rees algebra of the image of a 
     matrix over a quotient ring" ,
     Usage =>  "rees(f)",
     Inputs =>  {"f" => {"a ", ofClass Matrix}},
     Outputs => {{"an ", ofClass Ideal, " defining the Rees algebra of  
	       the image of ", ofClass Matrix, TT "f"}},
     "Stuff."
     }

document {
     Key => (rees,Module), 
     Headline => "compute the rees algebra of a module over a quotient ring",
     Usage =>  "rees(M)",
     Inputs => {"M" => {"a ", ofClass Module}},
     Outputs => {{"an ", ofClass Ideal, " defining the Rees algebra of  
	       the ", ofClass Module, TT "M"}},
     "Stuff."
     }

document { 
     Key => (rees,Ideal),
     Headline => "compute the rees algebra of an ideal over a quotient ring",
     Usage =>  "rees(J)",
     Inputs =>  {"J" => {"a ", ofClass Ideal}},
     Outputs => {{"an ", ofClass Ideal, " defining the Rees algebra of 
	       the ", ofClass Ideal, " ", TT "J"}},
     "Stuff."
     }

document {
     Key => {reesClassic, (reesClassic,Ideal)},
     Headline => "compute the classic Rees algebra of an ideal",
     Usage =>  "reesClassic(I)",
     Inputs =>  {"I" => {"an ", ofClass Ideal, " in a ", 
	  ofClass  PolynomialRing}},
     Outputs => {{"The defining ", ofClass Ideal, "for the rees algebra ",
	       "of the ", ofClass Ideal, TT "I"}},
     "Stuff."
     }

document {
     Key => [reesClassic, Variable],
     Headline=> "reesClassic introduces new variables and the option 
     Variable allows the user to specify a variable name for this purpose, 
     the default is", TT  "w"     
     }

 
document {
     Key => {distinguished, (distinguished,Ideal)},
     Headline => "compute the distinguished subvarieties of a variety",
     Usage => "distinguished I" ,
     Inputs =>  {"I" => {"an ", ofClass Ideal, "over a ", 
	  ofClass PolynomialRing}},
     Outputs => {{"a ", ofClass List, " of ideals defining the components 
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
     Inputs => { "I" => {"an ", ofClass Ideal, " over a ", 
	  ofClass PolynomialRing}},
     Outputs => {{"a ", ofClass List, " of pairs where the first entry 
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
     Key => [specialFiber, Strategy],
     Headline=> "Allows the user to define the matrix, ideal, or module over 
     a polynomial ring and then set an ideal to compute the special fiber 
     over a quotient ring defined by Strategy"
     }

document {
     Key => [specialFiber, Variable],
     Headline=> "specialFiber introduces new variables and the option 
     Variable allows the user to specify a variable name for this purpose, 
     the default is", TT  "w"     
     }


document {
     Key => (specialFiber,Matrix),
     Headline => "compute the special fiber of the image of a matrix over 
     a quotient ring",
     Usage =>  "specialFiber(f)",
     Inputs =>  {"f" => {"a ", ofClass Matrix}},
     Outputs => {{"an ", ofClass Ideal, " defining the special fiber of the 
	       image of ", TT "f"}},
     "Stuff."
     }
document { 
     Key => (specialFiber,Module), 
     Headline => "compute the special fiber of the image of a matrix over a", 
     "a quotient ring",
     Usage =>  "specialFiber(M)",
     Inputs =>  {"f" => {"a ", ofClass Module}},
     Outputs => {{"an ", ofClass Ideal, " defining the special fiber"}},
     "Stuff."
     }

document { 
     Key => (specialFiber,Ideal),
     Headline => "compute the special fiber of the image of a matrix over 
     a quotient ring",
     Usage =>  "specialFiber(J)",
     Inputs =>  {"f" => {"a ", ofClass Ideal}},
     Outputs => {{"an ", ofClass Ideal, " defining the special fiber"}},
     "Stuff."
     }

document {
     Key => analyticSpread, 
     Headline => "compute the analytic spread"
     }

document {
     Key => [analyticSpread, Strategy],
     Headline=> "Allows the user to define the matrix, ideal, or module over 
     a polynomial ring and then set an ideal to compute the analytic spread 
     over a quotient ring defined by Strategy"
     }

document {
     Key => (analyticSpread,Matrix), 
     Headline => "compute the analytic spread of the image of a matrix 
     over a quotient ring",
     Usage => "analyticSpread(f)",
     Inputs =>  {"f" => {"a ", ofClass Matrix}},
     Outputs => {{"the dimension of the special fiber of the image  
	  of ", TT "f", " over ", TT "R/I", " and is an element 
	   of ", ofClass ZZ}},
     "Stuff."
     }

document {
     Key => (analyticSpread,Module), 
     Headline => "compute the analytic spread of a module over a 
     quotient ring",
     Usage => "analyticSpread(M)",
     Inputs => {"M" => {"a ", ofClass Module}},
     Outputs => {{"the dimension, an element of ", ofClass ZZ, " of the 
	       special fiber of the ideal ", TT "M"}},
               "Stuff."
     }	   

document {
     Key => (analyticSpread,Ideal),
     Headline => "compute the analytic spread of an ideal over a 
     quotient ring",
     Usage => "analyticSpread(J)",
     Inputs =>  {"J" => {"an ", ofClass Ideal}},
     Outputs => {{"the dimension, an element of ", ofClass ZZ, " of the 
	       special fiber of the ideal", TT "J"}},
     "Stuff."
     }

document {
     Key => isLinearType, 
     Headline => "determine if a module is of linear type"
     }

document {
     Key => [isLinearType, Strategy],
     Headline=> "Allows the user to define the matrix, ideal, or module over 
     a polynomial ring and then set an ideal to compute test if it is linear 
     type over a quotient ring defined by Strategy"
     }

document {
     Key => (isLinearType,Matrix), 
     Headline => "determine if the image of a matrix is of linear type",
     Usage =>  "isLinearType(f)",
     Inputs =>  {"f" => {"a ", ofClass Matrix}},
     Outputs => {{"a ", ofClass Boolean, "; true if the image of ", 
	       TT "f", " is of linear type and false otherwise"}},
     "Stuff."
     }

document {
     Key => (isLinearType,Module), 
     Headline => "determine if the image of a matrix is of linear type",
     Usage =>  "isLinearType(M)",
     Inputs =>  {"M" => {"a ", ofClass Matrix}},
     Outputs => {{"a ", ofClass Boolean, "true if the module is of linear 
	  type and false otherwise."}},
     "Stuff."
     }

document {
     Key => (isLinearType,Ideal),
     Headline => "determine if the image of a matrix is of linear type",
     Usage =>  "isLinearType(J)",
     Inputs =>  {"f" => {"a ", ofClass Matrix}},
     Outputs => {{"a ", ofClass Boolean, "true if the ideal is of linear 
	  type and false otherwise."}},
     "Stuff."
     }


document {
     Key => {idealIntegralClosure, (idealIntegralClosure,Ideal)},
     Headline => "compute the integral closure of an ideal",
     Usage =>  " idealIntegralClosure(I)",
     Inputs =>  {"I" => {"an ", ofClass Ideal, " over a ", 
	  ofClass PolynomialRing}},
     Outputs => {{"an ", ofClass Ideal, " computed as the degree 1 piece of 
	  the integral closure of the Rees algebra of ", TT "I"}},
     "Stuff."
     }

document {
     Key => {multiplicity,(multiplicity,Ideal)},
     Headline => "compute the multiplicty of an ideal",
     Usage =>  "multiplicity I",
     Inputs =>  {"I" => {"an ", ofClass Ideal, "over a ", 
     ofClass PolynomialRing}},
     Outputs => {{"an integer in ", ofClass ZZ, " that is the classical 
	  multiplicity of ", TT "I", " that is the normalized leading 
	  coefficient of the Rees algebra of", TT "I"}},
     "Stuff."
     }

-- symmetricKernel, rees, reesClassic, analyticSpread, 
-- distinguishedAndMult, specialFiber, distinguished, universalEmbedding, 
-- idealIntegralClosure, isLinearType, multiplicity

///
restart
load "rees_eisenbud_me.m2"
R=QQ[a..e]
j=monomialCurveIdeal(R, {1,2,3,4})
I=ideal(0_R)
IS = symmetricKernel(gens j)
reesClassic(j)
M = coker gens j
IM = rees(M)
IR= time rees(j)
betti gens IR
degrees source vars ring IR
specialFiber(j, Strategy => I)
analyticSpread(j, Strategy => I)
----
restart
load( "rees_eisenbud_me.m2")
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
///
///
fu=universalEmbedding(I,f)   -- f = ????
betti symmetricKernel(I,fu)
betti symmetricKernel(I,f)
///
///
restart
load "rees_eisenbud_me.m2"
--kk=ZZ/32003
x = symbol x
R=ZZ/32003[x,y,z]
i=intersect(ideal(x),(ideal(x,y))^2, (ideal(x,y,z))^3)
distinguished i
use R
i
distinguishedAndMult i
///


end
installPackage "ReesAlgebra"
