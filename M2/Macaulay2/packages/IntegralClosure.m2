newPackage(
	"IntegralClosure",
    	Version => "1.0", 
    	Date => "April 4, 2008",
    	Authors => {
	     {Name => "Amelia Taylor",
	     HomePage => "http://faculty1.coloradocollege.edu/~ataylor/",
   	     Email => "amelia.taylor@coloradocollege.edu"}},
    	Headline => "Integral Closure",
    	DebuggingMode => false
    	)
   
export{isNormal, integralClosure, conductor, ICfractions, ICmap,
idealizerReal, nonNormalLocus, Index}

needsPackage "Elimination"


-- PURPOSE : Front end for the integralClosure function.  It governs the 
--           iterative process using the helper function next.
-- INPUT : any Ring that is a polynomial ring or a quotient of a polynomial
--         ring that is reduced. 
-- OUTPUT : a sequence of quotient rings R_1/I_1,..., R_n/I_n such that the integral closure 
--          of R/I is the direct sum of those rings
-- HELPER FUNCTIONS: next       - facilitiates the iteration.
--                   newICnode  - builds a mutable hash table used to track the pieces during the iteration.
--                   normal0  - computes the non-normal locus
--                   idealizer0 - DeJong's algorthim to compute the integral closure using the non-normal locus.
--                   indirectly, radical
-- COMMENT: The quotient rings are not necessarily domains.  The algorithm can correctly 
--          proceed without decomposing a reduced ring if it finds a non-zero divisor with 
--          which to compute 1/f(fJ:J).
integralClosure = method(Options=>{Variable => global w, Limit => infinity})
integralClosure Ring := Ring => o -> (R) -> (
     -- 1 arguments: Quotient ring. 
     -- 2 options: 
     -- return: The quotient ring that is the integral closure of R.
     M := flattenRing R;
     ICout := integralClosureHelper(nonNormalLocus M_0, gens M_0 ,M_1,o.Limit, o.Variable, 0);
     if #ICout == 2 then (
	  R.ICfractions = ICout#1;
     	  R.ICmap = ICout#0;
     	  target ICout#0
	  )
     else (
	  n := substitute((#ICout)/2, ZZ);
	  ICout = apply(n-1, i -> {ICout#(2*i), ICout#(2*i+1)});
	  R.ICfractions = apply(ICout, i -> i#1);
	  R.ICmap = apply(ICout, i -> i#0);
	  apply(R.ICmap, i -> target i)
	  )
     )

integralClosureHelper = (J, fractions, phi, counter, newVar, indexVar) -> (
     -- recursive helper function designed to build the integral
     -- closure of R = S/I.
     -- 6 arguments: J is in the non normal locus of the target of
     -- phi, answer is a set containg the relevent maps, fractions is a
     -- list of the fractions needed, numNewVars is a counter to keep
     -- track of the number of new variables being added, counter
     -- keeps track of the depth of recursion.
     -- return:  
     S := target phi;
     I := ideal presentation target phi;
     J1 := ideal(0_S):J_0; 
     -- need to check if J_0 is really the element we-- want - low degree. 
     if J1 != ideal(0_S) then(
	  -- If Jc_0 is a ZD then we split the ring.
	  S1 := flattenRing(S/J1);
	  S2 := flattenRing(S/(ideal(0_S):J1));
	  L := join(integralClosureHelper(nonNormalLocus(minimalPresentation S1_0), 
	       	    fractions,
		    ((S1_0).minimalPresentationMap)*(S1_1)*map(source S1_1, S)*phi, 
		    counter-1, newVar, indexVar),
	       integralClosureHelper(nonNormalLocus (minimalPresentation S2_0), 
			 fractions, 
			 ((S2_0).minimalPresentationMap)*(S2_1)*map(source S2_1, S)*phi, 
			 counter-1, newVar, indexVar));
	  return L
	  )	
     else(
	  -- If J_0 is a NZD then we continue setting f = J_0.
	  -- Compute Hom_R(J,J), with R = S/I.
	  -- From now on, we work in this quotient:
	  (newPhi, fracs) := idealizerReal(J, J_0, Variable => newVar, Index => indexVar);  
	  targ := target newPhi;
	  if targ == S then (
	       return {newPhi*phi, join(fracs,fractions)})
	  else (
	       newI1 := ideal presentation targ;
	       newJ1 := newPhi J;
	       newI := minimalPresentation(newI1);
	       S = ring newI;
	       B2 := S/newI;
	       FF := substitute(((newI1).cache.minimalPresentationMap).matrix, B2);
	       F := map(B2,target newPhi, FF);
	       return integralClosureHelper(radical(F newJ1), join(fracs,fractions), F*newPhi*phi, counter-1,newVar,indexVar + # gens targ - # gens source newPhi )  
     	       );
     	  );
     )

idealizerReal = method(Options=>{Variable => global w, Index => 0})
idealizerReal (Ideal, Thing) := o -> (J, f) -> (
     -- 3 arguments: An ideal J in the non normal locus of a ring R/I,
     -- f a non-zero divisor in R/I, and w is the new variable in use. 
     -- return: a sequence consisting of a ring map from the ring of J to
     -- B/I, where B/I is isomorphic to Hom(J,J) = 1/f(f*J:J), and
     -- list of the fractions that are added to the ring of J to form B/I.   
     R := ring J;
     I := ideal presentation R;
     idJ := mingens(f*J : J);
     if ideal(idJ) == ideal(f) then (
	  (map(R,R), {})) -- in this case R is ismorphic to Hom(J,J).
     else(
     	  H := compress (idJ % f);
     	  fractions := apply(first entries H,i->i/f);
     	  Hf := H | matrix{{f}};
     	  -- Make the new polynomial ring.
     	  n := numgens source H;
     	  newdegs := degrees source H - toList(n:degree f);
     	  degs = join(newdegs, (monoid R).Options.Degrees);
     	  MO := prepend(GRevLex => n, (monoid R).Options.MonomialOrder);
          kk := coefficientRing R;
     	  A := (if any(degs, d -> d#0 <= 0) then (
	       	    kk(monoid [o.Variable_(o.Index)..o.Variable_(o.Index+n-1), gens R,
			      MonomialOrder=>MO])) -- removed MonomialSize => 16
	       else(
	       	    kk(monoid [o.Variable_(o.Index)..o.Variable_(o.Index+n-1), gens R,
			      MonomialOrder=>MO, Degrees => degs]))-- removed MonomialSize => 16
	       );	 
     	  IA := ideal ((map(A,ring I,(vars A)_{n..numgens R + n-1})) (generators I));
     	  B := A/IA;
     	  varsB := (vars B)_{0..n-1};
     	  RtoB := map(B, R, (vars B)_{n..numgens R + n - 1});
     	  XX := varsB | matrix{{1_B}};
     	  -- Linear relations in the new variables
     	  lins := XX * RtoB syz Hf; 
     	  -- linear equations(in new variables) in the ideal
     	  -- Quadratic relations in the new variables
     	  tails := (symmetricPower(2,H) // f) // Hf;
     	  tails = RtoB tails;
     	  quads := matrix(B, entries (symmetricPower(2,varsB) - XX * tails));
     	  B2 := (flattenRing(B/(trim (ideal lins + ideal quads))))_0;
     	  F := map(B2, R, (vars B2)_{n..numgens R + n - 1});
	  (F, fractions)
	  )
     )


nonNormalLocus = method()
nonNormalLocus Ring := (R) -> (
     -- This handles the first node: finding an ideal that contains the NNL 
     -- locus.  
     local J;
     I := ideal presentation R;
     Jac := jacobian R;
     --error "check R and I";
     if isHomogeneous I and #(first entries generators I)+#(generators ring I) <= 20 then (
	  SIdets := minors(codim I, Jac);
	   -- the codimension of the singular locus.
	  cs := codim SIdets + codim R;  -- codim of SIdets in poly ring. 
	  if cs === dim ring I or SIdets == 1
	  -- i.e. the sing locus is empty.
	  then (J = ideal vars R;)
	  else (J = radical ideal SIdets_0);
	  )           	       
     else (
	  n := 1;
	  det1 := ideal (0_R);
	  while det1 == ideal (0_R) do (
	       det1 = minors(codim I, Jac, Limit=>n); -- this seems
	       -- very slow - there must be a better way!!  
	       n = n+1);
	     if det1 == 1
	     -- i.e. the sing locus is empty.
	     then (J = ideal vars R;)
	     else (J = radical det1)
	     );	 
     J
     )

-- PURPOSE: check if an affine domain is normal.  
-- INPUT: any quotient ring.  
-- OUTPUT:  true if the ring is normal and false otherwise. 
-- COMMENT: This computes the jacobian of the ring which can be expensive.  
-- However, it first checks the less expensive S2 condition and then 
-- checks R1.  
isNormal = method()     
isNormal(Ring) := Boolean => (R) -> (
     --Input:  Takes a quotient ring. 
     --Method:  Check if the Jacobian ideal of R has
     --codim >= 2, if true then check the codimension 
     --of Ext^i(S/I,S) where R = S/I and i>codim I. If 
     --the codimensions are >= i+2 then return true.
     I := ideal (R);
     M := cokernel generators I;
     n := codim I;         
     test := apply((dim ring I)-n-1,i->i);
     if all(test, j -> (codim Ext^(j+n+1)(M,ring M)) >= j+n+3) 
     then ( 
	  Jac := minors(n,jacobian R);  
	  dim R - dim Jac >=2)
     else false
     )

-- Needed because we have problems with multigraded over char 0  in 
-- using the pushForward function and 
-- have to kill the multigrading in this case at the very beginning.
isSinglyGraded := (R) -> (
     n := numgens (ring presentation R).degreesRing;
      n===1)     

--------------------------------------------------------------------
conductor = method()
conductor(RingMap) := Ideal => (F) -> (
     --Input:  A ring map where the target is finitely generated as a 
     --module over the source.
     --Output: The conductor of the target into the source.
     --NOTE:  If using this in conjunction with the command normalization,
     --then the input is R#IIICCC#"map" where R is the name of the ring used as 
     --input into normalization.  
     if isHomogeneous (source F)
     	  then(M := presentation pushForward(F, (target F)^1);
     	       P := target M;
     	       intersect apply((numgens P)-1, i->(
	       m:=matrix{P_(i+1)};
	       I:=ideal modulo(m,matrix{P_0}|M))))
	  else error "conductor: expected a homogeneous ideal in a graded ring"
     )

--------------------------------------------------------------------
beginDocumentation()

document {
     Key => IntegralClosure,
     ---------------------------------------------------------------------------
     -- PURPOSE : Compute the integral closure of a ring via the algorithm 
     --           in Theo De Jong's paper, An Algorithm for 
     --           Computing the Integral Closure, J. Symbolic Computation, 
     --           (1998) 26, 273-277. 
     -- PROGRAMS : integralClosure
     --            isNormal
     --            ICFractions
     --            ICMap
     -- The fractions that generate the integral closure over R/I are obtained 
     -- by the command ICfractions(R/I).  
     -- Included is a command conductor that computes the conductor of S into R
     -- where S is the image of a ring map from R to S where S is finite over
     -- R.  ICmap constructs this map (the natural map R/I->R_j/I_j) for R 
     -- into its integral closure and applying conductor to this map 
     -- yeilds the conductor of the integral closure into R.

     -- PROGRAMMERs : This implementation was written by and is maintained by
     --               Amelia Taylor.  
     -- UPDATE HISTORY : 25 October 2006
     ---------------------------------------------------------------------------
     PARA {
	  "This package computes the integral closure of a ring via the algorithm 
	  in Theo De Jong's paper, ", EM "An Algorithm for 
	  Computing the Integral Closure", ", J. Symbolic Computation, (1998) 26, 273-277.
	  The fractions that generate the integral closure over R are obtained 
     	  with the command ", TT "ICfractions R", "."
	  }
     }

document {
     Key => {isNormal, (isNormal, Ring)},
     Headline => "determine if a reduced ring is normal",
     Usage => "isNormal R",
     Inputs => {"R" => {ofClass Ring}},
     Outputs => {{ofClass Boolean, " that is true if ", TT "R", " is normal in the 
	       sense of satisfying Serre's conditions S2 and R1 and false if one or 
	       both conditions fail"}},      
     EXAMPLE{
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
	  "isNormal R",
	  "isNormal(integralClosure R)",
	  },
     PARA{},
     "This function computes the jacobian of the ring which can be costly for 
     larger rings.  Therefore it checks the less coslty S2 condition first and if 
     true, then tests the R1 condition using the jacobian of ", TT "R", "."
     }	 

document {
     Key => integralClosure,
     Headline => "compute the integral closure of a ring",
     "The code for this function allows users to retrieve 
     certain information if desired.  
     The information of largest interest is the fractions that 
     correspond to the added variables in this description of 
     the integral closure.  Unfortunately, all of the added features 
     currently only work on affine domains.
     The map and the corresponding fractions are obtained as 
     a matrix using the function ", TO "ICfractions", " where R is 
     an affine domain.  This function can be run without first 
     using ", TO "integralClosure", ".  The natural map from ", TT "R", " into 
     its integral closure is obtained using the function ", TO "ICmap", " and 
     the conductor of the integral closure of R into R is found 
     using ", TT "conductor (ICmap R)", ".  Note that 
     both ", TO "ICfractions", " and ", TO "ICmap", " take the input 
     ring ", TT "R", " as input rather than the output 
     of ", TO "integralClosure", ".  In this way you can use these 
     functions without running ", TO "integralClosure", ".  The 
     function ", TO "integralClosure", " is based on
     Theo De Jong's paper, An Algorithm for 
     Computing the Integral Closure, J. Symbolic Computation, 
     (1998) 26, 273-277.  This implementation is written and maintained 
     by Amelia Taylor, ", HREF {"mailto:amelia.taylor@coloradocollege.edu", 
     "<amelia.taylor@coloradocollege.edu>"}, ".",
     SeeAlso => {"ICmap", "ICfractions", "conductor", "isNormal"}
     }

document {
     Key => (integralClosure,Ring),
     Headline => "compute the integral closure of a reduced ring",
     Usage => "integralClosure R",
     Inputs => {
	  "R" => {"that is reduced and presented as a quotient ring"},
	  Variable => {"an unassigned symbol"},
	  },
     Outputs => {{"that is the integral closure of ", TT "R", " in its total 
	       ring of fractions when ", TT "R", " is a domain.  When ", TT "R", 
	       " is reduced, a list of rings is returned with the property that the 
	       direct product of the rings is isomorphic to the integral closure 
	       of ", TT "R", ". The output rings use new indexed variables based 
	       on the symbol ", TT "w"}
	  },
     "A domain example.",
      EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
      	  "integralClosure R"},
     "In the case that the input ring ", TT "R", " is reduced, but not a domain, 
     the direct sum of the rings returned is isomporphic to the integral closure 
     of ", TT "R", ", but the rings returned are not necessarily all domains.",
     EXAMPLE{"S=ZZ/101[a..d]/ideal(a*(b-c),c*(b-d),b*(c-d));",
	  "integralClosure S"
	  },
     "The algorithm can correctly  proceed without decomposing a reduced ring 
     if it finds a non-zero divisor ", TT "f", " with which to compute 1/f(fJ:J), 
     where ", TT "J", " is the ideal defining the non-normal locus at that stage.",
     PARA{},
     "A package implementing product rings is in development.  When this package is 
     complete, the interface for the integralClosure code will change.  
     In particular, in the case of a non-domain, a quotient ring of a polynomial ring  
     isomorphic to the direct product of the factors formed during the computation 
     will be returned. The individual 
     factors will be available either as a part of the ring, or via a function and in 
     this case, all the rings returned will be domains.  This provides two advantages: 
     first access to all the factors as domains and the ability to use other functions 
     described below in the reduced case.",  
     PARA{},
     "The code for this function allows users to retrieve 
     certain information if desired.  
     The information of largest interest is the fractions that 
     correspond to the added variables in this description of 
     the integral closure.  Unfortunately, all of the added features 
     currently only work on affine domains.
     The map and the corresponding fractions are obtained as 
     a matrix using the function where R is 
     an affine domain.  This function can be run without first 
     using ", TO "integralClosure", ".  The natural map from ", TT "R", " into 
     its integral closure is obtained using the function ", TO "ICmap", " and 
     the conductor of the integral closure of R into R is found 
     using ", TT "conductor (ICmap R)", ".  Note that 
     both ", TO "ICfractions", " and ", TT "ICmap", " take the input 
     ring ", TT "R", " as input rather than the output 
     of ", TO "integralClosure", ".  In this way you can use these 
     functions without running ", TT "integralClosure", ".",
     SeeAlso => {"conductor", "isNormal"},
     }
    
document {
     Key => [integralClosure,Variable],
     Headline=> "Sets the name of the indexed variables introduced in computing 
     the integral closure of a reduced ring."
     }

--document {
--     Key => {ICmap, (ICmap,Ring)},
--    Headline => "natural map from an affine domain into its integral closure.",
--     Usage => "ICmap R",
--     Inputs => {
--	  "R" => {ofClass Ring, " that is an affine domain"}
--	  },
--     Outputs => {
--	  {ofClass RingMap, " from ", TT "R", " to its integral closure"}
--	  },
--    "Note that if an integrally closed ring is given as input, the identity map from 
--     the ring to itself is returned.",
--    	  EXAMPLE {
--	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
--      	  "ICmap R"},
--     PARA{},
--     "This finite map is needed to compute the ", TO "conductor", " of the integral closure 
--     into the original ring.",
--     SeeAlso => {"conductor"}
--     }
    

--document {
 ---    Key => {ICfractions, (ICfractions,Ring)},
--     Headline => "Compute the fractions integral over a domain.",
--     Usage => "ICfractions R",
--     Inputs => {
--	  "R" => {ofClass Ring, " that is an affine domain"},
--	  },
 --    Outputs => {
--	  {ofClass Matrix, " whose entries are fractions that generate the integral 
--	       closure of ", TT "R", " over R."}
--	  },
 --    EXAMPLE {
--	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
 --     	  "ICfractions R",
--	  "integralClosure(R,Variable => a)"
--	  },
 --    "Thus the new variables ", TT "w_7", " and ", TT "w_6", " correspond to the 
  --   fractions respectively.  The program currently also returns the original 
   --  variables as part of the matrix.  In this way the user can see if any are 
    -- simplified out of the ring during the process of computing the integral
    -- closure.",
    -- PARA{},
    -- "The fractions returned correspond to the variables returned by the function 
    -- integralClosure.  The function integralClosure eliminates redundant fractions 
    -- during its iteration.  If the user would like to see all fractions generated 
    -- during the computation, use the optional argument ", TT "Strategy => Long", " as 
    -- illustrated here.",
    -- EXAMPLE {
--	  "ICfractions(R, Strategy => Long)"
--	  },
--     }

--document {
--     Key => [ICfractions,Strategy],
--     Headline=> "Allows the user to obtain all of the fractions considered in the 
--     process of building the integral closure",
--     }

document {
     Key => {conductor,(conductor,RingMap)},
     Headline => "compute the conductor of a finite ring map",
     Usage => "conductor F",
     Inputs => {
	  "F" => {ofClass RingMap, " from a ring ", TT "R", " to a ring ", TT "S", 
	       ". The map must be a finite"},
	  },
     Outputs => {
	  {ofClass Ideal, " that is the conductor of ", TT "S", " into ", TT "R", "."}
	  },
     "Suppose that the ring map F : R --> S is finite: i.e. S is a finitely 
     generated R-module.  The conductor of F is defined to be {",
     TEX "g \\in R \\mid g S \\subset f(R)", "}.  One way to think
     about this is that the conductor is the set of universal denominators
     of ", TT "S", " over ", TT "R", ", or as the largest ideal of ", TT "R", " 
     which is also an ideal in ", TT "S", ".",
     EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
	  "F = R.ICmap",
	  "conductor F"
	  },
     PARA{},
     "The command ", TT "conductor", " calls the 
     command ", TO pushForward, ".  Currently, the 
     command ", TT "pushForward", 
     " does not work if the source of the map ", TT "F", " is
     inhomogeneous.  If the source of the map ", TT "F", " is not
     homogeneous ", TT "conductor", " returns the message -- No
     conductor for ", TT "F", ".",
     SeeAlso =>{"pushForward", "integralClosure"} 
     }

-- integrally closed test
TEST ///
R = QQ[u,v]/ideal(u+2)
time J = integralClosure (R,Variable => symbol a) 
use ring ideal J
assert(ideal J == ideal(u+2))
///

-- degrees greater than 1 test
TEST ///
R = ZZ/101[symbol x..symbol z,Degrees=>{2,5,6}]/(z*y^2-x^5*z-x^8)
time J = integralClosure (R,Variable => symbol b) 
use ring ideal J
-- the engine has changed
oldIdeal = ideal(b_1*x^2-y*z, x^6-b_1*y+x^3*z, -b_1^2+x^4*z+x*z^2)
-- assert(ideal J == oldIdeal)
newIdeal = substitute(oldIdeal, b_1 => b_1/42 )
assert(ideal J == newIdeal)
-- assert(ICfractions R == substitute(matrix {{y*z/x^2, x, y, z}},frac R))
--assert(ICfractions R == substitute(matrix {{42 * y*z/x^2, x, y, z}},frac R))
///

-- multigraded test
TEST ///
R = ZZ/101[symbol x..symbol z,Degrees=>{{1,2},{1,5},{1,6}}]/(z*y^2-x^5*z-x^8)
time J = integralClosure (R,Variable=>symbol a) 
use ring ideal J
assert(ideal J == ideal(x^6+a_7*y+x^3*z-11*x*y^2,a_7*x^2-11*x^3*y+y*z,a_7^2-22*a_7*x*y-x^4*z+20*x^2*y^2-x*z^2))
///

-- Reduced not a domain test
TEST ///
S=ZZ/101[symbol a,symbol b,symbol c, symbol d]
I=ideal(a*(b-c),c*(b-d),b*(c-d))
R=S/I                              
time V = integralClosure R
assert(#V == 2)
///
--is it possible to do a second assert to test the pieces?  

--Craig's example as a test
TEST ///
S=ZZ/101[symbol x,symbol y,symbol z,MonomialOrder => Lex]
I=ideal(x^6-z^6-y^2*z^4)
Q=S/I
time J = integralClosure (Q, Variable => symbol a)
use ring ideal J
assert(ideal J == ideal (x^2-a_6*z, a_6*x-a_7*z, a_6^2-a_7*x, a_7^2-y^2-z^2))
use Q
assert(conductor(Q.ICmap) == ideal(z^3,x*z^2,x^3*z,x^4))
///

--Mike's inhomogenous test
TEST ///
R = QQ[symbol a..symbol d]
I = ideal(a^5*b*c-d^2)
Q = R/I
L = time integralClosure(Q,Variable => symbol x)
use ring ideal L
assert(ideal L == ideal(x_1^2-a*b*c))
///

--Ex from Wolmer's book - tests longer example and published result.
TEST ///
R = QQ[symbol a..symbol e]
I = ideal(a^2*b*c^2+b^2*c*d^2+a^2*d^2*e+a*b^2*e^2+c^2*d*e^2,a*b^3*c+b*c^3*d+a^3*b*e+c*d^3*e+a*d*e^3,a^5+b^5+c^5+d^5-5*a*b*c*d*e+e^5,a^3*b^2*c*d-b*c^2*d^4+a*b^2*c^3*e-b^5*d*e-d^6*e+3*a*b*c*d^2*e^2-a^2*b*e^4-d*e^6,a*b*c^5-b^4*c^2*d-2*a^2*b^2*c*d*e+a*c^3*d^2*e-a^4*d*e^2+b*c*d^2*e^3+a*b*e^5,a*b^2*c^4-b^5*c*d-a^2*b^3*d*e+2*a*b*c^2*d^2*e+a*d^4*e^2-a^2*b*c*e^3-c*d*e^5,b^6*c+b*c^6+a^2*b^4*e-3*a*b^2*c^2*d*e+c^4*d^2*e-a^3*c*d*e^2-a*b*d^3*e^2+b*c*e^5,a^4*b^2*c-a*b*c^2*d^3-a*b^5*e-b^3*c^2*d*e-a*d^5*e+2*a^2*b*c*d*e^2+c*d^2*e^4)
S = R/I
time V = integralClosure (S, Variable => X)
use ring ideal V
assert(
     ideal V == 
      ideal(a^2*b*c^2+b^2*c*d^2+a^2*d^2*e+a*b^2*e^2+c^2*d*e^2,
	   a*b^3*c+b*c^3*d+a^3*b*e+c*d^3*e+a*d*e^3,
	   a^5+b^5+c^5+d^5-5*a*b*c*d*e+e^5,
	   a*b*c^4-b^4*c*d-X_0*e-a^2*b^2*d*e+a*c^2*d^2*e+b^2*c^2*e^2-b*d^2*e^3,
	   a*b^2*c^3+X_1*d+a*b*c*d^2*e-a^2*b*e^3-d*e^5,
	   a^3*b^2*c-b*c^2*d^3-X_1*e-b^5*e-d^5*e+2*a*b*c*d*e^2,
	   a^4*b*c+X_0*d-a*b^4*e-2*b^2*c^2*d*e+a^2*c*d*e^2+b*d^3*e^2,
	   X_1*c+b^5*c+a^2*b^3*e-a*b*c^2*d*e-a*d^3*e^2,
	   X_0*c-a^2*b^2*c*d-b^2*c^3*e-a^4*d*e+2*b*c*d^2*e^2+a*b*e^4,
	   X_1*b-b*c^5+2*a*b^2*c*d*e-c^3*d^2*e+a^3*d*e^2-b*e^5,
	   X_0*b+a*b*c^2*d^2-b^3*c^2*e+a*d^4*e-a^2*b*c*e^2+b^2*d^2*e^2-c*d*e^4,
	   X_1*a-b^3*c^2*d+c*d^2*e^3,X_0*a-b*c*d^4+c^4*d*e,
	   X_1^2+b^5*c^5+b^4*c^3*d^2*e+b*c^2*d^3*e^4+b^5*e^5+d^5*e^5,
	   X_0*X_1+b^3*c^4*d^3-b^2*c^7*e+b^2*c^2*d^5*e-b*c^5*d^2*e^2-
	     a*b^2*c*d^3*e^3+b^4*c*d*e^4+a^2*b^2*d*e^5-a*c^2*d^2*e^5-b^2*c^2*e^6+b*d^2*e^7,
	   X_0^2+b*c^3*d^6+2*b^5*c*d^3*e+c*d^8*e-b^4*c^4*e^2+a^3*c^3*d^2*e^2+
	     2*a^2*b^3*d^3*e^2-5*a*b*c^2*d^4*e^2+4*b^3*c^2*d^2*e^3-3*a*d^6*e^3+
	     5*a^2*b*c*d^2*e^4-b^2*d^4*e^4-2*b*c^3*d*e^5-a^3*b*e^6+3*c*d^3*e^6-a*d*e^8)
	)
///

-- Test of ICfractions
--TEST ///
--S = QQ [(symbol Y)_1, (symbol Y)_2, (symbol Y)_3, (symbol Y)_4, symbol x, symbol y, Degrees => {{7, 1}, {5, 1}, {6, 1}, {6, 1}, {1, 0}, {1, 0}}, MonomialOrder => ProductOrder {4, 2}]
--J = ideal(Y_3*y-Y_2*x^2,Y_3*x-Y_4*y,Y_1*x^3-Y_2*y^5,Y_3^2-Y_2*Y_4*x,Y_1*Y_4-Y_2^2*y^3)
--T = S/J       
--assert(ICfractions T == substitute(matrix {{(Y_2*y^2)/x, (Y_1*x)/y, Y_1, Y_2, Y_3, Y_4, x, y}}, frac T))
----///

-- Test of isNormal
TEST ///
S = ZZ/101[x,y,z]/ideal(x^2-y, x*y-z^2)
assert(isNormal(S) == false)
assert(isNormal(integralClosure(S)) == true)
///

-- Test of ICmap and conductor
TEST ///
R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4)
J = integralClosure(R);
F = R.ICmap
assert(conductor F == ideal((R_2)^3, (R_0)*(R_2)^2, (R_0)^3*(R_2), (R_0)^4))
---///

end 

---- Homogeneous Ex
loadPackage"IntegralClosure"
loadPackage"ParameterSchemes"
R = ZZ/101[x,y, z]
I1 = ideal(x,y-z)
I2 = ideal(x-3*z, y-5*z)
I3 = ideal(x,y)
I4 = ideal(x-5*z,y-2*z)

I = intersect(I1^3, I2^3, I3^3, I4^3)
f = I_0 + I_1 + I_2+ I_3
S = R/f
V = integralClosure(S)
ring(presentation V)

installPackage "IntegralClosure"

-- Tests that Mike has added:
loadPackage "IntegralClosure"
S = ZZ/101[a..d]
I = ideal(b^2-b)
R = S/I
integralClosure(R)

-- M2 crash:
kk = QQ
R = kk[x,y,z]
p1 = ideal"x,y,z"
p2 = ideal"x,y-1,z-2"
p3 = ideal"x-2,y,5,z"
p4 = ideal"x+1,y+1,z+1"
D = trim intersect(p1^3,p2^3,p3^3,p4^3)
betti D
B = basis(4,D)
F = (super(B * random(source B, R^{-4})))_(0,0)
ideal F + ideal jacobian matrix{{F}}
decompose oo

factor F
A = R/F
loadPackage "IntegralClosure"
nonNormalLocus A  -- crashes M2!

ideal F + ideal jacobian matrix{{F}}
decompose oo
-------------------

kk = ZZ/101
R = kk[x,y,z]
p1 = ideal"x,y,z"
p2 = ideal"x,y-1,z-2"
p3 = ideal"x-2,y,5,z"
p4 = ideal"x+1,y+1"
D = trim intersect(p1^3,p2^3,p3^3,p4^2)
betti D
B = basis(5,D)
F = (super(B * random(source B, R^{-5})))_(0,0)
factor F
A = R/F
JF = trim(ideal F + ideal jacobian matrix{{F}})
codim JF
radJF = radical(JF, Strategy=>Unmixed)
-- OK, radical JF doesn't work right now, so do this instead:
jf1 = radical trim eliminate({x},JF)
jf2 = radical trim eliminate({y},JF)
jf3 = radical trim eliminate({z},JF)
jf = trim(JF + jf1 + jf2 + jf3)
NNL = trim radical jf
radJF == NNL
NNL = radJF
NNL = substitute(NNL,A)
(phi,fracs) = idealizerReal(NNL,NNL_0)
phi
#fracs

----------------------
random(ZZ,Ideal) := opts -> (d,J) -> random({d},J,opts)
random(List,Ideal) := opts -> (d,J) -> (
     R := ring J;
     B := basis(6,J);
     (super(B * random(source B, R^(-d), opts)))_(0,0)
     )

kk = ZZ/101
R = kk[x,y,z]
p1 = ideal"x,y,z"
p2 = ideal"x,y-1,z-2"
p3 = ideal"x-2,y,5,z"
p4 = ideal"x+1,y+1"
D = trim intersect(p1^3,p2^3,p3^3,p4^3)
betti D
F = random(6,D)
factor F
A = R/F
JF = trim(ideal F + ideal jacobian matrix{{F}})
codim JF
radJF = radical(JF, Strategy=>Unmixed)
decompose radJF
integralClosure A
