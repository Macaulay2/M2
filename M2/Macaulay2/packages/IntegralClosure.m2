newPackage(
	"IntegralClosure",
    	Version => "0.9", 
    	Date => "February 8, 2009",
    	Authors => {
	     {Name => "Amelia Taylor",
	     HomePage => "http://faculty1.coloradocollege.edu/~ataylor/",
   	     Email => "amelia.taylor@coloradocollege.edu"}},
    	Headline => "Integral Closure",
    	DebuggingMode => true
    	)
 --
 -- Needs updating in the comments and the documentation.  Needs Irena
-- code added in as an option as well as needs icFractions to be up
-- and running again.  Also needs updating the tests.  Are there
-- others where we know the answer in advance?  What about how this
-- program interacts with minPressy?  
-- Do I want Limit on integralClosure - how will we use this?
-- Do we still need isSinglyGraded?  i.e. is pushForward still a
-- problem?
   
export{"integralClosure", "idealizer", "nonNormalLocus", "Index",
"isNormal", "conductor", "icFractions", "icMap", "icFracP", "conductorElement",
"reportSteps", "icPIdeal"} 

needsPackage "Elimination"

-- PURPOSE : Front end for the integralClosure function.  It governs
--           the iterative process using the helper function
--           integralClosureHelper. Generally implements DeJong's
--           algorthim to compute the integral closure using the
--           non-normal locus.
-- INPUT : any Ring that is a polynomial ring or a quotient of a
--         polynomial ring that is reduced. 
-- OUTPUT : a sequence of quotient rings R_1/I_1,..., R_n/I_n such
--          that the integral closure of R/I is the direct sum of
--          those rings 
-- HELPER FUNCTIONS: integralClosureHelper      
--                   nonNormalLocus  - computes the non-normal locus
--                   idealizer - a sequence consisting of a ring map
--                   from the ring of J to B/I, where B/I is
--                   isomorphic to Hom(J,J) = 1/f(f*J:J), and list of
--                   the fractions that are added to the ring of J to
--                   form B/I  .
-- COMMENTS: 
-- (1) The quotient rings are not necessarily domains.  The
-- algorithm can correctly proceed without decomposing a reduced ring
-- if it finds a non-zero divisor with which to compute 1/f(fJ:J). 
--
-- (2) The functional design is to allow a user to do individual steps
-- in DeJong's algorithm easily.  In particular, nonNormalLocus and
-- idealizer are now stand alone functions.  This could alow study of
-- the role of different choices of the non-zero element f in
-- idealizer, or of different possibly choices that "work" for the
-- nonNormalLocus. 

--- Should Singh/Swanson be an option to integralClosure or its own
--- program.  Right now it is well documented on its own.  I'm not
--- sure what is best long term. 

integralClosure = method(Options=>{
	  Variable => global w,
	  Limit => infinity})


-- version that MES has just provided, not in use in 1.2 yet
integralClosure Ring := Ring => o -> (R) -> (
     -- 1 argument: Quotient ring. 
     -- 3 options: the variable name for new variables, a limit on the
     -- number of times to run the recurions and the choice to run
     -- Singh and Swansons characteristic p algorithm. 
     -- Return: The quotient ring that is the integral closure of R or
     -- a set of rings whose direct sum is the integral closure of R.
     -- Method:  We work primarily with maps to ensure access to key
     -- information at the end.  This also makes it easier to keep
     -- track of ring flattenings and the recursion. 
     (S,phi) := flattenRing R;
     G := map(frac R, frac S, substitute(phi^-1 vars S, frac R));
     (newPhi,newG) := mikeIntegralClosureHelper(nonNormalLocus S, phi, G, o.Limit, o.Variable, 0);
     R.icFractions = newG.matrix;
     R.icMap = newPhi;
     target newPhi
     )

integralClosure Ring := Ring => o -> (R) -> (
     -- 1 argument: Quotient ring. 
     -- 3 options: the variable name for new variables, a limit on the
     -- number of times to run the recurions and the choice to run
     -- Singh and Swansons characteristic p algorithm. 
     -- Return: The quotient ring that is the integral closure of R or
     -- a set of rings whose direct sum is the integral closure of R.
     -- Method:  We work primarily with maps to ensure access to key
     -- information at the end.  This also makes it easier to keep
     -- track of ring flattenings and the recursion. 
     M := flattenRing R;
     ICout := integralClosureHelper(nonNormalLocus M_0, gens M_0, M_1,o.Limit, o.Variable, 0);
     if #ICout == 2 then (
	  R.icFractions = ICout#1;
	  --R.icFractionsShort = ICout#2;
	  R.icMap = ICout#0;
	  target ICout#0
	  )
     else (
	  n := substitute((#ICout)/2, ZZ);
	  ICout = apply(n-1, i -> {ICout#(2*i), ICout#(2*i+1)});
	  R.icFractions = apply(ICout, i -> i#1);
	  R.icMap = apply(ICout, i -> i#0);
	  RIdeal := apply(R.icMap, i -> trim ideal target i);
	  apply(RIdeal, i -> (ring i)/i)
	  )
     )

-- Not in use in 1.2 yet
mikeIntegralClosureHelper = (J, phi, G, counter, newVar, indexVar) -> (
     -- recursive helper function designed to build the integral
     -- closure of R
     --
     -- phi : R --> S, where R is the original ring whose integral closure we seek
     --   and S is the current approximation to the integral closure
     -- G : frac S --> frac R, is the representation as fractions of R.
     -- J: an ideal in S s.t. V(J) is contained in the non-normal locus
     -- counter: used to do partial computation: number of loops left (possibly Infinity)
     -- newVar: a symbol which we use for new variables
     -- indexVar: the next index to use for newVar.
     -- Return value:
     --   In the domain case:
     --    (phi', G')
     --   Otherwise:
     --     Good question!
     if counter == 0 then return (phi, G);
     S := target phi;
     J0 := findSmallGen J;
     J1 := trim(ideal(0_S):J0); 
     if J1 != 0 then error "ring is not a domain";
     -- Compute Hom_S(J,J), using J0 as the common denominator.
     (newPhi, newG, fracs) := idealizer(J, J0, Variable => newVar, Index => indexVar);
     T := target newPhi;
     n := numgens T - numgens S;
     if T === S then return (phi, G);
     phi = newPhi*phi; -- phi : R --> T
     G = G*newG; -- G : frac T --> frac R.     
     -- Otherwise, let's clean up T, and modify the maps phi, G.
     -- we get a ring U isom to T, with T --> U, U --> T.
     U1 := minimalPresentation T;
     i1 := T.minimalPresentationMap; -- T --> U1
     i2 := T.minimalPresentationMapInv; -- U1 --> T
     gt := map(frac T, frac U1, substitute(i2.matrix,frac T));
     mikeIntegralClosureHelper(radical((i1*newPhi) J), i1*phi, G*gt, counter-1, newVar, indexVar+n)
     )

findSmallGen = (J) -> (
     a := toList((numgens ring J):1);
     L := sort apply(J_*, f -> ((weightRange(a,f))_1, size f, f));
     --<< "first choices are " << netList take(L,3) << endl;
     L#0#2
     )

integralClosureHelper = (J, fractions, phi, counter, newVar, indexVar) -> (
     -- recursive helper function designed to build the integral
     -- closure of R = S/I.
     -- 6 arguments: J is in the non normal locus of the target of
     -- phi, is the composition of the relevent maps, fractions is a
     -- list of the fractions being added, newVar is the letter used
     -- for the new variables and indexVar keeps track of the current
     -- index to be used for the new variables. 
     -- track of the number of new variables being added, counter
     -- keeps track of the depth of recursion.
     -- return:  a list consisting of maps and fractions.
     if counter == 0 then return (phi, fractions);
     S := target phi;
     I := ideal presentation target phi;
     R := ring I;

     J0 := findSmallGen J;
     J1 := trim(ideal(0_S):J0); 
     -- need to check if J0 is really the element we-- want - low degree. 
     if J1 != ideal(0_S) then(
	  -- If J0 is a ZD then we split the ring.
	  -- need to try and clean up ideals as much as possible as we proceed.
	  (S1, S1Map) := flattenRing(R/trim(substitute(J1, R) + I));
	  (S2, S2Map) := flattenRing(R/trim(substitute(ideal(0_S):J1, R) + I));
	  L := join(integralClosureHelper(nonNormalLocus (minimalPresentation S1), 
	       	    fractions,
		    (S1.minimalPresentationMap)*(S1Map)*map(source S1Map, S)*phi, 
		    counter-1, newVar, indexVar),
	       integralClosureHelper(nonNormalLocus (minimalPresentation S2), 
			 fractions, 
			 (S2.minimalPresentationMap)*(S2Map)*map(source S2Map, S)*phi, 
			 counter-1, newVar, indexVar));
	  return L
	  )	
     else(
	  -- If J0 is a NZD then we continue setting f = J0.
	  -- Compute Hom_R(J,J), with R = S/I.
	  -- From now on, we work in this quotient:
	  (newPhi, fracs) := idealizer(J, J0, Variable => newVar, Index => indexVar);  
     	  --error "check targ";
	  targ := target newPhi;
	  if targ === S then (
	       return {newPhi*phi, join(fracs,fractions)})
	  else (
	       newI1 := trim ideal presentation targ;
	       newJ1 := newPhi J;
	       newI := minimalPresentation(newI1);
	       S = ring newI;
	       B2 := S/newI;
	       FF :=
	       substitute(((newI1).cache.minimalPresentationMap).matrix, B2);
	       F := map(B2,target newPhi, FF);
	       --error("check Maps");
	       return integralClosureHelper(radical(F newJ1), join(fracs,fractions), F*newPhi*phi, counter-1,newVar,indexVar + # gens targ - # gens source newPhi )  
     	       );
     	  );
     )


idealizer = method(Options=>{Variable => global w, Index => 0})
idealizer (Ideal, RingElement) := o -> (J, f) -> (
     -- J is an ideal in a ring R
     -- f is a nonzero divisor in J
     -- computes a ring B2 = Hom(J,J) = (f*J:J)/f
     -- returns a sequence (F,fractions), where 
     --   F : R --> B2 is the natural inclusion
     --   fractions: a list of the images of the new generators in frac R.
     -- optional arguments:
     --   o.Variable: base name for new variables added
     --   o.Index: the first subscript to use for such variables
     R := ring J;
     I := ideal presentation R;
     idJ := mingens(f*J : J);
     if ideal(idJ) == ideal(f) then (
	  (id_R, {})) -- in this case R is isomorphic to Hom(J,J).
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
	  --newVars := reverse apply(n, i -> o.Variable_(o.Index+i));
	  -- o.Variable_(o.Index)..o.Variable_(o.Index+n-1)
     	  A := kk(monoid [o.Variable_(o.Index)..o.Variable_(o.Index+n-1), gens R,
		    MonomialOrder=>MO, Degrees => degs]);
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
     -- This handles the first key step in DeJong's algorithm: finding
     -- an ideal that contains the NNL locus. 
     -- 1 argument: a ring. it must be flattened. normally it will be
     -- a quotient ring. 
     -- Return: an ideal containing the non-normal locus of R.   
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
     -- 1 argument:  A ring - usually a quotient ring. 
     -- Return: A boolean value, true if the ring is normal and false
     -- otherwise. 
     -- Method:  Check if the Jacobian ideal of R has
     -- codim >= 2, if true then check the codimension 
     -- of Ext^i(S/I,S) where R = S/I and i>codim I. If 
     -- the codimensions are >= i+2 then return true.
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

icMap = method()
icMap(Ring) := RingMap => R -> (
     -- 1 argument: a ring.  May be a quotient ring, or a tower ring.
     -- Returns: The map from R to the integral closure of R.  
     -- Note:  This is a map where the target is finitely generated as
     -- a module over the source, so it can be used as the input to
     -- conductor and other methods that require this. 
     if R.?icMap then R.icMap
     else if isNormal R then id_R
     else (S := integralClosure R;
	  R.icMap)
     )

     

///
restart
loadPackage"IntegralClosure"
S = QQ [(symbol Y)_1, (symbol Y)_2, (symbol Y)_3, (symbol Y)_4, symbol x, symbol y, Degrees => {{7, 1}, {5, 1}, {6, 1}, {6, 1}, {1, 0}, {1, 0}}, MonomialOrder => ProductOrder {4, 2}]
J =
ideal(Y_3*y-Y_2*x^2,Y_3*x-Y_4*y,Y_1*x^3-Y_2*y^5,Y_3^2-Y_2*Y_4*x,Y_1*Y_4-Y_2^2*y^3)
T = S/J       
J = integralClosure T
KF = frac(ring ideal J)
M1 = first entries substitute(vars T, KF)
M2 = apply(T.icFractions, i -> matrix{{i}})

assert(icFractions T == substitute(matrix {{(Y_2*y^2)/x, (Y_1*x)/y,
Y_1, Y_2, Y_3, Y_4, x, y}}, frac T))
///

--------------------------------------------------------------------
icFractions = method()
icFractions(Ring) := Matrix => (R) -> (
     if R.?icFractions then R.icFractions
     else if isNormal R then vars R
     else (
	  if not R.?icFractions then integralClosure R;
     	  R.icFractions	  
     )
)

--------------------------------------------------------------------

icFracP = method(Options=>{conductorElement => null, Limit => infinity, reportSteps => false})
icFracP Ring := List => o -> (R) -> (
     -- 1 argument: a ring whose base field has characteristic p.
     -- Returns: Fractions
     if ring ideal presentation R === ZZ then (
	  D := 1_R;
	  U := ideal(D);
	  if o.reportSteps == true then print ("Number of steps: " | toString 0 | ",  Conductor Element: " | toString 1_R);
	  )    
     else if coefficientRing(R) === ZZ or coefficientRing(R) === QQ then error("Expected coefficient ring to be a finite field")
     else(
	  if o.conductorElement === null then (
     	       P := ideal presentation R;
     	       c := codim P;
     	       S := ring P;
	       J := promote(jacobian P,R);
	       n := 1;
	       det1 := ideal(0_R);
	       while det1 == ideal(0_R) do (
		    det1 = minors(c, J, Limit => n);
		    n = n+1
		    );
	       D = det1_0;
	       D = (mingens(ideal(D)))_(0,0);
	       ) 
     	  else D = o.conductorElement;
     	  p := char(R);
     	  K := ideal(1_R);
     	  U = ideal(0_R);
     	  F := apply(generators R, i-> i^p);
     	  n = 1;
     	  while (U != K) do (
	       U = K;
	       L := U*ideal(D^(p-1));
	       f := map(R/L,R,F);
	       K = intersect(kernel f, U);
	       if (o.Limit < infinity) then (
	       	    if (n >= o.Limit) then U = K;
	       	    );
               n = n+1;
     	       );
     	  if o.reportSteps == true then print ("Number of steps: " | toString n | ",  Conductor Element: " | toString D);
     	  );
     U = mingens U;
     if numColumns U == 0 then {1_R}
     else apply(numColumns U, i-> U_(0,i)/D)
     )

icPIdeal = method()
icPIdeal (RingElement, RingElement, ZZ) := Ideal => (a, D, N) -> (
     -- 3 arguments: An element in a ring of characteristic P that
     -- generates the principal ideal we are intersted in, a
     -- non-zerodivisor of $ in the conductor, and the number of steps
     -- in icFracP to compute the integral closure of R using the
     -- conductor element given.  
     -- Returns: the inetgral clousre of the ideal generated by the
     -- first argument.  
     n := 1;
     R := ring a;
     p := char(R);
     J := ideal(1_R);
     while (n <= N+1) do (
	F := apply(generators R, i-> i^(p^n));
	U := ideal(a^(p^n)) : D;
        f := map(R/U, R, F);
        J = intersect(J, kernel f);
	n = n+1;
     );
     J
     )


--------------------------------------------------------------------
--- integralClosure, idealizer, nonNormalLocus, Index,
--- isNormal, conductor, icFractions, icMap, icFracP, conductorElement,
--- reportSteps, icPIdeal, minPressy

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
     -- by the command icFractions(R/I).  
     -- Included is a command conductor that computes the conductor of S into R
     -- where S is the image of a ring map from R to S where S is finite over
     -- R.  icMap constructs this map (the natural map R/I->R_j/I_j) for R 
     -- into its integral closure and applying conductor to this map 
     -- yeilds the conductor of the integral closure into R.

     -- PROGRAMMERs : This implementation was written by and is maintained by
     --               Amelia Taylor.  
     -- UPDATE HISTORY : 25 October 2006
     ---------------------------------------------------------------------------
     PARA {
	  "This package computes the integral closure of a ring via the algorithm 
	  in Theo De Jong's paper, ", EM "An Algorithm for 
	  Computing the Integral Closure", ", J. Symbolic Computation, 
	  (1998) 26, 273-277, for a ring in any characteristic. It
	  also includes functions that uses 
	  the algorithm of Anurag Singh and Irena Swanson given in
	  arXiv:0901.0871 for rings in positive characteristic p.
	  The fractions that generate the integral closure over R are obtained 
     	  with the command ", TT "icFractions R", " if you use De
	  Jong's algorithm via ", TT "integralClosure R", " and the output of
	  Singh and Swanson's algorithm is already these
	  fractions."
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
     larger rings.  Therefore it checks the less costly S2 condition first and if 
     true, then tests the R1 condition using the jacobian of ", TT "R", "."
     }	 

--- needs better examples and check on how it reads...

document {
     Key => {integralClosure, (integralClosure, Ring)},
     Headline => "compute the integral closure of a reduced ring",
     Usage => "integralClosure R",
     Inputs => {
	  "R" => {" that is reduced and presented as a quotient ring"},
	  Variable => {" an unassigned symbol"},
	  Limit => {" limits the depth of the recursion"},
	  },
     Outputs => {{ofClass Ring, " that is the integral closure of ", TT "R", " in its total 
	       ring of fractions when ", TT "R", " is a domain.  When ", TT "R", 
	       " is reduced, a list of rings is returned with the property that the 
	       direct product of the rings is isomorphic to the integral closure 
	       of ", TT "R", ". The output rings use new indexed variables based 
	       on the symbol ", TT "w"}
	  },
     "The code for this function allows users to retrieve 
     certain information if desired.  
     The information of largest interest is the fractions that 
     correspond to the added variables in this description of 
     the integral closure.  Unfortunately, all of the added features 
     currently only work on affine domains.
     The map and the corresponding fractions are obtained as 
     a matrix using the function ", TO "icFractions", " where R is 
     an affine domain.  This function can be run without first 
     using ", TO "integralClosure", ".  The natural map from ", TT "R", " into 
     its integral closure is obtained using the function ", TO "icMap", " and 
     the conductor of the integral closure of R into R is found 
     using ", TT "conductor (icMap R)", ".  Note that 
     both ", TO "icFractions", " and ", TO "icMap", " take the input 
     ring ", TT "R", " as input rather than the output 
     of ", TO "integralClosure", ".  In this way you can use these 
     functions without running ", TO "integralClosure", ".  The 
     function ", TO "integralClosure", " is based on
     Theo De Jong's paper, An Algorithm for 
     Computing the Integral Closure, J. Symbolic Computation, 
     (1998) 26, 273-277.  This implementation is written and maintained 
     by Amelia Taylor, ", HREF {"mailto:amelia.taylor@coloradocollege.edu", 
     "<amelia.taylor@coloradocollege.edu>"}, ".",
     PARA{},
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
     its integral closure is obtained using the function ", TO "icMap", " and 
     the conductor of the integral closure of R into R is found 
     using ", TT "conductor (icMap R)", ".  Note that 
     both ", TO "icFractions", " and ", TT "icMap", " take the input 
     ring ", TT "R", " as input rather than the output 
     of ", TO "integralClosure", ".  In this way you can use these 
     functions without running ", TT "integralClosure", ".",
     SeeAlso => {"icMap", "icFractions", "conductor", "isNormal"}
     }
    
document {
     Key => [integralClosure,Variable],
     Headline=> "Sets the name of the indexed variables introduced in computing 
     the integral closure of a reduced ring."
     }

document {
     Key => [integralClosure,Limit],
     Headline=> "Sets the recursion level for the program allowing the
     user to see results without computing the full integral closure."
     }

document {
     Key => {idealizer, (idealizer, Ideal, RingElement)},
     Headline => "Compute Hom(I,I) as quotient ring",
     Usage => "idealizer(I, f)",
     Inputs => {"I" => {ofClass Ideal},
	  "f" => {{ofClass RingElement}, " that is a non-zero divisor in the
	  ring of ", TT "I"},
	  Variable => {" an unassigned symbol"},
	  Index => {" an integer"}},
     Outputs => {{ofClass Sequence, " where the first item is ", 
	       ofClass RingMap, " from the ring of ", TT "I", " to a
	       presentation of ", TT "Hom(I,I) = 1/f(f*J:J)", " and
	       the second item is ", ofClass List,
	       " consisting of the fractions that are added to the ring of J
	       to form ", TT "Hom(I,I)", "."}},
	       "We use this in integralClosure to complete a key step
	       in deJong's algorithm. Interested users might want to
	       use this to investigate different choices for ", 
	       TT "f", " in the algorithm."
     }

document {
     Key => [idealizer,Variable],
     Headline=> "Sets the name of the indexed variables introduced in computing 
     the endomorphism ring Hom(J,J)."
     }

document {
     Key => Index,
     Headline => "Optional input for idealizer",
     PARA{},
     "This option allows the user to select the starting index for the
     new variables added in computing Hom(J,J) as a ring.  The default
     value is 0 and is what most users will use.  The option is needed
     for the recurion implemented in integralClosure."
}


document {
     Key => [idealizer, Index],
     Headline=> "Sets the starting index on the new variables used to
     build the endomorphism ring Hom(J,J). If the program idealizer is
     used independently, the user will generally want to use the
     default value of 0.  However, when used as part of the
     integralClosure computation the number needs to start higher
     depending on the level of recursion involved. "
     }

document {
     Key => {nonNormalLocus, (nonNormalLocus, Ring)},
     Headline => "an ideal containing the non normal locus of a ring",
     Usage => "nonNormalLocus R",
     Inputs => {"R" => {ofClass Ring}},
     Outputs => {{ofClass Ideal, " an ideal containing the non-normal
	  locus of ", TT "R"}},
     	  "Primary use is as one step in deJong's algorithm for computing
     	  the integral closure of a reduced ring. If the presenting
	  ideal for the ring is homogeneous (e.g. the ring is graded)
	  and it has fewer than 20 generators then the implementation
	  checks to see if the singular locus is empty, if yes then
	  the maximal ideal is returned. In all other cases it returns
	  the radical of the first nonzero element of the jacobian ideal. "
     }

--- I don't love the third example in icMap
document {
     Key => {icMap, (icMap,Ring)},
     Headline => "natural map from an affine domain into its integral closure.",
     Usage => "icMap R",
     Inputs => {
	  "R" => {ofClass Ring, " that is an affine domain"}
	  },
     Outputs => {
	  	  {ofClass RingMap, " from ", TT "R", " to its integral closure"}
	  },
    "If an integrally closed ring is given as input, the identity map from 
     the ring to itself is returned.", 
     EXAMPLE{
	  "R = QQ[x,y]/ideal(x+2);",
	  "icMap R"},
     "This finite map is needed to compute the ", TO "conductor", " of the integral closure 
     into the original ring.",
    	  EXAMPLE {
	  "S = QQ[a,b,c]/ideal(a^6-c^6-b^2*c^4);",
      	  "conductor(icMap S)"},
     PARA{},
     "If the user has already run the computation ", TT "integralClosure R", 
     " then this map can also be obtained by typing ",
     TT "R.icMap", ".",
     EXAMPLE { 
	  "integralClosure S;",
	  "S.icMap"},
     SeeAlso => {"conductor"},
     }
    

document {
     Key => {icFractions, (icFractions,Ring)},
     Headline => "Compute the fractions integral over a domain.",
     Usage => "icFractions R",
     Inputs => {
	  "R" => {ofClass Ring, " that is an affine domain"},
	  },
     Outputs => {
  	  {ofClass List, " whose entries are fractions that generate the integral 
     	       closure of ", TT "R", " over R."}
	       },
    	  EXAMPLE {
	       "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
	       "integralClosure(R,Variable => a)",
	       "icFractions R"
	       },
     	  "Thus the new variables ", TT "a_7", " and ", TT "a_6", " in
	  the output from ", TT "integralClosure", " correspond to the 
     	  last two fractions given.  The other fractions are those
	  returned in intermediate recursive steps in the computation of the
	  integral closure. ", TT "a_0", " for example corresponds to the first
	  fraction to the left of the original ring variables.  
	  The program currently also returns the original 
    	  variables as part of the matrix.  In this way the user can see if any are 
     	  simplified out of the ring during the process of computing the integral
     	  closure.",
     	  PARA{},
	  "A future version of icFractions will return only the
	  fractions corresponding to the variables returned by the
	  function integralClosure. Thus the general format will be
	  much easier to use"
--     	  "The fractions returned correspond to the variables returned by the function 
--     	  integralClosure.  The function integralClosure eliminates redundant fractions 
--     	  during its iteration.  If the user would like to see all fractions generated 
--     	  during the computation, use the optional argument ", TT "Strategy => Long", " as 
--     	  illustrated here.",
--	      	  EXAMPLE {
--	       "icFractions(R)"
--	       },
     	  }

--document {
--     Key => [icFractions,Strategy],
--     Headline=> "Allows the user to obtain all of the fractions considered in the 
--     process of building the integral closure",
--     }

document {
     Key => {conductor,(conductor,RingMap)},
     Headline => "compute the conductor of a finite ring map",
     Usage => "conductor F",
     Inputs => {
	  "F" => {ofClass RingMap, " from a ring ", TT "R", " to a ring ", TT "S", 
	       ". The map must be a finite."},
	  },
     Outputs => {
	  {ofClass Ideal, " that is the conductor of ", TT "S", " into ", TT "R", "."}
	  },
     "Suppose that the ring map F : R --> S is finite: i.e. S is a finitely 
     generated R-module.  The conductor of F is defined to be {",
     TEX "g \\in R \\mid g S \\subset f(R)", "}.  One way to think
     about this is that the conductor is the set of universal denominators
     of ", TT "S", " over ", TT "R", ", or as the largest ideal of ", TT "R", " 
     which is also an ideal in ", TT "S", ". On natural use is the
     conductor of the map from a ring to its integral closure. ",
     EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
	  "S = integralClosure R",
	  "F = R.icMap",
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
     SeeAlso =>{"pushForward", "integralClosure", "icMap"} 
     }

document {
     Key => {icFracP, (icFracP, Ring)},
     Headline => "compute the integral closure in prime characteristic",
     Usage => "icFracP R, icFracP(R, conductorElement => D), icFracP(R, Limit => N), icFracP(R, reportSteps => Boolean)",
     Inputs => {
	"R" => {"that is reduced, equidimensional,
           finitely and separably generated over a field of characteristic p"},
	conductorElement => {"optionally provide a non-zerodivisor conductor element ",
               TT "conductorElement => D", ";
               the output is then the module generators of the integral closure.
               A good choice of ", TT "D", " may speed up the calculations?"},
	Limit => {"if value N is given, perform N loop steps only"},
	reportSteps => {"if value true is given, report the conductor element and number of steps in the loop"},
	},
     Outputs => {{"The module generators of the integral closure of ", TT "R",
               " in its total ring of fractions.  The generators are
               given as elements in the total ring of fractions."}
          },
     "Input is an equidimensional reduced ring in characteristic p
     that is finitely and separably generated over the base field.
     The output is a finite set of fractions that generate
     the integral closure as an ", TT "R", "-module.
     An intermediate step in the code
     is the computation of a conductor element ", TT "D",
     " that is a non-zerodivisor;
     its existence is guaranteed by the separability assumption.
     The user may supply ", TT "D",
     " with the optional ", TT "conductorElement => D", ".
     (Sometimes, but not always, supplying ", TT "D", " speeds up the computation.)
     In any case, with the non-zero divisor ", TT "D", ",
     the algorithm starts by setting the initial approximation of the integral closure
     to be the finitely generated ", TT "R", "-module
     ", TT "(1/D)R", ",
     and in the subsequent loop the code recursively constructs submodules.
     Eventually two submodules repeat;
     the repeated module is the integral closure of ", TT "R", ".
     The user may optionally provide ", TT "Limit => N", " to stop the loop
     after ", TT "N", " steps,
     and the optional ", TT "reportSteps => true", " reports the conductor
     element and the number of steps it took for the loop to stabilize.
     The algorithm is based on the
     Leonard--Pellikaan--Singh--Swanson algorithm.",
     PARA{},
     "A simple example.",
     EXAMPLE {
          "R = ZZ/5[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
          "icFracP R"
     },
     "The user may provide an optional non-zerodivisor conductor element ",
     TT "D",
     ".  The output generators need not
     be expressed in  the form with denominator ", TT "D", ".",
     EXAMPLE {
          "R = ZZ/5[x,y,u,v]/ideal(x^2*u-y^2*v);",
          "icFracP(R)",
          "icFracP(R, conductorElement => x)",
     },
     "In case ", TT "D", " is not in the conductor, the output is ",
     TT "V_e = (1/D) {r in R | r^(p^i) in (D^(p^i-1)) ", "for ",
     TT "i = 1, ..., e}",
     " such that ", TT "V_e = V_(e+1)", " and ", TT "e",
     " is the smallest such ", TT "e", ".",
     EXAMPLE {
	  "R=ZZ/2[u,v,w,x,y,z]/ideal(u^2*x^3+u*v*y^3+v^2*z^3);",
          "icFracP(R)",
          "icFracP(R, conductorElement => x^2)"
     },
     "The user may also supply an optional limit on the number of steps
     in the algorithm.  In this case, the output is a finitely generated ",
     TT "R", "-module contained in ", TT "(1/D)R",
     " which contains the integral closure (intersected with ", TT "(1/D)R",
     ".",
     EXAMPLE {
          "R=ZZ/2[u,v,w,x,y,z]/ideal(u^2*x^3+u*v*y^3+v^2*z^3);",
          "icFracP(R, Limit => 1)",
          "icFracP(R, Limit => 2)",
          "icFracP(R)"
     },
     "With the option above one can for example determine how many
     intermediate modules the program should compute or did compute
     in the loop to get the integral closure.  A shortcut for finding
     the number of steps performed is to supply the ",
     TT "reportSteps => true", " option.",
     EXAMPLE {
          "R=ZZ/3[u,v,w,x,y,z]/ideal(u^2*x^4+u*v*y^4+v^2*z^4);",
          "icFracP(R, reportSteps => true)"
     },
     "With this extra bit of information, the user can now compute
     integral closures of principal ideals in ", TT "R", " via ",
     TO icPIdeal, ".",
     SeeAlso => {"icPIdeal", "integralClosure", "isNormal"},
     Caveat => "NOTE: mingens is not reliable, neither is kernel of the zero map!!!"
}

document {
     Key => conductorElement,
     Headline => "Specifies a particular non-zerodivisor in the conductor."
}

document {
     Key => [icFracP,conductorElement],
     Headline => "Specifies a particular non-zerodivisor in the conductor.",
     "A good choice can possibly speed up the calculations.  See ",
     TO icFracP, "."
}


document {
     Key => [icFracP,Limit],
     Headline => "Limits the number of computed intermediate modules.",
     Caveat => "NOTE: How do I make M2 put icFracP on the list of all functions that use Limit?"
}

document {
     Key => reportSteps,
     Headline => "Optional in icFracP",
     PARA{},
     "With this option, ", TT "icFracP",
     " prints out the conductor element and
           the number of intermediate modules it computed;
           in addition to the output being
           the module generators of the integral closure of the ring.",
     Caveat => "NOTE: There is probably a better name for this, or a better way of doing this."
}

document {
     Key => [icFracP,reportSteps],
     Headline => "Prints out the conductor element and
           the number of intermediate modules it computed.",
     Usage => "icFracP(R, reportSteps => Boolean)",
     "The main use of the extra information is in computing the
     integral closure of principal ideals in ", TT "R",
     ", via ", TO icPIdeal,
     ".",
     EXAMPLE {
          "R=ZZ/3[u,v,x,y]/ideal(u*x^2-v*y^2);",
          "icFracP(R, reportSteps => true)",
	  "S = ZZ/3[x,y,u,v];",
          "R = S/kernel map(S,S,{x-y,x+y^2,x*y,x^2});",
	  "icFracP(R, reportSteps => true)"
     },
}

document {
     Key => {icPIdeal,(icPIdeal, RingElement, RingElement, ZZ)},
     Headline => "compute the integral closure
                  in prime characteristic of a principal ideal",
     Usage => "icPIdeal (a, D, N)",
     Inputs => {
	"a" => {"an element in ", TT "R"},
        "D" => {"a non-zerodivisor of ", TT "R",
                " that is in the conductor"},
        "N" => {"the number of steps in ", TO icFracP,
                " to compute the integral closure of ", TT "R",
                ", by using the conductor element ", TT "D"}},
     Outputs => {{"the integral closure of the ideal ", TT "(a)", "."}},
     "The main input is an element ", TT "a",
     " which generates a principal ideal whose integral closure we are
     seeking.  The other two input elements,
     a non-zerodivisor conductor element ", TT "D",
     " and the number of steps ", TT "N", 
     " are the pieces of information obtained from ",
     TT "icFracP(R, reportSteps => true)",
     ".  (See the Singh--Swanson paper, An algorithm for computing
     the integral closure, Remark 1.4.)",
     EXAMPLE {
          "R=ZZ/3[u,v,x,y]/ideal(u*x^2-v*y^2);",
          "icFracP(R, reportSteps => true)",
          "icPIdeal(x, x^2, 3)"
     },
     SeeAlso => {"icFracP"}
}


///
restart
loadPackage"IntegralClosure"
///
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
answer = ideal(b_1*x^2-y*z, x^6-b_1*y+x^3*z, -b_1^2+x^4*z+x*z^2)
assert(ideal J == answer)
use R
assert(conductor(R.icMap) == ideal(x^2,y))
--assert(icFractions R == substitute(matrix {{y*z/x^2, x, y, z}},frac R))
--assert(icFractions R == substitute(matrix {{42 * y*z/x^2, x, y, z}},frac R))
///

-- multigraded test
TEST ///
R = ZZ/101[symbol x..symbol z,Degrees=>{{1,2},{1,5},{1,6}}]/(z*y^2-x^5*z-x^8)
time J = integralClosure (R,Variable=>symbol a) 
use ring ideal J
assert(ideal J == ideal(-x^6+a_3*y-x^3*z,-a_3*x^2+y*z,a_3^2-x^4*z-x*z^2))
///

-- multigraded homogeneous test
TEST ///
R = ZZ/101[symbol x..symbol z,Degrees=>{{4,2},{10,5},{12,6}}]/(z*y^2-x^5*z-x^8)
time J = integralClosure (R,Variable=>symbol a) 
use ring ideal J
assert(ideal J == ideal(a_1*x^2-y*z,a_1*y-x^6-x^3*z,a_1^2-x^4*z-x*z^2))

-- This was the old answer, before changing choice of J0
--assert(ideal J == ideal(a_1*y-42*x^6-42*x^3*z,a_1^2-47*x^4*z-47*x*z^2,-12*a_1*x^2-z*y,-12*a_1*x*y-x^7-x^4
--      *z,43*a_1^2*x^2-x^6*z-x^3*z^2,-x^8-x^5*z+z*y^2))
use R
assert(conductor(R.icMap) == ideal(x^2,y))
///

-- Reduced not a domain test
TEST ///
S=ZZ/101[symbol a,symbol b,symbol c, symbol d]
I=ideal(a*(b-c),c*(b-d),b*(c-d))
R=S/I                              
time V = integralClosure R
assert(#V == 2)
icMap R
-- assert false-- added: MES
-- the second ring is not a domain.  Anyway, the fractions are messed up here.
///

--Craig's example as a test
TEST ///
S=ZZ/101[symbol x,symbol y,symbol z,MonomialOrder => Lex]
I=ideal(x^6-z^6-y^2*z^4)
Q=S/I
time J = integralClosure (Q, Variable => symbol a)
use ring ideal J
assert(ideal J == ideal (x^2-a_6*z, a_6*x-a_7*z, a_6^2-a_7*x, a_7^2-y^2-z^2))
use Q
assert(conductor(Q.icMap) == ideal(z^3,x*z^2,x^3*z,x^4))
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

oldanswer = ideal(a^2*b*c^2+b^2*c*d^2+a^2*d^2*e+a*b^2*e^2+c^2*d*e^2,
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

-- We need to check the correctness of this example!
newanswer = ideal(
  a^2*b*c^2+b^2*c*d^2+a^2*d^2*e+a*b^2*e^2+c^2*d*e^2,
    a*b^3*c+b*c^3*d+a^3*b*e+c*d^3*e+a*d*e^3,
    a^5+b^5+c^5+d^5-5*a*b*c*d*e+e^5,
    X_1*e-a^3*b^2*c+b*c^2*d^3,
    X_1*d+a*b^2*c^3-b^5*d-d^6+3*a*b*c*d^2*e-a^2*b*e^3-d*e^5,
    X_1*c-c*d^5+a^2*b^3*e+a*b*c^2*d*e-a*d^3*e^2,
    X_1*b-b^6-b*c^5-b*d^5+4*a*b^2*c*d*e-c^3*d^2*e+a^3*d*e^2-b*e^5,
    X_1*a-a*b^5-b^3*c^2*d-a*d^5+2*a^2*b*c*d*e+c*d^2*e^3,
    X_0*e-a*b*c^4+b^4*c*d,
    X_0*d+a^4*b*c-a^2*b^2*d^2+a*c^2*d^3-a*b^4*e-b^2*c^2*d*e+a^2*c*d*e^2,
    X_0*c-2*a^2*b^2*c*d+a*c^3*d^2-a^4*d*e+b*c*d^2*e^2+a*b*e^4,
    X_0*b-a^2*b^3*d+2*a*b*c^2*d^2+a*d^4*e-a^2*b*c*e^2-c*d*e^4,
    X_0*a-a^3*b^2*d+a^2*c^2*d^2-b*c*d^4+a*b^2*c^2*e+c^4*d*e-a*b*d^2*e^2,
    X_1^2-b^10-b^5*c^5+2*a*b^2*c^3*d^4-2*b^5*d^5-d^10-5*b^4*c^3*d^2*e+6*a*b*c*d^6*e-6*a^3*b^4*d*e^2-4*b^3*c*d^4*e^2+2*a^2*b*d^4*e^3-4*a*b^3*d^2*e^4+b*c^2*d^3*e^4-b^5*e^5-d^5*e^5,
    X_0*X_1-a^2*b^7*d+b^3*c^4*d^3+a^4*b*c*d^4-a^2*b^2*d^6+a*c^2*d^7+4*b^2*c^2*d^5*e+b^6*d^2*e^2+b*c^5*d^2*e^2+3*a^2*c*d^5*e^2+b*d^7*e^2+a^4*b^3*e^3+4*c^3*d^4*e^3-2*a^3*d^3*e^4+b*d^2*e^7,
    X_0^2-a^4*b^4*d^2-a^2*c^4*d^4+7*b*c^3*d^6-2*b^5*c*d^3*e-2*c^6*d^3*e+2*a^3*b*d^5*e+5*c*d^8*e+a^3*c^3*d^2*e^2-6*a^2*b^3*d^3*e^2-a*b*c^2*d^4*e^2-2*a*b^5*d*e^3-2*b^3*c^2*d^2*e^3+5*a*d^6*e^3-a^2*b*c*d^2*e^4+a^3*b*e^6+c*d^3*e^6+a*d*e^8)

assert(ideal V == newanswer)   
///

-- Test of icFractions
--TEST 
--///
--S = QQ [(symbol Y)_1, (symbol Y)_2, (symbol Y)_3, (symbol Y)_4, symbol x, symbol y, Degrees => {{7, 1}, {5, 1}, {6, 1}, {6, 1}, {1, 0}, {1, 0}}, MonomialOrder => ProductOrder {4, 2}]
--J = ideal(Y_3*y-Y_2*x^2,Y_3*x-Y_4*y,Y_1*x^3-Y_2*y^5,Y_3^2-Y_2*Y_4*x,Y_1*Y_4-Y_2^2*y^3)
--T = S/J       
--assert(icFractions T == substitute(matrix {{(Y_2*y^2)/x, (Y_1*x)/y, Y_1, Y_2, Y_3, Y_4, x, y}}, frac T))
--///

-- Test of isNormal
TEST ///
S = ZZ/101[x,y,z]/ideal(x^2-y, x*y-z^2)
assert(isNormal(S) == false)
assert(isNormal(integralClosure(S)) == true)
///

-- Test of icMap and conductor
TEST ///
R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4)
J = integralClosure(R);
F = R.icMap
assert(conductor F == ideal((R_2)^3, (R_0)*(R_2)^2, (R_0)^3*(R_2), (R_0)^4))
///

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
R = kk[x,y,z, MonomialOrder => Lex]
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
NNL = radJF
NNL = substitute(NNL,A)
(phi,fracs) = idealizer(NNL,NNL_0)
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

---------------------- Birational Work

R = ZZ/101[b_1, x,y,z, MonomialOrder => {GRevLex => {7}, GRevLex=>{2,5,6}}]
R = ZZ/101[x,y,z]
S = R[b_1, b_0]
I = ideal(b_1*x^2-42*y*z, x^6+12*b_1*y+ x^3*z, b_1^2 - 47*x^4*z - 47*x*z^2)
I = ideal(b_1*x-42*b_0, b_0*x-y*z, x^6+12*b_1*y+ x^3*z, b_1^2 -47*x^4*z - 47*x*z^2, b_0^2-x^6*z - x^4*z^2)
leadTerm gens gb I

R = ZZ/101[x,y,z]/(z*y^2-x^5*z-x^8)
J = integralClosure(R)
R.icFractions
describe J


S=ZZ/101[symbol x,symbol y,symbol z,MonomialOrder => Lex]
I=ideal(x^6-z^6-y^2*z^4)
Q=S/I
time J = integralClosure (Q, Variable => symbol a)


S = ZZ/101[a_7,a_6,x,y,z, MonomialOrder => {GRevLex => 2, GRevLex => 3}]
Inew = ideal(x^2-a_6*z,a_6*x-a_7*z,a_6^2-a_7*x,a_7^2-y^2-z^2)
leadTerm gens gb Inew
radical ideal oo


----- Minimal Presentation Tests 
///
restart
loadPackage "IntegralClosure"

U = ZZ/101[x,y,z]
f = x^2+x+2*y+z
g = x^2+y^2+x
h = x^2+3*x -2*y + 4*z

I = ideal(f)
J = ideal(z,g)

S = U/I
T= U/J

P = ideal(x)

minimalPresentation I
minimalPresentation P
minimalPresentation J

minimalPresentation S
minimalPresentation T

S.minimalPresentationMap
S.minimalPresentationMapInv
F = map(R, target I.cache.minimalPresentationMapInv)

C=ZZ/101[x,y,z,Degrees => {2,3,1}]/ideal(x-x^2-y,z+x*y)
V= time minPressy(C)
gens V == {x}
degrees V == {{2}}

C=ZZ/101[x,y,z,Degrees => {{1,2},{1,3},{1,1}}]/ideal(x-x^2-y,z+x*y)
V = time minPressy(C)
gens V == {a_0}
degrees V == {{1,2}}

C=ZZ/101[x,y,z,u,w]/ideal(x-x^2-y,z+x*y,w^2-u^2)
V= time minPressy(C)
gens V == {x,u,w}
use ring ideal V
ideal V == ideal(u^2-w^2)

w = symbol w
y = symbol y
S = ZZ/101[w_16, w_11, w_12, w_13, w_14, w_15, w_8, w_9, w_10, y_1, y_2, y_3, y_4, y_5, y_6, y_7, y_8,
     Degrees => {{1}, {1}, {2}, {2}, {2}, {2}, {2}, {3}, {3}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}}, MonomialOrder => ProductOrder {1, 5, 3, 8}, MonomialSize => 16]

J=ideal(y_2*y_6-y_3*y_7,w_11*y_6-w_8,w_11*y_1-y_3*y_7,w_11^2-w_15,w_16*y_6-y_1*y_5,w_16*y_3-w_13,w_16*y_2-w_15,w_16*y_1-w_8,w_16*w_11-y_2*y_5,w_16^2-w_11*y_5,y_1*y_4*y_5-y_2*y_3*y_8,w_11*y_3*y_8-y_4*y_5*y_6,w_11*y_2*y_8-y_4*y_5*y_7,w_11*y_4*y_7-w_9,w_14*y_6-y_1*y_2*y_8,w_14*y_5-w_15*y_8,w_14*y_3-w_8*y_4,w_14*y_2-w_9,w_14*y_1-y_4*y_6*y_7,w_12*y_7-y_1*y_2*y_8,w_12*y_6-y_1*y_3*y_8,w_12*y_5-w_13*y_8,w_12*y_3-w_10,w_12*y_2-w_8*y_4,w_12*y_1-y_4*y_6^2,w_11*w_14-y_2^2*y_8,w_11*w_12-y_2*y_3*y_8,w_16*y_4*y_7-y_2^2*y_8,w_16*w_14-y_4*y_5*y_7,w_16*w_12-y_4*y_5*y_6,w_14^2-y_2*y_4*y_7*y_8,w_12*w_14-y_3*y_4*y_7*y_8,w_12^2-y_3*y_4*y_6*y_8)

minPresIdeal(J) == ideal(y_2*y_6-y_3*y_7,y_1*y_5-w_16*y_6,w_11*y_1-y_3*y_7,w_16*y_1-w_11*y_6,w_11^2-w_16*y_2,w_16*w_11-y_2*y_5,w_16^2-w_11*y_5,y_4*y_5*y_7-w_11*y_2*y_8,w_16*y_4*y_7-y_2^2*y_8,w_12*y_7-y_1*y_2*y_8,y_4*y_5*y_6-w_11*y_3*y_8,w_14*y_6-y_1*y_2*y_8,w_12*y_6-y_1*y_3*y_8,w_16*y_4*y_6-y_2*y_3*y_8,w_14*y_5-w_16*y_2*y_8,w_12*y_5-w_16*y_3*y_8,w_14*y_3-w_11*y_4*y_6,w_14*y_2-w_11*y_4*y_7,w_12*y_2-w_11*y_4*y_6,w_14*y_1-y_4*y_6*y_7,w_12*y_1-y_4*y_6^2,w_11*w_14-y_2^2*y_8,w_16*w_14-w_11*y_2*y_8,w_11*w_12-y_2*y_3*y_8,w_16*w_12-w_11*y_3*y_8,w_14^2-y_2*y_4*y_7*y_8,w_12*w_14-y_3*y_4*y_7*y_8,w_12^2-y_3*y_4*y_6*y_8)

C = S/J
V = time minPressy(C,Variable => a)
gens V == {a_0,a_1,a_2,a_3,a_4,a_5,a_6,a_7,a_8,a_9,a_10,a_11}
use ring ideal V
ideal V == ideal(a_5*a_9-a_6*a_10,a_4*a_8-a_0*a_9,a_1*a_4-a_6*a_10,a_0*a_4-a_1*a_9,a_1^2-a_0*a_5,a_0*a_1-a_5*a_8,a_0^2-a_1*a_8,a_7*a_8*a_10-a_1*a_5*a_11,a_0*a_7*a_10-a_5^2*a_11,a_2*a_10-a_4*a_5*a_11,a_7*a_8*a_9-a_1*a_6*a_11,a_3*a_9-a_4*a_5*a_11,a_2*a_9-a_4*a_6*a_11,a_0*a_7*a_9-a_5*a_6*a_11,a_3*a_8-a_0*a_5*a_11,a_2*a_8-a_0*a_6*a_11,a_3*a_6-a_1*a_7*a_9,a_3*a_5-a_1*a_7*a_10,a_2*a_5-a_1*a_7*a_9,a_3*a_4-a_7*a_9*a_10,a_2*a_4-a_7*a_9^2,a_1*a_3-a_5^2*a_11,a_0*a_3-a_1*a_5*a_11,a_1*a_2-a_5*a_6*a_11,a_0*a_2-a_1*a_6*a_11,a_3^2-a_5*a_7*a_10*a_11,a_2*a_3-a_6*a_7*a_10*a_11,a_2^2-a_6*a_7*a_9*a_11)
///


--- Recent tests and experiments for integral closure.
///
restart
loadPackage"IntegralClosure"
R=ZZ/2[x,y,Weights=>{{8,9},{0,1}}]
I=ideal(y^8+y^2*x^3+x^9) -- eliminates x and y at some point. 
R=ZZ/2[x,y,Weights=>{{31,12},{0,1}}]
I=ideal"y12+y11+y10x2+y8x9+x31" -- really long
S=integralClosure(R/I)
transpose gens ideal S

M = flattenRing (R/I)
J = nonNormalLocus M_0
phi = M_1
fractions = gens M_0
indexVar = 0

S = target phi
I = ideal presentation target phi
R = ring I
J1 = trim(ideal(0_S):J_0)
J1 != ideal(0_S) 
(newPhi, fracs) = idealizer(J, J_0, Index => indexVar);
targ = target newPhi
targ == S 
newI1 = trim ideal presentation targ
newJ1 = newPhi J
newI = minimalPresentation(newI1)
S = ring newI
B2 = S/newI
FF = substitute(((newI1).cache.minimalPresentationMap).matrix, B2)
F = map(B2,target newPhi, FF)
J = radical(F newJ1)
fractions = join(fracs,fractions), 
phi = F*newPhi*phi
indexVar = indexVar + # gens targ - # gens source newPhi 

///
