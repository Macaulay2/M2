newPackage(
	"IntegralClosure2",
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
-- code added in as an option as well as needs icFractions2 to be up
-- and running again.  Also needs updating the tests.  Are there
-- others where we know the answer in advance?  What about how this
-- program interacts with minPressy?  
-- Do I want Limit on integralClosure - how will we use this?
-- Do we still need isSinglyGraded?  i.e. is pushForward still a
-- problem?
   
export{integralClosure2, idealizer2, nonNormalLocus2, Index2,
isNormal2, conductor2, icFractions2, icMap2, icFracP2, conductor2Element,
reportSteps2, icPIdeal2} 

needsPackage "Elimination"

-- PURPOSE : Front end for the integralClosure2 function.  It governs
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
--                   nonNormalLocus2  - computes the non-normal locus
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
-- in DeJong's algorithm easily.  In particular, nonNormalLocus2 and
-- idealizer are now stand alone functions.  This could alow study of
-- the role of different choices of the non-zero element f in
-- idealizer, or of different possibly choices that "work" for the
-- nonNormalLocus2. 

--- Should Singh/Swanson be an option to integralClosure2 or its own
--- program.  Right now it is well documented on its own.  I'm not
--- sure what is best long term. 

integralClosure2 = method(Options=>{
	  Variable => global w,
	  Limit => infinity})


-- OLD CODE: to be removed.
integralClosure2 Ring := Ring => o -> (R) -> (
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
     ICout := integralClosureHelper(nonNormalLocus2 M_0, gens M_0, M_1,o.Limit, o.Variable, 0);
     if #ICout == 2 then (
	  R.icFractions2 = ICout#1;
	  --R.icFractions2Short = ICout#2;
	  R.icMap2 = ICout#0;
	  target ICout#0
	  )
     else (
	  n := substitute((#ICout)/2, ZZ);
	  ICout = apply(n-1, i -> {ICout#(2*i), ICout#(2*i+1)});
	  R.icFractions2 = apply(ICout, i -> i#1);
	  R.icMap2 = apply(ICout, i -> i#0);
	  RIdeal := apply(R.icMap2, i -> trim ideal target i);
	  apply(RIdeal, i -> (ring i)/i)
	  )
     )

-- version that MES has just provided, not in use in 1.2 yet
integralClosure2 Ring := Ring => o -> (R) -> (
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
     (newPhi,newG) := mikeIntegralClosureHelper(nonNormalLocus2 S, phi, G, o.Limit, o.Variable, 0);
     R.icFractions2 = map(frac R,target newPhi,newG.matrix);
     R.icMap2 = newPhi;
     target newPhi
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
     (newPhi, newG, fracs) := idealizer2(J, J0, Variable => newVar, Index2 => indexVar);
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

-- OLD CODE: to be removed.
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
     << "entering helper" << endl;
     J0 := findSmallGen J;
     J1 := trim(ideal(0_S):J0); 
     -- need to check if J0 is really the element we-- want - low degree. 
     if J1 != ideal(0_S) then(
	  -- If J0 is a ZD then we split the ring.
	  -- need to try and clean up ideals as much as possible as we proceed.
	  (S1, S1Map) := flattenRing(R/trim(substitute(J1, R) + I));
	  (S2, S2Map) := flattenRing(R/trim(substitute(ideal(0_S):J1, R) + I));
	  L := join(integralClosureHelper(nonNormalLocus2 (minimalPresentation S1), 
	       	    fractions,
		    (S1.minimalPresentationMap)*(S1Map)*map(source S1Map, S)*phi, 
		    counter-1, newVar, indexVar),
	       integralClosureHelper(nonNormalLocus2 (minimalPresentation S2), 
			 fractions, 
			 (S2.minimalPresentationMap)*(S2Map)*map(source S2Map, S)*phi, 
			 counter-1, newVar, indexVar));
	  return L
	  )	
     else(
	  -- If J0 is a NZD then we continue setting f = J0.
	  -- Compute Hom_R(J,J), with R = S/I.
	  -- From now on, we work in this quotient:
	  (newPhi, fracs) := idealizer2(J, J0, Variable => newVar, Index2 => indexVar);  
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


idealizer2 = method(Options=>{Variable => global w, Index2 => 0})
idealizer2 (Ideal, RingElement) := o -> (J, f) ->  (
     -- J is an ideal in a ring R
     -- f is a nonzero divisor in J
     -- computes a ring B2 = Hom(J,J) = (f*J:J)/f
     -- returns a sequence (F,G,fractions), where 
     --   F : R --> B2 is the natural inclusion
     --   G : B2 --> frac R, 
     --   fractions: a list of the images of the new generators under G in frac R.
     -- optional arguments:
     --   o.Variable: base name for new variables added
     --   o.Index2: the first subscript to use for such variables
     R := ring J;
     I := ideal presentation R;
     idJ := mingens(f*J : J);
     if ideal(idJ) == ideal(f) then (
	  (id_R, map(frac R, frac R, vars frac R), {})) -- in this case R is isomorphic to Hom(J,J)
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
     	  A := kk(monoid [o.Variable_(o.Index2)..o.Variable_(o.Index2+n-1), gens R,
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
	  G := map(frac R, frac B2, matrix{fractions} | vars frac R);
	  (F, G, fractions)
	  )
     )


nonNormalLocus2 = method()
nonNormalLocus2 Ring := (R) -> (
     -- This handles the first key step in DeJong's algorithm: finding
     -- an ideal that contains the NNL locus. 
     -- 1 argument: a ring. it must be flattened. normally it will be
     -- a quotient ring. 
     -- Return: an ideal containing the non-normal locus of R.   
     local J;
     I := ideal presentation R;
     Jac := jacobian R;
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
isNormal2 = method()     
isNormal2(Ring) := Boolean => (R) -> (
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
conductor2 = method()
conductor2(RingMap) := Ideal => (F) -> (
     --Input:  A ring map where the target is finitely generated as a 
     --module over the source.
     --Output: The conductor2 of the target into the source.
     --NOTE:  If using this in conjunction with the command normalization,
     --then the input is R#IIICCC#"map" where R is the name of the ring used as 
     --input into normalization.  
     if isHomogeneous (source F)
     	  then(M := presentation pushForward(F, (target F)^1);
     	       P := target M;
     	       intersect apply((numgens P)-1, i->(
	       m:=matrix{P_(i+1)};
	       I:=ideal modulo(m,matrix{P_0}|M))))
	  else error "conductor2: expected a homogeneous ideal in a graded ring"
     )

icMap2 = method()
icMap2(Ring) := RingMap => R -> (
     -- 1 argument: a ring.  May be a quotient ring, or a tower ring.
     -- Returns: The map from R to the integral closure of R.  
     -- Note:  This is a map where the target is finitely generated as
     -- a module over the source, so it can be used as the input to
     -- conductor2 and other methods that require this. 
     if R.?icMap2 then R.icMap2
     else if isNormal2 R then id_R
     else (S := integralClosure2 R;
	  R.icMap2)
     )

     

///
restart
loadPackage"IntegralClosure2"
S = QQ [(symbol Y)_1, (symbol Y)_2, (symbol Y)_3, (symbol Y)_4, symbol x, symbol y, Degrees => {{7, 1}, {5, 1}, {6, 1}, {6, 1}, {1, 0}, {1, 0}}, MonomialOrder => ProductOrder {4, 2}]
J =
ideal(Y_3*y-Y_2*x^2,Y_3*x-Y_4*y,Y_1*x^3-Y_2*y^5,Y_3^2-Y_2*Y_4*x,Y_1*Y_4-Y_2^2*y^3)
T = S/J       
J = integralClosure2 T
KF = frac(ring ideal J)
M1 = first entries substitute(vars T, KF)
M2 = apply(T.icFractions2, i -> matrix{{i}})

assert(icFractions2 T == substitute(matrix {{(Y_2*y^2)/x, (Y_1*x)/y,
Y_1, Y_2, Y_3, Y_4, x, y}}, frac T))
///

--------------------------------------------------------------------
icFractions2 = method()
icFractions2(Ring) := Matrix => (R) -> (
     if R.?icFractions2 then R.icFractions2
     else if isNormal2 R then vars R
     else (
	  if not R.?icFractions2 then integralClosure2 R;
     	  R.icFractions2	  
     )
)

--------------------------------------------------------------------

icFracP2 = method(Options=>{conductor2Element => null, Limit => infinity, reportSteps2 => false})
icFracP2 Ring := List => o -> (R) -> (
     -- 1 argument: a ring whose base field has characteristic p.
     -- Returns: Fractions
     -- MES:
     --  ideal presentation R ==== ideal R
     --  dosn't seem to handle towers
     --  this ring in the next line can't really be ZZ?
     if ring ideal presentation R === ZZ then (
	  D := 1_R;
	  U := ideal(D);
	  if o.reportSteps2 == true then print ("Number of steps: " | toString 0 | ",  Conductor Element: " | toString 1_R);
	  )    
     else if coefficientRing(R) === ZZ or coefficientRing(R) === QQ then error("Expected coefficient ring to be a finite field")
     else(
	  if o.conductor2Element === null then (
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
     	  else D = o.conductor2Element;
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
     	  if o.reportSteps2 == true then print ("Number of steps: " | toString n | ",  Conductor Element: " | toString D);
     	  );
     U = mingens U;
     if numColumns U == 0 then {1_R}
     else apply(numColumns U, i-> U_(0,i)/D)
     )

icPIdeal2 = method()
icPIdeal2 (RingElement, RingElement, ZZ) := Ideal => (a, D, N) -> (
     -- 3 arguments: An element in a ring of characteristic P that
     -- generates the principal ideal we are intersted in, a
     -- non-zerodivisor of $ in the conductor2, and the number of steps
     -- in icFracP2 to compute the integral closure of R using the
     -- conductor2 element given.  
     -- Returns: the integral closure of the ideal generated by the
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
--- integralClosure, idealizer2, nonNormalLocus2, Index2,
--- isNormal2, conductor2, icFractions2, icMap2, icFracP2, conductor2Element,
--- reportSteps2, icPIdeal2, minPressy

beginDocumentation()

document {
     Key => IntegralClosure2,
     ---------------------------------------------------------------------------
     -- PURPOSE : Compute the integral closure of a ring via the algorithm 
     --           in Theo De Jong's paper, An Algorithm for 
     --           Computing the Integral Closure, J. Symbolic Computation, 
     --           (1998) 26, 273-277. 
     -- PROGRAMS : integralClosure
     --            isNormal2
     --            ICFractions
     --            ICMap
     -- The fractions that generate the integral closure over R/I are obtained 
     -- by the command icFractions2(R/I).  
     -- Included is a command conductor2 that computes the conductor2 of S into R
     -- where S is the image of a ring map from R to S where S is finite over
     -- R.  icMap2 constructs this map (the natural map R/I->R_j/I_j) for R 
     -- into its integral closure and applying conductor2 to this map 
     -- yeilds the conductor2 of the integral closure into R.

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
     	  with the command ", TT "icFractions2 R", " if you use De
	  Jong's algorithm via ", TT "integralClosure2 R", " and the output of
	  Singh and Swanson's algorithm is already these
	  fractions."
	  }
     }

document {
     Key => {isNormal2, (isNormal2, Ring)},
     Headline => "determine if a reduced ring is normal",
     Usage => "isNormal2 R",
     Inputs => {"R" => {ofClass Ring}},
     Outputs => {{ofClass Boolean, " that is true if ", TT "R", " is normal in the 
	       sense of satisfying Serre's conditions S2 and R1 and false if one or 
	       both conditions fail"}},      
     EXAMPLE{
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
	  "isNormal2 R",
	  "isNormal2(integralClosure2 R)",
	  },
     PARA{},
     "This function computes the jacobian of the ring which can be costly for 
     larger rings.  Therefore it checks the less costly S2 condition first and if 
     true, then tests the R1 condition using the jacobian of ", TT "R", "."
     }	 

--- needs better examples and check on how it reads...

document {
     Key => {integralClosure2, (integralClosure2, Ring)},
     Headline => "compute the integral closure of a reduced ring",
     Usage => "integralClosure2 R",
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
     a matrix using the function ", TO "icFractions2", " where R is 
     an affine domain.  This function can be run without first 
     using ", TO "integralClosure2", ".  The natural map from ", TT "R", " into 
     its integral closure is obtained using the function ", TO "icMap2", " and 
     the conductor2 of the integral closure of R into R is found 
     using ", TT "conductor2 (icMap2 R)", ".  Note that 
     both ", TO "icFractions2", " and ", TO "icMap2", " take the input 
     ring ", TT "R", " as input rather than the output 
     of ", TO "integralClosure2", ".  In this way you can use these 
     functions without running ", TO "integralClosure2", ".  The 
     function ", TO "integralClosure2", " is based on
     Theo De Jong's paper, An Algorithm for 
     Computing the Integral Closure, J. Symbolic Computation, 
     (1998) 26, 273-277.  This implementation is written and maintained 
     by Amelia Taylor, ", HREF {"mailto:amelia.taylor@coloradocollege.edu", 
     "<amelia.taylor@coloradocollege.edu>"}, ".",
     PARA{},
     "A domain example.",
      EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
      	  "integralClosure2 R"},
     "In the case that the input ring ", TT "R", " is reduced, but not a domain, 
     the direct sum of the rings returned is isomporphic to the integral closure 
     of ", TT "R", ", but the rings returned are not necessarily all domains.",
     EXAMPLE{"S=ZZ/101[a..d]/ideal(a*(b-c),c*(b-d),b*(c-d));",
	  "integralClosure2 S"
	  },
     "The algorithm can correctly  proceed without decomposing a reduced ring 
     if it finds a non-zero divisor ", TT "f", " with which to compute 1/f(fJ:J), 
     where ", TT "J", " is the ideal defining the non-normal locus at that stage.",
     PARA{},
     "A package implementing product rings is in development.  When this package is 
     complete, the interface for the integralClosure2 code will change.  
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
     using ", TO "integralClosure2", ".  The natural map from ", TT "R", " into 
     its integral closure is obtained using the function ", TO "icMap2", " and 
     the conductor2 of the integral closure of R into R is found 
     using ", TT "conductor2 (icMap2 R)", ".  Note that 
     both ", TO "icFractions2", " and ", TT "icMap2", " take the input 
     ring ", TT "R", " as input rather than the output 
     of ", TO "integralClosure2", ".  In this way you can use these 
     functions without running ", TT "integralClosure2", ".",
     SeeAlso => {"icMap2", "icFractions2", "conductor2", "isNormal2"}
     }
    
document {
     Key => [integralClosure2,Variable],
     Headline=> "Sets the name of the indexed variables introduced in computing 
     the integral closure of a reduced ring."
     }

document {
     Key => [integralClosure2,Limit],
     Headline=> "Sets the recursion level for the program allowing the
     user to see results without computing the full integral closure."
     }

document {
     Key => {idealizer2, (idealizer2, Ideal, RingElement)},
     Headline => "Compute Hom(I,I) as quotient ring",
     Usage => "idealizer2(I, f)",
     Inputs => {"I" => {ofClass Ideal},
	  "f" => {{ofClass RingElement}, " that is a non-zero divisor in the
	  ring of ", TT "I"},
	  Variable => {" an unassigned symbol"},
	  Index2 => {" an integer"}},
     Outputs => {{ofClass Sequence, " where the first item is ", 
	       ofClass RingMap, " from the ring of ", TT "I", " to a
	       presentation of ", TT "Hom(I,I) = 1/f(f*J:J)", " and
	       the second item is ", ofClass List,
	       " consisting of the fractions that are added to the ring of J
	       to form ", TT "Hom(I,I)", "."}},
	       "We use this in integralClosure2 to complete a key step
	       in deJong's algorithm. Interested users might want to
	       use this to investigate different choices for ", 
	       TT "f", " in the algorithm."
     }

document {
     Key => [idealizer2,Variable],
     Headline=> "Sets the name of the indexed variables introduced in computing 
     the endomorphism ring Hom(J,J)."
     }

document {
     Key => Index2,
     Headline => "Optional input for idealizer2",
     PARA{},
     "This option allows the user to select the starting index for the
     new variables added in computing Hom(J,J) as a ring.  The default
     value is 0 and is what most users will use.  The option is needed
     for the recurion implemented in integralClosure2."
}


document {
     Key => [idealizer2, Index2],
     Headline=> "Sets the starting index on the new variables used to
     build the endomorphism ring Hom(J,J). If the program idealizer2 is
     used independently, the user will generally want to use the
     default value of 0.  However, when used as part of the
     integralClosure2 computation the number needs to start higher
     depending on the level of recursion involved. "
     }

document {
     Key => {nonNormalLocus2, (nonNormalLocus2, Ring)},
     Headline => "an ideal containing the non normal locus of a ring",
     Usage => "nonNormalLocus2 R",
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

--- I don't love the third example in icMap2
document {
     Key => {icMap2, (icMap2,Ring)},
     Headline => "natural map from an affine domain into its integral closure.",
     Usage => "icMap2 R",
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
	  "icMap2 R"},
     "This finite map is needed to compute the ", TO "conductor2", " of the integral closure 
     into the original ring.",
    	  EXAMPLE {
	  "S = QQ[a,b,c]/ideal(a^6-c^6-b^2*c^4);",
      	  "conductor2(icMap2 S)"},
     PARA{},
     "If the user has already run the computation ", TT "integralClosure2 R", 
     " then this map can also be obtained by typing ",
     TT "R.icMap2", ".",
     EXAMPLE { 
	  "integralClosure2 S;",
	  "S.icMap2"},
     SeeAlso => {"conductor2"},
     }
    

document {
     Key => {icFractions2, (icFractions2,Ring)},
     Headline => "Compute the fractions integral over a domain.",
     Usage => "icFractions2 R",
     Inputs => {
	  "R" => {ofClass Ring, " that is an affine domain"},
	  },
     Outputs => {
  	  {ofClass List, " whose entries are fractions that generate the integral 
     	       closure of ", TT "R", " over R."}
	       },
    	  EXAMPLE {
	       "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
	       "integralClosure2(R,Variable => a)",
	       "icFractions2 R"
	       },
     	  "Thus the new variables ", TT "a_7", " and ", TT "a_6", " in
	  the output from ", TT "integralClosure2", " correspond to the 
     	  last two fractions given.  The other fractions are those
	  returned in intermediate recursive steps in the computation of the
	  integral closure. ", TT "a_0", " for example corresponds to the first
	  fraction to the left of the original ring variables.  
	  The program currently also returns the original 
    	  variables as part of the matrix.  In this way the user can see if any are 
     	  simplified out of the ring during the process of computing the integral
     	  closure.",
     	  PARA{},
	  "A future version of icFractions2 will return only the
	  fractions corresponding to the variables returned by the
	  function integralClosure2. Thus the general format will be
	  much easier to use"
--     	  "The fractions returned correspond to the variables returned by the function 
--     	  integralClosure2.  The function integralClosure2 eliminates redundant fractions 
--     	  during its iteration.  If the user would like to see all fractions generated 
--     	  during the computation, use the optional argument ", TT "Strategy => Long", " as 
--     	  illustrated here.",
--	      	  EXAMPLE {
--	       "icFractions2(R)"
--	       },
     	  }

--document {
--     Key => [icFractions2,Strategy],
--     Headline=> "Allows the user to obtain all of the fractions considered in the 
--     process of building the integral closure",
--     }

document {
     Key => {conductor2,(conductor2,RingMap)},
     Headline => "compute the conductor2 of a finite ring map",
     Usage => "conductor2 F",
     Inputs => {
	  "F" => {ofClass RingMap, " from a ring ", TT "R", " to a ring ", TT "S", 
	       ". The map must be a finite."},
	  },
     Outputs => {
	  {ofClass Ideal, " that is the conductor2 of ", TT "S", " into ", TT "R", "."}
	  },
     "Suppose that the ring map F : R --> S is finite: i.e. S is a finitely 
     generated R-module.  The conductor2 of F is defined to be {",
     TEX "g \\in R \\mid g S \\subset f(R)", "}.  One way to think
     about this is that the conductor2 is the set of universal denominators
     of ", TT "S", " over ", TT "R", ", or as the largest ideal of ", TT "R", " 
     which is also an ideal in ", TT "S", ". On natural use is the
     conductor2 of the map from a ring to its integral closure. ",
     EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
	  "S = integralClosure2 R",
	  "F = R.icMap2",
	  "conductor2 F"
	  },
     PARA{},
     "The command ", TT "conductor2", " calls the 
     command ", TO pushForward, ".  Currently, the 
     command ", TT "pushForward", 
     " does not work if the source of the map ", TT "F", " is
     inhomogeneous.  If the source of the map ", TT "F", " is not
     homogeneous ", TT "conductor2", " returns the message -- No
     conductor2 for ", TT "F", ".",
     SeeAlso =>{"pushForward", "integralClosure2", "icMap2"} 
     }

document {
     Key => {icFracP2, (icFracP2, Ring)},
     Headline => "compute the integral closure in prime characteristic",
     Usage => "icFracP2 R, icFracP2(R, conductor2Element => D), icFracP2(R, Limit => N), icFracP2(R, reportSteps2 => Boolean)",
     Inputs => {
	"R" => {"that is reduced, equidimensional,
           finitely and separably generated over a field of characteristic p"},
	conductor2Element => {"optionally provide a non-zerodivisor conductor2 element ",
               TT "conductor2Element => D", ";
               the output is then the module generators of the integral closure.
               A good choice of ", TT "D", " may speed up the calculations?"},
	Limit => {"if value N is given, perform N loop steps only"},
	reportSteps2 => {"if value true is given, report the conductor2 element and number of steps in the loop"},
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
     is the computation of a conductor2 element ", TT "D",
     " that is a non-zerodivisor;
     its existence is guaranteed by the separability assumption.
     The user may supply ", TT "D",
     " with the optional ", TT "conductor2Element => D", ".
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
     and the optional ", TT "reportSteps2 => true", " reports the conductor2
     element and the number of steps it took for the loop to stabilize.
     The algorithm is based on the
     Leonard--Pellikaan--Singh--Swanson algorithm.",
     PARA{},
     "A simple example.",
     EXAMPLE {
          "R = ZZ/5[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
          "icFracP2 R"
     },
     "The user may provide an optional non-zerodivisor conductor2 element ",
     TT "D",
     ".  The output generators need not
     be expressed in  the form with denominator ", TT "D", ".",
     EXAMPLE {
          "R = ZZ/5[x,y,u,v]/ideal(x^2*u-y^2*v);",
          "icFracP2(R)",
          "icFracP2(R, conductor2Element => x)",
     },
     "In case ", TT "D", " is not in the conductor2, the output is ",
     TT "V_e = (1/D) {r in R | r^(p^i) in (D^(p^i-1)) ", "for ",
     TT "i = 1, ..., e}",
     " such that ", TT "V_e = V_(e+1)", " and ", TT "e",
     " is the smallest such ", TT "e", ".",
     EXAMPLE {
	  "R=ZZ/2[u,v,w,x,y,z]/ideal(u^2*x^3+u*v*y^3+v^2*z^3);",
          "icFracP2(R)",
          "icFracP2(R, conductor2Element => x^2)"
     },
     "The user may also supply an optional limit on the number of steps
     in the algorithm.  In this case, the output is a finitely generated ",
     TT "R", "-module contained in ", TT "(1/D)R",
     " which contains the integral closure (intersected with ", TT "(1/D)R",
     ".",
     EXAMPLE {
          "R=ZZ/2[u,v,w,x,y,z]/ideal(u^2*x^3+u*v*y^3+v^2*z^3);",
          "icFracP2(R, Limit => 1)",
          "icFracP2(R, Limit => 2)",
          "icFracP2(R)"
     },
     "With the option above one can for example determine how many
     intermediate modules the program should compute or did compute
     in the loop to get the integral closure.  A shortcut for finding
     the number of steps performed is to supply the ",
     TT "reportSteps2 => true", " option.",
     EXAMPLE {
          "R=ZZ/3[u,v,w,x,y,z]/ideal(u^2*x^4+u*v*y^4+v^2*z^4);",
          "icFracP2(R, reportSteps2 => true)"
     },
     "With this extra bit of information, the user can now compute
     integral closures of principal ideals in ", TT "R", " via ",
     TO icPIdeal2, ".",
     SeeAlso => {"icPIdeal2", "integralClosure2", "isNormal2"},
     Caveat => "NOTE: mingens is not reliable, neither is kernel of the zero map!!!"
}

document {
     Key => conductor2Element,
     Headline => "Specifies a particular non-zerodivisor in the conductor2."
}

document {
     Key => [icFracP2,conductor2Element],
     Headline => "Specifies a particular non-zerodivisor in the conductor2.",
     "A good choice can possibly speed up the calculations.  See ",
     TO icFracP2, "."
}


document {
     Key => [icFracP2,Limit],
     Headline => "Limits the number of computed intermediate modules.",
     Caveat => "NOTE: How do I make M2 put icFracP2 on the list of all functions that use Limit?"
}

document {
     Key => reportSteps2,
     Headline => "Optional in icFracP2",
     PARA{},
     "With this option, ", TT "icFracP2",
     " prints out the conductor2 element and
           the number of intermediate modules it computed;
           in addition to the output being
           the module generators of the integral closure of the ring.",
     Caveat => "NOTE: There is probably a better name for this, or a better way of doing this."
}

document {
     Key => [icFracP2,reportSteps2],
     Headline => "Prints out the conductor2 element and
           the number of intermediate modules it computed.",
     Usage => "icFracP2(R, reportSteps2 => Boolean)",
     "The main use of the extra information is in computing the
     integral closure of principal ideals in ", TT "R",
     ", via ", TO icPIdeal2,
     ".",
     EXAMPLE {
          "R=ZZ/3[u,v,x,y]/ideal(u*x^2-v*y^2);",
          "icFracP2(R, reportSteps2 => true)",
	  "S = ZZ/3[x,y,u,v];",
          "R = S/kernel map(S,S,{x-y,x+y^2,x*y,x^2});",
	  "icFracP2(R, reportSteps2 => true)"
     },
}

document {
     Key => {icPIdeal2,(icPIdeal2, RingElement, RingElement, ZZ)},
     Headline => "compute the integral closure
                  in prime characteristic of a principal ideal",
     Usage => "icPIdeal2 (a, D, N)",
     Inputs => {
	"a" => {"an element in ", TT "R"},
        "D" => {"a non-zerodivisor of ", TT "R",
                " that is in the conductor2"},
        "N" => {"the number of steps in ", TO icFracP2,
                " to compute the integral closure of ", TT "R",
                ", by using the conductor2 element ", TT "D"}},
     Outputs => {{"the integral closure of the ideal ", TT "(a)", "."}},
     "The main input is an element ", TT "a",
     " which generates a principal ideal whose integral closure we are
     seeking.  The other two input elements,
     a non-zerodivisor conductor2 element ", TT "D",
     " and the number of steps ", TT "N", 
     " are the pieces of information obtained from ",
     TT "icFracP2(R, reportSteps2 => true)",
     ".  (See the Singh--Swanson paper, An algorithm for computing
     the integral closure, Remark 1.4.)",
     EXAMPLE {
          "R=ZZ/3[u,v,x,y]/ideal(u*x^2-v*y^2);",
          "icFracP2(R, reportSteps2 => true)",
          "icPIdeal2(x, x^2, 3)"
     },
     SeeAlso => {"icFracP2"}
}

-- integrally closed test
TEST ///
R = QQ[u,v]/ideal(u+2)
time J = integralClosure2 (R,Variable => symbol a) 
use ring ideal J
assert(ideal J == ideal(u+2))
icFractions2 R  -- NOT GOOD?
///

-- degrees greater than 1 test
TEST ///
R = ZZ/101[symbol x..symbol z,Degrees=>{2,5,6}]/(z*y^2-x^5*z-x^8)
time J = integralClosure2 (R,Variable => symbol b) 
use ring ideal J
answer = ideal(b_1*x^2-y*z, x^6-b_1*y+x^3*z, -b_1^2+x^4*z+x*z^2)
assert(ideal J == answer)
use R
assert(conductor2(R.icMap2) == ideal(x^2,y))
assert((icFractions2 R).matrix == substitute(matrix {{y*z/x^2, x, y, z}},frac R))
target icFractions2 R === frac R
source icFractions2 R === J
///

-- multigraded test
TEST ///
R = ZZ/101[symbol x..symbol z,Degrees=>{{1,2},{1,5},{1,6}}]/(z*y^2-x^5*z-x^8)
time J = integralClosure2 (R,Variable=>symbol a) 
use ring ideal J
assert(ideal J == ideal(-x^6+a_3*y-x^3*z,-a_3*x^2+y*z,a_3^2-x^4*z-x*z^2))
assert(0 == (icFractions2 R).matrix - substitute(matrix {{y*z/x^2, x, y, z}},frac R))
///

-- multigraded homogeneous test
TEST ///
R = ZZ/101[symbol x..symbol z,Degrees=>{{4,2},{10,5},{12,6}}]/(z*y^2-x^5*z-x^8)
time J = integralClosure2 (R,Variable=>symbol a) 
use ring ideal J
assert(ideal J == ideal(a_1*x^2-y*z,a_1*y-x^6-x^3*z,a_1^2-x^4*z-x*z^2))
assert(0 == (icFractions2 R).matrix - substitute(matrix {{y*z/x^2, x, y, z}},frac R))
-- This was the old answer, before changing choice of J0
--assert(ideal J == ideal(a_1*y-42*x^6-42*x^3*z,a_1^2-47*x^4*z-47*x*z^2,-12*a_1*x^2-z*y,-12*a_1*x*y-x^7-x^4
--      *z,43*a_1^2*x^2-x^6*z-x^3*z^2,-x^8-x^5*z+z*y^2))
use R
assert(conductor2(R.icMap2) == ideal(x^2,y))
///

-- Reduced not a domain test
TEST ///
S=ZZ/101[symbol a,symbol b,symbol c, symbol d]
I=ideal(a*(b-c),c*(b-d),b*(c-d))
R=S/I                              
time V = integralClosure2 R  -- FAILS in new version
assert(#V == 2)
icMap2 R
-- assert false-- added: MES
-- the second ring is not a domain.  Anyway, the fractions are messed up here.
///

--Craig's example as a test
TEST ///
S=ZZ/101[symbol x,symbol y,symbol z,MonomialOrder => Lex]
I=ideal(x^6-z^6-y^2*z^4)
Q=S/I
time J = integralClosure2 (Q, Variable => symbol a)
use ring ideal J
assert(ideal J == ideal (x^2-a_6*z, a_6*x-a_7*z, a_6^2-a_7*x, a_7^2-y^2-z^2))
use Q
assert(conductor2(Q.icMap2) == ideal(z^3,x*z^2,x^3*z,x^4))
assert((icFractions2 Q).matrix == substitute(matrix{{x^3/z^2,x^2/z,x,y,z}},frac Q))
///

--Mike's inhomogenous test
TEST ///
R = QQ[symbol a..symbol d]
I = ideal(a^5*b*c-d^2)
Q = R/I
L = time integralClosure2(Q,Variable => symbol x)
use ring ideal L
assert(ideal L == ideal(x_1^2-a*b*c))
use Q
(icFractions2 Q).matrix == substitute(matrix{{d/a^2,a,b,c}},frac Q)
///

--Ex from Wolmer's book - tests longer example and published result.
TEST ///
R = QQ[symbol a..symbol e]
I = ideal(a^2*b*c^2+b^2*c*d^2+a^2*d^2*e+a*b^2*e^2+c^2*d*e^2,a*b^3*c+b*c^3*d+a^3*b*e+c*d^3*e+a*d*e^3,a^5+b^5+c^5+d^5-5*a*b*c*d*e+e^5,a^3*b^2*c*d-b*c^2*d^4+a*b^2*c^3*e-b^5*d*e-d^6*e+3*a*b*c*d^2*e^2-a^2*b*e^4-d*e^6,a*b*c^5-b^4*c^2*d-2*a^2*b^2*c*d*e+a*c^3*d^2*e-a^4*d*e^2+b*c*d^2*e^3+a*b*e^5,a*b^2*c^4-b^5*c*d-a^2*b^3*d*e+2*a*b*c^2*d^2*e+a*d^4*e^2-a^2*b*c*e^3-c*d*e^5,b^6*c+b*c^6+a^2*b^4*e-3*a*b^2*c^2*d*e+c^4*d^2*e-a^3*c*d*e^2-a*b*d^3*e^2+b*c*e^5,a^4*b^2*c-a*b*c^2*d^3-a*b^5*e-b^3*c^2*d*e-a*d^5*e+2*a^2*b*c*d*e^2+c*d^2*e^4)
S = R/I
time V = integralClosure2 (S, Variable => X)
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
(icFractions2 S).matrix
///

-- Test of icFractions2
--TEST 
--///
--S = QQ [(symbol Y)_1, (symbol Y)_2, (symbol Y)_3, (symbol Y)_4, symbol x, symbol y, Degrees => {{7, 1}, {5, 1}, {6, 1}, {6, 1}, {1, 0}, {1, 0}}, MonomialOrder => ProductOrder {4, 2}]
--J = ideal(Y_3*y-Y_2*x^2,Y_3*x-Y_4*y,Y_1*x^3-Y_2*y^5,Y_3^2-Y_2*Y_4*x,Y_1*Y_4-Y_2^2*y^3)
--T = S/J       
--assert(icFractions2 T == substitute(matrix {{(Y_2*y^2)/x, (Y_1*x)/y, Y_1, Y_2, Y_3, Y_4, x, y}}, frac T))
--///

-- Test of isNormal2
TEST ///
S = ZZ/101[x,y,z]/ideal(x^2-y, x*y-z^2)
assert(isNormal2(S) == false)
assert(isNormal2(integralClosure2(S)) == true)
///

-- Test of icMap2 and conductor2
TEST ///
R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4)
J = integralClosure2(R);
F = R.icMap2
assert(conductor2 F == ideal((R_2)^3, (R_0)*(R_2)^2, (R_0)^3*(R_2), (R_0)^4))
(icFractions2 R).matrix
///

end 

TEST ///
  S = QQ[y,x,MonomialOrder=>Lex]
  F = poly"y5-y2+x3+x4"
  factor discriminant(F,y)
  R=S/F
  R' = integralClosure2 R
  (icFractions2 R).matrix
  describe R'
///

TEST ///
  -- of idealizer
  S = QQ[y,x,MonomialOrder=>Lex]
  F = poly"y4-y2+x3+x4"
  factor discriminant(F,y)
  R=S/F
  L = trim radical ideal(x_R)
  (f1,g1,fra) = idealizer2(L,L_0)
  U = target f1
  K = frac R
  f1
  g1
  fra

  L = trim ideal jacobian R

  R' = integralClosure2 R
  icFractions2 R
  icMap2 R
///

















---- Homogeneous Ex
loadPackage"IntegralClosure2"
loadPackage"ParameterSchemes"
R = ZZ/101[x,y, z]
I1 = ideal(x,y-z)
I2 = ideal(x-3*z, y-5*z)
I3 = ideal(x,y)
I4 = ideal(x-5*z,y-2*z)

I = intersect(I1^3, I2^3, I3^3, I4^3)
f = I_0 + I_1 + I_2+ I_3
S = R/f
V = integralClosure2(S)
ring(presentation V)

installPackage "IntegralClosure2"

-- Tests that Mike has added:
loadPackage "IntegralClosure2"
S = ZZ/101[a..d]
I = ideal(b^2-b)
R = S/I
integralClosure2(R)

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
loadPackage "IntegralClosure2"
nonNormalLocus2 A  -- crashes M2!

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
(phi,fracs) = idealizer2(NNL,NNL_0)
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
integralClosure2 A

---------------------- Birational Work

R = ZZ/101[b_1, x,y,z, MonomialOrder => {GRevLex => {7}, GRevLex=>{2,5,6}}]
R = ZZ/101[x,y,z]
S = R[b_1, b_0]
I = ideal(b_1*x^2-42*y*z, x^6+12*b_1*y+ x^3*z, b_1^2 - 47*x^4*z - 47*x*z^2)
I = ideal(b_1*x-42*b_0, b_0*x-y*z, x^6+12*b_1*y+ x^3*z, b_1^2 -47*x^4*z - 47*x*z^2, b_0^2-x^6*z - x^4*z^2)
leadTerm gens gb I

R = ZZ/101[x,y,z]/(z*y^2-x^5*z-x^8)
J = integralClosure2(R)
R.icFractions2
describe J


S=ZZ/101[symbol x,symbol y,symbol z,MonomialOrder => Lex]
I=ideal(x^6-z^6-y^2*z^4)
Q=S/I
time J = integralClosure2 (Q, Variable => symbol a)


S = ZZ/101[a_7,a_6,x,y,z, MonomialOrder => {GRevLex => 2, GRevLex => 3}]
Inew = ideal(x^2-a_6*z,a_6*x-a_7*z,a_6^2-a_7*x,a_7^2-y^2-z^2)
leadTerm gens gb Inew
radical ideal oo


----- Minimal Presentation Tests 
///
restart
loadPackage "IntegralClosure2"

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
loadPackage"IntegralClosure2"
R=ZZ/2[x,y,Weights=>{{8,9},{0,1}}]
I=ideal(y^8+y^2*x^3+x^9) -- eliminates x and y at some point. 
R=ZZ/2[x,y,Weights=>{{31,12},{0,1}}]
I=ideal"y12+y11+y10x2+y8x9+x31" -- really long
S=integralClosure2(R/I)
transpose gens ideal S

M = flattenRing (R/I)
J = nonNormalLocus2 M_0
phi = M_1
fractions = gens M_0
indexVar = 0

S = target phi
I = ideal presentation target phi
R = ring I
J1 = trim(ideal(0_S):J_0)
J1 != ideal(0_S) 
(newPhi, fracs) = idealizer2(J, J_0, Index2 => indexVar);
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


------------------------------------------------------------------
-- Examples coming from Janko Boehm:
loadPackage "IntegralClosure2"
kk = QQ
kk = ZZ/32003
S=kk[u,v,z]

L={v^4-2*u^3*z+3*u^2*z^2-2*v^2*z^2,
   (v^2-u*z)^2-u^3*z,
   v^5+2*u*v^2*z^2+2*u*v^3*z+u^2*v*z^2-4*u^3*v*z+2*u^5,
   25*u^8+184*u^7*v+518*u^6*v^2+720*u^5*v^3+576*u^4*v^4+282*u^3*v^5+84*u^2*v^6+14*u*v^7+v^8+244*u^7*z+1326*u^6*v*z+2646*u^5*v^2*z+2706*u^4*v^3*z+1590*u^3*v^4*z+546*u^2*v^5*z+102*u*v^6*z+8*v^7*z+854*u^6*z^2+3252*u^5*v*z^2+4770*u^4*v^2*z^2+3582*u^3*v^3*z^2+1476*u^2*v^4*z^2+318*u*v^5*z^2+28*v^6*z^2+1338*u^5*z^3+3740*u^4*v*z^3+4030*u^3*v^2*z^3+2124*u^2*v^3*z^3+550*u*v^4*z^3+56*v^5*z^3+1101*u^4*z^4+2264*u^3*v*z^4+1716*u^2*v^2*z^4+570*u*v^3*z^4+70*v^4*z^4+508*u^3*z^5+738*u^2*v*z^5+354*u*v^2*z^5+56*v^3*z^5+132*u^2*z^6+122*u*v*z^6+28*v^2*z^6+18*u*z^7+8*v*z^7+z^8,
   u^6+3*u^4*v^2+3*u^2*v^4+v^6-4*u^4*z^2-34*u^3*v*z^2-7*u^2*v^2*z^2+12*u*v^3*z^2+6*v^4*z^2+36*u^2*z^4+36*u*v*z^4+9*v^2*z^4,
   v^9+4*u*v^7*z+4*v^8*z+6*u^2*v^5*z^2+12*u*v^6*z^2+6*v^7*z^2+4*u^3*v^3*z^3+12*u^2*v^4*z^3+12*u*v^5*z^3+3*v^6*z^3+u^4*v*z^4+4*u^3*v^2*z^4+6*u^2*v^3*z^4+3*u*v^4*z^4-2*v^5*z^4+u^2*v^2*z^5+u*v^3*z^5-2*v^4*z^5+u^3*z^6+3*u^2*v*z^6-u*v^2*z^6-6*v^3*z^6-2*u^2*z^7-5*u*v*z^7-4*v^2*z^7-v*z^8,
   36*u^7+64*u^6*v+1440*u^5*v^2+216*u^4*v^3+120*u^3*v^4+1728*u^2*v^5+243*u*v^6+36*v^7-96*u^6*z+108*u^5*v*z+16*u^4*v^2*z+576*u^3*v^3*z+216*u^2*v^4*z-12*u*v^5*z+216*v^6*z+88*u^5*z^2+3456*u^4*v*z^2+243*u^3*v^2*z^2+124*u^2*v^3*z^2+3816*u*v^4*z^2+297*v^5*z^2+162*u^4*z^3-20*u^3*v*z^3+936*u^2*v^2*z^3+342*u*v^3*z^3-96*v^4*z^3+1944*u^3*z^4+81*u^2*v*z^4-32*u*v^2*z^4+1536*v^3*z^4,
   u^5*v^5+21*u^5*v^4*z-36*u^4*v^5*z-19*u^5*v^3*z^2+12*u^4*v^4*z^2+57*u^3*v^5*z^2+u^5*v^2*z^3+u^4*v^3*z^3-53*u^3*v^4*z^3-19*u^2*v^5*z^3+u^5*v*z^4+43*u^3*v^3*z^4+u*v^5*z^4+u^5*z^5-15*u^3*v^2*z^5+u^2*v^3*z^5+u*v^4*z^5+v^5*z^5,
   u^4-14*u^2*v^2+v^4+8*u^2*v*z+8*v^3*z,
   14440*u^5-16227*u^4*v+10812*u^3*v^2-13533*u^2*v^3+3610*u*v^4+1805*v^5+14440*u^4*z-18032*u^3*v*z+16218*u^2*v^2*z-12626*u*v^3*z+3610*v^4*z+3610*u^3*z^2-4508*u^2*v*z^2+5406*u*v^2*z^2-2703*v^3*z^2,
   -24135/322*u^6-532037/6440*u^5*v+139459/560*u^4*v^2-1464887/12880*u^3*v^3+72187/25760*u^2*v^4+9/8*u*v^5+1/8*v^6-403511/3220*u^5*z-40817/920*u^4*v*z+10059/80*u^3*v^2*z-35445/1288*u^2*v^3*z+19/4*u*v^4*z+3/4*v^5*z-20743/805*u^4*z^2+126379/3220*u^3*v*z^2-423417/6440*u^2*v^2*z^2+11/2*u*v^3*z^2+3/2*v^4*z^2+3443/140*u^3*z^3+u^2*v*z^3+u*v^2*z^3+v^3*z^3,
   u^9+u^7*v^2-u^6*v^3-6*u^5*v^4+3*u^4*v^5+4*u^3*v^6-3*u^2*v^7-u*v^8+v^9+4*u^8*z-u^7*v*z+5*u^6*v^2*z+9*u^5*v^3*z-21*u^4*v^4*z-6*u^3*v^5*z+16*u^2*v^6*z+u*v^7*z-5*v^8*z+6*u^7*z^2-4*u^6*v*z^2+3*u^5*v^2*z^2+30*u^4*v^3*z^2-12*u^3*v^4*z^2-27*u^2*v^5*z^2+6*u*v^6*z^2+10*v^7*z^2+4*u^6*z^3-6*u^5*v*z^3-8*u^4*v^2*z^3+25*u^3*v^3*z^3+15*u^2*v^4*z^3-14*u*v^5*z^3-10*v^6*z^3+u^5*z^4-4*u^4*v*z^4-10*u^3*v^2*z^4+2*u^2*v^3*z^4+11*u*v^4*z^4+5*v^5*z^4-u^3*v*z^5-3*u^2*v^2*z^5-3*u*v^3*z^5-v^4*z^5,
   1251*v^4*z^3+5184*u*v^3*z^3+5354*u^2*v^2*z^3+115*u^4*z^3-9552*u*v^4*z^2-22496*u^2*v^3*z^2-5424*u^3*v^2*z^2-32*115/23*u^4*v*z^2+192*115*u^2*v^4*z+17472*u^3*v^3*z-13824*u^3*v^4,
   -2*u*v^4*z^4+u^4*v^5+12*u^4*v^3*z^2+12*u^2*v^4*z^3-u^3*v*z^5+11*u^3*v^2*z^4-21*u^3*v^3*z^3-4*u^4*v*z^4+2*u^4*v^2*z^3-6*u^4*v^4*z+u^5*z^4-3*u^5*v^2*z^2+u^5*v^3*z-3*u*v^5*z^3-2*u^2*v^3*z^4+u^3*v^4*z^2+v^5*z^4,
   u^10+6*u^9*v-30*u^7*v^3-15*u^6*v^4+u^5*v^5+u^4*v^6+6*u^3*v^7+u^2*v^8+7*u*v^9+v^10+5*u^9*z+24*u^8*v*z-30*u^7*v^2*z-120*u^6*v^3*z-43*u^5*v^4*z+5*u^4*v^5*z+20*u^3*v^6*z+10*u^2*v^7*z+29*u*v^8*z+5*v^9*z+10*u^8*z^2+36*u^7*v*z^2-105*u^6*v^2*z^2-179*u^5*v^3*z^2-38*u^4*v^4*z^2+25*u^3*v^5*z^2+25*u^2*v^6*z^2+46*u*v^7*z^2+10*v^8*z^2+10*u^7*z^3+24*u^6*v*z^3-135*u^5*v^2*z^3-117*u^4*v^3*z^3-u^3*v^4*z^3+25*u^2*v^5*z^3+34*u*v^6*z^3+10*v^7*z^3+5*u^6*z^4+6*u^5*v*z^4-75*u^4*v^2*z^4-27*u^3*v^3*z^4+10*u^2*v^4*z^4+11*u*v^5*z^4+5*v^6*z^4+u^5*z^5-15*u^3*v^2*z^5+u^2*v^3*z^5+u*v^4*z^5+v^5*z^5,
   2*u^7+u^6*v+3*u^5*v^2+u^4*v^3+2*u^3*v^4+u^2*v^5+2*u*v^6+v^7-7780247/995328*u^6*z-78641/9216*u^5*v*z-10892131/995328*u^4*v^2*z-329821/31104*u^3*v^3*z-953807/331776*u^2*v^4*z-712429/248832*u*v^5*z+1537741/331776*v^6*z+2340431/248832*u^5*z^2+5154337/248832*u^4*v*z^2+658981/41472*u^3*v^2*z^2+1737757/124416*u^2*v^3*z^2-1234733/248832*u*v^4*z^2-1328329/82944*v^5*z^2-818747/248832*u^4*z^3-1822879/124416*u^3*v*z^3-415337/31104*u^2*v^2*z^3+1002655/124416*u*v^3*z^3+849025/82944*v^4*z^3,
   u^11+3*u^10*v+2*u^9*v^2+u^8*v^3+2*u^7*v^4+u^6*v^5+3*u^5*v^6+u^4*v^7+2*u^3*v^8+u^2*v^9+2*u*v^10+v^11-37646523511/5159780352*u^10*z-12735172937/644972544*u^9*v*z-92722810205/5159780352*u^8*v^2*z-6771611725/322486272*u^7*v^3*z-79705721155/2579890176*u^6*v^4*z-5691795857/161243136*u^5*v^5*z-52315373005/2579890176*u^4*v^6*z+2598387077/322486272*u^3*v^7*z+157674139405/5159780352*u^2*v^8*z+9450269981/644972544*u*v^9*z-1350789043/1719926784*v^10*z+12849479611/644972544*u^9*z^2+3879535279/71663616*u^8*v*z^2+11488988309/161243136*u^7*v^2*z^2+16022496731/161243136*u^6*v^3*z^2+14783031067/107495424*u^5*v^4*z^2+34074776537/322486272*u^4*v^5*z^2-2606453339/161243136*u^3*v^6*z^2-7551362827/53747712*u^2*v^7*z^2-71147279173/644972544*u*v^8*z^2-4491673835/214990848*v^9*z^2-5255202913/214990848*u^8*z^3-8675467489/107495424*u^7*v*z^3-1706519429/11943936*u^6*v^2*z^3-22409190037/107495424*u^5*v^3*z^3-11738664739/53747712*u^4*v^4*z^3-1469700833/35831808*u^3*v^5*z^3+23678835685/107495424*u^2*v^6*z^3+24664591385/107495424*u*v^7*z^3+4743235967/71663616*v^8*z^3+1955990257/161243136*u^7*z^4+10614149851/161243136*u^6*v*z^4+23866276253/161243136*u^5*v^2*z^4+10551670493/53747712*u^4*v^3*z^4+14951486563/161243136*u^3*v^4*z^4-22801627471/161243136*u^2*v^5*z^4-32691689713/161243136*u*v^6*z^4-3808427873/53747712*v^7*z^4-362334499/322486272*u^6*z^5-596548295/26873856*u^5*v*z^5-21950924039/322486272*u^4*v^2*z^5-1124117785/20155392*u^3*v^3*z^5+1234417927/35831808*u^2*v^4*z^5+5479236793/80621568*u*v^5*z^5+2660429561/107495424*v^6*z^5,
   5*v^6+7*v^2*u^4+6*u^6+21*v^2*u^3+12*u^5+21*v^2*u^2+6*u^4+7*v^2*u}


R = S/L_3
time integralClosure R
icFractions R
use S
discriminant(L_2,v)
factor oo



kk = ZZ/32003
S = kk[u,v]
I=ideal(5*v^6+7*v^2*u^4+6*u^6+21*v^2*u^3+12*u^5+21*v^2*u^2+6*u^4+7*v^2*u)
R = S/I
L = frac R
ib=matrix {{1, v, v^2, v^3/(u+1), v^4/(u^2+u), (v^5-7/5*v*u^3+7/5*v*u)/(u^3+u^2)}}
A = kk[x_0..x_5]
map(frac R, A, matrix{{u}} | ib_{1..5})
ker oo 
flatten entries ib
oo_4 * oo_5
use S
factor discriminant(I_0,u)
factor discriminant(I_0,v)


-- Example of plane curve singularity
restart
S = QQ[y,x,MonomialOrder=>Lex]
F = poly"y4-y2+x3+x4"
factor discriminant(F,y)
R=S/F
basis(R, Variables=>{y})

use S
My = matrix"0,0,0,-x3-x4;
  1,0,0,0;
  0,1,0,1;
  0,0,1,0"
trace My^0
trace My^2
trace My^3
trace My^4
trace My^5
trace My^6
My^4-My^2
W = matrix for i from 0 to 3 list for j from 0 to 3 list trace(My^(i+j))
D = (det W)//16
A = W || D*id_(S^4)
-- now do Hermitian row reduction on this matrix
Kx = QQ[x]
A = mutableMatrix sub(A,Kx)
rowMult(A,0,1/2)
rowMult(A,1,1/2)
rowMult(A,2,1/2)
rowMult(A,3,1/2)

-- One version
rowSwap(A,0,2)
rowAdd(A,2,-2,0)
rowAdd(A,4,-A_(4,0),0)
rowAdd(A,3,-A_(3,1),1)
rowAdd(A,5,-A_(5,1),1)
gcd(A_(2,2),A_(4,2),A_(6,2))
rowAdd(A,4,-A_(4,2)//A_(2,2),2)
rowAdd(A,6,-A_(6,2)//A_(2,2),2)
gcd(A_(3,3),A_(5,3),A_(7,3))
rowAdd(A,5,-A_(5,3)//A_(3,3),3)
rowAdd(A,7,-A_(7,3)//A_(3,3),3)

K = frac Kx
sub(sub((matrix A)^{0..3},Kx), K)
sub(D,K) * oo^-1

-- Another version, perhaps not as good
A = W || D*id_(S^4)
A = mutableMatrix sub(A,Kx)
rowMult(A,1,3)
rowMult(A,3,-2)
rowAdd(A,3,1,1)
rowSwap(A,3,7)
rowAdd(A,1,-A_(1,3),7)
rowAdd(A,3,-A_(3,3),7)
rowSwap(A,0,6)
rowAdd(A,0,-A_(0,2),6)
rowAdd(A,2,-A_(2,2),6)
gcd(A_(1,1),A_(3,1),A_(5,1))
rowMult(A,1,1/6)
rowSwap(A,1,5)
rowAdd(A,1,-A_(1,1)//A_(5,1),5)
rowAdd(A,3,-A_(3,1)//A_(5,1),5)
rowSwap(A,2,4)
rowAdd(A,0,-A_(0,0)//A_(4,0),4)
rowAdd(A,2,-A_(2,0)//A_(4,0),4)

K = frac Kx
sub(sub((matrix A)^{4..7},Kx), K)
sub(D,K) * oo^-1

trim radical ideal sub(D,R)
Hom(oo,oo)
prune oo
