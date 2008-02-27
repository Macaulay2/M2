newPackage(
	"IntegralClosure",
    	Version => "0.5", 
    	Date => "October 31, 2006",
    	Authors => {
	     {Name => "Amelia Taylor",
	     HomePage => "http://faculty1.coloradocollege.edu/~ataylor/",
   	     Email => "amelia.taylor@coloradocollege.edu"}},
    	Headline => "Rees algebras",
    	DebuggingMode => false
    	)
   
   -- making a small change

export{isNormal, integralClosure, ICmap, ICfractions, conductor}

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

-- First we have several helper functions for the integralClosure 
-- function.  These functions are described below with the actual 
-- front end function for integralClosure much further down.

-- As the engine code changes, comparisons of the time for 
-- radical and radical0 must be checked.  16.5.01 radical0 
-- in the code is universally faster.  However, for the 
-- example in Wolmer's book where one takes the radical of 3
-- generators, radical computes it in about 4 or 5 sec and 
-- radical0 does not finish.
radical0 = (I) -> (
     I = ideal mingens ideal generators gb I;
     comps := minimalPrimes I;
     result := if #comps === 1 then comps#0
               else intersect toSequence comps;
     result)


-- The idea of ICnode is to keep track of where we are in the process
-- and the various maps and fractions we generate.  It is particularly 
-- important in the reduced-not-domain case since we have to divide the 
-- the ring into pieces and keep track of those we have normalized.

-- The idea in general is to use normal0 to find the 
-- Non-normal locus.  First looking at the singular locus.  If it 
-- is just variables (max'l ideal) then it matches the NNL and we 
-- use it.  If not then we use the Jacobian to compute the NNL.

-- Once we have the defining ideal for the NNL we use idealizer0
-- to start looking at Hom(J,J).  (Recall (1/f)(fJ:J)=Hom(J,J) where f 
-- is a NZD in J mod I.)  For the reduced not domain case we must check 
-- for a NZD and if I:f strictly contains I and f is in J but not I then 
-- we consider the two new rings R/I:f and R/I:(I:f), continuing the 
-- process with these.  In the domain case we could just pick the first 
-- generator of J that is not in I works.

-- ICnode is a mutable hash table that allows us to collect all of the 
-- necessary information throughout this process.  Most of the elements 
-- in the hash table are clear, however, C#"todo", C#"pending", C#"storing" and 
-- C#"answer" are the key pieces to working through the algorithm.  C#"todo" is
-- the ideal that we need to work on next before starting the algorithm.  Once
-- the algorithm is started this ideal, along with its NNL (once computed) are
-- stored in C#"pending" and C#"todo" = {}.  After we have passed through the 
-- algorithm once newI and newJ are put into C#"todo" and we begin again.  If 
-- a zero divisor is detected (in idealizer0) then the first ideal, I, in 
-- C#"pending" is split into I:f and I:I:f.  I:f is placed in C#"todo" so that 
-- the algorithm can begin again with that ideal and I:I:f is placed in 
-- C#"storing".  Once the integral closure of R_i/I_i for some I_i is obtained, 
-- then the defining ideal of theis integral closure is placed in C#"answer".

ICnode = new Type of MutableHashTable;
 
protect IIICCC

newICnode = (R) -> (
     I := ideal presentation R;
     C := new ICnode;
     C#"todo" = {{I,null}};
     C#"pending" = null;
     C#"storing" = {};
     C#"answer" = {};
     C#"degrees" = degrees source vars ring I;
     C#"deglong" = degrees source vars ring I;
     C#"blocks" = {numgens ring I};
     C#"basefield" = coefficientRing ring I;
     C#"vars" = toSequence (ring I).gens;
     C#"numgens" = 0;
     C#"newvars" = {};
     C#"fractions" = generators R;
     C#"fraclong" = {};
     C#"map" = null;
     C#"rings" = R;
     R#IIICCC = C;
     C)

-- Tells us when to stop the algorithm.  Moves ideals from C#"todo" to C#"pending".
next := (C) -> (
     if C#"pending" =!= null then true
     else if #C#"todo" > 0
     then (
	  C#"pending" = C#"todo"#0;
	  C#"todo" = drop(C#"todo",1);
	  true)
     else false)

idealizer0 = (C,w) -> (
     -- Takes {I,J} off the pending list of C,
     -- computes the ring sturcture of Hom_R(J,J).
     -- This is done using the IC structure, since we wish to be able to
     -- handle interrupts, and the creation of the ring is somewhat
     -- easier.  It also facilitates handling the non prime case.
     I := C#"pending"#0;
     J := C#"pending"#1;
     Jc := ideal compress (generators J % generators I);
     -- Find an element of J, a nzd in S/I.  Need to make sure we donot 
     -- choose an element in I, so first we reduce J mod I.
     J1 := I:Jc_0;
     if J1 != I then(
	  -- If Jc_0 is a ZD then we split the ring.
	  C#"todo" = append(C#"todo",{J1,null});
	  C#"pending" = null;
	  C#"storing" = append(C#"storing",I:J1);
	  )
     else(
	  -- If Jc_0 is a NZD then we continue setting f = Jc_0.
	  f := Jc_0;
     	  -- Compute Hom_R(J,J), with R = S/I.
     	  -- From now on, we work in this quotient:
     	  R := (ring I)/I;
     	  JR := ideal (generators J ** R);
     	  fR := substitute(f, R);
     	  idJ := mingens ideal generators gb (fR * JR : JR);
     	  if ideal(idJ) == ideal(fR)  then (
	       -- We have the answer for this ideal!
	       C#"answer" = append(C#"answer", {ring I,I});
	       if #C#"storing" > 0 then (
	       	    C#"todo" = {{C#"storing"#0,null}};
	       	    C#"storing" = drop(C#"storing", 1);
	       	    C#"pending" = null;
	       	    )
	       else(C#"pending" = null;)
	       )
     	  else (
     	       H := compress (idJ % fR);
	       C#"fractions" = join((apply(first entries H,i->i/fR)),C#"fractions");
	       C#"fraclong" = join((apply(first entries H,i->i/fR)),C#"fraclong");
     	       Ha := (fR // generators JR);  
	       -- MES: what is this Ha all about: the problem is that
	       -- although f is a minimal generator of (fJ:J) mod I,
	       -- it might not be given immediately as one of the elements.
     	       Hf := H | matrix{{fR}};
     	       -- Make the new polynomial ring.
	       n := numgens source H;
	       newdegs := degrees source H - toList(n:degree fR);
	       C#"degrees" = join(newdegs, C#"degrees");
	       C#"deglong" = join(newdegs, C#"deglong");
     	       C#"blocks" = prepend(n, C#"blocks");
	       C#"numgens" = C#"numgens" + n;   	    	      	   	     
      	       varsA := w_(C#"numgens" - n) .. w_(C#"numgens" -1);  
       	       C#"vars" = splice(varsA, C#"vars");       
     	       C#"blocks" = select(C#"blocks", d -> d =!= 0);
     	       A := (if any(C#"degrees", d -> d#0 <= 0) then (
       			 (C#"basefield")(monoid [C#"vars", 
	  		      MonomialOrder=>ProductOrder (C#"blocks"),
	  		      MonomialSize=>16]))
     		    else (
       			 (C#"basefield")(monoid [C#"vars",
	  		      Degrees => C#"degrees", 
	  		      MonomialOrder=>ProductOrder (C#"blocks"),
	  		      MonomialSize=>16]))
     		    );
	       newvars := (vars A)_{0..n-1};
	       C#"newvars" = join(entries newvars,C#"newvars");
     	       RtoA := map(A,R,(vars A)_{n..numgens R + n - 1});
	       IA := ideal ((map(A,ring I,RtoA.matrix)) (generators I));
	       XX := newvars | matrix{{1_A}};
     	       -- Linear relations in the new variables
     	       lins := XX * RtoA syz Hf; 
	       -- linear equations(in new variables) in the ideal
     	       -- Quadratic relations in the new variables
	       tails := (symmetricPower(2,H) // fR) // Hf;
	       tails = RtoA tails;
	       quads := matrix(A, entries (symmetricPower(2,newvars) - XX * tails));
	       newI1 := trim ideal matrix entries generators (
		    ideal lins + ideal quads + IA);
	       newJ1 := newI1 + RtoA JR;
	       newI := minimalPresentation(newI1);
	       R2 := ring newI;
	       FF := substitute(((newI1).cache.minimalPresentationMap).matrix,R2);
	       F := map(R2,A,FF);
	       newJ :=  F newJ1;
	       --Making the map from S to it's integral closure.
	       S1 := C#"rings";  
	       F1 := map(A,S1); 
	       if C#"map" === null then C#"map" = F * F1
	       else C#"map" = F * F1 * C#"map";
	       --Resetting the necessary values of the hash table.
	       indexvars := apply(first entries substitute(vars R2,A), index);
      	       C#"degrees" = apply(indexvars,i->(C#"degrees")#i);
	       C#"fractions" = apply(indexvars, i->(C#"fractions")#i);
	       C#"blocks" = {numgens R2};
	       C#"vars" = toSequence R2.gens;
	       -- See the note in "normal0" about the if char R = 0 statement.
	       newJ = radical0 newJ;
	       C#"todo" = append(C#"todo", {newI,newJ});
	       C#"pending" = null;
	       C#"rings" = R2;
	       )
     	  );
     )


normal0 = (C) -> (
     -- This handles the first node: finding an ideal that contains the NNL 
     -- locus.  
     I := C#"pending"#0;
     local J;
     SI := jacobian I;
     R := (ring I)/I;
     SIR := substitute(SI,R);
     if isHomogeneous I and #(first entries generators I)+#(generators ring I) <= 20 then (
	  SIdets := minors(codim I, SIR);
	   -- the codimension of the singular locus.
	  cs := codim SIdets + codim R;  -- codim of SIdets in poly ring. 
	  if cs === dim ring I or SIdets == 1
	  -- i.e. the sing locus is empty.
	  then (J = ideal vars ring I;)
	  else (J = radical0(lift(ideal SIdets_0,ring I)));
	  )           	       
     else (
	  n := 1;
	  det1 := ideal (0_R);
	  while det1 == ideal (0_R) do (
	       det1 = minors(codim I, SIR, Limit=>n);
	       n = n+1);
	     if det1 == 1
	     -- i.e. the sing locus is empty.
	     then (J = ideal vars ring I;)
	     else (J = radical0(lift(ideal det1_0,ring I)))
	     );	  
     C#"todo" = append(C#"todo", {I,J});
     C#"pending" = null;
     )

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
--                   indirectly, radical0
-- COMMENT: The quotient rings are not necessarily domains.  The algorithm can correctly 
--          proceed without decomposing a reduced ring if it finds a non-zero divisor with 
--          which to compute 1/f(fJ:J).
integralClosure = method(Options=>{Variable => global w})
integralClosure Ring := Ring => o -> (R) -> (
     if not R#?IIICCC then newICnode R;
     C := R#IIICCC;
     while next C do (
      	  if C#"pending"#1 === null 
     	  then normal0 (C) --Compute J defining the NNL.
     	  else idealizer0(C, o.Variable));
     A := apply(C#"answer",i-> i_0/ trim i_1);
     if #A == 1 then A#0
     else toSequence A
     )


--------------------------------------------------------------------
-- PURPOSE : The natural map from a ring to its integral closure.
-- This map is needed for the function conductor to compute the 
-- conductor of the integral closure into R.  This map may be of 
-- other independent interest.
-- R#IIICCC#"map" is needed to find the conductor

ICmap = method()
ICmap(Ring) := RingMap => (R) -> (
     -- Input:  a quotient ring.
     -- Output:  The natural map from R to its integral closure S.
     -- Note:  This is needed to compute the conductor of R into S.
     if R#?IIICCC or not isNormal R then (
	  if not R#?IIICCC then integralClosure(R);
	  S := (R#IIICCC#"answer"#0)#0/(R#IIICCC#"answer"#0)#1;
	  U := R#IIICCC#"map";
     	  if U === null then R#IIICCC#"map" = map(S,S)
     	  else R#IIICCC#"map" = map(S,R,substitute((U).matrix,S));
	       R#IIICCC#"map"
	       )
     else (map(R,R))
     )

--------------------------------------------------------------------

-- PURPOSE : In the case of a domain we obtain the fractions 
-- added to a ring in order to obtain its integral closure.  
-- They are printed to correspond directly to the new variables 
-- used in the output of the integralClosure function.
-- CAVEAT : R MUST be a domain.  
-- FUTURE: Implement this for rings that are reduced and not a domain.

ICfractions = method(Options => {Strategy => null})
ICfractions(Ring) := Matrix => o-> R -> (
     -- Input:  A quotient ring that is a domain.
     -- Output:  A matrix of the fractions added to R to get the integral closure.
     -- These correspond directly to the variables in the output of integralClosure 
     -- and the matrix is in the fraction field of the original ring.
     if R#?IIICCC or not isNormal R then (
	  integralClosure R;
	  K := (R#IIICCC#"basefield")(monoid [join(flatten R#IIICCC#"newvars",
			 (options R).Variables), Degrees => R#IIICCC#"deglong"]);
	  -- This constructs the new ring using all of the new variables.
	  KF := frac(K);
	  M1 := first entries substitute(vars R,KF);  -- puts the vars of R in KF
	  M2 := apply(R#IIICCC#"fraclong", i->matrix{{i}});
	  M2' := apply(M2, i->substitute(i,KF));
	  M3 := flatten apply(M2', j-> first entries j);
	  numnew := # generators K - # (options R).Variables;
	  if o.Strategy === null then (
	       L1 := apply(R#IIICCC#"fractions", i->matrix{{i}});
	       L2 := matrix{flatten apply(apply(L1, i->substitute(i,KF)), j-> first entries j)};
	       G := map(KF,KF,matrix{join(M3,M1)});
	       while not all(flatten entries L2, 
		    f -> all({numerator f, denominator f}, 
			 r -> all(support r, x -> index x >= numnew)))
	       do L2 = G L2;
	       substitute(L2,frac R))
	  else (
	       G2 := matrix{join(M3,M1)};
	       Map := map(KF,KF,G2);
	       while not all(flatten entries G2, 
		    f -> all({numerator f, denominator f}, 
			 r -> all(support r, x -> index x >= numnew)))
	       do G2 = Map G2;
	       substitute(G2,frac R)))
     else vars R)
     
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
     if isSinglyGraded (source F) and isHomogeneous (source F)
     	  then(M := presentation pushForward(F, (target F)^1);
     	       P := target M;
     	       intersect apply((numgens P)-1, i->(
	       m:=matrix{P_(i+1)};
	       I:=ideal modulo(m,matrix{P_0}|M))))
	  else error "conductor: expected a homogeneous ideal in a singly graded ring"
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
     a matrix using the function ", TO (ICfractions,Ring), " where R is 
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

document {
     Key => {ICmap, (ICmap,Ring)},
     Headline => "natural map from an affine domain into its integral closure.",
     Usage => "ICmap R",
     Inputs => {
	  "R" => {ofClass Ring, " that is an affine domain"}
	  },
     Outputs => {
	  {ofClass RingMap, " from ", TT "R", " to its integral closure"}
	  },
     "Note that if an integrally closed ring is given as input, the identity map from 
     the ring to itself is returned.",
     	  EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
      	  "ICmap R"},
     PARA{},
     "This finite map is needed to compute the ", TO "conductor", " of the integral closure 
     into the original ring.",
     SeeAlso => {"conductor"}
     }
    

document {
     Key => {ICfractions, (ICfractions,Ring)},
     Headline => "Compute the fractions integral over a domain.",
     Usage => "ICfractions R",
     Inputs => {
	  "R" => {ofClass Ring, " that is an affine domain"},
	  },
     Outputs => {
	  {ofClass Matrix, " whose entries are fractions that generate the integral 
	       closure of ", TT "R", " over R."}
	  },
     EXAMPLE {
	  "R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4);",
      	  "ICfractions R",
	  "integralClosure(R,Variable => a)"
	  },
     "Thus the new variables ", TT "w_7", " and ", TT "w_6", " correspond to the 
     fractions respectively.  The program currently also returns the original 
     variables as part of the matrix.  In this way the user can see if any are 
     simplified out of the ring during the process of computing the integral
     closure.",
     PARA{},
     "The fractions returned correspond to the variables returned by the function 
     integralClosure.  The function integralClosure eliminates redundant fractions 
     during its iteration.  If the user would like to see all fractions generated 
     during the computation, use the optional argument ", TT "Strategy => Long", " as 
     illustrated here.",
     EXAMPLE {
	  "ICfractions(R, Strategy => Long)"
	  },
     }

document {
     Key => [ICfractions,Strategy],
     Headline=> "Allows the user to obtain all of the fractions considered in the 
     process of building the integral closure",
     }

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
	  "F = ICmap R",
	  "conductor F"
	  },
     PARA{},
     "The command ", TT "conductor", " calls the 
     command ", TO pushForward, ".  Currently, the 
     command ", TT "pushForward", 
     " does not work if the source of the map ", TT "F", " is multgraded 
     or inhomogeneous.  If the source of the map ", TT "F", " is multigraded 
     or in homogeneous ", TT "conductor", " returns the message -- No conductor
     for ", TT "F", ".",
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
assert(ICfractions R == substitute(matrix {{42 * y*z/x^2, x, y, z}},frac R))
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
assert(#V == 3)
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
assert(conductor(ICmap Q) == ideal(z^3,x*z^2,x^3*z,x^4))
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
TEST ///
S = QQ [(symbol Y)_1, (symbol Y)_2, (symbol Y)_3, (symbol Y)_4, symbol x, symbol y, Degrees => {{7, 1}, {5, 1}, {6, 1}, {6, 1}, {1, 0}, {1, 0}}, MonomialOrder => ProductOrder {4, 2}]
J = ideal(Y_3*y-Y_2*x^2,Y_3*x-Y_4*y,Y_1*x^3-Y_2*y^5,Y_3^2-Y_2*Y_4*x,Y_1*Y_4-Y_2^2*y^3)
T = S/J       
assert(ICfractions T == substitute(matrix {{(Y_2*y^2)/x, (Y_1*x)/y, Y_1, Y_2, Y_3, Y_4, x, y}}, frac T))
///

-- Test of isNormal
TEST ///
S = ZZ/101[x,y,z]/ideal(x^2-y, x*y-z^2)
assert(isNormal(S) == false)
assert(isNormal(integralClosure(S)) == true)
///

-- Test of ICmap and conductor
TEST ///
R = QQ[x,y,z]/ideal(x^6-z^6-y^2*z^4)
F = ICmap R
assert(conductor F == ideal((R_2)^3, (R_0)*(R_2)^2, (R_0)^3*(R_2), (R_0)^4))
///

end
installPackage "IntegralClosure"
