-- This file written by Amelia Taylor <ataylor@math.rutgers.edu>

-- Computes the integral closure of a reduced ring (R/I where I is radical).  
-- integralClosure takes R/I as input and outputs a sequence of ideals, 
-- I_1,..., I_n such that the integral closure of R/I is the direct product 
-- of R_1/I_1,..., R_n/I_n.

-- Included is a function for checking if an affine domain is normal.  
-- isNormal works for any ring given as a quotient ring.  
-- The fractions that generate the integral closure over R/I are obtained 
-- by the command ICfractions(R/I).  
-- Included is a command conductor that computes the conductor of S into R
-- where S is the image of a ring map from R to S where S is finite over
-- R.  ICmap constructs this map (the natural map R/I->R_j/I_j) for R 
-- into its integral closure and applying conductor to this map 
-- yeilds the conductor of the integral closure into R.

isNormal = method()     
isNormal(Ring) := Boolean => (R) -> (
     --Input:  Takes a quotient ring. 
     --Method:  Check if the Jacobian ideal of R has
     --codim >= 2, if true then check the codimension 
     --of Ext^i(S/I,S) where R = S/I and i>codim I. If 
     --the codimensions are >= i+2 then return true.
     I := ideal (R);
     M := coker gens I;
     n := codim I;
     m := dim ring I;    -- 11 June 01 -- Write this so that it checks
     m2:= dim R;         -- S2 first and prints "is S2".  Then checks
     Jac := minors(n,jacobian R);  -- R1 this way the user gets more info.
     S2 := apply (m-n-1, i-> codim Ext^(i+n+1)(M,ring M));
     check := apply(m-n-1,i-> i+n+3);
     if m2-dim Jac >= 2  then ( if S2 >= check then true else false)
	  else (false)
	  )

-- As the engine code changes, comparisons of the time for 
-- radical and radical0 must be checked.  16.5.01 radical0 
-- in the code is universally faster.  However, for the 
-- example in Wolmer's book where one takes the radical of 3
-- generators, radical computes it in about 4 or 5 sec and 
-- radical0 does not finish.
radical0 := (I) -> (
     I = ideal mingens ideal gens gb I;
     comps := decompose I;
     result := if #comps === 1 then comps#0
               else intersect toSequence comps;
     result)

-- 16.5.01 This has now been fixed in the system!
-- Needed because we have problems with multigraded over char 0 and 
-- have to kill the multigrading in this case at the very beginning.
isSinglyGraded := (R) -> (
     n := numgens (ring presentation R).degreesRing;
     if n===1 then true else false)


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
-- in the hash table are clear, however, C.todo, C.pending, C.storing and 
-- C.answer are the key pieces to working through the algorithm.  C.todo is
-- the ideal that we need to work on next before starting the algorithm.  Once
-- the algorithm is started this ideal, along with its NNL (once computed) are
-- stored in C.pending and C.todo = {}.  After we have passed through the 
-- algorithm once newI and newJ are put into C.todo and we begin again.  If 
-- a zero divisor is detected (in idealizer0) then the first ideal, I, in 
-- C.pending is split into I:f and I:I:f.  I:f is placed in C.todo so that 
-- the algorithm can begin again with that ideal and I:I:f is placed in 
-- C.storing.  Once the integral closure of R_i/I_i for some I_i is obtained, 
-- then the defining ideal of theis integral closure is placed in C.answer.
if ICnode === symbol ICnode then
    ICnode = new Type of MutableHashTable;

newICnode := (R) -> (
     I := ideal presentation R;
     C := new ICnode;
     C.todo = {{I,null}};
     C.pending = null;
     C.storing = {};
     C.answer = {};
     C.degrees = degrees source vars ring I;
     C.blocks = {numgens ring I};
     C.basefield = coefficientRing ring I;
     C.vars = toSequence (ring I).generatorSymbols;
     C.numgens = 0;
     C.newvars = {};
     C.fractions = gens R;
     C.fraclong = {};
     C.map = null;
     C.rings = R;
     R.IC = C;
     C)

-- Tells us when to stop the algorithm.  Moves ideals from C.todo to C.pending.
next := (C) -> (
     if C.pending =!= null then true
     else if #C.todo > 0
     then (
	  C.pending = C.todo#0;
	  C.todo = drop(C.todo,1);
	  true)
     else false)

idealizer0 := (C,w) -> (
     -- Takes {I,J} off the pending list of C,
     -- computes the ring sturcture of Hom_R(J,J).
     -- This is done using the IC structure, since we wish to be able to
     -- handle interrupts, and the creation of the ring is somewhat
     -- easier.  It also facilitates handling the non prime case.
     I := C.pending#0;
     J := C.pending#1;
     Jc := ideal compress (gens J % gens I);
     -- Find an element of J, a nzd in S/I.  Need to make sure we donot 
     -- choose an element in I, so first we reduce J mod I.
     J1 := I:Jc_0;
     if J1 != I then(
	  -- If Jc_0 is a ZD then we split the ring.
	  C.todo = append(C.todo,{J1,null});
	  C.pending = null;
	  C.storing = append(C.storing,I:J1);
	  )
     else(
	  -- If Jc_0 is a NZD then we continue setting f = Jc_0.
	  f := Jc_0;
     	  -- Compute Hom_R(J,J), with R = S/I.
     	  -- From now on, we work in this quotient:
     	  R := (ring I)/I;
     	  JR := ideal (gens J ** R);
     	  fR := substitute(f, R);
     	  idJ := mingens ideal gens gb (fR * JR : JR);
     	  if ideal(idJ) == ideal(fR)  then (
	       -- We have the answer for this ideal!
	       C.answer = append(C.answer, {ring I,I});
	       if #C.storing > 0 then (
	       	    C.todo = {{C.storing#0,null}};
	       	    C.storing = drop(C.storing, 1);
	       	    C.pending = null;
	       	    )
	       else(C.pending = null;)
	       )
     	  else (
     	       H := compress (idJ % fR);
	       C.fractions = join((apply(first entries H,i->i/fR)),C.fractions);
	       C.fraclong = join((apply(first entries H,i->i/fR)),C.fraclong);
     	       Ha := (fR // gens JR);  
	       -- MES: what is this Ha all about: the problem is that
	       -- although f is a minimal generator of (fJ:J) mod I,
	       -- it might not be given immediately as one of the elements.
     	       Hf := H | matrix{{fR}};
     	       -- Make the new polynomial ring.
	       n := numgens source H;
	       newdegs := degrees source H - toList(n:degree fR);
	       C.degrees = join(newdegs, C.degrees);
     	       C.blocks = prepend(n, C.blocks);
	       C.numgens = C.numgens + n;   	    	      	   	     
      	       varsA := w_(C.numgens - n) .. w_(C.numgens -1);  
       	       C.vars = splice(varsA, C.vars);       
     	       C.blocks = select(C.blocks, d -> d =!= 0);
     	       A := (if any(C.degrees, d -> d#0 <= 0) then (
       			 (C.basefield)[C.vars, 
	  		      MonomialOrder=>ProductOrder (C.blocks),
	  		      MonomialSize=>16])
     		    else (
       			 (C.basefield)[C.vars,
	  		      Degrees => C.degrees, 
	  		      MonomialOrder=>ProductOrder (C.blocks),
	  		      MonomialSize=>16])
     		    );
	       newvars := (vars A)_{0..n-1};
	       C.newvars = join(entries newvars,C.newvars);
     	       RtoA := map(A,R,(vars A)_{n..numgens R + n - 1});
	       IA := ideal ((map(A,ring I,RtoA.matrix)) (gens I));
	       XX := newvars | matrix{{1_A}};
     	       -- Linear relations in the new variables
     	       lins := XX * RtoA syz Hf; 
	       -- linear equations(in new variables) in the ideal
     	       -- Quadratic relations in the new variables
	       tails := (symmetricPower(2,H) // fR) // Hf;
	       tails = RtoA tails;
	       quads := matrix(A, entries (symmetricPower(2,newvars) - XX * tails));
	       newI1 := trim ideal matrix entries gens (
		    ideal lins + ideal quads + IA);
	       newJ1 := newI1 + RtoA JR;
	       newI := minPresIdeal(newI1);
	       R2 := ring newI;
	       FF := substitute(((newI1).cache.minPresMap).matrix,R2);
	       F := map(R2,A,FF);
	       newJ :=  F newJ1;
	       --Making the map from S to it's integral closure.
	       S1 := C.rings;  
	       --AT8/23: S1 needs to be global.  Appears to be a bug. 
	       --AT9/13: Not global now - ok?
	       F1 := map(A,S1); 
	       if C.map === null then C.map = F * F1
	       else C.map = F * F1 * C.map;
	       --Resetting the necessary values of the hash table.
	       indexvars := apply(first entries substitute(vars R2,A), index);
      	       C.degrees = apply(indexvars,i->(C.degrees)#i);
	       C.fractions = apply(indexvars, i->(C.fractions)#i);
	       C.blocks = {numgens R2};
	       C.vars = toSequence R2.generatorSymbols;
	       -- See the note in "normal0" about the if char R = 0 statement.
	       newJ = radical0 newJ;
	       C.todo = append(C.todo, {newI,newJ});
	       C.pending = null;
	       C.rings = R2;
	       )
     	  )
     )


normal0 := (C) -> (
     -- This handles the first node: finding an ideal that contains the NNL 
     -- locus.  
     I := C.pending#0;
     local J;
     SI := jacobian I;
     R := (ring I)/I;
     SIR := substitute(SI,R);
     if isHomogeneous I and #(first entries gens I)+#(gens ring I) <= 20 then (
	  SIdets := minors(codim I, SIR);
	   -- the codimension of the singular locus.
	  cs := codim SIdets + codim R;  -- codim of SIdets in poly ring. 
	  if cs === dim ring I or SIdets == ideal (1_R)
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
	     if det1 == ideal (1_R)
	     -- i.e. the sing locus is empty.
	     then (J = ideal vars ring I;)
	     else (J = radical0(lift(ideal det1_0,ring I)))
	     );	  
     C.todo = append(C.todo, {I,J});
     C.pending = null;
     )

integralClosure = method(Options=>{Variable => symbol w})
integralClosure Ring := Ring => o -> (R) -> (
     if not R.?IC then newICnode R;
     C := R.IC;
     while next C do (
      	  if C.pending#1 === null 
     	  then normal0 (C) --Compute J defining the NNL.
     	  else idealizer0(C,o.Variable));
     A := apply(C.answer,i->i_0/i_1);
     if #A == 1 then A#0
     else toSequence A
     )


--------------------------------------------------------------------

-- R.IC.map is needed to find the conductor
ICmap = method()
ICmap(Ring) := RingMap => (R) -> (
     -- Input:  a quotient ring.
     -- Output:  The natural map from R to its integral closure S.
     -- Note:  This is needed to compute the conductor of R into S.
     if R.?IC or not isNormal R then (
	  if not R.?IC then integralClosure(R);
	  S := (R.IC.answer#0)#0/(R.IC.answer#0)#1;
	  U := R.IC.map;
     	  if U === null then R.IC.map = map(S,S)
     	  else R.IC.map = map(S,R,substitute((U).matrix,S));
	       R.IC.map
	       )
     else (map(R,R))
     )

--------------------------------------------------------------------
ICfractions = method()
ICfractions(Ring) := RingMap => (R) -> (
     -- Input:  A quotient ring that is a domain.
     -- Output:  A matrix of the fractions added to R to get the integral closure.
     -- These correspond directly to the variables in the output of integralClosure 
     -- and the matrix is in the fraction field of the original ring.
     --
     -- I haven't figured out how to do the fractions and the maps
     -- for reduced rings yet.  #C.answer == 1 if and only if a 
     -- domain was the input into the function.  
     if R.?IC or not isNormal R then (
	  integralClosure R;
	  K := (R.IC.basefield)[join(flatten R.IC.newvars,R.generatorSymbols)];
	  K2 := (R.IC.basefield)[toList R.IC.vars];
	  -- This constructs the new ring using all of the new variables.
	  KF := frac(K);
	  KF2 := frac K2;  
	  M1 := first entries substitute(vars R,KF);  -- puts the vars of R in KF
	  M2 := apply(R.IC.fraclong, i->matrix{{i}});
	  M2' := apply(M2, i->substitute(i,KF));
	  M3 := flatten apply(M2', j-> first entries j);
	  L1 := apply(R.IC.fractions, i->matrix{{i}});
	  L2 := matrix{flatten apply(apply(L1, i->substitute(i,KF)), j-> first entries j)};
	  G := map(KF,KF,matrix{join(M3,M1)});
	  done := false;
	  while done == false do (
	       L2 = G(L2);
	       done = isSubset(ideal apply(first entries L2,i->numerator i), ideal take(gens K , {#(gens K)-#(R.generatorSymbols),#(gens K)}));
	       );
	  K3 := frac R;
	  substitute(L2,K3)
	  )
     else (
	  I := ideal(R);
	  vars R)
	  )

------------------
ICfractionsLong = method()
ICfractionsLong(Ring) := RingMap => (R) -> (
     -- Input:  A quotient ring which is a domain.
     -- Output:  A matrix of the fractions generated by integralClosure.
     -- The actual output of integralClosure does not add all off these as they
     -- are extraneous.  
     --
     -- I haven't figured out how to do the fractions and the maps
     -- for reduced rings yet.  #C.answer == 1 if and only if a 
     -- domain was the input into the function.  
     if R.?IC or not isNormal R then (
	  integralClosure(R);
	  K := (R.IC.basefield)[join(flatten R.IC.newvars,R.generatorSymbols)];
	  -- This constructs the new ring using all of the new variables.
	  KF := frac(K);  
     	  M1 := first entries substitute(vars R,KF);  -- puts the vars of R in KF
	  M2 := apply(R.IC.fraclong, i->matrix{{i}});
     	  M2' := apply(M2, i->substitute(i,KF));
     	  M3 := flatten apply(M2', j-> first entries j);
	  G2 := matrix{join(M3,M1)};
     	  Map := map(KF,KF,G2);
     	  done := false;
	  while done == false do (
	       G2 = Map(G2);
	       done = isSubset(ideal apply(first entries G2,i->numerator i), ideal take(gens K , {#(gens K)-#(R.generatorSymbols),#(gens K)}));
	       );
	  K2 := frac R;
	  substitute(G2,K2))
     else (
	  I := ideal(R);
	  map(frac(ring I),frac(ring I)))
     --if M3 === {} then R.IC.fractions = {G,G.matrix}
     --else R.IC.fractions = {G,transpose G (matrix{M3})};
     --R.IC.fractions
     )

--------------------------------------------------------------------
conductor = method()
conductor(RingMap) := Ideal => (F) -> (
     --Input:  A ring map where the target is finitely generated as a 
     --module over the source.
     --Output: The conductor of the target into the source.
     --NOTE:  If using this in conjunction with the command normalization,
     --then the input is R.IC.map where R is the name of the ring used as 
     --input into normalization.  
     if isSinglyGraded (source F) and isHomogeneous (source F)
     	  then(M := presentation pushForward(F, (target F)^1);
     	       P := target M;
     	       intersect apply((numgens P)-1, i->(
	       m:=matrix{P_(i+1)};
	       I:=ideal modulo(m,matrix{P_0}|M))))
	  else (<< " --No conductor for " << F << endl;)
     )

erase global w
