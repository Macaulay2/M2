-- Notes for Amelia, from Mike:
-- I have changed ICnode to ICComputation
-- and have used ICComputation instead of IC

-- Computes the integral closure of a reduced ring (R/I where I is radical).  
-- integralClosure takes R/I as input and outputs a sequence of ideals, 
-- I_1,..., I_n such that the integral closure of R/I is the direct product 
-- of R_1/I_1,..., R_n/I_n.

-- Included is a function for checking if an affine domain is normal.  
-- isNormal works for any ring given as a quotient ring.  
-- The fractions that generate the integral closure over R/I are obtained 
-- by the command ICfractions(R/I).  
-- In a separate package is a command to compute the conductor of the 
-- integral closure into R/I.
-- Last, there are natural maps R/I->R_j/I_j and these can be obtained 
-- using the command ICmaps(R/I)
-- Problems:  The code does not work for multigraded rings over QQ without the 
--     	      the fixes that are included.  The reasons are that  
--            1) radical won't take a multigraded ring.  
-- 	         (For finite fields we use radical0 which allows 
--     	    	 multigraded.  However, radical0 uses decompose which 
--     	    	 does not work over QQ).


isNormal = method()     
isNormal(Ring) := (R) -> (
     --Input:  Takes a quotient ring. 
     --Method:  Check if the Jacobian ideal of R has
     --codim >= 2, if true then check the codimension 
     --of Ext^i(S/I,S) where R = S/I and i>codim I. If 
     --the codimensions are >= i+2 then return true.
     I := ideal (R);
     M := coker gens I;
     n := codim I;
     m := dim ring I;
     m2:= dim R;
     Jac := minors(n,jacobian R);
     S2 := apply (m-n-1, i-> codim Ext^(i+n+1)(M,ring M));
     check := apply(m-n-1,i-> i+n+3);
     if m2-dim Jac >= 2  then ( if S2 >= check then true else false)
	  else (false)
	  )

-- For rings of char p>0 where we can use the command comps, radical9 is 
-- faster than radical.  As the engine code changes, this comparison should 
-- be rechecked.
radical0 := (I) -> (
     I = ideal mingens ideal gens gb I;
     comps := decompose I;
     result := if #comps === 1 then comps#0
               else intersect toSequence comps;
     result)

-- Needed because we have problems with multigraded over char 0 and 
-- have to kill the multigrading in this case at the very beginning.
isSinglyGraded := (R) -> (
     n := numgens (ring presentation R).degreesRing;
     if n===1 then true else false)


-- The idea of ICComputation is to keep track of where we are in the process
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


-- ICComputation is a mutable hash table that allows us to collect all of the 
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
if ICComputation === symbol ICComputation then
    ICComputation = new Type of MutableHashTable;

newICComputation := (R) -> (
     I := ideal presentation R;
     C := new ICComputation;
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
     C.fractions = {}; 
     C.map = null;
     C.rings = R;
     R.ICComputation = C;
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
	  f:=Jc_0;
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
	       C.fractions = join(C.fractions,(apply(first entries H,i->i/fR)));
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
	       C.newvars = join(C.newvars,entries newvars);
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
	       C.blocks = {numgens R2};
	       C.vars = toSequence R2.generatorSymbols;
	       -- See the note in "normal0" about the if char R = 0 statement.  
	       -- The key is the dependence of radical0 on decompose.  radical0
	       -- is faster than radical. 
	       if char R2 === 0 then (newJ = radical newJ)
	       else (newJ = radical0 newJ);
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
     if isHomogeneous I and isSinglyGraded R then (
	  SIdets := minors(codim I, SIR);
	   -- the codimension of the singular locus.
	  cs := codim SIdets + codim R;  -- codim of SIdets in poly ring. 
	  if cs === dim ring I or SIdets == ideal (1_R)
	  -- i.e. the sing locus is empty.
	  then (J = ideal vars ring I;)
	  else (
	       if char R === 0 then (J = radical(lift(ideal SIdets_0,ring I)))
	       else (J = radical0(lift(ideal SIdets_0,ring I)));
      	       )
	  )
     else (
	  n := 1;
	  det1 := ideal (0_R);
	  while det1 == ideal (0_R) do (
	       det1 = minors(codim I, SIR, Limit=>n);
	       n = n+1);
	  -- This if statement is required because for now, decompose only
	  -- works over char R>0. 12/15/00 decompose is written over char R =0
	  -- but not in distribution yet.  When available then need to test the
	  -- time of radical0 vs radical over char R = 0.
	       if char R === 0 then (J = radical(lift(ideal det1_0,ring I)))
	       else (J = radical0(lift(ideal det1_0,ring I)));
	  );	  
     C.todo = append(C.todo, {I,J});
     C.pending = null;
     )

integralClosure = method(Options=>{VarName => symbol w})
integralClosure Ring := o -> (R) -> (
     if not R.?ICComputation then newICComputation R;
     C := R.ICComputation;
     while next C do (
      	  if C.pending#1 === null 
     	  then normal0 C       --Compute J defining the NNL.
     	  else idealizer0(C,o.VarName));
     A := apply(C.answer,i->i_0/i_1);
     if #A == 1 then A#0
     else toSequence A
     )


--------------------------------------------------------------------

-- R.ICComputation.map is needed to find the conductor
ICmap = method()
ICmap(Ring) := (R) -> (
     if isNormal R then (map(R,R))
     else (
	  if not R.?ICComputation then V := integralClosure(R);
	  S := (R.ICComputation.answer#0)#0/(R.ICComputation.answer#0)#1;
	  U := R.ICComputation.map;
     	  if U === null then R.ICComputation.map = map(S,S)
     	  else R.ICComputation.map = map(S,R,substitute((U).matrix,S));
	       R.ICComputation.map))

--------------------------------------------------------------------

ICfractions = method()
ICfractions(Ring) := (R) -> (
     --I haven't figured out how to do the fractions and the maps
     --for reduced rings yet.  #C.answer == 1 if and only if a 
     --domain was the input into the function.  
     if isNormal R then (
	  I := ideal(R);
	  map(frac(ring I),frac(ring I)))
     else (
	  if not R.?ICComputation then V := integralClosure(R);
	  K := (R.ICComputation.basefield)[
	          join(flatten R.ICComputation.newvars,R.generatorSymbols)];
	  KF := frac(K);
     	  M1 := first entries substitute(vars R,KF);
	  M2 := apply(R.ICComputation.fractions, i->matrix{{i}});
     	  M2' := apply(M2, i->substitute(i,KF));
     	  M3 := flatten apply(M2', j-> first entries j);
     	  G := map(KF,KF,matrix{join(M3,M1)});
	  if M3 === {} then R.ICComputation.fractions = {G,G.matrix}
	  else R.ICComputation.fractions = {G,transpose G (matrix{M3})};
	  R.ICComputation.fractions))

--------------------------------------------------------------------
ICfractionMap = method()
ICfractionMap(Ring) := (R) -> (
     --I haven't figured out how to do the fractions and the maps
     --for reduced rings yet.  #C.answer == 1 if and only if a 
     --domain was the input into the function.  
     if isNormal R then (
	  I := ideal(R);
	  map(frac(ring I),frac(ring I)))
     else (
	  if not R.?ICComputation then V := integralClosure(R);
	  n := #gens V - #gens R;
	--K := (R.ICComputation.basefield)[
	--        join(flatten R.ICComputation.newvars,R.generatorSymbols)];
	  KF := frac(V);
     	  M1 := first entries substitute(vars R,KF);
	  F := R.ICComputation.fractions_{0..n-1};
	  M2 := apply(F, i->matrix{{i}});
     	  M2' := apply(M2, i->substitute(i,KF));
     	  M3 := flatten apply(M2', j-> first entries j);
     	  map(KF,V,matrix{join(M3,M1)}) ))
	  --if M3 === {} then R.ICComputation.fractions = {G,G.matrix}
	  --else R.ICComputation.fractions = {G,transpose G (matrix{M3})};
	  --R.ICComputation.fractions))

--------------------------------------------------------------------
conductor = method()
conductor(RingMap) := (F) -> (
     --Input:  A ring map where the target is finitely generated as a 
     --module over the source.
     --Output: The conductor of the target into the source.
     --NOTE:  If using this in conjunction with the command normalization,
     --then the input is R.ICComputation.map where R is the name of the ring used as 
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
