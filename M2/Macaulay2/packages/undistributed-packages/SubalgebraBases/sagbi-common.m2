debug Core

gbDone = (G) -> (rawStatus1 raw G == 6)

gbIsDone = (m) -> (
     -- only checks whether a 'non-syzygy' GB has completed
     m#?{false,0} and 
     (m#{false,0}).returnCode === 0)

Matrix % Ideal := (f,I) -> (
     if isHomogeneous f and isHomogeneous I then (
	  m := max degrees source f;
	  g := gb(I,DegreeLimit=>m);
          f % g)
     else f % gb I)

setMonomialOrderFlag = (R) -> (
     tempflag := 0;
     temp := (monoid R).Options.MonomialOrder;
     if (class temp) === Nothing then (tempflag = 0)
     else if temp === GRevLex then (tempflag = 0)
     else if temp === Lex then (tempflag = 1)
     else if temp === GLex then (tempflag = 2)
     else if (class temp) === Eliminate then (tempflag = 3)
     else if (class temp) === ProductOrder then (tempflag = 4)
     else if temp === RevLex then (tempflag = 5);
     tempflag)

appendElimination = (monorder, nold, nnew) -> (
     -- returns a monomial order
     append(monorder, Weights=>nold:1)
     )

submatrixByDegrees (Matrix,ZZ) := (m,d) -> (
    want := positions(0..numgens source m - 1, 
	             i -> (degrees source m)_i === {d});
    m_want)

submatrixBelowDegree = (m,d) -> (
    want := positions(0..numgens source m - 1, 
	             i -> (degrees source m)_i < {d});
    m_want)

rowReduce = (elems, d) -> (
     -- elems is a one row matrix of polynomials, all of degree d.
     -- return a (one row) matrix whose elements are row reduced
     -- CAUTION: Only the monomial orders GRevLex, Eliminate, Lex, and RevLex
     --	     	 are supported by this routine.  The monomial orders
     --	    	 Lex and ProductOrder ARE NOT SUPPORTED.
     (RH, RtoRH, RHtoR, elemsH) := 4:null;
     R := ring elems;
     n := numgens R;
     M := monoid R;
     moFlag := setMonomialOrderFlag R;
     k := coefficientRing R;
     if moFlag == 5 then (
	  N := monoid [Variables=>n+1, MonomialOrder => RevLex, Degrees => prepend({1},M.Options.Degrees)];
	  RH = k N;
	  RtoRH = map(RH,R,(vars RH)_{1..n});
     	  RHtoR = map(R,RH,matrix{{1_R}} | vars R);
          elemsH = homogenize(RtoRH elems, RH_0);)
     else (
	  if moFlag == 2 then (
	       << "WARNING: GLex is an unstable order for rowReduce" << endl)
	  else if moFlag == 4 then (
	       N = monoid [Variables=>n+1, 
	       	    MonomialOrder => append(M.Options.MonomialOrder,1),
	       	    Degrees => append(M.Options.Degrees,{1})];
     	       RH = k)
	  else (
	       N = monoid [Variables=>n+1, 
	       	    MonomialOrder => M.Options.MonomialOrder,
	       	    Degrees => append(M.Options.Degrees,{1})];
     	       RH = k N);
     	  RtoRH = map(RH,R,(vars RH)_{0..n-1});
     	  RHtoR = map(R,RH,vars R | matrix{{1_R}});
     	  elemsH = homogenize(RtoRH elems, RH_n););
     RHtoR gens gb(elemsH, DegreeLimit=>d))

rowReduce0 = (elems, d) -> (
     << "rowReduce(" << d << "): " << transpose elems << endl;
     result := rowReduce0(elems,d);
     << "  result: " << transpose result << endl;
     result
     )

subalgebraBasis = method(Options => {
	  Strategy => null,
	  Limit => 100,
	  PrintLevel => 0})

subalgebraBasis Matrix := opts -> (M) -> (
     if opts.Strategy =!= null then
       sagbiEngine(M, opts.Limit, opts.PrintLevel)
     else
       sagbiToplevel(M, opts.Limit, opts.PrintLevel)     
     )
