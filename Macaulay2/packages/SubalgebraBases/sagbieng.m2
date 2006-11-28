-- Copyright 1997 by Mike Stillman and Harry Tsai

---------------------------------
-- Inhomogeneous SAGBI bases ----
---------------------------------
sagbiEngine = (Gens, maxnloops, printlevel) -> (
     --local R, G, S, RS, RStoS, Gmap, inGmap, J;
     --local d, maxdeg, nloops, Pending;
     R = ring Gens;
     MOflag := setMonomialOrderFlag R;
     if MOflag == 3 then (
	  << "" << endl;
	  << "******************************************************" << endl;
	  << "** WARNING: ENGINE SUBDUCT NOT STABLE W/ ELIM ORDER **" << endl;
	  << "******************************************************" << endl;
	  << "" << endl;
	  );	  
     maxdeg := maxnloops;
     Pending = new MutableList from toList(maxdeg+1:{});
     insertPending := (m) -> (
	  -- append the entries of the one row matrix 'm' to Pending.
	  i := 0;
	  lodeg := (degree m_(0,0))_0;
	  while i < numgens source m do (
	      f := m_(0,i);
	      e := (degree f)_0;
	      if e < lodeg then lodeg = e;
	      Pending#e = append(Pending#e, f);
	      i = i+1;
	      ));
     lowestDegree := () -> (
	  -- returns maxdeg+1 if Pending list is empty, otherwise
	  -- returns the smallest non-empty strictly positive degree.
	  i := 0;
	  while i <= maxdeg and Pending#i === {} do i=i+1;
	  i);
     appendToBasis := (m) -> (
	  R := ring m;
	  M := monoid R;
	  G = G | m;
	  nR = numgens R;
	  nG = numgens source G;
     	  if MOflag == 5 then (
	       RS = (coefficientRing R)[symbol b_1.. symbol b_nG, 
	       	    symbol a_1.. symbol a_nR,
	       	    Degrees=>join(degrees source G, degrees source vars R),
	       	    MonomialOrder => RevLex];
	       RtoRS = map(RS,R,(vars RS)_{nG..nG+nR-1});
	       J = ideal((vars RS)_{0..nG-1} - RtoRS(leadTerm G));
	       Gmap = map(RS,RS, RtoRS(G) | (vars RS)_{nG..nG+nR-1});
     	       RStoR = map(R,RS,matrix {toList(nG:0_R)} | vars R);
	       RStoS = map(RS,RS, (vars RS)_{0..nG-1} |
		    matrix {toList(nR:0_RS)});)
   	  else (
	       if MOflag == 0 or MOflag == 3 then (NewOrder = Eliminate nR)
	       else if MOflag == 4 then (
		    NewOrder = append(M.Options.MonomialOrder,nG))
	       else (NewOrder = M.Options.MonomialOrder);
	       RS = (coefficientRing R)[symbol a_1.. symbol a_nR, 
	       	    symbol b_1.. symbol b_nG,
	       	    Degrees=>join(degrees source vars R, degrees source G),
	       	    MonomialOrder => NewOrder];
	       RtoRS = map(RS,R,(vars RS)_{0..nR-1});
	       RStoS = map(RS,RS, matrix {toList(nR:0_RS)} |
		    (vars RS)_{nR .. nR+nG-1});
	       J = ideal((vars RS)_{nR..nR+nG-1}-RtoRS(leadTerm G));
	       Gmap = map(R,RS, (vars R)_{0..nR-1} | G);
	       Gmap = map(RS,RS,(vars RS)_{0..nR-1} | RtoRS(G));
	       RStoR = map(R,RS,(vars R) | matrix {toList(nG:0_R)});
	       );
	  );
     grabLowestDegree := () -> (
	  -- assumes: lowest degree pending list is already autosubducted.
	  -- this row reduces this list, placing all of the
	  -- entries back into Pending, but then appends the lowest
	  -- degree part into the basis.
	  e := lowestDegree();
	  if e <= maxdeg then (
	       trr = timing rowReduce(matrix{Pending#e}, e);
	       timerr = trr#0;
	       if printlevel > 0 then
	         << "    rowred  done in " << timerr << " seconds" << endl;
	       m = trr#1;
	       Pending#e = {};
	       insertPending m;
	       e = lowestDegree();
	       numnewsagbi = #Pending#e;
	       timeapp = (timing appendToBasis matrix{Pending#e})#0;
	       if printlevel > 0 then 
	         << "    append  done in " << timeapp << " seconds" << endl;
	       Pending#e = {};
	       );
	  e);
     
     G = matrix(R, {{}});
     Gensmaxdeg = (max degrees source Gens)_0;
     Gens = compress submatrixBelowDegree(Gens, maxdeg+1);
     insertPending Gens;
     Pending#0 = {};
     d = grabLowestDegree();  -- initializes G 
     proplist = {{d,0,0,timeapp,timeapp,0,0,numnewsagbi}};
     d = d+1;
     nloops := d;
     isdone := false;
     while nloops <= maxnloops and not isdone do (
       ttotal := timing(
	  nloops = nloops+1;
	  if printlevel > 0 then
	    << "--- degree " << d << " ----" << endl;
     	  tgbJ := timing gb(J, DegreeLimit=>d);
	  gbJ = tgbJ#1;
	  timegbJ = tgbJ#0;
	  if printlevel > 0 then 
	    << "    gb comp done in " << timegbJ << " seconds" << endl;
	  -- spairs = time mingens ideal selectInSubring(1, gens gbJ);
	  spairs = submatrixByDegrees(selectInSubring(1, gens gbJ), d);
	  tGmap = timing Gmap(spairs);
	  spairs = tGmap#1;
	  timeGmap = tGmap#0;
	  if printlevel > 0 then 
	    << "    Gmap    done in " << timeGmap << " seconds" << endl;
	  if Pending#d != {} then (
	       newgens = RtoRS(matrix{Pending#d});
	       spairs = spairs | newgens;
	       Pending#d = {};);
	  numspairs = numgens source spairs;
	  tsub = timing map(RS,rawSubduction(raw spairs, raw Gmap, raw gbJ));
	  timesub = tsub#0;
	  if printlevel > 0 then 
	    << "    subduct done in " << timesub << " seconds" << endl;
     	  tRS = timing compress RStoR(tsub#1);
	  timeRS = tRS#0;
	  if printlevel > 0 then
	    << "    RStoR   done in " << timeRS << " seconds" << endl;
	  newguys = tRS#1;
	  if numgens source newguys > 0 
	  then (
	       if printlevel > 0 then 
     	         << "    GENERATORS ADDED!" << endl;
	       insertPending newguys;
	       d = grabLowestDegree();
	       if printlevel > 0 then 	       
	         << "    " << numnewsagbi << " NEW GENERATORS!" << endl;
	       )
	  else (
	       timerr = 0; 
	       timeapp = 0;
	       numnewsagbi = 0;
	       ngens := sum apply(toList Pending,i -> #i);
	       if ngens === 0 and gbIsDone gbJ === 0 and d>Gensmaxdeg then (
	           isdone = true;
		   if printlevel > 0 then 
		     << "    SAGBI basis is FINITE!" << endl;
		   );
	      );
	      );
	 if printlevel > 0 then (
	   timetotal = ttotal#0;
	   << "    deg " << d << "  done in " << timetotal << " seconds" << endl;
	   timeleftover = timetotal - timegbJ - timesub - timeapp;
	   proplist = append (proplist, 
	      {d,timegbJ,timesub,timeapp,timetotal,timeleftover,
		   numspairs, numnewsagbi});
	   timeunaccount = timetotal - timegbJ - timeGmap - timesub - timeRS - 
	      timerr - timeapp;
	   << "    leftover time   " << timeunaccount << " seconds" << endl;
	   );
	 d=d+1;
	 );
     G)
