-- Copyright 1997 by Mike Stillman and Harry Tsai

-- This file contains code to compute the sagbi basis
-- of a subalgebra of a polynomial ring.
-- Do 'help sagbi' for references.

split = (m1, m2) -> (
     -- 'split' returns a copy of the matrix 'm1', except that the 
     -- entries (i,j) where 'm2' is nonzero are set to 0.
     map(target m1, source m1, (i,j) -> 
	  if m2_(i,j) == 0
	  then m1_(i,j)
	  else 0))

splitRev = (m1, m2) -> (
     -- 'split' returns a copy of the matrix 'm1', except that the 
     -- entries (i,j) where 'm2' is nonzero are set to 0.
     map(target m1, source m1, (i,j) -> 
	  if m2_(i,j) == 0
	  then 0
	  else m1_(i,j)))

split2 = (m1, m2) -> (
     -- 'split2' returns a copy of the matrix 'm1', except that the 
     -- entries (i,j) where 'm2' is not equal to 'm1' are set to 0.
     map(target m1, source m1, (i,j) -> 
	  if m2_(i,j) == m1_(i,j)
	  then m1_(i,j)
	  else 0))

subductquo = (m, F, J, I, d) -> (
     -- m is the matrix whose entries we wish to reduce (subduct),
     -- these elements are all in the same degree d.
     -- F is the ringmap whose entries we may use to reduce
     -- J is the ideal (y_i - in(f_i)), which has a GB computed
     -- in degrees <= d.
     R := target F; -- also ring m
     nR := numgens R;
     RS := source F;
     nS := numgens RS - numgens R;
     -- S := (coefficientRing R)[z_1 .. z_nS];
     RStoS := map(RS,RS, matrix {toList(nR:0_RS)} |
	  (vars RS)_{nR .. nR+nS-1});
     RtoRS := map(RS, R, (vars RS)_{0..numgens R-1});
     gbJ := gb(J, DegreeLimit=>d);
     m = m % I;
     reduced := matrix{toList(numgens source m:0_R)};
     while m != 0 do (
	 m = matrix entries m;  -- to fix degrees
         errorterm1 := (RtoRS leadTerm m) % gbJ;
         errorterm2 := RStoS errorterm1;
	 mm := split(m, errorterm2);
	 h1 := leadTerm(mm);
	 reduced = reduced + h1;
	 h2 := F errorterm2;
	 m = m - h1 - (h2%I);
	 );
     reduced = compress reduced;
     reduced = matrix entries reduced; -- fix degrees
     reduced
     )

TEST ///
debug SubalgebraBases
S = QQ[a,b,c]
m = matrix{{a^2, a^2+a, b^2+a^2+1, a^2+b^2+3, a*b+a}}
rowReduce(m,2)
///

getSyzygies = method()
getSyzygies(Matrix, Ideal, Ideal) := (Gens, I, J) -> (
     R := ring Gens;
     RS := ring J;
     nR := numgens R;
     nRS := numgens RS;
     gb I;
     gbJ := gb J;
     mtemp := gens gbJ;
     RtoRS := map(RS,R, (vars RS)_{0..nR-1});
     RStoS := map(RS,RS, matrix{toList(nR:0_RS)} |
	  (vars RS)_{nR..nRS-1});
     Fmap := map(RS,RS, (vars RS)_{0..numgens R-1} | RtoRS Gens);
     -- spairs := mingens ideal split2(mtemp, RStoS mtemp);
     spairs := compress split2(mtemp, RStoS mtemp);
     temp := (Fmap spairs); -- %I
     reduced := matrix{toList(numgens source spairs:0_RS)};
     while temp != 0 do (
	 temp = matrix entries temp;  -- to fix degrees
         errorterm1 := (leadTerm temp) % gbJ;
         errorterm2 := RStoS errorterm1;
	 mm := split(temp, errorterm2);
	 if mm != 0 then (
	      << "Not a SAGBI basis" << endl;
	      temp = 0;
	      )
	 else (
	      spairs = spairs - errorterm2;
	      h2 := Fmap errorterm2;
	      temp = temp - (h2); -- h2%I
	      );
	 );
    spairs = matrix entries (compress spairs);
    if spairs == 0 then (spairs = matrix{{0_RS}});
    y := getSymbol "y";
    S := (coefficientRing R)[y_1 .. y_(nRS-nR)];
    PutS := map(S,RS, matrix{toList(nR:0_S)} | vars S);
    PutS spairs)

---------------------------------
-- Inhomogeneous SAGBI bases ----
---------------------------------

sagbiToplevel = (Gens, maxnloops, printlevel) -> (
     -- Gens is a 1 row matrix
     -- maxnloops is an integer
     -- printlevel is an integer
     R := ring Gens;
     I := ideal(0_R);
     sagbiquo(Gens,I,maxnloops,printlevel))
     
sagbiquo = method()
sagbiquo(Matrix,Ideal,ZZ,ZZ) := (Gens, I, maxnloops, printlevel) -> (
     (R, G, S, RS, RStoS, Gmap, inGmap, J) := 8:null;
     (d, maxdeg, nloops, Pending) := 4:null;
     (newgens, newguys) := 2:null;
     gbI := null;    
     Jquo := null;
     gbJquo := null;
     IinRS := null;
     RtoRS := null;
     RStoR := null;
     numnewsagbi := null;
     R = ring Gens;
     MOflag := setMonomialOrderFlag R;
     maxdeg = maxnloops;
     Pending = new MutableList from toList(maxdeg+1:{});
     autosubductionquo := (m) -> (
     	  -- m is the matrix whose entries we wish to reduce (subduct),
     	  -- these elements are all in the same degree d.
	  if I != 0 then m = m % IinRS;
     	  reduced := map(target m, source m, 0);
     	  while m != 0 do (
	       m = matrix entries m;  -- to fix degrees
	       errorterm1 := (leadTerm m) % gbJquo;
               errorterm2 := RStoS errorterm1;
	       mm := split(m, errorterm2);
	       h := leadTerm(mm);
	       reduced = reduced + h;
	       if I == 0 then (m = m - h - (Gmap errorterm2))
	       else (m = m - h - ((Gmap errorterm2)%IinRS));
	       );
     	  reduced = compress reduced;
     	  reduced = matrix entries reduced; -- fix degrees
     	  reduced
     	  );
     insertPending := (m) -> (
	  -- append the entries of the one row matrix 'm' to Pending.
	  i := 0;
	  while i < numgens source m do (
	      f := m_(0,i);
	      e := (degree f)_0;
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
	  nR := numgens R;
	  nG := numgens source G;
     	  if MOflag == 5 then (
	       RS = (coefficientRing R)[Variables=>nG+nR,--b_1..b_nG,a_1..a_nR,
	       	    Degrees=>join(degrees source G, degrees source vars R),
	       	    MonomialOrder => RevLex];
	       RtoRS = map(RS,R,(vars RS)_{nG..nG+nR-1});
	       Jquo = ideal(((vars RS)_{0..nG-1} - RtoRS(leadTerm G)) | 
	       	    RtoRS(leadTerm gbI));
	       Gmap = map(RS,RS, RtoRS(G) | (vars RS)_{nG..nG+nR-1});
	       IinRS = RtoRS I;
     	       RStoR = map(R,RS,matrix {toList(nG:0_R)} | vars R);
	       RStoS = map(RS,RS, (vars RS)_{0..nG-1} |
		    matrix {toList(nR:0_RS)});)
   	  else (
	       newOrder := if MOflag == 0 or MOflag == 3 then Eliminate nR
	       else if MOflag == 4 then append(M.Options.MonomialOrder,nG)
	       else M.Options.MonomialOrder;
	       RS = (coefficientRing R)[Variables=>nG+nR,--a_1..a_nR,b_1..b_nG,
	       	    Degrees=>join(degrees source vars R, degrees source G),
	       	    MonomialOrder => newOrder];
	       RtoRS = map(RS,R,(vars RS)_{0..nR-1});
	       RStoS = map(RS,RS, matrix {toList(nR:0_RS)} |
		    (vars RS)_{nR .. nR+nG-1});
	       Jquo = ideal(((vars RS)_{nR..nR+nG-1}-RtoRS(leadTerm G)) | 
	 		 RtoRS(leadTerm gbI));
     	       if MOflag == 3 then (
	       	    Gmap = map(R,RS, (vars R)_{0..nR-1} | G))
	       else (
	       	    Gmap = map(RS,RS,(vars RS)_{0..nR-1} | RtoRS(G));
	       	    IinRS = RtoRS I;
     	       	    RStoR = map(R,RS,(vars R) | matrix {toList(nG:0_R)}););
	       );
	  );
     grabLowestDegree := () -> (
	  -- assumes: lowest degree pending list is already autosubducted.
	  -- this row reduces this list, placing all of the
	  -- entries back into Pending, but then appends the lowest
	  -- degree part into the basis.
	  e := lowestDegree();
	  if e <= maxdeg then (
	       if MOflag == 2 then (
		    numnewsagbi = #Pending#e;
		    appendToBasis matrix{Pending#e};
		    Pending#e = {};)
	       else (
	       	    m := rowReduce(matrix{Pending#e}, e);
	       	    Pending#e = {};
	       	    insertPending m;
	       	    e = lowestDegree();
	       	    appendToBasis matrix{Pending#e};
		    numnewsagbi = #Pending#e;
	       	    Pending#e = {};);
	       );
	  e);

     gbI = gens gb I;
     Gens = compress (Gens % I);
     G = matrix(R, {{}});
     Gensmaxdeg := (max degrees source Gens)_0;
     Gens = compress submatrixBelowDegree(Gens, maxdeg+1);
     insertPending Gens;
     Pending#0 = {};
     d = grabLowestDegree();  -- initializes G 
     if printlevel > 0 then (
       << "--- degree " << d << " ----" << endl;
       << numnewsagbi << " new generators" << endl;
       );
     d = d+1;
     nloops = d;
     isdone := false;
     while nloops <= maxnloops and not isdone do (
	  nloops = nloops+1;
	  if printlevel > 0 then 
	    << "--- degree " << d << " ----" << endl;
	  gbJquo = gb(Jquo, DegreeLimit=>d);
	  mtemp := gens gbJquo;
	  spairs := submatrixByDegrees(split2(mtemp,RStoS mtemp),d);
	  if printlevel > 1 then << "spairs = " << transpose spairs << endl;
	  spairs = compress Gmap(spairs);
	  if Pending#d != {} then (
	       if MOflag == 3 then (newgens = matrix{Pending#d})
	       else (newgens = RtoRS(matrix{Pending#d}));
	       spairs = spairs | newgens;
	       Pending#d = {};);
	  if numgens source spairs > 0 then (
	       if MOflag == 3 then (
		    newguys = subductquo(spairs, Gmap, Jquo, I, d))
	       else (newguys = autosubductionquo(spairs));
	       stopcriteria := numgens source newguys)
	  else stopcriteria = 0;
          if stopcriteria > 0 then (
	       if MOflag == 3 then (insertPending newguys)
	       else (insertPending RStoR newguys);
	       d = grabLowestDegree();
	       if printlevel > 0 then 
	         << numnewsagbi << " new generators" << endl;
	       )
	  else (
	       ngens := sum apply(toList Pending,i -> #i);
	       if ngens === 0 and gbDone gbJquo and 
	       d>Gensmaxdeg then (
	           isdone = true;
		   if printlevel > 0 then (
     	       	     << "" << endl;
		     << "********************************" << endl;
		     << "**** SAGBI basis is FINITE! ****" << endl;
		     << "********************************" << endl;
		     << "" << endl;
		     );
		   );
	      );
	  d = d+1;
	  );
     G)

end
R = QQ[a..f,MonomialOrder=>Eliminate 3]
(monoid R).Options
S = (coefficientRing R)[Variables=>10, MonomialOrder=>prepend(Weights=>6:1, (monoid R).Options.MonomialOrder)]
(monoid S).Options
