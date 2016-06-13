


--   INPUT : '(M,v,N,w)',  where all four are matrices (although v and w are only vectors), such
--     	    	      	  that the polyhedron is given by P={x | Mx<=v and Nx=w} 
--  OUTPUT : 'P', the polyhedron
intersection(Matrix,Matrix,Matrix,Matrix) := (M,v,N,w) -> (
	-- checking for input errors
	if numColumns M =!= numColumns N then error("equations of half-spaces and hyperplanes must have the same dimension");
	if numRows M =!= numRows v or numColumns v =!= 1 then error("invalid condition vector for half-spaces");
	if numRows N =!= numRows w or numColumns w =!= 1 then error("invalid condition vector for hyperplanes");
	ineq := v | M;
   ezero := matrix {flatten {1 , toList ((numgens source M):0)}};
   ineq = ineq ||  ezero;
	eq := w | N;
   uCresult := new HashTable from {
      inequalities => ineq,
      equations => eq
   };
   result := new HashTable from {
      underlyingCone => cone uCresult
   };
   polyhedron result
)


--   INPUT : '(M,N)',  two matrices where either 'P' is the Cone {x | Mx<=0, Nx=0} if 'M' and 'N' have the same source space 
--     	    	       or, if 'N' is only a Column vector the Polyhedron {x | Mx<=v} 
--  OUTPUT : 'P', the Cone or Polyhedron
intersection(Matrix,Matrix) := (M,N) -> (
	-- Checking for input errors
	if ((numColumns M =!= numColumns N and numColumns N =!= 1) or (numColumns N == 1 and numRows M =!= numRows N)) and N != 0*N then 
		error("invalid condition vector for half-spaces");
	-- Decide whether 'M,N' gives the Cone C={p | M*p >= 0, N*p = 0}
   local result;
	if numColumns M == numColumns N and numColumns N != 1 then (
      result = new HashTable from {
         inequalities => M,
         equations => N
      };
      return cone result
	-- or the Polyhedron P={p | M*p >= N != 0}
	) else (	
      r := ring M;
      Nw := map(r^0, source M, 0);
      w := map(r^0, r^1, 0);
      intersection(M, N, Nw, w)
   )
)
   



--   INPUT : '(P1,P2)',  two polyhedra 
--  OUTPUT : 'P', the polyhedron that is the intersection of both
intersection(Polyhedron,Polyhedron) := (P1,P2) -> (
	-- Checking if P1 and P2 lie in the same space
	if ambDim(P1) =!= ambDim(P2) then error("Polyhedra must lie in the same ambient space");
   C1 := getProperty(P1, underlyingCone);
   C2 := getProperty(P2, underlyingCone);
   C12 := intersection(C1, C2);
   result := new HashTable from {
      underlyingCone => C12
   };
   polyhedron result
)





--   INPUT : '(C1,C2)',  two Cones
--  OUTPUT : 'C', the Cone that is the intersection of both
intersection(Cone,Cone) := (C1,C2) -> (
	-- Checking if C1 and C2 lie in the same space
	if ambDim(C1) =!= ambDim(C2) then error("Cones must lie in the same ambient space");
	M := halfspaces C1 || halfspaces C2;
	N := hyperplanes C1 || hyperplanes C2;
	intersection(M,N))
   
   
   
--   INPUT : '(C,P)',  a Cone and a Polyhedron
--  OUTPUT : 'Q', the Polyhedron that is the intersection of both
intersection(Cone,Polyhedron) := (C,P) -> intersection {C,P}



--   INPUT : '(P,C)',  a Polyhedron and a Cone
--  OUTPUT : 'Q', the Polyhedron that is the intersection of both
intersection(Polyhedron,Cone) := (P,C) -> intersection {P,C}



--   INPUT : 'L',   a list of Cones, Polyhedra, inequalities given by (M,v), 
--     	    	    and hyperplanes given by '{N,w}'
intersection List := L -> (
     -- This function checks if the inserted pair is a pair of matrices that gives valid in/equalities
     isValidPair := S -> #S == 2 and if S#1 == 0 then instance(S#0,Matrix) else instance(S#1,Matrix) and numRows S#0 == numRows S#1 and numColumns S#1 == 1;
     -- Checking for input errors  
     if L == {} then error("List of cones must not be empty");   
     C := L#0;
     -- The first entry in the list determines the ambient dimension 'n'
     n := 0;
     local Ml;
     local vl;
     local Nl;
     local wl;
     if (not instance(C,Cone)) and (not instance(C,Polyhedron)) and (not instance(C,Sequence)) and (not instance(C,List)) then 
	  error ("The input must be cones, polyhedra, inequalities, equalities.");
     -- Adding the inequalities and equalities to 'M,v,N,w', depending on the type of 'C'
     if instance(C,Cone) then (
	  n = ambDim(C);
	  Ml = -(halfspaces C);
	  vl = map(target halfspaces C,ZZ^1,0);
	  Nl = hyperplanes C;
	  wl = map(target hyperplanes C,ZZ^1,0))
     else if instance(C,Polyhedron) then (
	  n = ambDim(C);
	  Ml = (halfspaces C)#0;
	  vl = (halfspaces C)#1;
	  Nl = (hyperplanes C)#0;
	  wl = (hyperplanes C)#1)
     else if instance(C,Sequence) then (
	  -- Checking for input errors
	  if not isValidPair C then error("Inequalities must be given as a sequence of a matrix and a column vector");
	  --Ml = chkQQZZ(C#0,"half-spaces");
	  n = numColumns C#0;
	  Ml = if C#1 == 0 then ((transpose chkQQZZ(transpose C#0,"half-spaces"))|map(ZZ^(numRows C#0),ZZ^1,0)) else transpose chkQQZZ(transpose(C#0|C#1),"halfspaces or condition vector");
	  vl = Ml_{n};
	  Ml = submatrix'(Ml,{n});
     	  --vl = if C#1 == 0 then map(target Ml,ZZ^1,0) else chkQQZZ(C#1,"condition vector for half-spaces");
	  Nl = map(ZZ^1,source Ml,0);
	  wl = map(ZZ^1,ZZ^1,0))
     else (
	  -- Checking for input errors
	  if not isValidPair C then error("Equalities must be given as a list of a matrix and a column vector");
	  --Nl = chkQQZZ(C#0,"hyperplanes");
	  n = numColumns C#0;
	  Nl = if C#1 == 0 then ((transpose chkQQZZ(transpose C#0,"hyperplanes"))|map(ZZ^(numRows C#0),ZZ^1,0)) else transpose chkQQZZ(transpose(C#0|C#1),"hyperplanes or condition vector");
	  wl = Nl_{n};print wl;
	  Nl = submatrix'(Nl,{n});
	  Ml = map(ZZ^1,source Nl,0);
	  vl = map(ZZ^1,ZZ^1,0));
	  --wl = if C#1 == 0 then map(target Nl,ZZ^1,0) else chkQQZZ(C#1,"condition vector for half-spaces"));
     --  Adding the inequalities and equalities to 'M,v,N,w', for each remaining element in 'L', depending on the type of 'C'
     L = apply(drop(L,1), C1 -> (
	       -- Checking for further input errors
	       if (not instance(C1,Cone)) and (not instance(C1,Polyhedron)) and (not instance(C1,Sequence)) and (not instance(C1,List)) then 
		    error("The input must be cones, polyhedra, inequalities, equalities.");
	       if instance(C1,Cone) then (
		    if ambDim C1 != n then error("All Cones and Polyhedra must be in the same ambient space");
		    (-(halfspaces C1),map(target halfspaces C1,ZZ^1,0),hyperplanes C1,map(target hyperplanes C1,ZZ^1,0)))
	       else if instance(C1,Polyhedron) then (
		    if ambDim C1 != n then error("All Cones and Polyhedra must be in the same ambient space");
		    ((halfspaces C1)#0,(halfspaces C1)#1,(hyperplanes C1)#0,(hyperplanes C1)#1))
	       else if instance(C1,Sequence) then (
		    -- Checking for input errors
		    if not isValidPair C1 then error("Inequalities must be given as a sequence of a matrix and a column vector");
		    if numColumns C1#0 != n then error("Inequalities must be for the same ambient space.");
		    C1 = if C1#1 == 0 then ((transpose chkQQZZ(transpose C1#0,"half-spaces"))|map(ZZ^(numRows C1#0),ZZ^1,0)) else transpose chkQQZZ(transpose(C1#0|C1#1),"halfspaces or condition vector");
		    (submatrix'(C1,{n}),C1_{n},map(ZZ^1,ZZ^n,0),map(ZZ^1,ZZ^1,0)))		      	   
--		    C1 = (chkQQZZ(C1#0,"half-spaces"),chkQQZZ(C1#1,"condition vector for half-spaces"));
--		    if C1#1 == 0 then (C1#0,map(target C1#0,ZZ^1,0),map(ZZ^1,source C1#0,0),map(ZZ^1,ZZ^1,0))
--		    else (C1#0,C1#1,map(ZZ^1,source C1#0,0),map(ZZ^1,ZZ^1,0)))
	       else (
		    -- Checking for input errors
		    if not isValidPair C1 then error("Equalities must be given as a list of a matrix and a column vector");
		    if numColumns C1#0 != n then error ("Inequalities must be for the same ambient space.");
		    C1 = if C1#1 == 0 then ((transpose chkQQZZ(transpose C1#0,"hyperplanes"))|map(ZZ^(numRows C1#0),ZZ^1,0)) else transpose chkQQZZ(transpose(C1#0|C1#1),"hyperplanes or condition vector");
		    (map(ZZ^1,ZZ^n,0),map(ZZ^1,ZZ^1,0),submatrix'(C1,{n}),C1_{n}))));
--		    C1 = (chkQQZZ(C1#0,"hyperplanes"),chkQQZZ(C1#1,"condition vector for hyperplanes"));
--		    if C1#1 == 0 then (map(ZZ^1,source C1#0,0),map(ZZ^1,ZZ^1,0),C1#0,map(target C1#0,ZZ^1,0))
--		    else (map(ZZ^1,source C1#0,0),map(ZZ^1,ZZ^1,0),C1#0,C1#1))));
     LM := flatten apply(L, l -> entries(l#0));
     if LM != {} then Ml = Ml || matrix LM;
     LM = flatten apply(L, l -> entries(l#1));
     if LM != {} then vl = vl || matrix LM;
     LM = flatten apply(L, l -> entries(l#2));
     if LM != {} then Nl = Nl || matrix LM;
     LM = flatten apply(L, l -> entries(l#3));
     if LM != {} then wl = wl || matrix LM;
     if vl == 0*vl and wl == 0*wl then intersection(-Ml,Nl) else intersection(Ml,vl,Nl,wl));
