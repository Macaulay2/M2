-- Bertini interface for NAG4M2
-- used by ../NumericalAlgebraicGeometry.m2

solveBertini = method(TypicalValue => List)
solveBertini (List,HashTable) := List => (F,o) -> (
  	  dir := makeBertiniInput F; 
  	  run("cd "|dir|"; "|BERTINIexe|if DBG<2 then " >bertini_session.log" else "");
	  readSolutionsBertini(dir,"raw_solutions")
	  )

protect StartSolutions, protect StartSystem
makeBertiniInput = method(TypicalValue=>Nothing, Options=>{StartSystem=>{},StartSolutions=>{},NAG$gamma=>1.0+ii})
makeBertiniInput List := o -> T -> (
-- IN:
--	T = polynomials of target system
--      o.StartSystem = start system
  R := ring T#0;
  v := gens R; -- variables
  prec := precision coefficientRing R;
  dir := temporaryFileName(); 
  makeDirectory dir;
  f := openOut (dir|"/input"); -- THE name for Bertini's input file 
  f << "CONFIG" << endl;
  --f << "MPTYPE: 2;" << endl; -- multiprecision
  --f << "MPTYPE: 0;" << endl; -- double precision (default?)
  --f << "TRACKTYPE: 1;" << endl;
  f << "USEREGENERATION: 1;" << endl;
    
  if #o.StartSystem > 0 then
    f << "USERHOMOTOPY: 1;" << endl;
  f << endl << "END;" << endl << endl;
  f << "INPUT" << endl << endl;
  if #o.StartSystem > 0 then
    f << "variable "
  else f << "variable_group "; -- variable section
  scan(#v, i->
       if i<#v-1 
       then f << toString v#i << ", "
       else f << toString v#i << ";" << endl
       );
  f << "function "; -- "function" section
  scan(#T, i->
       if i<#T-1
       then f << "fff" << i << ", "
       else f << "fff" << i << ";" << endl << endl
      );
  bertiniNumbers := p->if class p === CC 
  then toString realPart p | "+" | toString imaginaryPart p | "*I"
  else ( L := toExternalString p; 
       L = replace("p"|toString prec, "", L);
       L = replace("\\bii\\b", "I", L); 
       L = replace("([0-9])e([0-9])", "\\1E\\2", L);
       L
       );
  if #o.StartSystem == 0 
  then scan(#T, i -> f << "fff" << i << " = " << bertiniNumbers T#i << ";" << endl)
  else (
       if #o.StartSystem != #T then error "expected equal number of equations in start and target systems";
       f << "pathvariable t;" << endl 
         << "parameter s;" << endl
         << "s = t;" << endl;
	 
       scan(#T, i -> f << "fff" << i 
	    << " = (" << bertiniNumbers T#i << ")*(1-s)+s*("<< bertiniNumbers o.NAG$gamma << ")*(" << bertiniNumbers o.StartSystem#i << ");" << endl 
	   );
       );
  f << endl << "END" << endl << endl;
  close f;
  
  if #o.StartSolutions > 0 then (
       f = openOut (dir|"/start"); -- THE name for Bertini's start solutions file 
       f << #o.StartSolutions << endl << endl;
       scan(o.StartSolutions, s->(
		 scan(s, c-> f << realPart c << " " << imaginaryPart c << ";" << endl );
		 f << endl;
		 ));
       close f;
       );
  dir
  )

cleanupOutput = method(TypicalValue=>String)
cleanupOutput String := s -> (
-- cleanup output (Bertini and hom4ps2)
  t := replace("E", "e", s);
  t = replace("[(,)]","", t);
  t = replace("e\\+","e",t)
  )

readSolutionsBertini = method(TypicalValue=>List)

-- -- Dan's new stuff
-- readSolutionsBertini (String,List) := (dir,F) -> (
-- local pt;
-- local coord;
-- local coords;
-- local funcResid;
-- local condNum;
-- local newtonResid;
-- local lastT;
-- local cycleNum;
-- local success;
-- local solNum;
-- local numVars;
-- local a;
-- local numCodims;
-- local ptsInCodim;
-- local ptType; 
-- local ptMult;
-- local compNum;
-- local numDeflations;
-- local nv;
-- local ws;
-- local codimen;
-- local l;
--        l = lines get (dir|"/witness_data"); -- grabs all lines of the file
--        numVars = value(first l);  l=drop(l,1);
--        numCodims = value(first l); l=drop(l,1);

--        wList := {};  --list of witness sets

--        for codimNum from 1 to numCodims do (

--          -- WARNING!!!!!????? 
--          -- For now, this data is overwritten in each pass through the codimNum loop: need to store codim's worth of data at end of loop (later, when witness data type is stabilized/understood)

--          pts := {};  --for each codim, we store all points and all codims (next line), then sort after gathering all points in the codim
--          compNums := {};
--          maxCompNum := 0;  --keeps track of max component number in this codim

--          codimen = value(first l); l=drop(l,1);
--          ptsInCodim = value(first l); l=drop(l,1);

--          for ptNum from 1 to ptsInCodim do (
--             maxPrec := value(first l);
--             l = drop(l,1);
--             coords = {};
--             for j from 1 to numVars do ( -- grab each coordinate
--               coord = select("[0-9.+-]+e[0-9+-]+", cleanupOutput(first l));  -- use regexp to get the two numbers from the string
--               coords = join(coords, {toCC(53, value(coord#0),value(coord#1))});  -- NOTE: we convert to a 53 bit floating point complex type -- beware that we might be losing data here!!!???
--               l = drop(l,1);
--               );

--             l = drop(l,numVars+1);  -- don't need second copy of point or extra copy of maxPrec
            
-- 	    condNum = value(cleanupOutput(first l)); l=drop(l,4);
--             ptType = value(first l); l=drop(l,1);
--             ptMult = value(first l); l=drop(l,1);
--             compNum = value(first l); l=drop(l,1);
--             numDeflations = value(first l); l=drop(l,1);
-- 	    --print(codimNum, ptNum, compNum);

--             pt = new Point;
--             pt.coordinates = coords;
--             pts = join(pts,{pt});
--             compNums = join(compNums,{compNum});
--             if (compNum > maxCompNum) then maxCompNum=compNum; 
--           ); 
	 
-- 	  for j from 0 to maxCompNum do ( --loop through the component numbers in this codim to break them into witness sets
--  	    ptsInWS := {}; --stores all points in the same witness set
--             for k from 0 to #pts-1 do (
-- 	      print (j,k); 
-- 	      if (compNums#k == j) then ptsInWS = join(ptsInWS,{pts#k}); --save the point if its in the current component (component j)
-- 	    );
--             ws = witnessSet(ideal F,ideal 0, ptsInWS); --turn these points into a witness set
--             wList = join(wList, {ws}); --add witness set to list
--           );
--         );
--         nv = numericalVariety(ideal F, wList);
--      	)

readSolutionsBertini (String,String) := (dir,f) -> (
  s := {};
  if f == "finite_solutions" then (
       --print "implementation unstable: Bertini output format uncertain";
       l := lines get (dir|"/"|f);
       nsols := value first separate(" ", l#0);
       l = drop(l,2);
       while #s < nsols do (	 
	    coords := {};
	    while #(first l) > 2 do ( 
	      	 coords = coords | {(
		   	   a := separate(" ",  cleanupOutput(first l));	 
		   	   (value a#0)+ii*(value a#1)
	      	   	   )};
    	      	 l = drop(l,1);
	      	 );	
	    l = drop(l,1);
            if DBG>=10 then << coords << endl;
	    s = s | {{coords}};
	    );	
       ) 
  else if f == "raw_solutions" then (
       l = lines get (dir|"/"|f);
       while #l>0 and #separate(" ", l#0) < 2 do l = drop(l,1);
       while #l>0 do (
	    if DBG>=10 then << "------------------------------" << endl;
	    coords = {};
	    while #l>0 and #separate(" ", l#0) >= 2 do ( 
	      	 coords = coords | {(
		   	   a = separate(" ",  cleanupOutput(first l));	 
		   	   (value a#0)+ii*(value a#1)
	      	   	   )};
    	      	 l = drop(l,1);
	      	 );
	    while #l>0 and #separate(" ", l#0) < 2 do l = drop(l,1);
            if DBG>=10 then << coords << endl;
	    s = s | {{coords}};
	    );     
    ) else error "unknown output file";
  s
  )

trackBertini = method(TypicalValue => List)
trackBertini (List,List,List,HashTable) := List => (S,T,solsS,o) -> (
     -- tempdir := temporaryFileName() | "NumericalAlgebraicGeometry-bertini";
     -- mkdir tempdir; 	  
     dir := makeBertiniInput(T, StartSystem=>S, StartSolutions=>solsS, NAG$gamma=>o.NAG$gamma);
     compStartTime := currentTime();      
     run("cd "|dir|"; "|BERTINIexe|if DBG<2 then " >bertini_session.log" else "");
     if DBG>0 then << "Bertini's computation time: " << currentTime()-compStartTime << endl;
     readSolutionsBertini(dir, "raw_solutions")
     )
     
