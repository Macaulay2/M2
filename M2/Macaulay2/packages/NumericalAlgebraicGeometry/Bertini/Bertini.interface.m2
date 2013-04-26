-- Bertini interface for NAG4M2
-- used by ../NumericalAlgebraicGeometry.m2

solveBertini = method(TypicalValue => List)
solveBertini (List,HashTable) := List => (F,o) -> (
     return solveBertini F; 
     )
solveBertini List := List => F -> ( -- uses Bertini package
     R := ring first F;
     coeffR := coefficientRing R;
     if not(
	  instance(ring 1_coeffR, ComplexField) 
	  or instance(ring 1_coeffR, RealField)
	  or coeffR===QQ or coeffR ===ZZ
	  ) then error "expected coefficients that can be converted to complex numbers";  
--     R' := CC[gens R];
     V := bertiniPosDimSolve F; --apply(F, f ->sub(f,R'));
     if dim V != 0 then error "input system is not 0-dimensional (infinite number of solutions)";
     apply(V#0, p->(
	       if #p.Points != 1 then error "expected 1 point per component";
	       first p.Points
	       ))
     )

protect StartSolutions, protect StartSystem
makeBertiniInput = method(TypicalValue=>Nothing, Options=>{StartSystem=>{},StartSolutions=>{},NumericalAlgebraicGeometry$gamma=>1.0+ii})
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
	    << " = (" << bertiniNumbers T#i << ")*(1-s)+s*("<< bertiniNumbers o.NumericalAlgebraicGeometry$gamma << ")*(" << bertiniNumbers o.StartSystem#i << ");" << endl 
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
     dir := makeBertiniInput(T, StartSystem=>S, StartSolutions=>solsS, NumericalAlgebraicGeometry$gamma=>o.NumericalAlgebraicGeometry$gamma);
     compStartTime := currentTime();      
     run("cd "|dir|"; "|BERTINIexe|if DBG<2 then " >bertini_session.log" else "");
     if DBG>0 then << "Bertini's computation time: " << currentTime()-compStartTime << endl;
     readSolutionsBertini(dir, "raw_solutions")
     )
     
