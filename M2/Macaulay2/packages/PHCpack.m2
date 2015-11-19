
newPackage(
  "PHCpack",
  Version => "1.6.2", 
  Date => "21 May 2015",
  Authors => {
    {Name => "Elizabeth Gross",
     Email => "egross7@uic.edu",
     HomePage => "http://www.math.uic.edu/~lizgross"},
    {Name => "Sonja Petrovic", 
     Email => "Sonja.Petrovic@iit.edu",
     HomePage => "http://mypages.iit.edu/~spetrov1/"},
    {Name => "Jan Verschelde", 
     Email => "jan@math.uic.edu",
     HomePage => "http://www.math.uic.edu/~jan"},
    {Name => "Contributing Author: Anton Leykin",
     HomePage => "http://www.math.gatech.edu/~leykin"}
  },
  Headline => "Interface to PHCpack",
  Configuration => { 
    "path" => "",
    "PHCexe"=>"phc", 
    "keep files" => true
  },
  Certification => {
	"journal name" => "The Journal of Software for Algebra and Geometry",
	"journal URI" => "http://j-sag.org/",
	"article title" => "Interfacing with PHCpack",
	"acceptance date" => "2013-07-11",
	"published article URI" => "http://www.j-sag.org/Volume5/jsag-4-2013.pdf",
	"published code URI" => "http://www.j-sag.org/Volume5/PHCpack.m2",
	"repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/PHCpack.m2",
	"release at publication" => "48d32ceffeeb807b23eee20a5c9c243ce085b3ef",
	"version at publication" => "1.6",
	"volume number" => "5",
	"volume URI" => "http://j-sag.org/Volume5/"
	},
  --DebuggingMode => false,
  DebuggingMode => true,
  AuxiliaryFiles => true,
  CacheExampleOutput => true,
  PackageImports => {"SimpleDoc"},
  PackageExports => {"NAGtypes"}
)

--Copyright 2013 Elizabeth Gross, Sonja Petrovic, Jan Verschelde.
--  You may redistribute this file under the terms of the GNU General
--  Public License as published by the Free Software Foundation,
--  either version 2 of the License, or any later version.

export { 
  "cascade",
  "constructEmbedding",
  "gamma",
  "factorWitnessSet",
  "isCoordinateZero",
  "isWitnessSetMember",
  "mixedVolume",
  "nonZeroFilter",
  "numericalIrreducibleDecomposition",
  "parseSolutions",
  "refineSolutions",
  "solveRationalSystem",
  "solveSystem",
  "StableMixedVolume",
  "StartDimension",
  "StartSystem",
  "tDegree",
  "toLaurentPolynomial",
  "topWitnessSet",
  "trackPaths",
  "zeroFilter"
}

protect ErrorTolerance, protect Iterations,
protect Bits, protect ResidualTolerance, 
protect Append 

--##########################################################################--
-- GLOBAL VARIABLES 
--##########################################################################--

PHCDBG = 0; -- debug level (10=keep temp files)
path'PHC = (options PHCpack).Configuration#"path";
PHCexe=path'PHC|(options PHCpack).Configuration#"PHCexe"; 
-- this is the executable string that make sures that calls to PHCpack run:
-- NOTE: the absolute path should be put into the init-PHCpack.m2 file 


--##########################################################################--
-- INTERNAL METHODS
--##########################################################################--

----------------------------------
-- NumericalVariety
----------------------------------

--the following defines the type NumericalVariety
--if the user is using an old version of NAGtypes.

if not((class(NumericalVariety))===Type) then
     (--export {"generalEquations", "IsIrreducible"};
      --protect generalEquations;
      protect IsIrreducible;
      NumericalVariety = new Type of MutableHashTable;
      NumericalVariety.synonym = "numerical variety";
      dim NumericalVariety := V -> max select(keys V, k->class k === ZZ);
      degree NumericalVariety := V -> (
     	   d := dim V;
     	   sum(keys V, k->if k =!= d then 0 else sum(V#k,degree))
     	   );
      numericalVariety = method(TypicalValue=>NumericalVariety);
      numericalVariety List := Ws -> (
     	   V := new NumericalVariety;
     	   scan(Ws, W->(
	       	     d := dim W;
	       	     if V#?d then V#d = V#d | {W} else V#d = {W};
	       	     ));     
     	   check V;
     	   V
     	   );
      check NumericalVariety := o-> V -> (
     	   if any(keys V, k->(class k =!= ZZ or k<0)) 
     	   then error "the keys of a NumericalVariety should be nonnegative integers";
     	   scan(keys V, k->if class k === ZZ then scan(V#k, W->(
		    	  if dim W != k then 
		    	  error "dimension of a witness set does not match the key in NumericalVariety";
		    	  )));
     	   );
      net NumericalVariety := V -> (
     	   out := "A variety of dimension " | net dim V |" with components in";
     	   scan(keys V, k->if class k === ZZ then (
	       	     row := "dim "|net k|": ";
	       	     scan(V#k, W->row = row|" "|net W);
	       	     out = out || row;
	       	     ));
     	   out
     	   ) 	  
      )  
----------------------------------
--- File read/write operations ---
----------------------------------

getFilename = () -> (
  filename := temporaryFileName();
  while fileExists(filename) 
    or fileExists(filename|"PHCinput") 
    or fileExists(filename|"PHCoutput") do filename = temporaryFileName();
  filename
)

parseSolutions = method(TypicalValue => Sequence, Options => {Bits => 53})
parseSolutions (String,Ring) := o -> (s,R) -> (
  -- parses solutions in PHCpack format 
  -- IN:  s = string of solutions in PHCmaple format 
  --      V = list of variable names
  -- OUT: List of solutions, each of type Point, 
  --      carrying also other diagnostic information about each.
  oldprec := defaultPrecision;
  defaultPrecision = o.Bits;
  L := get s; 
  L = replace("=", "=>", L);
  L = replace("I", "ii", L);
  L = replace("E\\+","e",L);
  L = replace("E", "e", L);
  L = replace("time", "\"time\"", L);
  L = replace("rco", "\"rco\"", L);
  L = replace("multiplicity", "\"mult\"", L); 
  L = replace("\\bres\\b", "\"residual\"", L);
  L = replace("\\bresolution\\b", "\"residual\"", L);
  -- because M2 automatically thinks "res"=resolution   	  
  sols := toList apply(value L, sol->new HashTable from toList sol);
  defaultPrecision = oldprec;
  apply(sols, sol->point( {apply(gens R, v->sol#v)} | outputToPoint sol ))
)

pointsToFile = method(TypicalValue => Nothing, Options => {Append => false})
pointsToFile (List,Ring,String) := o -> (S,R,name) -> (
  -- writes list of points to file in PHCpack format
  -- IN: S, list of points, e.g.: obtained as points(WitnessSet);
  --     R, ring (with symbols for the variables);
  --     name, string with file name.
  file := if o.Append then openOutAppend name else openOut name;
  if o.Append then 
    file << endl << "THE SOLUTIONS :" << endl;
  file << #S << " " << numgens R << endl << 
  "===========================================================" << endl;
  scan(#S, i->( 
    file << "solution " << i+1 << " :" << endl <<
    "t :  0.00000000000000E+00   0.00000000000000E+00" << endl <<
    "m :  1" << endl <<
    "the solution for t :" << endl;
     scan(numgens R, v->(
       L := " "|toString R_v|" :  "|
       format(0,-1,9,9,realPart toCC S#i#v)|"  "|
       format(0,-1,9,9,imaginaryPart toCC S#i#v);
       file << L << endl; 
    ));
    file <<  "== err :  0 = rco :  1 = res :  0 ==" << endl;
  ));
  close file;
) 

solutionsToFile = method(TypicalValue => Nothing, Options => {Append => false})
solutionsToFile (List,Ring,String) := o -> (S,R,name) -> (
  -- writes solutions to file in PHCpack format
  -- IN: S, list of solutions;
  --     R, ring (with symbols for the variables);
  --     name, string with file name.
  file := if o.Append then openOutAppend name else openOut name;
  if o.Append then 
    file << endl << "THE SOLUTIONS :" << endl;
  file << #S << " " << numgens R << endl << 
  "===========================================================" << endl;
  scan(#S, i->( 
    file << "solution " << i << " :" << endl <<
    "t :  0.00000000000000E+00   0.00000000000000E+00" << endl <<
    "m :  1" << endl <<
    "the solution for t :" << endl;
     scan(numgens R, v->(
       L := " "|toString R_v|" :  "|
       format(0,-1,9,9,realPart toCC S#i#Coordinates#v)|"  "|
       format(0,-1,9,9,imaginaryPart toCC S#i#Coordinates#v);
       file << L << endl; 
    ));
    file <<  "== err :  0 = rco :  1 = res :  0 ==" << endl;
  ));
  close file;
) 

startSystemFromFile = method(TypicalValue => List)
startSystemFromFile (String) := (name) -> (
  -- IN: file name starting with a random coefficient start system
  -- OUT: list of polynomials in a ring with coefficients in CC
  -- REQUIRED: the format of the system on file is a random coefficient
  --   system produced by phc -m, every term starts on a separate line
  --   with the "+" sign.  The coefficient field must be CC.
  s := get name;
  s = replace("i","ii",s);
  s = replace("E","e",s);
  s = replace("e\\+00","",s);
  L := lines(s);
  n := value L_0;
  result := {};
  i := 0; j := 1;
  local stop;
  local term;
  local p;
  while i < n do (
    stop = false; p = 0;
    while not stop do (
      if #L_j != 0 then (
        -- we have to bite off the first "+" sign of the term
        if (L_j_(#L_j-1) != ";") then (
           term = value substring(1,#L_j-1,L_j); p = p + term;
        ) else ( -- in this case (L_j_(#L_j-1) == ";") holds
          term = value substring(1,#L_j-2,L_j); p = p + term;
          stop = true; result = result | {p}
        );
      ); j = j + 1;
      stop = stop or (j >= #L);
    );
    i = i + 1; 
  );
  result
)

systemFromFile = method(TypicalValue => List)
systemFromFile (String) := (name) -> (
  -- IN: file name starting with a polynomial system
  -- OUT: list of polynomials in a ring with coefficients in CC
  -- NOTE: the format of the system on file may be such that the
  --   first term starts with +1*x which cannot be digested by M2.
  --   In contrast to the startSystemFromFile, the first term could
  --   also be "-1*x" so we must be a bit more careful...
  --   Another problem are constants as "3.0e+00", at least on a Mac.
  s := get name;
  s = replace("i","ii",s);
  s = replace("E","e",s);
  s = replace("e\\+","e",s);   -- M2 does not like 3.0e+00 as constant
  L := lines(s);
  dimL0 := separate(" ", replace ("^ *","",L_0)); -- deal with case of nonsquare systems
  n := value dimL0_0;          -- first is always number of equations
  result := {};
  i := 0; j := 1;
  local stop;
  local term;
  local p;
  while i < n do (
    stop = false; p = 0;
    while not stop do (
      if #L_j != 0 then (
        if (L_j_(#L_j-1) != ";") then (
          -- we have to bite off the first "+" sign of the term
          term = value substring(1,#L_j-1,L_j);
          if (L_j_0 == "+") then p = p + term else p = p - term;
        ) else ( -- in this case (L_j_(#L_j-1) == ";") holds
          term = value substring(1,#L_j-2,L_j);
          if (L_j_0 == "+") then p = p + term else p = p - term;
          stop = true; result = result | {p}
        );
      ); j = j + 1;
      stop = stop or (j >= #L);
    );
    i = i + 1; 
  );
  result
)

systemToFile = method(TypicalValue => Nothing)
systemToFile (List,String) := (F,name) -> (
  file := openOut name;
  if (#F == numgens ring F#0)
   then file << #F << endl
   else file << #F << " " << numgens ring F#0 << endl;
  scan(F, f->( 
    L := toExternalString f;
    L = replace("ii", "I", L);
    L = replace("e", "E", L);
    L = replace("p53","",L);
    L = replace("p[0-9]+","",L);
    file << L << ";" << endl;
  ));
  close file;
) 

witnessSetFromFile = method(TypicalValue => WitnessSet)
witnessSetFromFile (String) := (name) -> (
  -- IN: file name which contains a witness set in PHCpack format
  -- OUT: a witness set
  e := systemFromFile(name); 
  d := dimEmbedding(e);
  witnessPointsFile := temporaryFileName() | "PHCwitnessPoints";
  if fileExists witnessPointsFile then removeFile witnessPointsFile;
  run(PHCexe|" -z " | name | " "|witnessPointsFile);
  eR := ring first e;
  g := parseSolutions(witnessPointsFile,eR);
  w := witnessSet(ideal(take(e,{0,#e-d-1})),ideal(take(e,{#e-d,#e-1})),g);
  return w;
)

witnessSetToFile = method()
witnessSetToFile (WitnessSet,String) := (witset,name) -> (
  -- IN: name, a file name where to write a witness set to,
  --     witset, a witness set.
  R := ring ideal(witset);
  s := equations(witset)|slice(witset);
  p := points(witset);
  systemToFile(s,name);
  pointsToFile(p,R,name,Append=>true);
)

----------------------------
-- embedding dimension -----
----------------------------

dimEmbedding = method(TypicalValue => ZZ)
dimEmbedding (List) := (system) -> (
  -- IN: embedded system with slack variables.
  -- OUT: returns the number of slack variables = the dimension.
  eR := ring first system;
  v := gens eR;
  slack := v_(#v-1); 
  zz := toString(slack);
  ds := substring(2,#zz-1,zz);
  dimension := if (value(ds)===null) then 0 else value(ds);
  return dimension;
)

-----------------------------
---  conversion to Point  ---
-----------------------------

outputToPoint = method()
outputToPoint HashTable := (H)->{
  SolutionStatus => if H#"mult" == 1 then Regular else Singular, 
  ConditionNumber => (H#"rco")^(-1), 
  LastT => H#"time"
}

----------------------------------
----add slack variables-----------
----------------------------------

addSlackVars = method()
addSlackVars WitnessSet := (W) -> (
     -- creates a new system of polynomials, in variables:
     -- old set of variables, and zz1, ..., zzd, where
     -- d is the dimension of W.
     R := ring W;
     n := numgens R;
     d := dim W; -- this will be the number of slack variables to add
     W1 := generalEquations W;
     -- Add in new variables zz1, ..., zzd,
     --  this changes the equations, the slice, and the points
     slackvars := apply(d, i->getSymbol("zz"|toString (i+1)));
     newR := (coefficientRing R)[gens R, slackvars];
     newvars := (vars newR)_{n..n+d-1};
     -- new slice:
     newSlice := apply(d, i -> sub(W1.Slice#i,newR) + newR_(n + i));
     -- add a linear matrix 
     A := random(newR^(d),newR^(n-d));
     AZ := newvars * A;
     newEqns := (sub(gens ideal W1, newR) + AZ) | newvars;
     -- new points
     zeros := toList apply(d, i -> 0_(coefficientRing R));
     newPoints := apply(W1.Points, pt -> point({join(coordinates(pt),zeros)}));
     witnessSet(ideal newEqns, ideal newSlice, newPoints)
     )

-------------------------------------------------------
------------  Witness Superset Filters  ---------------
-------------------------------------------------------

witnessSuperSetFilter = method()
witnessSuperSetFilter (WitnessSet,List) := (witset,pts) -> (
  -- This is an auxiliary procedure to witnessSuperSetsFilter,
  -- applying isWitnessSetMember to filter points in a witness superset.
  -- IN: witset, a witness set to represent a witness set;
  --     pts, a list of points to be tested for membership.
  -- OUT: a list of points in pts for which isWitnessSetMember is false.
  result := new MutableList from {};
  for p in pts do (
    if not isWitnessSetMember(witset,p)
     then result = append(result,p);
  );
  return result;
)

witnessSuperSetsFilter = method()
witnessSuperSetsFilter (MutableList,List) := (witsets,pts) -> (
  -- This is an auxiliary procedure to cascade,
  -- applying witnessSuperFilter to filter points in the
  -- witness supersets given in the list.
  -- IN: witsets, a list of tuples (dimension,witness set),
  --     pts, a list of points to be tested for membership.
  -- OUT: a list of points in pts for which isWitnessSetMember is false.
  result := new MutableList from {};
  local c;
  for p in pts do (
    found := false;
    if instance(p,Point)
     then c = p
     else c = point{p};
    for w in witsets when (not found) do (
      ws := w#1;
      found = isWitnessSetMember(ws,c);
    );
    if not found then result = append(result,p);
  );
  return toList(result);
)


--##########################################################################--
-- EXPORTED METHODS
--
-- NOTE:
-- trackPaths and refineSolutions are methods adapted 
-- from  NumericalAlgebraicGEometry/PHCpack.interface.m2
--##########################################################################--
-----------------------------------------------
------------  CASCADE  ------------------------
-----------------------------------------------
cascade = method(TypicalValue => NumericalVariety, Options => {StartDimension => -1,Verbose => false})
cascade (List) := o -> (system) -> (
  -- IN: system, a polynomial system;
  --     dimension, top dimension of the solution set.
  -- OUT: a hash table with keys the dimension of each component,
  --      values are witness sets for positive dimensions,
  --      or a list of isolated solutions for key equal to zero.
  
  if not(class coefficientRing ring ideal system===ComplexField) then
    error "coefficient ring is not complex";
  
  R := ring ideal system;
  
  if # system > numgens R then error "the system is overdetermined";
    
  if o.StartDimension==-1 then startdim:=(numgens R)-1
    else startdim=o.StartDimension;  
    
  PHCinputFile := temporaryFileName() | "PHCinput";
  PHCoutputFile := temporaryFileName() | "PHCoutput";
  PHCbatchFile := temporaryFileName() | "PHCbatch";
  PHCsolsFile := temporaryFileName() | "PHCsols";
  PHCsessionFile := temporaryFileName() | "PHCsession";
  for f in {PHCinputFile, PHCoutputFile, PHCbatchFile, PHCsolsFile, PHCsessionFile} do if fileExists f then removeFile f;
  toList (0..startdim) / (i->if fileExists (PHCoutputFile | "_sw" | i) 
       then removeFile (PHCoutputFile | "_sw" | i) );
  if o.Verbose then
    stdio << "writing output to file " << PHCoutputFile << endl;
  
  systemToFile(system,PHCinputFile);
  s := concatenate("0\ny\n",PHCinputFile);
  s = concatenate(s,"\n",PHCoutputFile);
  s = concatenate(s,"\n");
  s = concatenate(s,toString(startdim));
  s = concatenate(s,"\nn\n");
  bat := openOut PHCbatchFile;
  bat << s;
  close bat;
  if o.Verbose then
    ( stdio << "calling phc -c < " << PHCbatchFile;
    stdio << " > " << PHCsessionFile << endl
    );
    run(PHCexe|" -c < " | PHCbatchFile | " > " | PHCsessionFile);
  if o.Verbose then
    ( stdio << "output of phc -c is in file " << PHCoutputFile << endl;
    stdio << "... constructing witness sets ... " << endl
    );
  
  local slackvars;
  local RwithSlack;
  
  --get solutions
  
  result := new MutableList from {};
  dims:=select(toList (0..startdim),j->(fileExists (PHCoutputFile | "_sw" | j) and 
	    match("THE SOLUTIONS",get (PHCoutputFile | "_sw" | j))));
  topdimension := max dims; 
  i := topdimension;
  while i>=0 do
  (   if member(i,dims) then (	
      fil := (PHCoutputFile | "_sw" | i);
      
      if i > 0 then
      (
        slackvars = apply(i, k->getSymbol("zz"|toString(k+1)));
        RwithSlack = (coefficientRing R)monoid(gens R | slackvars);
        use RwithSlack;
        supwit := witnessSetFromFile(fil);
        if i == topdimension then (
          result = append(result,(i,supwit));
        ) else (
          supsols := points(supwit);
          genpts := witnessSuperSetsFilter(result,supsols);
          g := toList(apply(genpts,x->point{x}));
          ws := witnessSet(ideal(equations(supwit)),ideal(slice(supwit)),g);
          if #g!=0 then result = append(result,(i,ws));
        );
      ) else (
        run(PHCexe | " -z " | fil | " " | PHCsolsFile);
        use R;
	supwit = witnessSetFromFile(fil);
	psols := parseSolutions(PHCsolsFile,R);
        isols := witnessSuperSetsFilter(result,psols);
        ws = witnessSet(ideal(equations(supwit)),ideal(slice(supwit)),isols);
	if #isols!=0 then result = append(result,(0,ws));
      );
    );  
    i = i-1
  );
  use R;
  numericalVariety(toList (apply(result,i-> last i)))
)

-----------------------------------------------
-------- CONSTRUCT EMBEDDING ------------------
-----------------------------------------------
 
constructEmbedding = method(TypicalValue => List,Options => {Verbose => false})
constructEmbedding (List, ZZ) := o->  (system, dimension) -> (
  -- IN: system, a polynomial system with complex coefficients;
  --     dimension, expected dimension of the solution set.
  -- OUT: system with as many random hyperplanes at the end
  --      as the value of dimension.
  
  if not(class coefficientRing ring first system===ComplexField) then
    error "coefficient ring of system is not complex";
  
  PHCinputFile := temporaryFileName() | "PHCinput";
  PHCoutputFile := temporaryFileName() | "PHCoutput";
  PHCbatchFile := temporaryFileName() | "PHCbatch";
  PHCsessionFile := temporaryFileName() | "PHCsession";
  for f in {PHCinputFile, PHCoutputFile, PHCbatchFile, PHCsessionFile} do if fileExists f then removeFile f;
  
  systemToFile(system,PHCinputFile);
  s := concatenate("1\ny\n",PHCinputFile);
  s = concatenate(s,"\n",PHCoutputFile);
  s = concatenate(s,"\n");
  s = concatenate(s,toString(dimension));
  s = concatenate(s,"\nn\n");
  bat := openOut PHCbatchFile;
  bat << s;
  close bat;
  if o.Verbose then
  (  stdio << "calling phc -c < " << PHCbatchFile;
     stdio << " > " << PHCsessionFile << endl
     );
  run(PHCexe|" -c < " | PHCbatchFile | " > " | PHCsessionFile);
  if o.Verbose then
    stdio << "output of phc -c is in file " << PHCoutputFile << endl;
  
  -- extending the ring with slack variables 
  slackvars := apply(dimension, i->getSymbol("zz"|toString(i+1)));
  R := ring ideal system;
  
  -- surplus variables are used when the initial system is overconstrained
  nv := numgens R; 
  nq := #system; 
  nbss := nq - nv;
  local RwithSlack;
  if (nbss > 0) then (
    surplusvars := apply(nbss, i->getSymbol("ss"|toString(i+1)));
    RwithSlack = (coefficientRing R)[gens R, surplusvars, slackvars];
  ) else (
    RwithSlack = (coefficientRing R)[gens R, slackvars];
  );
  use RwithSlack;
  return systemFromFile(PHCoutputFile);
)

---------------------------------------------
-----------  FACTOR WITNESS SET --------------
---------------------------------------------

factorWitnessSet = method(TypicalValue=>List, Options => {Verbose => false})
factorWitnessSet (WitnessSet ) := o->  w -> (
  -- IN: a witness set properly embedded with slack variables.
  -- OUT: a list of witness sets, every element is irreducible.
  
  PHCinputFile := temporaryFileName() | "PHCinput";
  PHCoutputFile := temporaryFileName() | "PHCoutput";
  PHCbatchFile := temporaryFileName() | "PHCbatch";
  PHCsessionFile := temporaryFileName() | "PHCsession";
  if o.Verbose then
    stdio << "preparing input file to " << PHCinputFile << endl;
  system := equations(w) | slice(w); 
  systemToFile(system,PHCinputFile);
  R := ring first system;
  use R;
  L := toList(points(w));
  pointsToFile(L,R,PHCinputFile,Append=>true);
  if o.Verbose then
    stdio << "preparing batch file to " << PHCbatchFile << endl;
  s := concatenate("2\n",PHCinputFile); -- option 2 of phc -f
  s = concatenate(s,"\n",PHCoutputFile);
  s = concatenate(s,"\n1\n"); -- use monodromy to factor
  s = concatenate(s,"0\n");   -- default settings of path trackers
  bat := openOut PHCbatchFile;
  bat << s;
  close bat;
  if o.Verbose then
    stdio << "... calling monodromy breakup ..." << endl;
  run(PHCexe|" -f < " | PHCbatchFile | " > " | PHCsessionFile);
  if o.Verbose then
    (stdio << "session information of phc -f is in " << PHCsessionFile << endl;
    stdio << "output of phc -f is in file " << PHCoutputFile << endl
    );
 
  -- counting the number of factors
  count := 0;
  result := new MutableList from {};
  name := PHCinputFile | "_f" | toString(count+1);
  while (fileExists name) do (   
    result = append(result,witnessSetFromFile(name));
    count = count + 1;
    name = PHCinputFile | "_f" | toString(count+1);
  );
  stdio << "found " << count << " irreducible factors " << endl;
  for ws in result do ws#IsIrreducible=true;
  return numericalVariety(toList(result));
)

----------------------------
---- IS COORDINATE ZERO ----
----------------------------

isCoordinateZero = method(TypicalValue => Boolean)
isCoordinateZero (Point,ZZ,RR) := (sol,k,tol) -> (
  -- IN: sol, a solution of a polynomial system;
  --     k, index to a coordinate must be within range;
  --     tol, tolerance for the absolute value of the k-th coordinate.
  -- OUT: true, if the k-th coordinate has abs less than tol;
  --      false, otherwise.
  L := sol#Coordinates;
  return abs(L_k)<=tol; 
)


----------------------------------------------
----------IS WITNESS SET MEMBER  -------------
----------------------------------------------
isWitnessSetMember = method(TypicalValue => Boolean, Options => {Verbose => false})
isWitnessSetMember (WitnessSet,Point) := o-> (witset,testpoint) -> (
  -- IN: witset, a witness set for a positive dimensional solution set,
  --     testpoint, does it belong to the solution set?
  -- OUT: true if testpoint is a member of the solution set,
  --      false otherwise.

  PHCwitnessFile := temporaryFileName() | "PHCwitset";
  PHCtestpointFile := temporaryFileName() | "PHCtestpoint";
  PHCbatchFile := temporaryFileName() | "PHCbatch";
  PHCoutputFile := temporaryFileName() | "PHCoutput";
  PHCsessionFile := temporaryFileName() | "PHCsession";
  if o.Verbose then
    stdio << "writing witness set to file " << PHCwitnessFile << endl;
  witnessSetToFile(witset,PHCwitnessFile);
  d := dim(witset); -- pad test point with zero values for slack variables
  dzeros := toList(apply(0..d,i->0));
  L := {coordinates(testpoint)|dzeros};
  R := ring ideal(witset);
  if o.Verbose then
    stdio << "writing test point to file " << PHCtestpointFile << endl;
  pointsToFile(L,R,PHCtestpointFile);
  s := concatenate("1\n0\n",PHCwitnessFile);
  s = concatenate(s,"\n",PHCtestpointFile);
  s = concatenate(s,"\n",PHCoutputFile);
  s = concatenate(s,"\n0\n");
  bat := openOut PHCbatchFile;
  bat << s;
  close bat;
  if o.Verbose then (
    stdio << "calling phc -f < " << PHCbatchFile;
    stdio << " > " << PHCsessionFile << endl;
  );
  run(PHCexe|" -f < " | PHCbatchFile | " > " | PHCsessionFile);
  if o.Verbose then
    stdio << "output of phc -f is in file " << PHCoutputFile << endl;
  -- if the point does not belong to the witness set,
  -- then the output file contains the word "not"
  r := get PHCoutputFile;
  result := not match("not",r);
  return result;
)


-----------------------------------
-------- MIXED VOLUME -------------
-----------------------------------

mixedVolume = method(Options => {StableMixedVolume => false, StartSystem => false, Verbose => false})
mixedVolume  List := Sequence => opt -> system -> (
  -- IN:  system = list of polynomials in the system 
  -- OUT: mixed volume of the system. if optional inputs specified, then output is
  --      a sequence containing a subset of: mixed volume, stable mixed volume,
  --      start system, and solutions to start system.
  -- Calls an Ada translation of ACM TOMS Algorithm 846:
  --  "MixedVol: a software package for mixed-volume computation" 
  -- by Tangan Gao, T. Y. Li, Mengnien Wu, ACM TOMS 31(4):555-560, 2005.
  -- With the introduction of double double and quad double arithmetic,
  -- the menu options after version 2.3.90 changed.
  -- Fixed in the distribution of 2.3.97 of PHCpack.
  R := ring ideal system;
  n := #system;
  
  if n < numgens R then error "the system is underdetermined";
  
  if n > numgens R then error "the system is overdetermined";
  
  if not(class coefficientRing R===ComplexField) then
    error "coefficient ring is not complex";
  
  filename := getFilename();
  if opt.Verbose then
    stdio   << "using temporary files " << filename|"PHCinput" << " and " << filename|"PHCoutput" << endl;
  infile := filename|"PHCinput";
  outfile := filename|"PHCoutput";
  cmdfile := filename|"PHCcommands";
  sesfile := filename|"PHCsession";
  if opt.StartSystem then startfile := filename|"PHCstart";

  -- writing data to the corresponding files
  file := openOut cmdfile; 
  file << "4" << endl; -- call MixedVol in PHCpack
  if opt.StartSystem
   then (file << "1" << endl)  -- random coefficient start system wanted
   else (file << "0" << endl); -- no random coefficient start system
  if opt.StableMixedVolume
   then (file << "y" << endl)  -- stable mixed volume wanted
   else (file << "n" << endl); -- no stable mixed volume 
  file << "n" << endl; -- no mixed-cell configuration on file
  if opt.StartSystem then (    -- file and options for start system
    file << startfile << endl;
    file << "0" << endl << "1" << endl;
  );

  close file;
  systemToFile(system,infile);
  
  -- launching mixed volume calculator :
  execstr := PHCexe|" -m "|infile|" "|outfile|" < "|cmdfile|" > "|sesfile;
  ret := run(execstr);
  if ret =!= 0 then 
    error "error occurred while executing PHCpack command: phc -m";
  F := get outfile; 
  
  -- search lines of outfile for:  " mixed volume : "
  -- once found, extract just the number and return its value:
  local mixvol;
  scanLines(line ->  
    if substring(0,21,line) == "common mixed volume :" then (
      mixvol = value replace("common mixed volume : ","",line);
      break
   ), outfile);
  local stabmv;
  if opt.StableMixedVolume then (
    scanLines(line ->  
      if substring(0,21,line) == "stable mixed volume :" then (
        stabmv = value replace("stable mixed volume : ","",line);
        break
      ), outfile);
  );
  local result;
  if not opt.StartSystem then (
    if opt.StableMixedVolume then result = (mixvol, stabmv) else result = mixvol;
  )
  else (
    solsfile := startfile | ".sols";
    p := startSystemFromFile(startfile);
    execstr = PHCexe|" -z "|startfile|" "|solsfile;
    ret = run(execstr);
    if ret =!= 0 then
      error "error occurred while executing PHCpack command: phc -z";
    sols := parseSolutions(solsfile, ring ideal system);
    if opt.StableMixedVolume
      then result = (mixvol,stabmv,p,sols)
      else result = (mixvol,p,sols);
  ); 
  result
)

-------------------------------------------
------------NON ZERO FILTER----------------
-------------------------------------------

nonZeroFilter = method(TypicalValue => List)
nonZeroFilter (List,ZZ,RR) := (sols,k,tol) -> (
  -- IN: sols, solutions of a polynomial system;
  --     k, index to a coordinate of a solution
  --     tol, tolerance for the absolute value of k-th coordinate
  -- OUT: list of solutions in sols where the k-th coordinate
  --      is less than the tolerance.
  return select(sols,t->(not isCoordinateZero(t,k,tol)));
)

-------------------------------------------
--NUMERICAL IRREDUCIBLE DECOMPOSITION------
-------------------------------------------

numericalIrreducibleDecomposition=method(TypicalValue=>NumericalVariety, Options=>{StartDimension=>-1})
numericalIrreducibleDecomposition (List) := o -> (L) -> (
  --IN: an ideal, top dimension
  --OUT: a NumericalVariety
setRandomSeed(random ZZ);
startdim:=o.StartDimension;  
W:=cascade(L,StartDimension=>startdim);
witsets:=apply(keys W, i->if i!=0 then (factorWitnessSet((W#i)_0))#i else W#i);
numericalVariety(flatten witsets)  
  )

-----------------------------------
----- REFINING SOLUTIONS ----------
-----------------------------------

refineSolutions = method(TypicalValue=>List, Options => {Verbose => false})
refineSolutions (List,List,ZZ) := o-> (f,sols,dp) -> (
  -- IN: f, a polynomial system with complex coefficients;
  --     sols, list of points, initial approximations for the solutions of f;
  --     dp, number of decimal places in the working precision.
  -- OUT: a list of refined solutions.
  
  if not(class coefficientRing ring first f===ComplexField) then
    error "coefficient ring of system is not complex";
    
  PHCinputFile := temporaryFileName() | "PHCinput";
  PHCoutputFile := temporaryFileName() | "PHCoutput";
  PHCbatchFile := temporaryFileName() | "PHCbatch";
  PHCsessionFile := temporaryFileName() | "PHCsession";
  PHCsolutions := temporaryFileName() | "PHCsolutions";
  if o.Verbose then
    stdio << "writing input system to " << PHCinputFile << endl;
  systemToFile(f,PHCinputFile);
  if o.Verbose then
    stdio << "appending solutions to " << PHCinputFile << endl;
  R := ring first f;
  solutionsToFile(sols, R, PHCinputFile, Append=>true);
  if o.Verbose then
    stdio << "preparing input data for phc -v in " << PHCbatchFile << endl;
  s := concatenate("3\n",PHCinputFile);
  s = concatenate(s,"\n");
  s = concatenate(s,PHCoutputFile);
  s = concatenate(s,"\n3\n1.0E-");
  s = concatenate(s,toString(dp-4));  -- tolerance for correction term
  s = concatenate(s,"\n4\n1.0E-");
  s = concatenate(s,toString(dp+16));  -- tolerance for residual
  s = concatenate(s,"\n6\n");
  nit := ceiling(dp/10.0);
  s = concatenate(s,toString(nit));   -- number of Newton iterations
  s = concatenate(s,"\n7\n");
  s = concatenate(s,toString(dp));    -- decimal places in working precision
  s = concatenate(s,"\n0\n");
  bat := openOut PHCbatchFile;
  bat << s;
  close bat;
  -- stdio << "running phc -v, writing output to " << PHCsessionFile << endl;
  run(PHCexe|" -v < " | PHCbatchFile | " > " | PHCsessionFile);
  if o.Verbose then
    (  stdio << "using temporary file " << PHCoutputFile;
    stdio << " for storing refined solutions " << endl;
    stdio << "solutions in Maple format in " << PHCsolutions << endl
    );
  run(PHCexe|" -z " | PHCoutputFile | " " | PHCsolutions);
  b := ceiling(log_2(10^dp));
  result := parseSolutions(PHCsolutions,R,Bits=>b);
  result
)

----------------------------------
------ SOLVE SYSTEM --------------
----------------------------------

solveSystem = method(TypicalValue => List, Options => {Verbose => false})
solveSystem  List := List =>  o->system -> (
  -- IN:  system = list of polynomials with complex coeffiecients, 
  -- i.e. the system to solved 
  -- OUT: solutions to the system, a list of Points
  
  if instance(ring ideal system, FractionField) then
     error "ring is a fraction field, use solveRationalSystem";
    
  if not(class coefficientRing ring first system===ComplexField) then
    error "coefficient ring is not complex";
    
  filename := getFilename();
  if o.Verbose then
    stdio  << "using temporary files " << filename|"PHCinput" << " and " << filename|"PHCoutput" << endl;
  infile := filename|"PHCinput";
  outfile := filename|"PHCoutput";
  solnsfile := filename|"PHCsolns";
  R := ring ideal system;
  n := #system;
  if n < numgens R then
    error "the system is underdetermined, positive dimensional";
     
  -- add slack variables if needed (i.e. if system is overdetermined)
  if n > numgens R then (
    nSlacks := n - numgens R;
    slackVars := apply(nSlacks, i->getSymbol("OOOO"|toString i));
    newR := CC(monoid[gens R, slackVars]);
    rM := random(CC^n,CC^nSlacks);
    system = apply(#system, i->sub(system#i,newR)
      +(rM^{i}*transpose submatrix'(vars newR,toList(0..numgens R - 1)))_(0,0))
  ) else newR=R; 

  -- writing data to the corresponding files:    
  systemToFile(system,infile);
  
  -- launching blackbox solver:
  execstr := PHCexe|" -b " |infile|" "|outfile;
  ret := run(execstr);
  if ret =!= 0 then 
    error "error occurred while executing PHCpack command: phc -b";
  execstr = PHCexe|" -z "|infile|" " |solnsfile;
  ret = run(execstr);
  if ret =!= 0 then 
    error "error occurred while executing PHCpack command: phc -z";
  
  -- parse and output the solutions:
  result := parseSolutions(solnsfile, R);
  result
)

------------------------------------
------SOLVE RATIONAL SYSTEM--------
-----------------------------------

solveRationalSystem = method(TypicalValue => List, Options => {Verbose => false})
solveRationalSystem  List :=  o-> system -> (
  -- IN:  system = list of rational equations with complex coeffiecients, 
  -- i.e. the system to solved
  -- OUT: solutions to the system, a list of Points
  
  origRing := ring ideal system;
   
  --convert to laurent polynomial
  if instance(ring ideal system, FractionField) 
      then system = toLaurentPolynomial(system, getSymbol "LLLL");
    
  filename := getFilename();
  if o.Verbose then
    stdio  << "using temporary files " << filename|"PHCinput" << " and " << filename|"PHCoutput" << endl;
  infile := filename|"PHCinput";
  outfile := filename|"PHCoutput";
  solnsfile := filename|"PHCsolns";
  R := ring ideal system;
  vars R;
  n := #system;
  if n < numgens R then
    error "the system is underdetermined, positive dimensional";
  
  -- add slack variables if needed (i.e. if system is overdetermined)
  if n > numgens R then (
    nSlacks := n - numgens R;
    slackVars := apply(nSlacks, i->getSymbol("OOOO"|toString i));
    newR := QQ(monoid(gens R | slackVars, Inverses=>true, MonomialOrder=>RevLex));
    rM := random(QQ^n,QQ^nSlacks);
    system = apply(#system, i->sub(system#i,newR)
      +(rM^{i}*transpose submatrix'(vars newR,toList(0..numgens R - 1)))_(0,0))
  ) else newR=R;   
     
  -- writing data to the corresponding files:    
  systemToFile(system,infile);
  
  -- launching blackbox solver:
  execstr := PHCexe|" -b " |infile|" "|outfile;
  ret := run(execstr);
  if ret =!= 0 then 
    error "error occurred while executing PHCpack command: phc -b";
  execstr = PHCexe|" -z "|infile|" " |solnsfile;
  ret = run(execstr);
  if ret =!= 0 then 
    error "error occurred while executing PHCpack command: phc -z";
  
  -- parse and output the solutions:
  result := parseSolutions(solnsfile, QQ[gens origRing]);
  result
)

-----------------------------------
------ TO LAURENT POLYNOMIAL ------
-----------------------------------

toLaurentPolynomial = method(TypicalValue => List)
toLaurentPolynomial (List, Symbol) := (system, var) -> (
  -- IN: system (or a polynomial) which is rational 
  --     (i.e. lives in some field of fractions of a polynomial ring)
  --	 and a symbol to be used for new indexed variables (e.g. x)
  -- OUT: same system converted to a Laurent polynomial system,
  --      where denominators are replaced with new indexed variables.
  
  R := ring ideal system;
  P := R.baseRings_(#R.baseRings-1); 
  -- P is the polynomial ring whose field of fractions the system lives in
  
  counter := 0;
  scan(system, f-> (
    if instance(class f, FractionField) then
     --if f is already polynomial, do not do anything!
     --  if liftable(f,P) then --*
     --if it can be lifted to P, then do so and update the system
     --	 system = system-set{f} | {lift(f,P)} --*
     --  else --*
     (  
     -- add one new variable "var_counter", and define the 
     -- appropriate Laurent polynomial ring: 
          P = (coefficientRing P) monoid(flatten entries vars P | {var_counter},
	         Inverses=>true, MonomialOrder=>RevLex);
	    
     -- add the new Laurent polynomial to replace the rational equation:  
          newvar := P_(numgens P - 1);
	  system = system - set{f} | {sub(numerator(f),P)*newvar^(-1)};
	  system = system | {newvar - sub(denominator(f),P)}; 
	  counter = counter+1; 
       )   
     )
  );   

     -- For consistency, we make sure that everyone lives in the Laurent 
     -- polynomial ring. This is not necessary for PHCpack, 
     -- but for any further M2 calculations for the system, it is.  
  system=apply(system,f-> sub(f,P)); 
  system
)
-----------------------------------------------
------------  TOP WITNESS SET  ----------------
-----------------------------------------------
 
topWitnessSet = method( Options => {Verbose => false})
topWitnessSet (List,ZZ) := o->(system,dimension) -> (
  -- IN: system, a polynomial system;
  --      dimension, top dimension of the solution set.
  -- OUT: a witness set for the top dimensional component,
  --      a list of nonsolutions
  
  if o.Verbose then
    stdio << "... calling constructEmbedding ..." << endl;
  e := constructEmbedding(system,dimension);
  if o.Verbose then
    stdio << "... calling solveSystem ..." << endl;
  s := solveSystem(e);
  g := zeroFilter(s,#e-1,1.0e-10);
  ns := nonZeroFilter(s,#e-1,1.0e-10);
  if o.Verbose then
    stdio << "... constructing a witness set ... " << endl;
  w := witnessSet(ideal(take(e,{0,#e-dimension-1})),
                  ideal(take(e,{#e-dimension,#e-1})),g);
  return (w,ns);
)




----------------------------------
--------  TRACK PATHS  -----------
----------------------------------

trackPaths = method(TypicalValue => List, Options=>{gamma=>0, tDegree=>2,Verbose => false})
trackPaths (List,List,List) := List => o -> (T,S,Ssols) -> (
  -- IN: T, target system to be solved;
  --     S, start system with solutions in Ssols;
  --     Ssols, solutions at the start of the paths.
  -- OPT INTPUTS: gamma, constant for gamma trick
  --     tDegree, degree of continuation parameter      
  -- OUT: Tsols, solutions at the end of the paths.
  
  if not(class coefficientRing ring first T===ComplexField) then
    error "coefficient ring of target system is not complex";

  if not(class coefficientRing ring first S===ComplexField) then
    error "coefficient ring of start system is not complex";  
  
  R := ring first T;
  n := #T;
  targetfile := temporaryFileName() | "PHCtarget";
  startfile := temporaryFileName() | "PHCstart";
  outfile := temporaryFileName() | "PHCoutput";
  Ssolsfile := temporaryFileName() | "PHCstartsols";
  Tsolsfile := temporaryFileName() | "PHCtargetsols";
  batchfile := temporaryFileName() | "PHCbat";
  logfile := temporaryFileName() | "phc_session_log";
  if o.Verbose then
    stdio   << "using temporary files " << outfile << " and " << Tsolsfile << endl;
  
  if n < numgens R then error "the system is underdetermined";
  
  if n> numgens R then error "the system is overdetermined"; 
  
  -- writing data to the corresponding files
  systemToFile(T,targetfile);
  systemToFile(S,startfile);
  solutionsToFile(Ssols,R,Ssolsfile);	  
  
  -- making batch file
  bat := openOut batchfile;
  bat << targetfile << endl << outfile << endl <<"n"<< endl 
  << startfile << endl << Ssolsfile << endl;
  
  -- first menu with settings of the construction of the homotopy
     bat << "k" << endl << o.tDegree << endl; 
  if o.gamma != 0 then (
    bat << "a" << endl << realPart o.gamma << endl;
    bat << imaginaryPart o.gamma << endl;
  );
  bat << "0" << endl;
  -- second menu 
  bat << "0" << endl; -- exit for now
  -- third menu
  bat << "0" << endl; -- exit for now
  -- fourth menu
  bat << "0" << endl; -- exit for now
  close bat;
  run(PHCexe|" -p <"|batchfile|" >" |logfile);
  run(PHCexe|" -z "|outfile|" "|Tsolsfile);
  
  -- parse and output the solutions
  result := parseSolutions(Tsolsfile, R);
  if n > numgens R then (
    result = apply(result, s->(
      if any(drop(first s, numgens R), x->abs x > 0.01) 
      then error "slack value is nonzero";
      {take(first s, numgens R)}|drop(s,1)
    ));
    totalN := #result;
    scan(result, s->(
      if s#1#"mult">1 then error "mutiple root encountered";
      if s#1#"mult"<0 then error "negative mutiplicity";
    ));			 
    result = select(result, 
      s-> 
      max(s#0/abs)<10000 -- path failed and/or diverged
    );
    if PHCDBG>0 and #result < totalN 
    then  -- error "discarded!" 
    << "track[PHCpack]: discarded "<< 
    totalN-#result << " out of " << totalN << " solutions" << endl;
  );
  return result;
)


-----------------------------------------------
------------ ZERO FILTER -----------------------
-----------------------------------------------

zeroFilter = method(TypicalValue => List)
zeroFilter (List,ZZ,RR) := (sols,k,tol) -> (
  -- IN: sols, solutions of a polynomial system;
  --     k, index to a coordinate of a solution
  --     tol, tolerance for the absolute value of k-th coordinate
  -- OUT: list of solutions in sols where the k-th coordinate
  --      is less than the tolerance.
  return select(sols,t->isCoordinateZero(t,k,tol));
)



--##########################################################################--
-- DOCUMENTATION
--##########################################################################--

beginDocumentation()

load "./PHCpack/PHCpackDoc.m2";





--##########################################################################--
-- TESTS
--##########################################################################

-----------------------------------
-- cascade
-----------------------------------
TEST/// 
      R=CC[x11,x22,x21,x12,x23,x13,x14,x24]
      L={x11*x22-x21*x12,x12*x23-x22*x13,x13*x24-x23*x14}
      assert( # cascade L == 1 )--there is one component of dim.5.
///;

-----------------------------------
--constructEmbedding
-----------------------------------
TEST///
      R=CC[x,y,z]
      L={x^2+y^2, x+y+z}
      Emb=constructEmbedding(L,1)
      assert( # Emb == 4) -- there are four equations
      assert ( member(zz1, Emb) == true ) --one of the eqns is zz1
///;
-----------------------------------
--factorWitnessSet
-----------------------------------
TEST///
    R = CC[x,y];
    system = {x*y};
    (w, ns)=topWitnessSet(system,1);
    V=factorWitnessSet(w);
    assert ((# V#1) == 2)
///;

-----------------------------------
--isCoordinateZero
-----------------------------------
TEST///
     P=point({{0,1.0e-12}})
     assert (isCoordinateZero(P,1,1.0e-10) == true)
///;     


-----------------------------------
-- isWitnessSetMember
-----------------------------------
TEST/// 
    R = CC [x,y]
    system = {2*y+2*x, 4*y + 4*x}
    (W, ns)=topWitnessSet(system, 1)
    assert isWitnessSetMember(W,point{{0,0}})
///;

-----------------------------------
-- mixedVolume
-----------------------------------
TEST/// 
     R=CC[x,y,z] 
     S={y-x^2,z-x^3,x+y+z-1}
     m=mixedVolume(S) 
     assert(m==3) 
     R=CC[x,y,z]
     S1={y^3+z^2+3,z^2+x^4+x^4*z^2+4,x^4+y^3+x^4*y^3+5}
     M=mixedVolume(S1)
     assert(M==48)--testing output against by-hand calculation
     R=CC[x,y]
     S2={x^2+x*y+y^2+x+y+1,x^5+x^4*y+x^3*y^2+x^2*y^3+x*y^4+y^5} --another example
     M=mixedVolume(S2)
     assert(M==10)--testing output against by-hand calculation 
///;

-----------------------------------
-- nonZeroFilter
-----------------------------------
TEST/// 
     R = CC[x,y]; 
     f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
     fSols = solveSystem(f);
     nonzeroSols = nonZeroFilter(fSols,0,1.0e-10);
     assert(# nonzeroSols ==10) -- this is just complement of zeroFilter which is tested in more detail. Here we are just counting the number of solutions to make sure the method ran.
///;

-------------------------------------
--numericalIrreducibleDecomposition
-------------------------------------

TEST///
      R = CC [x,y]
      system={x^2*y}
      V=numericalIrreducibleDecomposition(system)
      assert (dim V == 1) --there are two components of dimension 1
      assert ((# V#1)==2)
///;      

-----------------------------------
-- refineSolutions
-----------------------------------
TEST/// 
      R = CC[x,y]; 
      S = {x^2 - 1/3, x*y - 1}; 
      roots = solveSystem(S);
      r0 = roots#0#Coordinates#1
      newRoots = refineSolutions(S,roots,64) --recall that solutions are of type Point. 
      --check if precision increased:
      assert(precision newRoots#0#Coordinates#1 > precision roots#0#Coordinates#1) 
      --check if input number of decimal places, 64, used correctly: 
      assert(precision newRoots#0#Coordinates#1 == ceiling(log_2(10^64)))
///;

-----------------------------------
-- solveRationalSystem
-----------------------------------
TEST///
     QQ[x,y,z];
     sys = {y-x^2, z-x^3, (x+y+z-1)/x};
     sols = solveRationalSystem(sys);
     assert(# sols == 3); --there are 3 solutions
     real = realPoints(sols);
     assert(# real ==1); --one solution is real
///;

-----------------------------------
-- solveSystem
-----------------------------------
TEST/// 
     R=CC[x,y,z]
     S={x^2-y*z-3,y^2-x*z-4,z^2-x*y-5}
     L=solveSystem(S)
     n=# L
     assert(n==2) 
     sol1={11/6.,-1/6.,-13/6.}
     sol2={-11/6.,1/6.,13/6.}
     assert((abs((sol1-L_0#Coordinates)_0)<.00000000001 and abs((sol1-L_0#Coordinates)_1)<.00000000001 and abs((sol1-L_0#Coordinates)_2)<.00000000001) or 
     (abs((sol1-L_1#Coordinates)_0)<.00000000001 and abs((sol1-L_1#Coordinates)_1)<.00000000001 and abs((sol1-L_1#Coordinates)_2)<.00000000001))
     assert((abs((sol2-L_0#Coordinates)_0)<.00000000001 and abs((sol2-L_0#Coordinates)_1)<.00000000001 and abs((sol2-L_0#Coordinates)_2)<.00000000001) or 
     (abs((sol2-L_1#Coordinates)_0)<.00000000001 and abs((sol2-L_1#Coordinates)_1)<.00000000001 and abs((sol2-L_1#Coordinates)_2)<.00000000001))
///;


-----------------------------------
-- toLaurentPolynomial
-----------------------------------
TEST///
     QQ[x,y,z];
     sys = {y-x^2, z-x^3, (x+y+z-1)/x};
     convertedSys = toLaurentPolynomial(sys,w);
     R=ring ideal convertedSys 
     assert(isPolynomialRing(R)) --make sure it is a poly ring
     O=options(R); 
     assert(O#Inverses) -- want to make sure this is a Laurent ring
///;

-----------------------------------
--topWitnessSet
-----------------------------------
TEST///
    R = CC[x,y];
    system = {x*y};
    (w, ns)=topWitnessSet(system,1);
    assert (dim w == 1)
    assert (degree w == 2)
///;    
    
-----------------------------------
-- trackPaths
-----------------------------------
TEST/// 
     R = CC[x,y]; 
     f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
     (m,q,qsols) = mixedVolume(f,StartSystem=>true);
     fsols = trackPaths(f,q,qsols)
     assert(# fsols == 8)
///;

-----------------------------------
-- zeroFilter
-----------------------------------
TEST/// 
     R = CC[x,y]; 
     f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
     fSols = solveSystem(f);
     zeroSols = zeroFilter(fSols,1,1.0e-10);
     assert(  max \\ abs \ flatten ( sort {zeroSols_0#Coordinates,zeroSols_1#Coordinates} - {{ -1, 0}, {1, 0}} ) < 1e-17 )
///;

--##########################################################################--
end   -- terminate reading ... 
--##########################################################################--


restart
installPackage("PHCpack",RemakeAllDocumentation=>true)
installPackage ("PHCpack",RerunExamples=>true)
check "PHCpack"
viewHelp PHCpack
--many errors seem to be due to the fact some things are not exported:
--error: mutable unexported unset symbol(s) in package PHCpack: 'generalEquations', 'NumericalVariety', 'numericalVariety', 'IsIrreducible'
/Users/sxp61/m2svn/trunk/M2/Macaulay2/packages/PHCpack.m2:329:12-329:28: here is the first use of 'generalEquations'
/Users/sxp61/m2svn/trunk/M2/Macaulay2/packages/PHCpack.m2:401:34-401:50: here is the first use of 'NumericalVariety'
/Users/sxp61/m2svn/trunk/M2/Macaulay2/packages/PHCpack.m2:488:3-488:19: here is the first use of 'numericalVariety'
/Users/sxp61/m2svn/trunk/M2/Macaulay2/packages/PHCpack.m2:588:26-588:39: here is the first use of 'IsIrreducible'

restart
needsPackage "PHCpack";
var0 = {x_11,x_12,x_16,x_22,x_23,x_33,x_34,x_44,x_45,x_55,x_56,x_66,y_13,y_14,y_15,
  y_24,y_25,y_26,y_35,y_36,y_46}; 
QQ[var0];
rationalSystem = {
  (22/3)*x_11+(8/7)*x_12+2*x_16-1,            x_23*y_13+(22/3)*x_12+(8/7)*x_22, 	
  x_33*y_13+x_34*y_14+(8/7)*x_23,             x_34*y_13+x_44*y_14+x_45*y_15, 
  x_45*y_14+x_55*y_15+2*x_56,                 x_56*y_15+(22/3)*x_16+2*x_66,
  (8/7)*x_12+(14/11)*x_22+(12/5)*x_23-1,      x_34*y_24+(14/11)*x_23+(12/5)*x_33, 
  x_44*y_24+x_45*y_25+(12/5)*x_34,            x_45*y_24+x_55*y_25+x_56*y_26, 
  (12/5)*x_23+(28/51)*x_33+(102/144)*x_34-1,  x_56*y_25+x_66*y_26+(8/7)*x_16,        
  x_45*y_35+(28/51)*x_34+(102/144)*x_44,      x_55*y_35+x_56*y_36+(102/144)*x_45,
  (102/144)*x_34+(205/162)*x_44+(3/2)*x_45-1, x_16*y_13+x_56*y_35+x_66*y_36,         
  x_56*y_46+(205/162)*x_45+(3/2)*x_55,        x_16*y_14+x_66*y_46+(3/2)*x_56,
  (3/2)*x_45+(517/784)*x_55+(8/3)*x_56-1,     x_16*y_15+(517/784)*x_56+(8/3)*x_66,
  2*x_16+(8/3)*x_56+(29/196)*x_66-1};
system = (sub(ideal rationalSystem, CC[var0]))_*
solutions = solveSystem system; 
# solutions
solutions_0 

newSystem = {
  (22531/300)*x_11+(821/70)*x_12+(4507/210)*x_16-1, 
  x_23*y_13+(22531/300)*x_12+(821/70)*x_22,
  x_33*y_13+x_34*y_14+(821/70)*x_23, x_34*y_13+x_44*y_14+x_45*y_15,
  x_45*y_14+x_55*y_15+(4507/210)*x_56,
  x_56*y_15+(22531/300)*x_16+(4507/210)*x_66,
  (821/70)*x_12+(140953/11025)*x_22+(12325/504)*x_23-1,
  x_34*y_24+(140953/11025)*x_23+(12325/504)*x_33,
  x_44*y_24+x_45*y_25+(12325/504)*x_34, 
  x_45*y_24+x_55*y_25+x_56*y_26,
  x_56*y_25+x_66*y_26+(821/70)*x_16,
  (12325/504)*x_23+(282013/5184)*x_33+(10231/1440)*x_34-1,
  x_45*y_35+(282013/5184)*x_34+(10231/1440)*x_44,
  x_55*y_35+x_56*y_36+(10231/1440)*x_45, 
  x_16*y_13+x_56*y_35+x_66*y_36,
  (10231/1440)*x_34+(205697/16200)*x_44+(30529/2520)*x_45-1,
  x_56*y_46+(205697/16200)*x_45+(30529/2520)*x_55,
  x_16*y_14+x_66*y_46+(30529/2520)*x_56,
  (30529/2520)*x_45+(5175321/78400)*x_55+(897/35)*x_56-1,
  x_16*y_15+(5175321/78400)*x_56+(897/35)*x_66,
  (4507/210)*x_16+(897/35)*x_56+(293581/19600)*x_66-1};
newSolutions = solveSystem newSystem;
# newSolutions
smallSolution = zeroFilter(newSolutions, 11, 1.0e-18)
smallerSolution = refineSolutions(newSystem, smallSolution, 64)

mixedVolume system
time degree ideal rationalSystem

var1 = {x11, x22, x21, x12, x23, x13, x14, x24};
R = QQ[var1];
rationalSystem = { x11*x22-x21*x12, x12*x23-x22*x13, x13*x24-x23*x14};
system = (sub(ideal rationalSystem, CC[var1]))_*
V = numericalIrreducibleDecomposition system
WitSets = V#5;
w = first WitSets
w#IsIrreducible

R = ring rationalSystem_0
PD = primaryDecomposition ideal rationalSystem
for I in PD list << "(dim=" << dim I << ", deg=" << degree I << ") " 


