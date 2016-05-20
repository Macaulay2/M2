-- an interface to the Littlewood-Richardson homotopies in PHCpack.
-- Note that this package needs version 1.6.1 of PHCpack.m2.

export{"LRrule", "LRtriple", "parseTriplet", "wrapTriplet", "LRcheater"}

debug needsPackage "PHCpack"

LRruleIn = method();
LRruleIn(ZZ,ZZ,Matrix) := (a,n,m) -> (
-- 
-- DESCRIPTION :
--   Prepares the input for phc -e option #4 to resolve a Schubert
--   intersection condition, for example: [2 4 6]^3.
--
-- ON ENTRY :
--   a         should be 4 for root count, 5 for solutions;
--   n         ambient dimension;
--   m         matrix with in rows the intersection conditions,
--             the first element of each row is the number of times
--             the intersection bracket must be taken.
-- 
-- ON RETURN :
--   s         a string with input for phc -e, i.e.: if s is place
--             in the file "input" then phc -e < input will work.
--
   s := concatenate(toString(a),"\n");
   nr := numgens target m;
   nc := numgens source m;
   s = concatenate(s,toString(n),"\n");
   for i from 0 to nr-1 do
   (
       s = concatenate(s,"[ ");
       for j from 1 to nc-1 do s = concatenate(s,toString(m_(i,j))," ");
       if m_(i,0) > 1 then
          s = concatenate(s,"]^",toString(m_(i,0)))
       else
          s = concatenate(s,"]");
       if i < nr-1 then s = concatenate(s,"*");
   );
   s = concatenate(s,";");
   s
);

dataToFile = method()
dataToFile(String,String) := (data,name) -> (
--
-- DESCRIPTION :
--   Writes the all characters in the string data to the
--   file with the given name for the LRrule computation.
--
   file := openOut name;
   file << data << endl;
   close file;
);

lastLine = method()
lastLine(String) := (name) -> (
--
-- DESCRIPTION :
--   Returns a string with the contents of the last line
--   ond file with the given name.
--
   s := get name;
   L := lines(s);
   n := #L-1;
   result := L_n;
   result
);

LRrule = method();
LRrule(ZZ,Matrix) := (n,m) -> (
-- 
-- DESCRIPTION :
--   Returns the intersection condition and its result.
--
-- ON ENTRY :
--   n         ambient dimension;
--   m         matrix with in rows the intersection conditions,
--             the first element of each row is the number of times
--             the intersection bracket must be taken.
-- 
-- ON RETURN :
--   s         a string with an equation, with at the left the
--             intersection condition and at the right the result.
--
   d := LRruleIn(4,n,m);
   -- stdio << "the input data for phc -e : " << endl <<  d;
   PHCinputFile := temporaryFileName() | "PHCinput";
   PHCoutputFile := temporaryFileName() | "PHCoutput";
   stdio << endl << "writing data to file " << PHCinputFile << endl;
   dataToFile(d,PHCinputFile);
   stdio << "running phc -e, writing output to " << PHCoutputFile << endl;
   run("phc -e < " | PHCinputFile | " > " | PHCoutputFile);
   stdio << "opening output file " << PHCoutputFile << endl;
   outcome := lastLine(PHCoutputFile);
   s := substring(4,#d-5,d);
   s = concatenate(s,outcome);
   s
);

ringFromString = method();
ringFromString(String) := (p) -> (
--
-- DESCRIPTION :
--   Given in p a polynomial system in PHCpack format,
--   returns the polynomial ring.
--
   -- stdio << "the input data for phc -o : " << endl <<  p;
   PHCinpFile := temporaryFileName() | "PHCinp";
   PHCoutFile := temporaryFileName() | "PHCout";
   PHCsesFile := temporaryFileName() | "PHCses";
   stdio << endl << "writing data to file " << PHCinpFile << endl;
   dataToFile(p,PHCinpFile);
   stdio << "running phc -o, writing output to " << PHCoutFile << endl;
   run("phc -o " | PHCinpFile | " " | PHCoutFile | " > " | PHCsesFile);
   s := get PHCoutFile;
   -- stdio << "the string of symbols :" << s;
   if s_0 == " " then (
      s = substring(1, s)
   );
   L := separate(" ", s);
   -- stdio << "the list of symbols as strings" << L;
   v := apply(L, x -> value(x));
   -- stdio << "the list of symbols as strings" << v;
   CC_53[v]
);

systemFromString = method();
systemFromString (String, Ring) := (p, R) -> (
--
-- DESCRIPTION :
--   Given in the string p a polynomial system in PHCpack format
--   and a polynomial ring with as symbols the names of the variables
--   used in the string p, the polynomial system in p is returned as
--   a proper polynomial system in Macaulay2.   
--
   PHCinpFile := temporaryFileName() | "PHCipt";
   stdio << endl << "writing data to file " << PHCinpFile << endl;
   dataToFile(p,PHCinpFile);
   use R;
   return systemFromFile(PHCinpFile);
);

SchubertSystemFromFile = method();
SchubertSystemFromFile(String) := (name) -> (
--
-- DESCRIPTION :
--   Given the name of the output file of a run of phc -e with option #5,
--   this method returns a string with the polynomial system solved.
--
-- ON ENTRY :
--   name      name of the output file of a run of phc -e with option #5,
--             must contain the banner "POLYNOMIAL SYSTEM" followed by
--             the polynomial equations solved.
--            
-- ON RETURN :
--   (f,p,s)   a sequence with flag and polynomial system, and solutions,
--   f         random complex coordinates of the fixed flag,
--   p         the solved polynomial equations,
--   s         solutions to the polynomials in string format.
--
   data := get name;
   L := lines(data);
   nf := position(L,i->i=="THE FIXED FLAGS :");
   np := position(L,i->i=="THE POLYNOMIAL SYSTEM :");
   K := take(L,{np,#L-1});
   ns := np + position(K,i->i=="THE SOLUTIONS :");
   f := concatenate(L_(nf+1),"\n");
   for i from nf+2 to np-1 do f = concatenate(f,L_i,"\n");
   p := concatenate(L_(np+1),"\n");
   for i from np+2 to ns-1 do p = concatenate(p,L_i,"\n");
   s := concatenate(L_(ns+1),"\n");
   for i from ns+2 to #L-1 do s = concatenate(s,L_i,"\n");
   result := (f, p, s);
   result
);

LRtriple = method();
LRtriple(ZZ,Matrix) := (n,m) -> (
--
-- DESCRIPTION :
--   Solves one checker game for a triple Schubert intersection.
--
-- ON ENTRY :
--   n         ambient dimension;
--   m         matrix with in rows the intersection conditions,
--             the first element of each row is the number of times
--             the intersection bracket must be taken.
-- 
-- ON RETURN :
--   (f,r,p,s) a sequence with the result of the Schubert problem:
--   r         the polynomial ring for the symbols of the variables
--             representing the solutions in the matrix representations,
--   f         a string representation of a fixed flag,
--   p         the polynomial system solved,
--   s         a string with solutions to the polynomial system.
--
   d := LRruleIn(5,n,m);  -- option 5 of phc -e
   PHCinputFile := temporaryFileName() | "PHCip";
   PHCoutputFile := temporaryFileName() | "PHCout";
   PHCsessionFile := temporaryFileName() | "PHCses";
   PHCsolutions := temporaryFileName() | "PHCsolutions";
   d = concatenate(d,"\n0\n");  -- solve a generic instance for random flags
   d = concatenate(d,"0\n");  -- generate random flags
   d = concatenate(d,PHCoutputFile,"\n");
   d = concatenate(d,"y\n");  -- monitor Littlewood-Richardson homotopies
   d = concatenate(d,"0\n");  -- do not change default continuation parameters
   d = concatenate(d,"0\n");  -- no intermediate output during continuation
   stdio << "the input data for phc -e : " << endl <<  d;
   stdio << endl << "writing data to file " << PHCinputFile << endl;
   dataToFile(d,PHCinputFile);
   stdio << "running phc -e, session output to " << PHCsessionFile << endl;
   stdio << "                writing output to " << PHCoutputFile << endl;
   run("phc -e < " | PHCinputFile | " > " | PHCsessionFile);
   run("phc -z " | PHCoutputFile | " " | PHCsolutions);
   stdio << "opening output file " << PHCsolutions << endl;
  -- stdio << endl << "extracting fixed flags, polynomial system, solutions";
  -- stdio << endl;
   fps := SchubertSystemFromFile(PHCoutputFile);
   result := (fps_0,fps_1,fps_2);
   result
);

parseFlag = method();
parseFlag(String) := (f) -> (
--
-- DESCRIPTION :
--   Returns the matrix stored as a string in f
--   as a proper matrix object.
--
   s := replace("E","e",f);
   s = replace("e\\+","e",s); 
   L := lines(s);
   i := 0;
   while i < #L do (
      local line;     -- one line in the flag
      local newline;  -- line with all double spaces removes
      local listline; -- the line as a list
      local stop;     -- stop condition
      local vals;     -- real and imaginary values 
      local cvals;    -- values of the flag as complex numbers
      line = L_i;
      if #line > 0 then (
         stop = false;     -- first replace double by single spaces
         newline = line;
         while not stop do (
            newline = replace("  "," ",line);
            stop = (newline == line);
            line = newline;
         );
         if newline_0 == " " then (
            newline = substring(1, newline); -- bite off leading space
         );
         listline = separate(" ",newline);
         vals = apply(listline, x-> value(x));
         cvals = for i in 0..(#vals-1) list (
            if odd i then continue; vals_i + vals_(i+1)*ii
         );
         if i == 0 then values := {cvals};
         if i > 0 then values = values | {cvals};
      );
      i = i + 1;
   );
   matrix(values)
);

parseTriplet = method();
parseTriplet(String,String,String) := (f,p,s) -> (
--
-- DESCRIPTION :
--   Returns the polynomial ring of the variables, the polynomial system,
--   the solutions and the fixed flag as Macaulay objects, when given
--   the string representations of the flag f, the polynomial system p,
--   and the list of solutions in s.
--
   R := ringFromString(p);
   spR := systemFromString(p, R);
   s = concatenate("THE SOLUTIONS :\n", s);
   PHCiptsolsFile := temporaryFileName() | "PHCsols";
   PHCoptsolsFile := temporaryFileName() | "PHCsols";
   stdio << "writing solutions to " << PHCiptsolsFile;
   dataToFile(s,PHCiptsolsFile);
   stdio << "running phc -z ..." << endl;
   run(PHCexe|" -z " | PHCiptsolsFile | " "| PHCoptsolsFile);
   sols := parseSolutions(PHCoptsolsFile, R);
   result := (R, spR, sols, parseFlag(f));
   result
);

wrapTriplet = method();
wrapTriplet(String,String,String) := (f,p,s) -> (
--
-- DESCRIPTION :
--   Wraps the triplet of strings: fixed flag f, polynomial system p,
--   and solutions s into one string suitable for parsing by phc -e.
--
   result := concatenate("THE FIXED FLAGS :\n",f);
   result = concatenate(result,"THE POLYNOMIAL SYSTEM :\n",p);
   result = concatenate(result,"THE SOLUTIONS :\n",s);
   result
);

cheaterInputFile = method();
cheaterInputFile(String) := (data) -> (
--
-- DESCRIPTION :
--   Generates a file name and writes the data in the string to it.
--   Returns the name of this input file.
-- 
-- ON ENTRY :
--   w         the outcome of LRtriple(n,m), wrapped into a string.
--
-- ON RETURN :
--   name      name of the file that contains the data.
--
   name := temporaryFileName() | "PHCcheaterInput";
   stdio << "writing start data to " << name << endl;
   file := openOut name;
   file << data << endl;
   close file;
   name
);

LRcheater = method();
LRcheater(ZZ,Matrix,String) := (n,m,w) -> (
--
-- DESCRIPTION :
--   Runs a cheater's homotopy from a generic instance of a Schubert
--   triple intersection to a real instance.
--
-- ON ENTRY :
--   n         ambient dimension;
--   m         matrix with in rows the intersection conditions,
--             the first element of each row is the number of times
--             the intersection bracket must be taken,
--   w         the outcome of LRtriple(n,m), wrapped into string.
--
-- ON RETURN :
--   t         a real triple Schubert intersection problem.
--   
   PHCinputCheater := cheaterInputFile(w);
   PHCoutputCheater := temporaryFileName() | "PHCoutputCheater";
   PHCinputSession := temporaryFileName() | "PHCinputSession";
   PHCsessionCheater := temporaryFileName() | "PHCsessionCheater";
   -- PHCsolutionsCheater := temporaryFileName() | "PHCsolutionsCheater";
   d := LRruleIn(5,n,m);        -- option 5 of phc -e
   d = concatenate(d,"\n1\n");  -- run Cheater's homotopy
   d = concatenate(d,PHCinputCheater,"\n");
   d = concatenate(d,"y\n");    -- generate real flags
   d = concatenate(d,PHCoutputCheater,"\n");
   -- stdio << "the input data for phc -e : " << endl <<  d;
   stdio << endl << "writing data to file " << PHCinputSession << endl;
   dataToFile(d,PHCinputSession);
   stdio << "running phc -e, session output to " << PHCsessionCheater << endl;
   stdio << "                writing output to " << PHCoutputCheater << endl;
   run("phc -e < " | PHCinputSession | " > " | PHCsessionCheater);
   -- run("phc -z " | PHCoutputCheater | " " | PHCsolutionsCheater);
   -- stdio << "opening output file " << PHCsolutionsCheater << endl;
   -- stdio << endl << "extracting fixed flags, polynomial system, solutions";
   -- stdio << endl;
   fp := SchubertSystemFromFile(PHCoutputCheater);
   -- s := get PHCsolutionsCheater;
   result := (fp_0,fp_1,fp_2);
   result
);

