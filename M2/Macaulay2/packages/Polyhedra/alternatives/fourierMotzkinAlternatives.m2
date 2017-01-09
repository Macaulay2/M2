-- Get a temporary filename.
getFilename = () -> (
     filename := temporaryFileName();
     while fileExists(filename) or fileExists(filename|".ine") or fileExists(filename|".out") do filename = temporaryFileName();
     filename)


-- Method for creating the input files for glrs, lcdd, lcdd_gmp, lrs.
-- Matrices are the same as in fourierMotzkin. 
writeFMInput = method()
writeFMInput (File, Matrix) := (F,A) ->(
   F << "polytope" << endl;
   F << "H-representation" << endl;
   F << "begin" << endl;
   writeMatrixToLrsInputFile(F, A);
   F << "end" << endl;
)

writeFMInput (File, Matrix, Matrix) := (F,C,B) -> (
   F << "polytope" << endl;
   F << "H-representation" << endl;
   F << "linearity " << numColumns B << " ";
   apply(numColumns B, l-> F << l+1 << " ");
   F << endl;
   F << "begin" << endl;
   A := B|C;
   writeMatrixToLrsInputFile(F, A);
   F << "end" << endl;
)

writeMatrixToLrsInputFile = method()
writeMatrixToLrsInputFile(File, Matrix) := (F, A) -> (
   if debugLevel > 4 then << "lrs input is " << A << endl;
   m := numColumns A;
   n := numRows A;
   if m==0 then (
      A = map(ZZ^n, ZZ^1, 0);
      m = 1;
   );
   A = matrix({toList(m:0)})||A;
   F << m << " " << n+1 << " integer" << endl;
   L := entries transpose A;
   for i from 0 to m-1 do (
      for j from 0 to n do (
         if (class L#i#j)===QQ then F << numerator L#i#j << "/" << denominator L#i#j << " "
         else F << L#i#j << " ";
      );
      F << endl;
   );
)


preMatrixFromStringList = method()
preMatrixFromStringList(List, List) := (L, indices)->(
   apply(indices, 
      i -> (
         line := L#i;
         line = select(separateRegexp("[[:space:]]", line),m-> m=!="");
         -- line = apply(line, e-> value replace("E\\+?","e",e))
         -- line = primitive toZZ apply(line, e-> lift(promote(value replace("E\\+?","e",e),RR), QQ))
         line = primitive toZZ apply(line, e-> promote(value replace("E\\+?","e",e), QQ))
      )
   )
)


getSaneOutputPart = method()
getSaneOutputPart List := L -> (
   start := 0;
   if L#0 === "" then start = start + 1;
   start = start + 1;
   while(match("Input linearity", L#start)) do start = start + 1;
   end := start;
   while(not match("end", L#end)) do end = end+1;
   (start, end)
)


-- Method for parsing the output files of lcdd and lrs.
readFMOutput = method()
readFMOutput String := (filename) -> (
   L := lines get filename;
   (start, end) := getSaneOutputPart L;
   if debugLevel > 4 then << "output: " << L << endl << "end lrs output" << endl;
   hasLineality := match("linearity",L#(start+1));
   local lin;
   local nrows;
   local ncols;
   local linComp;
   if hasLineality then (
      lin = apply(drop(select(separateRegexp("[[:space:]]", L#(start+1)),m-> m=!=""),2), l-> (value l)-1 + start+4);    
      nrows = (end-start) - 4;
      ncols = L#(start+3);
      linComp = set ((start+4)..(end-1)) - lin;
      linComp = elements linComp;
   ) else (
      lin = {};
      nrows = (end-start) - 3;
      ncols = L#(start+2);
      linComp = toList ((start+3)..(end-1));
   );
   ncols = select(separateRegexp("[[:space:]]", ncols),m-> m=!="");
   ncols = value(ncols#1);
   preRays := preMatrixFromStringList(L, linComp);
   preRaysIndices := select(#preRays, pr -> preRays#pr#0 == 0);
   preRays = apply(preRaysIndices, pr -> drop(preRays#pr, 1));
   rays := if #preRays == 0 then map(ZZ^0,ZZ^(ncols-1),0) else matrix preRays;
   lineality := if #lin == 0 then map(ZZ^0,ZZ^(ncols-1),0) else (
      matrix apply(preMatrixFromStringList(L, lin), pr -> drop(pr, 1))
   );
   (sort transpose rays, sort transpose lineality)
)


runFMAlternativeOnInput = method()
runFMAlternativeOnInput(String, List) := (command, inputMatrices) -> (
   filename := getFilename();
   if debugLevel > 1 then << "using temporary file name " << filename << endl;
   if debugLevel > 2 then << "Running FM command " << command << endl;
   F := openOut(filename|".ine");
   input := prepend(F, inputMatrices);
   writeFMInput(toSequence input);
   close F;
   execstr := command | " " |rootPath | filename | ".ine " | rootPath | filename | ".ext > " | rootPath | filename |".txt 2>&1";
   if run execstr =!= 0 then error( "-- Error with lrs, for details see " | rootPath | filename | ".txt.");
   readFMOutput (filename | ".ext")
)



-- Method for calling lrs.
-- Creates an input file from the given matrices, applies lrs and parses output.
lrs = new MutableHashTable
lrs#fourierMotzkin = method()
lrs#fourierMotzkin Matrix := Matrix => A ->(
   input := {-A};
   runFMAlternativeOnInput("lrs", input)
)
lrs#fourierMotzkin (Matrix,Matrix) := Matrix => (A,B) ->(
   if B==0 then return lrs#fourierMotzkin A 
   else(
      input := {-A, B};
      runFMAlternativeOnInput("lrs", input)
   )
)



-- Method for calling lcdd
-- Creates an input file from the given matrices, applies lcdd and parses output.
lcdd = new MutableHashTable
lcdd#fourierMotzkin = method()
lcdd#fourierMotzkin Matrix := Matrix => A ->(
   input := {-A};
   runFMAlternativeOnInput("lcdd", input)
)
lcdd#fourierMotzkin (Matrix,Matrix) := Matrix => (A,B) ->(
   if B==0 then return lcdd#fourierMotzkin A 
   else(
      input := {-A, B};
      runFMAlternativeOnInput("lcdd", input)
   )
)


-- Method for calling lcdd_gmp
-- Creates an input file from the given matrices, applies lcdd_gmp and parses output.
lcddGmp = new MutableHashTable
lcddGmp#fourierMotzkin = method()
lcddGmp#fourierMotzkin Matrix := Matrix => A ->(
   input := {-A};
   runFMAlternativeOnInput("lcdd_gmp", input)
)
lcddGmp#fourierMotzkin (Matrix,Matrix) := Matrix => (A,B) ->(
   if B==0 then return lcddGmp#fourierMotzkin A 
   else(
      input := {-A, B};
      runFMAlternativeOnInput("lcdd_gmp", input)
   )
)
