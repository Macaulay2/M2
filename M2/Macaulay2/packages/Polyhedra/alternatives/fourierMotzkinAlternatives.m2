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
preMatrixFromStringList List := (L)->(
   L = apply(L,l -> select(separateRegexp("[[:space:]]", l),m-> m=!=""));
   L = apply(L,l -> apply(l,e->replace("E\\+?","e",e)));
   L = apply(L,l -> apply(l,e->value(e)));
   L = apply(L,l -> apply(l, e -> lift(promote(e, RR), QQ)))
)


sanitizeFMOutput = method()
sanitizeFMOutput String := filename -> (
   L := lines get filename;
   if L#0 === "" then L = drop(L,1);
   L = drop(L,1);
   while(match("Input linearity", L#0)) do L = drop(L,1);
   i := 0;
   while(not match("end", L#i)) do i = i+1;
   L = L_(toList(0..i))
)


-- Method for parsing the output files of lcdd and lrs.
readFMOutput = method()
readFMOutput String := (filename) -> (
   L := sanitizeFMOutput filename;
   if debugLevel > 4 then << "output: " << L << endl << "end lrs output" << endl;
   hasLineality := match("linearity",L#1);
   local lin;
   local nrows;
   local ncols;
   local linComp;
   local preMat;
   if hasLineality then (
      lin = apply(drop(select(separateRegexp("[[:space:]]", L#1),m-> m=!=""),2), l-> (value l)-1);    
      preMat = L_{4 .. #L-2};
      nrows = (#L-2) - 4;
      ncols = L#3;
      linComp = set (0..nrows) - lin;
      linComp = elements linComp;
   ) else (
      preMat = L_{3 .. #L-2};
      lin = {};
      nrows = (#L-2) - 3;
      ncols = L#2;
      linComp = toList (0..nrows);
   );
   ncols = select(separateRegexp("[[:space:]]", ncols),m-> m=!="");
   ncols = value(ncols#1);
   local preMat;
   preRays := preMatrixFromStringList(preMat_linComp);
   preRays = select(preRays, pr -> pr#0 == 0);
   preRays = apply(preRays, pr -> drop(pr, 1));
   rays := if #preRays == 0 then map(ZZ^0,ZZ^(ncols-1),0) else matrix preRays;
   lineality := if #lin == 0 then map(ZZ^0,ZZ^(ncols-1),0) else (
      matrix apply(preMatrixFromStringList(preMat_lin), pr -> drop(pr, 1))
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
   apply(readFMOutput (filename | ".ext"), b-> if b!=0 then transpose matrix apply(entries transpose b, e-> primitive toZZ e) else b)
)


-- divides a list of integers by their gcd.
primitive = method();
primitive List := List => L -> (
   -- finding greatest common divisor
   n := #L-1;
   g := abs(L#n);
   while (n > 0) do (
   n = n-1;
   g = gcd(g, L#n);
   if g === 1 then n = 0);
   if g === 1 then L
   else apply(L, i -> i // g)
)



-- Converts a list of 'QQ' to 'ZZ' by multiplying by a common denominator
toZZ = method();
toZZ List := List => L -> (
   -- finding common denominator
   d := apply(L, e -> denominator e);
   l := lcm d;
   apply(L, e -> (numerator(l*e)))
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
