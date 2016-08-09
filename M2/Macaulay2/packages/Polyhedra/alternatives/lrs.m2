-- Get a temporary filename.
getFilename = () -> (
     filename := temporaryFileName();
     while fileExists(filename) or fileExists(filename|".ine") or fileExists(filename|".out") do filename = temporaryFileName();
     filename)


-- Method for creating the input files for glrs, lcdd, lcdd_gmp, lrs.
-- Matrices are the same as in fourierMotzkin. 
writeLrsInput = method()
writeLrsInput (File, Matrix) := (F,A) ->(
   F << "polytope" << endl;
   F << "H-representation" << endl;
   F << "begin" << endl;
   writeMatrixToLrsInputFile(F, A);
   F << "end" << endl;
)

writeLrsInput (File, Matrix, Matrix) := (F,C,B) -> (
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
   if debugLevel > 2 then << "lrs input is " << A << endl;
   m := numColumns A;
   n := numRows A;
   if m==0 then (
      A = map(ZZ^n, ZZ^1, 0);
      m = 1;
   );
   A = matrix({toList(m:0)})||A;
   F << m << " " << n+1 << " rational" << endl;
   L := entries transpose A;
   for i from 0 to m-1 do (
      for j from 0 to n do (
         if (class L#i#j)===QQ then F << numerator L#i#j << "/" << denominator L#i#j << " "
         else F << L#i#j << " ";
      );
      F << endl;
   );
)

-- Method for parsing the output files of lcdd and lrs.
readLrsInput = method()
readLrsInput String := (filename) -> (
   if debugLevel > 3 then << get filename << endl;
   L := separateRegexp("linearity|begin|end", get filename);
   L = select(L, l -> not match("converted to inequality", l));
   if debugLevel > 3 then << "lrs output: " << L << endl << "end lrs output" << endl;
   if #L<3 then error "-- lrs or cdd failed to compute the dual cone.";
   local m;
   local M;
   if #L==3 then (
      L = L#1;
      M = select(separateRegexp("[[:space:]]", L), m->m=!="");
      m = value( M#1);
      R := select(pack_m apply(drop(M,3), m-> lift(promote(value replace("E\\+?","e",m),RR),QQ)),i-> i#0==0);
      local A;
      if #R != 0 then A = sort transpose matrix apply(R, l->drop(l,1))
      else A = map(ZZ^(m-1), ZZ^0, 0);
      (A, map(ZZ^(numRows A),ZZ^0,0))
   ) else (
      lin := apply(drop(select(separateRegexp("[[:space:]]", L#1),m-> m=!=""),1), l-> (value l)-1);
      M = select(separateRegexp("[[:space:]]", L#2), m->m=!="");
      m = value( M#1);
      mat :=  pack_m apply(drop(M,3), o-> lift(promote(value replace("E\\+?","e",o),RR),QQ));
      linearity := sort transpose matrix apply(mat_lin, l-> drop(l,1));
      r := select(toList(0..#mat-1), n-> not member(n,lin));
      preRays := select(mat_r, l-> l#0==0);
      local rays;
      if #preRays > 0 then rays = sort transpose matrix apply(preRays,l-> drop(l,1))
      else rays = map(ZZ^(m-1),ZZ^0,0);
      (rays, linearity)
   )
)

lrs = new MutableHashTable
-- Method for calling lrs.
-- Creates an input file from the given matrices, applies lrs and parses output.
lrs#fourierMotzkin = method()
lrs#fourierMotzkin Matrix := Matrix => A ->(
   input := {-A};
   runLrsOnInput input
--   filename := getFilename();
--   if debugLevel > 0 then << "using temporary file name " << filename << endl;
--   F := openOut(filename|".ine");
--   writeLrsInput(F,-A);
--   close F;
--   execstr := "lrs " |rootPath | filename | ".ine " | rootPath | filename | ".ext > " | rootPath | filename |".txt 2>&1";
--   if run execstr =!= 0 then error( "-- Error with lrs, for details see " | rootPath | filename | ".txt.");
--   apply(readLrsInput (filename | ".ext"), 
--      b-> if b!=0 then transpose matrix apply(entries transpose b, e-> primitive toZZ e) 
--      else b
--   )
)

lrs#fourierMotzkin (Matrix,Matrix) := Matrix => (A,B) ->(
   if B==0 then return lrs#fourierMotzkin A 
   else(
      input := {-A, B};
      runLrsOnInput input
   )
)



runLrsOnInput = method()
runLrsOnInput List := inputMatrices -> (
   filename := getFilename();
   if debugLevel > 0 then << "using temporary file name " << filename << endl;
   F := openOut(filename|".ine");
   input := prepend(F, inputMatrices);
   writeLrsInput(toSequence input);
   close F;
   execstr := "lrs " |rootPath | filename | ".ine " | rootPath | filename | ".ext > " | rootPath | filename |".txt 2>&1";
   if run execstr =!= 0 then error( "-- Error with lrs, for details see " | rootPath | filename | ".txt.");
   apply(readLrsInput (filename | ".ext"), b-> if b!=0 then transpose matrix apply(entries transpose b, e-> primitive toZZ e) else b)
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

