-- -*- coding: utf-8 -*-

error "the FastLinearAlgebra package has been deprecated"

newPackage(
	"FastLinearAlgebra",
	AuxiliaryFiles => true,
    	Version => "0.1",
    	Date => "May 12, 2011",
	Authors => {
	     {Name => "Michael E. Stillman", 
		  Email => "mike@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"},
	     {Name => "Jakob Kroeker", 
		  Email => "Jakob Kröker <kroeker@uni-math.gwdg.de>", 
		  HomePage => "" }
	     },
    	Headline => "Fast linear algebra over finite fields: interface to ffpack",
        DebuggingMode => false
    	)


-- also: rank, det, solve
export {
   "ARing",
   "powerMod",
   "mult",
   "RightSide",
   "characteristicPolynomial",
   "minimalPolynomial",
--   "nullSpace",
   "invert",
   "addMultipleTo",
   "addMultipleToExt",
   "solveLinear",
   "TransposeA",
   "TransposeB",
   "Alpha",
   "Beta"
   }

debug Core

initializeEngineLinearAlgebra QQ

powerMod = method()
powerMod(ZZ,ZZ,ZZ) := (a,b,c) -> error "ha ha: not done yet!"

-- THIS FUNCTION IS NOT FUNCTIONAL!!
-- reason: we need a function: powerMod(a, b, c) is a^b mod c.
-- write powerMod
isGenerator = (n,P) -> (
     -- n is an integer in the range 2..P-1
     -- P is a prime number
     -- returns true if n is a generator of the group of units of ZZ/P
     if P === 2 then n == 1
     else (
       n >= 2 and n <= P-1 and (
	    all(factor (P-1), v -> 1 != powerMod(n, (P-1)//v#0, P))))
  )


isPrimeField = method()
isPrimeField Ring := (R) -> (
     true
     )

-- Now in the engine:
--transpose MutableMatrix := (M) -> (
--     << "warning: rewrite to be in the engine" << endl;
--     mutableMatrix transpose matrix M
--     )

--MutableMatrix + MutableMatrix := (A,B) -> (
--     << "warning: rewrite to be in the engine" << endl;
--     mutableMatrix(matrix A + matrix B)
--     )

--MutableMatrix - MutableMatrix := (A,B) -> (
--     << "warning: rewrite to be in the engine" << endl;
--     mutableMatrix(matrix A - matrix B)
--     )

-- the following is now in m2/mutablemat.m2
--ZZ * MutableMatrix := (a,M) -> (a_(ring M)) * M

--RingElement * MutableMatrix := (a,M) -> (
--     << "warning: rewrite to be in the engine" << endl;
--     mutableMatrix(a*(matrix M))
--    C:=mutableMatrix(
--    addMultipleToExt()
--     )

--rank MutableMatrix := (M) -> rawLinAlgRank raw M

--determinant MutableMatrix := opts -> (M) -> (
--     R := ring M;
--     if hasEngineLinearAlgebra R then 
--       new R from rawLinAlgDeterminant raw M
--     else
--       error "determinant of mutable matrices over this ring is not implemented"
--     )

--invert = method()
--invert MutableMatrix := (A) -> (
--     R := ring A;
--     if hasEngineLinearAlgebra R then (
--         if numRows A =!= numColumns A then error "expected square matrix";
--         map(R,rawLinAlgInvert(raw A))
--         )
--     else 
--       error "inverse of mutable matrices over this ring is not implemented"
--     )

--MutableMatrix ^ ZZ := (A, r) -> (
--     if r == 0 then 
--       return mutableIdentity(ring A, numRows A);
--     if r < 0 then (
--	  r = -r;
--	  A = invert A;
--	  );
--     result := A;
--     if r > 1 then for i from 2 to r do result = result * A;
--     result     
--     )


--nullSpace = method(Options => {RightSide=>true})
--nullSpace(MutableMatrix) := opts -> (M) -> (
--     R := ring M;
--     map(R, rawLinAlgNullSpace(raw M, opts.RightSide))
--     )

solveLinear = method(Options => options nullSpace)
solveLinear(MutableMatrix, MutableMatrix) := opts -> (A,B) -> (
     -- solve A*X = B, or solve X*A = B
     R := ring A;
     if ring A =!= ring B then error "expected same base rings";
     map(R,rawLinAlgSolve(raw A,raw B))
     )


addMultipleTo = method()

addMultipleTo(MutableMatrix,MutableMatrix,MutableMatrix) :=  (C,A,B) -> (
     R := ring C;
     if ring A =!= ring C or ring B =!= ring C then 
       error "expected matrices over the same ring";
      m :=  numRows A;
      n := numColumns B;    
      k:=numColumns A;
      k2:=numRows B;
   
      if ( k!=k2 or numRows C != m or numColumns C != n ) then 
        error("matrix sizes are not compatible!");
        print("calling rawLinAlgAddMult");
      rawLinAlgAddMult(raw C, raw A, raw B   );
     C)


addMultipleToExt = method(Options => {
	  TransposeA => false, 
	  TransposeB => false, 
	  Alpha => null, 
	  Beta => null})

addMultipleToExt(MutableMatrix,MutableMatrix,MutableMatrix) := opts -> (C,A,B) -> (
     error("not implemented any more");
     R := ring C;
     if ring A =!= ring C or ring B =!= ring C then 
       error "expected matrices over the same ring";
     a := if opts.Alpha === null then 1_R else opts.Alpha;
     b := if opts.Beta === null then 1_R else opts.Beta;
--
      m :=  numRows A;
      n := numColumns B;    
      if opts.TransposeA then m= numColumns A;
      if opts.TransposeB then n= numRows B;
--
      k:=numColumns A;
      k2:=numRows B;
      if opts.TransposeA then k= numRows A;
      if opts.TransposeB then k2= numColumns B;
      if ( k!=k2 or numRows C != m or numColumns C != n ) then 
        error("matrix sizes are not compatible!");
--
        print("calling rawLinAlgaddMultipleToExt");
     -- rawLinAlgaddMultipleToExt(raw C, raw A, raw B, opts.TransposeA,   opts.TransposeB,   raw a, raw b       );
     C)

mult = method()
mult(MutableMatrix,MutableMatrix) :=  ( A,B) -> (
     R := ring A;
     if ring A =!= ring B  then 
       error "expected matrices over the same ring";
   
--
      m :=  numRows A;
      n := numColumns B;    
 
--
      k:=numColumns A;
      k2:=numRows B;
 
      if ( k!=k2  ) then 
        error("matrix sizes are not compatible!");
--
     print("calling rawLinAlgAddMult");
     rawC := rawLinAlgMult(raw A, raw B );
     map(ring A,rawC) 
    )

--MutableMatrix * MutableMatrix := (A,B) -> (
--     C := mutableMatrix(ring A, numRows A, numColumns B, Dense=>true);
--     addMultipleToExt(C,A,B)
--     )

characteristicPolynomial = method()
characteristicPolynomial(MutableMatrix, Ring) := (M, P) -> (
     R := ring M;
     time cp := rawLinAlgCharPoly raw M;
     t := P_0;
     time sum for i from 0 to #cp - 1 list (new R from cp#i) * t^i
     )
characteristicPolynomial(Matrix, Ring) := (M, P) -> characteristicPolynomial(mutableMatrix M, P)

minimalPolynomial = method()
minimalPolynomial(MutableMatrix, Ring) := (M, P) -> (
     R := ring M;
     cp := rawLinAlgMinPoly raw M;
     t := P_0;
     sum for i from 0 to #cp - 1 list (new R from cp#i) * t^i
     )
minimalPolynomial(Matrix, Ring) := (M, P) -> minimalPolynomial(mutableMatrix M, P)



beginDocumentation()

NONTEST = (str) -> null
--
-- loadPackage "FastLinearAlgebra"
-- debug Core


end

