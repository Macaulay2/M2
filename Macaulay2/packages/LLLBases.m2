newPackage("LLLBases",
     Version => "1.0", 
     Date => "May 16, 2005",
     Author => "Michael E. Stillman <mike@math.cornell.edu>",
     Headline => "a package for computing Lenstra-Lenstra-Lovasz bases",
     DebuggingMode => true
     )

export(
     LLL,
     isLLL, 
     kernelLLL,
     hermiteLLL,
     gcdLLL,
     gram,
     Threshold,
     ChangeOfBasisMatrix
     )

debug Macaulay2Core

---------------------------------------------------
dot = (m,i,j) -> (
     h := rawColumnDotProduct(m.RawMutableMatrix, i, j);
     new ring m from h
     )

caxy = (m,a,c1,c) -> (
     --column(c) += a * column(c1)
     columnAdd(m,c,a,c1)
     )

cflip = (m,i,j) -> columnSwap(m,i,j)

cscale = (m,a,i) -> columnMult(m,i,a)

iden = (R,n) -> mutableIdentity(R,n)

getEntry = (m,i,j) -> m_(i,j)

RR // ZZ := (a,b) -> a/b
RR // RR := (a,b) -> a/b
--------------------------------------------------
round = (a,b) -> (2*a + b) // (2*b)

setLLLthreshold := (result,alpha) -> (
     if class alpha =!= QQ then (
         if class alpha === ZZ then
	   alpha = alpha/1
	 else
	   error "bad argument");
     if alpha <= 1/4 or alpha > 1 then
         error "LLL threshold out of range (1/4,1]";
     result.threshold = alpha;)

LLLoptions := Options => {
     Threshold => null,
     ChangeOfBasisMatrix => false,
     Limit => infinity,
     Strategy => 0
     }

LLL = method LLLoptions

LLL MutableMatrix :=
LLL Matrix := options -> (M) -> (
     -- First check that the base ring is ZZ
     -- Check that the threshold is in range
     m := mutableMatrix M;
     if options.ChangeOfBasisMatrix then
          setColumnChange(m, iden(ring m, numcols m));
     strat := options.Strategy;
     thresh := if options.Threshold === null 
     then (if strat >= 3 then 99/100 else 3/4)
     else options.Threshold;
     result := if strat =!= 1 then (
	  rawLLL(m.RawMutableMatrix, thresh, strat);
	  matrix map(ZZ,m.RawMutableMatrix)
	  )
     else (
	  C := newLLLComputation(m,thresh);
	  doLLL(C, options.Limit);
	  matrix C.A
     	  );
--     if options.ChangeOfBasisMatrix then
--         M.cache#{LLL,options.Threshold} = {result, matrix getColumnChange m}
--     else
--         M.cache#{LLL,options.Threshold} = result;
     result	 
     )

--LLL MutableMatrix := options -> (M) -> (
--     m := mutableMatrix M;
--     if options.ChangeOfBasisMatrix then 
--     	 setColumnChange(m, mutableIdentity(ZZ, numcols m));
--     rawLLL(m.RawMutableMatrix, options.Threshold);
--     m
--     )

----------------------------------
-- LLL computations at top level--
----------------------------------
if LLLComputation === symbol LLLComputation
then LLLComputation = new Type of MutableHashTable


newLLLComputation = (m, threshold) -> (
     -- struct {
     --   SparseMutableMatrix A;
     --   SparseMutableMatrix changeOfBasis;
     --   QQ threshold;
     --   int k;
     --   int kmax;
     --   MutableHashTable lambda;
     --   MutableList D;
     --   bool isDone;
     -- }
     result := new LLLComputation;
     result.Engine = false;
     result.A = m;
     result.changeOfBasis = getColumnChange m;
     n := numcols result.A;
     setLLLthreshold(result,threshold);
     result.k = 2;
     result.kmax = 1;
     result.D = new MutableList from toList (n+1:0);
     result.lambda = new MutableHashTable;
     result.D#0 = 1;
     result.D#1 = dot(result.A,0,0);
     result)


doLLL = (C,count) -> (
     A := C.A;
     n := numcols A;
     lambda := C.lambda;
     D := C.D;
     alphaTop := numerator C.threshold;
     alphaBottom := denominator C.threshold;
     REDI := (k,ell) -> if abs(2*lambda#(k,ell)) > D#ell then (
	  q := (2*lambda#(k,ell) + D#ell) // (2*D#ell);
     	  caxy(A,-q,ell-1,k-1);
	  << "[" << k-1 << " " << -q << " " << ell-1 << "]\n";
	  lambda#(k,ell) = lambda#(k,ell) - q*D#ell;
	  scan(1..ell-1, i-> lambda#(k,i) = lambda#(k,i) - q*lambda#(ell,i));
	  );
     SWAPI := (k) -> (
	  cflip(A,k-1,k-2);
	  << "[flip " << k-1 << " " << k-2 << "]\n";
	  if k > 2 then (
	       scan(1..k-2, j->(tmp := lambda#(k,j); 
		                       lambda#(k,j) = lambda#(k-1,j); 
			 	       lambda#(k-1,j) = tmp)));
	  lam := lambda#(k,k-1);
	  B := (D#(k-2) * D#k + lam^2) // D#(k-1);
	  scan(k+1..C.kmax, i-> (
		 t := lambda#(i,k);
		 lambda#(i,k) = (D#k * lambda#(i,k-1) - lam * t) // D#(k-1);
		 lambda#(i,k-1) = (B*t + lam*lambda#(i,k))//(D#k);));
	  D#(k-1) = B;
	  );
     k := C.k;
     while k <= n and count =!= 0 do (
	  if gbTrace>0 then (
	      << "." << k << flush;
	      if count % 15 === 0 then (<< newline););
	  count=count-1;
	  if k > C.kmax 
	  then (
	       --<< "k larger than kmax" << newline;
	       j = 1;
	       while j <= k do (
		    u = dot(A,k-1,j-1);
		    i = 1;
		    while i <= j-1 do (
			 u = (D#i * u - lambda#(k,i)*lambda#(j,i))//(D#(i-1));
			 i = i+1;
			 );
		    if j < k 
		      then lambda#(k,j) = u
		      else (
			   D#k = u;
			   if D#k == 0 then error "LLL vectors not independent";
			   );
		    j = j+1;
		    );
	       C.kmax = k;
	       )
	  else (
	       --<< "redi(" << k << ")" << newline;
	       REDI(k,k-1);
	       if alphaBottom * (D#(k-2) * D#k + lambda#(k,k-1)^2) < 
	          alphaTop * D#(k-1)^2
	       then (
		    --<< "Lovasz condition failed" << newline;
		    SWAPI(k);
		    k = max(2,k-1);
		    C.k = k;
		    )
	       else (
		    --<< "Lovasz condition OK" << newline;
		    ell = k-2;
		    while ell >= 1 do (
			 REDI(k,ell);
			 ell = ell-1;
			 );
		    k = k+1;
		    C.k = k;
		    )
     	  	       
	       );
	  );
     -- Finally time to return:
     if k > n then C.isDone = true;
     )

----------------------------------------------------------------
-- Gram-Schmidt
gram = method()
gram Matrix := (m) -> (
     -- returns 2 things: a matrix whose columns are an orthonormal basis,
     -- and a hash table whose (i,j) entry is dot(b_i,bstar_j)/dot(bstar_j,bstar_j).
     b := mutableMatrix substitute(m,QQ);
     mu := new MutableHashTable;
     i := 0;
     n := numgens source m;
     while i < n do (
	  scan(0..i-1, j -> mu#(i,j) = dot(b,i,j) / dot(b,j,j));
	  scan(0..i-1, j -> caxy(b,-mu#(i,j),j,i));
	  i = i+1;
	  );
     (b,mu)
     )

gramMultipliers = (B) -> (
     -- B is a matrix with n columns.
     -- Returned: the lower triangular matrix of lambda's and D's.
     -- These are the integral values, see Cohen, pg 92.
     n := numgens source B;
     B = sparseMutableMatrix B;
     A = sparseMutableMatrix(ZZ,n,n);
     scan(0..n-1, i -> scan(0..i, j -> (
	  u := dot(B,i,j);
	  scan(0..j-1, k -> (
	       d := if k === 0 then 1 else getEntry(A,k-1,k-1);
	       u = (getEntry(A,k,k) * u - getEntry(A,i,k) * getEntry(A,j,k)) // d));
	  setEntry(A,i,j,u))));
     matrix A
     )

isLLL = method(Options => {Threshold => 3/4});
isLLL Matrix := options -> (m) -> (
     LLLalpha := options.Threshold;
     (B, mu) := gram m;
     n := numgens source m;
     -- first: each element of mu should be <= 1/2.
     result := true;
     scan(1..n-1, i-> scan(0..i-1, j-> if abs(mu#(i,j)) > 1/2 then (
		    << "LLL size failure " << i << "," << j << ": " << mu#(i,j) * 1.0 << newline;
		    result = false;
		    )));
     -- now check the Lovasz condition c=3/4
     scan(1..n-1, i -> (a := dot(B,i,i)/(dot(B,i-1,i-1) * (LLLalpha - (mu#(i,i-1))^2));
	                if a < 1 then (
			     << "LLL Lovasz failure " << i << ": " << a * 1.0 << newline;
			     result = false;)));
     result)

----------------------------------------------
-- KernellLLL: kernel of a matrix using LLL --
----------------------------------------------

if LLLKernelComputation === symbol LLLKernelComputation
then LLLKernelComputation = new Type of MutableHashTable

newLLLKernelComputation = (m) -> (
     -- struct {
     --   SparseMutableMatrix A;
     --   SparseMutableMatrix H;  -- the changeOfBasis matrix
     --   int k;
     --   int kmax;
     --   MutableHashTable lambda;  of ZZ
     --   MutableList D;  of ZZ
     --   MutableList F;  of bool
     --   bool isDone;
     -- }
     result := new LLLKernelComputation;
     result.A = mutableMatrix m;
     n := numcols result.A;
     result.H = iden(ring m, n);
     setColumnChange(result.A, result.H);
     result.k = 2;
     result.kmax = 1;
     result.D = new MutableList from toList (n+1:0);
     result.lambda = new MutableHashTable;
     result.F = new MutableList from toList (n+1:false);
     result.D#0 = 1;
     t := dot(result.A,0,0);
     if t == 0 then (
         result.D#1 = 1;
	 result.F#1 = false;)
     else (
	 result.D#1 = t;
	 result.F#1 = true;);
     result)

doKernelLLL = (C,count) -> (
     A := C.A;
     D := C.D;
     lambda := C.lambda;
     F := C.F;
     k := C.k;
     kmax := C.kmax;
     n := numcols A;
     REDI := (k,ell) -> if abs(2*lambda#(k,ell)) > D#ell then (
	  q := (2*lambda#(k,ell) + D#ell) // (2*D#ell);
	  caxy(A,-q,ell-1,k-1);
	  lambda#(k,ell) = lambda#(k,ell) - q*D#ell;
	  scan(1..ell-1, i-> lambda#(k,i) = lambda#(k,i) - q*lambda#(ell,i));
	  );
     SWAPK := (k) -> (
	  cflip(A,k-1,k-2);
	  if k > 2 then (
	       scan(1..k-2, j->(tmp := lambda#(k,j); 
		                       lambda#(k,j) = lambda#(k-1,j); 
			 	       lambda#(k-1,j) = tmp)));
	  lam := lambda#(k,k-1);
	  if lam === 0 then (
	       D#(k-1) = D#(k-2);
	       F#(k-1) = false;
	       F#k = true;
	       lambda#(k,k-1) = 0;
	       scan(k+1..kmax, i-> (
		     lambda#(i,k) = lambda#(i,k-1);
		     lambda#(i,k-1) = 0));
	       
	       )
	  else (
	       scan(k+1..kmax, i-> 
		    lambda#(i,k-1) = (lam * lambda#(i,k-1)) // D#(k-1));
	       t := D#k;
	       D#(k-1) = (lam^2) // D#(k-1);
	       D#k = D#(k-1);
	       scan(k+1..kmax-1, j -> scan(j+1..kmax, i -> 
		    lambda#(i,j) = ((lambda#(i,j)) * D#(k-1)) // t));
	       scan(k+1..kmax, j-> D#j = (D#j * D#(k-1)) // t);
	       );
	  );
     while k <= n and count =!= 0 do (
	  if gbTrace>0 then (
	      << "." << k << flush;
	      if count % 15 === 0 then (<< newline););
	  count = count-1;
	  if k > kmax 
	  then (
	       --<< "k larger than kmax" << newline;
	       j := 1;
	       while j <= k do (
		  if j<k and F#j == false then
		      lambda#(k,j) = 0
		  else (
		    u := dot(A,k-1,j-1);
		    i := 1;
		    while i <= j-1 do (
		       if F#i then
			 u = (D#i * u - lambda#(k,i)*lambda#(j,i))//(D#(i-1));
		       i = i+1;
		       );
		    if j < k 
		      then lambda#(k,j) = u
		      else (
			   if u === 0 then (
			     D#k = D#(k-1);
			     F#k = false;)
			   else (
			     D#k = u;
			     F#k = true;)
			   );
		    );
		  j = j+1;
		  );
	       kmax = k;
	       C.kmax = k;
	       )
	  else (
	       --<< "redi(" << k << ")" << newline;
	       if F#(k-1) then REDI(k,k-1);
	       if F#(k-1) and not F#k
	       then (
		    --<< "Lovasz condition failed" << newline;
		    SWAPK(k);
		    k = max(2,k-1);
		    C.k = k;
		    )
	       else (
		    --<< "Lovasz condition OK" << newline;
		    ell := k-2;
		    while ell >= 1 do (
			 if F#ell then REDI(k,ell);
			 ell = ell-1;
			 );
		    k = k+1;
		    C.k = k;
		    )
     	  	       
	       );
	  );
     -- Finally time to return:
     << newline;
     if k > n then C.isDone = true;
     )

LLLKernelOptions := Options => {
     Limit => -1
     }

kernelLLL = method LLLKernelOptions

kernelLLL Matrix := options -> (M) -> (
     if not M.cache.?kernelLLL then (
	  M.cache.kernelLLL = newLLLKernelComputation(M);
	  );
     doKernelLLL(M.cache.kernelLLL, options.Limit);
     C := M.cache.kernelLLL;
     r := 1;
     n := numcols C.A;
     while r <= n and not C.F#r do r=r+1;
     (matrix C.H)_{0..r-2}
     )

----------------
-- hermiteLLL --
----------------
-- This algorithm is from:
--   Havas, Majewski, Matthews, 
--   Extended GCD and Hermite Normal Form Algorithms via Lattice Basis Reduction,
--   Experimental Mathematics 7:2 p. 125 (1998)
--
-- Computes a Hermite normal form, along with a kernel, and small multiplier matrix.

if LLLHermiteComputation === symbol LLLHermiteComputation
then LLLHermiteComputation = new Type of MutableHashTable

newLLLHermiteComputation = (m, threshold, hasChangeOfBasis) -> (
     -- struct {
     --   SparseMutableMatrix A;
     --   SparseMutableMatrix changeOfBasis;
     --   QQ threshold;
     --   int k;
     --   int kmax;
     --   MutableHashTable lambda;
     --   MutableList D;
     --   bool isDone;
     -- }
     result := new LLLHermiteComputation;
     result.A = mutableMatrix m;
     n := numcols result.A;
     if hasChangeOfBasis then (
         result.changeOfBasis = iden(ring m, n);
	 setColumnChange(result.A, result.changeOfBasis);
	 );
     setLLLthreshold(result,threshold);
     result.k = 2;
     result.kmax = 1;
     result.D = new MutableList from toList (n+1:1);
     result.lambda = new MutableHashTable;
     --result.D#0 = 1;
     --result.D#1 = dot(result.A,0,0);
     result.nrows = numgens target m;
     result)

doHermiteLLL = (C,count) -> (
     A := C.A;
     m := numcols A;
     lambda := C.lambda;
     D := C.D;
     alphaTop := numerator C.threshold;
     alphaBottom := denominator C.threshold;
     n := C.nrows;
     row1 = 0;
     row2 = 0;
     scan(2..m, i -> scan(1..i-1, j-> lambda#(i,j) = 0));
     -- subroutines
     leadRow0 := (a,i) -> (
	  -- there are n rows
	  x := n-1;
	  while x >= 0 and getEntry(a,x,i) == 0 do x=x-1;
	  if x < 0 then x=n+1;
	  x);
     leadRow := (a,i) -> (
	  -- there are n rows
	  x := 0;
	  while x < n and getEntry(a,x,i) == 0 do x=x+1;
	  if x == n then x=n+1;
	  x);
     MINUS := (j) -> (
	  scan(2..m, r -> scan(1..r-1, s ->
		    if r === j or s === j then
		        lambda#(r,s) = - lambda#(r,s))));
     RED2 := (k,i) -> (
	  a1 := 0;
	  row1 = leadRow(A,i-1);
	  if row1 < n then (
	    a1 = getEntry(A,row1,i-1);
	    if a1 < 0 then (
	      MINUS(i);
	      cscale(A,-1,i-1)))
          else 
	    row1 = n+1;
	  row2 = leadRow(A,k-1);
	  if row2 >= n then row2 = n+1;
	  q := if row1 <= n then
	           getEntry(A,row1,k-1) // a1
	       else if 2*abs(lambda#(k,i)) > D#i then
	           round(lambda#(k,i), D#i)
	       else 0;
	  if q =!= 0 then (
	       caxy(A,-q,i-1,k-1);
	       lambda#(k,i) = lambda#(k,i) - q * D#i;
	       scan(1..i-1, j -> 
		    lambda#(k,j) = lambda#(k,j) - q * lambda#(i,j)));
	  );
     SWAP2 := (k) -> (
	  cflip(A,k-1,k-2);
	  scan(1..k-2, j-> (
	      tmp := lambda#(k,j);
	      lambda#(k,j) = lambda#(k-1,j);
	      lambda#(k-1,j) = tmp));
          scan(k+1..m, i -> (
	      t := lambda#(i,k-1) * D#k - lambda#(i,k) * lambda#(k,k-1);
	      lambda#(i,k-1) = (lambda#(i,k-1) * lambda#(k,k-1) + lambda#(i,k) * D#(k-2))//D#(k-1);
	      lambda#(i,k) = t//D#(k-1)));
          D#(k-1) = (D#(k-2) * D#k + lambda#(k,k-1)^2)//D#(k-1);
	  );
     k := C.k;
     while k <= m and count =!= 0 do (
	  if gbTrace>0 then (
	      << "." << k << flush;
	      if count % 15 === 0 then (<< newline););
	  count = count-1;
	  RED2(k,k-1);
	  if row1 <= min(row2,n) or (row1 === row2 and row1 === n+1 and
	       alphaBottom * (D#(k-2) * D#k + lambda#(k,k-1)^2) < 
	       alphaTop * D#(k-1)^2)
	  then (
	       SWAP2(k);
	       if k > 2 then (
		    k = k-1;
		    C.k = k);
	       )
	  else (
	       i := k-2;
	       while i >= 1 do (RED2(k,i); i=i-1);
	       k = k+1;
	       C.k = k;
	       )
	  );
     if k > m then C.isDone = true;
     );

HermiteLLLoptions := Options => {
     Threshold => 3/4,
     ChangeOfBasisMatrix => true,
     Limit => -1
     }

hermiteLLL = method HermiteLLLoptions

hermiteLLL Matrix := options -> (M) -> (
     -- Possible options: Threshold=>QQ, ChangeOfBasisMatrix,
     -- and for the specific computation: 
     if not M.cache.?hermiteLLL then (
	  M.cache.hermiteLLL = newLLLHermiteComputation(M,options.Threshold, options.ChangeOfBasisMatrix);
	  );
     doHermiteLLL(M.cache.hermiteLLL, options.Limit);
     M.cache.hermiteLLL.A
     )

------------
-- gcdLLL --
------------
gcdLLLoptions := Options => {
     Strategy => null,
     Threshold => 3/4,
     Limit => -1
     }

gcdLLL = method gcdLLLoptions

gcdLLL Matrix := options -> (M) -> (
     -- Possible options: Threshold=>QQ, ChangeOfBasisMatrix,
     -- and for the specific computation: 
     hermiteLLL(M,options,ChangeOfBasisMatrix=>true);
     m := M.cache.hermiteLLL.A;
     (getEntry(m,0,numcols m-1), matrix getColumnChange m)
     )

-- Input: list of positive integers 's' of length m.
-- Output: a matrix B of size (m+1) by m, whose first m-1 columns form a basis
--         for the kernel of 's', and whose last column is
--         (a1, a2, ..., am, g)
-- where dot(a,s) = g is the gcd of the integers 's', and 
-- the length of a is 'small'.
-- Algorithm: see Havas, Majewski, Matthews, 
--   Extended GCD and Hermite Normal Form Algorithms via Lattice Basis Reduction,
--   Experimental Mathematics 7:2 p. 125 (1998)

agcdLLL := (s,thresh) -> (
     alphaNumerator := numerator thresh;
     alphaDenominator := denominator thresh;
     m := #s;
     B = iden(ZZ,m);
     D = new MutableList from toList(m+1:1);
     a = new MutableList from prepend(0,s);
     lambda = new MutableHashTable;
     scan(2..m, i -> scan(1..i-1, j-> lambda#(i,j) = 0));
     -- subroutines
     RED := (k,i) -> (
	  q := if a#i =!= 0 then
	           round(a#k, a#i)
	       else if 2*abs(lambda#(k,i)) > D#i then
	           round(lambda#(k,i), D#i)
	       else 0;
	  if q =!= 0 then (
	       a#k = a#k - q * a#i;
	       caxy(B,-q,i-1,k-1);
	       lambda#(k,i) = lambda#(k,i) - q * D#i;
	       scan(1..i-1, j -> 
		    lambda#(k,j) = lambda#(k,j) - q * lambda#(i,j)));
	  );
     SWAP := (k) -> (
	  tmp := a#k;	  a#k = a#(k-1);	  a#(k-1) = tmp;
	  cflip(B,k-1,k-2);
	  scan(1..k-2, j-> (
	      tmp := lambda#(k,j);
	      lambda#(k,j) = lambda#(k-1,j);
	      lambda#(k-1,j) = tmp));
          scan(k+1..m, i -> (
	      t := lambda#(i,k-1) * D#k - lambda#(i,k) * lambda#(k,k-1);
	      lambda#(i,k-1) = (lambda#(i,k-1) * lambda#(k,k-1) + lambda#(i,k) * D#(k-2))//D#(k-1);
	      lambda#(i,k) = t//D#(k-1)));
          D#(k-1) = (D#(k-2) * D#k + lambda#(k,k-1)^2)//D#(k-1);
	  );
     k = 2;
     while k <= m do (
	  RED(k,k-1);
	  if a#(k-1) =!= 0 or (a#(k-1) === 0 and a#k === 0 and
	       alphaDenominator * (D#(k-2) * D#k + lambda#(k,k-1)^2) < 
	       alphaNumerator * D#(k-1)^2)
	  then (
	       SWAP(k);
	       if k > 2 then k = k-1;
	       )
	  else (
	       i := k-2;
	       while i >= 1 do (RED(k,i); i=i-1);
	       k = k+1;
	       )
	  );
     if a#m < 0 then (
	  a#m = -a#m;
	  cscale(B,-1,m-1);
	  );
     (a#m,matrix B)
     );

gcdLLL List := options -> (s) -> (
     if options.Strategy =!= Hermite
     then agcdLLL(s,options.Threshold)
     else (
     	  m := matrix{s};
     	  gcdLLL(m,options))
     )


beginDocumentation()

document { 
	Key => LLLBases,
	Headline => "lattice reduction (Lenstra-Lenstra-Lovasz bases)",
	EM "LLLBases", " is a package implementing several variants of LLL bases.
	Some of these are implemented in the Macaulay2 engine, 
	and some are implemented at top level.",
	PARA,
	"A matrix over ZZ determines a lattice: this is the ZZ-module generated by the columns.  It
	also determines a generating set.  If the columns form a basis for this lattice (that is, they
	are linearly independent), the basis is said to be a LLL basis, with respect to the rational
   number alpha, if ...",
   	PARA,
	"LLL bases have nice theoretical properties, but the main benefit of LLL bases is that
	the entries of the resulting matrix often have dramatically small size, even smaller than
	theory would imply.",
	PARA,
	"This package implements the following functions.",
	Subnodes => {
	     "main LLL algorithm",
	     TO LLL,
	     "applications and variants",
	     TO kernelLLL,
	     TO gcdLLL,
	     TO hermiteLLL,
	     "support routines that are occasionally useful",
	     TO gram,
	     TO isLLL
	     }
	}
document { 
     Key => {LLL,(LLL,Matrix),(LLL,MutableMatrix)},
     Headline => "compute an LLL basis",
     Usage => "LLL m",
     Inputs => {
	  "m" => Matrix => {"or a ", TO MutableMatrix, ", over ", TO ZZ, ", whose columns are linearly independent"}
	  },
     Outputs => {
	  {"A matrix or mutable matrix, respectively, whose columns form an LLL 
	       (Lenstra-Lenstra-Lovasz) basis of the image lattice of the matrix"}
	  },
     PARA,
     "This function is provided by the package ", TO LLLBases, ".",
     PARA,
     "In this example, we compute the LLL basis of the nullspace of a matrix.  This is 
     an example of Havas et al.",
     EXAMPLE {
	  ///load "LLLBases.m2"///,
	  "m1 = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)",
	  "m = syz m1",
	  "LLL m"
	  },
     "If m is a mutable matrix, then it is also possible to get the change of basis 
     matrix from the original basis to the LLL basis.  For example,",
     EXAMPLE {
     	  "n = mutableMatrix m",
	  "n1 = LLL(n, ChangeOfBasisMatrix=>true)",
	  "c = getColumnChange n1",
	  "(matrix n) * (matrix c) == matrix n1"
	  },
     Caveat => {"The columns of the matrix m must be linearly independent.",
	  "The matrix must be defined over the ring ZZ.  It should be possible to 
	  allow real and rational matrices too, but this is not yet implemented."},
     SeeAlso => {isLLL, gcdLLL, kernelLLL, hermiteLLL, getColumnChange}
     }
document { 
     Key => [LLL, Threshold],
     Headline => "a rational number in the range (1/4,1], default is 3/4",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [LLL, ChangeOfBasisMatrix],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [LLL, Limit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [LLL, Strategy],
     Headline => "choose among different algorithms",
     Usage => "LLL(...,Strategy=>n)",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document {
     Key => {isLLL, (isLLL,Matrix)},
     Headline => "is a basis an LLL basis?",
     Usage => "isLLL m",
     Inputs => {
	  "m" => Matrix => {"over ", TO ZZ, ", whose columns are linearly independent"}
	  },
     Outputs => {
	  Boolean => {"Whether the columns of the matrix form an LLL basis
	       with respect to the Threshold (which has default 3/4)"
	       }
	  },
     "This function is provided by the package ", TO LLLBases, ".",
     PARA,
     "If the matrix is not in LLL reduced form, then the offending conditions are
     displayed.  For example,",
     EXAMPLE {
	  ///load "LLLBases.m2"///,
	  "m = matrix {{1, 0}, {1, 1}, {1, 2}, {1, 3}}",
	  "isLLL m",
	  "n = LLL m",
	  "isLLL n"
	  },
     "If the optional argument Threshold is given, the conditions are checked using that value.",
     EXAMPLE {
	  ///load "LLLBases.m2"///,
	  "m = matrix {{1, 0}, {1, 1}, {1, 2}, {1, 3}}",
	  "isLLL(m, Threshold=>1)",
          },
     Caveat => {"A Gram-Schmidt reduction is done over QQ, so this
	  can be computationally intensive for larger matrix sizes.
	  It is usually easier and faster to see if LLL returns the
	  same matrix.  This routine was used to debug and test the
	  LLL routines here, and is provided as an alternate check of 
	  correctness.",
	  "The matrix must be defined over the ring ZZ.  It should be possible to 
	  allow real and rational matrices too, but this is not yet implemented."},
     SeeAlso => {LLL, gcdLLL, kernelLLL, hermiteLLL}
     }
document { 
     Key => [isLLL, Threshold],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document {
     Key => gcdLLL,
     Headline => "compute the gcd of integers, and small multipliers",
     Usage => "(g,z) = gcdLLL m",
     Inputs => {
	  "m" => List => {"of integers"}
	  },
     Outputs => {
          "g" => ZZ => {"the gcd of the integers in the list s"},
	  "z" => Matrix => {" of integers"}
	  },
     "This function is provided by the package ", TO LLLBases, ".",
     PARA,
     "The first n-1 columns of the matrix z form a basis of the kernel
     of the n integers of the list s, and the dot product of the last column of z 
     and s is the gcd g.",
     PARA,
     "The method used is described in the paper:",
     PARA,
     "Havas, Majewski, Matthews, ", EM"Extended GCD and Hermite Normal Form Algorithms via Lattice Basis Reduction",
     ", Experimental Mathematics 7:2 p. 125 (1998).",
     PARA,
     "For an example,",
     EXAMPLE {
	  ///load "LLLBases.m2"///,
	  "s = apply(5,i->372*(random 1000000))",
	  "gcdLLL s"
	  },
     SeeAlso => {LLL, isLLL, kernelLLL, hermiteLLL}
     }
document { 
     Key => (gcdLLL,List),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (gcdLLL,Matrix),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [gcdLLL, Threshold],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [gcdLLL, Limit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [gcdLLL, Strategy],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document { 
     Key => {gram,(gram,Matrix)},
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document { 
     Key => {kernelLLL,(kernelLLL,Matrix)},
     Headline => "compute the kernel of an integer matrix using LLL bases",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "This function is provided by the package ", TO LLLBases, ".",
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [kernelLLL, Limit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document {
	Key => hermiteLLL,
	Headline => "compute a Hermite normal form using LLL bases",
	"This function is provided by the package ", TO LLLBases, "."
	}
document { 
     Key => {hermiteLLL,(hermiteLLL,Matrix)},
     Headline => "compute the Hermite normal form and small multiplier matrix using LLL bases",
     Usage => "hermiteLLL m",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [hermiteLLL, Threshold],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [hermiteLLL, Limit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [hermiteLLL, ChangeOfBasisMatrix],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

------------------------------------------
--------- Tests --------------------------
------------------------------------------

---------------------------------------
-- LLL engine and frontend version-----
---------------------------------------
TEST ///
     loadPackage "LLLBases";

    time m1 = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)
    time m = syz m1
    time mz = LLL(m)
     time assert(m1 * matrix mz == 0)
    time assert(isLLL matrix mz)
    
    time mz2 = LLL(m, Threshold => 101/400)    
    assert( not isLLL matrix mz2 )
    assert isLLL(matrix mz2, Threshold=>101/400)

    time mz3 = LLL(m, Engine=>false)
    time assert(m1 * matrix mz3 == 0)
    time assert(isLLL matrix mz3)
    mz == mz3 -- not always true.  Why not?

    assert(hermiteLLL mz == hermiteLLL mz3)
    
    time mz4 = LLL(m, Engine=>false, Threshold => 101/400)    
    assert( not isLLL matrix mz4 )
    assert isLLL(matrix mz4, Threshold=>101/400)
    
    m1 = m1 ** RR
    -- LLL(m1,Engine=>false) FAILS

    time mz = LLL(m, ChangeOfBasisMatrix=>true)
    m.cache#{LLL,3/4}#1 -- this is the change of basis matrix
    time mz5 = LLL(m, Engine=>false, ChangeOfBasisMatrix=>true)
    m.cache#{LLL,3/4}#1 -- this is the change of basis matrix    
///

TEST ""
/// -- DON'T TEST YET??
     loadPackage "LLLBases";
     testLLL = (m) -> (
  	  -- Test 1:
  	  remove(m.cache,symbol LLL);
  	  time m1 = matrix LLL(m,Engine=>true);
  	  --time assert(isLLL m1);
  	  -- Test 2:
  	  --remove(m.cache,symbol LLL);
  	  --time m2 = matrix LLL(m, Engine=>true,ChangeOfBasisMatrix=>true);
  	  --assert(m1 == m2);
  	  --h = matrix getColumnChange m2;
  	  --assert(m2 == m*h);
  	  -- Test 3:
  	  remove(m.cache,symbol LLL);
  	  time m3 = matrix LLL(m,Engine=>false);
  	  --time assert(isLLL m3);
  	  -- Test 4:
  	  remove(m.cache,symbol LLL);
  	  time m4 = LLL(mutableMatrix m, Engine=>false,ChangeOfBasisMatrix=>true);
  	  assert(m3 == m4);
  	  h2 = matrix getColumnChange m4;  -- FAILS
  	  assert(m3 == m*h2);
  	  --assert(h == h2);
  	  )

     -- a random example
     m = matrix randomMutableMatrix(10,10,.5,500)
     testLLL m

     m = matrix randomMutableMatrix(10,10,.5,5)
     testLLL m

     m = matrix randomMutableMatrix(20,15,.5,5)
     testLLL m

     m = matrix randomMutableMatrix(20,20,.5,5)
     testLLL m

     m = matrix randomMutableMatrix(30,30,.5,5)
     testLLL m

     m = matrix randomMutableMatrix(30,30,.5,500)
     testLLL m

     m = matrix randomMutableMatrix(30,30,.5,500000)
     testLLL m

     m = matrix randomMutableMatrix(40,40,.5,5)
     testLLL m


///

-------------------
-- kernelLLL ------
-------------------
TEST ///
     loadPackage "LLLBases";

    time m1 = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)
    time m = syz m1
    time mz = LLL(m)

    time s1 = kernelLLL m1
     assert(m1 * s1 == 0)


    m = matrix{{1,1,1,1},{0,1,2,3}}  
    kernelLLL m

    m = matrix randomMutableMatrix(10,15,.5,5)
    mz = kernelLLL m
    mz2 = syz m
    hermiteLLL mz
    LLL mz
    LLL syz m

    m = matrix randomMutableMatrix(30,70,.5,5)
    time mz = kernelLLL m
    mz1 = time LLL mz
    -- time mz2 = syz m;  -- Time varies depending on m, often 
    -- is actually somewhat less that computing mz1.  Except 
    -- that the size of the integers is very large.
    -- time LLL mz2 -- too timeconsuming?

    -- This example is a lattice which was of interest to Persi Diaconis (1997)
    nn = 5
    NN = nn!
    SymmetricGroup = (indices) -> (
       if #indices == 0 then {{}} else 
       flatten apply(toList(0..#indices-1), i -> (
	 others := drop(indices, {i,i});
	 sn := SymmetricGroup others;
	 apply(sn, j -> prepend(indices#i, j)))))
    Sn = SymmetricGroup toList(0..nn-1)
    M = map(ZZ^NN, nn^2, (i,j) -> (
	  j1 = j % nn;
	  j2 = j // nn;
	  if Sn#i#j1 == j2 then 1 else 0))
    M = transpose M
    time m = syz M    -- time: 0.03 seconds
    time m1 = kernelLLL M -- time: 1.98 seconds (front-end implementation) 
                      -- BUT: quality of output was very much better...
    assert not isLLL m1   -- NO
    time LLL m1  -- .76 sec 44.9 seconds (front-end implementation), Quality of output is higher still
    time LLL(m1, Engine=>false) -- 2.43 sec

    --time LLL m -- 12.27 sec takes too long for the 'check' command
///


-------------------
-- hermiteLLL -----
-------------------
TEST ///
     loadPackage "LLLBases";

    time m1 = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)
    a = hermiteLLL m1
    b = hermiteLLL(m1,ChangeOfBasisMatrix=>true) -- comes out in a different format than 'gb'
    assert(a == b)
    c = matrix getColumnChange b
    b = matrix b
    assert(m1*c == b)

    -- A simple one:
    m = matrix{{1,1,1,1},{0,1,2,3}}  
    mh = hermiteLLL m
    mz = matrix getColumnChange mh 
    assert(m * mz == matrix mh)

    -- Test from Havas et al paper 1998:
    m = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)
    time mh = hermiteLLL m
    mz = matrix getColumnChange mh
    assert(m * mz == matrix mh)

    -- Random entries
    m = matrix randomMutableMatrix(10,15,.9,5)
    time mh = hermiteLLL m
    mz = matrix getColumnChange mh
    assert(m * mz == matrix mh)

    -- Random entries
    m = matrix randomMutableMatrix(20,35,.9,5)
    time mh = hermiteLLL m
    mz = matrix getColumnChange mh
    assert(m * mz == matrix mh)

    -- Random entries
    m = matrix randomMutableMatrix(20,35,.1,10)
    time mh = hermiteLLL m
    mz = matrix getColumnChange mh
    assert(m * mz == matrix mh)
    det mz
    
    -- One that caused an error in an earlier version:
    m = matrix {{0, 0, 0, 2, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {-4, 0, 0, 0, 0, 2, 0, -1, 0, 0, -1, 0, 0, 0, 0}, {-4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -5}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0}}
    time mh = hermiteLLL m
    mz = matrix getColumnChange mh
    assert(m * mz == matrix mh)

///


-------------------
-- gcdLLL ---------
-------------------
TEST ///
     loadPackage "LLLBases";

     time (g,mz) = gcdLLL {3,7,11}
     time gcdLLL ({3,7,11}, Strategy => Hermite)

     assert(matrix{{3,7,11}} * mz == map(ZZ^1, ZZ^2, 0) | matrix{{g}})

     arandomlist = apply(10,i->random 100000000)
     time (g,mz) = gcdLLL(arandomlist)
     gcd arandomlist == g
     assert(matrix{arandomlist} * mz == map(ZZ^1, ZZ^(#arandomlist-1), 0) | matrix{{g}})

     arandomlist = apply(10,i->random 100000000)
     time (g,mz) = gcdLLL(arandomlist, Strategy=>Hermite)
     gcd arandomlist == g
     assert(matrix{arandomlist} * mz == map(ZZ^1, ZZ^(#arandomlist-1), 0) | matrix{{g}})

     arandomlist = apply(10,i->372*random 100000000)
     time (g,mz) = gcdLLL(arandomlist)
     gcd arandomlist == g
     assert(matrix{arandomlist} * mz == map(ZZ^1, ZZ^(#arandomlist-1), 0) | matrix{{g}})

     arandomlist = apply(10,i->372*random 100000000)
     time (g,mz) = gcdLLL(arandomlist, Strategy=>Hermite)
     gcd arandomlist == g
     assert(matrix{arandomlist} * mz == map(ZZ^1, ZZ^(#arandomlist-1), 0) | matrix{{g}})


     time gcdLLL {116085838, 181081878, 314252913, 10346840}
     time gcdLLL( {116085838, 181081878, 314252913, 10346840}, Strategy=>Hermite)


     mylist = {763836, 1066557, 113192, 1785102, 1470060, 3077752, 114793, 3126753, 1997137, 2603018}
     time gcdLLL mylist
     time (g,z) = gcdLLL( mylist, Strategy=>Hermite)

     assert(matrix {mylist} * z == matrix{{0,0,0,0,0,0,0,0,0,1}})

     (g,z) = time gcdLLL(mylist,Threshold=>1/1)
     assert(matrix {mylist} * z == matrix{{0,0,0,0,0,0,0,0,0,g}})
     (g,z) = time gcdLLL(mylist,Threshold=>101/400)
     assert(matrix {mylist} * z == matrix{{0,0,0,0,0,0,0,0,0,g}})
     time gcdLLL(mylist,Threshold=>6/7)

///

-------------------
-- gram, isLLL ----
-------------------
TEST ///
     loadPackage "LLLBases";

    time m1 = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)
    time m = syz m1
    time mz = LLL(m)
    (gm, H) = gram mz 
    gm = matrix gm
    h = (transpose gm) * gm -- should be diagonal:
    scan(numgens source h, i -> scan(numgens source h, j -> 
	      assert(i === j or h_(i,j) == 0)))
///

