--needs "../e/test-sparse.m2"
needs "../e/sparsemat.m2"

LLLalpha := 3/4
alphaNumerator := 3
alphaDenominator := 4

----------------------
-- LLL computations --
----------------------
if LLLComputation === quote LLLComputation
then LLLComputation = new Type of MutableHashTable

setLLLthreshold := (result,alpha) -> (
     if class alpha =!= QQ then (
         if class alpha === ZZ then
	   alpha = alpha/1
	 else
	   error "bad argument");
     if alpha <= 1/4 or alpha > 1 then
         error "LLL threshold out of range (1/4,1]";
     result.threshold = alpha;)

newLLLComputation = (m, threshold, hasChangeOfBasis) -> (
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
     result.A = sparseMutableMatrix m;
     n := numcols result.A;
     if hasChangeOfBasis then (
         result.changeOfBasis = iden(ring m, n);
	 setColumnChange(result.A, result.changeOfBasis);
	 );
     setLLLthreshold(result,threshold);
     result.k = 2;
     result.kmax = 1;
     result.D = new MutableList from toList (n+1:0);
     result.lambda = new MutableHashTable;
     result.D#0 = 1;
     result.D#1 = dot(result.A,0,0);
     result)


doLLL = (C,count) -> (
     verbose := gbTrace(0);
     gbTrace(verbose);
     A := C.A;
     n := numcols A;
     lambda := C.lambda;
     D := C.D;
     alphaTop := numerator C.threshold;
     alphaBottom := denominator C.threshold;
     REDI := (k,ell) -> if abs(2*lambda#(k,ell)) > D#ell then (
	  q := (2*lambda#(k,ell) + D#ell) // (2*D#ell);
     	  caxy(A,-q,ell-1,k-1);
	  lambda#(k,ell) = lambda#(k,ell) - q*D#ell;
	  scan(1..ell-1, i-> lambda#(k,i) = lambda#(k,i) - q*lambda#(ell,i));
	  );
     SWAPI := (k) -> (
	  cflip(A,k-1,k-2);
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
	  if verbose>0 then (
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

newLLLEngineComputation = (m, threshold, hasChangeOfBasis) -> (
     -- In the engine case:
     -- struct {
     --   bool Engine;
     --   int Status;  -- 0: done
     --   SparseMutableMatrix A;
     --   SparseMutableMatrix changeOfBasis;
     --   QQ threshold;
     --   SparseMutableMatrix LLLstate;
     -- }
     result := new LLLComputation;
     result.Engine = true;
     result.A = sparseMutableMatrix m;
     n := numcols result.A;
     if hasChangeOfBasis then (
         result.changeOfBasis = iden(ring m, n);
	 setColumnChange(result.A, result.changeOfBasis);
	 );
     setLLLthreshold(result,threshold);
     sendgg(ggPush result.A, ggPush threshold, ggLLLinit);
     result.LLLstate = newSparseMatrix ZZ;
     result
     )

doEngineLLL = (A, LLLstate, nsteps) -> (
     sendgg(ggPush A, ggPush LLLstate, ggPush nsteps, ggLLLcalc);
     new ZZ)
     
LLLoptions := Options => {
     Threshold => 3/4,
     ChangeOfBasisMatrix => false,
     Steps => -1,
     Engine => true
     }

LLL = method LLLoptions

LLL Matrix := (M,options) -> (
     -- Possible options: Threshold=>QQ, ChangeOfBasisMatrix,
     -- and for the specific computation: 
     if not M.?LLL then (
	  if options.Engine then
	    M.LLL = newLLLEngineComputation(M,options.Threshold, options.ChangeOfBasisMatrix)
	  else
	    M.LLL = newLLLComputation(M,options.Threshold, options.ChangeOfBasisMatrix);
	  );
     if M.LLL.Engine then (
       M.LLL.Status = doEngineLLL(M.LLL.A, M.LLL.LLLstate,options.Steps);
       -- MES: should we remove the LLL state, if the computation is done?
       )
     else
       doLLL(M.LLL, options.Steps);
     M.LLL.A
     )

----------------------------------------------
-- KernellLLL: kernel of a matrix using LLL --
----------------------------------------------

if LLLKernelComputation === quote LLLKernelComputation
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
     result.A = sparseMutableMatrix m;
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
     verbose := gbTrace(0);
     gbTrace(verbose);
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
	  if verbose>0 then (
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
     Steps => -1
     }

kernelLLL = method LLLKernelOptions

kernelLLL Matrix := (M,options) -> (
     if not M.?kernelLLL then (
	  M.kernelLLL = newLLLKernelComputation(M);
	  );
     doKernelLLL(M.kernelLLL, options.Steps);
     C := M.kernelLLL;
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

if LLLHermiteComputation === quote LLLHermiteComputation
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
     result.A = sparseMutableMatrix m;
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
     verbose := gbTrace(0);
     gbTrace(verbose);
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
     leadRow = (a,i) -> (
	  -- there are n rows
	  x := n-1;
	  while x >= 0 and getEntry(a,x,i) == 0 do x=x-1;
	  if x < 0 then x=n+1;
	  x);
     leadRow = (a,i) -> (
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
	  if verbose>0 then (
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
     Steps => -1
     }

hermiteLLL = method HermiteLLLoptions

hermiteLLL Matrix := (M,options) -> (
     -- Possible options: Threshold=>QQ, ChangeOfBasisMatrix,
     -- and for the specific computation: 
     if not M.?hermiteLLL then (
	  M.hermiteLLL = newLLLHermiteComputation(M,options.Threshold, options.ChangeOfBasisMatrix);
	  );
     doHermiteLLL(M.hermiteLLL, options.Steps);
     M.hermiteLLL.A
     )

------------
-- gcdLLL --
------------
gcdLLLoptions := Options => {
     Threshold => 3/4,
     ChangeOfBasisMatrix => true,
     Steps => -1
     }

gcdLLL = method gcdLLLoptions

gcdLLL Matrix := (M,options) -> (
     -- Possible options: Threshold=>QQ, ChangeOfBasisMatrix,
     -- and for the specific computation: 
     hermiteLLL(M,options);
     m := M.hermiteLLL.A;
     {getEntry(m,0,numcols m-1), matrix getColumnChange m}
     )

gcdLLL List := (s,options) -> (
     m := matrix{s};
     gcdLLL(m,options))
----------------------------------------------------------------
-- Gram-Schmidt
gram = method()
gram Matrix := (m) -> (
     -- returns 2 things: a matrix whose columns are an orthonormal basis,
     -- and a hash table whose (i,j) entry is dot(b_i,bstar_j)/dot(bstar_j,bstar_j).
     b := sparseMutableMatrix substitute(m,QQ);
     mu := new MutableHashTable;
     i := 0;
     n := numgens source m;
     while i < n do (
	  scan(0..i-1, j -> mu#(i,j) = dot(b,i,j) / dot(b,j,j));
	  scan(0..i-1, j -> caxy(b,-mu#(i,j),j,i));
	  i = i+1;
	  );
     {b,mu}
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

isLLL = (m) -> (
     LLLalpha := 3/4;
     ans := gram m;
     B := ans#0;
     mu := ans#1;
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

     
-- integral LLL: 
aLLL = method()
aLLL Matrix := (M) -> (
     A := sparseMutableMatrix M;
     integralLLL(A,numgens source M);
     matrix A)


integralLLL = (b,n) -> (
     H = iden(ring b,n);
     k = 2;
     kmax = 1;
     d = new MutableList from toList (n+1:0);
     lambda = new MutableHashTable;
     REDI = (k,ell) -> if abs(2*lambda#(k,ell)) > d#ell then (
	  q := (2*lambda#(k,ell) + d#ell) // (2*d#ell);
     	  caxy(H,-q,ell-1,k-1);
	  caxy(b,-q,ell-1,k-1);
	  lambda#(k,ell) = lambda#(k,ell) - q*d#ell;
	  scan(1..ell-1, i-> lambda#(k,i) = lambda#(k,i) - q*lambda#(ell,i));
	  );
     SWAPI = (k) -> (
	  cflip(H,k-1,k-2);
	  cflip(b,k-1,k-2);
	  if k > 2 then (
	       scan(1..k-2, j->(tmp := lambda#(k,j); 
		                       lambda#(k,j) = lambda#(k-1,j); 
			 	       lambda#(k-1,j) = tmp)));
	  lam = lambda#(k,k-1);
	  B = (d#(k-2) * d#k + lam^2) // d#(k-1);
	  scan(k+1..kmax, i-> (
		 t := lambda#(i,k);
		 lambda#(i,k) = (d#k * lambda#(i,k-1) - lam * t) // d#(k-1);
		 lambda#(i,k-1) = (B*t + lam*lambda#(i,k))//(d#k);));
	  d#(k-1) = B;
	  );
     d#0 = 1;
     d#1 = dot(b,0,0);
     count := 0;
     while k <= n do (
	  --<< "k = " << k << " kmax = " << kmax << newline;
	  << "." << k << flush;
	  if count == 15 then (count = 0; << newline) else count=count+1;
	  if k > kmax 
	  then (
	       --<< "k larger than kmax" << newline;
	       kmax = k;
	       j = 1;
	       while j <= k do (
		    u = dot(b,k-1,j-1);
		    i = 1;
		    while i <= j-1 do (
			 u = (d#i * u - lambda#(k,i)*lambda#(j,i))//(d#(i-1));
			 i = i+1;
			 );
		    if j < k 
		      then lambda#(k,j) = u
		      else (
			   d#k = u;
			   if d#k == 0 then error "LLL vectors not independent";
			   );
		    j = j+1;
		    );
	       )
	  else (
	       --<< "redi(" << k << ")" << newline;
	       REDI(k,k-1);
	       if 4*(d#k)*(d#(k-2)) < 3*(d#(k-1))^2 - 4*(lambda#(k,k-1))^2
	       then (
		    --<< "Lovasz condition failed" << newline;
		    SWAPI(k);
		    k = max(2,k-1);
		    )
	       else (
		    --<< "Lovasz condition OK" << newline;
		    ell = k-2;
		    while ell >= 1 do (
			 REDI(k,ell);
			 ell = ell-1;
			 );
		    k = k+1;
		    )
     	  	       
	       );
	  );
     -- Finally time to return:
     << newline;
     matrix H
     )

-- Kernel
akernelLLL = method()
akernelLLL Matrix := (M) -> (
     A := sparseMutableMatrix M;
     n := numgens source M;
     H := iden(ring A,n);
     k := 2;
     kmax := 1;
     d := new MutableList from toList (n+1:0);
     f := new MutableList from toList (n+1:false);
     lambda := new MutableHashTable;
     REDI := (k,ell) -> if abs(2*lambda#(k,ell)) > d#ell then (
	  q := (2*lambda#(k,ell) + d#ell) // (2*d#ell);
     	  caxy(H,-q,ell-1,k-1);
	  caxy(A,-q,ell-1,k-1);
	  lambda#(k,ell) = lambda#(k,ell) - q*d#ell;
	  scan(1..ell-1, i-> lambda#(k,i) = lambda#(k,i) - q*lambda#(ell,i));
	  );
     SWAPK := (k) -> (
	  cflip(H,k-1,k-2);
	  cflip(A,k-1,k-2);
	  if k > 2 then (
	       scan(1..k-2, j->(tmp := lambda#(k,j); 
		                       lambda#(k,j) = lambda#(k-1,j); 
			 	       lambda#(k-1,j) = tmp)));
	  lam := lambda#(k,k-1);
	  if lam === 0 then (
	       d#(k-1) = d#(k-2);
	       f#(k-1) = false;
	       f#k = true;
	       lambda#(k,k-1) = 0;
	       scan(k+1..kmax, i-> (
		     lambda#(i,k) = lambda#(i,k-1);
		     lambda#(i,k-1) = 0));
	       
	       )
	  else (
	       scan(k+1..kmax, i-> 
		    lambda#(i,k-1) = (lam * lambda#(i,k-1)) // d#(k-1));
	       t := d#k;
	       d#(k-1) = (lam^2) // d#(k-1);
	       d#k = d#(k-1);
	       scan(k+1..kmax-1, j -> scan(j+1..kmax, i -> 
		    lambda#(i,j) = ((lambda#(i,j)) * d#(k-1)) // t));
	       scan(k+1..kmax, j-> d#j = (d#j * d#(k-1)) // t);
	       );
	  );
     d#0 = 1;
     t := dot(A,0,0);
     if t == 0 then (
         d#1 = 1;
	 f#1 = false;)
     else (
	 d#1 = t;
	 f#1 = true;);
     count := 0;
     while k <= n do (
	  --<< "k = " << k << " kmax = " << kmax << newline;
	  << "." << k << flush;
	  if count == 15 then (count = 0; << newline) else count=count+1;
	  if k > kmax 
	  then (
	       --<< "k larger than kmax" << newline;
	       kmax = k;
	       j := 1;
	       while j <= k do (
		  if j<k and f#j == false then
		      lambda#(k,j) = 0
		  else (
		    u := dot(A,k-1,j-1);
		    i := 1;
		    while i <= j-1 do (
		       if f#i then
			 u = (d#i * u - lambda#(k,i)*lambda#(j,i))//(d#(i-1));
		       i = i+1;
		       );
		    if j < k 
		      then lambda#(k,j) = u
		      else (
			   if u === 0 then (
			     d#k = d#(k-1);
			     f#k = false;)
			   else (
			     d#k = u;
			     f#k = true;)
			   );
		    );
		  j = j+1;
		  )
	       )
	  else (
	       --<< "redi(" << k << ")" << newline;
	       if f#(k-1) then REDI(k,k-1);
	       if f#(k-1) and not f#k
	       then (
		    --<< "Lovasz condition failed" << newline;
		    SWAPK(k);
		    k = max(2,k-1);
		    )
	       else (
		    --<< "Lovasz condition OK" << newline;
		    ell := k-2;
		    while ell >= 1 do (
			 if f#ell then REDI(k,ell);
			 ell = ell-1;
			 );
		    k = k+1;
		    )
     	  	       
	       );
	  );
     -- Finally time to return:
     << newline;
     r := 1;
     while r <= n and not f#r do r=r+1;
     (matrix H)_{0..r-2}
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

round = (a,b) -> (2*a + b) // (2*b)
agcdLLL = (s) -> (
     m := #s;
     B = iden(ZZ,m);
     D = new MutableList from toList(m+1:1);
     a = new MutableList from prepend(0,s);
     lambda = new MutableHashTable;
     scan(2..m, i -> scan(1..i-1, j-> lambda#(i,j) = 0));
     -- subroutines
     RED = (k,i) -> (
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
     SWAP = (k) -> (
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
     {a#m,matrix B}
     );
     
ahermiteLLL = (G) -> (
     verbose := gbTrace(0);
     gbTrace(verbose);
     m := numgens source G;
     n := numgens target G;
     row1 = 0;
     row2 = 0;
     A = sparseMutableMatrix G;
     B = iden(ZZ,m);
     D = new MutableList from toList(m+1:1);
     lambda = new MutableHashTable;
     scan(2..m, i -> scan(1..i-1, j-> lambda#(i,j) = 0));
     -- subroutines
     leadRow = (a,i) -> (
	  -- there are n rows
	  x := n-1;
	  while x >= 0 and getEntry(a,x,i) == 0 do x=x-1;
	  if x < 0 then x=n+1;
	  x);
     leadRow = (a,i) -> (
	  -- there are n rows
	  x := 0;
	  while x < n and getEntry(a,x,i) == 0 do x=x+1;
	  if x == n then x=n+1;
	  x);
     MINUS = (j) -> (
	  scan(2..m, r -> scan(1..r-1, s ->
		    if r === j or s === j then
		        lambda#(r,s) = - lambda#(r,s))));
     RED2 = (k,i) -> (
	  a1 := 0;
	  row1 = leadRow(A,i-1);
	  if row1 < n then (
	    a1 = getEntry(A,row1,i-1);
	    if a1 < 0 then (
	      MINUS(i);
	      cscale(A,-1,i-1);
	      cscale(B,-1,i-1)))
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
	       --see A;
	       --<< "caxy(" << -q << "," << i << "," << k << ")" << newline;
	       caxy(A,-q,i-1,k-1);
	       caxy(B,-q,i-1,k-1);
	       lambda#(k,i) = lambda#(k,i) - q * D#i;
	       scan(1..i-1, j -> 
		    lambda#(k,j) = lambda#(k,j) - q * lambda#(i,j)));
	  );
     SWAP2 = (k) -> (
	  cflip(A,k-1,k-2);
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
     count := -1;
     while k <= m do (
	  if verbose>0 then (
	      << "." << k << flush;
	      if count % 15 === 0 then (<< newline););
	  count = count-1;
	  RED2(k,k-1);
	  if row1 <= min(row2,n) or (row1 === row2 and row1 === n+1 and
	       alphaDenominator * (D#(k-2) * D#k + lambda#(k,k-1)^2) < 
	       alphaNumerator * D#(k-1)^2)
	  then (
	       SWAP2(k);
	       if k > 2 then k = k-1;
	       )
	  else (
	       i := k-2;
	       while i >= 1 do (RED2(k,i); i=i-1);
	       k = k+1;
	       )
	  );
     {matrix A,matrix B}
     );

----------------
-- Engine LLL --
----------------
initLLL = (m) -> (
     sendgg(ggPush m, ggPush (3/4), ggPush 1, ggLLLinit);
     LLLstate := newSparseMatrix ZZ;
     A := newSparseMatrix ZZ;
     {A,LLLstate})

calcLLL = (A,LLLstate,nsteps) -> (
     sendgg(ggPush A, ggPush LLLstate, ggPush nsteps, ggLLLcalc);
     new ZZ)

testLLL = (m) -> (
  -- Test 1:
  remove(m,quote LLL);
  time m1 = matrix LLL(m,Engine=>true);
  --time assert(isLLL m1);
  -- Test 2:
  remove(m,quote LLL);
  time m2 = matrix LLL(m, Engine=>true,ChangeOfBasisMatrix=>true);
  assert(m1 == m2);
  h = matrix getColumnChange m.LLL.A;
  assert(m2 == m*h);
  -- Test 3:
  remove(m,quote LLL);
  time m3 = matrix LLL(m,Engine=>false);
  --time assert(isLLL m3);
  -- Test 4:
  remove(m,quote LLL);
  time m4 = matrix LLL(m, Engine=>false,ChangeOfBasisMatrix=>true);
  assert(m3 == m4);
  h2 = matrix getColumnChange m.LLL.A;
  assert(m3 == m*h2);
  --assert(h == h2);
  )

///
load "../e/LLL.m2"
m = transpose matrix{{1,1,1,1},{0,1,2,3}}  
AA = LLL(m,Engine=>true)
gbTrace 2
calcLLL(AA_0,AA_1,-1)

time gcdLLL {3,7,11}
arandomlist = apply(10,i->random 100000000)
time gcdLLL arandomlist
time agcdLLL arandomlist
time gcdLLL {116085838, 181081878, 314252913, 10346840}
time agcdLLL {116085838, 181081878, 314252913, 10346840}

mylist = {763836, 1066557, 113192, 1785102, 1470060, 3077752, 114793, 3126753, 1997137, 2603018}
time gcdLLL mylist
time agcdLLL mylist

  assert(matrix {mylist} * answer#1 == matrix{{0,0,0,0,0,0,0,0,0,1}})

alphaNumerator = 1
alphaDenominator = 1
answer = time gcdLLL(mylist,Threshold=>1)
assert(matrix {mylist} * answer#1 == matrix{{0,0,0,0,0,0,0,0,0,1}})
time agcdLLL mylist
  alphaNumerator = 101
  alphaDenominator = 400
  answer = time gcdLLL mylist
  assert(matrix {mylist} * answer#1 == matrix{{0,0,0,0,0,0,0,0,0,1}})

-----------------
-- hermiteLLL ---
-----------------
-- A simple one:
m = matrix{{1,1,1,1},{0,1,2,3}}  
mh = hermiteLLL m
mz = matrix getColumnChange m.hermiteLLL.A
assert(m * mz == matrix mh)

-- Test from Havas et al paper 1998:
m = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)
time mh = hermiteLLL m
mz = matrix getColumnChange m.hermiteLLL.A
assert(m * mz == matrix mh)

-- Random entries
m = matrix sparsemat(10,15,.9,5)
time mh = hermiteLLL m
mz = matrix getColumnChange m.hermiteLLL.A
assert(m * mz == matrix mh)

-- One that caused an error in an earlier version:
m = matrix {{0, 0, 0, 2, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {-4, 0, 0, 0, 0, 2, 0, -1, 0, 0, -1, 0, 0, 0, 0}, {-4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -5}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0}}
time mh = hermiteLLL m
mz = matrix getColumnChange m.hermiteLLL.A
assert(m * mz == matrix mh)

---------
-- LLL --
---------
m = matrix {{1, 0, 0}, {0, 0, 1}, {0, 1, -1}}
m1 = matrix LLL(m,ChangeOfBasisMatrix=>true)
mh = matrix getColumnChange m.LLL.A
assert(isLLL m1)
assert(m * mh == m1)

-- a good test, based on the Havas example
m1 = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)
m = syz m1
mz = LLL(m)
gbTrace 2
assert(m1 * matrix mz == 0)
assert(isLLL matrix mz)

remove(m,quote LLL)
mz = LLL(m,ChangeOfBasisMatrix=>true)
h = getColumnChange m.LLL.A
assert(m * matrix h == matrix mz)

testLLL = (m) -> (
  time m1 = matrix LLL m;
  time assert(isLLL m1);
  remove(m,quote LLL);
  time m2 = matrix LLL(m, ChangeOfBasisMatrix=>true);
  assert(m1 == m2);
  h = matrix getColumnChange m.LLL.A;
  assert(m2 == m*h);
  )

-- a random example
m = matrix sparsemat(10,10,.5,500)
testLLL m

m = matrix sparsemat(10,10,.5,5)
testLLL m

m = matrix sparsemat(20,15,.5,5)
testLLL m

m = matrix sparsemat(20,20,.5,5)
testLLL m

m = matrix sparsemat(30,30,.5,5)
testLLL m

m = matrix sparsemat(30,30,.5,500)
testLLL m

m = matrix sparsemat(30,30,.5,500000)
testLLL m

m = matrix sparsemat(40,40,.5,5)
testLLL m

----------------
-- kernelLLL ---
----------------
m = matrix{{1,1,1,1},{0,1,2,3}}  
kernelLLL m

m1 = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)
mz = kernelLLL m1
assert(m1 * mz == 0)
syz m1

m = matrix sparsemat(10,15,.5,5)
mz = kernelLLL m
isLLL oo
LLL mz
LLL syz m

m = matrix sparsemat(30,70,.5,5)
time mz = kernelLLL m
mz1 = time LLL mz
time syz m  -- Time is actually somewhat less that computing mz1.  Except that the size of the matrices
            -- is very large.

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
time m = syz M    -- time: 0.2 seconds
time m1 = kernelLLL M -- time: 21.5 seconds (front-end implementation) 
                      -- BUT: quality of output was very much better...
isLLL m1   -- NO
time LLL m1  -- 44.9 seconds (front-end implementation)
             -- Quality of output is higher still

gbTrace 1
hermiteLLL M -- LONG time

-----------------------------------------------------------
m = matrix{{1,1,1,1},{0,1,2,3}}  
m = syz m
ms = sparseMutableMatrix m
H = integralLLL(ms, 2)
m * H
isLLL oo
m = matrix{{1,0,0},{0,1,0},{0,1,1}}
gram m
isLLL m
integralLLL(sparseMutableMatrix m, 3)
isLLL oo
n = 5
m = sparsemat(n,n,.1,5)
mm = matrix m
gram mm
isLLL mm
integralLLL(m,5)

-- LLL test:
mmm = sparsemat(4,2,.1,5)


m = matrix sparsemat(30,30,.5,5)
time answer = hermiteLLL m
assert(m * answer#1 == answer#0)
time smith m
getsmith m
getcolchange m
///
