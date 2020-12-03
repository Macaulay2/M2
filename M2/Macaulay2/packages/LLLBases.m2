-- -*- coding: utf-8 -*-
newPackage("LLLBases",
     Version => "1.1", 
     Date => "July 7, 2005",
     Authors => {{Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"}},
     Headline => "Lenstra-Lenstra-Lovasz bases",
     Keywords => {"Algebraic Number Theory", "Linear Algebra"},
     DebuggingMode => false
     )

export{
     "Hermite",
     "LLL",
     "isLLL", 
     "kernelLLL",
     "hermite",
     "gcdLLL",
     "gramm",
     "Threshold",
     "NTL",
     "CohenEngine",
     "fpLLL",
     "CohenTopLevel",
     "Givens",
     "BKZ",
     "RealFP",
     "RealQP1",
     "RealQP",
     "RealXD",
     "RealRR"
     }

debug Core

-- these are used as keys, formerly borrowed from Core.Dictionary, now private symbols
protect A
protect D
protect F
protect k

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

reverseColumns = m -> m^(toList reverse (0 .. numgens target m - 1))

--------------------------------------------------
LLLstrategies = new HashTable from {
     NTL => 2,
     CohenEngine => 0,
     CohenTopLevel => 1,
     RealFP => 2 + 16,
     RealQP => 2 + 48,
     RealXD => 2 + 64,
     RealRR => 2 + 80,
     fpLLL => 4,
     {Givens,RealFP} => 6 + 16,
     {Givens,RealQP} => 6 + 48,  
     {Givens,RealXD} => 6 + 64,
     {Givens,RealRR} => 6 + 80,
     {BKZ,RealFP} => 10 + 16,
     {BKZ,RealQP1} => 10 + 32,
     {BKZ,RealQP} => 10 + 48,
     {BKZ,RealXD} => 10 + 64,
     {BKZ,RealRR} => 10 + 80,
     {BKZ,Givens,RealFP} => 14 + 16,
     {BKZ,Givens,RealQP} => 14 + 48,
     {BKZ,Givens,RealQP1} => 14 + 32,
     {BKZ,Givens,RealXD} => 14 + 64,
     {BKZ,Givens,RealRR} => 14 + 80
     }
--------------------------------------------------
round := (a,b) -> (2*a + b) // (2*b)

processLLLstrategy := (strat) -> (
     if instance(strat,List) then strat = sort strat;
     if LLLstrategies#?strat then LLLstrategies#strat
     else error("unknown Strategy option: "|strat))

protect threshold

setLLLthreshold := (result,alpha) -> (
     if class alpha =!= QQ then (
         if class alpha === ZZ then
	   alpha = alpha/1
	 else
	   error "bad argument");
     if alpha <= 1/4 or alpha > 1 then
         error "LLL threshold out of range (1/4,1]";
     result.threshold = alpha;)

LLL = method (
     Options => {
     	  Threshold => null,
     	  ChangeMatrix => false,
     	  Limit => infinity,
     	  Strategy => NTL
     	  })

protect changeOfBasis					    -- used as a key

LLL MutableMatrix :=
LLL Matrix := options -> (M) -> (
     -- First check that the base ring is ZZ
     -- Check that the threshold is in range
     if ring M =!= ZZ then
       error "expected a matrix over the integers";
     m := mutableMatrix M;
     mchange := if options.ChangeMatrix then
     	          iden(ring m, numcols m)
                else
		  null;
     strat := processLLLstrategy options.Strategy;
     thresh := if options.Threshold === null 
     then (if strat >= 3 then 99/100 else 3/4)
     else options.Threshold;
     if mchange === null then (
     	  result := if strat =!= 1 then (
	       rawLLL(m.RawMutableMatrix, null, thresh, strat);
	       matrix map(ZZ,m.RawMutableMatrix)
	       )
     	  else (
	       C := newLLLComputation(m,null,thresh);
	       doLLL(C, options.Limit);
	       matrix C.A
     	       );
	  result)
     else (
	  mchange = iden(ring m, numcols m);
     	  result = if strat =!= 1 then (
	       rawLLL(m.RawMutableMatrix, mchange.RawMutableMatrix, thresh, strat);
	       (matrix map(ZZ,m.RawMutableMatrix),
		    matrix map(ZZ,mchange.RawMutableMatrix))
	       )
     	  else (
	       C = newLLLComputation(m,mchange,thresh);
	       doLLL(C, options.Limit);
	       (matrix C.A, matrix C.changeOfBasis)
     	       );
	  result)
     )

----------------------------------
-- LLL computations at top level--
----------------------------------
if LLLComputation === symbol LLLComputation
then LLLComputation = new Type of MutableHashTable

protect kmax
protect lambda

newLLLComputation = (m, mchange,threshold) -> (
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
     result.changeOfBasis = mchange;
     n := numcols result.A;
     setLLLthreshold(result,threshold);
     result.k = 2;
     result.kmax = 1;
     result.D = new MutableList from toList (n+1:0);
     result.lambda = new MutableHashTable;
     result.D#0 = 1;
     result.D#1 = dot(result.A,0,0);
     result)

protect isDone

doLLL = (C,count) -> (
     A := C.A;
     Achange := C.changeOfBasis;
     n := numcols A;
     lambda := C.lambda;
     D := C.D;
     alphaTop := numerator C.threshold;
     alphaBottom := denominator C.threshold;
     REDI := (k,ell) -> if abs(2*lambda#(k,ell)) > D#ell then (
	  q := (2*lambda#(k,ell) + D#ell) // (2*D#ell);
     	  caxy(A,-q,ell-1,k-1);
	  if Achange =!= null then 
	    caxy(Achange,-q,ell-1,k-1);
	  --<< "[" << k-1 << " " << -q << " " << ell-1 << "]\n";
	  lambda#(k,ell) = lambda#(k,ell) - q*D#ell;
	  scan(1..ell-1, i-> lambda#(k,i) = lambda#(k,i) - q*lambda#(ell,i));
	  );
     SWAPI := (k) -> (
	  cflip(A,k-1,k-2);
	  if Achange =!= null then 
	    cflip(Achange,k-1,k-2);
	  --<< "[flip " << k-1 << " " << k-2 << "]\n";
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
	       j := 1;
	       while j <= k do (
		    u := dot(A,k-1,j-1);
		    i := 1;
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
		    ell := k-2;
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
gramm = method()
gramm Matrix := (m) -> (
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

-- This next function needs to be tested, or removed (9 Sep 2010 MES)
-- I don't think we really need it, except maybe to avoid using gramm above
gramMultipliers = method()
gramMultipliers Matrix := (M) -> matrix gramMultipliers mutableMatrix M
gramMultipliers MutableMatrix := (B) -> (
     -- B is a matrix with n columns.
     -- Returned: the lower triangular matrix of lambda's and D's.
     -- These are the integral values, see Cohen, pg 92.
     R := ring B;
     n := numColumns B;
     A := mutableIdentity(ZZ,n);
     for i from 0 to n-1 do
       for j from 0 to i do (
	  u := dot(B,i,j);
	  for k from 0 to j-1 do (
	       d := if k === 0 then 1_R else A_(k-1,k-1);
	       u = A_(k,k) * u - A_(i,k) * A_(j,k) // d;
	       );
	  A_(i,j) = u);
     A
     )

isLLL = method(Options => {Threshold => 3/4})

isLLL Matrix := options -> (m) -> (
     LLLalpha := options.Threshold;
     (B, mu) := gramm m;
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
     result.changeOfBasis = iden(ring m, n);
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
     U := C.changeOfBasis;
     D := C.D;
     lambda := C.lambda;
     F := C.F;
     k := C.k;
     kmax := C.kmax;
     n := numcols A;
     REDI := (k,ell) -> if abs(2*lambda#(k,ell)) > D#ell then (
	  q := (2*lambda#(k,ell) + D#ell) // (2*D#ell);
	  caxy(A,-q,ell-1,k-1);
	  caxy(U,-q,ell-1,k-1);
	  lambda#(k,ell) = lambda#(k,ell) - q*D#ell;
	  scan(1..ell-1, i-> lambda#(k,i) = lambda#(k,i) - q*lambda#(ell,i));
	  );
     SWAPK := (k) -> (
	  cflip(A,k-1,k-2);
	  cflip(U,k-1,k-2);
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
     -- << newline;
     if k > n then C.isDone = true;
     )

kernelLLL = method (Options => { Limit => -1 })

kernelLLL Matrix := options -> (M) -> (
     C := newLLLKernelComputation M;
     doKernelLLL(C, options.Limit);
     r := 1;
     n := numcols C.A;
     while r <= n and not C.F#r do r=r+1;
     (matrix C.changeOfBasis)_{0..r-2}
     )

----------------
-- hermite --
----------------
-- This algorithm is from:
--   Havas, Majewski, Matthews, 
--   Extended GCD and Hermite Normal Form Algorithms via Lattice Basis Reduction,
--   Experimental Mathematics 7:2 p. 125 (1998)
--
-- Computes a Hermite normal form, along with a kernel, and small multiplier matrix.

if LLLHermiteComputation === symbol LLLHermiteComputation
then LLLHermiteComputation = new Type of MutableHashTable

protect nrows						    -- used as a key

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
     result.changeOfBasis = if hasChangeOfBasis then
         iden(ring m, n) else null;
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
     U := C.changeOfBasis;
     m := numcols A;
     lambda := C.lambda;
     D := C.D;
     alphaTop := numerator C.threshold;
     alphaBottom := denominator C.threshold;
     n := C.nrows;
     row1 := 0;
     row2 := 0;
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
	      cscale(A,-1,i-1);
	      if U =!= null then cscale(U,-1,i-1);
	      ))
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
	       if U =!= null then caxy(U,-q,i-1,k-1);
	       lambda#(k,i) = lambda#(k,i) - q * D#i;
	       scan(1..i-1, j -> 
		    lambda#(k,j) = lambda#(k,j) - q * lambda#(i,j)));
	  );
     SWAP2 := (k) -> (
	  cflip(A,k-1,k-2);
	  if U =!= null then cflip(U,k-1,k-2);
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

hermite = method (Options => {
     	  Threshold => 3/4,
     	  ChangeMatrix => false,
     	  Limit => -1,
     	  Strategy => LLL
     	  })

hermite Matrix := opts -> (M) -> (
     if ring M =!= ZZ then error "hermite: expected matrix over ZZ";
     if opts.Strategy === LLL then (
	  -- Possible opts: Threshold=>QQ, ChangeMatrix,
	  -- and for the specific computation: 
	  C := newLLLHermiteComputation(reverseColumns M,opts.Threshold, opts.ChangeMatrix);
	  doHermiteLLL(C, opts.Limit);     
	  if opts.ChangeMatrix then (reverseColumns matrix C.A, matrix C.changeOfBasis)
	  else reverseColumns matrix C.A
	  )
     else error "hermite: expected Strategy => LLL")

------------
-- gcdLLL --
------------

gcdLLL = method(Options => {
     	  Strategy => null,
     	  Threshold => 3/4
     	  })

--gcdLLL Matrix := options -> (M) -> (
--     -- Possible options: Threshold=>QQ, ChangeMatrix,
--     -- and for the specific computation: 
--     (m,u) := hermite(M,options,ChangeMatrix=>true);
--     (getEntry(m,0,numcols m-1), matrix u)
--     )

bgcdLLL = (s,thresh) -> (
     M := matrix{s};
     (m,u) := hermite(M,Threshold=>thresh,ChangeMatrix=>true);
     (m_(0,numgens source m - 1), u)
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

agcdLLL = (s,thresh) -> (
     alphaNumerator := numerator thresh;
     alphaDenominator := denominator thresh;
     m := #s;
     B := iden(ZZ,m);
     D := new MutableList from toList(m+1:1);
     a := new MutableList from prepend(0,s);
     lambda := new MutableHashTable;
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
     k := 2;
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

--gcdLLL List := options -> (s) -> (
--     if options.Strategy =!= Hermite
--     then agcdLLL(s,options.Threshold)
--     else (
--     	  m := matrix{s};
--     	  gcdLLL(m,options))
--     )


gcdLLL List := options -> (s) -> (
     if options.Strategy === Hermite
     then bgcdLLL(s,options.Threshold)
     else agcdLLL(s,options.Threshold))

addHook(Module, symbol resolution, (o,M) -> if ring M === ZZ then break chainComplex compress LLL presentation M)

addHook(Module, symbol minimalPresentation, (o,M) -> (
	  return null;					    -- this routine isn't correct yet -- it only partially minimizes; we could use this for trim and improve it for this
	  R := ring M;
	  if R === ZZ then (
	       h := presentation M;
	       p := hermite h;				    -- check the column ordering, too!
	       m := rank target p;
	       n := rank source p;
	       piv := pivots p;
	       rows' := toList(0 .. m-1) - set(first\piv);
	       cols' := toList(0 .. n-1) - set(last\piv);
	       p' := p^rows'_cols';
	       N := coker p';
	       pm := N.cache.pruningMap = map(M,N,id_(target p)_rows');
	       if debugLevel > 5 then (
		    stderr
		    << "-- M    = " << M << endl
		    << "-- h    = " << h << endl
		    << "-- p    = " << p << endl
		    << "-- piv  = " << piv << endl
		    << "-- rows'= " << rows' << endl
		    << "-- cols'= " << cols' << endl
		    << "-- p'   = " << p' << endl
		    << "-- N    = " << N << endl
		    << "-- pm   = " << pm << endl;
     	       	    assert( isIsomorphism pm );
		    );
	       break N)))

beginDocumentation()

document { 
	Key => LLLBases,
	Headline => "lattice reduction (Lenstra-Lenstra-Lovasz bases)",
	EM "LLLBases", " is a package implementing several variants of LLL bases.
	Some of these are implemented in the Macaulay2 engine, some by Victor Shoup's NTL package,
	and some are implemented at top level.",
	PARA{},
	"A matrix over ZZ determines a lattice: this is the ZZ-module generated by the columns.  It
	also determines a generating set. See the book: [H. Cohen, ...] for the definition and
	the basic algorithms for computing LLL bases.",
   	PARA{},
	"LLL bases have nice theoretical properties, but the main benefit of LLL bases is that
	the entries of the resulting matrix often have dramatically small size, even smaller than
	theory would imply.",
	PARA{},
	"This package implements the following functions.",
	Subnodes => {
	     "main LLL algorithm",
	     TO LLL,
	     "applications and variants",
	     TO kernelLLL,
	     TO gcdLLL,
	     TO hermite,
	     "support routines that are occasionally useful",
	     TO gramm,
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
     PARA{},
     "This function is provided by the package ", TO LLLBases, ".  If the optional argument
     ", TT "ChangeMatrix=>true", " is given, then the output is a pair of matrices: the first is the
     LLL matrix as above, and the second is the change of basis matrix from the original basis to the new basis.",
     PARA{},
     "In this example, we compute the LLL basis of the nullspace of a matrix.  This is 
     an example of Havas et al.",
     EXAMPLE {
	  "m1 = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)",
	  "m = syz m1",
	  "LLL m"
	  },
     "It is also possible to get the change of basis 
     matrix from the original basis to the LLL basis.  For example,",
     EXAMPLE {
	  "(n,c) = LLL(m, Strategy => NTL, ChangeMatrix=>true)",
	  "m * c == n"
	  },
     Caveat => {"If the strategy given is not an NTL strategy, then the columns of the matrix m must be linearly independent.",
	  "In any case, the matrix must be defined over the ring ZZ."},
     SeeAlso => {isLLL, gcdLLL, kernelLLL, hermite}
     }
document { 
     Key => [LLL, ChangeMatrix],
     Headline => "also find change of basis matrix",
     Usage => "(B,U) = LLL(A,ChangeMatrix=>true)",
     Inputs => {
	  "A" => Matrix => "over the integers, of size d by n",
	  },
     Outputs => {
	  "B" => Matrix => "the LLL matrix (also having d rows)",
	  "U" => Matrix => "the n by n invertible transform matrix"
	  },
     Consequences => {
	  "The routine returns a pair of matrices, rather than just one"
	  },     
     "Constructs the change of basis matrix U from the basis A to the basis
     B. This is an invertible matrix U such that", TEX "$AU = B$", ".",
     EXAMPLE {
	  "A = matrix randomMutableMatrix(10,10,.5,100000)",
	  "(B,U) = LLL(A, ChangeMatrix=>true)",
	  "B == A*U",
	  },
     SeeAlso => {LLLBases}
     }
document { 
     Key => [LLL, Strategy],
     Headline => "choose among different algorithms",
     Usage => "LLL(...,Strategy=>n)",
     Inputs => {
	  "The strategy n can be one of the symbols or lists
	  given below."
	  },
     "There are several variants of the LLL reduction algorithm
     implemented.  There are three all integer versions: ", TT "NTL",
     ", ", TT "CohenEngine", ", and ", TT "CohenTopLevel",".
     The NTL version 
     (NTL is an excellent package written by Victor shoup) is
     generally the best, however, the top level version is written
     in the Macaulay2 language, and so is easily modifiable and
     can be used to understand the algorithm better.  There are also
     a number of approximate LLL variants implemented in NTL.  These
     use real numbers instead of exact integer arithmetic, and so are
     often much faster, but only provide approximate answers (i.e. the
     result might not be an LLL basis, only close to one).  Much of the
     information here about NTL's algorithms comes directly from the NTL 
     documentation (translated to be relevant here).",
     PARA{},
     "Here is the complete list of possible strategies:",
     UL {
	  "LLL(m, Strategy => NTL)",
	  "LLL(m, Strategy => CohenEngine)",
 	  "LLL(m, Strategy => CohenTopLevel)",
	  HR{},
	  "LLL(m, Strategy => RealFP)",
	  "LLL(m, Strategy => RealQP)",	  
	  "LLL(m, Strategy => RealXD)",
	  "LLL(m, Strategy => RealRR)",
	  HR{},
	  "LLL(m, Strategy => {Givens,RealFP})",
	  "LLL(m, Strategy => {Givens,RealQP})",	  
	  "LLL(m, Strategy => {Givens,RealXD})",
	  "LLL(m, Strategy => {Givens,RealRR})",
	  HR{},
	  "LLL(m, Strategy => {BKZ,RealFP})",
	  "LLL(m, Strategy => {BKZ,RealQP})",
	  "LLL(m, Strategy => {BKZ,RealQP1})",
	  "LLL(m, Strategy => {BKZ,RealXD})",
	  "LLL(m, Strategy => {BKZ,RealRR})",
	  HR{},
	  "LLL(m, Strategy => {BKZ,Givens,RealFP})",
	  "LLL(m, Strategy => {BKZ,Givens,RealQP})",
	  "LLL(m, Strategy => {BKZ,Givens,RealQP1})",
	  "LLL(m, Strategy => {BKZ,Givens,RealXD})",
	  "LLL(m, Strategy => {BKZ,Givens,RealRR})",
	  },     
     "The first three are similar all-integer algorithms, 
     basically the one which 
     appears in H. Cohen's book.  The rest of the algorithms are
     approximate variants, provided by Victor Shoup's NTL package.
     For these, there are three choices to be made: 
     (1) the reduction condition, (2) the choice of orthogonalization
     strategy, and (3) the choice of precision.",
     SUBSECTION "Reduction condition",
     UL {
	  "default -- the classical LLL reduction condition",
	  {"BKZ -- Block Korkin-Zolotarev reduction.",
     	  "This is slower, but yields a higher-quality basis,
     	  i.e., one with shorter vectors.
     	  For a description, see 
     [C. P. Schnorr and M. Euchner, Proc. Fundamentals of Computation Theory, 
     LNCS 529, pp. 68-85, 1991].  
     	  This basically generalizes the LLL reduction condition
     	  from blocks of size 2 to blocks of larger size."}
     },
     SUBSECTION "Orthogonalization Strategy",
     UL {
  	  {"default -- Classical Gramm-Schmidt Orthogonalization, ",
     "This choice uses classical methods for computing
     the Gramm-Schmidt othogonalization.
     It is fast but prone to stability problems.
     This strategy was first proposed by Schnorr and Euchner in the paper
     mentioned above.
     The version implemented here is substantially different, improving
     both stability and performance."},
          {"Givens -- Givens Orthogonalization, ",
     "This is a bit slower, but generally much more stable,
     and is really the preferred orthogonalization strategy.
     For a nice description of this, see Chapter 5 of  
     [G. Golub and C. van Loan, Matrix Computations, 3rd edition,
     Johns Hopkins Univ. Press, 1996]."}
     },
     SUBSECTION "Precision",
     UL {
  	  "RealFP -- double",
  	  "RealQP -- quad_float (quasi quadruple precision)
             useful when roundoff errors can cause problems",
	  "RealQP1 -- only availabel in the BKZ variant, uses
	     double precision for the search phase of the BKZ
	     reduction, and quad_float for the orthogonalization",
  	  "RealXD -- xdouble (extended exponent doubles)
             useful when numbers get too big",
  	  "RealRR -- RR (arbitrary precision floating point)
             useful for large precision and magnitudes"
	  },
     "Generally speaking, the choice RealFP will be the fastest,
     but may be prone to roundoff errors and/or overflow.",
     SUBSECTION "Putting it all together",
     "This subsection comes directly from Victor Shoup's LLL
     documentation",
     PARA{},
"I think it is safe to say that nobody really understands
how the LLL algorithm works.  The theoretical analyses are a long way
from describing what \"really\" happens in practice.  Choosing the best
variant for a certain application ultimately is a matter of trial
and error.",
PARA{},
"The first thing to try is ", TT "Strategy => RealFP", ".
It is the fastest of the routines, and is adequate for many applications.",
PARA{},
"If there are precision problems, you will most likely get
a warning message, something like \"warning--relaxing reduction\".
If there are overflow problems, you should get an error message
saying that the numbers are too big.",
PARA{},
"If either of these happens, the next thing to try is 
", TT "Strategy=>{Givens,RealFP}",
", which uses the somewhat slower, but more stable, Givens rotations.
This approach also has the nice property that the numbers remain
smaller, so there is less chance of an overflow.",
PARA{},
"If you are still having precision problems
try ", TT "Strategy=>RealQP", " or ", TT "Strategy=>{Givens,RealQP}",
", which use quadratic precision.",
PARA{},
"If you are still having overflow problems,
try ", TT "Strategy=>RealXD", " or ", TT "Strategy=>{Givens,RealXD}",
PARA{},
"I haven't yet come across a case where one *really* needs the
extra precision available in the RealRR variants.",
PARA{},
"All of the above discussion applies to the ", TT "BKZ", " variants as well.
In addition, if you have a matrix with really big entries, you might try 
using ", TT "Strategy=>{Givens,RealFP}", " or ", TT "Strategy=>RealXD",
" first to reduce the sizes of the numbers,
before running one of the ", TT "BKZ", " variants.",
PARA{},
"Also, one shouldn't rule out using the \"all integer\" LLL routines.
For some highly structured matrices, this is not necessarily
much worse than some of the floating point versions, and can
under certain circumstances even be better.",
EXAMPLE {
	  "m1 = map(ZZ^50, ZZ^50, (j,i) -> (i+1)^8 * (j+1)^4 + i + j + 2);",
	  "m = syz m1;",
	  "time LLL m;",
	  "time LLL(m, Strategy=>CohenEngine);",
	  "time LLL(m, Strategy=>CohenTopLevel);",
	  "time LLL(m, Strategy=>{Givens,RealFP});",
	  "time LLL(m, Strategy=>{Givens,RealQP});",
	  "time LLL(m, Strategy=>{Givens,RealXD});",
	  "time LLL(m, Strategy=>{Givens,RealRR});",
     	  -- this one takes too long:
	  -- "time LLL(m, Strategy=>{BKZ,Givens,RealFP});",
	  "time LLL(m, Strategy=>{BKZ,Givens,RealQP});",
     	  -- this one takes too long:
	  -- "time LLL(m, Strategy=>{BKZ,Givens,RealXD});",
     	  -- maybe this one takes too long:
	  -- "time LLL(m, Strategy=>{BKZ,Givens,RealRR});"
	  },
     Caveat => {"For most of the options, the columns do not need to be 
	  linearly independent.  The strategies CohenEngine and CohenTopLevel
	  currently require the columns to be linearly independent."},
     SeeAlso => {LLLBases}
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
     PARA{},
     "If the matrix is not in LLL reduced form, then the offending conditions are
     displayed.  For example,",
     EXAMPLE {
	  "m = matrix {{1, 0}, {1, 1}, {1, 2}, {1, 3}}",
	  "isLLL m",
	  "n = LLL m",
	  "isLLL n"
	  },
     "If the optional argument Threshold is given, the conditions are checked using that value.",
     EXAMPLE {
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
     SeeAlso => {LLL, gcdLLL, kernelLLL, hermite}
     }

document {
     Key => {gcdLLL,(gcdLLL,List)},
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
     PARA{},
     "The first n-1 columns of the matrix z form a basis of the kernel
     of the n integers of the list s, and the dot product of the last column of z 
     and s is the gcd g.",
     PARA{},
     "The method used is described in the paper:",
     PARA{},
     "Havas, Majewski, Matthews, ", EM"Extended GCD and Hermite Normal Form Algorithms via Lattice Basis Reduction",
     ", Experimental Mathematics 7:2 p. 125 (1998).",
     PARA{},
     "For an example,",
     EXAMPLE {
	  "s = apply(5,i->372*(random 1000000))",
	  "(g,z) = gcdLLL s",
	  "matrix{s} * z"
	  },
     SeeAlso => {LLLBases, LLL, kernelLLL, hermite}
     }

document {
     Key => NTL,
     Headline => "use the all-integer LLL strategy from NTL library",
     TT "NTL", " -- the default strategy value for ", TO [LLL,Strategy], " used to specify
     that the all-integer LLL algorithm from Victor Shoup's NTL library should be used.
     Overall, the performance of this algorithm is very good in many situations."
     }
document {
     Key => CohenEngine,
     Headline => "use the original Macaulay2 LLL algorithm",
     TT "CohenEngine", " -- a strategy value for ", TO [LLL,Strategy], " used to specify
     that the all-integer LLL algortithm from H.Cohen's book (with improvements
	  by Euchner and Schnorr) should be used.  This is basically the same 
     algorithm as ", TT "Strategy=>NTL", ", but is often outperformed by that
     algorithm."
     }
document {
     Key => CohenTopLevel,
     Headline => "use the Macaulay2 language LLL algorithm",
     TT "CohenTopLevel", " -- a strategy value for ", TO [LLL,Strategy], " used to specify
     that the all-integer LLL algortithm from H.Cohen's book (with improvements
	  by Euchner and Schnorr) should be used, as coded in the front-end of Macaulay2.  
     This is basically the same 
     algorithm as ", TT "Strategy=>CohenEngine", ", but is written at top level, so that 
     it is relatively easy to adapt the code to other situations."
     }
document {
     Key => Givens,
     Headline => "use Givens rotations instead of Gram-Schmidt during LLL",
     TT "Givens", " -- a strategy value for ", TO [LLL,Strategy], " used to specify
     that Givens rotations should be used instead of the usual (but inferior)
     Gram-Schmidt orthogonalization.  Only valid for approximate LLL computations."
     }
document {
     Key => BKZ,
     Headline => "compute BKZ reduced basis instead of an LLL reduced basis",
     TT "BKZ", " -- a strategy value for ", TO [LLL,Strategy], " used to specify
     that a BKZ basis (see ", TO [LLL,Strategy], " for references) should be constructed
     rather than an (approximate) LLL basis.  Apparently produces higher quality 
     bases, but can be slower."
     }
document {
     Key => RealFP,
     Headline => "use double precision real numbers",
     TT "RealFP", " -- a strategy value for ", TO [LLL,Strategy], " used to specify
     that an approximate LLL basis will be constructed, using 
     double precision."
     }
document {
     Key => RealQP,
     Headline => "use quadruple precision real numbers",
     TT "RealQP", " -- a strategy value for ", TO [LLL,Strategy], " used to specify
     that an approximate LLL basis will be constructed, using 
     double precision."
     }
document {
     Key => RealQP1,
     Headline => "use a combination of double precision and quad precision real numbers",
     TT "RealQP1", " -- a strategy value for ", TO [LLL,Strategy], " used to specify
     that an approximate LLL basis will be constructed, using 
     double precision.  Only valid for the BKZ reduction condition."
     }
document {
     Key => RealXD,
     Headline => "use extended exponent real numbers",
     TT "RealXD", " -- a strategy value for ", TO [LLL,Strategy], " used to specify
     that an approximate LLL basis will be constructed, using 
     extended exponent real numbers.  Useful if the integer entries are very large."
     }
document {
     Key => RealRR,
     Headline => "use arbitrary precision real numbers",
     TT "RealRR", " -- a strategy value for ", TO [LLL,Strategy], " used to specify
     that an approximate LLL basis will be constructed, using 
     arbitrary precision real numbers."
     }
document {
     Key => Threshold,
     Headline => "the LLL threshold, in interval (1/4,1]",
     TT "Threshold", " -- an optional value to ", TO LLL, " used to specify
     the threshold value, see ", TO [LLL,Threshold], "."
     }

------------------------------------------
--------- Tests --------------------------
------------------------------------------

---------------------------------------
-- LLL engine and frontend version-----
---------------------------------------

TEST ///
f = matrix {
     {13650502, 198662, -1514226}, {-528389638951, -7688266050, 58613349522}, {1819050, 26473, -201784},
     {-34721130542, -505205335, 3851555009}, {13943863165, 202888407, -1546768644}, {112371429966, 1635046125, -12465168534}}
h = hermite f
b = gens gb h
-- hermite and gb used to do the same thing, but now gb reduces off-diagonal elements to a range of -d/2...d/2 instead of 0..d:
assert(b * matrix {{1,1,1},{0,1,0},{0,0,1}} == h)
assert isIsomorphism map(coker f, coker h, id_(target f))
(k,c) = hermite(f,ChangeMatrix => true)
assert( k == h )
assert isIsomorphism map(image f, image h, c)
///


TEST ///

    time m1 = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)
    time m = syz m1
    time mz = LLL(m, Strategy=>CohenTopLevel)
    time assert(m1 * matrix mz == 0)
    time assert(isLLL matrix mz)
    
    time mz2 = LLL(m, Strategy=>CohenEngine, Threshold => 101/400)    
    assert( not isLLL matrix mz2 )
    assert isLLL(matrix mz2, Threshold=>101/400)

    time mz3 = LLL(m, Strategy=>CohenTopLevel)
    time assert(m1 * matrix mz3 == 0)
    time assert(isLLL matrix mz3)
    mz == mz3 -- not always true.  Why not?

    assert(hermite mz == hermite mz3)
    
    time mz4 = LLL(m, Strategy=>CohenTopLevel, Threshold => 101/400)    
    assert( not isLLL matrix mz4 )
    assert isLLL(matrix mz4, Threshold=>101/400)
    
    time (mz,ch) = LLL(m, ChangeMatrix=>true)
    time (mz5,ch5) = LLL(m, Strategy=>CohenTopLevel, ChangeMatrix=>true)
///

TEST 
/// -- DON'T TEST YET??
     testLLL = (m) -> (
  	  -- Test 1:
  	  remove(m.cache,symbol LLL);
  	  time m1 = matrix LLL(m,Engine=>true);
  	  time assert(isLLL m1);
  	  -- Test 2:
  	  remove(m.cache,symbol LLL);
  	  time m2 = matrix LLL(m, Engine=>true,ChangeMatrix=>true);
  	  assert(m1 == m2);
  	  h = matrix getColumnChange m2;
  	  assert(m2 == m*h);
  	  -- Test 3:
  	  remove(m.cache,symbol LLL);
  	  time m3 = matrix LLL(m,Engine=>false);
  	  --time assert(isLLL m3);
  	  -- Test 4:
  	  remove(m.cache,symbol LLL);
  	  time m4 = LLL(mutableMatrix m, Engine=>false,ChangeMatrix=>true);
  	  assert(m3 == m4);
  	  h2 = matrix getColumnChange m4;  -- FAILS
  	  assert(m3 == m*h2);
  	  assert(h == h2);
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
    hermite mz
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
	  j1 := j % nn;
	  j2 := j // nn;
	  if Sn#i#j1 == j2 then 1 else 0))
    M = transpose M
    time m = syz M    -- time: 0.03 seconds
    time m1 = kernelLLL M -- time: 1.98 seconds (front-end implementation) 
                      -- BUT: quality of output was very much better...
    assert not isLLL m1   -- NO
    time LLL m1;  -- .76 sec 44.9 seconds (front-end implementation), Quality of output is higher still
    time LLL(m1, Strategy=>CohenEngine); -- 
    time LLL(m1, Strategy=>CohenTopLevel); -- 
    time LLL(m1, Strategy=>{BKZ,RealFP}); -- 
    time LLL(m, Strategy=>{BKZ,RealFP}); --
    --time LLL(m, Strategy=>{BKZ,RealRR}); -- 4.9 sec (in 2015)
    time LLL(m); 

    assert isLLL (a = time LLL(m, Strategy=>NTL))
    assert isLLL (a = time LLL(m, Strategy=>CohenEngine))
    assert isLLL (a = time LLL(m, Strategy=>CohenTopLevel))
    
    assert isLLL (a = time LLL(m, Strategy=>RealFP))
    assert isLLL (a = time LLL(m, Strategy=>RealQP))
    assert isLLL (a = time LLL(m, Strategy=>RealXD))
    assert isLLL (a = time LLL(m, Strategy=>RealRR)) -- many times slower than all other variants

    assert isLLL (a = time LLL(m, Strategy=>{Givens,RealFP}))
    assert isLLL (a = time LLL(m, Strategy=>{Givens,RealQP}))
    assert isLLL (a = time LLL(m, Strategy=>{Givens,RealXD}))
    --assert isLLL (a = time LLL(m, Strategy=>{Givens,RealRR})) -- really slow

    assert isLLL (a = time LLL(m, Strategy=>{BKZ,Givens,RealFP}))
    assert isLLL (a = time LLL(m, Strategy=>{BKZ,Givens,RealQP}))
    assert isLLL (a = time LLL(m, Strategy=>{BKZ,Givens,RealQP1}))    
    assert isLLL (a = time LLL(m, Strategy=>{BKZ,Givens,RealXD}))
    --assert isLLL (a = time LLL(m, Strategy=>{BKZ,Givens,RealRR})) -- too slow

    assert isLLL (a = time LLL(m, Strategy=>{BKZ,RealFP}))
    assert isLLL (a = time LLL(m, Strategy=>{BKZ,RealQP}))
    assert isLLL (a = time LLL(m, Strategy=>{BKZ,RealQP1}))    
    assert isLLL (a = time LLL(m, Strategy=>{BKZ,RealXD}))
    --assert isLLL (a = time LLL(m, Strategy=>{BKZ,RealRR})) -- too slow
///

TEST ///
  -- trivial and edge cases
  m = random(ZZ^0, ZZ^5)
  a = LLL m
  assert(numRows a == 0)
  --assert(numColumns a == 0) -- what is the correct value here?
  assert(a == 0)  

  m = random(ZZ^5, ZZ^0)
  a = LLL m
  assert(numRows a == 5)
  assert(numColumns a == 0) -- no, what is the correct value here?
  assert(a == 0)

  m = matrix{{1}}  
  a = LLL m
  assert (a == m)
///
TEST ///
    setRandomSeed 0
    m0 = random(ZZ^20, ZZ^30, Height=>100000)
    m1 = syz m0;
    m = m1;

    setRandomSeed 0
    m0 = random(ZZ^40, ZZ^50, Height=>10000)
    m1 = syz m0;
    m2 = kernelLLL m0;
    m = m1;
    m = m2;

    time LLL(m1);
    ---time LLL(m1, Strategy=>fpLLL);
    -- TODO: run these on all variants that work
///

-------------------
-- hermite -----
-------------------
TEST ///

    time m1 = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)
    a = hermite m1
    (b,c) = hermite(m1,ChangeMatrix=>true) -- comes out in a different format than 'gb'
    assert(a == b)
    assert(m1*c == b)
///

TEST ///
    -- A simple one:
    m = matrix{{1,1,1,1},{0,1,2,3}}  
    mh = hermite m
    (mh1,mz) = hermite(m,ChangeMatrix=>true)
    assert(m * mz == mh)
///

TEST ///
    -- Test from Havas et al paper 1998:
    m = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)
    time (mh,mz) = hermite(m, ChangeMatrix=>true)
    assert(m * mz == mh)
///

TEST ///
    -- Random entries
    m = matrix randomMutableMatrix(10,15,.9,5)
    time (mh,mz) = hermite(m, ChangeMatrix=>true)
    assert(m * mz == mh)
///

TEST ///
    -- Random entries
    m = matrix randomMutableMatrix(20,35,.9,5)
    time (mh,mz) = hermite(m, ChangeMatrix=>true)
    assert(m * mz == mh)
///

TEST ///
    -- Random entries
    m = matrix randomMutableMatrix(20,35,.1,10)
    time (mh,mz) = hermite(m, ChangeMatrix=>true)
    assert(m * mz == mh)
    det mz
///

TEST ///
    -- One that caused an error in an earlier version:
    m = matrix {{0, 0, 0, 2, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {-4, 0, 0, 0, 0, 2, 0, -1, 0, 0, -1, 0, 0, 0, 0}, {-4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -5}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0}}
    time (mh,mz) = hermite(m, ChangeMatrix=>true)
    assert(m * mz == mh)
///


-------------------
-- gcdLLL ---------
-------------------
TEST ///

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
///

TEST ///
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
-- gramm, isLLL ----
-------------------
TEST ///
    time m1 = map(ZZ^10, ZZ^10, (j,i) -> (i+1)^3 * (j+1)^2 + i + j + 2)
    time m = syz m1
    time mz = LLL(m)
    (gm, H) = gramm mz 
    gm = matrix gm
    h = (transpose gm) * gm -- should be diagonal:
    scan(numgens source h, i -> scan(numgens source h, j -> 
	      assert(i === j or h_(i,j) == 0)))
///

TEST ///
     debugLevel = 10
     scan(2 .. 7, d -> scan(5, i -> scan(-1 .. 1, e -> (
			 f := random(ZZ^d, ZZ^(d+e));
			 prune coker f;
			 prune image f;
			 prune subquotient(f, f * random(ZZ^(d+e), ZZ^(d+e-1)));
			 prune subquotient(f, random(ZZ^d, ZZ^(d-1)));
			 ))));
///

end

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
     Key => {gramm,(gramm,Matrix)},
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
	  ///
	  A = map(ZZ^10, ZZ^7, (i,j) -> if random 1.0 > .2 then random 1000 else 0)
	  hermite A
	  (B,U) = hermite(A, ChangeMatrix=>true)
	  A*U == B
	  ///
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
     Key => {hermite,(hermite,Matrix)},
     Headline => "compute the Hermite normal form and small multiplier matrix using LLL bases",
     Usage => "hermite m",
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
     Key => [hermite, Threshold],
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
     Key => [hermite, Limit],
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
     Key => [hermite, ChangeMatrix],
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


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=LLLBases pre-install"
-- End:
