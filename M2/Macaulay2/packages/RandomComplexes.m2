///
restart
uninstallPackage "RandomComplexes"
restart
installPackage "RandomComplexes"
check("RandomComplexes", UserMode=>true)
loadPackage("RandomComplexes", Reload=>true)
viewHelp "RandomComplexes"
///

newPackage(
        "RandomComplexes",
        Version => "0.2", 
        Date => "4 April 2018",
        Authors => {{Name => "Frank-Olaf Schreyer", 
                        Email => "schreyer@math.uni-sb.de",
		        HomePage => "http://www.math.uni-sb.de/ag/schreyer/"},
                    {Name => "Michael E. Stillman", 
		        Email => "mike@math.cornell.edu", 
		        HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
                    },
        Headline => "random complexes over fields or the integers",
	Keywords => {"Examples and Random Objects"},
        PackageExports => {"SimplicialComplexes"},
	PackageImports => {"LLLBases"}
        )

export {
    "histogram",
    "maximalEntry",
    "testTimeForLLLonSyzygies",
    "randomChainComplex",
    "randomSimplicialComplex",
    "disturb",
--    "oneMatrix",
    "normalize",
    "WithLLL",
    "ZeroMean",
    "Discrete",
    "Continuous"
}

histogram=method()
histogram(List,ZZ) := (L,n) -> (
    -- L list with entries in RR, QQ or ZZ
    ma:=max L;
    mi:=min L;
    delta:= (ma-mi)/n;
    L1:=prepend(0,append(apply(n-1,i->#select(L,l-> l<=mi+(i+1)*delta)),#L));
    apply(n,i->L1_(i+1)-L1_i)
    )    

TEST ///
  needsPackage "RandomComplexes"
  needsPackage "SVDComplexes"
  M=(randomChainComplex({50,50},{50},ZeroMean=>true)).dd_1;
  (svds,U,Vt)=SVD(M**RR_53);
  maximalEntry M
  L=svds/log
  histogram(svds/log,10)
  D=diagonalMatrix apply(100,i->2^i);
  histogram(first SVD(D*M**RR_53)/log,10)
  numericRank(M*D**RR_53)
  histogram(first SVD(M*D**RR_53)/log,10)
  D=diagonalMatrix apply(100,i->(i+1)^10);
  histogram(first SVD(M*D**RR_53)/log,10)
  numericRank(M*D**RR_53)
///    

maximalEntry=method()
maximalEntry(Matrix) := m -> (
    max(flatten entries m/abs)+0.0)

maximalEntry(ChainComplex) := C -> (
    R:= ring C; 
    if not( R === ZZ or R === QQ or R === RR_53 ) then 
    error "expect a ChainComplex over ZZ ,QQ or RR_53";
    for i from min C+1 to max C list maximalEntry C.dd_i)

disturb = method(Options => {Strategy => Discrete})
disturb(ChainComplex,RR) := opts -> (C,epsilon) -> (   
    chainComplex for i from 1 to length C list (
	c := rank C_(i-1);
	d := rank C_i;
	e := maximalEntry C.dd_i;
	entry := null;
	if opts.Strategy == symbol Discrete then
	  matrix apply(numrows C.dd_i,k->apply(numcols C.dd_i,l -> (
		    entry=C.dd_i_(k,l)*(1+epsilon*(2*random(2)-1)))))
	else if opts.Strategy == symbol Continuous then
	  matrix apply(numrows C.dd_i,k->apply(numcols C.dd_i,l -> (
		    entry=C.dd_i_(k,l)*(1+epsilon*(2*random(RR)-1)))))
--	C.dd_i +e*epsilon*(2*random(RR^c,RR^d)-oneMatrix(c,d))
    ))
disturb(ChainComplex,RR) := ChainComplex => opts -> (C,epsilon) -> (   
    if ring C =!= ZZ and ring C =!= QQ and not instance(ring C, RealField) then
      error "expected a chain complex over ZZ, QQ, or RR";
    chainComplex for i from 1 to length C list (
	c := rank C_(i-1);
	d := rank C_i;
        elems := entries C.dd_i;
	if opts.Strategy == symbol Discrete then
          matrix applyTable(elems, a -> a * (1+epsilon*(2*random(2)-1)))
	else if opts.Strategy == symbol Continuous then
          matrix applyTable(elems, a -> a * (1+epsilon*(2*random(RR)-1)))
    ))

testTimeForLLLonSyzygies=method(Options=>{Height=>11})
testTimeForLLLonSyzygies(ZZ,ZZ):= opts->(r,n)->(
    mean:=floor (opts.Height/2);
    A:=random(ZZ^r,ZZ^n,Height=>opts.Height)-mean*oneMatrix(r,n);
    t1:=timing (B:=syz A**QQ);
    B=lift(B,ZZ);
    t2:=timing(C:=LLL B);
    (append(maximalEntry chainComplex(A,B),maximalEntry C),t1#0,t2#0)
    )

TEST///
  (m,t1,t2)=testTimeForLLLonSyzygies(15,30,Height=>100)

  1/10*sum apply(10,c->(testTimeForLLLonSyzygies(10,20))_1)
  1/10*sum apply(10,c->(testTimeForLLLonSyzygies(10,20))_2)
///

randomChainComplex=method(Options=>{Height=>10,WithLLL=>true,ZeroMean=>true})

oneMatrix=method()
oneMatrix(ZZ,ZZ):= (n,m) -> matrix apply(n,i->apply(m,j-> 1))

randomChainComplex(List,List):= opts -> (h,r)-> (
    -- lists h_0,..,h_n,r_1,...,r_n
    -- of possible possible homology dimensions and ranks of maps in a chain complex
    if #h =!= #r+1 then error "expected list of non-negative integers of length n+1 and n";
    rr:=append(prepend(0,r),0);
    c:=for i from 0 to #h-1 list h_i+rr_i+rr_(i+1); 
    A:= id_(ZZ^(c_0));
    mean:=floor(opts.Height/2);
    B:= random(ZZ^(c_0),ZZ^(rr_1),Height=>opts.Height);
    if opts.ZeroMean then B=B-mean*oneMatrix(c_0,rr_1);
    C:= random(ZZ^(rr_1),ZZ^(c_1),Height=>opts.Height);
    if opts.ZeroMean then C=C-mean*oneMatrix(rr_1,c_1);
    L:={B*C};
    for i from 2 to #c-1 do (
	A=syz C;
	if opts.WithLLL then A=LLL A;
	B= random(source A, ZZ^(rr_i),Height=>opts.Height);
	if opts.ZeroMean then B=B-mean*oneMatrix(rank source A,rr_i);
	C= random( ZZ^(rr_i),ZZ^(c_i),Height=>opts.Height);
	if opts.ZeroMean then C=C-mean*oneMatrix(rr_i,c_i);
	L=append(L,A*B*C);
	);
    return chainComplex L)

TEST ///
  needsPackage("SVDComplexes")
  h={1,4,6,5,1} 
  r={1,3,3,4}
  C=randomChainComplex(h,r)
  prune HH C
  CR=C**RR_53    
  C=CR
  SVDHomology CR
  (h,U)=SVDComplex CR
  auts=apply(min CR..max CR, i-> U_i*transpose U_i)
  e=1e-10
  apply(auts,M->clean_e M)
    
  apply(auts,M->(betti M==betti id_(source M)))
  apply(auts,M->clean_e ( M-id_(source M)))


  Cplus=pseudoInverse C

  Cplus.dd^2
    betti C, betti Cplus
    maxC= max C; minC= min C;range=toList(minC+1..maxC)
    proj1=append(apply(range,i->(C.dd_i*Cplus.dd_(-i+1))),map(C_maxC,C_maxC,0))
    proj2=prepend(map(C_minC,C_minC,0),apply(range,i->Cplus.dd_(-i+1)*C.dd_(i)))

    proj3=apply(#proj1,i->proj1_i+proj2_i)
    apply(proj1,p->clean_e(p^2-p))
    apply(proj2,p->clean_e(p^2-p))
    apply(proj3,p->clean_e(p^2-p))
    apply(#proj3,i->clean_e(proj1_i*proj2_i))
    apply(#proj3,i->clean_e(proj2_i*proj1_i))
///

randomSimplicialComplex=method()
randomSimplicialComplex(ZZ,ZZ):= (k,n) -> (
   --k=6,n=15
   x:= symbol x;
   S:=QQ[x_0..x_k];
   sets:=subsets(toList(0..k));
   N:=#sets-k-2;
   I:=monomialIdeal apply(apply(n,i->sets_(random(N)+k+2)),s->product(s,i->x_i));
   c:=simplicialComplex I;
   CQ:=chainComplex c; 
   C:=(chainComplex apply(length CQ-1,i->lift(CQ.dd_(i+1),ZZ)))
   )

TEST ///
  A=randomSimplicialComplex(7,25)
  apply(length A+1,i->rank HH_i A)
  prune HH A
  Cs=apply(10,i->(
	while(
	while( A=randomSimplicialComplex(8,40);length A <1) do(); 
	max select(length A+1,i->rank HH_i A !=0)< length A) do();
    	A))
  netList apply(Cs,A->(A, apply(length A+1,i-> rank HH_i A)) )
///
    
normalize = method()
normalize ChainComplex := C-> (
    minC := min C;
    maxC := max C;
    D := if ring C === ZZ then C**QQ else C;
    C' := for i from minC+1 to maxC list (
        -- for some reason, if D.dd_i is 0, then this returns the 0 matrix:
	    m := max(flatten entries D.dd_i/abs);
   	    (1/m) * D.dd_i
        );
    -- this next line is not correct: it might negate some differentials.
    -- if the complex has minC odd...
    chainComplex C'[-minC]
    )
 
beginDocumentation()

doc ///
   Key
     RandomComplexes
   Headline
     support for creating random complexes over the integers 
   Description
    Text
      We implement two methods to create a random @TO "ChainComplex"@ over the integers.
      The first method (@TO randomChainComplex@) builds the complex from products of randomly chosen matrices of desired rank.
      The limitation of this method to produce large complexes over the integers with
      moderate Height is the use of the LLL algorithm to improve the presentation of
      syzygy matrices.

      The second method (@TO "randomSimplicialComplex"@) uses Stanley-Reisner rings from randomly chosen monomial ideals.
   Caveat
      Some functionality here should be moved elsewhere, e.g. 
        @TO "disturb"@, @TO "histogram"@, @TO "maximalEntry"@, and @TO "normalize"@.
///
 

doc ///
   Key
     randomChainComplex
     (randomChainComplex,List,List)
     [randomChainComplex, Height]
     [randomChainComplex, WithLLL]
     [randomChainComplex, ZeroMean]
   Headline
     random chain complex over the integers with prescribed ranks of the homology group and ranks of the matrices
   Usage
     C = randomChainComplex(h,r)
   Inputs
     h:List
       of desired ranks of the homology groups, of some length $n$
     r:List
       of desired ranks of the matrices in the complex, of length $n-1$
     Height => ZZ
       the sizes of the random integers used
     WithLLL => Boolean
       use the LLL algorithm to keep the sizes of the integers small
     ZeroMean => Boolean
       whether to balance the random numbers around zero
   Outputs
     C:ChainComplex
       a random chain complex over the integers whose homology ranks match $h$, and 
       whose matrices have ranks given by $r$
   Description
    Text
    
    Example
      h={1,4,6,5,1} 
      r={1,3,3,4}
      C=randomChainComplex(h,r)
      prune HH C
      for i from 0 to 4 list rank HH_i C
      for i from 1 to 4 list rank(C.dd_i)
    Text
      The optional argument {\tt Height} chooses the maximum sizes of the random numbers used.
      The actual numbers are somewhat larger (twice as many bits), as matrices are multiplied together.
    Example
      h={1,4,0,5,1} 
      r={2,3,3,4}
      C=randomChainComplex(h,r, Height=>1000)
      C.dd
      C.dd^2 == 0
      prune HH C
      for i from 0 to 4 list rank HH_i C
      for i from 1 to 4 list rank(C.dd_i)
   Caveat
     This returns a chain complex over the integers.  Notice that if one gives h to be a list of zeros, then
     that doesn't mean that the complex is exact, just that the ranks are as expected.
   SeeAlso
     "SVDComplexes::SVDComplexes"
///

doc ///
   Key
     randomSimplicialComplex
     (randomSimplicialComplex,ZZ,ZZ)
   Headline
     the chainComplex over ZZ of a random Stanley-Reisner simplicial complex 
   Usage
     C = randomSimplicialComplex(k,n)
   Inputs
     k:ZZ
     n:ZZ
   Outputs
     C:ChainComplex
       the chainComplex of the Stanley-Reisner simplicial complex of a random 
       square free monomial ideal in k+1 variables and n generators
   Description
    Text
       We compute the simplicial complex associated to a square free monomial ideal in k+1 variables
       whose n generators we choose randomly among the square free monomials.
     
    Example
      setRandomSeed "nice example 2";
      C = randomSimplicialComplex(7,20)
      prune HH C
   SeeAlso
     "SVDComplexes::SVDComplexes"
///

doc ///
   Key
     normalize
     (normalize,ChainComplex)
   Headline
     normalize a ChainComplex over QQ or RR
   Usage
     B = normalize C
   Inputs
     C:ChainComplex
       over RR or QQ
   Outputs
     B:ChainComplex
       an isomorphic ChainComplex over QQ or RR
   Description
    Text
       We divide each matrix by its entry of maximal absolute value, to obtain a complex with entries of absolute size $\le 1$.
    Example
      setRandomSeed "nice example 2";
      C=randomChainComplex({1,1,1},{2,2})
      C.dd
      B=normalize C
      B.dd
///

doc ///
   Key
     maximalEntry
     (maximalEntry,ChainComplex)
     (maximalEntry,Matrix)
   Headline
     maximal absolute value of the entries of the matrix or matrices 
   Usage
     m = maximalEntries C
   Inputs
     C:ChainComplex
       or a @TO "Matrix"@, over ZZ, QQ, or RR
   Outputs
     m:List
       of the maximal absolute values of the entries in matrices defining the differential
   Description
    Text
       For each matrix we compute the of maximal absolute value of the entries
    Example
      setRandomSeed "nice example 2";
      C=randomChainComplex({1,1,1},{2,2},Height=>10)
      C.dd
      maximalEntry C
      B=randomChainComplex({2,2,4,2,5,2,2},{2,3,3,2,3,3},Height=>5)
      maximalEntry B
      apply(min B..max B,i->rank HH_i(B**QQ))
/// 

doc ///
   Key
     disturb
     (disturb,ChainComplex,RR)
     [disturb, Strategy]
   Headline
     disturb the matrices of a chain complex over RR
   Usage
     B = disturb(C,epsilon)
   Inputs
     C:ChainComplex
       over RR or QQ
     epsilon:RR
     Strategy => Symbol
       either Discrete or Continuous, whether the disturbed values should be drawn from a
       discrete distribution or a continuous distribution
   Outputs
     B:ChainComplex
       a sequence of matrices over RR
   Description
    Text
       We disturb the entries of the matrices by a relative error of size epsilon depending on either a discrete with values in \{-1,1\}\ or a continuous random variable
       with values in [-1..1].
    Example
      needsPackage "RandomComplexes"
      setRandomSeed "nice example 2";
      C=randomChainComplex({1,1,1},{2,2})
      C.dd
      B=disturb(C,1e-4)
      B.dd
      B.dd^2
      B1=disturb(C,1e-4,Strategy => Continuous)
      B1.dd^2
   Caveat
      The result is only approximately a complex
   SeeAlso
      Continuous
      Discrete
///

doc ///
   Key
     histogram 
     (histogram,List,ZZ)
   Headline
     histogram of a list of real numbers 
   Usage
     h = histogram(L,n)
   Inputs
     L:List
       of numbers in RR or QQ or ZZ
     n:ZZ
       the number of subintervals to be considered.  
   Outputs
     h:List
       of n integers, the number of entries in the L in i-th equidistant
       subdivision of the interval from min L to max L
   Description
    Text
       We combute h_i th number to elements in the i-th equidistant subdivision
       of the interval [min L, max L] into n parts 
    Example
       M=(randomChainComplex({20,20},{20},ZeroMean=>true)).dd_1;
       (svds,U,Vt)=SVD(M**RR_53);
       (entries matrix {svds})_0/log
       maximalEntry M
       histogram(svds/log,10)
       histogram(svds_{0..19}/log,10)
       histogram(svds_{20..39}/log,10)
/// 


doc ///
   Key
     testTimeForLLLonSyzygies
     (testTimeForLLLonSyzygies,ZZ,ZZ)
     [testTimeForLLLonSyzygies, Height]
   Headline
     test timing for LLL on syzygies 
   Usage
     (m,t1,t2)=testTimeForLLLonSyzygies(r,n,Height=>100)
   Inputs
     r:ZZ
     n:ZZ
     Height => ZZ
       the sizes of the random integers used
   Outputs
     m:List
       of maximal absolute values of the entries of A, B and the LLL basis of B
     t1:RR
       the time in seconds to compute B = ker A
     t2:RR
       the time to compute the LLL basis of B
   Description
    Text
       We randomly choose an $r \times\ n$ matrix A over ZZ with entries up to the given Height,
       and take the time to compute B=ker A and an LLL basis of B.
    Example
      setRandomSeed "nice example 2";
      r=10,n=20
      (m,t1,t2)=testTimeForLLLonSyzygies(r,n,Height=>11)
      (m,t1,t2)=testTimeForLLLonSyzygies(15,30,Height=>100)
      L=apply(10,c->(testTimeForLLLonSyzygies(15,30))_{1,2})
      1/10*sum(L,t->t_0)
      1/10*sum(L,t->t_1)
///   	    

doc ///
   Key
    ZeroMean
   Headline
    Option for randomComplex
   Description
    Text
     If ZeroMean=>true then the integer of given Height values are randomly chosen with a zero mean
///

doc ///
   Key
    WithLLL
   Headline
    Option for randomComplex
   Description
    Text
     If WithLLL=>true then syzygy matrices of the randomly chosen matrices
     are improved for their Height by applying the LLL algorithm.
///

doc ///
   Key
    Discrete
    Continuous
   Headline
    Value for the Strategy in disturb
   Description
    Text
     If Strategy=>Continuous then we disturb the complex by floating point numbers
     otherwise by discrete values.
   SeeAlso
     disturb
///

TEST ///
-* 
  restart
  needsPackage "RandomComplexes"
*-
  rks = {4,5,6}
  C = randomChainComplex({2,2,2,2},rks)
  assert(C.dd^2 == 0)
  for i from 0 to 3 do assert(rank prune HH_i C == 2)
  assert(rks == for i from 1 to 3 list rank C.dd_i)
///

TEST ///
-* 
  restart
  needsPackage "RandomComplexes"
*-
  rks = {2,2,2}
  C = randomChainComplex({1,1,1,1},rks)
  assert(C.dd^2 == 0)
  for i from 0 to 3 do assert(rank prune HH_i C == 1)
  assert(rks == for i from 1 to 3 list rank C.dd_i)
///

TEST ///
-* 
  restart
  needsPackage "RandomComplexes"
*-
  hr = {5,5,5,5}
  rks = {2,2,2}
  C = randomChainComplex(hr,rks)
  assert(C.dd^2 == 0)
  hr == for i from 0 to #hr-1 list rank prune HH_i C
  assert(rks == for i from 1 to #rks list rank C.dd_i)
///

TEST ///
-* 
  restart
  needsPackage "RandomComplexes"
*-
  hr = {5,5,5,5}
  rks = {20,20,20}

  C = randomChainComplex(hr,rks)
  assert(C.dd^2 == 0)
  -- hr == for i from 0 to #hr-1 list rank prune HH_i C -- ouch, this needs improvement!!
  assert(rks == for i from 1 to #rks list rank C.dd_i)
///

TEST ///
  -- XXX
-* 
  restart
  needsPackage "RandomComplexes"
*-
  setRandomSeed "nice example 2";
  C = randomChainComplex({1,1,1},{2,2})
  C.dd
  B=normalize C
  B.dd

  CR = C ** RR
  BR = normalize CR
  BR.dd
  
  D = chainComplex{map(RR^1, RR^3, 0), map(RR^3, RR^1, {{1.0},{3.0},{5.0}})}
  D.dd^2
  normalize D
  D.dd
///

TEST ///
  -- test of disturb
-* 
  restart
  needsPackage "RandomComplexes"
*-
  setRandomSeed "nice example";
  C = randomChainComplex({1,1,1},{2,2})
  C.dd
  disturb(C,.000001)
///
	    
end--

restart
uninstallPackage "RandomComplexes"
restart
installPackage "RandomComplexes"
check("RandomComplexes", UserMode=>true)

viewHelp "RandomComplexes"









TEST ///
restart
needsPackage "RandomComplexes"
needsPackage "SVDComplexes"
h={1,3,5,2,1} 
r={5,11,3,2}
setRandomSeed "alpha"
elapsedTime C=randomChainComplex(h,r,Height=>5,WithLLL=> true)
C.dd_4

C.dd^2

p=nextPrime 1000
CF=C**ZZ/p
length C
elapsedTime apply(length C +1,i-> rank HH_i CF)
--prune HH C

CR=normalize(C**RR_53)
--CR=C**RR_53
elapsedTime SVDHomology CR
elapsedTime SVDHomology(CR,Strategy=>Laplacian)
elapsedTime (h,U)=SVDComplex CR;
elapsedTime (hL,V)=SVDComplex(CR,Strategy=>Laplacian);
e=1e-9
clean_e (transpose U#0*C.dd_1* U#1)
clean_e (transpose V#0*C.dd_1*V#1)
///

TEST ///

setRandomSeed 7

C= randomSimplicialComplex(6,20)
prune HH C

F=ZZ/nextPrime 1000
CF=C**F
elapsedTime apply(length C+1,i-> rank HH_i CF)
rF=apply(length C,i-> rank CF.dd_(i+1))

CR=C**RR_53
elapsedTime SVDHomology CR
elapsedTime (h,U)=SVDComplex CR;
Sigma = source U
betti Sigma === betti C
rR=elapsedTime apply(length Sigma ,i-> rank  Sigma.dd_(i+1))
assert(rR==rF)
Sigma.dd_1

e=1e-10
apply(length C, i->clean_e (U_i*Sigma.dd_(i+1)*transpose U_(i+1)-C.dd_(i+1)))
e=1e-20
apply(length C, i->clean_e (U_i*Sigma.dd_(i+1)*transpose U_(i+1)-C.dd_(i+1)))


setRandomSeed 3

elapsedTime Cs= apply(10,i->(
	while (
	    while ( C= randomSimplicialComplex(6,15); length C <1) do ();
	    max select(length C+1, i-> rank HH_i C != 0) < length C) do ();
	C)
    ) 
netList apply(Cs, C-> (C, apply(length C+1,i->rank HH_i C)))
    
Cs=reverse Cs_(sort apply (#Cs,j->(Z=Cs_j;{sum(length Z+1,i->rank Z_i),j}))/last)
tally apply(Cs,C-> apply(length C+1,i->rank HH_i C))

netList apply(Cs, C-> (C, apply(length C+1,i->rank HH_i C)))

C=Cs_1**Cs_3
F=ZZ/nextPrime 1000
CF=C**F
hF=elapsedTime apply(length C+1,i-> rank HH_i CF)

CR=C**RR_53
A=elapsedTime SVDHomology CR
hR=apply(keys A_0,k->A_0#k)
 assert(hR == hF)
elapsedTime (h,U)=SVDComplex CR;
Sigma = source U

e=1e-10
nearlyZero=elapsedTime chainComplex apply(length C,i->(U_i*Sigma.dd_(i+1)*transpose U_(i+1)-C.dd_(i+1)))
-- needs speed up for multiplication
maximalEntry nearlyZero
///
TEST ///
restart
needsPackage("RandomComplexes")
needsPackage("SVDComplexes")

setRandomSeed 7

elapsedTime Cs= apply(10,i->(
	while (
	    while ( C= randomSimplicialComplex(5,10); length C <1) do ();
	    max select(length C+1, i-> rank HH_i C != 0) < length C) do ();
	C)
    ) 
netList apply(Cs, C-> (C, apply(length C+1,i->rank HH_i C)))
    
Cs=Cs_(sort apply (#Cs,j->(Z=Cs_j;{sum(length Z+1,i->rank Z_i),j}))/last)
tally apply(Cs,C-> apply(length C+1,i->rank HH_i C))

netList apply(Cs, C-> (C, apply(length C+1,i->rank HH_i C)))

C=Cs_0**Cs_0**Cs_1
CR=C**RR_53
A=elapsedTime SVDHomology CR  -- 1.75474 seconds elapsed
A_0
hF = new MutableHashTable
F=ZZ/nextPrime 1000
CF=C**F
time for i from 0 to length C do hF#i = rank HH_i CF
B=new HashTable from hF    
assert(A_0 === B)
///



TEST ///
restart
needsPackage "RandomComplexes"
needsPackage "SVDComplexes"
setRandomSeed"test SVD"
h={1,4,10,4,1} 
r={10,20,20,10}
elapsedTime C=randomChainComplex(h,r,Height=>3,WithLLL=>true,ZeroMean=>true)
maximalEntry C
CR=C**RR_53
elapsedTime SVDHomology CR
elapsedTime SVDHomology(CR,Strategy=>Laplacian)
C1=randomChainComplex({1,1},{5})**RR_53
CR1=CR**C1
A=elapsedTime SVDHomology CR1
h={1,5,14,14,5,1}
r={10,10,10,10,10}
C=randomChainComplex(h,r,Height=>1000,WithLLL=>true,ZeroMean=>true)
CR=C**RR_53
maximalEntry CR
B=elapsedTime SVDHomology CR
elapsedTime SVDHomology(CR,Strategy=>Laplacian)
assert(A_0===B_0)
///

TEST ///
restart
needsPackage "RandomComplexes"
needsPackage "SVDComplexes"
setRandomSeed"test SVD"
h={1,5,20,5,1} 
r={10,20,20,10}
elapsedTime C=randomChainComplex(h,r,Height=>3,WithLLL=>true,ZeroMean=>true)
maximalEntry C
CR=C**RR_53
elapsedTime SVDHomology(CR,Strategy=>Laplacian)
D=disturb(CR,1e-5)
elapsedTime SVDHomology D
elapsedTime SVDHomology CR
elapsedTime SVDHomology(D,CR)
elapsedTime SVDHomology(D,Strategy=>Laplacian)
///


TEST ///
restart
needsPackage "RandomComplexes"
needsPackage "SVDComplexes"
hts=new MutableHashTable
for r from 4 to 18 do (
    n=r+1; a=testTimeForLLLonSyzygies(r,n);
    while (n=n+1,b=testTimeForLLLonSyzygies(r,n); b_1+b_2 <0.1) do (a=b); 
    hts#(r,n-1)=a;
    hts#(r,n)=b;
    )
Hts=new HashTable from hts

t=apply(100,c->(b=testTimeForLLLonSyzygies(10,43);b_1+b_2)) 
e=1e3
tally apply(t,c->floor(c*e)*1/e)
min t, max t

///



TEST ///
restart
needsPackage "RandomComplexes"
M=(randomChainComplex({50,50},{50},ZeroMean=>true)).dd_1;
(svds,U,Vt)=SVD(M**RR_53);
maximalEntry M
histogram(svds/log,10)
D=diagonalMatrix apply(100,i->2^i);
histogram(first SVD(D*M**RR_53)/log,10)
numericRank(M*D**RR_53)
histogram(first SVD(M*D**RR_53)/log,10)
D=diagonalMatrix apply(100,i->(i+1)^10);
histogram(first SVD(M*D**RR_53)/log,10)
numericRank(M*D**RR_53)
///


TEST ///
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%
-- for paper??
-- %%%%%%%%%%%%%%%%%%%%%%%
restart
needsPackage "RandomComplexes"
needsPackage "SVDComplexes"
h={1,1,1,1}
r={2,2,2}
setRandomSeed 2
C=randomChainComplex(h,r,Height=>9,WithLLL=>true,ZeroMean=>true)
prune HH C
C.dd_1,C.dd_2,C.dd_3
ker transpose C.dd_1,LLL syz C.dd_2,LLL syz transpose C.dd_2,ker C.dd_3 
--C1=randomChainComplex({1,1},{5},Height=>5,WithLLL=>true,ZeroMean=>true)
CR=C**RR_53
tally sort apply(10,c->random(RR_53)) --CR=C**C1**RR_53
elapsedTime SVDHomology(CR,Threshold=>1e-15)
elapsedTime SVDHomology(CR,Strategy=>Laplacian,Threshold=>1e-13)
28.7143^2,47.1932^2,35.208^2
U=last SVDComplex CR
V=last SVDComplex (CR,Strategy=>Laplacian)
(source U).dd_1, (source V).dd_1
(source U).dd_2, (source V).dd_2
(source U).dd_3, (source V).dd_3
CRd=dual CR[-3]
Ud=last SVDComplex CRd;


(source U).dd_1, (source Ud).dd_3
(source U).dd_2, (source Ud).dd_2
(source U).dd_3, (source Ud).dd_1
U#0-V#0
U#1-V#1,U#1_4
U#2-V#2,U#2_0
U#3-V#3
setRandomSeed 1
D=disturb(CR,1e-3)
D.dd_1,D.dd_2,D.dd_3
D.dd^2
D'=disturb(CR,1e-3)
CR.dd_1
(h,Ud) = SVDComplex(D',D,Threshold=>1e-2);
h 
elapsedTime SVDHomology(D,Threshold=>1e-2)
elapsedTime SVDHomology(D,Strategy=>Laplacian,Threshold=>1e-2)
elapsedTime SVDHomology(D,CR,Threshold=>1e-2)
elapsedTime SVDHomology CR
Vd=last SVDComplex(D,CR,Threshold=>1e-2);
Ud=last SVDComplex (D,Strategy=>Laplacian,Threshold=>1e-2);

(source Ud).dd_1, (source Vd).dd_1, (source V).dd_1
(source Ud).dd_2, (source Vd).dd_2, (source V).dd_2
(source Ud).dd_3,(source Vd).dd_3, (source V).dd_3

2.0^(-53)
 apply(3,i->maximalEntry(U#i*transpose U#i-id_(source U#i)))
U#0 *(source U).dd_1 *transpose U#1 - CR.dd_1
D.dd_1-CR.dd_1
Ud#0 *(source Ud).dd_1 *transpose Ud#1 - CR.dd_1  
Ud#0 *(source Ud).dd_1 *transpose Ud#1 - D.dd_1 
Ud#1 *(source Ud).dd_2 *transpose Ud#2 - D.dd_2 
Ud#2 *(source Ud).dd_3 *transpose Ud#3 - D.dd_3 

F=chainComplex apply(3,i->U#i *(source U).dd_(i+1) *transpose U#(i+1))
E=chainComplex apply(3,i->Ud#i *(source Ud).dd_(i+1) *transpose Ud#(i+1))
D.dd^2
E.dd^2
(h,Ue)=SVDComplex(E,Strategy=>Laplacian,Threshold=>1e-8);
h
SVDHomology(E,Strategy=>Laplacian)
euclideanDistance(E,D)
euclideanDistance(CR,D)
euclideanDistance(CR,F)
Ue#0 *(source Ue).dd_1 *transpose Ue#1 - E.dd_1 
Ue#1 *(source Ue).dd_2 *transpose Ue#2 - E.dd_2 
Ue#2 *(source Ue).dd_3 *transpose Ue#3 - E.dd_3 
sum(flatten entries (E.dd_1-D.dd_1)/abs)
sum(flatten entries (CR.dd_1-D.dd_1)/abs)
,D.dd_1-E.dd_1
,E.dd_2,E.dd_3

first SVD CR.dd_1, first SVD D.dd_1
first SVD CR.dd_2, first SVD D.dd_2
first SVD CR.dd_3, first SVD D.dd_3

restart
needsPackage "RandomComplexes"
needsPackage "SVDComplexes"
h={1,1,1,1}
r={2,2,2}
setRandomSeed 2
C=randomChainComplex(h,r,Height=>9,WithLLL=>true,ZeroMean=>true)
prune HH C
C.dd_1,C.dd_2,C.dd_3
CR=C**RR
Cplus=(pseudoInverse CR)
Cplus.dd^2
CplusL=pseudoInverse(CR,Strategy=>Laplacian)
CplusQ=pseudoInverse(C**QQ)
CplusQ.dd_-2**RR_53-Cplus.dd_(-2)
CplusQ.dd_-2*CplusQ.dd_-1
CplusQ.dd_-0
Cplus.dd_-0

printingPrecision =6
Cplus.dd_0

tex Cplus.dd_0
tex CplusQ.dd_0


B=chainComplex CplusQ
B.dd^2
C.dd^2
B[3]
apply(3,i->Cplus.dd_(i+1)-CplusL.dd_(i+1))
CplusL.dd^2
Uplus=last SVDComplex Cplus
apply(3,i->(source Uplus).dd_(i+1))
Cplusplus =pseudoInverse Cplus
Cplusplus.dd_1,Cplusplus.dd_2,Cplusplus.dd_3
Cplus.dd_1,Cplus.dd_2,Cplus.dd_3
p1=C.dd_1* Cplus.dd_3
p1^2-p1
p2=C.dd_2*Cplus.dd_2
p3=C.dd_3*Cplus.dd_1

p2^2-p2,
p3^2-p3
q1=Cplus.dd_3*C.dd_1
q1^2-q1
q2=Cplus.dd_2*C.dd_2
q2^2-q2
q3=Cplus.dd_1*C.dd_3
q3^2-q3
betti p1, betti p2, betti p3
betti q1, betti q2, betti q3
q1*p2-p2*q1
q2*p3-p3*q2
h1=id_(RR^5)-p2-q1
h1^2-h1
first SVD h1
C.dd_3
Cplus.dd^2
apply(3,i->Cplus.dd_(i+1))
(source Ue).dd^2
E.dd^2
F.dd^2
D.dd^2
CR.dd^2
maximalEntry CR, maximalEntry D
U#0 *(source U).dd_1 *transpose U#1 - CR.dd_1 
U#1 *(source U).dd_2 *transpose U#2 - CR.dd_2 
U#2 *(source U).dd_3 *transpose U#3 - CR.dd_3


--viewHelp 
tex C.dd_1
tex C.dd_2
tex C.dd_3

printingPrecision = 5
tex (source U).dd_1
tex  (source U).dd_2
tex  (source U).dd_3

printingAccuracy = 4
tex U#0
tex U#1
tex U#2
tex U#3
///

/// -- for David
restart
needsPackage "AGRExamples"
needsPackage "InverseSystems"
--viewHelp InverseSystems
R=QQ[a..h]
Rp=(ZZ/32003)(monoid R)
deg=4
nextra=10
setRandomSeed "1"
F=sum(gens R,x->x^deg)+sum(nextra,i->(random(1,R))^deg);

I = ideal fromDual2 matrix {{F}} ;
Ip=sub(I,Rp);
Cp=res(Ip, FastNonminimal=>true)
betti Cp
betti(Cp,Minimize=>true)
J=ideal fromDual matrix{{F}};
betti J
Jp=sub(J,Rp);
CJp=res(Jp, FastNonminimal=>true)
betti CJp
betti(CJp,Minimize=>true)
pts=ideal (gens Ip)_{0..17};
Cpts=res(pts,FastNonminimal=>true)
betti(Cpts,Minimize=>true)
hilbertPolynomial pts
apply(5,i->hilbertFunction(i,Rp/Ip))
A=Rp/(Ip+ideal random(1,Rp))
apply(5,i->hilbertFunction(i,A))
/// 
restart
needsPackage "SVDComplexes"
needsPackage "AGRExamples"
needsPackage "RandomComplexes"
needsPackage "InverseSystems"

R=QQ[a..h]
Rp=(ZZ/32003)(monoid R)
R0=(RR_53)(monoid R)
deg=4
nextra=10
setRandomSeed "1"
F=sum(gens R,x->x^deg)+sum(nextra,i->(random(1,R))^deg);
I = ideal fromDual2 matrix {{F}} ;
betti I
elapsedTime C=res(I,Strategy=>4.1);
betti C
elapsedTime minimalBetti sub(I,Rp)
elapsedTime SVDBetti C
Ls=constantStrands(C,RR_53)
D=Ls#9
SVDHomology(D)
SVDHomology(D,Strategy=>Laplacian)
SVDHomology(D,Strategy=>Laplacian,Threshold=>1e-2)
Ls1=constantStrands(C,RR_1000)
D1=Ls1#9
D1=D1**RR_53
SVDHomology(D1,Strategy=>Laplacian)
SVDHomology(D1,D)
SVDHomology D1
(h,U)=SVDComplex(D,D1);
(h,U)=SVDComplex(D1,D);
reverse sort unique flatten entries (source U).dd_6
reverse sort unique flatten entries (source U).dd_7


maximalEntry D1
///
TEST///
restart
needsPackage "RandomComplexes"
needsPackage "SVDComplexes"
setRandomSeed "alpha"
time Cs=apply(toList(1..6),i->(
   h={1+i,3*i,2*i,i}; 
   r={5,10+3*i,11+2*i};
elapsedTime C=randomChainComplex(h,r,Height=>19,WithLLL=>true);
CR=normalize(C**RR_53)));
netList Cs
printingPrecision=3
data=apply(Cs,CR->(
	t1=(elapsedTiming SVDHomology CR)#0;
        t2=(elapsedTiming SVDComplex CR)#0;
        t3=(elapsedTiming SVDComplex(CR,Strategy=>Laplacian))#0;
	h=(SVDHomology CR)#0;
	h1=(SVDComplex(CR,Strategy=>Laplacian))#0;
	assert(h===h1);
	c=apply(4,i->rank CR_i);
    {c,	apply(4,i->h#i),t2,t3}
    ));
netList data    
-*
${{{7, 21, 28, 14}, {2, 3, 2, 1}, .00211, .011}, {{8, 27, 35, 17}, {3, 6,
     4, 2}, .00225, .0182}, {{9, 33, 42, 20}, {4, 9, 6, 3}, .00254, .0294},
     {{10, 39, 49, 23}, {5, 12, 8, 4}, .00291, .0647}, {{11, 45, 56, 26}, {6,
     15, 10, 5}, .00355, .109}, {{12, 51, 63, 29}, {7, 18, 12, 6}, .00442,
     .115}}$
*-
///
TEST///
restart
needsPackage "RandomComplexes"
needsPackage "SVDComplexes"
setRandomSeed "alpha"
time Cs=apply(toList(1..6),i->(
	k=10+random(4);N=20+random 20;
elapsedTime C=randomSimplicialComplex(k,N);
CR=C**RR_53;
{k,N,CR}));
netList Cs
printingPrecision=3
data=apply(Cs,kNCR->(k=kNCR_0;
	N=kNCR_1;
	CR=kNCR_2;
	t1=(elapsedTiming SVDHomology CR);
	h=t1#1#0;
	c=apply(length CR,i->rank CR_i);
    {k+1,N,c,	apply(length CR,i->h#i),t1#0}
    ));
netList data 

 
tex data 
-*
o9 = ${{13, 25, {13, 78, 284, 695, 1193, 1443, 1179, 570, 117}, {1, 0, 0, 0, 0,
     0, 0, 6, 0}, 31.5}, {11, 29, {11, 53, 147, 255, 276, 169, 47}, {1, 0, 0,
     0, 0, 2, 0}, .0647}, {12, 24, {12, 65, 210, 450, 668, 685, 453, 156}, {1,
     0, 0, 0, 0, 0, 2, 0}, 1.37}, {11, 25, {11, 54, 154, 276, 312, 210, 73},
     {1, 0, 0, 0, 0, 0, 0}, .0869}, {12, 32, {12, 66, 218, 475, 701, 680, 382,
     89}, {1, 0, 0, 0, 0, 0, 6, 0}, 1.46}, {14, 26, {14, 91, 364, 999, 1982,
     2909, 3156, 2447, 1234, 327}, {1, 0, 0, 0, 0, 0, 0, 0, 3, 0}, 504}}$
*-
///
viewHelp "RandomComplexes"
