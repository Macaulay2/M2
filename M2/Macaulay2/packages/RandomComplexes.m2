newPackage(
        "RandomComplexes",
        Version => "0.1", 
        Date => "",
        Authors => {{Name => "Frank Schreyer", 
                  Email => "", 
                  HomePage => ""},
              {Name => "Mike Stillman", 
                  Email => "", 
                  HomePage => ""}},
        Headline => "SVD of a complex, includes nonminimal resolutions over the reals",
        DebuggingMode => true
        )

export {
    "maximalEntry",
    "testTimeForLLLonSyzygies",
    "randomChainComplex",
    "oneMatrix",
    "normalize",
    "WithLLL",
    "zeroMean"
}








maximalEntry=method()
maximalEntry(Matrix) := m -> (
    max(flatten entries m/abs)+0.0)

maximalEntry(ChainComplex) := C -> (
    R:= ring C; 
    if not( R === ZZ or R === QQ or R === RR_53 ) then 
    error "expect a ChainComplex over ZZ ,QQ or RR_53";
    for i from min C+1 to max C list maximalEntry C.dd_i)

testTimeForLLLonSyzygies=method(Options=>{Height=>3})
testTimeForLLLonSyzygies(ZZ,ZZ):= opts->(r,n)->(
    mean:=floor (opts.Height/2);
    A:=random(ZZ^r,ZZ^n,Height=>opts.Height)-mean*oneMatrix(r,n);
    T:=timing (B:=kernelLLL A; LLL B);
    (maximalEntry chainComplex(A,B),T#0,maximalEntry T#1))


randomChainComplex=method(Options=>{Height=>10,WithLLL=>true,zeroMean=>false})

needsPackage("SimplicialComplexes")

randomChainComplex(ZZ,ZZ):= opts-> (k,n) -> (
   --k=6,n=15
   x:= symbol x;
   S:=QQ[x_0..x_k];
   sets:=subsets(toList(0..k));
   N:=#sets-1;
   I:=monomialIdeal apply(apply(n,i->sets_(random(N)+1)),s->product(s,i->x_i));
   c:=simplicialComplex I;
   CQ:=chainComplex c; 
   C:=(chainComplex apply(length CQ-1,i->lift(CQ.dd_(i+1),ZZ)))
   )

TEST ///
A=randomChainComplex(7,25)
apply(length A+1,i->rank HH_i A)
prune HH A
Cs=apply(10,i->(
	while(
	while( A=randomChainComplex(8,40);length A <1) do(); 
	max select(length A+1,i->rank HH_i A !=0)< length A) do();
    	A))
netList apply(Cs,A->(A, apply(length A+1,i-> rank HH_i A)) )
///
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
    if opts.zeroMean then B=B-mean*oneMatrix(c_0,rr_1);
    C:= random(ZZ^(rr_1),ZZ^(c_1),Height=>opts.Height);
    if opts.zeroMean then C=C-mean*oneMatrix(rr_1,c_1);
    L:={B*C};
    for i from 2 to #c-1 do (
	A=syz C;
	if opts.WithLLL then A=LLL A;
	B= random(source A, ZZ^(rr_i),Height=>opts.Height);
	if opts.zeroMean then B=B-mean*oneMatrix(rank source A,rr_i);
	C= random( ZZ^(rr_i),ZZ^(c_i),Height=>opts.Height);
	if opts.zeroMean then C=C-mean*oneMatrix(rr_i,c_i);
	L=append(L,A*B*C);
	);
    return chainComplex L)
TEST ///
h={1,4,6,5,1} 
r={1,3,3,4}
C=randomChainComplex(h,r)
prune HH C
CR=C**RR_53    
C=CR
SVDHomology CR
U=SVDComplex CR
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

    
normalize=method()
normalize ChainComplex := C-> (
    minC:= min C;
    maxC:= max C;
    D := if ring C === ZZ then C**QQ else C;  
    C':=for i from minC+1 to maxC list (
	m:=max(flatten entries D.dd_i/abs);
   	1/m*D.dd_i);
    return chainComplex C'[-minC]) 
 

needsPackage "SVDComplexes"

beginDocumentation()

doc ///
   Key
     RandomComplexes
   Headline
     support for creating randomly complexes over the integers 
   Description
    Text
      Some functionality here should be moved elsewhere.
      
      Here is an example of the usage.
   Caveat
     Currently, this package requires that the Macaulay2 being run is from the res-2107 ?? git branch
///
 

doc ///
   Key
     randomChainComplex
     (randomChainComplex,List,List)
     (randomChainComplex,ZZ,ZZ)
     [randomChainComplex, Height]
   Headline
     random chain complex over the integers with prescribed homology group and matrix ranks
   Usage
     C = randomChainComplex(h,r) or randomComplex(k,n)
   Inputs
     h:List
       of desired ranks of the homology groups, of some length $n$
     r:List
       of desired ranks of the matrices in the complex, of length $n-1$
     k:ZZ
     n:ZZ
     Height => ZZ
       the sizes of the random integers used
   Outputs
     C:ChainComplex
       a random chain complex over the integers whose homology ranks match $h$, and 
       whose matrices have ranks given by $r$
       or 
       the chainComplex of the Stanley-Reisner simplicial complex of a random 
       square free monomial ideal in k+1 variables and n generators
   Description
    Text
      Here is an example of the first kind
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
      r={1,3,3,4}
      C=randomChainComplex(h,r, Height=>1000)
      C.dd
      C.dd^2 == 0
      prune HH C
      for i from 0 to 4 list rank HH_i C
      for i from 1 to 4 list rank(C.dd_i)
    Text
      Here is an example of the second kind
    Example
      setRandomSeed "nice example 2";
      C=randomChainComplex(7,20)
      prune HH C
   Caveat
     This returns a chain complex over the integers.  Notice that if one gives h to be a list of zeros, then
     that doesn't mean that the complex is exact, just that the ranks are as expected.
   SeeAlso
     SVDComplexes
///
   	    
end

restart
uninstallPackage "RandomComplexes"
restart
installPackage "RandomComplexes"
viewHelp "RandomComplexes"

TEST ///
restart
needsPackage "RandomComplexes"

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
elapsedTime U=SVDComplex CR;
elapsedTime (h,V)=SVDComplex(CR,Strategy=>Laplacian);
e=1e-9
clean_e (U_0*C.dd_1*transpose U_1)
clean_e (transpose V#0*C.dd_1*V#1)
///

TEST ///
restart
load "randomComplexes.m2"

setRandomSeed 7

C= randomChainComplex(6,20)
prune HH C

F=ZZ/nextPrime 1000
CF=C**F
elapsedTime apply(length C+1,i-> rank HH_i CF)
rF=apply(length C,i-> rank CF.dd_(i+1))

CR=C**RR_53
elapsedTime SVDHomology CR
elapsedTime U=SVDComplex CR;
Sigma = target U
betti Sigma === betti C
rR=elapsedTime apply(length Sigma ,i-> rank  Sigma.dd_(i+1))
assert(rR==rF)
Sigma.dd_1

Sigma = target U
Sigma.dd_1
e=1e-10
apply(length C, i->clean_e (U_i*C.dd_(i+1)*transpose U_(i+1)-Sigma.dd_(i+1)))


setRandomSeed 3

elapsedTime Cs= apply(10,i->(
	while (
	    while ( C= randomChainComplex(6,15); length C <1) do ();
	    max select(length C+1, i-> rank HH_i C != 0) < length C) do ();
	C)
    ) 
netList apply(Cs, C-> (C, apply(length C+1,i->rank HH_i C)))
    
Cs=Cs_(sort apply (#Cs,j->(Z=Cs_j;{sum(length Z+1,i->rank Z_i),j}))/last)
tally apply(Cs,C-> apply(length C+1,i->rank HH_i C))

netList apply(Cs, C-> (C, apply(length C+1,i->rank HH_i C)))

C=Cs_1**Cs_2
F=ZZ/nextPrime 1000
CF=C**F
hF=elapsedTime apply(length C+1,i-> rank HH_i CF)

CR=C**RR_53
A=elapsedTime SVDHomology CR
hR=apply(keys A_0,k->A_0#k)
 assert(hR == hF)
elapsedTime U=SVDComplex CR;
Sigma = target U

e=1e-10
nearlyZero=chainComplex apply(length C,i->(U_i*C.dd_(i+1)*transpose U_(i+1)-Sigma.dd_(i+1)))
maximalEntry nearlyZero
///
TEST ///
restart
load "randomComplexes.m2"

setRandomSeed 7

elapsedTime Cs= apply(10,i->(
	while (
	    while ( C= randomChainComplex(5,10); length C <1) do ();
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
hF = new MutableHashTable
F=ZZ/nextPrime 1000
CF=C**F
time for i from 0 to length C do hF#i = rank HH_i CF
B=new HashTable from hQ    
assert(A_0 === B)
///



TEST ///
restart
load "randomComplexes.m2"
setRandomSeed"test SVD"
h={1,4,10,4,1} 
r={10,20,20,10}
elapsedTime C=randomChainComplex(h,r,Height=>3,WithLLL=>true,zeroMean=>true)
maximalEntry C
CR=C**RR_53
elapsedTime SVDHomology CR
elapsedTime SVDHomology(CR,Strategy=>Laplacian)
C1=randomChainComplex({1,1},{5})**RR_53
CR1=CR**C1
A=elapsedTime SVDHomology CR1
h={1,5,14,14,5,1}
r={10,10,10,10,10}
C=randomChainComplex(h,r,Height=>1000,WithLLL=>true,zeroMean=>true)
CR=C**RR_53
maximalEntry CR
B=elapsedTime SVDHomology CR
elapsedTime SVDHomology(CR,Strategy=>Laplacian)
assert(A_0===B_0)
///

TEST ///
restart
load "randomComplexes.m2"
hts=new MutableHashTable
for r from 4 to 18 do (
    n=r+1; a=testTimeForLLLonSyzygies(r,n);
    while (n=n+1,b=testTimeForLLLonSyzygies(r,n); b_1 <0.1) do (a=b); 
    hts#(r,n-1)=a;
    hts#(r,n)=b;
    )
Hts=new HashTable from hts

t=apply(100,c->(testTimeForLLLonSyzygies(10,43))_1); 
min t, max t
histogram(t,10)
///

TEST ///
restart
load "randomComplexes.m2"
needsPackage "FreeResolutions"
needsPackage "AGRExamples"
R=QQ[a..h]
Rp=(ZZ/32003)(monoid R)
R0=(RR_53)(monoid R)
deg=4
nextra=10
setRandomSeed "1"
F=sum(gens R,x->x^deg)+sum(nextra,i->(random(1,R))^deg);
elapsedTime I=ideal fromDual matrix{{F}};
elapsedTime C=res(I,FastNonminimal =>true);
betti C
elapsedTime minimalBetti sub(I,Rp)
elapsedTime SVDBetti C 
C1=constantStrand(C,RR_53,9)


C=chainComplex({C1.dd_6,C1.dd_7})
betti C
maximalEntry C
SVDHomology C
SVDHomology(C,Strategy=>Laplacian)

histogram(first SVD C.dd_1/log,10)
histogram(first SVD C.dd_2/log,10)
numericRank C.dd_1, numericRank C.dd_2
betti C
///

TEST ///
restart
load "randomComplexes.m2"
M=(randomChainComplex({50,50},{50},zeroMean=>true)).dd_1;
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