newPackage(
        "HigherCIOperators",
        Version => "0.5", 
        Date => "May 10, 2015",
        Authors => {{Name => "David Eisenbud", 
                  Email => "de@msri.org",
                  HomePage => "http://www.msri.org/~de/"}},
        Headline => "higher CI operators",
	Keywords => {"Commutative Algebra"},
        DebuggingMode => false,
     	PackageImports => { "CompleteIntersectionResolutions" },
	PackageExports =>{"MCMApproximations", "OldChainComplexes"}
        )

export {"exteriorMultiplication",
	"higherCIOperators",
	"makeALDifferential",
	"ciOperatorResolution",
	--"syzygyModule",
	"trueKoszul"
       }
--notify = true

trueKoszul=method()
trueKoszul Matrix := ff -> (
    len := numcols ff;
    gg := map(target ff,source ff, (i,j)-> (-1)^j*ff_(len-1-j)_0); -- was source ff
    K1 := koszul gg;
    Klist := apply(len, i-> 
	map(K1_i, K1_(i+1), (-1)^i*transpose K1.dd_(len -i)));
    mapList := {map((ring gg)^1,,matrix Klist_0)};
    scan(toList(1..len-1), i-> mapList =  append(mapList, map(source last mapList, , matrix Klist_i)));
    chainComplex mapList
    )

-*syzygyModule = method()
syzygyModule (ZZ, Module) := (d,M)->(
    F:= res(M, LengthLimit => d+1);
    coker F.dd_(d+1))
*-
ciOperatorResolution=method()
ciOperatorResolution(ChainComplex, ChainComplex) := (A,L) ->(
    u :=higherCIOperators(A,L);
    chainComplex apply(toList(1..length A), 
           k->makeALDifferential(k,A,L,u))
       )

makeALDifferential= method()
makeALDifferential(ZZ,ChainComplex, ChainComplex, HashTable) := 
                         (j, A,L,u) ->(
--make the differential from the j-th diag A_j**L_0++A_(j-1)**L_1...
--to the (j-1)-st diagonal A_(j-1)**L_0++...
    if j>length A then error "j is too large";
    sour :=directSum apply(min(j+1, length L+1), 
	        i-> A_(j-i)**L_i);
    tar := directSum apply(min(j, length L+1), 
	        i-> A_(j-1-i)**L_i);
    matList := apply(min(j, length L+1), r -> --pos in tar
	             apply(min(j+1,length L+1), s-> --pos in sour
			     if s>r+1 then 
			     map(A_(j-1-r)**L_r,A_(j-s)**L_s,0) 
			     else
			     u#{r-s+1,j-s,s})
			 );
    map(tar, sour, matrix matList)
    )


higherCIOperators = method()
higherCIOperators(ChainComplex, ChainComplex) := (A,L) ->(
--L is a koszul complex, resolution of coker ff over S
--A is the lift to S of (part of) a resolution over R = S/ideal ff
--u#{n,p,q}: A_p **L_q --> A_(p-n)**L_(q+n-1)
--will be defined for all {n,p,q} such that:
--length A >= p >= n >=0
--and 
--if n=0 then length L >= q >= 1
--else
--length L-n+1 >= q >=0  
--
--method: first define the maps for n=0,1 "by hand".
--Then do induction: suppose p>=n>=1, and 
--that all maps for smaller n have been made.
--(note that because q=0, we don't need the map
--with same n, smaller q; it would be 0.)
--note that we need length A >=p and length L >= p-1
--n>=2
--L := res coker ff; -- a Koszul complex
mu := exteriorMultiplication length L; -- multiplication maps for the Koszul complex,
--defined over ZZ.
u := new MutableHashTable;
--n=0
scan(length A+1, p->
    scan(length L, q->
    u # {0,p,q+1} = A_(p)**((-1)^p)*L.dd_(q+1)
    ));
--n=1
scan(length A, p->
    scan(length L+1, q->
	u # {1,p+1,q} = A.dd_(p+1) **L_(q)
	));
--Note that we could have just done the case q=0, then used the tensor method below.
--n>=2

--first make the maps for a given n, p with q = 0
--then define u#{n,p,q} for q<= length L -n+1 from u#{n,p,0}
scan(toList(2..min(length A, length L+1)), n->(
	scan(toList(n..length A), p->(
		u#{n,p,0} = map(A_(p-n)**L_(n-1),A_p**L_0,
		-sum(toList(1..n-1), i -> (
  		 (u#{i, p-n+i, n-i-1}*u#{n-i,p,0})))//u#{0,p-n,n-1});
		scan(toList(1..length L - n + 1), q->(
		    u#{n,p,q} = map(A_(p-n)**L_(q+n-1) , A_p**L_q,
		    (A_(p-n)**mu#{n-1,q})*(u#{n,p,0}**L_q))))
	));
));
hashTable pairs u)

-*scan(toList(2..min(length A, length L+1)), n->(
	scan(toList(n..length A), p->(
		u#{n,p,0} = 
		-sum(toList(1..n-1), i -> (
  		 (u#{i, p-n+i, n-i-1}*u#{n-i,p,0})))//u#{0,p-n,n-1};
		scan(toList(1..length L - n + 1), q->(
		    u#{n,p,q} = 
		    (A_(p-n)**mu#{n-1,q})*(u#{n,p,0}**L_q)))
*-



exteriorMultiplication = method()
exteriorMultiplication ZZ := n ->(
mu := new MutableHashTable;
numvars := n;
x := symbol x;
y := symbol y;
X := symbol X;
Y := symbol Y;
--T := QQ[vars(0..2*numvars-1), 
--    Degrees => {numvars:{1,0}, numvars:{0,1}},
--    SkewCommutative =>true];
T := QQ[x_0..x_(2*numvars -1),
    Degrees => {numvars:{1,0}, numvars:{0,1}},
    SkewCommutative =>true];
T1 := QQ[x_0..x_(2*numvars -1),
    SkewCommutative =>true];
move := map(T1,T,vars T1);
diag := ideal apply(numvars, i->T1_i - T1_(numvars+i));
Tbar := T1^1/diag;
reduce := map(Tbar, T1^1, matrix{{1_T1}});
scan(numvars+1, p->
    scan(numvars+1-p, q->
    mu#{p,q} = lift((reduce*move(basis({p,0}, T^1)**basis({0,q},T^1)))//
               basis(p+q,Tbar), ZZ)
	       ));
       hashTable pairs mu)


///
--the following routines were used to verify that we got the
--exteriorMultiplication routines right.

checkAssociativity = method()
checkAssociativity(ZZ, HashTable) := (n,mu) ->(
    numvars:=n;
scan(toList(0..numvars), p1->
    scan(toList(0..numvars-p1), p2->
	scan(toList(0..numvars-p1-p2), p3->
	   print(
	      0== (mu#{p1+p2,p3}*(mu#{p1,p2}**mu#{p3,0}) - 
	   mu#{p1,p2+p3}*(mu#{0,p1}**mu#{p2,p3}))
	       )
	   )))
)

reverseTensor = method()
reverseTensor(ZZ,ZZ) := (p,q) ->(
    m := mutableMatrix map(ZZ^(p*q), ZZ^(p*q), 0);
    scan(p, x -> 
	scan(q, y->
	    m_(x+p*y, y+q*x) = 1
	    ));
	matrix m)
///
TEST///
reverseTensor = method()
reverseTensor(ZZ,ZZ) := (p,q) ->(
    m := mutableMatrix map(ZZ^(p*q), ZZ^(p*q), 0);
    scan(p, x -> 
	scan(q, y->
	    m_(x+p*y, y+q*x) = 1
	    ));
	matrix m)

N = 7;p=2;q=3;
mu = exteriorMultiplication N;
--check skew commutativity
assert(
    mu#{p,q}*reverseTensor(binomial(N,q),binomial(N,p)) == (-1)^(p*q)*(mu#{q,p})
    )
///


-----------------------------
--------Documentation-----------
--------------------------------
-- <<docTemplate


-*
restart
loadPackage ("CompleteIntersectionResolutions", Reload=>true)
uninstallPackage "CompleteIntersectionResolutions"
installPackage "CompleteIntersectionResolutions"
check "CompleteIntersectionResolutions"
viewHelp CompleteIntersectionResolutions
viewHelp higherCIOperators
viewHelp complexity
*-

beginDocumentation()

doc ///
   Key 
    trueKoszul
    (trueKoszul, Matrix)
   Headline
    "Makes Koszul complex, with bases sorted in lex"
   Usage
    K = trueKoszul ff
   Inputs
    ff:Matrix
     Matrix with the elements on which to build the Koszul complex
   Outputs
    K:ChainComplex
   Description
    Text
     The usual Koszul command produces a complex with the basis
     sorted in revlex. The sort in lex matches the sort of the monomials
     in the exterior algebra.
    Example
     S = ZZ/101[a,b,c,d]
     ff = matrix{{a,b,c,d}}
     (koszul ff).dd_2
     (trueKoszul ff).dd_2
     basis(2,(ZZ/101[a,b,c,d, SkewCommutative => true]))
   SeeAlso
    koszul    
///

doc ///
   Key
    exteriorMultiplication
    (exteriorMultiplication,ZZ)
   Headline
    "multiplication maps in the exterior algebra"
   Usage
    mu = exteriorMultiplication n
   Inputs
    n:ZZ
     positive integer
   Outputs
    mu:HashTable
     $mu\{p,q\}\wedge^p k^n\otimes \wedge^q k^n \to \wedge^{p+q}k^n$
   Description
    Text
     The basis of each $\wedge^p k^n$ is given in lex order.
   Caveat
    This is not the order of the basis in the output of
    koszul ff; rather, use trueKoszul ff.
   SeeAlso
    trueKoszul
///

doc ///
   Key
    higherCIOperators
    (higherCIOperators, ChainComplex, ChainComplex)
   Headline
    "creates the HashTable of higher CI operators on a lifted resolution"
   Usage
    u = higherCIOperators(A,L)
   Inputs
    A:ChainComplex
     lifted resolution from complete intersection $S\to R$
    L:ChainComplex
     Koszul complex resolving R over S
   Outputs
    u:HashTable
     $u\{n,p,q\}$ is a map $A_p\otimes L_q \to A_{p-n}\otimes L_{q+n-1}$
   Description
    Text
     $A$ is the sequence of maps (generally not really a complex)
     obtained by lifting the differentials of a free resolution
     over $R$ back to $S$.
     
     Definition: $u\{n,p,q\}$ 
     is determined by induction on $n$ and the
     rules
     
     $u\{0,p,q\} = (-1)^q(A_p \otimes d_L) :\ A_p\otimes L_q \to 
     A_p\otimes L_{q-1}$
     
     $u\{1,p,q\} = d_A \otimes  L_q: A_p\otimes L_q \to 
     A_{p-1}\otimes L_q$
     
     $\sum_{i+j=n} u\{j,p-i,q+i-1\} * u\{i,p,q\} = 0$
     
     and
     
     $u\{n,p,q\}: A_p\otimes L_q \to
     A_{p-n}\otimes L_{q+n-1} = \mu * u\{n,p,0\}\otimes L_q$,
     
     where 
     
     $\mu:L_{n-1}\otimes L_q \to L_{n+q-1}$
     
     is the multiplication in
     the Koszul algebra.
     
     The output $u\{n,p,q\}$ 
     will be defined for all keys  $\{n,p,q\}$ such that:
     $length(A) \geq p \geq n \geq 0$
     and 
     if $n=0$ then $length(L)\geq q \geq 1$,
     else
     $length(L)-n+1 \geq q \geq 0$.

     
     The maps $u\{2,p,q\}$ are thus the classical CI operators 
     from Eisenbud [1981], while the $u\{3,p,q\}$ 
     define maps of the  modules
     $Ext_R^{odd, \geq 3}(M,k) \to E_R^{even}(M,k)$
     and are obstructions to commutativity of the classic ci operators
     on the R-free resolution of M.
     
     These maps are used to construct the differentials in the
     lifted CI resolution
   SeeAlso
    makeALDifferential
    ciOperatorResolution
///

doc ///
   Key
    makeALDifferential
    (makeALDifferential,ZZ,ChainComplex, ChainComplex, HashTable)
   Headline
    "makes the differential used in ciOperatorResolution"
   Usage
    dj = makeALDifferential(j, A, L, u)
   Inputs
    j:ZZ
     which differential
    A:ChainComplex
     lift of resolution from complete intersection S-->R
    L:ChainComplex
     Koszul complex resolving R over S
    u:HashTable
     output of higherCIOperators
   Outputs
    dj:Matrix
     j-th differential of the ciOperatorResolution AL
   SeeAlso
    ciOperatorResolution
    higherCIOperators
///

doc ///
   Key
    ciOperatorResolution
    (ciOperatorResolution, ChainComplex, ChainComplex)
   Headline
    "lift resolution from complete intersection using higher ci-operators"
   Usage
    AL = ciOperatorResolution(A,L)
   Inputs
    A:ChainComplex
     lift over $S\to R$ of an R-free resolution
    L:ChainComplex
     algebra resolution of R over S; for now, Koszul complex
   Outputs
    AL:ChainComplex
     resolution over S of same module
   Description
    Text
     If S is a ring, R = S/(f1..fc) a complete intersection, 
     A the lift to S of an R-free resolution of a module M,
     and L the Koszul complex resolving R over S,
     the script constructs the "higher ci operators" on A of 
     Eisenbud-Peeva-Schreyer and uses them to construct a -usually nonminimal-
     S-free resolution of M. The resulting resolution has the
     structure of a module over the exterior algebra.
     
     This construction is in some sense dual to the Shamash
     construction of an R-free resolution from an S-free resolution,
     that uses higher homotopies and
     yields a resolution that is a module over the 
     divided power algebra.
     
     The same procedure would work starting with an algebra resolution
     of any S-algebra R, given a description of the multiplication
     on the algebra resolution.
    Example
     needsPackage "CompleteIntersectionResolutions"
     S = ZZ/101[a,b,c];
     ff = matrix"a4,b4,c4";
     N = coker matrix"a,b,c;b,c,a";
     R = S/ideal ff;
     M = highSyzygy (R**N);
     AA = res(M, LengthLimit => 5);
     Alist = apply(length AA, i-> lift(AA.dd_(i+1), S));
     A = chainComplex Alist;
     L = trueKoszul ff;
     AL = ciOperatorResolution(A,L)
     G = res pushForward(map(R,S),M)
     betti AL
     betti G
   SeeAlso
    higherCIOperators
///

-*
doc ///
   Key
    syzygyModule
    (syzygyModule, ZZ, Module)
   Headline
    "Make the d-th module of syzygies"
   Usage
    M = syzygyModule(d,N)
   Inputs
    d:ZZ
     non-negative integer
    N:Module
   Outputs
    M:Module
///
*-
doc ///
   Key
    HigherCIOperators
   Headline
    "Higher CI operators on a resolution over a complete intersection"
   Description
    Text
     The "higher CI operators" complete the structure of the
     ordinary CI operators on (sometimes called "Eisenbud operators")
     on a resolution over a complete intersection in the same sense
     that the "higher homotopies" complete the structure of homotopies on
     with respect to a sequence of elements. Details will appear in a
     preprint in preparation by Burke, Eisenbud and Schreyer.
     
     The higher CI operators are constructed by the routine higherCIOperators.
     
     Just as a system of higher
     homotopies for a regular sequence f_1..f_c
     on a resolution over a ring S allow  one to construct the Shamash resolution
     over R = S/(f_1..f_c), the higher CI operators are involved in a sort of
     dual construction: from a resolution F over R, lifted to
     a sequence of maps A over S, and lifted higher CI operators on A\otimes L,
     where L is the Koszul complex on f, one can construct a nonminimal resolution
     AL over S using the routine ciOperatorResolution.
 ///

end--
restart
loadPackage("HigherCIOperators", Reload=>true)
uninstallPackage "HigherCIOperators"
installPackage "HigherCIOperators"
viewHelp HigherCIOperators
check HigherCIOperators

