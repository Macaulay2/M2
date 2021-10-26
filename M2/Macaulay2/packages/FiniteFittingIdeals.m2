--FiniteFittingIdeals - A package for computing Fitting ideals of finite modules
--*- coding: utf-8 -*-
---------------------------------------------------------------------------
--
-- PURPOSE: Computing the defining ideals of Quot schemes of points
-- PROGRAMMERS : Gustav Sædén Ståhl 
-- UPDATE HISTORY : March 2015 v0.1, May 2015 v1.0
---------------------------------------------------------------------------
newPackage("FiniteFittingIdeals",
    Headline => "Fitting ideals of finite modules",
    Version => "1.0",
    Date => "May 5, 2015",
    Authors => {
	{Name => "Gustav Sædén Ståhl",
	    HomePage => "http://www.math.kth.se/~gss",
	    Email => "gss@math.kth.se"}
      	},
    Keywords => {"Commutative Algebra"},
    DebuggingMode => false
    )

export{
    "nextDegree",
    "gaussCol",
    "co1Fitting",
    "affinePart",
    "gotzmannTest",
    "quotScheme"
    }


nextDegree = method(TypicalValue => Matrix)
nextDegree(Matrix,ZZ,Ring) := (M,d,S) -> (
    L:=entries transpose M;
    v:=flatten entries vars S;
    k:=#flatten entries gens power(ideal(v),d);
    p:=sub(numrows(M)/k,ZZ);
    R:=(ring M)[v]; 
    f := (L,p,d,v,t) -> (
    	q:=symbol q;
    	X:=t*(entries gens power(ideal(v),d))_0;
    	AA:=(entries gens power(ideal(v),d+1))_0;
    	LL:=for r in X list position(AA,i->i==r);
    	n:=#AA;
    	R2:=R[q_1..q_n];
    	N:={};
    	u:=-1;
    	for j to p-1 do (
    	    K:=toList(q_1..q_n);
    	    for l in LL do (
	    	K=apply(K,k->(if position(K,i->i==k)==l then (u=u+1;L_u) else k)););
	    K=apply(K,k->sub(k,ring(L_0)));
	    N=append(N,K););
    	flatten flatten N);
    L2:=for t in v list f(L_0,p,d,v,t);
    N:=transpose matrix L2;
    for i from 1 to (numcols(M)-1) do (
	L2=for t in v list f(L_i,p,d,v,t);
    	N=N|(transpose matrix L2));
    N);

nextDegree(Module,ZZ,Ring) := (M,d,S) -> (
    nextDegree(gens M,d,S));


-- PURPOSE: Doing column reductions in a matrix
gaussCol = method(TypicalValue => Matrix)

gaussCol(MutableMatrix) := M -> (
    n:=min(numcols M-1,numrows M-1);
    gaussa:=(M,i)-> (
	R:=(entries matrix M)_i;
	p:=position(R,r->r==(-1)_(ring M));
	if class p === Nothing then return M;
	for j from 0 to (numcols M)-1 do (
	    M=columnAdd(M,j,M_(i,j),p));
	return M);
    for q to n do (
	M=gaussa(M,q));  
    M);

gaussCol(Matrix) := M -> (
    matrix gaussCol(mutableMatrix M));



-- PURPOSE: Compute the (n-1)'th Fitting ideal of a matrix M

co1Fitting = method(TypicalValue => Ideal)

co1Fitting(Matrix) := (M) -> (
    M=matrix gaussCol(M);
    L:=entries transpose M;
    L2:=for l in L list if not member((-1)_(ring M),l) then l else {};
    L2=delete({},L2);
    L3:=flatten for l in L2 list delete(0_(ring M),l);
    trim ideal(L3));

co1Fitting(Module) := M -> (
    co1Fitting(relations M));
	
affinePart = method(TypicalValue => Matrix)
	 	    
affinePart(Matrix,List) := (M,L) -> (
    if not #L == numrows M then (print "Not correct number of rows";return;);
    M=mutableMatrix M;
    R:=ring M;
    for i in L do (
	p:=position(L,r->r==i);
	for j to p-1 do M_(j,i)=0_R;
	M_(p,i)=1_R;
	for j from p+1 to numrows M-1 do M_(j,i)=0_R); 
    matrix M);

affinePart(Matrix,ZZ) := (M,n) -> (
    affinePart(M,{n}));

dividingCoefficients := m -> (
    L:={};
    for l in flatten entries vars (A:=ring m) do if not sub(m/l,A)==0 then L=append(L,l);
    if L=={} then {1} else L);


--PURPOSE: Check if we have chosen a Gotzmann set.
gotzmannTest = method();

gotzmannTest(Module,ZZ,List) := (M,d,L) -> (
    S:=ring M;
    p:=rank M;
    if not relations M == 0_S then (print "Not free";return;);
    v:=flatten entries vars S;
    x:=v_0;
    Lista:=flatten entries gens power(ideal(v),d);   
    n:=#Lista; 
    bool:=true;
    test:= B -> (
	boolean:=true;
	boolean=all(B,m -> not sub(m/x,S)==0);
	for m in B do (
	    T:=for l in dividingCoefficients(m) list member(sub(x*m/l,S),B);
	    boolean=boolean and (unique T=={true}));
	boolean);
    for i to p-1 do (
	B:=delete(0,for l in L list if l//n==i then Lista_(l%n) else 0);
	bool=bool and test(B));
    bool);

gotzmannTest(List,RingElement) := (B,x) -> (
    	A:=ring x;
	d:=degree B_0;
	boolean:=all(B,m -> degree m == d);
	boolean=boolean and all(B,m -> not sub(m/x,A)==0);
	for m in B do (
	    T:=for l in dividingCoefficients(m) list member(sub(x*m/l,A),B);
	    boolean=boolean and (unique T=={true}));
	boolean);

quotScheme = method();
quotScheme(Module,ZZ,List):=(Q,n,L) -> (
    a:=symbol a;
    S:=ring Q;
    if not relations Q == 0_S then (print "Not a free module";return;);
    p:=rank Q;
    r:=#(flatten entries vars S)+1;    
    d:=n;
    v:=flatten entries vars S;
    mon:=flatten entries gens power(ideal(v),d);
    q:=#mon;
    B:=ZZ[a_1..a_(q*p*n)];
    M:=matrix{toList(a_1..a_(q*p))};
    for w from 2 to n do M=M||matrix{toList(a_((w-1)*q*p+1)..a_(w*q*p))};
    if not gotzmannTest(Q,d,L) then (print "NO"; return;);
    M=affinePart(M,L);
    K:=gens ker M;
    K2:=nextDegree(K,d,S);
    co1Fitting(K2))
	    
	    
	    
----------------------
beginDocumentation()

document {
     	Key => FiniteFittingIdeals,
	Headline => "Computing Fitting ideals and Quot schemes of points",
	
	"This package implements an algorithm for computing Fitting
	ideals of finite modules from the paper ", EM "Gotzmann's
	persistence theorem for finite modules ",
	HREF("http://arxiv.org/abs/1411.7940",  "arXiv:1411.7940"),
	" by Gustav Sædén Ståhl.",
		
	PARA{}, "The following is an example illustrating the main functions provided in the package.",
	UL {
	     {TO "Fitting ideals of finite modules"},
	     },
		
	}
 doc ///
  Key
    "Fitting ideals of finite modules"
  Headline
    An introductory example
  Description
   Text
     Consider a polynomial ring S=\mathbb{Z}[x_0,\dots,x_r], and a quotient
     Q=S^p/N where N is a homogeneous submodule generated in degrees at
     most d. Suppose that Q_d is free of rank n. We then have a short
     exact sequence 0\to N_d\to S_d^p\to Q_d\to 0 where also N_d is
     free. Thus, N_d\otimes S_1\to S^p_{d+1}\to Q_{d+1}\to 0 is a free
     presentation of Q_{d+1}. If the basis of Q_d can be chosen as a so
     called Gotzmann set, then Q_{d+1} is free of rank n if and only if
     the (n-1)'th Fitting ideal of Q_{d+1} is zero.
     
     As an example, we consider a quotient Q of S^2=\mathbb{Z}[x_0,x_1]^2 such
     that Q_1 is free of rank 1. As 
     S^2_1=\mathbb{Z}<xe_1,ye_1,xe_2,ye_2> is a free
     \mathbb{Z}^4-module we have that Q_1 is the cokernel of a
     (1\times 4)-matrix.
   Example
     S=ZZ[x_0,x_1];
     R=ZZ[a_1..a_4];
     Q1=matrix{{a_1,a_2,a_3,a_4}}
   Text
     Suppose that Q_1 has a basis given by xe_1, corresponding to the 0'th column. 
     We check that this is a Gotzmann set, and calculate a free presentation of Q_2. 
   Example
     gotzmannTest(S^2,1,{0})
     Q1=affinePart(Q1,{0})
     N1=gens ker Q1
     N2=nextDegree(N1,1,S)
   Text
     N_2 is the matrix corresponding to the map N_1\otimes S_1\to S^2_2,
     so Q_2 is the cokernel. We want to calculate the (n-1)'th Fitting
     ideal of N_2 with n=1. 
   Example
     co1Fitting(N2)
   Text
     Thus, the obstruction for Q_2 to be free of rank 1 is the
     equation a_2a_3-a_4. 
     
     A result is that the Quot scheme of rank n quotients of \mathcal{O}^p is
     cut out by a single (n-1)'th Fitting ideal in the Grassmannian of
     locally free rank n quotients of a push forward of
     \mathcal{O}(d)^p for d\ge n. 
     
     In the case above, we have the Grassmannian Gr(1,4)=\mathbb{P}^3,
     and the Quot scheme is given by a (n-1)'th Fitting ideal. 
     All of the above calculations can also be done directly by:
   Example
     quotScheme(S^2,1,{0})
     
   Text
     We can calculate much bigger examples with these function than
     with the ordinary Fitting ideal function. As an example, we
     consider the following with rank 2 quotients of S^2.
   Example
     S=ZZ[x,y];
     R=ZZ[a_1..a_12];
     Q2=matrix{toList(a_1..a_6),toList(a_7..a_12)}
     Q2=affinePart(Q2,{0,1})
     K3=nextDegree(gens ker Q2,2,S);
     time I=co1Fitting(K3)
     time J=fittingIdeal(2-1,coker K3);
     I==J
   Text
     Note that our method is a bit faster for this small example, and
     for rank 2 quotients of S^3=\mathbb{Z}[x,y]^3 the time difference
     is massive.
   
  SeeAlso
    gaussCol
    co1Fitting
    nextDegree
    gotzmannTest
    affinePart
    quotScheme

/// 
     
 
 doc ///
  Key
    gaussCol
    (gaussCol,MutableMatrix)
    (gaussCol,Matrix)
  Headline
     Makes column reductions of a matrix
  Usage
     gaussCol M
  Inputs
    M:Matrix
    M:MutableMatrix
  Outputs
    :Matrix
  Description
   Text
     This function does certain column reductions in a matrix M, if M
     is of a certain type as in this example.
   Example
     S=ZZ[x,y];
     R=S[a_1..a_4];
     M=matrix{{1,a_2,a_3,a_4}};
     K=nextDegree(ker M,1,S)
   Text
     What it does is that it starts from the bottom right and if it finds a -1
     then it uses that column to reduces all other columns to a form with a zero
     in the corresponding row. It then continues up left searching for the next
     -1 and repeats the procedure.
   Example   
     gaussCol(K)
   
  SeeAlso
    nextDegree
    co1Fitting
    affinePart
///

 doc ///
  Key
    nextDegree
    (nextDegree,Module,ZZ,Ring)
    (nextDegree,Matrix,ZZ,Ring)
  Headline
     Lifts the kernel to the next degree
  Usage
     nextDegree M,d,S
  Inputs
    M:Matrix
    M:Module
    d:ZZ
    L:Ring
  Outputs
    :Matrix
  Description
   Text
     Let S be a polynomial ring and consider a quotient Q=S^p/N where
     N is a submodule generated in degrees at most d. If the graded
     component Q_d is free, then N_d is free as well, and
     N_d\otimes S_1 \to S^p_{d+1} \to Q_{d+1}\to 0 gives a free
     resolution of Q_{d+1}. The function nextDegree takes the matrix
     corresponding to the map N_d\to S^p_d and gives the matrix
     corresponding to the map N_d\otimes S_1\to S^p_{d+1}. 	 
   Example
     S=ZZ[x_0,x_1];
     R=S[a_1..a_4];
     K=gens ker matrix{{1,a_2,a_3,a_4}}
     nextDegree(K,1,S)
        
  SeeAlso
    gaussCol
    affinePart
    co1Fitting
///

 doc ///
  Key
    co1Fitting
    (co1Fitting,Module)
    (co1Fitting,Matrix)
  Headline
     Calculates the (n-1)'th Fitting ideal of a finite module
  Usage
     co1Fitting Q
  Inputs
    Q:Matrix
    Q:Module
  Outputs
    :Ideal
  Description
   Text
     Let S be a polynomial ring and consider a quotient Q=S^p/N where
     N is a submodule generated in degrees at most d. If the graded
     component Q_d is free of rank n, then N_d is free as well, and
     N_d\otimes S_1 \to S^p_{d+1} \to Q_{d+1}\to 0 gives a free
     resolution of Q_{d+1}. Let K be the matrix corresponding to the
     map  N_d\otimes S_1\to S^p_{d+1}. The function co1Fitting
     calculates the (n-1)'th Fitting ideal of Q_{d+1} assuming that the
     basis of Q_d was given by a Gotzmann set.
   Example
     S=ZZ[x_0,x_1];
     R=S[a_1..a_4];
     K=gens ker matrix{{1,a_2,a_3,a_4}}
     K2=nextDegree(K,1,S)
     co1Fitting(K2)
        
  SeeAlso
    gaussCol
    nextDegree
    gotzmannTest
    affinePart
///

 doc ///
  Key
    gotzmannTest
    (gotzmannTest,Module,ZZ,List)
    (gotzmannTest,List,RingElement)
  Headline
     Checks if a set of monomials is a Gotzmann set
  Usage
     gotzmannTest(M,d,L)
     gotzmannTest(B,x)
  Inputs
    M:Module
    d:ZZ
    L:List
    B:List
    x:RingElement
  Outputs
    :Boolean
  Description
   Text
     For us, a Gotzmann set will be a set L of monomials of degree d in the
     variables x_0,\dots,x_r with the property that if m\in L, then x_0 divides m
     and if x_i divides m, then (x_0m)/x_i\in L. The function
     gotzmannTest checks if a set of monomials fulfills this property.
   Example
     S=ZZ[x,y,z];
     L={x^3,x^2*y,x^2*z,x*y*z}
     gotzmannTest(L,x)
   Text
     A non example of a Gotzmann set is L_2=\{x^3,x^2y,xz^2\}.
   Example
     L2={x^3,x^2*y,x*z^2}
     gotzmannTest(L2,x)
   Text
     L_2 is not a Gotzmann set since it does not contain x^2z. 
     
     When we consider a free S-module S^p with basis e_1,\dots,e_p, 
     then we generalize our notion of Gotzmann set for x so that a 
     set L is a Gotzmann set if it is a union of Gotzmann sets for 
     x for e_1,\dots,e_p.
     
     As an example in S^2=\mathbb{Z}[x,y,z]^2 we have a Gotzmann set 
     L=\{x^2e_1,xye_1,x^2e_2\}\ as it is a Gotzmann set in each coordinate.
     We can test this be gotzmannTest(S^p,d,I), where d is the degree of
     the monomials, and I is the index of the monomials of L listed in the 
     lexicographical order x<y<z<e_1<e_2. In our case we have d=2 and 
     I=\{0,1,6\}\ since:
     
     x^2e_1<xye_1<xze_1<y^2e_1<yze_1<z^2e_1<x^2e_2<xye_2<xze_2<y^2e_2<yze_2<z^2e_2.
     
   Example
     gotzmannTest(S^2,2,{0,1,6})

        
  SeeAlso
    affinePart
///

 doc ///
  Key
    quotScheme
    (quotScheme,Module,ZZ,List)
  Headline
     Calculates the defining equations for Quot schemes of points
  Usage
     quotScheme(Q,n,L)
  Inputs
    Q:Module
    n:ZZ
    L:List
  Outputs
    :Ideal
  Description
   Text
     The Quot scheme of n points of \mathcal{O}^p on \mathbb{P}^r
     embeds as a closed subscheme of the  Grassmannian of rank n
     quotients  of a push forward of \mathcal{O}(d)^p.  
     This function gives the defining equations of this closed subscheme. 
   Example
     S=ZZ[x_0,x_1];
     quotScheme(S^2,1,{0})
        
  SeeAlso
    co1Fitting
///

doc ///
  Key
    affinePart
    (affinePart,Matrix,List)
    (affinePart,Matrix,ZZ)
  Headline
     Replaces columns in a matrix with an identity matrix
  Usage
     affinePart(M,L)
     affinePart(M,n)
  Inputs
    M:Matrix
    L:List
    n:ZZ
  Outputs
    :Matrix
  Description
   Text
     Given an n\times m matrix M, with n<m, and a subset
     L\subseteq\{0,\dots,m-1\}\  of size n, this function replaces the
     columns with index L by an identity matrix.
   Example
     S=ZZ[a_1..a_8];
     M=matrix{toList(a_1..a_4),toList(a_5..a_8)}
     affinePart(M,{0,2})
        
  SeeAlso
    nextDegree
///

TEST ///
S=ZZ[x_0,x_1];
assert(gotzmannTest(S^2,1,{0}))
///

TEST ///
S=ZZ[x_0,x_1];
assert(numgens quotScheme(S^2,1,{0})==1);
///

TEST ///
S=ZZ[x,y];
R=ZZ[a_1..a_12];
Q2=matrix{toList(a_1..a_6),toList(a_7..a_12)};
assert(numrows nextDegree(gens ker(affinePart(Q2,{0,1})),2,S)==8);
///


TEST ///
S=ZZ[x,y];
R=ZZ[a_1..a_12];
Q2=matrix{toList(a_1..a_6),toList(a_7..a_12)};
K=nextDegree(gens ker(affinePart(Q2,{0,1})),2,S);
assert(numgens co1Fitting(K)==4);
///
