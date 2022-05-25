newPackage(
	"WhitStrat",
	Version => "1.1", 
    	Date => "May 24, 2022",
    	Authors => {{Name => "Martin Helmer", 
		  Email => "martin.helmer@anu.edu.au", 
		  HomePage => "http://martin-helmer.com/"}},
    	Headline => "Compute Whitney Statifications Algebraically",
    	DebuggingMode => false,
	Reload => true
    	);
needsPackage "Elimination";
needsPackage "PrimaryDecomposition";
needsPackage "Saturation";
needsPackage "SegreClasses";
export{
    "conormal",
    "conormRing",
    "whitneyStratify"
    }

conormRing=method(TypicalValue=>Ring);
conormRing(Ideal):=(I)->(
     R:=ring I;
    n:=numgens(R)-1;
    kk:=coefficientRing R;
    degs:=join(for i from 0 to n list {1,0},for i from 0 to n list {0,1});
    v:=symbol v;
    S:=kk[gens(R),v_0..v_n,Degrees=>degs];
    return S;

    );

conormRing(Ideal,List,List):=(I,Alist,alpha)->(
    R:=ring I;
    n:=numgens(R)-1;
    kk:=coefficientRing R;
    degs:=join(for i from 0 to n list {1,0},for i from 0 to n list {0,1});
    indset:=for vert in alpha list position(Alist,i->i==vert);
    v:=symbol v;
    S:=kk[gens(R),for i in indset list v_i,Degrees=>degs];
    print S;
    return S;

    );

conormal=method(TypicalValue=>Ideal);
conormal (Ideal):=(I)->(
    return conormal(I,conormRing(I));
    );
conormal (Ideal,List,List):=(I,A,alpha)->(
    return conormal(I,conormRing(I,A,alpha));
    );
conormal (Ideal,List,List,Ring):=(I,A,alpha,S)->(
    N:=conormal(I,conormRing(I,A,alpha));
    S1:=ring(N);
    indset:=for vert in alpha list position(A,i->i==vert);
    xN:=select(gens(S1),i->degree(i)=={1,0});
    vN:=select(gens(S1),i->degree(i)=={0,1});
    xS:=select(gens(S),i->degree(i)=={1,0});
    vS:=select(gens(S),i->degree(i)=={0,1});
    subList:={};
    count:=0;
    for i in indset do (
	subList=append(subList,xN_count=>xS_i);
	subList=append(subList,vN_count=>vS_i);
	count=count+1;
	);
    return sub(N,subList);
    );

conormal (Ideal,Ring):=(I,S)->(
    gbI:= groebnerBasis(I,Strategy=>"MGB");
    c:=codim ideal leadTerm gbI;
    J1:=jacobian(I);
    J:=minors(c, J1);
    IS:=sub(I,S);
    vs:=select(gens(S),i->degree(i)=={0,1});
    K:=minors(c+1,matrix{vs}||sub((transpose J1),S));
    N:=saturate((IS+K),sub(J,S));
    return N;
    );

whitneyStratify=method(TypicalValue=>Ideal);
whitneyStratify (Ideal):=(I)->(
    I=radical I;
    R:=ring I;
    Rint:=R;
    kk:=coefficientRing(R); 
    zzz:=symbol zzz;
    isAffine:=false;
    if not isHomogeneous(I) then (
	R=kk[gens R,zzz];
	I=homogenize(sub(I,R),zzz);
	isAffine=true; 
	);
    S:=conormRing(I);
    wS:=whitneyStratify(I,R,S);
    if isAffine then(
	V:=new MutableHashTable;
	v:={};
	for i from 0 to #wS-1 do(
	    v={};
	    for s in wS#i do(
		s2:=sub(sub(s, (ring s)_(numgens(R)-1)=>1),Rint);
		if not(s2==ideal(1_Rint))then (v=append(v,s2));
		);
	    V#i=v;
	    );
	wS=V;
	);
    return wS;
    );
whitneyStratify (Ideal,Ring,Ring):=(I,R,S)->(
    n:=numgens(R)-1;
    V:=new MutableHashTable;
    if codim(I)>n then return V;
    dimI:=n-codim(I);
    if dimI<0 then return V;
    V#dimI=decompose I;
    for i from 0 to dimI-1 do V#i={};
    J:=I+minors(codim I,jacobian I);
    if codim(J)>n then return V;
    mu:=n-codim(J);
    ell:=0;
    CI:=conormal(ideal mingens I,S);
    Cpull:=0;
    Ccap:=0;
    W:=0;
    for K in decompose(J) do(
	for i from 0 to dimI-1 do(
	    if i==(n-codim(K)) then V#i=append(V#i,K);
	    );
	);
    for i from mu+1 to dimI-1 do V#i=V#mu;
    if codim(J)==n then(
	use R;
	return V;
	);
    for j from 0 to mu do(
	ell=mu-j;
	for Z in V#ell do(
	    Cpull=(CI+sub(Z,S));
	    Ccap=conormal(Z,S)+CI;
	    W=(Z+sub(eliminate((gens S)_{(numgens(R))..numgens(S)-1},saturate(Cpull,Ccap)),R));
	    for K in decompose(W) do(
		for i from 0 to ell-1 do(
		    if i==(n-codim(K)) then V#i=append(V#i,K);
		    );
		);
	    );
	if V#ell!={} then(
	    V2:=whitneyStratify(intersect(V#ell),R,S);
	    for i from 0 to mu do(
		if V2#?i then V#i=unique(join(V#i,V2#i));
		);
	    );
	);
    use R;
    return V;
    );



beginDocumentation()
multidoc ///

Node 
     Key
     	  WhitStrat
     Headline
     	  Computes Whitney Statifications of complex varities.
     Description
     	  Text
	      This package computes Whitney stratifications of complex algebraic varieites using algorithms described in [1]. Homogeneous ideals will be treated as defining a projective variety, non-homogeneous ideals will be treated as defining affine varieties. Input ideals are assumed to define pure dimensional varieties.   
	      
	      References:
	      
	      [1] Martin Helmer and Vidit Nanda. "Cornormal Spaces and Whitney Stratifications". 
	      
	      
Node 
    Key
    	whitneyStratify
	(whitneyStratify, Ideal)
    Headline
    	Computes a Whitney stratification of the complex variety in affine or projective space.
    Usage
    	whitneyStratify(I)
    Inputs
    	I:Ideal
	    an ideal defining a closed subvariety of $\CC^n$ or $\PP^{n}$.
    Outputs
        WS:MutableHashTable
	    a hash table indexed by dimension, with the entry of dimension $i$ consisting of a list of prime ideals. The variety of each prime ideal defines a connected open stratum of $V(I)$ obtained by removing the varities of all lower dimensional entries of the hash table.    
    Description 
    	Text
	    For a affine or projective complex variety $X$ this command computes a Whitney stratification WS where WS#i is a list of strata closures in dimension $i$; for a prime ideal $P$ in WS#i the associated open (connected) strata is given by $V(P)-Z$ where $Z$ is the union of the varieties defined by the entries of WS#(i-1). We demonstrate the method for the affine Whitney umbrella and its projective closure below.
    	Example
	    R=QQ[x..z]
	    I=ideal(y^2*z-x^2)
	    WS=whitneyStratify I
	    peek WS
	Text
	    Now the projective version. 
	Example 
	    S=QQ[x..z,w]
	    I=ideal(y^2*z-x^2*w)
	    WS=whitneyStratify I
	    peek WS
	Text 
	    Another projective example.
	Example
	    R=QQ[a_0..a_4]
	    I=ideal(a_0^2*a_4-a_1*a_2^2+a_3^3-a_3*a_0^2-a_4^2*a_3)
	    V=whitneyStratify I
	    peek V
	Text 
	    Note that as with the Whitney umbrella simpley taking successive singular loci will not yield the correct stratification, in particular one would miss the two points defined by the second entry of V#0. 
	Example 
	    J=radical (I+minors(codim I, jacobian I))
	    J2=radical (J+minors(codim J, jacobian J))                   
Node 
    Key
    	conormal
	(conormal, Ideal)
    Headline
    	Computes the conormal variety
    Usage
    	conormal(I)
    Inputs
    	I:Ideal
	    a homogeneous ideal defining a closed subvareity of $\PP^{n}$.
    Outputs
        C:
	    an ideal defining the conormal variety in $\PP^n \times (\PP^n)^*$.
    Description 
    	Text
	    For a complex projective variety $X=V(I)\subset \PP^n$ this command computes the ideal of the conormal variety $Con(X)$ in $\PP^n \times (\PP^n)^*$.
	Example 
	    S=QQ[x..z,w]
	    I=ideal(y^2*z-x^2*w)
	    conormal I  	    		      	    
///	      
	      
TEST ///
-*  
    restart
    needsPackage "WhitStrat"
    needsPackage "SegreClasses"
    installPackage "WhitStrat"
*- 
n=3;
R=QQ[x_0..x_3]
I=ideal(x_1^2*x_2-x_3*x_0^2)
time V=whitneyStratify I
peek V
Eu=eulerObsToTop(V)
peek Eu
mu=linkingNumbers I
mu#"Strat"
mu#(0,3)
mu#"Eu"_(2,3)



sum for i from 0 to n-codim(I) list V#i
peek V
peek Eu
remove(V,2)
peek V
eulerObsToTop(V)
peek oo

peek V
peek Eu
V1=last V#0
pols=polarVars I
for p in pols list codim p
for p in pols list codim (p+V1)
polMults=new MutableHashTable;
Eu=new MutableHashTable;
for i from 0 to n-codim(I) do(
    pMdimi={};
    Eudimi={};
    for v in V#i do(
	pMult={};
	for p in pols do(
	    if codim(p+v)==codim(v) then(
		pMult=append(pMult,multiplicity(v,p));
		)
    	    else(
		pMult=append(pMult,0);
		);
    	    );
	pMult=reverse pMult;
	EuVi=sum(0..#pMult-1, j-> (-1)^j*pMult_j);
	pMdimi=append(pMdimi,pMult);
	Eudimi=append(Eudimi,EuVi);
	);
    Eu#i=Eudimi;
    polMults#i=pMdimi;
    );
peek Eu
peek polMults

v0={ideal(x_2,x_1,x_0), ideal(x_3,x_1,x_0)}
assert(V#0==v0)

R=QQ[x_0..x_2]
I=ideal(x_1^2*x_2-x_0^2)
V=whitneyStratify I
assert((first V#0)==ideal(gens R))

R=QQ[x_0..x_4]
I=ideal(x_0^2*x_4-x_1*x_2^2+x_3^3-x_3*x_0^2-x_4^2*x_3)
time V=whitneyStratify I
peek V
v0={ideal(x_4,x_3,x_2,x_0), ideal(x_3-x_4,x_2,x_1,x_0^2-2*x_4^2)}
assert(V#0==v0)
///
