newPackage(
	"WhitneyStratifications",
	Version => "1.2", 
    	Date => "July 29, 2022",
    	Authors => {{Name => "Martin Helmer", 
		  Email => "mhelmer@ncsu.edu", 
		  HomePage => "http://martin-helmer.com/"}},
    	Headline => "Compute Whitney Statifications Algebraically",
    	DebuggingMode => false,
	PackageImports=>{"Elimination","PrimaryDecomposition","Saturation"}
    	);
export{
    "conormal",
    "conormalRing",
    "whitneyStratify"
    }

conormalRing=method(TypicalValue=>Ring);
conormalRing(Ideal):=(I)->(
     R:=ring I;
    n:=numgens(R)-1;
    kk:=coefficientRing R;
    degs:=join(for i from 0 to n list {1,0},for i from 0 to n list {0,1});
    v:=symbol v;
    S:=kk[gens(R),v_0..v_n,Degrees=>degs];
    return S;

    );

conormalRing(Ideal,List,List):=(I,Alist,alpha)->(
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
    return conormal(I,conormalRing(I));
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
    S:=conormalRing(I);
    wS:=whitneyStratify(I,R,S);
    for i from 0 to #wS-1 do wS#i=unique toList(wS#i);
    if isAffine then(
	V:=new MutableHashTable;
	v:=new MutableList;
	for i from 0 to #wS-1 do(
	    v=new MutableList;
	    for s in wS#i do(
		s2:=sub(sub(s, (ring s)_(numgens(R)-1)=>1),Rint);
		if not(s2==ideal(1_Rint))then (v#(#v)=s2);
		);
	    V#i=toList(v);
	    );
	wS=V;
	);
    use R;
    return wS;
    );
whitneyStratify (Ideal,Ring,Ring):=(I,R,S)->(
    n:=numgens(R)-1;
    V:=new MutableHashTable;
    if codim(I)>n then return V;
    dimI:=n-codim(I);
    if dimI<0 then return V;
    V#dimI=decompose I;
    for i from 0 to dimI-1 do V#i=new MutableList;
    J:=I+minors(codim I,jacobian I);
    if codim(J)>n then return V;
    mu:=n-codim(J);
    ell:=0;
    W:=0;
    for K in decompose(J) do(
	for i from 0 to dimI-1 do(
	    if i==(n-codim(K)) then (V#i)#(#(V#i))=K;
	    );
	);
    for i from mu+1 to dimI-1 do V#i=V#mu;
    if codim(J)==n then return V;
    CI:=conormal(ideal mingens I,S);
    Cpull:=0;
    for j from 0 to mu do(
	ell=mu-j;
	for Z in V#ell do(
	    Cpull=(CI+sub(Z,S));
	    W=for q in decompose Cpull list sub(eliminate((gens S)_{(numgens(R))..numgens(S)-1},q),R);
	    for K in W do(
		for i from 0 to ell-1 do(
		    if i==(n-codim(K)) then (V#i)#(#(V#i))=ideal mingens(K+Z);
		    );
		);
	    );
	if #(V#ell)!=0 then(
	    V2:=whitneyStratify(intersect(toList(V#ell)),R,S);
	    for i from 0 to mu do(
		if V2#?i then V#i=(join(V#i,V2#i));
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
     	  WhitneyStratifications
     Headline
     	  Computes Whitney Statifications of complex varities.
     Description
     	  Text
	      This package computes Whitney stratifications of complex algebraic varieites using algorithms described in [1]. Homogeneous ideals will be treated as defining a projective variety, non-homogeneous ideals will be treated as defining affine varieties. Input ideals are assumed to define pure dimensional varieties.   
	      
	      References:
	      
	      [1] Martin Helmer and Vidit Nanda. "Cornormal Spaces and Whitney Stratifications", Foundations of Computational Mathematics, DOI: 10.1007/s10208-022-09574-8.
	      
	      
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
    needsPackage "WhitneyStratifications"
    installPackage "WhitneyStratifications"
*- 
--Whitney Umbrella, projective
R=QQ[x_0..x_3];
I=ideal(x_1^2*x_2-x_3*x_0^2);
V=whitneyStratify I;
v0={ideal(x_2,x_1,x_0), ideal(x_3,x_1,x_0)};
assert(V#0==v0);

--Whitney Umbrella, affine
R=QQ[x_0..x_2];
I=ideal(x_1^2*x_2-x_0^2);
V=whitneyStratify I;
assert((first V#0)==ideal(gens R));

--Whitney Cusp, affine 
R=QQ[x_1..x_3];
I=ideal(x_2^2+x_1^3-x_1^2*x_3^2);
V=whitneyStratify I;
assert((first V#0)==ideal(gens R));


--Whitney Cusp, projective 
R=QQ[x_0..x_3]
I=ideal(x_2^2*x_0^2+x_1^3*x_0-x_1^2*x_3^2);
V=whitneyStratify I;
assert((first V#0)==ideal(R_1,R_2,R_3));

R=QQ[x_0..x_4];
I=ideal(x_0^2*x_4-x_1*x_2^2+x_3^3-x_3*x_0^2-x_4^2*x_3);
V=whitneyStratify I;
v0={ ideal(x_3-x_4,x_2,x_1,x_0^2-2*x_4^2),ideal(x_4,x_3,x_2,x_0)};
assert(V#0==v0);

///
