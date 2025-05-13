///
restart
loadPackage( "AdjunctionForSurfaces",Reload=>true)

 
uninstallPackage("AdjunctionForSurfaces")

restart
installPackage("AdjunctionForSurfaces")
viewHelp AdjunctionForSurfaces

check ("AdjunctionForSurfaces")

peek loadedFiles
///

newPackage(
	"AdjunctionForSurfaces",
    	Version => "0.5", 
    	Date => "January 24, 2024",
    	Authors => { 
	         {Name => "Frank-Olaf Schreyer", 
		  Email => "schreyer@math.uni-sb.de", 
		  HomePage => "http://www.math.uni-sb.de/ag/schreyer/"}                 
	         },
    	Headline => "Adjunction for Surfaces",
        PackageImports => {"OldChainComplexes"},
	Keywords => {"Projective Algebraic Geometry"}
    	)

export {
    "linearSystemOnRationalSurface",
    "expectedDimension",
    "rationalSurface",
    "adjointMatrix",
    "slowAdjunctionCalculation",
    "adjunctionProcess",
    "parametrization",
    "specialFamiliesOfSommeseVandeVen"
    }
    
linearSystemOnRationalSurface=method()
linearSystemOnRationalSurface(Ring,ZZ,List) := (P2,d,L) -> (
    -- P2, the ring of P2
    -- d, a degree
    -- L, the multiplicities of the assigned base points
    pts:=apply(#L,i->ideal random(P2^1,P2^{2:-1}));
    assert(degree intersect pts == #L);
    I:=intersect apply(#L,i->(pts_i)^(L_i));
    Id:=gens intersect(I, ideal basis(d,P2));
    pos:=positions(toList(0..rank source Id-1),i->degree ideal Id_(0,i)==d);
    Id_pos)

linearSystemOnRationalSurface(List,ZZ,List) := (points,d,L) -> (
    -- P2, the ring of P2
    -- d, a degree
    -- L, the multiplicities of the assigned base points
    P2 := ring points_0;
    assert(degree intersect points == #L);
    I:=intersect apply(#L,i->(points_i)^(L_i));
    Id:=gens intersect(I, ideal basis(d,P2));
    pos:=positions(toList(0..rank source Id-1),i->degree ideal Id_(0,i)==d);
    Id_pos)



expectedDimension=method()
expectedDimension(ZZ,List) := (d,L) -> (
    -- d, a degree
    -- L, the multiplicities of the assigned base points
    max(0,binomial(d+2,2)-sum(L,r->binomial(r+1,2))))

rationalSurface=method()
rationalSurface(Ring,ZZ,List,Symbol) := (P2,d,L,x) -> (
    -- P2, the ring of P2
    -- d, a degree
    -- L, the multiplicities of the assigned base points
    -- x, a symbol for the new variable names
    n:= expectedDimension(d,L)-1;
    Pn := coefficientRing P2[x_0..x_n];
    H := linearSystemOnRationalSurface(P2,d,L);
    phi:= map(P2,Pn,H);
    trim  ker phi
    )

rationalSurface(Ring,ZZ,List,Ring) := (P2,d,L,Pn) -> (
    -- P2, the ring of P2
    -- d, a degree
    -- L, the multiplicities of the assigned base points
    -- Pn, coordinate ring of Pn
    H := linearSystemOnRationalSurface(P2,d,L);
    assert( rank source H == dim Pn);
    phi:= map(P2,Pn,H);
    trim  ker phi)

rationalSurface(List,ZZ,List,Symbol) := (points,d,L,x) -> (
    -- points, a List of points in P2, 
    -- d, a degree
    -- L, the multiplicities of the assigned base points
    -- x, a symbol for the new variable names
 -- n:= expectedDimension(d,L)-1;
    H := linearSystemOnRationalSurface(points,d,L);
    P2 := ring H;
    n := rank source H-1;
    Pn := (coefficientRing P2)[x_0..x_n];
    phi:= map(P2,Pn,H);
    trim  ker phi
    )



adjointMatrix=method()
adjointMatrix(Matrix,Symbol) := (D,z) -> (
    --  Input: D, matrix, transposed presentation matrix of omega_X
    --            where X is the surface defined by ann coker D
    --         z, symbol
    --  Output: adj, matrix, presentation matrix of O_X(H) 
    --               as a module over the adjoint surface, X', i.e.,
    --               the image of X under |K+H|             
    R := ring D;
    kk := coefficientRing R;
    r := rank source D; 
    P := kk[z_0..z_(r-1)];
    -- assert that D is a linear matrix
    tal1:=tally degrees source D;
    tal2:=tally degrees target D;
    degs1:=flatten keys tal1;
    degs2:=flatten keys tal2;
    assert(#degs1==1 and #degs2 ==1);
    assert(degs1_0==degs2_0+1);
    RxP := R**P;
    --tripel tensor flip
    adj := map(P^(numgens R),,sub(transpose diff( sub(vars R,RxP),sub(D,RxP)*sub(transpose vars P,RxP)),P));
    adj)

slowAdjunctionCalculation=method()
slowAdjunctionCalculation(Ideal,Matrix,Symbol):= (I,D,y) -> (
    n:=rank source D-1;
    P:= ring I;
    kk:= coefficientRing P;
    Pn:=kk[y_0..y_n];
    PxPn:= P**Pn;
    graph:= ideal( (sub(D,PxPn)*sub(transpose vars Pn,PxPn)))+sub(I,PxPn);
    gs:= gens saturate(graph,ideal PxPn_0);
    tally degrees source gs;
    -- we assume that I is non-degenerate, more precisely, V(I) is not contained in V(P_0);
    pos0:= positions(degrees source gs,de-> de_0==0);
    I1:=ideal sub(gs_pos0,Pn);
    pos1:= positions( degrees source gs,de-> de_0==1);
    adj:=sub(diff(transpose sub(vars P,PxPn),(gs_pos1)),Pn);
    adj=map(Pn^(rank target adj),,adj);
    (I1,adj)
    )


    
   
    
    



adjunctionProcess=method()
adjunctionProcess(Ideal):= J -> (
    --  Input: I, ideal of a surface
    --  Output:
    --       numList, list of numerical data ()
    --       adjList, list of adjunction matrices, 
    --                (useful for the computation of a rational parametrization)
    --       ptsList, list of the ideals of exceptional pts
    --       I, ideal of the last adjoint surface
    I:=J;
    numList:={}, rd:=null; adjList:={};ptsList:={};
    a:=symbol a; b:=symbol b;
    ab:={symbol a,symbol b};
    Pn:=ring J;n:= dim Pn-1; deg := degree I; sectGenus:=genus(I+ideal(Pn_0));
    betti(fI:=res(I,FastNonminimal=>true));
    c:=codim I;
	betti (G:=((dual fI[-c])**Pn^{-n-1}));
	betti(omega:=prune HH_0 G);
        betti(D:=transpose presentation omega);
	numList=append(numList,(n,deg,sectGenus));
    y:= null; adj:=null; pts:=null; l:=null;
    tal1:=null;tal2:=null,degs1:=null;degs2:=null;I1:=null;
    while ( rank source D >2 ) 
    	do ( 
	    y=first ab;ab=reverse ab;
	    tal1=tally degrees source D;
    	    tal2=tally degrees target D;
    	    degs1=flatten keys tal1;
    	    degs2=flatten keys tal2;
    	    if (#degs1==1 and #degs2 ==1 and degs1_0==degs2_0+1) then (
		betti(adj=adjointMatrix(D,y));
		I1=ann coker adj) else return (numList,adjList,ptsList,I);
	    while (
		rd=random((ring adj)^3,target adj);
		not dim (ideal (sub(rd,Pn)*transpose vars Pn)+I)==0) do ();
	    --pts=saturate ann coker(transpose (syz rd)*adj);
	    pts=sum apply(3,i->saturate ann coker(transpose syz(rd^{i})*adj));
	    ptsList=append(ptsList,pts);
	    l=if dim pts ==1 then degree pts else 0;
	    numList=append(numList,l);
	    adjList=append(adjList, adj);
	    I=I1;
	    c =codim I;
    	    Pn=ring I;
	    n=dim Pn-1;
	    deg=degree I; 
	    sectGenus=genus(I+ideal (Pn_0));
	    betti(fI=res(I,FastNonminimal=>true));
	    betti (G=((dual fI[-c])**Pn^{-n-1}));
	    betti(omega=prune HH_0 G);
            betti(D=transpose presentation omega);
	    numList=append(numList,(n,deg,sectGenus));
	    );
	(numList,adjList,ptsList,I))


adjunctionProcess(Ideal,ZZ):= (J,k) -> (
    --  Input: I, ideal of a surface
    --         k, number of adjunction steps 
    --  Output:
    --       numList, list of numerical data ()
    --       adjList, list of adjunction matrices, 
    --                (useful for the computation of a rational parametrization)
    --       ptsList, list of the ideals of exceptional points
    --       I, ideal of the last adjoint surface
    I:=J;
    numList:={}, rd:=null; adjList:={};ptsList:={};
    a:=symbol a; b:=symbol b;
    N:=0;
    ab:={symbol a,symbol b};
    Pn:=ring J;n:= dim Pn-1; deg := degree I; sectGenus:=genus(I+ideal(Pn_0));
    betti(fI:=res(I,FastNonminimal=>true));
    c:= codim I;
	betti (G:=((dual fI[-c])**Pn^{-n-1}));
	betti(omega:=prune HH_0 G);
        betti(D:=transpose presentation omega);
	numList=append(numList,(n,deg,sectGenus));
    y:= null; adj:=null; pts:=null; l:=null;
    tal1:=null;tal2:=null,degs1:=null;degs2:=null;I1:=null;
    while (
	rank source D >2 and N<k
	) 
    	do ( N=N+1;
	    y=first ab;ab=reverse ab;
	    tal1=tally degrees source D;
    	    tal2=tally degrees target D;
    	    degs1=flatten keys tal1;
    	    degs2=flatten keys tal2;
    	    if (#degs1==1 and #degs2 ==1 and degs1_0==degs2_0+1) then (
		betti(adj=adjointMatrix(D,y));
		I1=ann coker adj) else return (numList,adjList,ptsList,I);
	    while (
		rd=random((ring adj)^3,target adj);
		not dim (ideal (sub(rd,Pn)*transpose vars Pn)+I)==0) do ();
	    --pts=saturate ann coker(transpose (syz rd)*adj);
	    pts=sum apply(3,i->saturate ann coker(transpose syz(rd^{i})*adj));
	    ptsList=append(ptsList,pts);
	    l=if dim pts ==1 then degree pts else 0;
	    numList=append(numList,l);
	    adjList=append(adjList, adj);
    	    I=I1;
	    c =codim I;
    	    Pn=ring I;
	    n=dim Pn-1;
	    deg=degree I; 
  	    sectGenus=genus(I+ideal (Pn_0));
  	    betti(fI=res(I,FastNonminimal=>true));
	    betti (G=((dual fI[-c])**Pn^{-n-1}));
	    betti(omega=prune HH_0 G);
            betti(D=transpose presentation omega);
	    numList=append(numList,(n,deg,sectGenus));
	    );
	(numList,adjList,ptsList,I))




parametrization=method()
parametrization(Ring,List) := (P2,adjList) -> (
    n:=#adjList;
    H:=vars P2;
    Ht:= null;
    while n>0 do (
	betti(Ht= syz transpose map(P2^(rank target adjList_(n-1)), , sub(adjList_(n-1),H)));
	H=map(P2^1,,transpose (Ht_{0}));
	n=n-1);
   H)

specialFamiliesOfSommeseVandeVen=method()
specialFamiliesOfSommeseVandeVen(Ring,ZZ) := (kk,fam) -> (
    assert(member(fam,{1,2,3,4}));
    t := symbol t;
    P2 := kk[t_0..t_2];Y:=null;
    x := symbol x;
    if fam==1 then Y=rationalSurface(P2,6,toList(7:2),x);
    if fam==2 then Y=rationalSurface(P2,6,toList(7:2)|{1},x);
    if fam==3 then Y=rationalSurface(P2,9,toList(8:3),x);
    if fam==4 then (
	h:=matrix{{t_0^2,t_1^2,t_2^2,t_0*t_1-t_0*t_2,t_1*t_2-t_0*t_2}};
	y := symbol y;
	P4:=kk[y_0..y_4];
	veronese:=ker map(P2,P4,h);
	ci := ideal (gens veronese*random(source gens veronese,P4^{2:-3}));
	X:=ci:veronese;plane:=null;fivePts:=null;pts:=null;pt:=null;
	tangPlane:=null; ctp:=null;
	Lines:=apply(2,i->(
		while( plane=ideal random(P4^1,P4^{2:-1});
	    	    fivePts=decompose(plane+X);
 	    	    pts=select(fivePts,c->(dim c, degree c)==(1,1));
	    	    #pts==0 ) do ();
	pt=transpose syz transpose jacobian first pts;
	tangPlane=vars P4*syz transpose syz transpose  sub(jacobian X,pt);
	ctp=decompose (ideal(tangPlane*random(kk^2,kk^1))+X);
	line:=first ctp
	));
        H:=(intersect Lines)_0;
	B:=last decompose(ideal H+X);
	betti(threeB:=saturate(B^3+X));
	H3:=threeB_0;
	betti(residual:=(ideal H3+X):threeB);
	betti(h=gens trim ideal (gens residual % X));
	P5:=kk[x_0..x_5];
	PX:=P4/X;
	phi:=map(PX,P5,h);
	betti(Y=trim ker phi));
    return Y)


beginDocumentation()

document { 
  Key => "AdjunctionForSurfaces",
  Headline => "Rational surfaces and extensions",
   
   PARA{},"The main functions concerning surfaces are:",
   UL{   TO  "linearSystemOnRationalSurface",
         TO  "rationalSurface",
	 TO  "expectedDimension",
	 TO  "adjointMatrix",
	 TO  "slowAdjunctionCalculation",
	 TO  "adjunctionProcess",
	 TO  "parametrization",
	 TO  "specialFamiliesOfSommeseVandeVen"  
      },
 

   PARA{}, "This realizes the djunction process of Sommese and Van de Ven for smooth projective surfaces.
   Given a smooth projectiv surface X in Pn, Sommese and Van de Ven study X via the morphism
   
   phi=phi_{|H+K|}: X -> X1 subset Pn1
       
   where H denotes the hyperplane class and K the canonical divisor on X.",
   
   PARA{}, " Theorem[SVdV]. Let X subset Pn be a smooth projective curve. Then phi is a birational map
   onto a smooth projective surface X1, which blows down all (-1) lines E of X, i.e., (-1) curves of X
   which are embedded into Pn as lines, and is regular otherwise, unless
   
    	(1) X is a P2 linearly or quadratically embedded, or ruled in lines,
	
	(2) X is a anticanonical embedded del Pezzo surface,
	
	(3) X is a conic bundle in which case phi_{|H+K|}: X -> B
	maps X to a curve B and the fibers are conics, or
	
	(4) X is an element of one of the four families of exceptions where |K+H|
	defines a finite to 1 map. See ", TO "specialFamiliesOfSommeseVandeVen",
   
   PARA{}, "Since a (-1) conic on X becomes (-1) line on X1, repeating this process finally reaches 
	a minimal surface unless X has negative Kodaira dimension.",
	
    PARA{}, "In case of rational surfaces, repeating the process the process we end in one of the 
    exceptional cases (1),..,(4). Whether we are in case (3) or (4)  can be detect from the presentation
    matrix of O_X(H+K). If this presentation matrix is not linear, then a smooth hyperplane section 
    C in |H| will be a hyperelliptic curves by Green's K_{p,1} theorem and phi is no longer birational, because 
    it induces the canonical system on C which is 2:1. 
    In our function adjunctionProcess we stop with this last surface.",
   
   PARA{},"References:",
   PARA{},"[DES] W. Decker, L. Ein, F.-O. Schreyer. Construction of surfaces in P4. J. Alg. Geom. 2 (1993), 185-237.",
   PARA{},"[SVdV] A. Sommese, A. Van de Ven. On the adjunction mapping, Math. Ann. 278 (1987), 593–603.",
   PARA{},"[G] M.Green. Koszul cohomology and the geometry of projective varieties. J. Differ. Geom. 19, 125-167, 168-171 (1984)."
   
   }



doc ///
  Key
    linearSystemOnRationalSurface
    (linearSystemOnRationalSurface,Ring,ZZ,List)
    (linearSystemOnRationalSurface,List,ZZ,List)
  Headline
    compute a linear system on a rational surface
  Usage
     H=linearSystemOnRationalSurface(P2,d,L)
     H=linearSystemOnRationalSurface(points,d,L)
  Inputs 
    P2: Ring
          homogeneous coordinate ring of P2
    points: List
         a list of ideals defining points in P2
    d: ZZ
          degree of the desired forms
    L: List
          {r_1,...,r_s} of multiplicities 
  Outputs
     H: Matrix
        a 1xn matrix whose entries are a bases of L(d;r_1p_1,...,r_sp_s)      
  Description
     Text
       The function chooses randomly s point p_i in P2 and computes the 
       linear system of form of degree d which have multiplicity r_i at the point p_i       
     Example
        kk=ZZ/nextPrime(10^3)
	t=symbol t
	P2=kk[t_0..t_2]
	d=8
	L=toList(3:3)|toList(2:4)|{1}
        expectedDimension(d,L)	 
        betti(H=linearSystemOnRationalSurface(P2,d,L))	
///
 

doc ///
  Key
    expectedDimension
    (expectedDimension,ZZ,List)
  Headline
    compute the expected dimension of a linear system on a rational surface
  Usage
     expectedDimension(d,L)
  Inputs 
    d: ZZ
          degree of the desired forms
    L: List
          {r_1,...,r_s} of multiplicities 
  Outputs
     : ZZ
       the expected dimension of L(d;r_1p_1,...,r_sp_s)     
  Description
     Text
       computes the expected dimension
       max(0,binomial(d+2,2)-sum binomial(r_i+1,2))
       of a linear system with assigned base points on P2
     Example
	d=8
	L=toList(3:3)|toList(2:4)|{1}
        expectedDimension(d,L)
	kk=ZZ/nextPrime(10^3)
	t=symbol t
	P2=kk[t_0..t_2]	 
        betti(H=linearSystemOnRationalSurface(P2,d,L))	
///	 
      

doc ///
  Key
    rationalSurface
    (rationalSurface,Ring,ZZ,List,Ring)
    (rationalSurface,Ring,ZZ,List,Symbol)
    (rationalSurface,List,ZZ,List,Symbol)
  Headline
    compute the ideal I of the rational surface      
  Usage
    I=rationalSurface(P2,d,L,Pn)
    I=rationalSurface(P2,d,L,x)
    I=rationalSurface(points,d,L,x)
  Inputs 
    P2: Ring
         homogeneous coordinate ring of P2,
    points: List
        list of ideals of points in P2
    d: ZZ
         degree of the desired forms
    L: List
          {r_1,...,r_s} of multiplicities
    Pn: Ring
       coordinate ring of Pn
    x: Symbol
        variable name 
  Outputs
     I: Ideal
       of a rational surface 
  Description
     Text
       The function chooses randomly s point p_i in P2 and computes the 
       linear system H=L(d;L) of form of degree d which have multiplicity r_i at 
       the point p_i. 
       
       Then the ideal of the image of P2 of under the rational map
       defined by the linear system H is computed.    
     Example
	d=6
	L=toList(6:2)|toList(2:1)
        n=expectedDimension(d,L)-1
	kk=ZZ/nextPrime(10^3)
	t=symbol t
	P2=kk[t_0..t_2]	 
        y=symbol y
	betti(I=rationalSurface(P2,d,L,y))
	x = symbol x
	Pn=kk[x_0..x_n]
	betti(I=rationalSurface(P2,d,L,Pn))
	degree I, genus I, dim I
	minimalBetti I
	d^2-sum(L,r->r^2)== degree I
	(numList,adjList,ptsList,J)=adjunctionProcess(I);
	numList
	minimalBetti J
///	 

doc ///
  Key
    adjointMatrix
    (adjointMatrix,Matrix,Symbol)
  Headline
    compute the adjoint matrix
  Usage
     adj=adjointMatrix(D,z)
  Inputs 
    D: Matrix
           the transpose of a linear presentation matrix of omega_X(1)
  Outputs
    adj: Matrix
       the presentation matrix of O_X(1)     
  Description
     Text
        compute the presentation matrix of the line bundle O_X(1)
	as a module on the adjoint variety
     Example
	d=7
	L=toList(7:2)|toList(8:1)
        n=expectedDimension(d,L)-1
	kk=ZZ/nextPrime(10^3)
	t=symbol t, x= symbol x
	P2=kk[t_0..t_2]
	Pn=kk[x_0..x_n]	 
        betti(I=rationalSurface(P2,d,L,Pn))
	c=codim I
	elapsedTime fI=res I
        betti(omega=presentation coker transpose fI.dd_c**Pn^{-n-1})
	D=transpose omega;
	z= symbol z
	betti(adj=adjointMatrix(D,z))
	support adj
    	minimalBetti ann coker adj
	(numList,adjList,ptsList,J)=adjunctionProcess(I,1);
	numList
	betti(adjList_0)
	minimalBetti J		
///	 

doc ///
  Key
    slowAdjunctionCalculation
    (slowAdjunctionCalculation,Ideal,Matrix,Symbol)
  Headline
    compute the adjoint variety and the presentation of O(1)
  Usage
     (I1,adj)=slowAdjunctionCalculation(I,D,z)
  Inputs
    I: Ideal
    	of a smooth projective surface X
    D: Matrix
           the transpose of a linear presentation matrix of omega_X(1)
    z: Symbol
    	variable name
    
  Outputs
    I1 : Ideal
    	of the adjoint surface in a Pn
    adj: Matrix
       the presentation matrix of O_X(1) as O_Pn-module    
  Description
     Text
        compute the ideal of the adjoint variety and the presentation matrix of the line bundle O_X(1)
	as a module on the adjoint variety
     Example
        kk=ZZ/101
	P2=kk[x_0..x_2]
	betti(Y=rationalSurface(P2,8,toList(4:3)|toList(4:2)|{1,1},symbol z))
	P6=ring Y, dim P6==7
	betti(fY=res Y)
	betti(omegaY=coker transpose fY.dd_4**P6^{-dim P6})
	betti(D=transpose presentation omegaY)
	(I,adj)=slowAdjunctionCalculation(Y,D,symbol x);
	betti adj
	P4=ring I, dim P4==5
	PI=P4/I
	m=adj**PI;
	betti(sm=syz transpose m)
	rank sm==1
	(numList,adjList,ptsList,I)=adjunctionProcess Y;
	numList, minimalBetti I
///


doc ///
  Key
    adjunctionProcess
    (adjunctionProcess,Ideal)
    (adjunctionProcess,Ideal,ZZ)
  Headline
    perform the adjunction process
  Usage
     (numList,adjList,ptsList,J)=adjointMatrix(I)
     (numList,adjList,ptsList,J)=adjointMatrix(I,N)
  Inputs 
    I: Ideal
           of a projective surface
    N: ZZ
        number of adjunction steps
  Outputs
    numList: List
       numerical Data of the adjunction process
    adjList: List
       list of adjoint matrices
    ptsList: List
       list of ideal if the exceptional points
    J: Ideal
        ideal of the final surface in the adjunction process       
  Description
     Text
        Adjunction determines the image X' of X under the morphism phi: X -> X'
	defined by |H+K|. By Sommese and Van de Ven [SVdV]
	|H+K| is birational and blows down presisely all (-1) lines of unless
	
	(1) X is a P2 linearly or quadratically embedded, or ruled in lines,
	
	(2) X is a anticanonical embedded Del Pezzo surface,
	
	(3) X is a conic bundle in which case phi_{|H+K|}: X -> B
	maps X to a curve B and the fibers are conics, or
	
	(4) X is an element of one of the four families of exceptions where |K+H|
	defines a finite to 1 map. See TO "specialFamiliesOfSommeseVandeVen".
	
	Since a (-1) conic on X become (-1) line on X', repeating this process finally reaches 
	a minimal surface unless X has negative Kodaira dimension.
	
	In case X is a rational surface, and the lucky case that the
	final surface is P2, one can use the list adjList to get a rational 
	parametrization.
	
	In case one ends up with a del Pezzo surface or a conic bundle, one has to identify the 
	exceptional lines, thus one might need an algebraic field extension, 
	to get a rational parametrization. However, one can always parametrize X in terms of the final
	surface in the adjunction process.
	
	The numerical data are collected in a list with an entry 
	
	(n,d,pi)= (dim Pn_i, degree X_i, sectional genus of X_i)
	
	for each surface X_i in the adjunction process, starting with X=X_0
	and an entry the integer k if X_i -> X_(i+1) blows down k (-1) lines.	
     Example
	d=7
	L=toList(8:2)|toList(5:1)
        n=expectedDimension(d,L)-1
	kk=ZZ/nextPrime(10^3)
	t=symbol t, x= symbol x
	P2=kk[t_0..t_2]
	Pn=kk[x_0..x_n]	 
        betti(I=rationalSurface(P2,d,L,Pn))
	minimalBetti I	
	(numList,adjList,ptsList,J)=adjunctionProcess(I);
	numList
	P2=ring J
	betti(H=parametrization(P2,adjList))
	phi=map(P2,Pn,H);
	elapsedTime betti(I'=trim ker phi)
	I'== I
        elapsedTime basePts=primaryDecomposition ideal H;
	tally apply(basePts,c->(dim c, degree c, betti c))  		
///	 


doc ///
  Key
    parametrization
    (parametrization,Ring,List)
  Headline
    compute a rational parametrization
  Usage
     H=parametrization(PJ,adjList)
  Inputs
    PJ: Ring
           coordinate ring of the last adjoint surface
    adjList: List
        list of adjoint matrices
  Outputs
    H: Matrix
       parametrization of the rational surface X      
  Description
     Text
        Let adjList be the list of adjoint matrices coming out of the adjunction
	process of a rational surface X. If the final surface is a P2 then
	the function computes the rational parametrization of X. In other cases the 
	function returns rational parametrization from the final surface X'' in the adjunction
	process.
		
     Example
	d=4
	L=toList(7:1)
        n=expectedDimension(d,L)-1
	kk=ZZ/nextPrime(10^3)
	t=symbol t, x= symbol x
	P2=kk[t_0..t_2]
	Pn=kk[x_0..x_n]	 
        betti(I=rationalSurface(P2,d,L,Pn))
	minimalBetti I	
	(numList,adjList,ptsList,J)=adjunctionProcess(I);
	numList
	P2=ring J
	betti(H=parametrization(P2,adjList))
    	elapsedTime sub(I,H)
	phi=map(P2,Pn,H);
	elapsedTime betti(I'=trim ker phi)
	I'== I
        elapsedTime basePts=primaryDecomposition ideal H;
	tally apply(basePts,c->(dim c, degree c, betti c))		
///


doc ///
  Key
    specialFamiliesOfSommeseVandeVen   
    (specialFamiliesOfSommeseVandeVen,Ring,ZZ)
  Headline
    produce a member of the special family
  Usage
     Y=specialFamiliesOfSommeseVandeVen(kk,fam)
  Inputs
    kk: Ring
          the ground field
    fam: ZZ
        number of the family
  Outputs
    Y: Ideal
       the ideal of a surface   
  Description
     Text
        By [SVdV] four families of smooth surfaces Y behave differently
	in the adjunction process: |H+K_Y| defines a morphism Y -> Y_1
	which is generically finite to 1 instead of birational.
	These families are
	
	1) P2(6;2p_1,...,2p_7)
    
    	2) P2(6;2p_1,...,2p_7,p_8)
    
    	3) P2(9;3p_1,...,3p_8)
    
    	4) Y=P(E) where E is a indecomposabe rank 2 vector bundle over an elliptic curve
    	    and H=3B, where B in Y is the section with B^2=1.

     Example   
        kk=ZZ/nextPrime(10^3)	
	Y=specialFamiliesOfSommeseVandeVen(kk,1);
	betti(fY= res Y)
	betti (fib=trim(ideal(fY.dd_4*random(kk^3,kk^1))))
	dim fib, degree fib
	ll=adjunctionProcess Y;
	ll_0,ll_1,ll_2, minimalBetti ll_3
     Text 
        The adjunction map |H+K_Y|: Y -> P2
	is 2:1 in case of family 1.
     Example   	
	Y=specialFamiliesOfSommeseVandeVen(kk,2);
	betti(fY= res Y)
	betti (fib=trim(ideal(fY.dd_3*random(kk^3,kk^1))))
	dim fib, degree fib
     Text 
        The adjunction map |H+K_Y|: Y -> P2
	is 2:1 in case of family 2.
     Example
        Y=specialFamiliesOfSommeseVandeVen(kk,3);
     	betti(fY=res Y)
     	P6=ring Y,dim P6==7
	(Q,adj)=slowAdjunctionCalculation(Y,fY.dd_4,symbol u);
    	dim Q, degree Q
     	P3=ring Q; dim P3==4
     	while (L=ideal random(P3^1,P3^{2:-1});
    	    pts=decompose (L+Q);
    	    #pts<2) do ()
     	pt=sub(syz transpose jacobian first pts,kk)
     	betti(fib= trim ideal(fY.dd_4*pt))
     	dim fib, degree fib
     Text 
        The adjunction map |H+K_Y|: Y -> Q 
	is 2:1 onto a quadric in P3 in case of family 3.
     Example
        Y=specialFamiliesOfSommeseVandeVen(kk,4);
	betti(fY=res Y)
	P5=ring Y, dim P5==6
	betti(omegaY=prune Ext^2(module Y,P5^{-6}))
	betti(fib=trim ideal (random(kk^1,kk^3)*presentation omegaY))
	dim fib, degree fib
     Text 
        The adjunction map |H+K_Y|: Y -> P2 
	is 3:1 in case of family 4.         
///	

end
