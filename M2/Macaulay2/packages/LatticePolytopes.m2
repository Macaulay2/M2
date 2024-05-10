--*- coding: utf-8 -*-
-*
   Written by Anders Lundman & Gustav Sædén Ståhl , 2015.

   This file is in the public domain.
   *-
---------------------------------------------------------------------------
-- PURPOSE: A package for computations with lattice polytopes 
-- PROGRAMMERS : Anders Lundman and Gustav Sædén Ståhl 
-- UPDATE HISTORY : March 2014, May 2015
---------------------------------------------------------------------------
newPackage("LatticePolytopes",
    Headline => "lattice polytopes",
    Version => "1.0",
    Date => "May 4, 2015",
    Authors => {
	{Name => "Anders Lundman",
	    HomePage => "http://www.math.kth.se/~alundman",
	    Email => "alundman@math.kth.se"},
	{Name => "Gustav Sædén Ståhl",
	    HomePage => "http://www.math.kth.se/~gss",
	    Email => "gss@math.kth.se"}
      	},
    Keywords => {"Convex Geometry"},
    PackageExports => {"Polyhedra","NormalToricVarieties"}
    )

export{
    "cayley",
    "isCayley",
    "areIsomorphic",
    "toricBlowUp",
    "listSmooth3D",
    "listSmooth2D",
    "toricDiv",
    "codegree",
    "adjointPolytope",
    "torusEmbedding",
    "jetMatrix",
    "isJetSpanned",
    "smoothTest",
    "gaussImage",
    "gaussFiber",
    "ambientHalfspaces",
    "gausskImage",
    "gausskFiber",
    "randQPoly",
    "randZPoly",    
    "degreeOfJetSeparation",
    "epsilonBounds",
    "iskCayleykEdges"
       }

-- PURPOSE : Create the Cayley sum of polytopes
cayley = method(TypicalValue => Polyhedron)

--  INPUT : Two Polyhedra P1,P2
-- OUTPUT : Polyhedron - The Cayley sum of P1 and P2 with height 1
cayley(Polyhedron,Polyhedron):=(P1,P2)->(
    cayley({P1,P2},1));

cayley(Matrix,Matrix):=(M1,M2)->(
    cayley({M1,M2},1));

--  INPUT : Two Polyhedra P1,P2 and an integer k
-- OUTPUT : Polyhedron - The Cayley sum of P1 and P2 with height k
cayley(Polyhedron,Polyhedron,ZZ):=(P1,P2,k)->(
    cayley({P1,P2},k));

cayley(Matrix,Matrix,ZZ):=(M1,M2,k)->(
    cayley({M1,M2},k));
    
--  INPUT : Three Polyhedra P1,P2,P3
-- OUTPUT : Polyhedron - The Cayley sum of P1, P2 and P3 with height 1
cayley(Polyhedron,Polyhedron,Polyhedron):=(P1,P2,P3)->(
    cayley({P1,P2,P3},1));

cayley(Matrix,Matrix,Matrix):=(M1,M2,M3)->(
    cayley({M1,M2,M3},1));

--  INPUT : Three Polyhedra P1,P2,P3 and an integer k
-- OUTPUT : Polyhedron - The Cayley sum of P1, P2 and P3 with height k
cayley(Polyhedron,Polyhedron,Polyhedron,ZZ):=(P1,P2,P3,k)->(
    cayley({P1,P2,P3},k));
   
cayley(Matrix,Matrix,Matrix,ZZ):=(M1,M2,M3,k)->(
    cayley({M1,M2,M3},k));   
   
--  INPUT : A list of Polyhedras or Matrices
-- OUTPUT : Polyhedron - The Cayley sum with height 1 of the elements in the list
cayley(List):=(Plist)->(
    cayley(Plist,1));

--  INPUT : A list of Polyhedras or Matrices and an integer k
-- OUTPUT : Polyhedron - The Cayley sum with height k of the elements in the list
cayley(List,ZZ):=(Plist,k)->(
    if all(Plist,p -> (class p === Matrix)) then Plist=apply(Plist,convexHull);
    if not all(Plist,p -> (class p === Polyhedron)) then (
	print "Must be of type Polyhedron";
	return);
    if (n:=#Plist)<2 then (
	print "Must be more than one polytope!";
	return);
    ---Single out the case when all polytopes are points
    if all(Plist,p->dim p==0) then (
    	return convexHull(matrix(mutableMatrix(ZZ,n-1,1))|(k*id_(ZZ^(n-1)))));
    ------Assert that the ambient dimension of the polytopes is the same.
    m:=max for P in Plist list ambDim P;
    Plist=apply(Plist,P->(
	    if (j:=ambDim P)<m then (
		convexHull(vertices P||matrix(for i from m-j+1 to m list(toList((nVertices P):0)))))else(P)));
    ---------
    N:=(vertices Plist_0)||matrix(mutableMatrix(ZZ,n-1,numcols (vertices Plist_0)));
    N=N|((vertices Plist_1)||matrix{toList((numcols (vertices Plist_1)):k)}||matrix(mutableMatrix(ZZ,n-2,numcols (vertices Plist_1))));
    for j from 2 to n-1 do (
	N=N|((vertices Plist_j)||matrix(mutableMatrix(ZZ,j-1,numcols (vertices Plist_j)))||matrix{toList((numcols (vertices Plist_j)):k)}||matrix(mutableMatrix(ZZ,(n-1-j),numcols (vertices Plist_j)))););
    return convexHull(N));


-- PURPOSE : Test if a polytope is a Cayley polytope
isCayley = method(TypicalValue => Boolean)

--  INPUT : A matrix 
-- OUTPUT : true or false
isCayley(Matrix) := M -> (
    isCayley(convexHull(M)));

--  INPUT : A Polyhedron P
-- OUTPUT : true or false
isCayley(Polyhedron) := P -> (
    (A,b):=ambientHalfspaces(P);
    for i to (numRows(A)-1) do (
	if (length(unique (flatten entries (A^{i}*vertices(P))))==2) then (
	    return true));
    return false);



-- PURPOSE : Test if two smooth polytopes are isomorphic
areIsomorphic = method(TypicalValue => Boolean, Options => {smoothTest => true})

--  INPUT : Two matrices M,N giving the vertices of two polyhedra
-- OUTPUT : true or false
areIsomorphic(Matrix,Matrix) := opts -> (M,N) -> (
    areIsomorphic(convexHull(M),convexHull(N), smoothTest => opts.smoothTest))

--  INPUT : Two Polyhedra P,Q
-- OUTPUT : true or false
areIsomorphic(Polyhedron,Polyhedron) := opts -> (P,Q) -> (
    if opts.smoothTest then (if not (isSmooth(normalFan P) and isSmooth(normalFan Q)) then (
	    print "Polytopes must be smooth!";
	    return));
    if ((numRows ((facets P)#0))==(numRows ((facets Q)#0))) and ((numColumns rays P)==(numColumns rays Q)) and ((nVertices P)==(nVertices Q)) and ((dim P)==(dim Q)) and (#latticePoints(P)==#latticePoints(Q)) then(
	origo:=(facesAsPolyhedra((dim P),P))_0;
	edges:={};
	--Create edges through origo
	for edge in facesAsPolyhedra(((dim P))-1,P) do(
	    if contains(edge,origo) then(
		e:=matrix{(vertices edge)_0-( vertices origo)_0+(vertices edge)_1-(vertices origo)_0}*(1/(#latticePoints(edge)-1));
	    	edges=append(edges,e)));
	Ainv:=matrix{edges};
	A:=inverse(Ainv);
	Ptransformed:=affineImage(A,(affineImage(P,(vertices origo)*(-1))));
	for v in facesAsPolyhedra((dim Q),Q) do (
	    edges={};
	    --Create edges through v
	    for edge in facesAsPolyhedra(((dim Q))-1,Q) do (
	    	if contains(edge,v) then (
		    e:=matrix{(vertices edge)_0-(vertices v)_0+(vertices edge)_1-(vertices v)_0}*(1/(#latticePoints(edge)-1));
	    	    edges=append(edges,e)));
	    for perm in permutations(edges) do(
	    	Ainv=matrix{perm};
	    	A=inverse(Ainv);
	    	Qtransformed:=affineImage(A,(affineImage(Q,(vertices v)*(-1))));
	    	-- print(vertices(Qtransformed));
	    	-- print " ";
	    	if (vertices Qtransformed==vertices Ptransformed) then return true)));
    false)	  

-- PURPOSE : Calculate the stellar subdivision of a polytope along a face with a given height
toricBlowUp = method(TypicalValue => Polyhedron)

toricBlowUp(Polyhedron,Polyhedron) := (P,F) -> (
    toricBlowUp(P,F,1));

toricBlowUp(Matrix,Matrix) := (M,N) -> (
    toricBlowUp(convexHull(M),convexHull(N),1));

toricBlowUp(Matrix,Matrix,ZZ) := (M,N,k) -> (
    toricBlowUp(convexHull(M),convexHull(N),k));

--  INPUT : A Polyhedron P, a Polyhedron F (which is a face of P) and a height k
-- OUTPUT : A Polyhedron
toricBlowUp(Polyhedron,Polyhedron,ZZ) := (P,F,k) -> (
    if not contains (P,F) then (
	print "The second Polyhedron is not a face of the first!";
	return P
        );
    (A,b) := ambientHalfspaces(P);
    E := matrix{toList((numcols A):0)};
    for i to (numrows(A)-1) do (
	B := matrix{
            toList( (numcols(vertices(F))) : (b^{i})_0_0)
            };
	if (sub(A^{i}*(vertices F),ZZ)==B) then (
	    E=E+sub(A^{i},QQ)*(1/gcd(flatten entries A^{i})));
        );
    Edges := facesAsPolyhedra(((dim P)-1),P);
    i := position(Edges,e -> (
            not contains(F,e) 
            and contains(e,(facesAsPolyhedra((dim F),F))_0)
        ));
    pt := k * matrix{(vertices Edges_i)_0+(vertices Edges_i)_1-2*(vertices F)_0}*(1/(#latticePoints(Edges_i)-1))+matrix((vertices F)_0);
    return intersection(A||E,b||(E*pt)));



-- PURPOSE : Create the toric Weil divisor associated to a polytope
toricDiv = method(TypicalValue => ToricDivisor)

toricDiv(Matrix) := (M) -> (
    toricDiv(convexHull(M)));

--  INPUT : Polyhedron P
-- OUTPUT : A ToricDivisor
toricDiv(Polyhedron) := (P) -> (
    if isEmpty(P) then( print("Polytope needs to be non-empty.");return);
    r:=rays normalFan(P);
    (A,b):=ambientHalfspaces(P);
    X:=normalToricVariety(P);
    Zeros:=toList(rank(weilDivisorGroup(X)):0);
    D:=toricDivisor(Zeros,X);
    for i to numColumns r - 1 do (
    	for j to numrows(A)-1 do (
	    if (-A^{j}==transpose r_{i}) then (
    	    	D=D+(b^{j})_0_0*X_i)));
    return D);



-- PURPOSE : Calculate the codegree of a polytope
codegree = method(TypicalValue => ZZ)

codegree(Matrix) := M -> (
    codegree(convexHull(M)));

codegree(Polyhedron) := P -> (
    for k from 1 to ((dim P)+1) do (
    	if not (interiorLatticePoints(k*P)=={}) then return k));


-- PURPOSE : Calculate the adjoint polytope 
adjointPolytope = method(TypicalValue => Polyhedron)

adjointPolytope(Matrix,ZZ) := (M,k) -> (
    adjointPolytope(convexHull(M),k));

adjointPolytope(Polyhedron,ZZ) := (P,k) -> (
    (A,b):=ambientHalfspaces(P);	
    return intersection(A,b-transpose(matrix{toList(numrows(b):k)})));



-- PURPOSE : Calculate the torus embedding 
torusEmbedding = method(TypicalValue => List)

torusEmbedding(Matrix) := (M) -> (
    L:=for i to numcols(M)-1 list matrix{M_i};
    torusEmbedding(L));

torusEmbedding(List) := (A) -> (
    if (not all(A,c -> (class c === Matrix))) or #A==0 then (
	print "Must be a list of column matrices";
	return);
    n:=numrows A_0;
    if not all(A,c -> numrows c == n) then (
	print "The column matrices must have the same size";
	return);
    x:=symbol x;
    R:=ZZ[x_0..x_(numrows A_0-1),MonomialOrder=>Lex,Inverses=>true];
   apply(A,c->product for i to -1+numgens R list R_i^((flatten entries c)_i)))


derivate := method()

derivate(RingElement,Matrix) := (v,M) -> (
    matrix(for m in entries M list (for n in m list derivate(v,n))))

derivate(RingElement,RingElement):= (v,m) -> (
    variabler:=(entries vars ring m)_0;
    e:=exponents m;
    ev:=exponents v;
    ny:=e-ev;
    koeff:=product(for i to #e_0-1 list ((ev_0_i)!*binomial(e_0_i,ev_0_i)));
    svar:=koeff*(product(for i to #e_0-1 list power(variabler_i,(ny_0_i))));
    svar)


-- PURPOSE : Calculate the matrix of k-jets
jetMatrix = method(TypicalValue => Matrix)

jetMatrix(Matrix,ZZ) := (M,k) -> (
     L:=for i to numcols(M)-1 list matrix{M_i};
     jetMatrix(L,k));

jetMatrix(List,ZZ) := (A,k) -> (
    x:=symbol x;
    S:=ZZ[x_0..x_(numrows A_0 -1)];
    expo:=flatten entries basis(1,k,S);
    J:=(Emb:=matrix{torusEmbedding(A)});
    R:=ZZ[x_0..x_(numrows A_0-1),MonomialOrder=>Lex,Inverses=>true];
    for e in expo do (
       	J=J||derivate(e,Emb);
       	);
   return J);

jetMatrix(Matrix,ZZ,Matrix) := (M,k,p) -> (
     L:=for i to numcols M-1 list matrix{M_i};
     jetMatrix(L,k,p));

jetMatrix(List,ZZ,Matrix) := (A,k,p) -> (
    if (not all(A,c -> (class c === Matrix))) or #A==0 then (
	print "Must be a list of column matrices";
	return);
    n:=numrows A_0;
    if not all(A,c -> numrows c == n) then (
	print "The column matrices must have the same size";
	return);
    if not numrows p == n then (
	print "This is not a point in the right space";
	return);
    x:=symbol x;
    R:=ZZ[x_0..x_(numrows A_0-1)];
    expo:=flatten entries basis(1,k,R);
    J:=(Emb:=matrix{torusEmbedding(A)});
    for e in expo do (
       	J=J||derivate(e,Emb));
   return sub(J,transpose p));


-- PURPOSE : Test if a torus embedding is jet spanned at a point
isJetSpanned = method(TypicalValue => Boolean) 

isJetSpanned(Matrix,ZZ,Matrix) :=(M,k,p) -> (
    L:=for i to numcols M-1 list matrix{M_i};
    isJetSpanned(L,k,p));

isJetSpanned(List,ZZ,Matrix) :=(A,k,p) -> (
    if (not all(A,c -> (class c === Matrix))) or #A==0 then (
	print "Must be a list of column matrices";
	return);
    n:=numrows A_0;
    if not all(A,c -> numrows c == n) then (
	print "The column matrices must have the same size";
	return);
    if not numrows p == n then (
	print "This is not a point in the right space";
	return);
    J:=jetMatrix(A,k,p);
    rank(J)==numrows(J))

 -- PURPOSE : Find the degree of jet separation at a point
degreeOfJetSeparation = method(TypicalValue => ZZ) 

degreeOfJetSeparation(Matrix,Matrix) :=(M,p) -> (
    L:=for i to numcols M-1 list matrix{M_i};
   degreeOfJetSeparation(L,p));

degreeOfJetSeparation(List,Matrix) :=(A,p) -> (
    if (not all(A,c -> (class c === Matrix))) or #A==0 then (
	print "Must be a list of column matrices";
	return);
    n:=numrows A_0;
    if not all(A,c -> numrows c == n) then (
	print "The column matrices must have the same size";
	return);
    if not numrows p == n then (
	print "This is not a point in the right space";
	return);
    boolean:=true;
    k:=1;
    while boolean do(
	if (not isJetSpanned(A,k,p)) then(
	    boolean=false;
	    s:=k-1;
	    );
	k=k+1;
	);
    return s
    )
         


-- PURPOSE : Returns the list of all smooth 2-polytopes with up to
--           12 lattice points
Sm12in2D:={matrix {{0, 0, 1}, {0, 1, 1}}, matrix {{0, 1, 0, 1}, {0, 0, 1, 1}}, matrix{{0, 1, 0, 2}, {0, 0, 1, 1}}, matrix {{0, 1, 0, 3}, {0, 0, 1, 1}}, matrix {{0,1, 0, 4}, {0, 0, 1, 1}}, matrix {{0, 1, 0, 5}, {0, 0, 1, 1}}, matrix {{0, 1, 0,6}, {0, 0, 1, 1}}, matrix {{0, 1, 0, 7}, {0, 0, 1, 1}}, matrix {{0, 1, 0, 8},{0, 0, 1, 1}}, matrix {{0, 1, 0, 9}, {0, 0, 1, 1}}, matrix {{0, 2, 0, 2}, {0,0, 1, 1}}, matrix {{0, 2, 0, 3}, {0, 0, 1, 1}}, matrix {{0, 2, 0, 4}, {0, 0, 1,1}}, matrix {{0, 2, 0, 5}, {0, 0, 1, 1}}, matrix {{0, 2, 0, 6}, {0, 0, 1, 1}},matrix {{0, 2, 0, 7}, {0, 0, 1, 1}}, matrix {{0, 2, 0, 8}, {0, 0, 1, 1}},matrix {{0, 3, 0, 3}, {0, 0, 1, 1}}, matrix {{0, 3, 0, 4}, {0, 0, 1, 1}},matrix {{0, 3, 0, 5}, {0, 0, 1, 1}}, matrix {{0, 3, 0, 6}, {0, 0, 1, 1}},matrix {{0, 3, 0, 7}, {0, 0, 1, 1}}, matrix {{0, 4, 0, 4}, {0, 0, 1, 1}},matrix {{0, 4, 0, 5}, {0, 0, 1, 1}}, matrix {{0, 4, 0, 6}, {0, 0, 1, 1}},matrix {{0, 5, 0, 5}, {0, 0, 1, 1}}, matrix {{0, 0, 2}, {0, 2, 2}}, matrix {{0,1, 0, 3}, {0, 0, 2, 2}}, matrix {{0, 1, 0, 5}, {0, 0, 2, 2}}, matrix {{0, 2, 0,2}, {0, 0, 2, 2}}, matrix {{0, 2, 0, 4}, {0, 0, 2, 2}}, matrix {{0, 3, 0, 3},{0, 0, 2, 2}}, matrix {{0, 0, 3}, {0, 3, 3}}, matrix {{0, 4, 0, 1, 2}, {0, 0,1, 2, 2}}, matrix {{1, 3, 0, 0, 1}, {0, 0, 1, 2, 2}}, matrix {{2, 4, 0, 0, 1},{0, 0, 2, 3, 3}}, matrix {{1, 2, 0, 2, 0, 1}, {0, 0, 1, 1, 2, 2}}, matrix {{2,3, 3, 0, 0, 1}, {0, 0, 1, 2, 3, 3}}, matrix {{1, 3, 0, 3, 0, 1}, {0, 0, 1, 1, 3, 3}}, matrix {{3, 4, 1, 0, 0, 1}, {0, 0, 1, 2, 3, 3}}, matrix {{3, 4, 1, 4, 0, 3, 0, 1}, {0, 0, 1, 1, 2, 2, 3, 3}}};
listSmooth2D = () -> for P in Sm12in2D list convexHull P;



-- PURPOSE : Returns the list of all smooth 3-polytopes with up to
--           16 lattice points
Sm16in3D := {matrix {{0, 0, 0, 1}, {0, 1, 0, 0}, {0, 0, 1, 1}}, matrix {{0, 1, 0, 1, 0, 1},{0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 1, 0, 2}, {0, 0, 1,1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 1, 0, 3}, {0, 0, 1, 1, 0, 0},{0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 1, 0, 4}, {0, 0, 1, 1, 0, 0}, {0, 0, 0,0, 1, 1}}, matrix {{0, 1, 0, 1, 0, 5}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}},matrix {{0, 1, 0, 1, 0, 6}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix{{0, 1, 0, 1, 0, 7}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0,1, 0, 8}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 1, 0, 9},{0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 1, 0, 10}, {0, 0, 1,1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 1, 0, 11}, {0, 0, 1, 1, 0, 0},{0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 2, 0, 2}, {0, 0, 1, 1, 0, 0}, {0, 0, 0,0, 1, 1}}, matrix {{0, 1, 0, 2, 0, 3}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}},matrix {{0, 1, 0, 2, 0, 4}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix{{0, 1, 0, 2, 0, 5}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0,2, 0, 6}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 2, 0, 7},{0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 2, 0, 8}, {0, 0, 1,1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 2, 0, 9}, {0, 0, 1, 1, 0, 0},{0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 2, 0, 10}, {0, 0, 1, 1, 0, 0}, {0, 0, 0,0, 1, 1}}, matrix {{0, 1, 0, 3, 0, 3}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}},matrix {{0, 1, 0, 3, 0, 4}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix{{0, 1, 0, 3, 0, 5}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0,3, 0, 6}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 3, 0, 7},{0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 3, 0, 8}, {0, 0, 1,1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 3, 0, 9}, {0, 0, 1, 1, 0, 0},{0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 4, 0, 4}, {0, 0, 1, 1, 0, 0}, {0, 0, 0,0, 1, 1}}, matrix {{0, 1, 0, 4, 0, 5}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}},matrix {{0, 1, 0, 4, 0, 6}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix{{0, 1, 0, 4, 0, 7}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0,4, 0, 8}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 5, 0, 5},{0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 5, 0, 6}, {0, 0, 1,1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 5, 0, 7}, {0, 0, 1, 1, 0, 0},{0, 0, 0, 0, 1, 1}}, matrix {{0, 1, 0, 6, 0, 6}, {0, 0, 1, 1, 0, 0}, {0, 0, 0,0, 1, 1}}, matrix {{0, 2, 0, 2, 0, 2}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}},matrix {{0, 2, 0, 2, 0, 3}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix{{0, 2, 0, 2, 0, 4}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 2, 0,2, 0, 5}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 2, 0, 2, 0, 6},{0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 2, 0, 2, 0, 7}, {0, 0, 1,1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 2, 0, 2, 0, 8}, {0, 0, 1, 1, 0, 0},{0, 0, 0, 0, 1, 1}}, matrix {{0, 2, 0, 2, 0, 9}, {0, 0, 1, 1, 0, 0}, {0, 0, 0,0, 1, 1}}, matrix {{0, 2, 0, 3, 0, 3}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}},matrix {{0, 2, 0, 3, 0, 4}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix{{0, 2, 0, 3, 0, 5}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 2, 0,3, 0, 6}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 2, 0, 3, 0, 7},{0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 2, 0, 3, 0, 8}, {0, 0, 1,1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 2, 0, 4, 0, 4}, {0, 0, 1, 1, 0, 0},{0, 0, 0, 0, 1, 1}}, matrix {{0, 2, 0, 4, 0, 5}, {0, 0, 1, 1, 0, 0}, {0, 0, 0,0, 1, 1}}, matrix {{0, 2, 0, 4, 0, 6}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}},matrix {{0, 2, 0, 4, 0, 7}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix{{0, 2, 0, 5, 0, 5}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 2, 0,5, 0, 6}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 3, 0, 3, 0, 3},{0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 3, 0, 3, 0, 4}, {0, 0, 1,1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 3, 0, 3, 0, 5}, {0, 0, 1, 1, 0, 0},{0, 0, 0, 0, 1, 1}}, matrix {{0, 3, 0, 3, 0, 6}, {0, 0, 1, 1, 0, 0}, {0, 0, 0,0, 1, 1}}, matrix {{0, 3, 0, 3, 0, 7}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}},matrix {{0, 3, 0, 4, 0, 4}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix{{0, 3, 0, 4, 0, 5}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 3, 0,4, 0, 6}, {0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 3, 0, 5, 0, 5},{0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 4, 0, 4, 0, 4}, {0, 0, 1,1, 0, 0}, {0, 0, 0, 0, 1, 1}}, matrix {{0, 4, 0, 4, 0, 5}, {0, 0, 1, 1, 0, 0},{0, 0, 0, 0, 1, 1}}, matrix {{0, 0, 0, 2}, {0, 2, 0, 0}, {0, 0, 2, 2}}, matrix{{0, 1, 0, 1, 0, 1}, {0, 0, 2, 2, 0, 0}, {0, 0, 0, 0, 2, 2}}, matrix {{0, 1, 0,1, 0, 3}, {0, 0, 2, 2, 0, 0}, {0, 0, 0, 0, 2, 2}}, matrix {{0, 0, 1, 0, 0, 2},{0, 1, 1, 0, 2, 2}, {0, 0, 0, 1, 1, 1}}, matrix {{0, 0, 1, 0, 0, 3}, {0, 1, 1, 0, 3, 3}, {0, 0, 0, 1, 1, 1}}, matrix {{0, 1, 0, 1, 0, 1, 0, 1}, {0, 0, 1, 1, 0, 0, 1, 1}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 1, 0, 1, 0, 2, 0, 2}, {0,0, 1, 1, 0, 0, 1, 1}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 1, 0, 1, 0, 3, 0, 3}, {0, 0, 1, 1, 0, 0, 1, 1}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 1, 0, 1, 0, 4, 0, 4}, {0, 0, 1, 1, 0, 0, 1, 1}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 1, 0, 1, 0, 5, 0, 5}, {0, 0, 1, 1, 0, 0, 1, 1}, {0, 0, 0, 0, 1, 1, 1, 1}},matrix {{0, 1, 0, 1, 0, 2, 0, 2}, {0, 0, 1, 1, 0, 0, 2, 2}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 1, 0, 1, 0, 3, 0, 3}, {0, 0, 1, 1, 0, 0, 2, 2}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 1, 0, 2, 0, 2, 0, 3}, {0, 0, 1, 1, 0, 0, 1, 1},{0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 1, 0, 2, 0, 3, 0, 4}, {0, 0, 1, 1, 0, 0,1, 1}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 1, 0, 2, 0, 4, 0, 5}, {0, 0, 1,1, 0, 0, 1, 1}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 1, 0, 2, 0, 1, 0, 3},{0, 0, 1, 1, 0, 0, 2, 2}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 1, 0, 3, 0, 3,0, 5}, {0, 0, 1, 1, 0, 0, 1, 1}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 2, 0,2, 0, 2, 0, 2}, {0, 0, 1, 1, 0, 0, 1, 1}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix{{0, 2, 0, 2, 0, 3, 0, 3}, {0, 0, 1, 1, 0, 0, 1, 1}, {0, 0, 0, 0, 1, 1, 1, 1}},matrix {{0, 2, 0, 2, 0, 4, 0, 4}, {0, 0, 1, 1, 0, 0, 1, 1}, {0, 0, 0, 0, 1, 1,1, 1}}, matrix {{0, 2, 0, 2, 0, 2, 0, 2}, {0, 0, 1, 1, 0, 0, 2, 2}, {0, 0, 0,0, 1, 1, 1, 1}}, matrix {{0, 2, 0, 3, 0, 3, 0, 4}, {0, 0, 1, 1, 0, 0, 1, 1},{0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 2, 0, 3, 0, 1, 0, 3}, {0, 0, 1, 1, 0, 0,2, 2}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 3, 0, 3, 0, 3, 0, 3}, {0, 0, 1,1, 0, 0, 1, 1}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 0, 2, 0, 0, 3}, {0, 2,2, 0, 3, 3}, {0, 0, 0, 1, 1, 1}},matrix {{0, 2, 2, 0, 1, 2, 0, 1}, {0, 0, 1, 2, 2, 0, 0, 0}, {0, 0, 0, 0, 0, 1,2, 2}}, matrix {{1, 2, 0, 2, 0, 1, 0, 2, 0, 1}, {0, 0, 1, 1, 2, 2, 0, 0, 0, 0},{0, 0, 0, 0, 0, 0, 1, 1, 2, 2}}, matrix {{0, 1, 0, 2, 1, 2, 2, 0, 0, 2}, {0, 0,1, 1, 2, 2, 0, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 1, 2, 2}}, matrix {{1, 2, 0, 2,0, 1, 0, 2, 0, 0, 1, 0}, {0, 0, 1, 1, 2, 2, 0, 0, 2, 0, 0, 1}, {0, 0, 0, 0, 0,0, 1, 1, 1, 2, 2, 2}}, matrix {{0, 4, 0, 4, 0, 1, 0, 1}, {0, 0, 1, 1, 0, 0, 2, 2}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 3, 0, 3, 0, 1, 0, 1}, {0, 0, 1, 1, 0, 0, 3, 3}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 3, 0, 3, 0, 1, 0, 1}, {0,0, 1, 1, 0, 0, 2, 2}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{0, 2, 0, 2, 0, 1, 0, 1}, {0, 0, 1, 1, 0, 0, 2, 2}, {0, 0, 0, 0, 1, 1, 1, 1}}, matrix {{1, 2, 0, 2,0, 1, 2, 0, 2, 0}, {0, 0, 1, 1, 3, 0, 0, 1, 1, 3}, {0, 0, 0, 0, 0, 1, 1, 1, 1,1}}, matrix {{1, 2, 0, 2, 0, 1, 1, 2, 0, 2, 0, 1}, {0, 0, 1, 1, 2, 2, 0, 0, 1,1, 2, 2}, {0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1}}};
listSmooth3D = () -> for P in Sm16in3D list convexHull P;



-- PURPOSE : Test if a polyhedron is smooth.
--           Extends isSmooth from package Polyhedra
isSmooth Polyhedron := {} >> o -> P -> isSmooth normalFan P
isSmooth Matrix     := {} >> o -> M -> isSmooth normalFan convexHull M



-- used in gaussFiber and gaussImage
bLattice = method(TypicalValue => List) 

bLattice(Matrix) :=(M) -> (
    L:=for i to numcols M-1 list matrix{M_i};
    bLattice(L));

bLattice(List) := (A) -> (
    if (not all(A,c -> (class c === Matrix))) or #A==0 then (
	print "Must be a list of column matrices";
	return);
    n:=numrows A_0;
    if not all(A,c -> numrows c == n) then (
	print "The column matrices must have the same size";
	return);
    genpt:=transpose matrix{toList(numrows A_0:1)};
    if not isJetSpanned(A,1,genpt) then (
	print "The set of lattice points needs to span the lattice!";
	return);
    B:={};
    C:=subsets(A,(numrows A_0)+1);
    for c in C do (
	if isJetSpanned(c,1,genpt) then (
	    if not member(sum(c),B) then (
		B=append(B,sum(c)))));  
    return B)

-- PURPOSE : Computes the image of the Gauss map 
gaussImage = method(TypicalValue => List) 

gaussImage(Matrix) := (M) -> torusEmbedding(bLattice(M));
gaussImage(List) := (A) -> torusEmbedding(bLattice(A));



-- PURPOSE : Computes the general fiber of the Gauss map
gaussFiber = method(TypicalValue => List) 

gaussFiber(Matrix) := (M) -> (
    L:=for i to numcols M-1 list matrix{M_i};
    gaussFiber(L));

gaussFiber(List) := (A) -> (
    if (not all(A,c -> (class c === Matrix))) or #A==0 then (
	print "Must be a list of column matrices";
	return);
    n:=numrows A_0;
    if not all(A,c -> numrows c == n) then (
	print "The column matrices must have the same size";
	return);
    B:=bLattice(A);
    BminusB:={};
    for b in B do (
    	BminusB=join(BminusB,apply(B,c->c-b));
    	);
    BminusB=unique(BminusB);
    M:=linSpace(affineHull(convexHull(BminusB)));
    M = lift(M,ZZ);
    seq:=toSequence(for i to (numcols M)-1 list M_i);
    if seq==() then (return torusEmbedding(A));
    projection:=inducedMap(ZZ^(numrows A_0)/seq,ZZ^(numrows A_0));
    return torusEmbedding(unique(projection*A)))

--- PURPOSE : Compute the halfspace description of a polytope
ambientHalfspaces = method(TypicalValue => Sequence)
ambientHalfspaces(Polyhedron) := P -> (
    local A;
    local b;
    if (dim P)==(ambDim P) then (A,b) = halfspaces(P)
    else (
        orto:=matrix(entries inducedMap(cokernel linSpace(affineHull(P)),QQ^((ambDim P))));
        orto=transpose gens trim image transpose(orto);
        (A,b)=halfspaces(P);
        vert:=vertices(P);
        for i to numrows orto-1 do(
	    A=A||matrix(orto^{i})||(-1)*matrix(orto^{i});
	    b=b||matrix( matrix(orto^{i})*vert_0)||matrix((matrix(orto^{i})*vert_0));
	    );
        );
    (lift(A,ZZ),lift(b,ZZ))
    )

-- Used in randQPoly and randZPoly
eNorm:=(v)->(
    temp:=0;
    for i to numrows v-1 do(
	temp=temp+((v^{i})_0_0)^2;
	);
    return(sqrt(temp))
    )


-- Used in randQPoly and randZPoly
ratapprox:=(x,h)->(
    matrix apply(entries x,c->{lift(round(h,c_0),QQ)})
)


-- Used in randQPoly and randZPoly
randPt:=(n)->(
    v:=matrix apply(flatten entries sub(random((ZZ/2)^1,(ZZ/2)^n),ZZ),a->{(-1_RR)^a});
    u:=random(RR^n,RR^1);
    x:=matrix apply(entries (u|v), l->{product l});
    return(x*(1/eNorm(x)))
    )


-- Used in randQPoly and randZPoly
randS:=(V,n,h)->(
    L:=ratapprox(randPt(n),h);
    for i from 2 to V do(
	 L=L|ratapprox(randPt(n),h);
	 );
     return L;
     )


--- PURPOSE : Create an random rational polytope
randQPoly = method(TypicalValue => Polyhedron)

randQPoly(ZZ,ZZ) := (v,n) -> (
    P:=convexHull(randS(v,n,10));
    if not (v1:=(nVertices P))==v then (
	print("This has only "|toString v1|" vertices. Is it possible to have #vertices == "|toString v|"? If so, try again!"));
    P) 


--- PURPOSE : Create an random lattice polytope
randZPoly = method(TypicalValue => Polyhedron)

randZPoly(ZZ,ZZ) := (v,n) -> (
    randZPoly(v,n,3))

randZPoly(ZZ,ZZ,ZZ) := (v,n,e) -> (
    if e>5 then (print "To high precision, must be less than 6.";return emptyPolyhedron(n));
    M:=(randS(v,n,e))*10^e;
    P:=convexHull(M);
    test:=r->sub(sub(r,ZZ),QQ)==r;
    if not (v1:=(nVertices P))==v then (
	print("This has only "|toString v1|" vertices. Is it possible to have #vertices == "|toString v|"? If so, try again!"));
    if not all(entries M, p -> all(p,pp->test(pp))) then (
	print "For some reason this is not a lattice polytope. We do not know why. Try again");
    P)


-- Used in gausskFiber and gausskImage
bkLattice = method(TypicalValue => List) 

bkLattice(Matrix,ZZ) :=(M,k) -> (
    L:=for i to numcols M-1 list matrix{M_i};
    bLattice(L));

bkLattice(List,ZZ) := (A,k) -> (
    if (not all(A,c -> (class c === Matrix))) or #A==0 then (
	print "Must be a list of column matrices";
	return);
    n:=numrows A_0;
    if not all(A,c -> numrows c == n) then (
	print "The column matrices must have the same size";
	return);
    genpt:=transpose matrix{toList(numrows A_0:1)};
    if not isJetSpanned(A,k,genpt) then (
	print "The set of lattice points needs to be k-jet!";
	return);
    B:={};
    C:=subsets(A,binomial(numrows A_0+k,k));
    for c in C do (
	if isJetSpanned(c,k,genpt) then (
	    if not member(sum(c),B) then (
		B=append(B,sum(c)))));  
    return B)

-- PURPOSE : Computes the image of the k-th Gauss map 
gausskImage = method(TypicalValue => List) 

--  INPUT : A list of columns matrices, R a PolynomialRing
gausskImage(Matrix,ZZ) := (M,k) -> torusEmbedding(bkLattice(M,k));

gausskImage(List,ZZ) := (A,k) -> torusEmbedding(bkLattice(A,k));



-- PURPOSE : Computes the general fiber of the k-th Gauss map
gausskFiber = method(TypicalValue => List) 

gausskFiber(Matrix,ZZ) := (M,k) -> (
    L:=for i to numcols M-1 list matrix{M_i};
    gausskFiber(L,k));

gausskFiber(List,ZZ) := (A,k) -> (
    if (not all(A,c -> (class c === Matrix))) or #A==0 then (
	print "Must be a list of column matrices";
	return);
    n:=numrows A_0;
    if not all(A,c -> numrows c == n) then (
	print "The column matrices must have the same size";
	return);
    B:=bkLattice(A,k);
    BminusB:={};
    for b in B do (
    	BminusB=join(BminusB,apply(B,c->c-b));
    	BminusB=unique(BminusB));
    M:=linSpace(affineHull(convexHull(BminusB)));
    M = lift(M,ZZ);
    seq:=toSequence(for i to (numcols M)-1 list M_i);
    if seq==() then (return torusEmbedding(A));
    projection:=inducedMap(ZZ^(numrows A_0)/seq,ZZ^(numrows A_0));
    return torusEmbedding(unique(projection*A)))

--PURPOSE: Compute bounds for the Seshadri constant at a general point
epsilonBounds = method(TypicalValue => List) 

epsilonBounds(Polyhedron,ZZ):=(P,n)->(
    V:=vertices(P);
    V=apply(entries(transpose(V)),x->transpose(matrix{x}));
    a1:=0;
    a2:=infinity;
    eps:=1/10;
    (A,b):=halfspaces(P);
    for k from 0 to 1 do(
   	for i from 0 to n do(
	    proj:=(1/gcd(i,n-i))*sub(matrix{{(i,(-1)^k*(n-i))}},QQ);
    	    vmax:=max(apply(V,v->((proj*v)_0)_0));
	    vmin:=min(apply(V,v->((proj*v)_0)_0));
	    piP:=abs(vmax-vmin);
	    Fvs:={};
	    for v in V do(
	    	intsect:={};
	    	for i from 0 to numrows(A)-1 do( 
		    M:=sub((A^{i}||proj),RR);
		    B:=sub((b^{i}||(proj*v)),RR);
	    	    if det(M)!=0 then(
			X:=solve(M,B);
		    	intsect=append(intsect,X);
			);
		    );
		pos:=positions(intsect,pt->all(flatten entries(sub(A*pt,RR)-sub(b,RR)),x->x<=0));
		intInP:=unique(intsect_pos);
		if (#intInP==2) then(
		    fVec:=intInP_0-intInP_1;
		    fv:=gcd(apply(flatten entries(fVec),x->promote(x,QQ)));
		    Fvs=append(Fvs,fv);
		    );
      	    	);
	    a1=max({a1,min({piP,max(Fvs)})});
	    a2=min({a2,piP});
	    );
	);
    return({a1,a2})
    )



--PURPOSE: Check if a given polytope is of type [P0*P1]^k with all edges of length at least k
iskCayleykEdges=method(TypicalValue=>Sequence)

iskCayleykEdges(Polyhedron):=P->(
    boolean:=true;
    (A,b):=halfspaces(P);
    V:=vertices(P);
    E:=facesAsPolyhedra((dim P)-1,P);
    minLen:=infinity;
    for e in E do(
	len:=gcd(flatten entries((vertices(e))_0-(vertices(e))_1));
	minLen=min(len,minLen);
	);
    P0:=0;
    P1:=0;
    for i to (numrows(A)-1) do(
       	 L:=unique(flatten entries(A^{i}*V));
	 if length(L)==2 then(
	     k:=(L_1-L_0)/gcd(flatten entries(A^{i}));
	     if k==minLen then(
		 H0:=intersection(A^{i}||-A^{i},matrix{{L_0},{-L_0}});
		 P0=intersection(P,H0);
		 H1:=intersection(A^{i}||-A^{i},matrix{{L_1},{-L_1}});
		 P1=intersection(P,H1);
		 boolean=false;
	     );
	 );
     );
  if (not boolean) then(return((P0,P1,minLen));)else(print("Conditions not met"););
 )






----------------------
beginDocumentation()

document {
     	Key => LatticePolytopes,
	Headline => "for computations with lattice polytopes",
	
	"A lattice polytope is a bounded object of the type ", TO Polyhedron, ". This package is focused on  
	 functions that are specific for lattice polytopes rather than general polyhedra. Examples of such methods are ", TO isCayley,"
	, ", TO cayley, " and ", TO randZPoly, ". Moreover the package contains known classifications
	 of smooth 2-polytopes with up to 12 lattice points and smooth 3-polytopes with up to 16 lattice points. These classifications are accessible via the functions ", TO listSmooth2D," and ", TO listSmooth3D," ",
	
	PARA{}, TT "LatticePolytopes", " uses the ", TO Polyhedra, " package by ", 
	HREF("http://page.mi.fu-berlin.de/rbirkner/", "René Birkner"), " and the ", TO NormalToricVarieties, " package by ",
	HREF("http://www.mast.queensu.ca/~ggsmith/", "Gregory Smith"),
	
	PARA{}, "The following is an example illustrating the main functions provided in the package.",
	UL {
	     {TO "Working with lattice polytopes"},
	     },
	
	PARA{}, "For an introduction to polytopes, we recommend ",
	HREF("http://www.math.tu-berlin.de/~ziegler/", "Günter
	M. Ziegler's"), " ", EM "Lectures on Polytopes", ", Graduate
	Texts in Mathematics 152, Springer-Verlag, New York, 1995.",
		
	}
    
document {
     Key => "Working with lattice polytopes",
     
     "Given a polytope (",TO Polyhedron,") which is the ",TO convexHull," of a given set of points we might ask if the polytope is a Cayley polytope",
     
     EXAMPLE {
	  " V = matrix {{0,2,-2,0},{0,1,1,1},{1,2,3,4}}",
	  " P = convexHull V",
	  " isCayley P"
	  },
     
     PARA{}, "We can also construct Cayley polytopes by taking the Cayley sum of several polytopes.",
     
     EXAMPLE {
	  " P2 = convexHull matrix{{0,1,2,3},{0,5,5,5},{1,2,3,2}}",
	  " cayley(P,P2,2)",
	  "vertices oo"
	  },
     
     }

doc ///
  Key
    isCayley
    (isCayley,Matrix)
    (isCayley,Polyhedron)
  Headline
     checks if a polytope is Cayley
  Usage
     isCayley M,  
     isCayley P
  Inputs
    M:Matrix
    P:Polyhedron
  Outputs
    :Boolean
  Description
   Text
     A lattice polytope is Cayley if its vertices are contained in two parallel hyperplanes. The function isCayley checks this condition for a given polytope P.
     
   Example
     P=hypercube 3;
     isCayley(P)
   
   Text
     One can also input a matrix M to the function isCayley. In this case the function checks if the convex hull och the columns of M is a Cayley polytope.
     
   Example
     isCayley matrix{{0,2,0},{0,0,2}} 
    
  SeeAlso
    cayley
///

doc ///
  Key
    cayley
    (cayley,Polyhedron, Polyhedron)
    (cayley,Polyhedron, Polyhedron,ZZ)
    (cayley,Polyhedron, Polyhedron,Polyhedron)
    (cayley,Polyhedron, Polyhedron,Polyhedron,ZZ)
    (cayley,List)
    (cayley,List,ZZ)
    (cayley,Matrix,Matrix)
    (cayley,Matrix,Matrix,ZZ)
    (cayley,Matrix,Matrix,Matrix)
    (cayley,Matrix,Matrix,Matrix,ZZ)
  Headline
     constructs the Cayley sum of polytopes
  Usage
     cayley(P1,P2),
     cayley(P1,P2,k),
     cayley(P1,P2,P3),
     cayley(P1,P2,P3,k),
     cayley(L),
     cayley(L,k),  
     cayley(M1,M2),
     cayley(M1,M2,k),
     cayley(M1,M2,M3),
     cayley(M1,M2,M3,k)
  Inputs
    P1:Polyhedron
    P2:Polyhedron
    P3:Polyhedron
    M1:Matrix
    M2:Matrix
    M3:Matrix
    k:ZZ
    L:List
  Outputs
    :Polyhedron
  Description
   Text
     Given polytopes P and Q the function computes the cayley sum of P and Q.
   
   Example
     P=convexHull(matrix{{0,1}});
     Q=convexHull(matrix{{0,2}});
     C=cayley(P,Q)
     vertices C
      
   Text
     One can also construct the Cayley polytope of order k by specifying the positive integer k.
     
   Example
     C=cayley(P,Q,3)
     vertices C 
     
   Text
     You can also compute the Cayley sum of several polytopes of any order, by placing the polytopes in a list.
   
   Example
     C=cayley({P,Q,Q,P,P},2)
     vertices C
     
  SeeAlso
    isCayley
///

doc ///
  Key
    areIsomorphic
    (areIsomorphic,Polyhedron, Polyhedron)
    (areIsomorphic,Matrix,Matrix)
  Headline
     checks if two smooth polytopes are isomorphic
  Usage
     areIsomorphic(P,Q),
     areIsomorphic(M,N)
  Inputs
    P:Polyhedron
    Q:Polyhedron
    M:Matrix
    N:Matrix
  Outputs
    :Boolean
  Description
   Text
     Checks if two smooth polytopes P and Q are isomorphic, i.e. checks if there exist a unitary matrix A with integer entries and a vector v such that Q=A*P+v. Currently the function only works on smooth polytopes.
   
   Example
     P=convexHull(matrix{{0,1}});
     Q=convexHull(matrix{{0,2}});
     areIsomorphic(P,Q)
   
   Text
     As a standard, areIsomorphic will check if the polytopes are smooth first. This takes some time, so if one is sure that they are smooth then it is possible to suppress this test.
     
   Example
     M = transpose matrix{{0,0,0},{1,0,0},{0,1,0},{0,0,1},{1,1,0},{1,0,1},{0,1,1},{1,1,1}}
     P = convexHull(M);
     time areIsomorphic(P,P);
     time areIsomorphic(P,P,smoothTest=>false);
     
     
///

doc ///
  Key
    toricBlowUp
    (toricBlowUp,Polyhedron, Polyhedron)
    (toricBlowUp,Polyhedron, Polyhedron,ZZ)
    (toricBlowUp,Matrix,Matrix)
    (toricBlowUp,Matrix,Matrix,ZZ)
  Headline
     calculates the stellar subdivision of a polytope at a given face.
  Usage
     toricBlowUp(P,Q),
     toricBlowUp(P,Q,k),
     toricBlowUp(M,N),
     toricBlowUp(M,N,k)
  Inputs
    P:Polyhedron
    Q:Polyhedron
    M:Matrix
    N:Matrix
    k:ZZ
  Outputs
    :Polyhedron
  Description
   Text
     Calculates the stellar subdivision of height k of a polytope P at the face Q. This corresponds to constructing the embedding given by the global sections of L-kE for the blow-up at the torus invariant subvariety associated to Q. Here L is the ample line bundle on the toric variety corresponding to P and E is the exceptional divisor.   
   Example
     P=cayley(matrix{{0,2,0}},matrix{{0,0,2}})
     vertices oo
     Q=convexHull(matrix{(vertices P)_0})
     toricBlowUp(P,Q,1)
     vertices oo
     
    
///

doc ///
  Key
    listSmooth2D
  Headline
     gives the list of all smooth 2-polytopes with up to 12 lattice points
  Usage
     listSmooth2D()
  Outputs
    :List
  Description
   Text
     Provides the list of all smooth 2-polytopes with up to 12 lattice points based on the classification by  Benjamin Lorenz.
   
   Example
     L=listSmooth2D();
   
   Text
     One can then check how many of these are Cayley.
     
   Example
     tally apply(L,isCayley) 
    
  SeeAlso
    listSmooth3D
    isCayley
///

doc ///
  Key
    listSmooth3D
  Headline
     gives the list of all smooth 3-polytopes with up to 16 lattice points
  Usage
     listSmooth3D()
  Outputs
    :List
  Description
   Text
     Provides the list of all smooth 3-polytopes with up to 16 lattice points based on the classification by Anders Lundman.
   
   Example
     L=listSmooth3D();
   
   Text
     A. Lundman claims that 99 of the listed polytopes are Cayley. This can readily be tested.
     
   Example
     #L
     tally apply(L,isCayley) 
    
  SeeAlso
    listSmooth2D
    isCayley
///

doc ///
  Key
    randQPoly
    (randQPoly,ZZ,ZZ)
  Headline
     gives a random rational polytope
  Usage
     randQPoly(v,n)
  Inputs
    v:ZZ
    n:ZZ
  Outputs
    :Polyhedron
  Description
   Text
     Given integers v and n this function returns a random rational polytope with v vertices in ambient dimension n, such that the vertices lie close to the unit n-sphere.
   
   Example
     P=randQPoly(5,3)
     vertices P
    
    
  SeeAlso
    randZPoly
///

doc ///
  Key
    randZPoly
    (randZPoly,ZZ,ZZ)
    (randZPoly,ZZ,ZZ,ZZ)
  Headline
     gives a random lattice polytope
  Usage
     randZPoly(v,n)
     randZPoly(v,n,e)
  Inputs
    v:ZZ
    n:ZZ
    e:ZZ
  Outputs
    :Polyhedron
  Description
   Text
     Given integers v and n this function returns a random lattice polytope with v vertices in ambient dimension n contained in [-1000,1000]^n.
   Example
     P=randZPoly(5,3)
     vertices P
   
   Text
     You can also specify an integer e. This has the effect of changing the randomisation procedure so that a random polytope in [-10^e,10^e]^n is returned.
   Example
     P=randZPoly(5,3,5)
     vertices P
    
    
  SeeAlso
    randQPoly
///

doc ///
  Key
    isJetSpanned
    (isJetSpanned,Matrix,ZZ,Matrix)
    (isJetSpanned,List,ZZ,Matrix)
  Headline
     checks if the polarized toric variety associated to a set of lattice points is k-jet spanned at a given point. 
  Usage
     isJetSpanned(A,k,pt)
  Inputs
    A:Matrix
    k:ZZ
    pt:Matrix
  Outputs
    :Boolean
  Description
   Text
     The function checks if the polarized toric variety associated to the set of lattice points A is k-jet spanned at the point pt.
   Example
     A=latticePoints(convexHull(matrix{{0,0,2},{0,2,0}}))
     pt=matrix{{1},{1}}
     isJetSpanned(A,2,pt) 
  SeeAlso
    jetMatrix
///

doc ///
  Key
    jetMatrix
    (jetMatrix,Matrix,ZZ,Matrix)
    (jetMatrix,List,ZZ,Matrix)
    (jetMatrix,Matrix,ZZ)
    (jetMatrix,List,ZZ)
  Headline
     construct the matrix of k-jets evaluated at a given point.
  Usage
     jetMatrix(A,k,pt)
  Inputs
    A:Matrix
    k:ZZ
    pt:Matrix
  Outputs
    :Matrix
  Description
   Text
     The function constructs the matrix of k-jets evaluated at the point pt for the polarized toric variety associated to the set of lattice points A.
   
   Example
     A=latticePoints(convexHull(matrix{{0,0,2},{0,2,0}}))
     pt=matrix{{1},{1}}
     jetMatrix(A,2,pt)
    
   Text
    If no point is provided the matrix of k-jets is provided as a matrix over a polynomial ring.
    
   Example
     A=latticePoints(convexHull(matrix{{0,0,2},{0,2,0}}))
     jetMatrix(A,2)
  
  SeeAlso
    isJetSpanned
///

document {
     Key => {torusEmbedding, (torusEmbedding,Matrix), (torusEmbedding,List)},
     Headline => "gives the toric embedding corresponding to a set of lattice points",
     Usage => " torusEmbedding(X)",
     Inputs => {
	  "X" => Matrix => {"in which the columns are the lattice points or ",ofClass List, " of column vectors"},
      	  },
     Outputs => {
	  List => {"a list of monomials giving the embedding"},
      	  },
     
     PARA{}, TT "torusEmbedding", " is a function. It takes a set of lattice points as input and returns a list of monomials giving the corresponding torus embedding.",
     
     EXAMPLE {
	  "torusEmbedding(matrix{{0,1,2},{0,3,4}})",
	  },
     
     }
 
 document {
     Key => {toricDiv, (toricDiv,Matrix), (toricDiv,Polyhedron)},
     Headline => "constructs the toric Weil divisor associated to a polytope",
     Usage => " toricDiv(X)",
     Inputs => {
	  "X" => Matrix => {"of vertices or ",ofClass Polyhedron, "."},
      	  },
     Outputs => {
	  ToricDivisor
      	  },
     
     PARA{}, TT "toricDiv", " is a function. It returns ",ofClass ToricDivisor, " corresponding to a polytope P on the toric variety associated to the normal fan of P.",
     
     EXAMPLE {
	  "toricDiv(hypercube(2))",
	  },
     
     }
 
  document {
     Key => {codegree, (codegree,Matrix), (codegree,Polyhedron)},
     Headline => "computes the codegree of a polytope",
     Usage => " codegree(X)",
     Inputs => {
	  "X" => Matrix => {"of vertices or ",ofClass Polyhedron, "."},
      	  },
     Outputs => {
	  ZZ => {"the codegree of X"}
      	  },
     
     PARA{}, TT "codegree", " is a function that returns the codegree of a polytope. ",
     
     EXAMPLE {
	  "codegree(hypercube(4))",
	  },
     
     }
 
   document {
     Key => {ambientHalfspaces, (ambientHalfspaces,Polyhedron)},
     Headline => "gives the defining halfspaces of a polytope",
     Usage => " (A,b) = ambientHalfspaces(P)",
     Inputs => {
	  "P" => Polyhedron,
      	  },
     Outputs => {
	  Sequence => {"(A,b) where A is a ", ofClass Matrix, " and b is a column vector of class ", ofClass Matrix, " where each row corresponds to a halfspace."}
      	  },
     
     PARA{}, TT "ambientHalfspaces", " is a function. It provides the halfspace description of a polytope in the ambient space. The function ", TT "halfspaces", " instead gives the halfspace description in the affine span of the polytope.",
     
     EXAMPLE {
	  "halfspaces(convexHull matrix{{0,1},{0,0}})",
	  "ambientHalfspaces(convexHull matrix{{0,1},{0,0}})",
	  },
     
     }
 
  document {
     Key => {adjointPolytope, (adjointPolytope,Polyhedron,ZZ), (adjointPolytope,Matrix,ZZ)},
     Headline => "computes the adjoint of a polytope",
     Usage => " adjointPolytope(X,k)",
     Inputs => {
	  "X" => Polyhedron => {" or a ", ofClass Matrix," where the columns are the vertice points"},
	  "k" => ZZ => {" an integer."},
      	  },
     Outputs => {
	  Polyhedron => {"The adjoint Polytope of X corresponding to the line bundle kK+L, see the article ", HREF("http://msp.org/ant/2013/7-10/p02.xhtml", "Polyhedral adjunction theory"), " by Sandra Di Rocco, Christian Haase, Benjamin Nill and Andreas Paffenholz."},
      	  },
     
     PARA{}, TT "adjointPolytope", " is a function that compute the k:th adjoint of a polytope",
     
     EXAMPLE {
	  "adjointPolytope(hypercube(2),1)",
	  },
     
     }
 
  document {
     Key => {gaussFiber, (gaussFiber,Matrix), (gaussFiber,List)},
     Headline => "computes the general fiber of the Gauss map",
     Usage => " gaussFiber(A)",
     Inputs => {
	  "A" => Matrix => {" or ", ofClass List," where the columns are lattice points"},
	  },
     Outputs => {
	  List => {"of monomials giving the torus embedding of an irreducible component of the general fiber of the Gauss map of the toric variety X_A"}
      	  },
     
     PARA{}, TT "gaussFiber", " is a function. It computes an irreducible component of the general fiber of the Gauss map as described in the paper ", HREF("http://arxiv.org/abs/1403.0793","Gauss Maps of Toric Varieties"),
     
     EXAMPLE {
	  "P=convexHull(matrix{{0,1}});",
	  "Q=convexHull(matrix{{0}});",
	  "A=latticePoints(cayley({P,Q}));",
	  "B=latticePoints(cayley({P,P}));",
	  "gaussFiber(A)",
	  "gaussFiber(B)",
	  },
      SeeAlso=>{gausskFiber,gaussImage,gausskImage}
     }
 
  document {
     Key => {gausskFiber, (gausskFiber,Matrix,ZZ), (gausskFiber,List,ZZ)},
     Headline => "computes the general fiber of the Gauss map of order k",
     Usage => " gausskFiber(A,k)",
     Inputs => {
	  "A" => Matrix => {" or a ", ofClass List," where the columns are lattice points"},
	  "k"=>ZZ,
	  },
     Outputs => {
	  List => {"of monomials giving the torus embedding of an irreducible component of the general fiber of the Gauss map of order k for the generically k-jet spanned toric variety X_A"}
      	  },
     
     PARA{}, TT "gausskFiber", " is a function. It computes an irreducible component of the general fiber of the Gauss map order k as described in the paper ", HREF("http://arxiv.org/abs/1410.4811","A note on higher order Gauss maps"),
     
     EXAMPLE {
	  "P=hypercube(2,1);",
	  "Q=convexHull(matrix{{0}});",
	  "A=latticePoints(P);",
	  "B=latticePoints(cayley({Q,Q,Q},2));",
	  "gausskFiber(A,2)",
	  "gausskFiber(B,2)",
	  },
      SeeAlso=>{gaussFiber,gaussImage,gausskImage}
     }
 
  document {
     Key => {gaussImage, (gaussImage,Matrix), (gaussImage,List)},
     Headline => "computes the image the Gauss map",
     Usage => " gaussImage(A)",
     Inputs => {
	  "A" => Matrix => {" or ", ofClass List," where the columns are lattice points"},
	  },
     Outputs => {
	  List => {"of monomials giving the torus embedding via the Plücker embedding of the image of the Gauss map of the toric variety X_A"}
      	  },
     
     PARA{}, TT "gaussImage", " is a function. It computes the image of the Gauss map embedded via the Plücker embedding as described in the paper ", HREF("http://arxiv.org/abs/1403.0793","Gauss Maps of Toric Varieties"),
     
     EXAMPLE {
	  "P=convexHull(matrix{{0,1}});",
	  "Q=convexHull(matrix{{0}});",
	  "A=latticePoints(cayley({P,Q}));",
	  "B=latticePoints(cayley({P,P}));",
	  "gaussImage(A)",
	  "gaussImage(B)",
	  },
      SeeAlso=>{gaussFiber,gausskFiber,gausskImage}
     }
  document {
     Key => {gausskImage, (gausskImage,Matrix,ZZ), (gausskImage,List,ZZ)},
     Headline => "computes the image of the Gauss map of order k",
     Usage => " gausskImage(A,k)",
     Inputs => {
	  "A" => Matrix => {" or a ", ofClass List," where the columns are lattice points"},
	  "k"=>ZZ,
	  },
     Outputs => {
	  List => {"of monomials giving the torus embedding via the Plücker embedding of the image of the Gauss map of order k for generically k-jet spanned the toric variety X_A"}
      	  },
     
     PARA{}, TT "gausskImage", " is a function. It computes the image of the Gauss map of order k embedded via the Plücker embedding as described in the paper ", HREF("http://arxiv.org/abs/1410.4811","A note on higher order Gauss maps"),
     
     EXAMPLE {
	  "P=hypercube(2,1);",
	  "Q=convexHull(matrix{{0}});",
	  "A=latticePoints(P);",
	  "B=latticePoints(cayley({Q,Q,Q},2));",
	  "gausskImage(A,2)",
	  "gausskImage(B,2)",
	  },
      SeeAlso=>{gaussImage,gaussFiber, gausskFiber}
     }
 
  document {
     Key => {degreeOfJetSeparation, (degreeOfJetSeparation,Matrix,Matrix),(degreeOfJetSeparation,List,Matrix)},
     Headline => "computes the degree of jetSeparation at a given point",
     Usage => " degreeOfJetSeparation(A,pt)",
     Inputs => {
	  "A" => Matrix => {" or  ", ofClass List," where the columns are lattice points"},
	  "pt"=>Matrix =>{"a point as a column vector"},
	  },
     Outputs => {
	  "k"=>ZZ => {"the maximal such that the embedding specified by A is k-jet spanned at pt"}
      	  },
     
     PARA{}, TT "degreeOfJetSeparation", " is a function. Given the set of lattice points of ", ofClass Polyhedron," and a point ", ofClass Matrix, " it computes the maximal k such that the associated line bundle is k-jet spanned at the point",
     
     EXAMPLE {
	  "P=convexHull(matrix{{0,2}});",
	  "Q=convexHull(matrix{{0,4}});",
	  "A=latticePoints(cayley({P,Q},2));",
	  "pt=matrix{{1},{1}};",
	  "degreeOfJetSeparation(A,pt)",
	  },
      SeeAlso=>isJetSpanned
     }
 
  document {
     Key => {epsilonBounds, (epsilonBounds,Polyhedron,ZZ)},
     Headline => "computes bounds for the Seshadri constant a general point",
     Usage => "epsilonBounds(P)",
     Inputs => {
	  "P" => Polyhedron=>{"of dimension 2"},
	  "n"=> ZZ=>{},
	  },
     Outputs => {
	  "L"=>List => {"Containing bounds a and b such that a<=epsilon(X_P,L_P;1_P)<=b"},
      	  },
     
     PARA{}, TT "epsilonBounds", " is a function. It computes bounds for the Seshadri constant at a general point by applying a set of projections. The integer n determines which projections are considered.",
     
     EXAMPLE {
	  "P=convexHull(matrix{{0,2}});",
	  "Q=convexHull(matrix{{0,4}});",
	  "R=cayley({P,Q},2);",
	  "S=convexHull(matrix{{0,1,-1},{1,0,-1}});",
	  "epsilonBounds(R,10)",
	  "epsilonBounds(S,10)",
	  },
      
      SeeAlso=>isJetSpanned
     }
  document {
     Key => {iskCayleykEdges, (iskCayleykEdges,Polyhedron)},
     Headline => "Checks if a polytope is Cayley of type [P_0*P_1]^k and has every edge of length k",
     Usage => "iskCayleykEdges(P)",
     Inputs => {
	  "P" => Polyhedron=>{},
	  },
     Outputs => {
	  "L"=>Sequence => {"(P_0,P_1,k) such that conditions are satisfied. Or declare that conditions are not satisfied."},
      	  },
     
     PARA{}, TT "iskCayleykEdges", " is a function. It checks if at given polytope is of type [P_0*P_1]^k with every edge of length k",
     
     EXAMPLE {
	  "P=convexHull(matrix{{0,2}});",
	  "Q1=convexHull(matrix{{0,4}});",
	  "Q2=convexHull(matrix{{0,3}});",
	  "R1=cayley({P,Q1},2);",
	  "R2=cayley({P,Q2},2);",
	  "(P0,P1,k)=iskCayleykEdges(R1)",
	  "vertices(P0)",
	  "vertices(P1)",
	  "iskCayleykEdges(R2)",
	  },
      
      SeeAlso=>isCayley
     }
TEST ///
P = convexHull matrix {{0,0},{0,2}};
Q = hypercube(2);
assert(isCayley cayley(P,Q))
///

TEST ///
(A,b)=ambientHalfspaces(hypercube(3));
assert(areIsomorphic(intersection(A,b),hypercube(3)));
///

TEST ///
assert(codegree(convexHull(matrix{{0,2,0},{0,0,2}}))==2);
///

TEST ///
assert(isJetSpanned(latticePoints(convexHull(matrix{{0,2,0},{0,0,2}})),2,matrix{{1},{1}}));
///

TEST ///
assert(jetMatrix(latticePoints(convexHull(matrix{{0,2,0},{0,0,2}})),2,matrix{{1},{1}}) == matrix {{1, 1, 1, 1, 1, 1}, {0, 0, 0, 1, 1, 2}, {0, 0, 0, 0, 0, 2}, {0, 0, 0, 0, 1, 0}, {0, 1, 2, 0, 1, 0}, {0, 0, 2, 0, 0, 0}});
///

TEST ///
assert(not isEmpty(randZPoly(5,2)))
///

TEST ///
assert(areIsomorphic(toricBlowUp(convexHull(matrix{{0,2,0},{0,0,2}}),convexHull(matrix{{0},{0}}),1),convexHull(matrix{{1,0,2,0},{0,1,0,2}})))
///

TEST ///
assert(exponents(product(torusEmbedding(latticePoints(convexHull(matrix{{0,2,0},{0,0,2}})))))=={{4,4}})
///
