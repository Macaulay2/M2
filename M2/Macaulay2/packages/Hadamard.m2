newPackage(
    "Hadamard",
    Version => "0.1",
    Date => "November 2020",
    Authors => {

	{Name => "Iman Bahmani Jafarloo",
	    Email => "ibahmani89@gmail.com",
	    HomePage => "http://calvino.polito.it/~imanbj"
	    }
	}, -- TODO
    Headline => "Hadamard products of projective subvarieties",
    AuxiliaryFiles => false,
    DebuggingMode => false,
    Reload => false,
    PackageExports => {"Points"},
    Keywords => {"Commutative Algebra"}
    )
export {
    -- types
    "Point",
    -- methods
    "point",
    "hadamardProduct",
    "hadamardPower",
    "idealOfProjectivePoints"
    }

--defining a new type of objects: points

Point = new Type of BasicList

point=method()
point(VisibleList):=(P)->(
    if all(P, i->i==0) then error("all entries are zero") else
    if any(P,i->i===null) then error("null entries are not allowed")
    else new Point from P)


Point * Point:=(p,q)->(
    if #p =!= #q then error("points should be in a same projective space") else
    pp:=apply(p,q,times);
    if all(pp, i->i==0) then error("product of points has no nonzero coordinate")
    else return pp
    )

Point == Point := (p,q)->(
    rank pointsToMatrix({p,q}) == 1
    )

---Hadamard product of two points---
hadProdPoints = method()	 
hadProdPoints(Point,Point):=(p,q)->(
     p * q
    )

hadProdPoints(List,List):=(p,q)->(
 point p * point q
    )

hadProdPoints(Array,Array):=(p,q)->(
   point p * point q
    )

-- Hadamrd product of two varieties
hadProdOfVariety = method()
hadProdOfVariety (Ideal, Ideal):= (I,J) -> (
    newy := symbol newy;
    newz := symbol newz;
    varI:= gens ring I;
    varJ:= gens ring J;
    CRI:=coefficientRing ring I;
    CRJ:=coefficientRing ring J;
    RYI:=CRI[newy_0.. newy_(# varI -1)];
    RZJ:=CRI[newz_0..newz_(# varJ -1)];
    IY:=sub(I,apply(varI,gens RYI,(a,b)->a=>b));
    JZ:=sub(J,apply(varJ,gens RZJ,(a,b)->a=>b));
    TensorRingIJ:=RYI/IY ** RZJ/JZ;
    use TensorRingIJ;
    Projvars:=apply(#(gens ring I),i->newy_i * newz_i);
    hadMap:=map(TensorRingIJ,ring I, Projvars);
    ker hadMap
    )


-------Hadmard product of two subsets of points on two varieties---------
hadProdListsOfPoints = method()
hadProdListsOfPoints(List,List) :=(X,Y)->(
     convert:= obj -> if not instance(obj, Point) then point(obj) else obj;
     newX:=apply(X,convert);
     newY:=apply(Y,convert);
     return delete(null, for I in toList(set(newX) ** set(newY)) list (
	 try(I_0 * I_1) then (I_0 * I_1) else null
	 ))
     )


pointsToMatrix=method()
pointsToMatrix(List):= (PTM) ->( matrix apply(PTM, toList))

---Hadamard powers of varieties------------

hadamardPower = method()
hadamardPower(Ideal,ZZ):=(I,r)->(
    if r<1 then error("the second argument should be positive integer >=1");
   NewI := I;
   for i from 1 to r-1 do NewI = hadProdOfVariety(NewI,I);
   return NewI)

---Hadamard powers of sets of points ------------

hadamardPower(List,ZZ):=(L,r)->(
    if r<1 then error("the second argument should be a positive integer >=1");
   NewL := L;
   for i from 1 to r-1 do NewL = hadProdListsOfPoints(NewL,L);
   return toList set NewL)


-----------------%%%--------------------------------%%%---------------

hadamardMult=method()
hadamardMult(List):=(L)->(
    if not uniform L then
     error("entries should be in the same class");
    if instance(first L,Ideal) then fold(hadProdOfVariety,L)
    else
    if instance(first L,List) or instance(first L,Point) then fold(hadProdPoints,L)
    else error("input should be a list of ideals or points")
    )

-------general product------

hadamardProduct=method()
hadamardProduct(Ideal,Ideal):=(I,J)->(hadProdOfVariety(I,J))
hadamardProduct(List,List):=(X,Y)->(hadProdListsOfPoints(X,Y))
hadamardProduct(List):=(L)->(hadamardMult(L));


------------------Hadamard product ends ------------------




-----------------------new results-------------
idealOfProjectivePoints=method()
idealOfProjectivePoints(List,Ring):=(L,R)->(
    if not uniform L then 
     error("entries should be in the same class");
    MP:=transpose pointsToMatrix L; 
    return ideal projectivePointsByIntersection(MP,R)
    )

--------Terracini lemma------






-----------------------------------------------
beginDocumentation()

doc ///
     Key
       Hadamard
     Headline
       a package to study Hadamard products of varieties.
     Description
       Text
	 This package provides a class for representing points in projective
	 space and methods for computing Hadamard products of varieties.
///

doc ///
    Key
    	Point
    Headline
    	a new type for points in projective space
    Description
    	Text
	   A point in projective space is represented as an object in the class @TO Point@. An element of this class is a @TO BasicList@.
///

doc ///
    Key
       (symbol *, Point, Point)
    Headline
    	entrywise product of two projective points
    Usage
    	p * q
    Inputs
    	p:Point
	q:Point
    Outputs
    	:Point
    Description
    	Example
	    p = point {1,2,3};
	    q = point {-1,2,5};
	    p * q
	Text
	    Note that this operation is not always well-defined in projective space, 
	    e.g., the Hadamard product of the points $[1:0]$ and $[0:1]$ is not well-defined
///

doc ///
    Key
    	(symbol ==, Point, Point)
    Headline
    	check equality of two projective points
    Usage
    	p == q
    Inputs
    	p:Point
	q:Point
    Outputs
    	:Boolean
    Description
    	Example
	    p = point {1,1};
	    q = point {2,2};
	    p == q
///

doc ///
    Key
    	point
        (point, VisibleList)
    Headline
        constructs a projective point from the list (or array) of coordinates.
    Usage
        point(L)
    Inputs
        L:List 
	   or @TO2{Array, "array"}@
	   or @TO2{VisibleList, "visible list"}@
    Outputs
        :Point
    Description
        Example
            point {1,2,3}
            point [1,4,6]
///

doc ///
    Key
    	hadamardProduct
    Headline
        computes the Hadamard product of varieties
///

doc ///
    Key
	(hadamardProduct, Ideal, Ideal)
    Headline
         Hadamard product of two homogeneous ideals
    Usage
        hadamardProduct(I,J)
    Inputs
        I:Ideal
	    (homogeneous)
	J:Ideal
	    (homogeneous)
    Outputs
         :Ideal
    Description
        Text
            Given two projective subvarieties $X$ and $Y$, their Hadamard product is defined as the
	    Zariski closure of the set of (well-defined) entrywise products of pairs of points in the cartesian 
	    product $X \times Y$. This can also be regarded as the image of the Segre product of $X \times Y$
	    via the linear projection on the $z_{ii}$ coordinates. The latter is the way the function is implemented.
	
	    Consider for example the entrywise product of two points.
	Example
	    S = QQ[x,y,z,t];
	    p = point {1,1,1,2};
	    q = point {1,-1,-1,-1};
	    idealOfProjectivePoints({p*q},S)
	Text
	    This can be computed also from their defining ideals as explained.
	Example
	    IP = ideal(x-y,x-z,2*x-t)
	    IQ = ideal(x+y,x+z,x+t)
            hadamardProduct(IP,IQ)
	Text
	    We can also consider Hadamard product of higher dimensional varieties. 
	    For example, the Hadamard product of two lines.
	Example    
            I = ideal(random(1,S),random(1,S));
            J = ideal(random(1,S),random(1,S));
	    hadamardProduct(I,J)
///

doc ///
    Key
       (hadamardProduct, List)
    Headline
        Hadamard product of a list of homogeneous ideals, or points
    Usage
    	hadamardProduct(L)
    Inputs
    	L:List
            of @TO2{Ideal, "(homogeneous) ideals"}@ or @TO2{Point, "(projective) points"}@
    Outputs
    	:Ideal
	:Point
    Description
    	Text
	    The Hadamard product of a list of ideals or points constructed by using iteratively the binary function
	    @TO (hadamardProduct, Ideal, Ideal)@, or  @TO (symbol *, Point, Point)@.
	Example
	    S = QQ[x,y,z,t];
	    I = ideal(random(1,S),random(1,S));
            J = ideal(random(1,S),random(1,S));
	    L = {I,J};
	    hadamardProduct(L)
	    P = point\{{1,2,3},{-1,1,1},{1,1/2,-1/3}}
	    hadamardProduct(P)
///

doc ///
    Key
    	(hadamardProduct, List, List)
    Headline
        Hadamard product of two sets of points    
    Usage
    	hadamardProduct(L,M)
    Inputs
    	L:List
	    of @TO Point@
	M:List
	    of @TO Point@
    Outputs
    	:List
	    of @TO Point@
    Description
    	Text
	    Given two sets of points $L$ and $M$ returns the list of (well-defined) entrywise
	    multiplication of pairs of points in the cartesian product $L\times M$.
	Example
	    L = {point{0,1}, point{1,2}};
	    M = {point{1,0}, point{2,2}};
	    hadamardProduct(L,M)
///

doc ///
    Key
      hadamardPower
    Headline
        computes the Hadamard powers of varieties
///


doc ///
    Key
     (hadamardPower,Ideal, ZZ)
    Headline
        computes the $r$-th Hadmard powers of varieties
    Usage
        hadamardPower(I,r)
    Inputs
        I:Ideal
	   of @TO2{Ideal, "(homogeneous) ideals"}@ 
        r:ZZ
	  a positive integer $>=1$
    Outputs
         :Ideal
    Description
        Text
         Give a homogeneous ideal $I$, the $r$-th Hadamard power of $I$ is $r$-times Hadamard product of I to itself; $( I x\cdots x I)_{r-times}$
        Example
            S=QQ[x,y,z,w]
            I=ideal(random(1,S),random(1,S),random(1,S))
            hadamardPower(I,3)
///

doc ///
    Key
     (hadamardPower,List,ZZ)
    Headline
        computes the $r$-th Hadmard powers of a set points
    Usage
        hadamardPower(L,r)
    Inputs
        L:List 
	  of @TO2{Point, "(projective) points"}@
	r:ZZ
	  a positive integer $>=1$
    Outputs
         :List
    Description
        Text
         Give a set of points $L$, the $r$-th Hadamard power of $L$ is $r$-times Hadamard product of L to itself; $( L x\cdots x L)_{r-times}$
        Example
            L={point{1,1,1/2},point{1,0,1},point{1,2,4}}
            hadamardPower(L,3)
///

doc ///
    Key
     idealOfProjectivePoints
     (idealOfProjectivePoints,List,Ring)
    Headline
        computes the ideal of set of points
    Usage
        idealOfProjectivePoints(L,S)
    Inputs
        L:List
	   of @TO2{Point, "(projective) points"}@
	S:Ring
    Outputs
        I:Ideal
    Description
        Text
          Given a set points $X$, it returns the defining ideal of $I(X)$
        Example
            S = QQ[x,y,z] 
            X = {point{1,1,0},point{0,1,1},point{1,2,-1}}
            I = idealOfProjectivePoints(X,S)
            I2 = hadamardPower(I,2)
            X2 = hadamardPower(X,2)
            I2 == idealOfProjectivePoints(X2,S)
///




     TEST ///
     assert(point{1,2,3,4} * point{1,2,3,4}==point{1,4,9,16})

     -- may have as many TEST sections as needed
     ///

TEST /// -- point: List/Array constructors land in the Point type (Hadamard.m2:33-37,241-242)
    p = point {1,2,3};
    assert instance(p, Point);
    assert(toList p == {1,2,3});
    -- Array form, per the doc example
    pa = point [1,4,6];
    assert instance(pa, Point);
    assert(toList pa == {1,4,6});
///

TEST /// -- point: invalid inputs error (Hadamard.m2:35-36)
    -- all-zero coordinates: not a projective point
    assert(try (point {0,0,0}; false) else true);
    -- null entries are rejected
    assert(try (point {1,null,3}; false) else true);
///

TEST /// -- (symbol *, Point, Point): doc example and error paths (Hadamard.m2:40-45,198-200)
    p = point{1,2,3};
    q = point{-1,2,5};
    assert(p * q == point{-1,4,15});
    -- product with all-zero result is rejected: {1,0} * {0,1} -> {0,0}
    assert(try (point{1,0} * point{0,1}; false) else true);
    -- mismatched ambient dimension is rejected
    assert(try (point{1,2} * point{1,2,3}; false) else true);
///

TEST /// -- (symbol ==, Point, Point): projective equality is proportionality (Hadamard.m2:47-49,220-222)
    -- doc example: {1,1} == {2,2}
    assert(point{1,1} == point{2,2});
    -- proportionality, not coordinate equality
    assert(point{1,2,3} == point{2,4,6});
    assert(point{1,2,3} == point{-3,-6,-9});
    -- non-proportional points are not equal
    assert(not (point{1,1} == point{1,2}));
    assert(not (point{1,2,3} == point{1,2,4}));
///

TEST /// -- hadamardProduct(Ideal, Ideal): doc example with pinned generators (Hadamard.m2:281-284)
    S = QQ[x,y,z,t];
    IP = ideal(x-y, x-z, 2*x-t);
    IQ = ideal(x+y, x+z, x+t);
    H = hadamardProduct(IP, IQ);
    assert(ring H === S);
    assert isHomogeneous H;
    -- the doc example yields these three linear generators
    assert(H == ideal(2*z-t, 2*y-t, 2*x+t));
///

TEST /// -- hadamardProduct(List, List): doc example, output is in the same projective space (Hadamard.m2:341-343)
    L = {point{0,1}, point{1,2}};
    M = {point{1,0}, point{2,2}};
    LM = hadamardProduct(L, M);
    assert all(LM, p -> instance(p, Point));
    -- {0,1}*{1,0} = {0,0} is dropped; the remaining three pairwise products are kept
    assert(set LM === set {point{0,2}, point{1,0}, point{2,4}});
///

TEST /// -- hadamardProduct(List): list overload agrees with binary form on ideals (Hadamard.m2:136,312-316)
    S = QQ[x,y,z,t];
    IP = ideal(x-y, x-z, 2*x-t);
    IQ = ideal(x+y, x+z, x+t);
    HL = hadamardProduct{IP, IQ};
    assert(ring HL === S);
    assert(HL == hadamardProduct(IP, IQ));
    -- non-uniform list (mixed ideal+point) is rejected
    assert(try (hadamardProduct{IP, point{1,2}}; false) else true);
    -- list of unsupported type is rejected (Hadamard.m2:128)
    assert(try (hadamardProduct{1, 2}; false) else true);
///

TEST /// -- hadamardProduct(List): list overload on points (Hadamard.m2:317-318)
    P = {point{1,2,3}, point{-1,1,1}, point{1,1/2,-1/3}};
    HP = hadamardProduct P;
    assert instance(HP, Point);
    -- fold(point *) applied entrywise: (1*-1*1, 2*1*(1/2), 3*1*(-1/3)) = (-1, 1, -1)
    assert(toList HP == {-1, 1, -1});
///

TEST /// -- hadamardPower(Ideal, ZZ): r=1 is the identity; r<1 is rejected (Hadamard.m2:104-108)
    setRandomSeed 0;
    S = QQ[x,y,z,w];
    I = ideal(random(1,S), random(1,S), random(1,S));
    assert(hadamardPower(I, 1) == I);
    H2 = hadamardPower(I, 2);
    assert(ring H2 === S);
    assert isHomogeneous H2;
    -- non-positive exponent errors
    assert(try (hadamardPower(I, 0); false) else true);
    assert(try (hadamardPower(I, -3); false) else true);
///

TEST /// -- hadamardPower(List, ZZ): output is deduplicated, r<1 is rejected (Hadamard.m2:112-116)
    L = {point{1,1,1/2}, point{1,0,1}, point{1,2,4}};
    -- r=1: returns the dedup'd input
    assert(set hadamardPower(L, 1) === set L);
    H2 = hadamardPower(L, 2);
    assert all(H2, p -> instance(p, Point));
    -- 3x3 = 9 ordered pairs, two collisions on symmetric pairs and one on (1,0,1)*(1,0,1) = (1,0,1):
    -- distinct survivors = 6
    assert(length H2 == 6);
    assert(try (hadamardPower(L, 0); false) else true);
///

TEST /// -- idealOfProjectivePoints + promoted doc claim (Hadamard.m2:145-151,417-422)
    S = QQ[x,y,z];
    X = {point{1,1,0}, point{0,1,1}, point{1,2,-1}};
    I = idealOfProjectivePoints(X, S);
    assert(ring I === S);
    assert isHomogeneous I;
    -- 3 points in P^2: 1-dimensional affine cone, degree 3
    assert(dim I == 1);
    assert(degree I == 3);
    -- doc text in prose form: I2 == idealOfProjectivePoints(X2,S); promoted to a real assertion
    I2 = hadamardPower(I, 2);
    X2 = hadamardPower(X, 2);
    assert(I2 == idealOfProjectivePoints(X2, S));
    -- non-uniform input is rejected (Hadamard.m2:147-148)
    assert(try (idealOfProjectivePoints({ideal x, point{1,2}}, S); false) else true);
///

end


restart
uninstallPackage "Hadamard"
installPackage "Hadamard"
loadPackage ("Hadamard",Reload=>true)
viewHelp "Hadamard"
check "Hadamard"
