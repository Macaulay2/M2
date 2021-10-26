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
    Headline => "A package for the Hadamard products of projective subvarieties",
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
        consructs a projective point from the list (or array) of coordinates.
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

end


restart
uninstallPackage "Hadamard"
installPackage "Hadamard"
loadPackage ("Hadamard",Reload=>true)
viewHelp "Hadamard"
check "Hadamard"
