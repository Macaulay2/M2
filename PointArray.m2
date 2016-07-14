{*
PointArray
is an array of points (labelled with 0,1,...)
to which one may append new elements.

Useful methods:

pointArray      -- constructor
#               -- number of points
_               -- return points corresponding to labels
appendPoint     -- tack on a point at the end
appendPoints    -- ....... list of points ..........
position        -- find a poisiton of a point in the array (null = not there)


!!!The goal is to make searching the array fast, 
but for now it works only in linear time!!!
*}
needsPackage "NAGtypes"
PointArray = new Type of MutableHashTable
pointArray = method()
pointArray List := B -> (
    A := new PointArray from {};
    appendPoints(A,B);
    A	 
    ) 
net PointArray := P -> net values P

appendPoint = method()
appendPoint(PointArray,Point) := (A,b) -> A#(#A) = b
appendPoints = method()
appendPoints(PointArray,List) := (A,B) -> for b in B do appendPoint(A,b)

member(Point,PointArray) := (b,A) -> position(b,A) =!= null

position(Point,PointArray) := o -> (b,A) -> position(keys A, k->areEqual(A#k,b))

PointArray_List := (A,inds) -> apply(inds,i->A#i)

PointArray_ZZ := (A,i) -> A#i

TEST /// 
    restart
    needs "PointArray.m2"
    A = {{{1,3}},{{2,5}},{{0,3}},{{1+ii,3}}} /point // pointArray
    b = point {{1,3.0001}}
    c = point {{2+1e-7*ii,5.000001}}
    member(b,A)
    member(c,A)
    position(b,A)    
    position(c,A)
    A_{1,2}
///
