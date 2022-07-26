export {
    "pointArray",
    "appendPoint",
    "appendPoints",
    "PointArray"
    }

-*
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
points          -- returns a List of points

Searching the array is likely to be O(log n), which is achieved via std::map. 
(See M2/Macaulay2/e/NAG.hpp for implementation.)  
*-

debug Core

PointArrayTolerance = 1e-4

PointArray = new Type of MutableHashTable
pointArray = method(Options=>{})
pointArray List := o -> B -> (
    A := new PointArray from {};
    A#"raw" = null;
    appendPoints(A,B);
    A	 
    ) 
net PointArray := P -> net values P

length PointArray := P -> #P - 1

indices PointArray := P -> toList(0..length(P)-1) 

points PointArray := P -> P_(indices P)

appendPoint = method()
appendPoint(PointArray,AbstractPoint) := (A,b) -> (
    if A#"raw" === null then A#"raw" = rawPointArray(PointArrayTolerance,2*#coordinates b); -- 2*n real coordinates
    lookupOrAppendResult := rawPointArrayLookupOrAppend(A#"raw",raw mutableMatrix transpose sub(matrix b,CC_53),0);
    if  lookupOrAppendResult =!= length A then error "can't append";
    A#(length A) = b
    )
appendPoints = method()
appendPoints(PointArray,List) := (A,B) -> for b in B do appendPoint(A,b)

member(AbstractPoint,PointArray,FunctionClosure) := (b,A,eq) -> position(b,A,eq) =!= null
member(AbstractPoint,PointArray) := (b,A) -> member(b,A,x->x)

position(AbstractPoint,PointArray,FunctionClosure) := o -> (b, A, eq) -> (
    if A#"raw" === null then return null;
    ret := rawPointArrayLookup(A#"raw",raw mutableMatrix transpose sub(matrix b,CC_53),0);
    if ret == -1 then null else ret
    )
position(AbstractPoint,PointArray) := o -> (b, A) -> position(b, A, x -> x)

PointArray_List := (A,inds) -> apply(inds,i->A#i)

PointArray_ZZ := (A,i) -> A#i

toExternalString PointArray := A -> "pointArray " | toExternalString points A     
-- toExternalString is a bad option, since arrays might be large
File << PointArray := File => (f,A) -> ( -- TODO
    f << toExternalString A
    ) 

TEST /// 
    needsPackage "MonodromySolver"
    p = point {{1_CC,2_CC}}
    A = pointArray {p}
    assert(position(p,A)==0)    
    assert(try appendPoint(A,p) else true)
    A = {{{1,3_CC}},{{2_CC,5_CC}},{{0_CC,3_CC}},{{1+ii,3}}} /point // pointArray
    b = point {{1,3.000001_CC}}
    c = point {{2+1e-7*ii,5.000001}}
    member(b,A)
    member(c,A)
    position(b,A)    
    position(c,A)
    A_{1,2}
    p = point {{1_(CC_100),2}}
    appendPoint(A,p)        
    assert member(point{{1,2}},A)        
    value toExternalString A

    p = point {{1.79463+.302691*ii, -.379269+1.29466*ii, 2.49917+.526336*ii, 2.28917-1.3737*ii, -1.78834+.847366*ii}}
    A = pointArray {p}
    debug Core
    assert(rawPointArrayLookupOrAppend(A#"raw",raw mutableMatrix transpose matrix p,0)==0)
///
