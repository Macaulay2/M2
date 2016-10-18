export {
    "pointArray",
    "appendPoint",
    "appendPoints",
    "PointArray"
    }

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
points          -- returns a List of points

!!!The goal is to make searching the array fast, 
but for now it works only in linear time!!!
*}

PointArrayTolerance = 1e-4
FAST = class rawPointArray === CompiledFunction
if not FAST then (
    export {"rawPointArray","rawPointArrayLookupOrAppend","rawPointArrayLookup"} 
    )

-- needsPackage "NAGtypes"
PointArray = new Type of MutableHashTable
pointArray = method()
pointArray List := B -> (
    A := new PointArray from {};
    if FAST then A#"raw" = null;
    appendPoints(A,B);
    A	 
    ) 
net PointArray := P -> net values P

length PointArray := P -> if FAST then #P - 1 else #P

indices PointArray := P -> toList(0..length(P)-1) 

points PointArray := P -> if FAST then P_(indices P) else values P

appendPoint = method()
appendPoint(PointArray,Point) := (A,b) -> (
    if FAST then (
	if A#"raw" === null then A#"raw" = rawPointArray(PointArrayTolerance,2*#coordinates b); -- 2*n real coordinates
	if rawPointArrayLookupOrAppend(A#"raw",raw mutableMatrix transpose matrix b,0) =!= length A 
    	then error "can't append"
	);
    A#(length A) = b
    )
appendPoints = method()
appendPoints(PointArray,List) := (A,B) -> for b in B do appendPoint(A,b)

member(Point,PointArray) := (b,A) -> position(b,A) =!= null

position(Point,PointArray) := o -> (b,A) -> 
    if FAST then (
	if A#"raw" === null then return null;
	ret := rawPointArrayLookup(A#"raw",raw mutableMatrix transpose matrix b,0);
	if ret == -1 then null else ret
	) else position(keys A, k->areEqual(A#k,b,Tolerance => PointArrayTolerance))
    
PointArray_List := (A,inds) -> apply(inds,i->A#i)

PointArray_ZZ := (A,i) -> A#i

--This is inefficient, but it works for now.
positions(PointArray, Function) := (A, f) -> (
    error "this function is probably not needed...";
    select(keys A,k->f(A#k))
    );

TEST /// 
    restart
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
    p = point {{1.79463+.302691*ii, -.379269+1.29466*ii, 2.49917+.526336*ii, 2.28917-1.3737*ii, -1.78834+.847366*ii}}
    A = pointArray {p}
    rawPointArrayLookupOrAppend(A#"raw",raw mutableMatrix transpose matrix p,0)
///
