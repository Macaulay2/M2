-- -*- coding: utf-8 -*-
load "BitwiseRepresentationPolynomials.m2"
newPackage(
	"BuchbergerForBrp",
    	Version => "0.0", 
    	Date => "January 11,2010",
    	Authors => {
	     {Name => "Franziska Hinkelmann", Email => "fhinkel@vt.edu"}
	     },
    	HomePage => "http://www.math.vt.edu/people/fhinkel",
    	Headline => "compute a Boolean Groebner Basis using a bit-wise representation",
      AuxiliaryFiles => false, -- set to true if package comes with auxiliary files
    	DebuggingMode => true		 -- set to true only during development
    	)

needsPackage "BitwiseRepresentationPolynomials"

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {makePairsFromLists, 
      SPolynomial, 
      gbBrp, 
      reduceOneStep, 
      reduceLtBrp,
      reduce,
      updatePairs,
      gbComputation,
      minimalGbBrp,
      isReducible }
exportMutable {}

-- keys should start with 0
gbComputation = new Type of MutableHashTable; 

-- generate the unit vectors representing x_i
unitvector = memoize((i,n) -> ( apply(n,j -> if i === j then 1 else 0)));

-- wrapper script for debugging purposes, creates a Groebner basis from the
-- input list - all with Brps
gbBrp = method()
gbBrp (gbComputation, ZZ) := gbComputation => (F,n) -> ( 
  listOfIndexPairs := makePairsFromLists( keys F, keys F) | makePairsFromLists( keys F, toList(-n..-1) );
  listOfIndexPairs = updatePairs( listOfIndexPairs, F, n );
  while #listOfIndexPairs > 0 do (
    pair := first listOfIndexPairs;
    listOfIndexPairs = delete(pair, listOfIndexPairs); -- very slow, order n^2
    S := SPolynomial(pair, F, n);
    reducedS := reduce (S,F);
    if reducedS != 0 then (
      -- add reducedS to intermediate basis
      listOfIndexPairs = listOfIndexPairs | toList((-n,#F)..(-1, #F)) | apply( keys F, i-> (i,#F) ) ;
      F##F = reducedS;
      listOfIndexPairs = updatePairs( listOfIndexPairs,F,n )
    );
  );
  --minimizeBasis(F)
  F
)



-- delete elements where the leading term is divisible by another leading term
minimalGbBrp = method()
minimalGbBrp( gbComputation ) := gbComputation => (F) -> (
  -- Todo remove extra looping, we want to scan over the "changing" values F
  scan( values F, f -> ( print "starting with"; print f; scan( pairs F, (gKey, g) -> (print gKey; if f != g and isReducible( g, f) then remove(F,gKey) ))) )
)



--Reduce lower terms of the first polynomial with the leading term of the second

reduceLtBrp = method()
reduceLtBrp(Brp, Brp) := Brp => (f,g) -> (
  while (l= select(f, m ->  isReducible(new Brp from {m}, leading(g)));  #l!=0) do (
   	  f= f+ divide(new Brp from {first l}, leading(g))*g
  );
  f
)

-- Reduce lower terms of intermediate GB by leading terms of other polynomials
reduceGbBrp = method()
reduceGbBrp( gbComputation ) := gbComputation => F -> (
  scan( pairs F, (fKey,f) -> ( print "starting with"; print f; scan(values F, g ->( F#fKey = if f==g then f else reduceLtBrp(f,g); print F#fKey ))) )
)

 R = ZZ[x,y,z]
  F = new gbComputation from { 0 => convert( x*y+y*z + z),
                           1 => convert(y*z+z ) ,
                           2 => convert(x*z+z)
                          
                           }
reduceGbBrp(F)


-- remove all relatively prime pairs
updatePairs = method()
updatePairs(List, HashTable, ZZ) := List => ( l, F, n) -> (
  select( l, (i,j) -> (
    if i < 0 then (
      i = - i;
      f := F#j;
      g := new Brp from {unitvector( i-1,n)}
    ) 
    else (
      f = F#i;
      g = F#j
    );
    not isRelativelyPrime(leading f, leading g)
  )
  )
)

  
-- from pair of indices get corresponding polynomials, then compute their S
-- polynomial
-- assume that the pairs are good (i.e., leading terms not relatively prime)
SPolynomial = method()
SPolynomial( Sequence, HashTable, ZZ ) := Brp => (pair,G,n) -> (
  (i,j) := pair;
  if i < 0 then ( -- we are working with an FP
    i = - i;
    f := G#j;
    xx := new Brp from {unitvector( i-1,n)};
    g := new Brp from select( f, mono -> isDivisible( new Brp from {mono}, xx) == false );
    g*xx+g
  )
  else (
    f = G#i;
    g = G#j;
    leadingLcm := lcmBrps(leading(f), leading(g));
    f* (divide( leadingLcm, leading f)) + g* (divide( leadingLcm, leading g)) 
  )
)

-- Reduce the polynomial until the leading term is not divisible by the
-- leading element of any element in G
reduce = method()
reduce (Brp, HashTable) := Brp => (f,G) -> (
  while (newF = reduceOneStep(f,G); newF != f and newF != 0) do 
    f = newF;
  newF
)

reduce (Brp, Brp) := Brp => (f,g) -> (
  reduce ( f, new HashTable from { 1=> g} )
)

-- Reduce the leading term of a polynomial one step using a polynomial
reduceOneStep = method()
reduceOneStep(Brp, Brp) := Brp => (f,g) -> (
  if f != 0 then (
    assert( isReducible(f, g));
    leadingLcm :=  lcmBrps(leading(f), leading(g));
    f + g * divide(leadingLcm, leading g) 
  ) else new Brp from {} -- TODO make 0 automatically turn into 0
)

-- reduce the leading term of a polynomial f one step by the first polynomial
-- g_i in the intermediate basis that satisfies isReducible(f,g_i)
reduceOneStep(Brp, HashTable) := Brp => (f,G) -> (
  if f != 0 then (
    scan( (values G), p -> if isReducible(f, p) then (break f = reduceOneStep(f,p)));
    f
  ) else new Brp from {} 
)

-- Make a list with all possible pairs of elements of the separate lists, but
-- remove self-pairs 
makePairsFromLists = method()
makePairsFromLists (List,List) := List => (a,b) -> (
  ll := (apply( a, i-> apply(b, j-> if i != j then toSequence sort {i,j} else 0 ) ));
  unique delete(0, flatten ll)
)

-- check if the leading term of one polynomial can be reduced by another polynomial
isReducible = method()
isReducible (Brp, Brp) := Boolean => (f,g) -> (
  assert (f != 0 );
  isDivisible(leading f, leading g)
)

--doc ///
--key 
--  BuchbergerForBrp
--Headline
--  BuchbergerForBrp making use of bit-wise representation
--///
beginDocumentation()
document { 
	Key => BuchbergerForBrp,
	Headline => "BuchbergerForBrp making use of bit-wise representation",
	EM "PackageTemplate", " is an example package which can
	be used as a template for user packages."
	}
TEST ///
  assert( makePairsFromLists( {1,2,3,4,5}, {1,2,3,4,5}) ==  {(1, 2), (1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5), (3, 4), (3, 5), (4, 5)})
  assert(  makePairsFromLists( {1,2,3}, {10,100,1000}) == {(1, 10), (1, 100), (1, 1000), (2, 10), (2, 100), (2, 1000), (3, 10), (3, 100), (3, 1000)})
  assert ( makePairsFromLists( {-1,-3,-2}, {100, 10}) == {(-1, 100), (-1, 10), (-3, 100), (-3, 10), (-2, 100), (-2, 10)} )
  assert ( makePairsFromLists ( {0,1,2}, {22} ) == {(0, 22), (1, 22), (2, 22)})
  
  R = ZZ[x,y,z]
  myPoly1 = convert( x*y + z)
  myPoly2 = convert( x )
  myPoly3 = convert( y*z + z)
  myPoly4 = convert( x*y*z + x*y + x)
  myPoly5 = convert( x*y + y*z)
  F = new gbComputation from { 1 => myPoly1,
                           2 => myPoly2,
                           3 => myPoly3,
                           4 => myPoly4,
                           5 => myPoly5
                           }
  FOnePoly = new gbComputation from { 1 => convert(x+y+z) } 

  S = SPolynomial((-1,1), F, numgens R)
  assert (S == convert( x*z + z) )
  S = SPolynomial((-1,2), F, numgens R)
  assert (S == 0 )
  S = SPolynomial((1,3), F, numgens R)
  assert (S == convert( x*z+z) ) 
  S = SPolynomial((4,5), F, numgens R)
  assert (S == convert( x*y + x + y*z) ) 

  assert ( reduceOneStep( convert(x*y*z + y*z + z), convert(x+y+z) ) == convert( y*z+z))
  assert ( reduce( convert(x*y*z + y*z + z), convert(x+y+z) ) == convert( y*z+z))
  assert ( reduce( convert(x*y*z + y*z + z), FOnePoly ) == convert( y*z+z))
  assert ( reduceOneStep( convert(y+z), F) == convert( y+z) )
  assert ( reduceOneStep( convert(x*y*z + y*z + z), F ) == convert( y*z))
  assert ( reduce( convert(x*y*z + y*z + z), F ) == convert( z))

  l = makePairsFromLists( keys F, keys F) 
  assert( l == {(1, 2), (1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5), (3, 4), (3, 5), (4,5)})
  assert ( updatePairs (l, F, numgens R) == {(1, 2), (1, 3), (1, 4), (1, 5), (2, 4), (2, 5), (3, 4), (3, 5), (4, 5)})
  ll = makePairsFromLists( keys F, {-1} )
  assert ( updatePairs( ll, F, numgens R) ==  {(-1, 1), (-1, 2), (-1, 4), (-1, 5)} )
  lll = makePairsFromLists( keys F, {-3} )
  assert ( updatePairs( lll, F, numgens R) == {(-3, 3), (-3, 4)} )
  
  R = ZZ/2[x,y,z];
  F = new gbComputation from { 0 => convert x }
  assert ( first values gbBrp(F,numgens R) == new Brp from {{1, 0, 0}} )
  F = new gbComputation from { 0 => convert x,
                                  1 => convert y}
  gbBrp(F,numgens R)                                
  assert ( first values gbBrp(F,numgens R) == new Brp from {{1, 0, 0}} )
  F = new gbComputation from { 0 => convert (x*y),
                                  1 => convert y}
  gbBrp(F,numgens R)                                

  R = ZZ/2[x,y,z];
  F = new gbComputation from { 0 => convert (x*y+z) }
  gbBrp(F,numgens R)
  assert(flatten flatten values gbBrp(F, numgens R) == {1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1})


-- R = ZZ/2[x,y,z]/ideal(x*y+z)
-- i11 : gens gb ideal(x*y+z)

-- o11 = | yz+z xz+z xy+z |

  myPoly1 = new Brp from {{1,0,0}};
  myPoly2 = new Brp from {{1,0,1}};
  myPoly3 = new Brp from {{1,1,0}, {0,0,1}};
  -- list of input polynomials
  F = new MutableHashTable from {0 => myPoly1,
                          1 => myPoly2,
                          2 => myPoly3};

 -- R = ZZ/2[x,y,z,w,MonomialOrder=>Lex]/(x^2+x,y^2+y,z^2+z,w^2+w)
 -- J = ideal(x*y*w+w*x+z, x*z+w*y)
 -- gens gb J

  R = ZZ/2[x,y,z,w]
  F = new gbComputation from { 0 => convert(x*y*w+w*x+z),
                              1 => convert (x*z+w*y) }

  gbBasis = gbBrp(F,numgens R)

  assert( apply (values gbBasis, poly -> convert(poly,R) ) == {x*y*w + x*w + z, x*z + y*w, z, y*w + z, x*w + y*w + z})


  R = ZZ[x,y,z]
  F = new gbComputation from { 0 => convert( x*y + z),
                           1 => convert( x ) ,
                           2 => convert( y*z + z),
                           3 => convert( x*y*z + x*y + x) ,
                           4 => convert( x*y + y*z)
                           }
R = ZZ[x,y,z]
  myPoly1 = convert( x*y + z)
  myPoly2 = convert( x )
  myPoly3 = convert( y*z + z)
  myPoly4 = convert( x*y*z + x*y + x)
  myPoly5 = convert( x*y + y*z)
  F = new gbComputation from { 1 => myPoly1,
                           2 => myPoly2,
                           3 => myPoly3,
                           4 => myPoly4,
                           5 => myPoly5
                           }
  FOnePoly = new gbComputation from { 1 => convert(x+y+z) } 

minimalGbBrp(F)

peek F
assert ( #F == 2 ) 
assert (F#2 == new Brp from {{1, 0, 0}} )
assert (F#3 == new Brp from {{0, 1, 1}, {0, 0, 1}} )

R = ZZ/2[x,y,z]
a = convert(x*z + y*z + z)
b = convert(y+z)
assert( reduceLtBrp(a,b) == new Brp from {{1, 0, 1}, {0, 0, 1}} )

a = convert(x*z + y*z + y)
b = convert(y+z)
assert( reduceLtBrp(a,b) == new Brp from {{1, 0, 1}} )
a = convert(x*z + y*z + y + z)
assert( reduceLtBrp(a,b) == new Brp from {{1, 0, 1}, {0,0,1}} )

a = convert(x*y + y*z +z)
b= convert(y*z +z)
reduceLtBrp(a,b)
assert( reduceLtBrp(a,b)== new Brp from {{1, 1, 0}, {0, 0, 1}})
///
  
       
end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
installPackage "BuchbergerForBrp"
installPackage("BuchbergerForBrp", RemakeAllDocumentation=>true)
check BuchbergerForBrp
