-- -*- coding: utf-8 -*-
load "BitwiseRepresentationPolynomials.m2"
newPackage(
	"BuchbergerForBrp",
    	Version => "1.0", 
    	Date => "April 28, 2005",
    	Authors => {
	     {Name => "Jane Doe", Email => "doe@math.uiuc.edu"}
	     },
    	HomePage => "http://www.math.uiuc.edu/~doe/",
    	Headline => "an example Macaulay2 package",
	AuxiliaryFiles => false, -- set to true if package comes with auxiliary files
    	DebuggingMode => true		 -- set to true only during development
    	)

needsPackage "BitwiseRepresentationPolynomials"

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {makePairsFromLists, 
      SPolynomial, 
      runner, 
      reduceOneStep, 
      reduce,
      updatePairs,
      isReducible }
exportMutable {}


-- wrapper script for debugging purposes, creates a Groebner basis from the
-- input list - all with Brps
runner = method()
runner (MutableHashTable, ZZ) := MutableHashTable => (F,n) -> ( 
  listOfIndexPairs = makePairsFromLists( keys F, keys F) | makePairsFromLists( keys F, toList(-n..-1) );
  listOfIndexPairs = updatePairs( listOfIndexPairs, F, n );
  nextKey = #F ;
  zeroBrp = new Brp from {};

  while (#listOfIndexPairs > 0) do (
    pair = first listOfIndexPairs;
    listOfIndexPairs = delete(pair, listOfIndexPairs);
    S = SPolynomial(pair, F, n);
    reducedS = reduce (S,F);
    if (reducedS != zeroBrp ) then (
      nextKey = nextKey + 1;
      -- add reducedS to intermediate basis
      listOfIndexPairs = listOfIndexPairs | toList({-n,nextKey}..{-1, nextKey}) | apply( keys F, i-> {i,nextKey} ) ;
      F#nextKey = reducedS;
      listOfIndexPairs = updatePairs( listOfIndexPairs,F,n )
    );
  );
  --minimizeBasis(F)
  F
)

-- remove all relatively prime pairs
updatePairs = method()
updatePairs(List, HashTable, ZZ) := List => ( l, F, n) -> (
  variables = entries(id_(ZZ^n));
  select( l, pair -> (
    i = first pair;
    j = last pair;
    if ( i < 0 ) then (
      i = - i;
      f = F#j;
      g = new Brp from {variables#(i-1)}
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
SPolynomial( List, HashTable, ZZ ) := Brp => (l,G,n) -> (
  assert (#l == 2);
  i = first l;
  j = last l;
  variables = entries(id_(ZZ^n));
  if ( i < 0 ) then (-- we are working with a FP
    i = - i;
    f = G#j;
    xx = new Brp from {variables#(i-1)};
    g = new Brp from select( f, mono -> isDivisible( new Brp from {mono}, xx) == false );
    g*xx+g
  )
  else (
    f = G#i;
    g = G#j;
    leadingLcm = lcmBrps(leading(f), leading(g));
    f* (divide( leadingLcm, leading f)) + g* (divide( leadingLcm, leading g)) 
  )
)

-- Reduce the polynomial until the leading term is not divisible by the
-- leading element of any element in G
reduce = method()
reduce (Brp, HashTable) := Brp => (f,G) -> (
  zeroBrp = new Brp from {};
  while (newF = reduceOneStep(f,G); newF != f and newF != zeroBrp ) do 
    f = newF;
  newF
)
reduce (Brp, Brp) := Brp => (f,g) -> (
  reduce ( f, new HashTable from { 1=> g} )
)

-- Reduce the leading term of a polynomial one step using a polynomial
reduceOneStep = method()
reduceOneStep(Brp, Brp) := Brp => (f,g) -> (
  zeroBrp = new Brp from {};
  if f != zeroBrp then (
    assert( isReducible(f, g));
    leadingLcm =  lcmBrps(leading(f), leading(g));
    f + g * divide(leadingLcm, leading g) 
  ) else zeroBrp
)

-- reduce the leading term of a polynomial f one step by the first polynomial
-- g_i in the intermediate basis that satisfies isReducible(f,g_i)
reduceOneStep(Brp, HashTable) := Brp => (f,G) -> (
  zeroBrp = new Brp from {};
  if f != zeroBrp then (
    scan( (values G), p -> if isReducible(f, p) then (break f = reduceOneStep(f,p)));
    f
  ) else zeroBrp
)

-- Make a list with all possible pairs of elements of the separate lists, but
-- remove self-pairs 
makePairsFromLists = method()
makePairsFromLists (List,List) := List => (a,b) -> (
  ll = (apply( a, i-> apply(b, j-> if (i != j) then sort {i,j} else 0 ) ));
  unique delete(0, flatten ll)
)

-- check if the leading term of one polynomial can be reduced by another polynomial
isReducible = method()
isReducible (Brp, Brp) := Boolean => (f,g) -> (
  assert (f != new Brp from {} );
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
  assert( makePairsFromLists( {1,2,3,4,5}, {1,2,3,4,5}) ==  {{1, 2}, {1, 3}, {1, 4}, {1, 5}, {2, 3}, {2, 4}, {2, 5}, {3, 4}, {3, 5}, {4, 5}})
  assert(  makePairsFromLists( {1,2,3}, {10,100,1000}) == {{1, 10}, {1, 100}, {1, 1000}, {2, 10}, {2, 100}, {2, 1000}, {3, 10}, {3, 100}, {3, 1000}})
  assert ( makePairsFromLists( {-1,-3,-2}, {100, 10}) == {{-1, 100}, {-1, 10}, {-3, 100}, {-3, 10}, {-2, 100}, {-2, 10}} )
  assert ( makePairsFromLists ( {0,1,2}, {22} ) == {{0, 22}, {1, 22}, {2, 22}})
  
  R = ZZ[x,y,z]
  n = 3
  myPoly1 = convert( x*y + z)
  myPoly2 = convert( x )
  myPoly3 = convert( y*z + z)
  myPoly4 = convert( x*y*z + x*y + x)
  myPoly5 = convert( x*y + y*z)
  F = new HashTable from { 1 => myPoly1,
                           2 => myPoly2,
                           3 => myPoly3,
                           4 => myPoly4,
                           5 => myPoly5
                           }
  FOnePoly = new HashTable from { 1 => convert(x+y+z) } 

  S = SPolynomial({-1,1}, F, n)
  assert (S == convert( x*z + z) )
  S = SPolynomial({-1,2}, F, n)
  assert (S == new Brp from {}) 
  S = SPolynomial({1,3}, F, n)
  assert (S == convert( x*z+z) ) 
  S = SPolynomial({4,5}, F, n)
  assert (S == convert( x*y + x + y*z) ) 

  assert ( reduceOneStep( convert(x*y*z + y*z + z), convert(x+y+z) ) == convert( y*z+z))
  assert ( reduce( convert(x*y*z + y*z + z), convert(x+y+z) ) == convert( y*z+z))
  assert ( reduce( convert(x*y*z + y*z + z), FOnePoly ) == convert( y*z+z))
  
  assert ( reduceOneStep( convert(y+z), F) == convert( y+z) )


  assert ( reduceOneStep( convert(x*y*z + y*z + z), F ) == convert( y*z))
  assert ( reduce( convert(x*y*z + y*z + z), F ) == convert( z))

  l = makePairsFromLists( keys F, keys F) 
  assert( l == {{1, 2}, {1, 3}, {1, 4},
  {1, 5}, {2, 3}, {2, 4}, {2, 5}, {3, 4}, {3, 5}, {4, 5}} )

  assert ( updatePairs (l, F, n) == {{1, 2}, {1, 3}, {1, 4}, {1, 5}, {2, 4},
  {2, 5}, {3, 4}, {3, 5}, {4, 5}} )

  ll = makePairsFromLists( keys F, {-1} )
  assert ( updatePairs( ll, F, n) == {{-1, 1}, {-1, 2}, {-1, 4}, {-1, 5}} )
  
  lll = makePairsFromLists( keys F, {-3} )
  assert ( updatePairs( lll, F, n) == {{-3,3}, {-3,4}} )
  
  R = ZZ/2[x,y,z];
  n=3;
  F = new MutableHashTable from { 1 => convert x }
  assert ( first values runner(F,n) == new Brp from {{1, 0, 0}} )
  F = new MutableHashTable from { 1 => convert x,
                                  2 => convert y}
  runner(F,n)                                
  assert ( first values runner(F,n) == new Brp from {{1, 0, 0}} )
  F = new MutableHashTable from { 1 => convert (x*y),
                                  2 => convert y}
  runner(F,n)                                

  R = ZZ/2[x,y,z];
  n=3;
  F = new MutableHashTable from { 1 => convert (x*y+z) }
  assert(flatten flatten values runner(F,n) == {1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1})


-- R = ZZ/2[x,y,z]/ideal(x*y+z)
-- i11 : gens gb ideal(x*y+z)

-- o11 = | yz+z xz+z xy+z |

  myPoly1 = new Brp from {{1,0,0}};
  myPoly2 = new Brp from {{1,0,1}};
  myPoly3 = new Brp from {{1,1,0}, {0,0,1}};
  -- list of input polynomials
  F = new MutableHashTable from {1 => myPoly1,
                          2 => myPoly2,
                          3 => myPoly3};
///
  
       
end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
installPackage "BuchbergerForBrp"
installPackage("BuchbergerForBrp", RemakeAllDocumentation=>true)
check BuchbergerForBrp
