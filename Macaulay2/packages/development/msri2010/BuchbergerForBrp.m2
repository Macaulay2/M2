-- -*- coding: utf-8 -*-
load "BitwiseRepresentationPolynomials.m2"
newPackage(
	"BuchbergerForBrp",
    	Version => "0.0", 
    	Date => "January 11,2010",
    	Authors => {
	     {Name => "Franziska Hinkelmann, Elizabeth Arnold, Samuel S", Email => "fhinkel@vt.edu"}
	     },
    	HomePage => "http://www.math.vt.edu/people/fhinkel",
    	Headline => "compute a Boolean Groebner Basis using a bit-wise representation",
      AuxiliaryFiles => false, -- set to true if package comes with auxiliary files
    	DebuggingMode => true		 -- set to true only during development
    	)

needsPackage "BitwiseRepresentationPolynomials"

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
exportMutable {
      makePairsFromLists, 
      gbComputation,
      isReducible,
      minimalGbBrp,
      reduce,
      reduceGbBrp,
      reduceLtBrp,
      reduceOneStep, 
      reduceOneStepList, 
      removeDoubleEntries,
      SPolynomial, 
      updatePairs,
      gbBrp,
      scanProfile,
      applyProfile
      }
      
export {
      getPolysFromList,
      inList
      }

scanProfile = method();
scanProfile = scan;

applyProfile = method();
applyProfile = applyValues;

-- keys should start with 0
gbComputation = new Type of MutableHashTable; 

-- generate the unit vectors representing x_i
unitvector = memoize((i,n) -> ( apply(n,j -> if i === j then 1 else 0)));

-- wrapper script for debugging purposes, creates a Groebner basis from the
-- input list - all with Brps
gbBrp = method()
gbBrp (gbComputation, ZZ) := gbComputation => (F,n) -> ( 
  removeDoubleEntries F;
  reduceGbBrp F;
  listOfIndexPairs := makePairsFromLists( keys F, keys F) | makePairsFromLists( keys F, toList(-n..-1) );
  --listOfIndexPairs = updatePairs( listOfIndexPairs, F, n );
  --listOfIndexPairs = updatePairsFast( listOfIndexPairs, F, n );
  while #listOfIndexPairs > 0 do (
    pair := first listOfIndexPairs;
    listOfIndexPairs = delete(pair, listOfIndexPairs); -- very slow, order n^2
    if isGoodPair(pair, F, listOfIndexPairs, n) then (
      S := SPolynomial(pair, F, n);
      reducedS := reduce (S,F);
      if reducedS != 0 then (
        -- add reducedS to intermediate basis and update the list of pairs
        newPairs = toList( (-n,#F)..(-1, #F)) | apply( keys F, i-> (i,#F) );
        F##F = reducedS;
        --newPairs = updatePairsFast( newPairs, F, n );
        listOfIndexPairs = listOfIndexPairs | newPairs
      );
    );
  );
  reduceGbBrp F;
  F
)

-- return true if the Spolynomail for this pair needs to be computed
-- input: * a Sequence (i,j)
--        * an intermediate Groebner basis
--        * the remaining list of pairs
--        * n, number of variables in ring
-- a pair (i,j) is good if both
--        * not relatively prime
--        * for all k, (i,k) or (j,k) must still be in the
--            list, or leading term of k does not divide lcm(i,j)
isGoodPair = method()
isGoodPair(Sequence, gbComputation, List,ZZ) := Boolean => ( pair, F, l, n) -> (
  assert (#pair == 2);
  (i,j) := pair;
  if not F#?j or (i >= 0 and not F#?i) then false 
  else ( 
    if i < 0 then g := new Brp from {unitvector( -i-1,n)}
    else g = F#i;
    f := F#j;
    if isRelativelyPrime(leading f, leading g) then false
    else (
      lcmPair := lcmBrps(leading f, leading g);
      not any( pairs F, (hKey, h)-> hKey != i and hKey != j and isDivisible(lcmPair, leading h) and not inList(i,hKey,l) and not inList(j,hKey,l) )
    )
  )
)

-- remove polynomials that appear twice in the ideal
removeDoubleEntries = method()
removeDoubleEntries(gbComputation) := gbComputation => F -> (
  scan( pairs F, (fKey,f) -> 
    scan( pairs F, (gKey,g) -> if fKey != gKey and f == g then remove(F,fKey)) 
  );
  F
)




--Take polynomials from a list and make into a gbComputation
getPolysFromList = method()
getPolysFromList(List) := gbComputation => S -> (
  F:= new gbComputation;
  scan(S, i -> F##F = convert(i));
  F
)
    

-- delete elements where the leading term is divisible by another leading term
minimalGbBrp = method()
minimalGbBrp( gbComputation ) := gbComputation => (F) -> (
  -- Todo remove extra looping, we want to scan over the "changing" values F
  notFinished = true;  
  while ( notFinished ) do (
    notFinished = false;
    resetF = false;
    scan( pairs F, (fKey, f) -> (
      scan( pairs F, (gKey, g) -> (
        if fKey =!= gKey and isReducible( g, f) then (
          remove(F,gKey); 
          resetF = true
        )
        )
      ) ;
      if resetF then (
        notFinished = true;
        break
      )
     )
    );
  );
  F
)


--Reduce all terms of the first polynomial with the leading term of the second
reduceLtBrp = method()
reduceLtBrp(Brp, Brp) := Brp => (f,g) -> (
  while ( p := scan(f, m ->  (m = new Brp from {m}; if isReducible(m, leading g) then break m)); instance(p,Brp) ) do (
      --assert isDivisible( p, leading g);
   	  f = f + g*divide( p, leading g)
  );
  f
)

-- Reduce allterms of intermediate GB by leading terms of other polynomials
reduceGbBrp = method()
reduceGbBrp( gbComputation ) := gbComputation => F -> (
  changesHappened := true;
  while changesHappened do (
    changesHappened = false;
    N = sort apply (values F, p -> convert(p,R) );
    print N;
    scan( pairs F, (fKey,f) -> ( 
      N = sort apply (values F, p -> convert(p,R) );
      print N;
      scan( pairs F, (gKey,g) ->
        if fKey != gKey then (
          tmpF := reduceLtBrp(f,g);
          if f !=  tmpF then (
            if tmpF != 0 then F#fKey = tmpF else remove(F,fKey);
            changesHappened = true;
            break
          )
        )
      )
     )
    )
  );
  F
)

-- remove all relatively prime pairs
-- updatePairs returns a list of all the pairs, that have to be used to compute S polynomials
updatePairs = method()
updatePairs(List, gbComputation, ZZ) := List => ( l, F, n) -> (
  select(l, (i,j) -> (
    if not F#?j or (i >= 0 and not F#?i) then false 
    else ( 
      if i < 0 then g := new Brp from {unitvector( -i-1,n)}
      else g = F#i;
      f := F#j;
      not isRelativelyPrime(leading f, leading g) 
    )
  ))
)

-- updatePairs returns a list of all the pairs, that have to be used to compute S polynomials
-- remove all relatively prime pairs, and throw out all pairs (i,j), if there is an h in G, with 
-- lcm(i,j) is divisible by leading h
-- this must be run on a intermediate basis with no double entries. 
updatePairsFast = method()
updatePairsFast(List, gbComputation, ZZ) := List => ( l, F, n) -> (
  select(l, (i,j) -> (
    if not F#?j or (i >= 0 and not F#?i) then false 
    else ( 
      if i < 0 then g := new Brp from {unitvector( -i-1,n)}
      else g = F#i;
      f := F#j;
      if isRelativelyPrime(leading f, leading g) then false
      else (
        lcmPair := lcmBrps(leading f, leading g);
        --not any( pairs F, (hKey, h) -> hKey != i and hKey != j and isDivisible(lcmPair, leading h))
        not any( pairs F, (hKey, h)-> hKey != i and hKey != j and isDivisible(lcmPair, leading h) and not inList(i,hKey,l) and not inList(j,hKey,l) )
      )
    )
  ))
)

-- use keys of f and h and check if (f,h) is in the list
inList= method()
inList( ZZ, ZZ, List) := Boolean => (fKey,hKey,l) -> (
  if fKey < hKey then 
    pair := (fKey, hKey) 
  else 
    pair = (hKey, fKey);
  any(l, p-> p == pair) -- true if it is in the list
)

-- from pair of indices get corresponding polynomials, then compute their S
-- polynomial
-- assume that the pairs are good (i.e., leading terms not relatively prime)
SPolynomial = method()
SPolynomial( Sequence, gbComputation, ZZ ) := Brp => (pair,G,n) -> (
  (i,j) := pair;
  if i < 0 then ( -- we are working with an FP
    if G#?j then (
      f := G#j;
      i = - i;
      xx := new Brp from {unitvector( i-1,n)};
      g := new Brp from select( f, mono -> isDivisible( new Brp from {mono}, xx) == false );
      g*xx+g
    ) else new Brp from {}
  )
  else (
    -- check if keys i,j are really in G, otherwise return 0
    if G#?i and G#?j then (
      g = G#i;
      f = G#j;
      leadingLcm := lcmBrps(leading(f), leading(g));
      f* (divide( leadingLcm, leading f)) + g* (divide( leadingLcm, leading g)) 
    ) else new Brp from {}
  )
)

-- Reduce the polynomial until the leading term is not divisible by the
-- leading element of any element in G
-- f could be 0
reduce = method()
reduce (Brp, gbComputation) := Brp => (f,G) -> (
  fullyReducedFlag := false;
  while (not fullyReducedFlag) do (
    (f, fullyReducedFlag) = reduceOneStepList(f,G)
  );
  f
)

-- 
reduce (Brp, Brp) := Brp => (f,g) -> (
  reduce ( f, new gbComputation from { 1=> g} )
)

-- Reduce the leading term of a polynomial one step using a polynomial
-- only call this when isReducible(f,g)
reduceOneStep = method()
reduceOneStep(Brp, Brp) := Brp => (f,g) -> (
  if f != 0 then (
    f + g * divide(leading f, leading g) 
  ) else new Brp from {} -- TODO make 0 automatically turn into 0
)

-- reduce the leading term of a polynomial f one step by the first polynomial
-- g_i in the intermediate basis that satisfies isReducible(f,g_i)
reduceOneStepList = method()
reduceOneStepList(Brp, gbComputation) := (Brp, Boolean) => (f, G) -> (
  if f != 0 then (
    ret := true;
    scanProfile( values G, g -> if isReducible(f, g) then ( f = reduceOneStep(f,g); ret = false; break) );
    (f,ret)
  ) 
  else ( new Brp from {}, true)
)

-- Make a list with all possible pairs of elements of the separate lists, but
-- remove self-pairs 
makePairsFromLists = method()
makePairsFromLists (List,List) := List => (a,b) -> (
  l := (apply( a, i-> apply(b, j-> if i != j then toSequence sort {i,j} else 0 ) ));
  unique delete(0, flatten l)
)

-- check if the leading term of one polynomial can be reduced by another polynomial
isReducible = method()
isReducible (Brp, Brp) := Boolean => (f,g) -> (
  --assert (f != 0 );
  isDivisible(leading f, leading g)
)

doc ///
Key 
  BuchbergerForBrp
Headline
  BuchbergerForBrp making use of bit-wise representation
///

doc ///
Key 
  (updatePairs,List, gbComputation, ZZ) 
  updatePairs
Headline
  update a list of indices for good indices
///

doc ///
Key 
  (reduceLtBrp,Brp,Brp)
  reduceLtBrp
Headline
  reduce polynomial by the leading term of another
///

doc ///
Key 
  gbComputation
Headline
  MutableHashTable for an intermediate Groebner basis
///

doc ///
Key 
  (minimalGbBrp,gbComputation)
  minimalGbBrp
Headline
  delete elements where the leading term is divisible by another leading term
///

doc ///
Key 
  (isReducible,Brp,Brp)
  isReducible
Headline
  check if the leading term of one polynomial can be reduced by another polynomial
///

doc ///
Key 
  (SPolynomial, Sequence, gbComputation, ZZ)
  SPolynomial
Headline
  from pair of indices get corresponding polynomials, then compute their S polynomial assume that the pairs are good (i.e., leading terms not relatively prime)
///

doc ///
Key
  (makePairsFromLists,List,List)
  makePairsFromLists
Headline
  Make a list with all possible pairs of elements of the separate lists, but remove self-pairs 
///

doc ///
Key
  --(reduceOneStep,Brp,List)
  (reduceOneStep,Brp,Brp)
  reduceOneStep
Headline
  Reduce the leading term of a polynomial one step using a polynomial
///

doc ///
Key
  (gbBrp,gbComputation,ZZ)
  gbBrp
Headline
  wrapper script for debugging purposes, creates a Groebner basis from the input list - all with Brps
///

doc ///
Key 
  (reduce,Brp,gbComputation)
  (reduce,Brp,Brp)
  reduce
Headline
  Reduce the polynomial until the leading term is not divisible by the leading element of any element in G
Usage
  g=reduce(f,F)
Inputs 
  f:Brp
    a polynomial
  F:gbComputation
    a list of polynomials
Outputs
  g:Brp
    f reduced by F 
///

TEST ///

  R = ZZ/2[x,y,z]
  F = new gbComputation from { 0=>convert(x*y), 
                                1=>convert(x*y)
                              }
  removeDoubleEntries F
  N = sort apply (values F, poly -> convert(poly,R) )
  assert( N == {x*y} )

  F = new gbComputation from { 0=>convert(x*y), 
                                1=>convert(x*y),
                                2=>convert(x*y)
                              }
  removeDoubleEntries F
  N = sort apply (values F, poly -> convert(poly,R) )
  assert( N == {x*y} )

  F = new gbComputation from { 0=>convert(x*y), 
                                1=>convert(x*y+x),
                                2=>convert(x*y)
                              }
  removeDoubleEntries F
  N = sort apply (values F, poly -> convert(poly,R) )
  assert( N == {x*y, x*y+x} )

  F = new gbComputation from { 0=>convert(x*y), 
                                1=>convert(x*y+x),
                                2=>convert(x*y),
                                3=>convert(x*y+x+z),
                                4=>convert(x*y+x)
                              }
  removeDoubleEntries F
  N = sort apply (values F, poly -> convert(poly,R) )
  assert( N == {x*y, x*y+x, x*y+x+z} )

  l = {(1,2), (1,3), (-2,3), (-5,3)}
  assert inList( 1, 2, l)
  assert inList( 2, 1, l)
  assert inList( 3, -2, l)
  assert not inList(1,-2, l)
  assert not inList(1,5, l)
  
  longTest = false
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
  assert ( reduceOneStepList( convert(y+z), F) == (convert( y+z), true) )
  assert ( reduceOneStepList( convert(x*y*z + y*z + z), F ) == (convert( y*z), false))
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

  R = ZZ/2[a..j, MonomialOrder=>Lex]
 I = ideal (a^2+a,
            b^2+b,
            c^2+c,
            d^2+d,
            e^2+e,
            f^2+f,
            g^2+g,
            h^2+h,
            i^2+i,
            j^2+j)
  J = ideal(a*b*c*d*e, a+b*c+d*e+a+b+c+d, j*h+i+f, g+f, a+d, j+i+d*c)
  J = J + I

  R = ZZ/2[a..j, MonomialOrder=>Lex]
  F = new gbComputation from { 0=> convert(a*b*c*d*e),
          1=> convert( a+b*c+d*e+a+b+c+d),
          2=> convert( j*h+i+f),
          3=> convert( g+f),
          4=> convert( a+d),
          5=> convert( j+i+d*c)
          }
  gbBasis = gbBrp( F, numgens R)
  N = sort apply (values gbBasis, poly -> convert(poly,R) )
  QR = R/(a^2+a,
          b^2+b,
          c^2+c,
          d^2+d,
          e^2+e,
          f^2+f,
          g^2+g,
          h^2+h,
          i^2+i,
          j^2+j)
  J = ideal(a*b*c*d*e, a+b*c+d*e+a+b+c+d, j*h+i+f, g+f, a+d, j+i+d*c)
  M = apply(flatten entries gens gb J, i-> lift(i,R))
  assert(N == M)

  R = ZZ/2[a..t, MonomialOrder=>Lex]
  F = new gbComputation from { 0=> convert(a*b*c*d*e),
          1=> convert( a+b*c+d*e+a+b+c+d),
          2=> convert( j*h+i+f),
          3=> convert( g+f),
          4=> convert( a+d),
          5=> convert( j+i+d*c),
          6=> convert( r+s+t),
          7=> convert( m*n+o*p),
          8=> convert( t+a),
          9=> convert( b*s+q+p*n*m+i),
          10=> convert( b*s+q+p+h),
          11=> convert( b*s+q*n*m+i),
          12=> convert( b*s+q*n*m+i+j*s*t +s),
          13=> convert( b*k+s+t),
          14=> convert( b*k+r*q+l*m+i*j+n),
          15=> convert( b*a+l+q*m+i),
          16=> convert( b*k+d*n*m+i),
          17=> convert( b+q+l*n*m+i*d),
          18=> convert( a*k+c*l*n*f),
          19=> convert( q*r+c+q+l*n*m+i)
          }
longTest = false
  if longTest then          
    time gbBasis = gbBrp( F, numgens R)
  N = sort apply (values gbBasis, poly -> convert(poly,R) )

  QR = R/(a^2+a, b^2+b, c^2+c, d^2+d, e^2+e, f^2+f, g^2+g, h^2+h, i^2+i, j^2+j, k^2+k, l^2+l, m^2+m, n^2+n, o^2+o, p^2+p, q^2+q, r^2+r, s^2+s, t^2+t)
  J = ideal(a*b*c*d*e,a+b*c+d*e+a+b+c+d, j*h+i+f, g+f,a+d,j+i+d*c, r+s+t, m*n+o*p, t+a, b*s+q+p*n*m+i,  b*s+q+p+h, b*s+q*n*m+i, b*k+q+l*n*m+i, b*s+q*n*m+i, b*s+q*n*m+i+j*s*t+s, b*k+s+t, b*k+r*q+l*m+i*j+n, b*a+l+q*m+i, b*k+d*n*m+i, b+q+l*n*m+i*d, a*k+c*l*n*f, q*r+c+q+l*n*m+i)
  time M = apply(flatten entries gens gb J, i-> lift(i,R))
if longTest then          
  assert(N == M)
  

longTest = true
  R = ZZ/2[a..t, MonomialOrder=>Lex]
  F = new gbComputation from { 0=> convert(a*b*c*d*e),
          1=> convert( a+b*c+d*e+a+b+c+d),
          2=> convert( j*h+i+f),
          3=> convert( g+f),
          4=> convert( a+d),
          5=> convert( j+i+d*c),
          6=> convert( r+s+t),
          7=> convert( m*n+o*p),
          8=> convert( t+a),
          9=> convert( b*s+q+p*n*m+i),
          10=> convert( b*s+q+p+h),
          11=> convert( b*s+q*n*m+i),
          12=> convert( b*k+q+l*n*m+i)
          }
if longTest then          
  time gbBasis = gbBrp( F, numgens R)
  N = sort apply (values gbBasis, poly -> convert(poly,R) )

  QR = R/(a^2+a, b^2+b, c^2+c, d^2+d, e^2+e, f^2+f, g^2+g, h^2+h, i^2+i, j^2+j, k^2+k, l^2+l, m^2+m, n^2+n, o^2+o, p^2+p, q^2+q, r^2+r, s^2+s, t^2+t)
  J = ideal(a*b*c*d*e,a+b*c+d*e+a+b+c+d, j*h+i+f, g+f,a+d,j+i+d*c, r+s+t, m*n+o*p, t+a, b*s+q+p*n*m+i,  b*s+q+p+h, b*s+q*n*m+i, b*k+q+l*n*m+i)
  time M = apply(flatten entries gens gb J, i-> lift(i,R))
if longTest then          
  assert(N == M)

  JJ = ideal(a*b*c*d*e, b*c+d*e+b+c+d, j*h+i+f, g+f,a+d,j+i+d*c, r+s+t, m*n+o*p, t+a, b*s+q+p*n*m+i,  b*s+q+p+h, b*s+q*n*m+i, b*k+q+l*n*m+i)
  time MM = apply(flatten entries gens gb JJ, i-> lift(i,R))
if longTest then          
  assert(MM == M)

  F = new gbComputation from { 0 => convert(a), 1 => convert(a)}
  time gbBasis = gbBrp( F, numgens R)
  N = sort apply (values gbBasis, poly -> convert(poly,R) )
  assert (N == {a})
  
  F = new gbComputation from { 0 => convert(a), 1 => convert(a)}
  time gbBasis = minimalGbBrp F
  N = sort apply (values gbBasis, poly -> convert(poly,R) )
  assert (N == {a})
  
  R = ZZ/2[a..t, MonomialOrder=>Lex]
  F = new gbComputation from { 0 => convert(a), 1 => convert(a)}
  time gbBasis = reduceGbBrp F
  N = sort apply (values gbBasis, poly -> convert(poly,R) )
  assert (N == {a})
  
  F = new gbComputation from { 0 => convert(a+b*c), 1 => convert(a+d*e), 2 => convert(a)}
  time gbBasis = gbBrp( F, numgens R)
  N = sort apply (values gbBasis, poly -> convert(poly,R) )
  time M = apply(flatten entries gens gb ideal(a+b*c, a+d*e, a), i-> lift(i,R))
  assert( N == M)

  F = new gbComputation from { 0 => convert(a+b*c), 1 => convert(a+d*e), 2 => convert(a)}
  time gbBasis = reduceGbBrp F
  N = sort apply (values gbBasis, poly -> convert(poly,R) )
  time M = apply(flatten entries gens gb ideal(a+b*c, a+d*e, a), i-> lift(i,R))
  assert( N == M)

  R = ZZ/2[x,y,z,w]
  F = new gbComputation from { 0 => convert(x*y*w+w*x+z),
                              1 => convert (x*z+w*y) }
  gbBasis = gbBrp(F,numgens R)
  apply (values gbBasis, poly -> convert(poly,R) )
  assert( sort apply (values gbBasis, poly -> convert(poly,R) ) == sort {x*w, z, y*w})

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
  assert ( #F == 2 ) 
  assert (F#2 == new Brp from {{1, 0, 0}} )
  assert (F#3 == new Brp from {{0, 1, 1}, {0, 0, 1}} )

  R = ZZ/2[x,y,z]
  a = convert(x*z + y*z + z)
  b = convert(y+z)
  assert( reduceLtBrp(a,b) == new Brp from {{1, 0, 1}} )
  a = convert(x*z + y*z + y)
  b = convert(y+z)
  assert( reduceLtBrp(a,b) == new Brp from {{1, 0, 1}} )
  a = convert(x*z + y*z + y + z)
  assert( reduceLtBrp(a,b) == new Brp from {{1, 0, 1}, {0,0,1}} )

  a = convert(x*y + y*z +z)
  b= convert(y*z +z)
  assert( reduceLtBrp(a,b)== new Brp from {{1, 1, 0}})
  R = ZZ/2[x,y,z]
  a = convert(x*y + y*z + z)
  b = convert(y*z + z)
  c = convert(x*z + z)
  F = new gbComputation from {0=>a, 1=>b, 2=>c}
  reduceGbBrp(F)
  assert( apply( values F, i -> convert(i,R) ) == {x*y, y*z + z, x*z + z} )

  R = ZZ/2[x,y,z,w]
  a = convert(x*y + y*w*z + w*z +w)
  b = convert(y*z + w*z)
  c = convert(w)
  F = new gbComputation from {0=>a, 1=>b, 2=>c}
  reduceGbBrp(F)
  assert( sort apply( values F, i -> convert(i,R) ) == sort{x*y, y*z, w} )

 R=ZZ/2[x,y,z]
 S={x*y+y*z+1, y*z+x+1, x*y*z+x*y+y*z+x*z, z+y, y+1}
 L=getPolysFromList(S)
 assert(#L == 5)
 assert(L#0 == new Brp from {{1, 1, 0}, {0, 1, 1}, {0, 0, 0}})          
 assert(L#1 == new Brp from {{1, 0, 0}, {0, 1, 1}, {0, 0, 0}})
 assert(L#2 == new Brp from {{1, 1, 1}, {1, 1, 0}, {1, 0, 1}, {0, 1, 1}})
 assert(L#3 == new Brp from {{0, 1, 0}, {0, 0, 1}})
 assert(L#4 == new Brp from {{0, 1, 0}, {0, 0, 0}})
	
 
  
///
  
       
end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
installPackage "BuchbergerForBrp"
installPackage("BuchbergerForBrp", RemakeAllDocumentation=>true)
check BuchbergerForBrp

  
  R = ZZ/2[a..j, MonomialOrder=>Lex]
  F = new gbComputation from { 0=> convert(a*b*c*d*e),
          1=> convert( a+b*c+d*e+a+b+c+d),
          2=> convert( j*h+i+f),
          3=> convert( g+f),
          4=> convert( a+d),
          5=> convert( j+i+d*c)
          }
 time  gbBasis = gbBrp( F, numgens R)
 

restart
installPackage "BuchbergerForBrp"
installPackage("BuchbergerForBrp", RemakeAllDocumentation=>true)
leading = profile leading;
gbBrp= profile gbBrp;
makePairsFromLists= profile  makePairsFromLists; 
isReducible= profile  isReducible;
minimalGbBrp= profile  minimalGbBrp;
reduce= profile  reduce;
reduceGbBrp= profile  reduceGbBrp;
reduceLtBrp= profile  reduceLtBrp;
reduceOneStep= profile  reduceOneStep; 
reduceOneStepList= profile  reduceOneStepList; 
SPolynomial= profile  SPolynomial; 
updatePairs= profile  updatePairs;
lcmBrps = profile lcmBrps;
divide = profile divide;
scanProfile = profile scan;
applyProfile = profile apply;


R = ZZ/2[a..t, MonomialOrder=>Lex]
F = new gbComputation from { 0=> convert(a*b*c*d*e),
        1=> convert( a+b*c+d*e+a+b+c+d),
        2=> convert( j*h+i+f),
        3=> convert( g+f),
        4=> convert( a+d),
        5=> convert( j+i+d*c),
        6=> convert( r+s+t),
        7=> convert( m*n+o*p),
        8=> convert( t+a),
        9=> convert( b*s+q+p*n*m+i),
        10=> convert( b*s+q+p+h),
        11=> convert( b*s+q*n*m+i),
        12=> convert( b*k+q+l*n*m+i)
        }
time gbBasis =  gbBrp( F, numgens R) 
N = sort apply (values gbBasis, poly -> convert(poly,R) )
QR = R/(a^2+a, b^2+b, c^2+c, d^2+d, e^2+e, f^2+f, g^2+g, h^2+h, i^2+i, j^2+j, k^2+k, l^2+l, m^2+m, n^2+n, o^2+o, p^2+p, q^2+q, r^2+r, s^2+s, t^2+t)
J = ideal(a*b*c*d*e,a+b*c+d*e+a+b+c+d, j*h+i+f, g+f,a+d,j+i+d*c, r+s+t, m*n+o*p, t+a, b*s+q+p*n*m+i,  b*s+q+p+h, b*s+q*n*m+i, b*k+q+l*n*m+i)
time M = apply(flatten entries gens gb J, i-> lift(i,R))
assert(N == M)

restart
installPackage "BuchbergerForBrp"
installPackage("BuchbergerForBrp", RemakeAllDocumentation=>true)
check BuchbergerForBrp

l = 10000
a = cpuTime();
myHash = new HashTable from apply( l, i -> if i == l-1 then i => new Brp from {} else i => new Brp from { {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} });
b = cpuTime();
b-a
a = cpuTime();
myList = apply( l, i -> if i == l-1 then new Brp from {} else new Brp from { {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} });
b = cpuTime();
b-a
values myHash == myList
f = false
a = cpuTime();
applyValues(myHash, i -> if i == 0 then 0 else i)
b = cpuTime();
b-a
a = cpuTime();
values myHash;
b = cpuTime();
b-a
a = cpuTime();
scan(myList, i -> if i == 0 then (found = true,break) )
b = cpuTime();
b-a

apply( values myHash, i -> if i == 0 then break f )


restart
installPackage "BuchbergerForBrp"
installPackage("BuchbergerForBrp", RemakeAllDocumentation=>true)
check BuchbergerForBrp
 
 R = ZZ/2[a..t, MonomialOrder=>Lex]
  F = new gbComputation from { 0=> convert(a*b*c*d*e),
          1=> convert( a+b*c+d*e+a+b+c+d),
          2=> convert( j*h+i+f),
          3=> convert( g+f),
          4=> convert( a+d),
          5=> convert( j+i+d*c),
          6=> convert( r+s+t),
          7=> convert( m*n+o*p),
          8=> convert( t+a),
          9=> convert( b*s+q+p*n*m+i),
          10=> convert( b*s+q+p+h),
          11=> convert( b*s+q*n*m+i),
          12=> convert( b*k+q+l*n*m+i)
          };
timeInReduce = 0;
  time ( gbBrp( F, numgens R) )
timeInReduce
profileSummary


-- reduce the leading term of a polynomial f one step by the first polynomial
-- g_i in the intermediate basis that satisfies isReducible(f,g_i)
reduceOneStepList = method()
reduceOneStepList(Brp, gbComputation) := (Brp, Boolean) => (f, G) -> (
  t1 := cpuTime();
  if f != 0 then (
    ret := true;
    scanProfile( values G, g -> if isReducible(f, g) then ( f = reduceOneStep(f,g); ret = false; break) );
  ) 
  else ( 
    f = new Brp from {};
    ret = true
  );
  t2 := cpuTime();
  scanTimeSum = scanTimeSum + t2 - t1;
  (f,ret)
)

-- Reduce the polynomial until the leading term is not divisible by the
-- leading element of any element in G
-- f could be 0
reduce = method()
reduce (Brp, gbComputation) := Brp => (f,G) -> (
  t1 := cpuTime();
  fullyReducedFlag := false;
  while (not fullyReducedFlag) do (
    (f, fullyReducedFlag) = reduceOneStepList(f,G)
  );
  t2 := cpuTime();
  timeInReduce = timeInReduce + t2 - t1;
  f
)



restart
installPackage "BuchbergerForBrp"
installPackage("BuchbergerForBrp", RemakeAllDocumentation=>true)
check BuchbergerForBrp
