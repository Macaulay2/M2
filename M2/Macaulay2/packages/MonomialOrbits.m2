 newPackage(
    "MonomialOrbits",
    Version => "1.5", 
    Date => "18 December 2020, last rev 1 June 2021",
    Authors => {{Name => "David Eisenbud", 
            Email => "de@msri.org", 
            HomePage => "http://www.msri.org/~de"},
                {Name => "Mike Stillman", 
            Email => "mike@math.cornell.edu", 
            HomePage => "http://pi.math.cornell.edu/~mike"}},
    Headline => "Orbit representatives of monomial ideals",
    Keywords => {"Combinatorial Commutative Algebra"},
    PackageExports =>{"Truncations"}, -- for 'truncate'
    DebuggingMode => false
    )

export {
    "orbitRepresentatives",    
    "hilbertRepresentatives",
    "normalForms",
    --options
    "MonomialType"
    }

squareFree = method()
squareFree(List, Ring) := Matrix => (d,R) -> (
    if degreeLength R != #d then 
        error("expected degrees of length "|degreeLength R);
    z := symbol z;
    R' := coefficientRing R[z_1..z_(numgens R), SkewCommutative => true, Degrees => degrees R];
    phi := map(R,R',vars R);
    phi basis(d,R')
    )
squareFree(ZZ, Ring) := Matrix => (d,R) -> squareFree({d}, R)

monomialsInDegree = method()
monomialsInDegree(ZZ, Ring, String) := Matrix => (d, R, type) -> monomialsInDegree({d}, R, type)
monomialsInDegree(List, Ring, String) := Matrix => (d, R, type) -> (
    -- d: integer, or list (multidegree).
    -- R: polynomial ring
    -- type is either "All", "SquareFree" (anything else is an error)
    -- return: is a matrix of monomials of the given type and degree
    if #d != degreeLength R then error"expected valid (multi)degree";
    if type === "SquareFree" then 
        squareFree(d, R)
    else if type === "All" then 
        basis(d, R)
    else 
        error "expected MonomialType to be either \"All\" or \"SquareFree\""
    )

hilbertRepresentatives = method(Options=>{MonomialType => "All"})
hilbertRepresentatives(Ring, VisibleList) := List => o -> (R, h) -> (
    --orbit representatives of all monomial ideals I, if any, such that
    --hilbertFunction(i,R/I) = h_(i-1) for all i = 1,..,#h.
    G := permutations R;
    
    if h_0 > numgens R then error "not enough variables";
    if min h < numgens R and o.MonomialType == "SquareFree" then return {};
    
    result := if h_0 == numgens R then 
                  {monomialIdeal 0_R} 
              else
                  {monomialIdeal((gens R)_{0..numgens R - h_0 -1})};
    rawMonsMat := matrix{{}};
    mons := {};
    for i from 2 to #h do (
        rawMonsMat = monomialsInDegree(i, R, o.MonomialType);
        mons = flatten entries sort(rawMonsMat,
                     DegreeOrder => Ascending, MonomialOrder => Descending);
        result = flatten for I in result list (
            mons = flatten entries sort(compress(rawMonsMat % truncate(i, I)),
                DegreeOrder => Ascending, MonomialOrder => Descending);
            defect := hilbertFunction(i, R^1/I) -  h_(i-1);
            if defect < 0 then continue;
            if h_(i-1) == 0  then (
                if mons == {} then result1 := {I}
                else result1 = {monomialIdeal trim (I+ideal mons)}
                )
            else (
                result1 = {I};
                scan(defect, j->(
                        result1 = normalForms(sumMonomials(result1, mons), G);
                        ))
                );
     	    result1);
        );
    result
    )

permutations Ring := R -> (
    if not R.?cache then R.cache = new CacheTable;
    if not R.cache.?MonomialOrbits then R.cache.MonomialOrbits = new MutableHashTable;
    H := R.cache.MonomialOrbits;
    if not H#?"GroupElements" then
        H#"GroupElements" = for p in permutations numgens R list
            map(R, R, (vars R)_p);
    H#"GroupElements"
    )

sumMonomials = method()
sumMonomials(List, List) := List => (L1, L2) -> (
    --L1 list of monomial ideal
    --L2 list of monomials
    --return list of monomial ideals I' where 
    --I' is an ideal I in L1 with a monomial from L2 adjoined 
    --that is not in the ideal I
    --
    --sorted.
    unique flatten for I in L1 list (
        for m in L2 list (
            if m %  I != 0 then I+monomialIdeal m
            else continue
            )
        )
    )

sumMonomials(Ideal, List) := List => (I, L2) -> sumMonomials({I}, L2)

normalForms = method()
normalForms(List, List) := (Fs, G) -> (
    <<"---"<< #Fs<<endl;
    -- Fs is a list of MonomialIdeals, G a list of ring maps
    -- returns a minimal subset F of Fs such that G F = Fs.
    if #Fs == 0 then return {};
    S := ring Fs_0;
    G1 := select(G, s -> s vars S != vars S); -- remove the identity element if present.
    L := new MutableList from Fs;
    LH := hashTable for i from 0 to #Fs-1 list Fs#i => i;
    count := #L;
    if debugLevel > 0 then << "-- " << #L << " ideals" << endl;

    ans := for i from 0 to #L-1 list (
        if L#i === null then continue;
        F := L#i;
        for f in G1 do (
            H := monomialIdeal(f F);
            if LH#?H then (
                j := LH#H;
                if j > i and L#j =!= null then (
                    L#j = null;
                    count = count - 1;
                    if count % 1000 == 0 and debugLevel > 0 then
                        << "--  remaining count: " << count << endl;
                    );
                );
            );
        F
        )
    )

--The Lis versions: Here a monomial is an exponent vector, which is a list of integers, and
--a monomial ideal is a sorted list of exponent vectors.
--Note that the zero monomial ideal is (), represented by {{}}, and this often has to be handled separately.

--first methods for converting between monomial ideals and lists of lists
toLis = method()

toLis RingElement := List => m -> (exponents m)_0
toLis MonomialIdeal := List => I -> if I == 0 then {{}} else 
                                    --reverse sort( I_*/(m-> toLis m))
				    sort( I_*/(m-> toLis m))

toMonLis = (S,e) -> product(#e, i-> S_i^(e_i))

fromLis = method()
fromLis (Ring, List) := MonomialIdeal => (S,L) -> if L === {} or L === {{}} then monomialIdeal 0_S else 
                                                              monomialIdeal apply(L,e-> toMonLis (S,e))

notIn = method()
notIn(List, List) := Boolean => (m, L2) -> (
    --returns true if m is not "divisible" by any element of L2.
    if L2 == {{}} then return true;
    diffs := apply(L2, n -> m-n);
    all(diffs, L -> min L < 0)
    )

monomialsInDegreeLis = method()
monomialsInDegreeLis (VisibleList,Ring,String) := 
monomialsInDegreeLis(ZZ, Ring, String) := List => (d, R, type) -> (
    -- d: integer, or list (multidegree).
    -- R: polynomial ring
    -- type is either "All", "SquareFree" (anything else is an error)
    -- return: is a matrix of monomials of the given type and degree
    flatten entries sort(monomialsInDegree(d,R,type), MonomialOrder => Descending)/toLis
    )

orbitRepresentatives = method(Options=>{MonomialType => "All"})
orbitRepresentatives(Ring, Ideal, VisibleList) := List => o -> (R, I, degs) -> (
    if not isMonomialIdeal I then error"orbitRepresentatives:arg 1 is not a monomial ideal";
    if #degs >1 then  degs = sort toList(degs); -- more efficient to add the small degree gens first.

    n := numgens R;
    G := permutations n;

    result := {toLis monomialIdeal I}; --if I = 0, this gives {{}} ; has to be treated specially
    for d in degs do( 
        mons := monomialsInDegreeLis(d, R, o.MonomialType);
	sumList := sumMonomialsLis(result, mons);
	result = normalFormsLis(sumList, G);
    	);
     result/(L -> fromLis(R,L))
    )

orbitRepresentatives(Ring, VisibleList) := List => o -> (R, degs) -> (
    ze := monomialIdeal 0_R;
    orbitRepresentatives(R, ze, degs, o)
    )

orbitRepresentatives(Ring, Ideal, Ideal, ZZ) := List => o -> (R, I, startmons, numelts) -> (
     --take or subtract numelts elements from startmons mod I, plus I.
    if not isMonomialIdeal I then error"orbitRepresentatives:arg 1 is not a monomial ideal";
    I = monomialIdeal I;
    if not isMonomialIdeal startmons then error"orbitRepresentatives:arg 2 is not a monomial ideal";
    startmons = monomialIdeal startmons;

    n := numgens R;
    G := permutations n;

    num := abs(numelts);
    
    Ilis := if I == 0 then {} else toLis monomialIdeal I;
    startLis := toLis monomialIdeal startmons;
    start := if Ilis ==={} then startLis else
             for m in startLis list if notIn(m,Ilis) then m else continue;

   result := {Ilis}; --if I = 0, this gives {{}} ; has to be treated specially   
   mons := start;

   apply(num, i-> (
	sums := sumMonomialsLis(result, mons);
	result = normalFormsLis(sums, G)
        ));
   result = apply(result, L -> fromLis(R,L));
   
   --case of subtraction:
   if numelts < 0 then(
       bigideal := fromLis(R, Ilis|start);
       result = for K in result list(I + (bigideal - K))
    );
result)

sumMonomialsLis = method()
sumMonomialsLis(List, List) := List => (L1, L2) -> (
    --L1: list of lists of lists, representing a list of monomial ideals, or a list representing a single
    --monomial ideal.
    --L2: list of lists, representing monomials
    --return list of lists L of lists; where 
    --returns the list of monomial ideals I, each an ideal I' from L1 with a "monomial" from L2 adjoined 
    --that is not divisible by any monomial in I',
     unique flatten for I in L1 list (
        for m in L2 list if I == {{}} then {m} else
            if notIn(m, I) then sort (I | { m })
            else continue
            ))

normalFormsLis = method()
normalFormsLis(List, List) := List => (Fs, G) -> (
    -- Fs is a list of lists representing MonomialIdeals, G a list of permutations
    -- returns a minimal subset F of Fs such that G F = Fs.
    if #Fs == 0 then return {{}};

    n := #(Fs_0_0); -- "number of variables"
    ident := apply(n, i-> i);
    G1 := select(G, g-> g != ident); -- remove the identity element if present.

   
    L := new MutableList from Fs;
    LH := hashTable for i from 0 to #Fs-1 list Fs#i => i;
    count := #L;
    if debugLevel > 0 then << "-- " << #L << " ideals" << endl;

    ans := for i from 0 to #L-1 list (
        if L#i === null then continue;
        F := L#i;
        for f in G1 do (
            H := sort apply(F, FF -> FF_f);
            if LH#?H then (
                j := LH#H;
                if j > i and L#j =!= null then (
                    L#j = null;
                    count = count - 1;
                    if count % 1000 == 0 and debugLevel > 0 then
                        << "--  remaining count: " << count << endl;
                    );
                );
            );
        F
        );
    ans
    )

beginDocumentation()

doc ///
    Key
        MonomialOrbits
    Headline
        find orbit representatives of monomial ideals, under permutations of the variables
    Description
        Text
            This package contains functions for the construction of
            representatives of the orbits of monomial ideals of a
            given type in a polynomial ring $S$ under the group of
            permutations of the variables of $S$.
            
            The type of the ideals may be defined either by the number
            of minimal generators of each degree, or by
	    the set of monomials from which to choose 
	    or by the set of monomials from which to
	    subtract in @TO
            orbitRepresentatives @ or by the Hilbert function, in @TO
            hilbertRepresentatives@.  If the option {\tt MonomialType
            => "SquareFree"} is given, then only square-free monomial
            ideals are considered.
    Subnodes                                         
        :Enumerating monomial ideals with given generator degrees or number of elements
            @TO orbitRepresentatives@
        :Enumerating monomial ideals with given Hilbert function
            @TO hilbertRepresentatives@
        :Options limiting the type of ideals generated
            @TO MonomialType@ 
///

doc ///
    Key
        orbitRepresentatives
        (orbitRepresentatives, Ring, VisibleList)
        (orbitRepresentatives, Ring, Ideal, VisibleList)
        (orbitRepresentatives, Ring, Ideal, Ideal, ZZ)	
        [orbitRepresentatives, MonomialType]    
    Headline
        find representatives of monomial ideals under permutations of variables
    Usage
        L = orbitRepresentatives(R, degs)
        L = orbitRepresentatives(R, I, degs)
	L = orbitRepresentatives(R, I, J, numelts)
    Inputs
        R:PolynomialRing
        degs:List 
            or @ofClass Sequence@, of the degrees of the generators
        I:Ideal
            The starting ideal; all the ideals returned will contain this one.
	J:Ideal
	    A monomial ideal containing monomials from which to add to I;
	    when numelts < 0, then the ideals formed are I+J minus 
	    a certain number of monomials.
	numelts:ZZ
	    If numelts >0 then each monomial ideal produced is
	    I+ numelts elements of J; if numelts < 0 then 
	    each monomial ideal produced is I+J minus |numelts| elements of J.
        MonomialType => String
            (either {\tt "All"} or {\tt "SquareFree"}).  For {\tt "All"}, 
            all monomials are
            considered, and for {\tt "SquareFree"},
            only square free monomials are considered
    Outputs
        L:List
            of monomial ideals
    Description
        Text
            This method generates a list of representatives of the orbits of
            monomial ideals with given minimal generator degrees
            under the group of permutations of the variables.

            If the option @TO MonomialType@ is set to "SquareFree",
            then only ideals of square-free monomials are considered.

            The program works by induction on the number of
            generators; given the list L of orbit representatives for
            the ideals minimally generated by the first k of the
            generators, the program adds all possible generators of
            the (k+1)-st degree to each of ideals in L in a certain
            order, and then removes those in the list that can be
            obtained by a permutation of variables from one that is earlier
            in the list.
	    
            Because the generators are constrained to be minimal generators,
            it is advantageous to specify the low degrees of generators first.  

            Note that {\tt degs} is specified as a VisibleList, which could
            be either a list or a sequence.
        Example
            S = ZZ/101[a..d];
            L = orbitRepresentatives(S,(2,2,2))
            #L
            tally apply(L, m->betti res m)
            L' = orbitRepresentatives(S,(2,2,2), MonomialType => "SquareFree")
            #L'
            tally apply(L', m->betti res m)
            I = monomialIdeal"a2,b2,c2,d2"
            L'' = orbitRepresentatives(S,I,{2,2,2})
            tally apply(L'', m->betti res m)
        Text
            Multi-gradings are also allowed:
        Example
            S = ZZ/101[x_0..x_3, Degrees=>{{1,2},{2,1},{1,1},{1,0}}];
            orbitRepresentatives(S,{{2,2},{2,1}})
        Text
            Since the input data specifies degrees of minimal generators,
            the set of ideals may be empty:
        Example
            S = ZZ/101[a,b];
            L = orbitRepresentatives(S,(2,2,2,2))
	Text
	    It is possible to give a starting monomial ideal, and add
	    a given number of its generators.
	Example
            L = orbitRepresentatives(S,monomialIdeal a^3, (ideal(a,b))^3, 2)
	Text
	    If the number given is negative, then all but that number
	    of elements of the starting monomial ideal in arg 2 are taken. The 
	    starting monomial ideal is reduced mod the ideal in arg 1 before
	    the process begins
	Example
	    L = orbitRepresentatives(S,monomialIdeal a^3, (ideal(a,b))^3, -2)
    SeeAlso
        hilbertRepresentatives
        MonomialType
///

doc ///
    Key
        hilbertRepresentatives
        (hilbertRepresentatives, Ring, VisibleList)
        [hilbertRepresentatives, MonomialType]    
    Headline
        find representatives of monomial ideals under permutations of the variables
    Usage
        L = hilbertRepresentatives(R,s)
    Inputs
        R:PolynomialRing
        s:VisibleList 
            of desired values of {\tt d->hilbertFunction(R/I,d)} for d in (1..length s)
        MonomialType => String
            (either {\tt "All"} or {\tt "SquareFree"}).  For {\tt "All"}, 
            all monomials are
            considered, and for {\tt "SquareFree"},
            only square free monomials are considered
    Outputs
        L:List
            of monomial ideals
    Description
        Text
            This method generates a list of representatives of the orbits of
            monomial ideals with given Hilbert function, 
            under the group of permutations of the variables.

            If the option @TO MonomialType@ is set to "SquareFree",
            then only ideals of square-free monomials are considered.

            Starting with orbit representatives of monomial ideals
            generated by all but s_0 linear forms, it successively adds to each 
            monomial ideal already found as
            many forms of degree d in (2..1+length s) as necessary to
            achieve the desired Hilbert function, in all possible ways.  After each addition
            it chooses representatives under the action of the group permuting the
            variables of the ring.

            Note that the (partial) Hilbert function is specified as a
            @TO VisibleList@, which could be either a list or a sequence.
        Example
            S = ZZ/101[a..d];
            netList(L = hilbertRepresentatives(S,{4,7,10}))
            #L
            tally apply(L, m-> hilbertPolynomial(m,Projective => false))
            tally apply(L, m->betti res m)	    
	    tally apply(L, m->primaryDecomposition m)	    
	Text
            If the option @TO MonomialType@ is set to "SquareFree",
            then only ideals of square-free monomials are considered.
        Example
    	    netList hilbertRepresentatives(S,{4,7,10,13}, MonomialType => "SquareFree")
        Text
            It is possible to specify data which results in no ideals:
        Example
            S = ZZ/101[a,b];
            hilbertRepresentatives(S,{1,4}) == {}
    SeeAlso
        orbitRepresentatives
        MonomialType
///

doc ///
Key
 normalForms
 (normalForms, List, List)
Headline
 chooses orbit representatives from a list of monomial ideals, under a group of permutations
Usage
 L' = normalForms(L,G)
Inputs
 L:List
  list of monomial ideals in a ring R
 G:List
  list of permutations, written as automorphisms of R
Outputs
 L':List
  list of representatives of  the G-orbits in L
Description
  Text
   To test a conjecture on monomial ideals of a certain type, one typically makes a 
   computation on each one. If the computation is costly, and the result is invariant
   under a group G of permutations of the variables, it may be more efficient to
   choose orbit representatives first.

   Applying the ring automorphisms and then comparing the ideals can be slow. 
   In the routine orbitRepresentatives, the monomial ideals are first
   turned into lists of exponent vector, and the normalForms operations are done on these
   using "normalFormsLis" instead of on the original ideals. The conversions to and from
   monomialIdeals and lists of lists are done with toLis and fromLis.
   
  Example
   R = ZZ/101[w,x,y,z]
   monlist = flatten entries basis(4,R)
   idlist = subsets(monlist, 3)/monomialIdeal; 
   #oo
  Text
   There are 6545 ideals in 4 variables generated by 3 monomials of degree 4
  Example
   G0 = permutations numgens R
   G = apply(G0, g -> map(R,R,(vars R)_g))
   normalForms(idlist,G); --there are 244
   #oo
  Text
   But only 333 orbits, a 19-fold reduction.
SeeAlso
 orbitRepresentatives
 hilbertRepresentatives
///

doc ///
    Key
        MonomialType
    Headline
        MonomialType => "SquareFree" or "All"
    Usage
        orbitRepresentatives(S,degs,MonomialType => "SquareFree")
    Description
        Text
            The default is "All".
///

TEST///
   R = ZZ/101[w,x,y,z]
   monlist = flatten entries basis(4,R)
   assert (#(idlist = subsets(monlist, 3)/monomialIdeal) == 6545)
   G0 = permutations numgens R
   G = apply(G0, g -> map(R,R,(vars R)_g))
   assert(#normalForms(idlist,G) == 333)
///

TEST///
S = ZZ/101[a..d];
assert(#orbitRepresentatives(S,(2,2,2)) == 11)
assert(#orbitRepresentatives(S,{2,2,2}, MonomialType => "SquareFree") == 3)
///

TEST///
debug MonomialOrbits
#(G = permutations 4)
#(G1 = drop(G,1))
Fs = {{{1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}, {{1, 0, 0, 0}, {0, 0, 0, 1}, {0, 1, 0, 0}}, {{1, 0, 0, 0}, {0, 0, 1, 0}, {0, 1, 0, 0}}}
Fs = Fs/sort
assert(#normalFormsLis(Fs,G) == 1) 
///    

TEST///
debug MonomialOrbits
S = ZZ/101[x,y,z]
L = monomialsInDegreeLis(4,S,"All")
M = monomialsInDegree(4,S,"All")
M' = sort(M, DegreeOrder => Ascending, MonomialOrder => Descending)
assert(all(#L, i->toMonLis(S, L_i) === (flatten entries M')_i))
///

TEST///
restart
loadPackage"MonomialOrbits"
debug MonomialOrbits
S = ZZ/101[x,y,z]
L = monomialsInDegreeLis(4,S,"All")
M = monomialsInDegree(4,S,"All")
M' = sort(M, DegreeOrder => Ascending, MonomialOrder => Descending)
assert(all(#L, i->toMonLis(S,L_i) === (flatten entries M')_i))
///

TEST///
  S = ZZ/101[x_0..x_3, Degrees=>{{1,2},{2,1},{1,1},{1,0}}]

  result = orbitRepresentatives(S,{{2,2},{2,1}})
  ans = {monomialIdeal(x_1, x_0*x_3), 
      monomialIdeal(x_2*x_3, x_0*x_3),
      monomialIdeal(x_1, x_2^2),
      monomialIdeal(x_2*x_3, x_2^2)}
  assert(#result == 4)
  assert(set ans === set result)
///

TEST///
  S = ZZ/101[a,b,c]
  I = monomialIdeal"a3,b3,c3"
  assert(#orbitRepresentatives(S,{3,3,3}) == 25)
  assert(#orbitRepresentatives(S,I,{3}) == 2)
  assert(#orbitRepresentatives(S,monomialIdeal 0_S, (monomialIdeal vars S)^3, 3) == 25)
  assert(#orbitRepresentatives(S,I, (monomialIdeal vars S)^3, 1) == 2)
  R = ZZ/101[a..f]
  assert(
      orbitRepresentatives(R,{4,5}, MonomialType => "SquareFree") 
      == {monomialIdeal (a*b*c*d, a*b*c*e*f)})
///    

TEST///
  R = ZZ/101[a,b]
  assert(hilbertRepresentatives(R,{2,2}) == {monomialIdeal a^2 , monomialIdeal(a*b)})
  assert(toString\hilbertRepresentatives(R,{2,2,1,0}) =={"monomialIdeal(a^2,a*b^2,b^4)", "monomialIdeal(a^2,b^3)", "monomialIdeal(a^3,a*b,b^4)"})
  assert(hilbertRepresentatives(R,{2,3,0}) =={monomialIdeal(a^3,a^2*b,a*b^2,b^3)})

  R = ZZ/101[a,b,c]
  assert(#hilbertRepresentatives(R,{2}) == 1)
  assert(#hilbertRepresentatives(R,{2,0}) == 1)

  assert(#hilbertRepresentatives(R,{2,2,1})  == 3)
  assert(#hilbertRepresentatives(R,{2,2,1,0}) == #hilbertRepresentatives(R,{2,2,1}))

  assert(#hilbertRepresentatives(R,{3,4,5}) == 2)
  assert(#hilbertRepresentatives(R,{3,4,0}) == 4)
///

TEST///
  debug needsPackage "MonomialOrbits"
  S = ZZ/101[a,b,c,d]
  assert(# permutations S == 24)
///
///
restart
debug loadPackage("MonomialOrbits", Reload=>true)
///

TEST///   
S = ZZ/101[a..d]
mm = monomialIdeal gens S
assert ({monomialIdeal (a, b, c)} ==
     orbitRepresentatives(S, monomialIdeal S_0, mm, -1)
     )
assert (
     #orbitRepresentatives(S, monomialIdeal S_0, mm, -1) == 1
     )
 assert(
    {monomialIdeal(a, b^2,b*c,c^2,b*d,c*d), monomialIdeal(a,b^2,b*c,c^2,b*d,d^2)} ==
    orbitRepresentatives(S, monomialIdeal S_0, mm^2, -1)
    )
assert({monomialIdeal (a, d)} == 
       orbitRepresentatives(S, monomialIdeal S_0, mm, 1)
       )
assert(
      {monomialIdeal(a,c*d,d^2), monomialIdeal(a,c^2,d^2), monomialIdeal(a,b*c,d^2), monomialIdeal(a,b*d,c*d)} == 
      orbitRepresentatives(S, monomialIdeal S_0, mm^2, 2)
      )
///


///--new TEST
restart
loadPackage "MonomialOrbits"
debugLevel = 1
///

TEST///
debug MonomialOrbits;
S = ZZ/101[x,y,z]
mm = ideal vars S
I = monomialIdeal monomialsInDegree(3,S,"All")
L = toLis I
assert(I_* == (fromLis(S, toLis I))_*)
ze = monomialIdeal 0_S

ans1 = orbitRepresentatives(S,ze, {2,2}) -- both of these pairs should be singletons:
ans2 = orbitRepresentatives(S,ze, {2,2}) -- both of these pairs should be singletons:
assert(ans1==ans2)

ans1 = orbitRepresentatives(S,ze, {3}) 
ans2 = orbitRepresentatives(S,ze, {3})
assert(ans1==ans2)

ans1 = orbitRepresentatives(S,monomialIdeal(x^3), {3})
ans2 = orbitRepresentatives(S,monomialIdeal(x^3), {3})
assert(ans1==ans2)

ans1 = orbitRepresentatives(S,monomialIdeal(z^3), {3}) 
ans2 = orbitRepresentatives(S,monomialIdeal(z^3), {3})
assert(ans1 == ans2)

ans1 = orbitRepresentatives(S,monomialIdeal(0_S), {3,3})
ans2 = orbitRepresentatives(S,monomialIdeal(0_S), {3,3})
assert(ans1 == ans2)

assert(#orbitRepresentatives (S, ze, 3:5) == 238)

ans1 = orbitRepresentatives (S, ze, {2,3,4})
ans2 = orbitRepresentatives (S, ze, (2,3,4))
assert(ans1 == ans2)
ans3 = orbitRepresentatives (S, {2,3,4})
assert(ans1 == ans3)

assert(#orbitRepresentatives (S, monomialIdeal x, mm, -1) == 1)
assert(#orbitRepresentatives (S, ze, mm, -1) == 1)
///

-- Tests added in the 2026 test-audit pass: regression test for the empty-degs
-- boundary (previously crashed in fromLis on the {{}} zero-ideal-as-list
-- convention from L154-155); explicit cardinality assertions for
-- orbitRepresentatives (the existing block at L669 only checked reflexivity
-- ans1==ans2 with identical RHS, so it silently passes regardless of result);
-- Sequence/List dispatch agreement; MonomialType output validation;
-- hilbertRepresentatives error paths; the (Ring,Ideal,Ideal,ZZ) numelts=0
-- identity; structural properties; and normalForms with identity-only G.

TEST///
  -- regression test for the empty-degs crash.  Before the fix,
  -- orbitRepresentatives(S, {}) crashed in fromLis because toLis returns
  -- {{}} for the zero ideal but fromLis only handled L === {}.
  S = ZZ/101[x,y,z];
  ze = monomialIdeal 0_S;
  assert(orbitRepresentatives(S, {}) == {ze})
  assert(orbitRepresentatives(S, ze, {}) == {ze})
  -- the (Ring, Ideal, Ideal, ZZ) overload with numelts = 0 already worked;
  -- pin that down too
  assert(orbitRepresentatives(S, monomialIdeal x, monomialIdeal gens S, 0) == {monomialIdeal x})
  assert(orbitRepresentatives(S, ze, monomialIdeal gens S, 0) == {ze})
///

TEST///
  -- cardinality assertions on orbitRepresentatives.  The block at L669 set
  -- ans1 = orbitRepresentatives(...) and ans2 = orbitRepresentatives(...)
  -- with identical RHS, so assert(ans1 == ans2) was trivially reflexive and
  -- proved nothing about the actual orbit counts.
  S = ZZ/101[x,y,z];
  ze = monomialIdeal 0_S;
  -- 3 orbits of degree-3 monomials in 3 vars: {x^3 type, x^2 y type, xyz}
  assert(#orbitRepresentatives(S, ze, {3}) == 3)
  -- 4 orbits when adding two degree-2 generators
  assert(#orbitRepresentatives(S, ze, {2,2}) == 4)
  -- 5 orbits with a fixed cube as base
  assert(#orbitRepresentatives(S, monomialIdeal(x^3), {3}) == 5)
  assert(#orbitRepresentatives(S, monomialIdeal(z^3), {3}) == 5)
  -- 10 orbits with two degree-3 generators
  assert(#orbitRepresentatives(S, ze, {3,3}) == 10)
  -- mixed-degree case
  assert(#orbitRepresentatives(S, ze, {2,3,4}) == 47)
///

TEST///
  -- the (Ring, VisibleList) overload should agree with (Ring, Ideal, VisibleList)
  -- when the Ideal is the zero monomial ideal.  Also verify Sequence/List dispatch.
  S = ZZ/101[x,y,z];
  ze = monomialIdeal 0_S;
  assert(orbitRepresentatives(S, {3}) == orbitRepresentatives(S, ze, {3}))
  assert(orbitRepresentatives(S, (2,3)) == orbitRepresentatives(S, {2,3}))
  assert(orbitRepresentatives(S, ze, (2,3)) == orbitRepresentatives(S, ze, {2,3}))
  -- and the doc-example value
  T = ZZ/101[a..d];
  assert(#orbitRepresentatives(T, (2,2,2)) == 11)
///

TEST///
  -- MonomialType => "SquareFree" really restricts to squarefree generators.
  S = ZZ/101[a..f];
  sf = orbitRepresentatives(S, {4,5}, MonomialType => "SquareFree");
  assert(all(sf, J -> all(J_*, m -> max first exponents m <= 1)))
  -- toggling between All and SquareFree only shrinks the orbit set
  assert(#orbitRepresentatives(S, {4,5}) >= #sf)
  -- on (Ring, Ideal, VisibleList), the option still flows through
  T = ZZ/101[a,b,c];
  I = monomialIdeal(a*b);
  assert(all(orbitRepresentatives(T, I, {2}, MonomialType => "SquareFree"),
        J -> all(J_*, m -> max first exponents m <= 1)))
///

TEST///
  -- hilbertRepresentatives error and empty-result paths.
  R = ZZ/101[a,b];
  -- h_0 > numgens R is rejected with an error
  assert(try (hilbertRepresentatives(R, {3}); false) else true)
  -- a Hilbert sequence with no realisations yields the empty list
  assert(hilbertRepresentatives(R, {1,4}) == {})
  -- a SquareFree request with h_i < numgens R also yields the empty list
  R3 = ZZ/101[a,b,c];
  assert(hilbertRepresentatives(R3, {2,2}, MonomialType => "SquareFree") == {})
  -- every returned ideal actually realises the requested Hilbert function
  R4 = ZZ/101[a..d];
  hr = hilbertRepresentatives(R4, {4,7,10});
  assert(all(hr, J -> hilbertFunction(1, R4^1/J) == 4))
  assert(all(hr, J -> hilbertFunction(2, R4^1/J) == 7))
  assert(all(hr, J -> hilbertFunction(3, R4^1/J) == 10))
  assert(all(hr, J -> class J === MonomialIdeal))
///

TEST///
  -- non-monomial starting ideal must error explicitly
  T = QQ[a,b,c];
  assert(try (orbitRepresentatives(T, ideal(a + b), {2}); false) else true)
  assert(try (orbitRepresentatives(T, monomialIdeal 0_T, ideal(a + b), 1); false) else true)
///

TEST///
  -- structural properties of orbitRepresentatives output.
  S = ZZ/101[x,y,z];
  ze = monomialIdeal 0_S;
  -- every element is a MonomialIdeal living in the supplied ring
  L = orbitRepresentatives(S, ze, {2,2,2});
  assert(all(L, J -> class J === MonomialIdeal))
  assert(all(L, J -> ring J === S))
  -- with degs = {d,d,d} every output has exactly 3 generators of degree d
  assert(all(L, J -> numgens J == 3))
  assert(all(L, J -> all(J_*, m -> first degree m == 2)))
  -- determinism: the same call produces an identical answer
  assert(orbitRepresentatives(S, ze, {2,3}) === orbitRepresentatives(S, ze, {2,3}))
///

TEST///
  -- normalForms behaves correctly under the trivial group: every input is
  -- already a representative, so the length is preserved.
  S = ZZ/101[x,y,z];
  idmap = map(S, S, vars S);
  F = {monomialIdeal(x^2, y^2), monomialIdeal(x*y, x*z)};
  assert(#normalForms(F, {idmap}) == 2)
  -- and under the full S_3 action on a hand-picked input, only one orbit
  -- representative survives.
  G = apply(permutations 3, g -> map(S, S, (vars S)_g));
  F2 = {monomialIdeal(x^2, y^2), monomialIdeal(y^2, z^2), monomialIdeal(x^2, z^2)};
  assert(#normalForms(F2, G) == 1)
///


end-----------------------------------

///
  restart
  loadPackage("MonomialOrbits", Reload => true)
  uninstallPackage "MonomialOrbits"
  restart
  installPackage "MonomialOrbits"
  check "MonomialOrbits"

  viewHelp MonomialOrbits
///

n = 4
x = symbol x
S = ZZ/101[x_1..x_n]
ze = monomialIdeal  0_S
mm = monomialIdeal gens S

--timing for version of June 1, 2021 on DE's IMac Pro:
(d,s) = (4,4)
#elapsedTime orbitRepresentatives (S, ze, mm^d, s) --.958 sec
#elapsedTime orbitRepresentatives (S, ze, mm^d, -s) --1.021 sec
#elapsedTime orbitRepresentatives (S, ze, s:d) --.980 sec
--for an earlier version, the timing was 41.5 sec.


