newPackage(
    "MonomialOrbits",
    Version => "1.0", 
    Date => "18 December 2020, rev 10 May 2021",
    Authors => {{Name => "David Eisenbud", 
            Email => "de@msri.org", 
            HomePage => "http://www.msri.org/~de"},
        {Name => "Mike Stillman", 
            Email => "mike@math.cornell.edu", 
            HomePage => "http://pi.math.cornell.edu/~mike"}},
    Headline => "Orbit representatives of monomial ideals",
    Keywords => {"Combinatorial Commutative Algebra"},
    PackageImports =>{"Truncations"}, -- for 'truncate'
    DebuggingMode => false
    )

export {
    "orbitRepresentatives",
    "hilbertRepresentatives",
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

monomialsInDegree = (d, R, type) -> (
    -- d: integer, or list (multidegree).
    -- R: polynomial ring
    -- type is either "All", "SquareFree" (anything else is an error)
    -- return: is a matrix of monomials of the given type and degree
    if type === "SquareFree" then 
        squareFree(d, R)
    else if type === "All" then 
        basis(d, R)
    else 
        error "expected MonomialType to be either \"All\" or \"SquareFree\""
    )

orbitRepresentatives = method(Options=>{MonomialType => "All"})

orbitRepresentatives(Ring, VisibleList) := List => o -> (R, degs) -> (
    orbitRepresentatives(R, monomialIdeal 0_R, degs, o))

orbitRepresentatives(Ring, Ideal, VisibleList) := List => o -> (R, I, degs) -> (

    if not isMonomialIdeal I then error"orbitRepresentatives:arg 1 is not a monomial ideal";
    result := {monomialIdeal I};

    G := permutations R;
    rawMonsMat := matrix{{}};
    mons := {};
    for d in degs do (
        rawMonsMat = monomialsInDegree(d, R, o.MonomialType);
        mons = flatten entries sort(rawMonsMat, 
                     DegreeOrder => Ascending, MonomialOrder => Descending);
        result = normalForms(sumMonomials(result, mons), G)
        );
    result
    )

orbitRepresentatives(Ring, Ideal, Ideal, ZZ) := List => o -> (R, I, startmons, numelts) -> (
         
    --take or subtract numelts elements from startmons mod I, plus I.

    if not isMonomialIdeal I then error"orbitRepresentatives:arg 1 is not a monomial ideal";
    I = monomialIdeal I;

    if not isMonomialIdeal startmons then error"orbitRepresentatives:arg 2 is not a monomial ideal";
    startmons = monomialIdeal startmons;

    G := permutations R;
    start := compress ((gens startmons) % I);

    if numelts < 0 then(
	    L := flatten entries start;
    	    sL := subsets(L, #L+numelts)/monomialIdeal;
    	    result := normalForms(apply(sL, ell -> ell + I), G)
    ) else (
        result = {I};
        mons := flatten entries sort(start,
                    DegreeOrder => Ascending, MonomialOrder => Descending);
        apply(numelts, i-> (
           result = normalForms(sumMonomials(result, mons), G)
           ));
    result
    ))


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
    --L1 list of monomial ideals
    --L2 list of monomials
    --return list of monomial ideals: an element of L1 
    --plus an element of L2 which is a minimal generator.
    unique flatten for I in L1 list (
        for m in L2 list (
            if m % I != 0 then I + monomialIdeal m 
            else continue
            )
        )
    )
sumMonomials(Ideal, List) := List => (I, L2) -> sumMonomials({I}, L2)

normalForms = method()
normalForms(List, List) := (Fs, G) -> (
    -- Fs is a list of MonomialIdeals, G a list of ring maps
    -- returns a minimal subset F of Fs such that G F = Fs.
    if #Fs == 0 then return {};
    S := ring Fs_0;
    G1 := select(G, s -> s vars S != vars S); -- remove the identity element if present.
    L := new MutableList from Fs;
    LH := hashTable for i from 0 to #Fs-1 list Fs#i => i;
    count := #L;
    if debugLevel > 0 then << "-- " << #L << " ideals" << endl;
    for i from 0 to #L-1 list (
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
        :Options limiting the type of ideals generated and whether to add or subtract monomials
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
	    If numelts $\geq 0$ then each monomial ideal produced is
	    I+(numelts elements of J); if numelts $< 0$ then 
	    each monomial ideal produced is I+J minus (|numelts| elements of J).
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
     orbitRepresentatives(S, monomialIdeal S_0, mm, -1))
assert(orbitRepresentatives(S, monomialIdeal S_0, mm^2, -1) == 
         {monomialIdeal(a,b^2,b*c,c^2,b*d,c*d), monomialIdeal(a,b^2,b*c,c^2,b*d,d^2)})
assert({monomialIdeal (a, b)} == 
       orbitRepresentatives(S, monomialIdeal S_0, mm, 1))
assert(orbitRepresentatives(S, monomialIdeal S_0, mm^2, 2) ==
       {monomialIdeal(a,b^2,b*c), monomialIdeal(a,b^2,c^2), monomialIdeal(a,b^2,c*d), monomialIdeal(a,b*c,b*d)})
///

TEST///
S = ZZ/101[x_1..x_4]
I0 = ideal x_1^2
mm = ideal vars S
mm2 = mm^2
assert(all(apply(2, e -> orbitRepresentatives(S,I0,toList(e: 2))),L -> class L === List))
assert(class orbitRepresentatives(S,I0,{}) === List)
///

end---------------------------------------------------------------------

///
  restart
  loadPackage("MonomialOrbits", Reload => true)
  uninstallPackage "MonomialOrbits"
  restart
  installPackage "MonomialOrbits"
  check "MonomialOrbits"
  viewHelp MonomialOrbits
///
