newPackage(
    "MonomialOrbits",
    Version => "0.9", 
    Date => "12 December 2020",
    Authors => {{Name => "David Eisenbud", 
            Email => "de@msri.org", 
            HomePage => "http://www.msri.org/~de"},
        {Name => "Mike Stillman", 
            Email => "mike@math.cornell.edu", 
            HomePage => "http://pi.math.cornell.edu/~mike"}},
    Headline => "Orbit representatives of monomial ideals",
    PackageExports =>{"Truncations"}, -- for 'truncate'
    DebuggingMode => true
    )

export {
    "orbitRepresentatives",
    "hilbertRepresentatives",
    "Perms",
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

///
  R = ZZ/101[x_0..x_5]
  squareFree(3,R)
///

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
orbitRepresentatives = method(Options=>{Perms=>"SymmetricGroup", MonomialType => "All"})

orbitRepresentatives(Ring, VisibleList) := List => o -> (R, degs) -> (
    setupRing(R,o); -- creates G as a list of ring automorphisms
    info := R.cache.MonomialOrbits;
    G := info#"GroupElements";
    result := {monomialIdeal 0_R};
    rawMonsMat := matrix{{}};
    mons := {};
    for d in degs do (
        rawMonsMat = if o.MonomialType === "SquareFree" then squareFree(d,R)
                     else basis(d,R);
        mons = flatten entries sort(rawMonsMat, 
                     DegreeOrder => Ascending, MonomialOrder => Descending);
        result = normalForms(sumMonomials(result, mons), G)
        );
    result
    )

orbitRepresentatives(Ring, MonomialIdeal, VisibleList) := List => o -> (R, I, degs) -> (
    setupRing(R, o); -- creates G and a list of lists of monomials in R.cache
    info := R.cache.MonomialOrbits;
    G := info#"GroupElements";
    result := {I};
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

///
  restart
  debug loadPackage("MonomialOrbits", Reload => true)
///

--orbitRepresentatives(Ring, Sequence) := List => o->(R, degs) -> 
--   orbitRepresentatives(R, toList degs, Perms => o.Perms, MonomialType => o.MonomialType)

hilbertRepresentatives = method(Options=>{Perms=>"SymmetricGroup", MonomialType => "All"})
hilbertRepresentatives(Ring, VisibleList) := List => o -> (R, h) -> (
    --orbit representatives of all monomial ideals I, if any, such that
    --hilbertFunction(i,R/I) = h_(i-1) for all i = 1,..,#h.
    setupRing(R, o); -- creates G and a list of lists of monomials in R.cache
    G := R.cache.MonomialOrbits#"GroupElements";
    
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

///
  restart
  debug loadPackage("MonomialOrbits", Reload => true)
///


setupRing = method(Options => {Perms => "SymmetricGroup", MonomialType => "all"})
--Perms is either "SymmetricGroup" or a list of ring automorphisms 
--or a list of permutations of 0..numgens R-1
setupRing Ring := o -> R -> (
    if not R.?cache then R.cache = new CacheTable;
    if not R.cache.?MonomialOrbits then R.cache.MonomialOrbits = new MutableHashTable;
    H := R.cache.MonomialOrbits;
    if H#?"MonomialType" then oldMonomialType := H#"MonomialType";
    
    if o.Perms === "SymmetricGroup" then (
        H#"Perms" = "SymmetricGroup";
        H#"GroupElements" = for p in permutations numgens R list
            map(R, R, (vars R)_p)
        )
    else (
        if not instance(o.Perms, VisibleList) then
            error "expected a list of ringmaps or lists representing permutations";
        if #o.Perms > 0 and class((o.Perms)_0) === RingMap then
            H#"GroupElements" = o.Perms 
        else
            H#"GroupElements" = permsToRingMaps(R,o.Perms);
        H#"Perms" = "Other"
        );
    )

///
restart
needsPackage "MonomialOrbits"
///

sumMonomials = method(Options => {Perms => "SymmetricGroup"})
sumMonomials(List, List) := List => o -> (L1, L2) -> (
    --L1 list of monomial ideals
    --L2 llist of monomials
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

stabilizer = method()
stabilizer(List, Ideal) := List => (G, I) -> (
    --  I and ideal in S
    --  G is a list of automorphisms S->S
    --  ouput is a list of those maps in G that fix I
    select(G, f-> gens f I % I == 0)
    )

cosets = method()
cosets(List, List) := List => (G, H) -> (
    H' := set H;
    G' := set G;
    representatives := {G_0}; -- should be the identity of G
    while G' - H' =!= set{} do (
        g := (toList(G'-H'))_0;
        representatives = append(representatives, g);
        H' = H' + set for h in H list g*h;
        );
    representatives
    )    

permsToRingMaps = method()
permsToRingMaps(Ring, List) := List => (S,L) ->(
    --L is a list of permutations of n = numgens S;
    --the output is a list of the corresponding self-maps of S.
    apply(L, ell -> map(S,S,apply(ell, i-> S_i)))
    )

///
  restart
  loadPackage("MonomialOrbits", Reload => true)
  uninstallPackage "MonomialOrbits"
  restart
  installPackage "MonomialOrbits"
  check "MonomialOrbits"
  viewHelp MonomialOrbits
///

beginDocumentation()

doc ///
    Key
        MonomialOrbits
    Headline
        find orbit representatives of monomial ideals, up to permutations of the variables
    Description
        Text
            This package contains functions for the construction of
            representatives of the orbits of monomial ideals of a
            given type in a polynomial ring S under a user-defined set
            G of permutations of the variables of S; by default G is
            the full symmetric group.
            
            The type of the ideals may be defined either by the number
            of minimal generators of each degree, in @TO
            orbitRepresentatives @ or by the Hilbert function, in @TO
            hilbertRepresentatives@.  If the option {\tt MonomialType
            => "SquareFree"} is given, then only square-free monomial
            ideals are considered.
            
            The set G is specified as a list of ring maps that permute the variables
            by an option of the form {\tt Perms => {...}}.
///

doc ///
    Key
        orbitRepresentatives
        (orbitRepresentatives, Ring, VisibleList)
        (orbitRepresentatives, Ring, MonomialIdeal, VisibleList)
        [orbitRepresentatives, Perms]
        [orbitRepresentatives, MonomialType]    
    Headline
        find representatives of monomial ideals under a set of permutations of variables
    Usage
        L = orbitRepresentatives(R, degs)
        L = orbitRepresentatives(R, I, degs)
    Inputs
        R:PolynomialRing
        degs:List 
            or @ofClass Sequence@, of the degrees of the generators
        I:Ideal
            The starting ideal; all the ideals returned will contain this one. 
        Perms => List
            or @ofClass String@.  If not given, or is the String {\it SymmetricGroup}, 
            the symmetric group of permutations on
            the variables of $R$ is used.  If a list is given, it
            should either be a list of ring maps, each corresponding to a
            permutation of the variables or a list of lists, each a permutation of 0..numgens R -1
        MonomialType => String
            Allowed values are {\tt "All"}, in which case all monomials are
            considered, and {\tt "SquareFree"},
            in which case only square free monomials are considered
    Outputs
        L:List
            of monomial ideals
    Description
        Text
            This method generates a list of representatives of the orbits of
            monomial ideals with given minimal generator degrees
            under a set of permutations of the variables. By default the
            set is the set of elements of the symmetric group, but the user may specify
            any list of permutations the variables.

            If the option @TO MonomialType@ is set to "SquareFree",
            then only ideals of square-free monomials are considered.

            The program works by induction on the number of
            generators; given the list L of orbit representatives for
            the ideals minimally generated by the first k of the
            generators, the program adds all possible generators of
            the (k+1)-st degree to each of ideals in L in a certain
            order, and then removes those in the list that can be
            obtained by a transformation in G from one that is earlier
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
            A subset of the symmetric group can be specified as a list of 
            permutations. The identity element can be included or not; it is eliminated
            by the program at the start. In the following example we illustrate with
            the trivial group G0 the cyclic group Gc, the set G1 consisting of
            just one cyclic permutation, the set Gsymm of all six permutations; they produce distinct lists of representatives.
        Example
            S = ZZ/101[a,b,c];
            G0 = {{0,1,2}}
            G0 = {}
            G1 = {map(S,S,{b,c,a})}
            Gc = {{1,2,0},{2,0,1}}
            Gsymm = "SymmetricGroup"
            # orbitRepresentatives(S,{2,2,2},Perms => G0)
            # orbitRepresentatives(S,{2,2,2},Perms => G1)
            # orbitRepresentatives(S,{2,2,2},Perms => Gc)
            # orbitRepresentatives(S,{2,2,2},Perms => Gsymm)
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
    SeeAlso
        hilbertRepresentatives
        Perms
        MonomialType
///

doc ///
    Key
        hilbertRepresentatives
        (hilbertRepresentatives, Ring, VisibleList)
        [hilbertRepresentatives, Perms]
        [hilbertRepresentatives, MonomialType]    
    Headline
        find representatives of monomial ideals under a set of permutations of the variables
    Usage
        L = hilbertRepresentatives(R,s)
    Inputs
        R:PolynomialRing
        s:VisibleList 
            of desired values of {\tt d->hilbertFunction(R/I,d)} for d in (1..length s)
        Perms => List
            or @ofClass String@.  If not given, or is {\it "SymmetricGroup"}, 
            the symmetric group of permutations on
            the variables of $R$ is used.  If a list is given, it
            should be a list of ring maps, each corresponding to a
            permutation of the variables, or a list of lists, each a 
	    permutation of 0..numgens R -1.
        MonomialType => String
            Allowed values are {\tt "All"}, in which case all monomials are
            considered, and {\tt "SquareFree"},
            in which case only square free monomials are considered
    Outputs
        L:List
            of monomial ideals
    Description
        Text
            This method generates a list of representatives of the orbits of
            monomial ideals with given Hilbert function,
            under a given set of permutations of the variables. By default the
            set is the set of elements of the symmetric group, but the user may specify
            any list of permutations the variables.


            If the option @TO MonomialType@ is set to "SquareFree",
            then only ideals of square-free monomials are considered.

            Starting with orbit representatives of monomial ideals
            generated by all but s_0 linear forms, it successively adds to each 
            monomial ideal already found as
            many forms of degree d in (2..1+length s) as necessary to
            achieve the desired Hilbert function, in all possible ways.  After each addition
            it chooses representatives under the action of the given set of permutations.

            Note that the (partial) Hilbert function is specified as a
            @TO VisibleList@, which could be either a list or a sequence.
        Example
            S = ZZ/101[a..d];
            L = hilbertRepresentatives(S,{4,4,1}) 
            #L
            L' = hilbertRepresentatives(S,{4,4,1,1}) 
            #L'
            L'' = hilbertRepresentatives(S,{4,4,1,1,1})	    
            L' == L''
            L = hilbertRepresentatives(S,(4,7,10,13,16))
            Lsqf = hilbertRepresentatives(S,(4,7,10,13,16), MonomialType => "SquareFree"); 	    
            #L
            #Lsqf
            LP = unique apply(L, m-> hilbertPolynomial m)
            tally apply(L, m->betti res m)
            tally apply(Lsqf, m->betti res m)	    
            #unique apply(L, m->primaryDecomposition m)
    	Text
            A subset of the symmetric group can be specified as a list of 
            permutations. The identity element can be included or not; it is eliminated
            by the program at the start. In the following example we illustrate with
            the trivial group G0, the cyclic group Gc, and the set consisting of
            just one cyclic permutation; they produce distinct lists of representatives.
        Example
            S = ZZ/101[a,b,c];
            G1 = {map(S,S,{b,c,a})}
            Gc = {{1,2,0},{2,0,1}}
            Gsymm = "SymmetricGroup"
            I1s = hilbertRepresentatives(S,{3,2,2,2},Perms => G1)
            Ics = hilbertRepresentatives(S,{3,2,2,2},Perms => Gc)
            Is = hilbertRepresentatives(S,{3,2,2,2},Perms => Gsymm)
        Text
            It is possible to specify data which results in no ideals:
        Example
            S = ZZ/101[a,b];
            hilbertRepresentatives(S,{1,4}) == {}
    SeeAlso
        orbitRepresentatives
        Perms
        MonomialType
///

doc ///
    Key
        Perms
    Headline
        Perms => "SymmetricGroup" or {f_1..f_t}
    Description
        Text
            This option specifies a group of permutations of
            variables.  Perms => "SymmetricGroup" or Perms => GG,
            where GG is a list of permutations of the variables as
            maps S -> S OR GG is a list of permutations of the numbers 
            0..numgens S-1.  The default, "SymmetricGroup" uses the full
            symmetric group.
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
G = {{0,2,1},{1,0,2},{1,2,0}}
Gs = permutations 3
assert(#orbitRepresentatives(S, (2,2,2), Perms => G_{0}) == 12 and
#orbitRepresentatives(S, (2,2,2), Perms => G_{1}) == 12 and
#orbitRepresentatives(S, (2,2,2), Perms => G_{2}) == 14 and
#orbitRepresentatives(S, (2,2,2), Perms => G_{0,1}) == 12 and
#orbitRepresentatives(S, (2,2,2), Perms => G) == 9 and
#orbitRepresentatives(S, (2,2,2)) == 6 and 
#orbitRepresentatives(S, (2,2,2), Perms => Gs) == 6
)
///

TEST///
  S = ZZ/101[a,b,c]
  I = monomialIdeal"a3,b3,c3"
  assert(#orbitRepresentatives(S,{3,3,3}) == 25)
  assert(#orbitRepresentatives(S,I,{3}) == 2)

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
  setupRing S	
  assert(#S.cache.MonomialOrbits#"GroupElements" == 24)
  G = S.cache.MonomialOrbits#"GroupElements"
  H = {G_0,G_1}
  C = cosets(G,H)
  assert(#C == 12)
  assert(24 ==#unique flatten for h in H list for c in C list (c*h))
///

end--

uninstallPackage "MonomialOrbits"
restart
installPackage "MonomialOrbits"
check "MonomialOrbits"
viewHelp MonomialOrbits
