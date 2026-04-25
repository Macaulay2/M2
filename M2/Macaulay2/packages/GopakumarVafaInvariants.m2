newPackage(
    "GopakumarVafaInvariants",
    Version => "0.1", 
    Date => "23 April 2026",
    Authors => {
        {
            Name => "Mike Stillman",  
            Email => "mike@math.cornell.edu", 
            HomePage => "http://www.math.cornell.edu/~mike"}
        },
    Headline => "computing GV invariants of Calabi-Yau's in toric varieties",
    Keywords => {"Computer Algebra"},
    DebuggingMode => true
    )

export {
    "Mori",
    "gvInvariants",
    "encodeArrayArrayInt",
    "decodeArrayArrayInt"
    }

exportFrom_"Core" { "rawGVInvariants" }

gvInvariants = method(Options => {
    Mori => null, -- null means: compute rays of the Mori cone of V (in ZZ^(h11))
    Heft => null, -- null means: compute it
    DegreeLimit => infinity,
    Precision => 150
    --FilePrefix => "foo"
    })

encodeArrayArrayInt = method()
encodeArrayArrayInt List := List => L -> (
    -- L is a list of list of ints.
    -- Encode as follows:
    -- #L #L#0 L#0#0 ... #L#1 L#1#0 ... etc
    parts := flatten for i from 0 to #L - 1 list
      prepend(#L#i, L#i);
    prepend(#L, parts)
    )

decodeArrayArrayInt = method()
decodeArrayArrayInt List := List => M -> (
    if #M == 0 then error "expected an encoded list of list of machine length integers"; 
    idx := 1;
    print idx;
    for i from 0 to M#0 - 1 list (
        nelems := M#idx;
        ans := for j from 1 to nelems list M#(idx + j);
        idx = idx + nelems + 1;
        print idx;
        ans
        )
    )

-*
restart
needsPackage "GopakumarVafaInvariants"
*-
TEST ///
  L = {{1,2,3}, {8,9}, {78,79,80}}
  M = encodeArrayArrayInt L
  L' = decodeArrayArrayInt M
  assert(L == L')

  L = {{1,2,3}, {}, {78,79,80}}
  M = encodeArrayArrayInt L
  L' = decodeArrayArrayInt M
  assert(L == L')

  L = {}
  M = encodeArrayArrayInt L
  L' = decodeArrayArrayInt M
  assert(L == L')

  assert try (decodeArrayArrayInt {}; false) else true
  ///

-*
restart
needsPackage "GopakumarVafaInvariants"
*-
-- Sample input for rawGVInvariants (for h11=3 example).
-- {{-1, -1, 0}, {1, 0, 1}, {1, 2, 0}}
-- {}
-- {-3, 2, 4}
-- {{1, 0, 0, -1, -1, -1, -2}, {0, 1, 0, -1, 0, 1, 1}, {0, 0, 1, 1, 1, 1, 0}}
-- {}
-- {{0, 0, 0, 1}, {0, 0, 1, -1}, {0, 1, 1, -1}, {1, 1, 1, -2}, {1, 1, 2, 1}, {1, 2, 2, 3}, {2, 2, 2, 3}}
-- {20, 150, 0, 300000}

TEST ///
mori = encodeArrayArrayInt {{-1, -1, 0}, {1, 0, 1}, {1, 2, 0}}
lightcone = encodeArrayArrayInt {}
gradingvec = {-3, 2, 4}
Q = encodeArrayArrayInt {{1, 0, 0, -1, -1, -1, -2}, {0, 1, 0, -1, 0, 1, 1}, {0, 0, 1, 1, 1, 1, 0}}
nefpart = encodeArrayArrayInt {}
intnums = encodeArrayArrayInt {{0, 0, 0, 1}, {0, 0, 1, -1}, {0, 1, 1, -1}, {1, 1, 1, -2}, {1, 1, 2, 1}, {1, 2, 2, 3}, {2, 2, 2, 3}}
settings = {20, 150, 0, 300000}
settings = {30, 150, 0, 300000}

rawGVInvariants(mori, lightcone, gradingvec, Q, nefpart, intnums, settings)
///

-*
restart
needsPackage "GopakumarVafaInvariants"
*-
TEST ///
-- seems to be id 7, h11=3 from Kreuzer-Skarke.
needsPackage "StringTorics"
      Q = reflexivePolytope(
          {{-1,-1,-1,-1},{-1,-1,-1,0},{-1,-1,0,2},
           {-1,0,-1,-1},{0,-1,-1,-1},{1,-1,0,-1},{1,2,2,2}},
          ID => 7)
      R = ZZ[a,b,c];
      X = makeCY(Q, PicardRing => R, ID => 0)
      GV = gvInvariants(X, DegreeLimit => 10)
      sort pairs GV
ans = {((-1, -1, 1), 1), ((-1, 0, 1), 252), ((-1, 1, 0), 1), ((0, -11, 6), 11),
    ((0, -9, 5), 9), ((0, -7, 4), 7), ((0, -5, 3), 5), ((0, -3, 2), 4),
    ((0, -2, 1), -2), ((0, -2, 2), 8734), ((0, -1, 1), 581), ((0, 0, 1), 198766),
    ((0, 1, 0), 580), ((0, 2, 0), 9912), ((0, 3, -1), 1), ((1, -16, 8), -16),
    ((1, -14, 7), -14), ((1, -12, 6), -12), ((1, -10, 5), -10), ((1, -8, 4), -8),
    ((1, -7, 4), 4130), ((1, -6, 3), -6), ((1, -5, 3), 2930), ((1, -4, 2), -4),
    ((1, -3, 2), 2007), ((1, -2, 1), -1), ((1, -1, 1), 21200), ((1, 0, 0), 126),
    ((1, 1, 0), 20615), ((1, 2, -1), 1), ((1, 2, 0), 2310762), ((1, 3, -1), 252),
    ((1, 4, -1), 5130), ((2, -12, 6), -240), ((2, -10, 5), -70), ((2, -6, 3), 18),
    ((2, -4, 2), 16), ((2, -3, 2), 66744), ((2, -2, 1), 8), ((2, -1, 1), 324805),
    ((2, 0, 0), 126), ((2, 1, 0), 304272), ((2, 3, -1), 5130), ((2, 6, -2), -9252),
    ((3, -8, 4), 328), ((3, -6, 3), 63), ((3, -4, 2), -8), ((3, -2, 1), -11),
    ((3, 0, 0), 108), ((3, 1, 0), 2859290), ((3, 3, -1), 54760), ((4, -4, 2), -88),
    ((4, -2, 1), -4), ((4, 0, 0), 126), ((5, 0, 0), 126)}

mori = encodeArrayArrayInt {{-1, 3, -1}, {0, -2, 1}, {1, 2, -1}}
lightcone = encodeArrayArrayInt {}
gradingvec = {2, 4, 9}
Q = encodeArrayArrayInt {{1, 0, 0, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 1}, {0, 0, 1, 3, 1, 2, 2}}
nefpart = encodeArrayArrayInt {}
intnums = encodeArrayArrayInt {{0, 0, 0, -1}, {0, 1, 1, -2}, {0, 1, 2, 1}, {1, 1, 1, 8}, {1, 1, 2, -4}, {1, 2, 2, 2}, {2, 2, 2, -1}}
settings = {10, 150, 0, 300000}
transpose map(ZZ, rawGVInvariants(mori, lightcone, gradingvec, Q, nefpart, intnums, settings))
///

-- The function to write the data needed by the computeGV program
gvInput = (moriGenerators, heftval, GLSM, intersectionnums, degreelimit, prec) -> (
    -- moriGenerators: list of lists. Hilbert basis of the cone of 
    --   irreducible curves induced from the toric variety.
    -- heftval: list of ints
    -- GLSM: list of list of ints
    -- intersectionnums: list of triples of ints
    -- degreelimit: infinity or positive integer
    -- prec: positive integer
    str1 := toString moriGenerators;
    str3 := toString heftval;
    str4 := toString GLSM;
    str5 := toString intersectionnums;
    str6 := toString ({
            if degreelimit === infinity then -1 else degreelimit, 
            prec,
            0,
            300000
            });
    concatenate between("\n", {str1, toString {}, str3, str4, toString {}, str5, str6})
    )


-* Documentation section *-
beginDocumentation()

doc ///
  Key
    GopakumarVafaInvariants
  Headline
    computing GV invariants of CY 3-fold complete intersections in toric varieties
  Description
    Text
--    Tree
    Example
      {1,2,3}
--  Contributors
--  References
  Caveat
  SeeAlso
--  Subnodes
///

///
  Key
  Headline
  Usage
  Inputs
  Outputs
  Description
    Text
    Example
  References
  Caveat
  SeeAlso
///

TEST /// -* [insert short title for this test] *-
  assert false
///

end--

-* Development section *-
restart
debug needsPackage "GopakumarVafaInvariants"
check "GopakumarVafaInvariants"

uninstallPackage "GopakumarVafaInvariants"
restart
installPackage "GopakumarVafaInvariants"
viewHelp "GopakumarVafaInvariants"
