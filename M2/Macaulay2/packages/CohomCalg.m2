newPackage(
        "CohomCalg",
        Version => "0.8", 
        Date => "24 May 2019",
        Authors => {
            {Name => "Michael E. Stillman", 
             Email => "mike@math.cornell.edu", 
             HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"
             }},
        Headline => "interface to cohomCalg software for computing sheaf cohomology of line bundles on toric varieties",
	Keywords => {"Toric Geometry", "Interfaces"},
        AuxiliaryFiles => true,
        PackageExports => {"NormalToricVarieties"},
        Configuration => {
            "executable" => "",
            "silent" => "true"
            }
        )

-- TODO:
--   add in documentation for these "monomialfile" routines
--   document Silent
--   add in use of --Verbose1, ..., Verbose6.

-- These 3 symbols should perhaps be added to the public interface
protect CohomCalgMonomialFile
--    "useExistingMonomialFile",
--    "useNewMonomialFile",

export {
    "cohomCalg",
    "Silent"
    }

-- cohomCalg X => MutableHashtable (of already computed cohomologies)
-- cohomCalg(X, L) => 
--  if L is a single degree (list of integers, or a toric divisor): return cohomology vector of L
--  if L is a list (of either degree vectors or toric divisors), returns a list of the same length.
-- allow the use of Verbose1, ..., and perhaps monomialfile

-- for backward compatibility
if not programPaths#?"cohomcalg" and
    CohomCalg#Options#Configuration#"executable" != "" then
        programPaths#"cohomcalg" = replace("cohomcalg$", "",
	    CohomCalg#Options#Configuration#"executable")
silent = (
    if CohomCalg#Options#Configuration#"silent" == "false" then false 
    else if CohomCalg#Options#Configuration#"silent" == "true"  then true 
    else error "expected 'silent' configuration value to be \"true\" or \"false\""
    )

-- The following two defs are used only to help parse output from cohomcalg.
Symbol * Symbol := (a,b) -> Product{a,b}
ZZ * Symbol := (a,b) -> Product{a,b}

---------------------------------------------------------------------
-- Routines for translating to form of input expected by cohomcalg --
---------------------------------------------------------------------
displayAsSequence = (L) -> if #L > 1 then toString toSequence L else ("("|toString L#0|")");

toCohomCalg = method()
toCohomCalg NormalToricVariety := (X) -> (
    SR := dual monomialIdeal X;
    GLSM := entries transpose fromWDivToCl X;
    vertices := for i from 0 to #rays X - 1 list (
        -- the string for each variable
        "vertex x"|i|" | GLSM:"|displayAsSequence GLSM_i|";"
        );
    str1 := concatenate between("\n", vertices);
    stanleyreisner := for f in SR_* list (
        concatenate between("*", (support f)/(v -> "x"|toString index v))
        );
    str2 := "\nsrideal [" | concatenate between(",", stanleyreisner) | "];\n";
    str3 :=  if X.cache#?CohomCalgMonomialFile then "monomialfile \""|X.cache#CohomCalgMonomialFile|"\";\n"
             else "monomialfile off;\n";
    str1 | str2 | str3
    )

toCohomCalg List := (L) -> (
    concatenate for p in L list (
        "ambientcohom O"|(displayAsSequence p)|";\n"
        )
    )

cohomCalgProgram = null

-- The actual call to cohomcalg
-- Also, cohomcalg has a limit of 1024 computations at a time
cohomCalg0 = (X,pneeded,issilent) -> (
    if cohomCalgProgram === null then
        cohomCalgProgram = findProgram("cohomcalg",
	    "cohomcalg " | prefixDirectory |
	    replace("PKG", "CohomCalg", currentLayout#"package") | "dP1.in");
    -- X: NormalToricVariety
    -- pneeded: list of multi-degrees, of size <= 1024
    -- issilent: Boolean, whether to quiet the output of cohomcalg.
    H := X.cache.CohomCalg;
    filename := temporaryFileName();
    filename << (toCohomCalg X) | (toCohomCalg pneeded) << close;
    executable := runProgram(cohomCalgProgram, "--integrated " | filename |
	" | tail -1");
    if debugLevel >= 1 then << "-- CohomCalg: using temporary file " << filename << endl;
    if debugLevel >= 2 then << "-- CohomCalg: going to call: " << executable#"command" << endl;
    if not issilent then << executable#"error";
    valstr := executable#"output";
    if debugLevel >= 5 then << "-- CohomCalg: output = " << net valstr << endl;
    --val := replace("\\*", "**", valstr);
    val := replace("False", "false", valstr);
    val = replace("True", "true", val);
    result := value val;        
    if not instance(result, List) or not instance(first result, Boolean) then 
        error("result of cohomCalg not in expected form: received: "|valstr);
    if not first result then
        error("cohomCalg returned false, meaning it could not complete the computation");
    -- remove the 'true': start at i==1.
    for i from 1 to #result-1 do H#(pneeded#(i-1)) = result#i;
    H
    )

useExistingMonomialFile = method()
useExistingMonomialFile(NormalToricVariety, String) := String => (X, monomialfile) -> (
    if not fileExists monomialfile then error "expected a name of an existing file";
    X.cache#CohomCalgMonomialFile = monomialfile;
    monomialfile
    )

useNewMonomialFile = method()
useNewMonomialFile(NormalToricVariety, String) := String => (X, monomialfile) -> (
    if fileExists monomialfile then (
        << "removing file: " << monomialfile << endl;
        removeFile monomialfile;
        );
    X.cache#CohomCalgMonomialFile = monomialfile;
    monomialfile
    )
useNewMonomialFile NormalToricVariety := String => X -> useNewMonomialFile(X, temporaryFileName())

cohomCalg = method(Options=>{Silent=>null})
cohomCalg NormalToricVariety := MutableHashTable => opts -> X -> (
    if not X.cache.?CohomCalg then X.cache.CohomCalg = new MutableHashTable;
    X.cache.CohomCalg
    )
cohomCalg(NormalToricVariety, List) := List => opts -> (X, p) -> (
    if all(p, p1 -> instance(p1, ZZ)) 
    then first cohomCalg(X, {p}, opts)
    else (
        H := cohomCalg(X, opts);
        issilent := if opts.Silent === null then silent else opts.Silent;
        ndegs := # degree(X_0);
        p = for a in p list (
            da := if instance(a,ToricDivisor) 
                  then degree a
                  else if instance(a,List) then a
                  else error "expected lists of degrees and/or toric divisors";
            if #da != ndegs or not all(da, x -> instance(x, ZZ))
            then error("expected degrees of size " | ndegs);
            da
            );
        pneeded := toList(set p - set keys X.cache.CohomCalg); 
        if #pneeded > 0 then (
            p1 := pack(pneeded, 1024);
            for p0 in p1 do cohomCalg0(X,p0,issilent);
            );
        for d in p list first H#d
        )
    )
cohomCalg ToricDivisor := List => opts -> D -> first cohomCalg(variety D, {degree D}, opts)
cohomCalg(NormalToricVariety, ToricDivisor) := List => opts -> (X, D) -> (
    if X =!= variety D then error "wrong variety for toric divisor";
    first cohomCalg(X, {degree D}, opts)
    )

beginDocumentation()

doc ///
Key
  CohomCalg
Headline
  an interface to the CohomCalg software for computing cohomology of torus invariant divisors on a toric variety
Description
  Text
     CohomCalg is software written by Benjamin Jurke and Thorsten Rahn (in collaboration with 
     Ralph Blumenhagen and Helmut Roschy) for computing the cohomology vectors of torus
     invariant divisors on a (normal) toric variety (see @HREF "https://github.com/BenjaminJurke/cohomCalg"@ for 
     more information).
  Text
     CohomCalg is an efficient and careful implementation.
     One limitation is that the number of rays in the fan
     and the number of generators of the Stanley-Reisner ideal of the fan must both be no larger
     than 64.
  Text
     Here is a sample usage of this package in Macaulay2.  Let's compute the cohomology of some 
     divisors on a smooth Fano toric variety.
  Example
     needsPackage "NormalToricVarieties"
     X = smoothFanoToricVariety(3,15)
     rays X
     max X
     S = ring X
     SR = dual monomialIdeal X
     KX = toricDivisor X
     assert isVeryAmple (-KX)
     cohoms1 = for i from 0 to 6 list X_i => cohomCalg X_i
     cohoms2 = for i from 0 to 6  list X_i => (
         for j from 0 to dim X list rank HH^j(X, OO_X(toSequence degree X_i))
         )
     assert(cohoms1 === cohoms2)
  Text
     For efficiency reasons, it is better, if this works for your use, to 
     call CohomCalg by batching together several cohomology requests.
  Example
     needsPackage "ReflexivePolytopesDB"
     topes = kreuzerSkarke(21, Limit => 20);
     A = matrix topes_10
     P = convexHull A
     X = normalToricVariety P
     SR = dual monomialIdeal X
     
     D2 = subsets(for i from 0 to #rays X - 1 list (-X_i), 2)
     D2 = D2/sum/degree
     elapsedTime hvecs = cohomCalg(X, D2)
     
     peek cohomCalg X
     
     degree(X_3 + X_7 + X_8)
     elapsedTime cohomvec1 = cohomCalg(X_3 + X_7 + X_8)
     elapsedTime cohomvec2 = for j from 0 to dim X list rank HH^j(X, OO_X(0,0,1,2,0,-1))
     assert(cohomvec1 == cohomvec2)
     
     degree(X_3 + X_7 - X_8)
     elapsedTime cohomvec1 = cohomCalg(X_3 + X_7 - X_8)
     elapsedTime cohomvec2 = elapsedTime for j from 0 to dim X list rank HH^j(X, OO_X(0,0,1,2,-2,-1))
     assert(cohomvec1 == cohomvec2)
  Text
     @TO "cohomCalg"@ computes cohomology vectors by calling CohomCalg.  It also stashes 
     it's results in the toric variety's cache table, so computations need not be performed twice.
SeeAlso
  cohomCalg
///


doc ///
Key
  (cohomCalg, NormalToricVariety)
Headline
  locally stashed cohomology vectors from CohomCalg
Usage
  H = cohomCalg X
Inputs
  X:NormalToricVariety
  Silent => Boolean
    Not used in this particular method
Outputs
  H:MutableHashTable
Consequences
  Item
    The mutable hash table is stashed into {\tt X.cache#CohomCalg}.  If it doesn't
    exist yet, it is created.
Description
  Text
    The keys of this hash table are the divisor classes (degrees) whose
    cohomology vector has already been computed.  The value of the hash table for
    this key is a list of two things: the cohomology vector, and a list representing
    the denominators which appear for this degree.
  Example
     needsPackage "ReflexivePolytopesDB"
     topes = kreuzerSkarke(5, Limit => 20);
     A = matrix topes_15
     P = convexHull A
     X = normalToricVariety P
     H = cohomCalg X
  Text
     Notice that the hash table {\tt H} is empty, as we haven't tried computing any cohomology
     vectors yet.
  Example
     cohomCalg(X, {-4, 10, -9})
     for i from 0 to dim X list rank HH^i(X, OO_X(-4, 10, -9))
     peek cohomCalg X
SeeAlso
  cohomCalg
///

doc ///
Key
  cohomCalg
  (cohomCalg, NormalToricVariety, List)
  (cohomCalg, ToricDivisor)
  (cohomCalg, NormalToricVariety, ToricDivisor)
  [cohomCalg, Silent]
Headline
  compute cohomology vectors using the CohomCalg software
Usage
  cvecs = cohomCalg(X, L)
  cvecs = cohomCalg D
Inputs
  X:NormalToricVariety
  L:List
    of toric divisors, or degrees, or a single degree, or a toric divisor
  D:ToricDivisor
    a single toric divisor
  Silent => Boolean
    but if null, then the default value from the configuration file is overridden by this value.
    If true, then all output from CohomCalg is suppressed.
Outputs
  cvecs:List
    The list of cohomology vectors of the toric divisors in L (or a single list, if the input
        was a single degree or toric divisor)
Consequences
  Item
    The answers are stashed into the mutable hashtable {\tt X.cache#CohomCalg},
    and are not recomputed if possible.
Description
  Text
    Given a toric divisor D, or its degree d, its cohomology vector is the
    list $\{h^0(X, OO_X(d)), h^1(X, OO_X(d)), \ldots, h^{dim X}(X, OO_X(d)) \}$.
  Text
    Here is an example.
  Example
     needsPackage "ReflexivePolytopesDB"
     topes = kreuzerSkarke(5, Limit => 20);
     A = matrix topes_15
     P = convexHull A
     X = normalToricVariety P
     D = 3 * X_0 - 5 * X_4
     cohomCalg(D, Silent => true)
  Text
     We compare this to the results from @TO "NormalToricVarieties::NormalToricVarieties"@.
  Example
     cohomCalg(X, D)
     for i from 0 to dim X list rank HH^i(X, OO D)
     peek cohomCalg X
SeeAlso
  (cohomCalg, NormalToricVariety)
  "ReflexivePolytopesDB::ReflexivePolytopesDB"
///


TEST ///
  needsPackage "CohomCalg"
  X = smoothFanoToricVariety(3,2)
  ans = for i from 0 to # rays X - 1 list cohomCalg X_i
  assert(ans === {{3, 0, 0, 0}, {3, 0, 0, 0}, {3, 0, 0, 0}, {4, 0, 0, 0}, {1, 0, 0, 0}})
  
  degrees ring X
  ans1 = {0,0,1,0}
  D = X_3-3*X_0  
  assert(degree D == {1,-3})
  assert(cohomCalg(X, {1,-3}) == ans1)
  assert(cohomCalg D == ans1)
  assert(ans1 == for i from 0 to dim X list rank HH^i(X, OO D))

  assert(cohomCalg(D-D) == {1,0,0,0})
///

TEST ///
  needsPackage "CohomCalg"
  X = smoothFanoToricVariety(3,2)
  Ds = for i from 0 to # rays X - 1 list X_i
  allDs = (drop(subsets Ds,1))/sum/(d -> -d);
  H = cohomCalg(X,allDs)
  peek H
  peek cohomCalg(X, {{0,-6}})
///


TEST ///
-- of internal creation routines
-*
restart
*-
  debug needsPackage "CohomCalg"
  X = smoothFanoToricVariety(3,2)

  assert(toCohomCalg X === 
"vertex x0 | GLSM:(0,1);
vertex x1 | GLSM:(0,1);
vertex x2 | GLSM:(0,1);
vertex x3 | GLSM:(1,0);
vertex x4 | GLSM:(1,-1);
srideal [x0*x1*x2,x3*x4];
monomialfile off;
")

  Ds = for i from 0 to # rays X - 1 list X_i
  allDs = (drop(subsets Ds,1))/sum/(d -> -d);
  degs = take(allDs/degree, 5)
  assert(
      toCohomCalg degs
      ===
"ambientcohom O(0,-1);
ambientcohom O(0,-1);
ambientcohom O(0,-2);
ambientcohom O(0,-1);
ambientcohom O(0,-2);
")

  debugLevel = 5
  hvecs = cohomCalg(X,allDs)

  debugLevel = 1  
  hvecs = cohomCalg(X,allDs)

  debugLevel = 2
  hvecs = cohomCalg(X,allDs)

  debugLevel = 3
  hvecs = cohomCalg(X,allDs)
  
  -- compare these results to NormalToricVarieties:
  H = cohomCalg X
  for d in keys H do (
    assert(first H#d === for i from 0 to dim X list rank HH^i(X, OO_X(toSequence d)))
    )
///


TEST ///
-- of internal creation routines, harder example
-*
restart
*-
  debug needsPackage "CohomCalg"
  X = smoothFanoToricVariety(4,100)

--  useNewMonomialFile X
  assert(drop(lines toCohomCalg X, -1) === 
{"vertex x0 | GLSM:(-1,-2,0,0,0);",
"vertex x1 | GLSM:(1,0,0,1,0);",
"vertex x2 | GLSM:(1,0,1,0,0);",
"vertex x3 | GLSM:(0,1,0,0,0);",
"vertex x4 | GLSM:(0,1,0,0,0);",
"vertex x5 | GLSM:(0,1,0,0,0);",
"vertex x6 | GLSM:(0,0,-1,0,1);",
"vertex x7 | GLSM:(0,0,0,-1,1);",
"vertex x8 | GLSM:(0,0,1,1,-1);",
"srideal [x1*x2,x3*x4*x5,x0*x6,x1*x6,x0*x7,x2*x7,x6*x7,x0*x8,x1*x8,x2*x8];"
})

  Ds = for i from 0 to # rays X - 1 list X_i
  allDs = (drop(subsets Ds,1))/sum/(d -> -d);
  allDs = take(allDs, 100);
  degs = take(allDs/degree, 5)
  assert(
      toCohomCalg degs
      ===
"ambientcohom O(1,2,0,0,0);
ambientcohom O(-1,0,0,-1,0);
ambientcohom O(0,2,0,-1,0);
ambientcohom O(-1,0,-1,0,0);
ambientcohom O(0,2,-1,0,0);
")

  hvecs = cohomCalg(X,allDs)
  cohomCalg(X, {{-1,0,-1,0,0}})
  
  cohomCalg(X, {{2,3,-4,0,-3}})
  
  -- compare these results to NormalToricVarieties:
  H = cohomCalg X
  for d in keys H do (
    assert(first H#d === for i from 0 to dim X list rank HH^i(X, OO_X(toSequence d)))
    )
///




end--

restart
uninstallPackage "CohomCalg"
restart
needsPackage "CohomCalg"
installPackage "CohomCalg"
check "CohomCalg"
