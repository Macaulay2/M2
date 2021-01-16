--*- coding: utf-8 -*-

------------------------------------------------------------------------------
-- PURPOSE : Check positivity of toric vector bundles
-- PROGRAMMER : Andreas Hochenegger
------------------------------------------------------------------------------

newPackage("PositivityToricBundles",
           Headline => "check positivity of toric vector bundles",
           Version => "1.1",
           Date => "June, 2020",
           Authors => { 
            {Name => "Andreas Hochenegger",
             Email => "andreas.hochenegger@sns.it"}},
           Keywords => {"Toric Geometry"},
           Configuration =>{},
           PackageExports => {"ToricVectorBundles"}
          )

export {
        "groundSet", 
        "parliament",
        "compatibleBases",
        "toricChernCharacter",
        "graphToricChernCharacter",
        "separatesJets",
        "isGloballyGenerated",
        "restrictToInvCurves",
        "isNef",
        "isAmple",
        "drawParliament2Dtikz",
-- Options
        "Verbosity",
        "DrawCohomology",
        "DrawChernCharacter"
        }

-- cacheValues
protect basesSortedByFiltrations 
protect filtrationFlags
protect restrictionsToInvCurves
-- Options
protect DrawCohomology
protect DrawChernCharacter
protect Verbosity

---------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--
-- Copyright 2020 Andreas Hochenegger
--
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
---------------------------------------------------------------------------


------------------------------------------------------------------------------
-- CAVEATS
------------------------------------------------------------------------------
-- 1) [K,RJS,P] uses decreasing filtration for Klyachko's description,
--    in contrast to the package 'ToricVectorBundles' (increasing filtration).
--    Source of confusion about different signs.
-- 2) We use same sign convention as in ToricVectorBundles, i.e.
--    given a polytope, the associated fan consists of inner normals
--    ([RJS,P] uses outer normals)
--    Another source of confusion about signs.
-- 3) We implicitly assume that the variety is at least complete and also smooth.
--    This is not tested, so  methods might break or results might become 
--    meaningless for varieties which are not complete or not smooth.
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- METHOD: groundSet
------------------------------------------------------------------------------
-- AUXILIARY METHODS FOR groundSet:
-- distinctModules, sortByDim, intersectAll, signum
------------------------------------------------------------------------------

-- PURPOSE : Given a list, remove duplicates
--           the built-in function 'unique' is too strict here:
--           it uses '===' instead of '==', so it distinguishes between
--           e.g. <(-1,0)> and <(1,0)>
--   INPUT : List of submodules
--  OUTPUT : List of distinct submodules
distinctModules = method ()
distinctModules (List) := L -> (
 if #L == 1 then
  return L;
 for i from 0 to #L-2 do 
  for j from i+1 to #L-1 do
   -- 'rank' is already stored (so comes for free)
   if rank L_i == rank L_j then
    -- '==' quite expensive computation (in contrast to 'rank')
    if image promote( generators L_i, QQ) == image promote( generators L_j, QQ) then
     return distinctModules drop(L,{j,j});
 L
)

-- PURPOSE : Given a list of modules, 
--           order it by rank
--   INPUT : List of modules
--  OUTPUT : List of modules ordered by rank
sortByDim = method()
sortByDim (List) := L -> (
 if #L == 1 then return L;
 for i from 0 to #L-2 do 
  if rank L_(i+1) < rank L_i then
   return sortByDim switch(i,i+1,L);
 L
)

sortColumnsByFiltration = method()
sortColumnsByFiltration (Matrix,Matrix) := (base,filtration) -> (
 if #base == 1 then return (base,filtration);
 n := numgens source base;
 for i from 0 to n-2 do
  if filtration_(0,i+1) < filtration_(0,i) then (
   reorder := toList switch(i,i+1, 0 ..< n);
   return sortColumnsByFiltration( base_reorder, filtration_reorder )
  );
 (base,filtration)
)

sortBasesByFiltrations = method()
sortBasesByFiltrations (ToricVectorBundleKlyachko) := (cacheValue symbol basesSortedByFiltrations) ( tvb -> (
 bases := base tvb;
 filtrations := filtration tvb;
 reordered := hashTable apply(rays tvb, ray -> ray=>sortColumnsByFiltration(bases#ray,filtrations#ray));
 ( hashTable apply (rays tvb, ray -> ray=>reordered#ray#0), 
   hashTable apply (rays tvb, ray -> ray=>reordered#ray#1) )
))

flags = method()
flags (ToricVectorBundleKlyachko) := (cacheValue symbol filtrationFlags)( tvb -> (
 (bases,filtrations) := sortBasesByFiltrations tvb;

 hashTable for ray in rays tvb list (
  degrees := flatten entries filtrations#ray;
  ray => for j in 0 ..< #degrees list (
   if j < #degrees-1 and degrees_j == degrees_(j+1) then continue;
   submatrix(bases#ray, 0 .. j)
  )
 )
))

iterateIntersection = method()
iterateIntersection (Module, List) := (mod, flagMats) -> (
 if #flagMats == 1 then 
  return flatten apply(flagMats_0, m -> intersect( mod, image m));
 return flatten apply(flagMats_0, m -> iterateIntersection( intersect(mod, image m), drop(flagMats,1)))
)

-- PURPOSE : Given a toric vector bundle in Klyachko's description,
--           compute all intersections of the filtrations,
--           this will be the set L(E) of [RJS, Section 3]
--   INPUT : 'tvb' toric vector bundle
--  OUTPUT : hashTable of submodules (rk => modules of rank rk)
intersectAll = method()
intersectAll (ToricVectorBundleKlyachko) := tvb -> (
 bases := (sortBasesByFiltrations tvb)#0;
 flagMats := values flags tvb;
 intAll := flatten for mat in flagMats_0 list
  iterateIntersection(image mat, drop(flagMats,1) );
 intAll = sortByDim distinctModules intAll;
 partition(i -> rank i, intAll)
)

-- PURPOSE : Given a number, compute its sign
signum = n -> (if n>0 then return 1; if n<0 then return -1; 0)

-- MAIN METHOD: groundSet ----------------------------------------

-- PURPOSE : Given a toric vector bundle in Klyachko's description,
--           compute the ground set of the associated matroid
--           IMPORTANT: Due to the implementation, elements might appear several times
--   INPUT : 'tvb', a ToricVectorBundleKlyachko
--  OUTPUT : ground set (list of nx1-matrices)
groundSet = method( Options => true )
groundSet (ToricVectorBundleKlyachko) :=  {Verbosity => 0} >> opts -> (cacheValue groundSet) (tvb -> (
 if opts#Verbosity>0 then << "METHOD: groundSet" << endl;
 intersections := intersectAll tvb;
 if opts#Verbosity>0 then << "Intersections L(tvb):" << endl << intersections << endl;
 G := if intersections#?1 then intersections#1 else {};
 if opts#Verbosity>0 then << "G := spaces of dimension 1:" << endl << G << endl;
 for i in keys intersections do (
  if i <= 1 then continue;
  if opts#Verbosity>0 then << "Intersecting with spaces of dimension " << i << ":" << endl;
  sumG := sum G;
  for subspace in intersections#i do (
   if sumG == 0 or rank intersect {sumG, subspace} < i+1 then (
    if opts#Verbosity>0 then << "Following space is not contained: " << endl << subspace << endl;
    for j from 0 to numgens subspace-1 do (
     -- is there an easier way to turn it into a module?
     vecj := matrix (generators subspace)_j;
     if sumG == 0 or not isSubset(image promote(vecj, QQ), image promote( generators sumG, QQ )) then (
      if opts#Verbosity>0 then << "Add to G the vector: " << endl << vecj << endl;
      G = append(G, image vecj);
     );
    );
   );
  );
 );

 G = apply(G, generators);
 -- Macaulay2 has a strong tendency to choose vectors with negative entries
 for g in G list (
  if opts#Verbosity>0 then << "Maybe we have to adjust vector: " << endl << g << endl;
  m := 1;
  if instance(g_(0,0),QQ) then 
   m = lcm apply(flatten entries g, denominator);
  if opts#Verbosity>0 then if m > 1 then << "multiply with common denominator " << m << endl;
  gmod := m*g;
  m = gcd flatten entries gmod;
  gmod = lift(1/m*promote(gmod,QQ),ZZ);
  sgn := fold((i,j) -> i + signum(j), 0, flatten entries transpose gmod);
  if sgn < 0 then 
   gmod = -gmod;
  if opts#Verbosity>0 then if not g == gmod then << "changed to: " << endl << gmod << endl;
  gmod
 )
))

------------------------------------------------------------------------------
-- METHOD: parliament
------------------------------------------------------------------------------

-- PURPOSE : Given a toric vector bundle in Klyachko's description
--           and the ground set of the associated matroid,
--           compute the parliament of polytopes [RJS]
--   INPUT : 'tvb', a ToricVectorBundleKlyachko
--           'gs', ground set
--  OUTPUT : parliament of polytopes, hash table (ground set => parliament)
parliament = method( Options => true )
parliament (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (cacheValue parliament)( tvb -> (
 gs := groundSet(tvb, Verbosity=>(opts#Verbosity-1));
 if opts#Verbosity>0 then << "METHOD: parliament" << endl;
 (basetvb,filttvb) := sortBasesByFiltrations tvb;
 if opts#Verbosity>0 then << "Bases used: " << endl << basetvb << endl;
 rk := rank tvb;
 index := -infinity;
 coeffMat := matrix flatten apply( keys basetvb, b-> entries transpose b );

 hashTable for e in gs list (
  if opts#Verbosity>0 then << "For element of ground set:" << endl << e << endl;
  maxima := transpose matrix {
   for ray in keys basetvb list (
    for i in 0 ..< rk do (
     if rank(submatrix(basetvb#ray,0 .. i)) != rank(submatrix(basetvb#ray,0 .. i) | e) then (
      continue;
     );
     index = filttvb#ray_(0,i);
     break;
    );
   index
   ) 
  };
  -- IMPORTANT: the minus sign on the rhs is there because,
  -- [RJS] uses decreasing filtration in contrast to ToricVectorBundle
  -- the minus sign on the lhs is to have same sign convention as in ToricVectorBundles
  polytope := polyhedronFromHData(-coeffMat, -maxima);
  if opts#Verbosity>0 then << "the equations for the polytope are:" << endl <<  coeffMat << "*x >= " << maxima << endl;
  if opts#Verbosity>0 then << "the associated polytope has vertices:" << endl << vertices polytope << endl;
  e => polytope
 )
))

------------------------------------------------------------------------------
-- METHOD: compatibleBases
------------------------------------------------------------------------------
-- AUXILIARY METHODS FOR compatibleBases:
-- distinctLines, possibleFlags, removeChosenBasisVectorsFromFlags, compatibleBasis
------------------------------------------------------------------------------

-- PURPOSE: Given a toric vector bundle in Klyachko's description and
--          given the ground set of the associated matroid,
--          compute all possible flags for this filtration using vectors of the ground set
--   INPUT: 'tvb', toric vector bundle
--          'gs', ground set
--  OUTPUT: hash table: ray => possible flags (i-th entry is list of possible i-th basis vectors)
possibleFlags = method()
possibleFlags (ToricVectorBundleKlyachko, List) := (tvb, gs) -> (
 flagTable := flags tvb;

 hashTable for ray in rays tvb list (
  vecs := gs;
  ray => for filtMat in flagTable#ray list (
   parted := partition(e -> isSubset(image promote(matrix e, QQ), image promote(filtMat, QQ)), vecs);
   vecs = if parted#?false then parted#false else {};
   if parted#?true then parted#true else continue
  )
 )
)

-- PURPOSE : Given a list, remove duplicates
--           the built-in function 'unique' is too strict here:
--           it uses '===' instead of '==', so it distinguishes between
--           e.g. <(-1,0)> and <(1,0)>
--   INPUT : List of one column matrices
--  OUTPUT : List of one column matrices
distinctLines = method ()
distinctLines (List) := L -> (
 if #L == 1 then
  return L;
 for i from 0 to #L-2 do 
  for j from i+1 to #L-1 do
    if image promote( L_i,QQ) == image promote( L_j,QQ) then
     return distinctLines drop(L,{j,j});
 L
)

-- PURPOSE: Helper method for compatibleBasis
--          if vectors are chosen for compatible basis,
--          remove these from flag (and all other vectors which lie in same span)
removeChosenBasisVectorsFromFlags := (compatB,possFlagsList) -> (
 for possFlag in possFlagsList list (
  for i in 0..< #possFlag list (
   vecs :=for p in possFlag_i list (
    addp := true;
    for b in compatB do (
     matB := if i==0 then b else fold( apply(take(possFlag,i), pB -> matrix pB_0), (i,j) -> i|j) | b;
     if isSubset(image promote( matrix p, QQ), image promote(matB, QQ)) then (
      addp = false;
      break;
     )
   );
    if not addp then continue;
    p
   );
   if #vecs==0 then continue;
   vecs
  )
 )
)

-- PURPOSE: Given the possible flags (as computed by possibleFlags) for rays of maximal cone
--          compute a common set of basis vectors
--   INPUT: 'possFlagsList', list of possible flags
--  OUTPUT: compatible base (list of vectors)
compatibleBasis = method( Options => true )
compatibleBasis (ZZ, List) := {Verbosity => 0} >> opts -> (n,possFlagsList) -> (
 if opts#Verbosity>0 then << "METHOD: compatibleBasis" << endl;

 compatB := flatten for possFlags in possFlagsList list 
  flatten select(possFlags, b -> #b==1);
 compatB = distinctLines apply(compatB, matrix);
 if opts#Verbosity>0 then << "From the possible flag we have to take the unique: " << endl << compatB << endl;
 
 while #compatB  < n do (
  if opts#Verbosity>0 then << "These are not enough, we are missing " << (n - #compatB) << " element(s)." << endl;
  remainingB := removeChosenBasisVectorsFromFlags(compatB,possFlagsList);
  if opts#Verbosity>0 then (<< "Removing the already chosen elements, we get: " << endl; for r in remainingB do << r << endl);
  rB0 := remainingB_0;
  rBrest := drop(remainingB,1);
  for i0 in 0 ..< #remainingB_0 do (
   for v0 in remainingB_0_i0 do (
    addv := true;
    for k in 1 ..< #remainingB do (
     foundv := false;
     for ik in 0 ..< #remainingB_k do (
      for vk in remainingB_k_ik do (
       if image promote(matrix v0,QQ) == image promote(matrix vk,QQ) then ( 
        foundv = true;
        break;
       );
      );
      if foundv then break;
     );
     if not foundv then ( 
      addv = false;
      break;
     )
    );
    if addv then (
     if opts#Verbosity>0 then << "Add following vector to compatible basis: " << endl << v0 << endl;
     compatB = append(compatB, matrix v0);
     break;
    )
   )
  )
 );

 matrix fold(compatB, (i,j) -> matrix i | matrix j)
)

-- MAIN METHOD: compatibleBases ----------------------------------------------

-- PURPOSE: Given a toric vector bundle in Klyachko's description and
--          given the ground set of the associated matroid,
--          compute list of compatible bases as in [RJS, Section 3]
--   INPUT: 'tvb', toric vector bundle
--          'gs', ground set
--  OUTPUT: hash table: max cone => compatible basis
compatibleBases = method ( Options => true )
compatibleBases (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (cacheValue symbol compatibleBases) (tvb -> (
 gs :=  groundSet(tvb, Verbosity=>(opts#Verbosity-1) );
 if opts#Verbosity>0 then << "METHOD: compatibleBases" << endl;
 maxcones := apply(maxCones tvb, rays);
 basetvb := (sortBasesByFiltrations tvb)#0;
 compatibleBs := hashTable for sigma in maxcones list (
  if opts#Verbosity>0 then << "For maximal cone:" << endl << sigma << endl;
  possFlagsTable := possibleFlags(tvb,gs);
  possFlagsSigma := for i in 0 ..< numgens target sigma list possFlagsTable#(matrix sigma_i);
  
  if opts#Verbosity>0 then (<< "possible flags are:" << endl; for pB in possFlagsSigma do  << pB << endl);
  cB := compatibleBasis(rank tvb, possFlagsSigma, Verbosity=>(opts#Verbosity-1));
  if opts#Verbosity>0 then << "actual compatible basis is:" << endl << cB << endl;
  sigma => cB
 )
))

------------------------------------------------------------------------------
-- toricChernCharacter
------------------------------------------------------------------------------

-- PURPOSE : Given a toric vector bundle in Klyachko's description and
--           given compatible bases as computed by compatibleBases,
--           compute the toric Chern character as introduced in [Payne] 
--   INPUT : 'tvb', toric vector bundle
--           'compBases', compatible bases
--  OUTPUT : hash table: max cone => points of toric Chern character.
toricChernCharacter = method( Options => true )
toricChernCharacter (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (cacheValue toricChernCharacter) ( tvb -> (
 compBases := compatibleBases(tvb, Verbosity=>(opts#Verbosity-1));
 if opts#Verbosity>0 then << "METHOD: toricChernCharacter" << endl;
 (basetvb,filttvb) := sortBasesByFiltrations tvb;

 hashTable for cb in pairs compBases list (
  maxcone := cb_0;
  if opts#Verbosity>0 then << "For maximal cone sigma: " << endl << maxcone << endl;
  base := cb_1;
  if opts#Verbosity>0 then << "the compatible basis is: " << endl << base << endl;

  cb_0 => for k in 0 ..< rank tvb list (
   if opts#Verbosity>0 then << "To compute " << (k+1) << "-th part of u(sigma), check when " << endl << base_k << endl << "is contained in first columns of: " << endl;
   RHS := {};
   for i in 0 ..< numgens source maxcone do (
    basemati := promote(basetvb#(matrix maxcone_i),QQ);
    if opts#Verbosity>0 then << "filtration basis of " << i << "-th ray of sigma:" << endl <<  basemati << endl;
    for j in 0 ..< numgens source basemati do (
     if opts#Verbosity>0 then << j+1 << " ";
     if isSubset(image promote(matrix base_k,QQ), image submatrix(basemati, 0 .. j)) then (
      RHS = append(RHS, filttvb#(matrix maxcone_i)_(0,j));
      break;
     )
    );
    if opts#Verbosity>0 then << "columns" << endl;
   );
   if opts#Verbosity>0 then << "Equation to solve: " << maxcone << " *u =" << vector RHS << endl;
   u := solve(transpose maxcone, transpose matrix {RHS});
   if opts#Verbosity>0 then << "and the component of Chern character: " << endl << u  << endl;
   u
  )
 )
))

-- PURPOSE : Given a toric vector bundle in Klyachko's description and
--           its toric Chern character,
--           connect components in adjacent maximal cones by lines
--   INPUT : 'tvb', toric vector bundle
--           'torChern', toric Chern character
--  OUTPUT : hash table: cone of codim 1 (curve) => list of pairs
graphToricChernCharacter = method( Options => true )
graphToricChernCharacter (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (cacheValue graphToricChernCharacter) ( tvb -> (
 torChern := toricChernCharacter(tvb, Verbosity=>opts#Verbosity);
 if opts#Verbosity>0 then  << "METHOD: graphToricChernCharacter" << endl;
 F := fan tvb;
 n := dim F;
 curveCones := apply(cones(n-1,F), c -> (rays F)_c);
 hashTable for tau in curveCones list (
  if opts#Verbosity>0 then  << "For curve cone: " << endl << tau << endl;
  sigmas := {};
  normal := kernel transpose tau;
  if opts#Verbosity>0 then  << "has normal: " << endl << normal << endl;
  for sigma in keys torChern do
   if contains(coneFromVData sigma, coneFromVData tau) then
     sigmas = append(sigmas,sigma);
  if opts#Verbosity>0 then  << "the adjacent maximal cones are: " << endl << sigmas << endl;
  edges := {};
  for u1 in torChern#(sigmas_0) do ( 
   for u2 in torChern#(sigmas_1) do (
    if isSubset(image(u1-u2), normal) then (
     if opts#Verbosity>0 then  << "connect Chern components: " << endl << {u1,u2} << endl;
     edges = append(edges, {u1,u2});
    ); 
   );
  );
  tau => edges
 )
))

------------------------------------------------------------------------------
-- METHOD: separateJets
------------------------------------------------------------------------------

-- PURPOSE : Given a toric vector bundle in Klyachko's description,
--           its parliament bases and toric Chern character
--           compute the maximal l such that the bundle separates l-jets
--   INPUT : 'tvb', toric vector bundle
--           'parliament', parliament of polytopes
--           'torChern', toric Chern character
--  OUTPUT :  Integer, -1 if not separates any l-jets, otherwise l
separatesJets = method( Options => true )
separatesJets (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (cacheValue separatesJets) ( tvb -> (
 if opts#Verbosity>0 then << "METHOD: separatesJets" << endl;
 parPolytopes := parliament(tvb, Verbosity=>(opts#Verbosity-1));
 torChern := toricChernCharacter(tvb, Verbosity=>(opts#Verbosity-1));
-- exclude another trivial case
 if min apply(values parPolytopes, dim) < 0 then (
  if opts#Verbosity>0 then << "There are empty polytopes, so not even 0-jets are separated." << endl;
  return -1;
 );
 parVertices := applyValues(parPolytopes, vertices);
 n := dim fan tvb;
 onefaces := applyValues(applyValues(parPolytopes, p -> faces(n-1, p)), L -> apply(L, l -> l#0));
-- for each maximal cone sigma
-- the following searches for each component u of the character u(sigma)
-- all polytopes parParliament#g with g in ground set such that u is j0-th vertex
-- then checks whether the cone at u is (degenerate form of) dual cone of sigma
-- and computes the edge lengths
 uPositions := hashTable for sigma in keys torChern list (
  usigma := torChern#sigma;
  if opts#Verbosity>0 then << "For the maximal cone" << endl << sigma << endl;
  sigma => hashTable for u in usigma list (
   if opts#Verbosity>0 then << "the component " << u << " of the associated character" << endl;
   uPos := for g in keys parPolytopes list (
    j0 := -1;
    for j in 0 ..< numgens source parVertices#g do (
     if flatten entries u == entries parVertices#g_j then (
      j0 = j;
      break;
     )
    );
    if j0 >= 0 then (
     if #(onefaces#g) == 0 then (
      if opts#Verbosity>0 then << "is a vertex of the point polytope " << endl << parVertices#sigma << endl;
      if opts#Verbosity>0 then << "with minimal edge length: 0" << endl;
      {g, j0,0}
     )
     else (
      coneAtU := fold( apply( flatten select(apply(onefaces#g, f -> delete(j0,f)), f-> #f == 1), f -> parVertices#g_f - parVertices#g_j0 ), (a,b) -> matrix a| matrix b);
      if opts#Verbosity>0 then << "is a vertex of the polytope " << endl  << parVertices#g << endl << "with cone at corner " << endl << coneAtU << endl;
      dualSigma := coneFromHData transpose matrix sigma;
      if isFace(coneFromVData coneAtU, dualSigma) then (
       if opts#Verbosity>0 then << "is subcone of dual cone of sigma " << endl << rays dualSigma << endl;
 -- Here: gcd = lattice length of vector
       l := min apply(entries transpose coneAtU, gcd);
       if opts#Verbosity>0 then << "with minimal edge length: " << l << endl;
       {g, j0,l}
      )
      else (
       if opts#Verbosity>0 then << "is not a subcone of dual cone of sigma " << endl << rays dualSigma << endl;
       continue
      )
     )
    )
    else 
     continue
   );
   if #uPos == 0 then (
    if opts#Verbosity>0 then << "is not a vertex of any polytope, so not even 0-jets are separated." << endl;
    return -1;
   );
-- At this point: in uPos list for every u in usigma, possible places of u as a vertex, which vertex, and edge length
   u =>  uPos
  )
 );
-- helper function to parse through all choices of u as a vertex (usually not too many)
 combinations := L -> (
  if #L == 1 then 
   return L_0
  else
   return combinations({flatten apply(L_0, l0 -> apply(L_1, l1 -> {l0,l1} | flatten drop(L,2) ))})
 );
 max {-1, min for sigma in keys uPositions list (
  max for c in combinations values uPositions#sigma list (
   if rank fold((transpose c)_0, (i,j) -> i|j) != rank tvb then
    continue;
   lift(min (transpose c)_2,ZZ)
  )
 )}
))




------------------------------------------------------------------------------
-- METHODS: isGloballyGenerated, isVeryAmple
------------------------------------------------------------------------------


-- PURPOSE : Given a toric vector bundle in Klyachko's description,
--           its parliament, compatible bases and toric Chern character
--           check if the vector bundle is globally generated, using [RJS, Thm. 1.2]
--   INPUT : 'tvb', toric vector bundle
--           'parliament', parliament of polytopes
--           'torChern', toric Chern character
--  OUTPUT :  'true' if globally generated, otherwise 'false'
isGloballyGenerated = method( Options => true )
isGloballyGenerated (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (tvb) -> 
if separatesJets(tvb, Verbosity=>opts#Verbosity) >= 0 then true else false;

-- PURPOSE : Given a toric vector bundle in Klyachko's description,
--           its parliament, compatible bases and toric Chern character
--           check if the vector bundle is very ample, using [RJS, Cor. 6.7]
--   INPUT : 'tvb', toric vector bundle
--           'parliament', parliament of polytopes
--           'torChern', toric Chern character
--  OUTPUT :  'true' if very ample, otherwise 'false'
--isVeryAmple = method( Options => true ) -- conflicts with Polyhedra
isVeryAmple (ToricVectorBundleKlyachko) := (tvb) -> if separatesJets(tvb) >= 1 then true else false;
  
------------------------------------------------------------------------------
-- METHOD: restrictToInvCurves, isNef, isAmple
------------------------------------------------------------------------------
-- AUXILIARY METHODS FOR isNef, isAmple:
-- restrictToCurve
------------------------------------------------------------------------------

-- PURPOSE : Given a toric vector bundle in Klyachko's description,
--           a cone corresponding to a curve of the toric variety and
--           its toric Chern character,
--           compute the restriction of the vector bundle to the curve.
--   INPUT :
--           'tau', (n-1)-dim cone given as matrix of rays
--           'torChern', toric Chern character
--  OUTPUT : list of integers (a_i) such that E|_C = \sum O_C(a_i)
restrictToCurve = method( Options => true)
restrictToCurve (Matrix,HashTable) := {Verbosity => 0} >> opts -> (tau,torChern) -> (
 normal := generators kernel transpose tau;
 if opts#Verbosity>0 then << "a vector normal to tau:" << endl << normal << endl;
 sigmas := {};
 for sigma in keys torChern do (
  if contains(coneFromVData sigma, coneFromVData tau) then (
   proj := flatten entries (transpose normal * sigma);
   if all(proj, n -> n >= 0) then
    sigmas = prepend(sigma,sigmas)
   else
    sigmas = append(sigmas,sigma);
  );
 );
 if opts#Verbosity>0 then << "the adjacent maximal cones sigma1 and sigma2 are:" << endl << sigmas << endl;
 for u0 in torChern#(sigmas_0) list (
  local a;
  for u1 in torChern#(sigmas_1) do (
   -- opposite due to different sign convention in [RJS]
   diff := u1-u0;
   as := unique select(apply(entries(diff|normal), i -> if i_1==0 then (if not i_0==0 then infinity) else i_0/i_1), x -> instance(x,Number) or instance(x,InfiniteNumber));
   if #as == 1 then (
    a = as_0;
    if opts#Verbosity>0 then << "the Chern components u1 and u2 lie on a line orthogonal to tau: " << endl << {u0,u1} << endl 
                           << "their difference is " << a << " times the normal." << endl;
    break;
   );
  );
  a
 )
)

-- MAIN METHODS: restrictToInvCurves, isNef, isAmple -----------------------------

restrictToInvCurves = method ( Options => true)
restrictToInvCurves (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (cacheValue symbol restrictionsToInvCurves) ( tvb -> (
 torChern := toricChernCharacter( tvb, Verbosity=>(opts#Verbosity-1));
 if opts#Verbosity>0 then << "METHOD: restrictToInvCurves" << endl;
 F := fan tvb;
 n := dim fan tvb;
 curveCones := apply(cones(n-1,F), c -> (rays F)_c);

 hashTable for tau in curveCones list (
  if opts#Verbosity>0 then << "Restriction to codim1 cone tau:" << endl << tau << endl;
  tau => restrictToCurve(tau,torChern, Verbosity=>(opts#Verbosity-1))
 )
))

-- PURPOSE : Given a toric vector bundle in Klyachko's description and
--           its toric Chern character,
--           compute whether the bundle is nef or ample, using [HMP, Thm. 2.1]
--   INPUT : 'tvb', toric vector bundle
--           'torChern', toric Chern character
--  OUTPUT : hash table
isNef = method( Options => true)
isNef (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> tvb -> (
 if opts#Verbosity>0 then << "METHOD: isNef" << endl;
 restrictions := restrictToInvCurves(tvb, Verbosity => opts#Verbosity);
 all(values restrictions, r -> all(r, x -> x >=0))
)

isAmple = method( Options => true)
isAmple (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> tvb -> (
 if opts#Verbosity>0 then << "METHOD: isAmple" << endl;
 restrictions := restrictToInvCurves(tvb, Verbosity => opts#Verbosity);
 all(values restrictions, r -> all(r, x -> x >0))
)

------------------------------------------------------------------------------
-- METHOD: drawParliament2Dtikz
------------------------------------------------------------------------------
-- AUXILIARY METHODS FOR drawParliament2Dtikz:
-- sortByAngle, circularOrder
------------------------------------------------------------------------------

-- PURPOSE : Given a list of points in the plane, 
--           and a list of the angles (interpret points as complex numbers),
--           sort the points by angle
sortByAngle = method()
sortByAngle (List,List) := (p, angles) -> (
 if #p == 1 then return p;
 for i from 0 to #p-2 do 
  if angles_(i+1) < angles_i then
   return sortByAngle(switch(i,i+1,p),switch(i,i+1,angles));
 p
)

-- PURPOSE : Given a list of points in the plane,
--           sort them in circular way around center of gravity
circularOrder = method()
circularOrder (List) := p -> (
 mid := sum p/#p;
 pmoved := apply(p, pt->pt-mid);
 angles := apply(pmoved, pt -> atan2(pt_1,pt_0));
 sortByAngle(p,angles)
)

-- MAIN METHOD: drawParliament2Dtikz -----------------------------------------

-- PURPOSE: Draw a two-dimensional parliamant of polytopes using tikz
--   INPUT: 'tvb', toric vector bundle,
--          'parliament', parliament of 2-dim polytopes
--          'file', string with file name
--  OUTPUT: nothing to M2, output goes to file
drawParliament2Dtikz = method( Options => true)
drawParliament2Dtikz (ToricVectorBundleKlyachko,String) := {DrawCohomology => true, DrawChernCharacter => true } >> opts -> (tvb,file) -> (
-- check for dimension 2
if dim fan tvb != 2 then (
 error "Error: only works for 2-dim parliaments.";
 return;
);

polys := apply(values parliament tvb, vertices);

offset := 0.3;
-- want (0,0) to be inside picture
maxx := max append(apply(polys, p->max flatten entries p^{0}),0);
maxy := max append(apply(polys, p->max flatten entries p^{1}),0);
minx := min append(apply(polys, p->min flatten entries p^{0}),0);
miny := min append(apply(polys, p->min flatten entries p^{1}),0);

-- <<"" is taken from documentation about
-- creating and writing files in M2
f := file << "";

f << ///\begin{tikzpicture}[scale=0.5]/// << endl;

if not minx == infinity then (
f << /// \draw[thin, color=black!75]/// << endl;
f << "  ("  << minx-offset << "," << miny-offset << ") grid (" << maxx+offset << "," << maxy+offset << ");" << endl << endl;
);

for pMat in polys do (
 -- check for empty polytope
 if source pMat==0 then continue;

 pts := circularOrder entries transpose pMat;
 f << /// \fill[color=black,opacity=0.1]/// << endl << "  ";
 for p in pts do (
  f << "(" << p_0 << "," << p_1 << ") -- ";
 );
 f << "cycle;" << endl;

 f << /// \draw[color=black,opacity=0.5]/// << endl << "  ";
 for p in pts do (
  f << "(" << p_0 << "," << p_1 << ") -- ";
 );
 f << "cycle;" << endl;
);


if opts#DrawCohomology then (
 H := apply(0 .. 2, i->degrees HH^i tvb);
 
 for h in H_0 do (
  f << /// \fill[color=blue,opacity=0.3]/// << endl;
  f << "  (" << h_0 << "," << h_1 << ") circle (2pt);" << endl;
 );
 
 for h in H_1 do (
  f << /// \fill[color=red,opacity=0.3]/// << endl;
  f << "  (" << h_0 << "," << h_1 << ") circle (2pt);" << endl;
 );
 
 for h in H_2 do (
  f << /// \fill[color=green,opacity=0.3]/// << endl;
  f << "  (" << h_0 << "," << h_1 << ") circle (2pt);" << endl;
 );
);

if opts#DrawChernCharacter then (
 chern := values toricChernCharacter tvb;
 for cs in chern do 
  for c in cs do (
   f << /// \draw[thick, color=yellow!50]/// << endl;
   f << "  (" << c_(0,0) << "," << c_(1,0) << ") circle (2pt);" << endl;
  );
 graphChern := graphToricChernCharacter tvb;
 for ray in keys graphChern do
  for u in graphChern#ray do (
   f << /// \draw[thin, color=yellow!50]/// << endl;
   f << "  (" << u_0_(0,0) << "," <<  u_0_(1,0) << ") -- (" << u_1_(0,0) << "," << u_1_(1,0) <<");" << endl;
  );
);

f << /// \draw[thick, color=black!50]/// << endl;
f << "  (0,0) circle (3pt);" << endl;

f << ///\end{tikzpicture}/// << endl;

f << close;
)
 
---------------------------------------------------------------------------
-- DOCUMENTATION
---------------------------------------------------------------------------

beginDocumentation()

document {
  Key => {PositivityToricBundles, Verbosity},
  Headline => "checks positivity of toric vector bundles",


  "Given a toric vector bundle, i.e. an equivariant vector bundle on a smooth complete toric variety, ",
  "this package can check the positivity of this bundle. ",
  TT "PositivityToricBundles", " can check whether a toric vector bundle is",
  UL {
    {"nef, i.e. whether the line bundle ", TEX ///$\mathcal{O}(1)$ on $\mathbb{P}(\mathcal E)$///, " is nef;"},
    {"(very) ample, i.e. whether the line bundle ", TEX ///$\mathcal{O}(1)$ on $\mathbb{P}(\mathcal E)$///, " is (very) ample;"},
    {"globally generated."}
  },
  "Additionally, ", TT "PositivityToricBundles", " can compute the toric Chern character of a toric vector bundle as introduced by Sam Payne.",
  PARA{},
  "For the computational purposes, ", TT "PositivityToricBundles", " uses the description of a toric vector bundles by filtrations developed by Alexander Klyachko, ",
  "and relies on its implementation via the ", TO ToricVectorBundles, " package by René Birkner, Nathan Ilten and Lars Petersen. ",
  BR{},
  "To check nefness and ampleness, ", TT "PositivityToricBundles", " uses a result of Milena Hering, Mircea Mustaţă and Sam Payne, ",
  "namely, that it is sufficient to check this for the restriction of the bundle to the torus invariant curves. ",
  "The central method for this is ", TO restrictToInvCurves, "; the methods ", TT "isNef", " and ", TT "isAmple", " are based on it.",
  BR{},
  "For global generation and very ampleness, ", TT "PositivityToricBundles", " uses results of Sandra Di Rocco, Kelly Jabbusch and Gregory Smith, ",
  "who describe these properties in terms of the so-called parliament of polytopes of a toric vector bundle. ",
  "From the parliament of polytopes one can extract the information up to which order jets are separated by the vector bundle. Globally generated or very ample toric vector bundles are those that separate 0-jets or 1-jets, respectively. ",
  "Here, the central method is ", TO separatesJets, "; built on it are ", TT "isGloballyGenerated", " and ", TT "isVeryAmple", ".",
  PARA{},
  "For the mathematical background see ",
  UL {
   {"[K] Alexander Klyachko, ", EM "Equivariant bundles over toral varieties", ", Izv. Akad. Nauk SSSR Ser. Mat., 53, 1989."},
   {"[P] Sam Payne, ", EM "Moduli of toric vector bundles", ", Compos. Math, 144, 2008."},
   {"[HMP] Milena Hering, Mircea Mustaţă, Sam Payne, ", EM "Positivity properties of toric vector bundles", ", Ann. Inst. Fourier (Grenoble), 60, 2010"},
   {"[RJS] Sandra Di Rocco, Kelly Jabbusch, Gregory Smith, ", EM "Toric vector bundles and parliaments of polytopes", ", Trans. AMS, 370, 2018."},
  },

  "The following example computes the positivity for the tangent sheaf of ", TEX ///\mathbb P^2///, ":",
  EXAMPLE {
   "E = tangentBundle projectiveSpaceFan 2",
   "isNef E",
   "isAmple E",
   "isVeryAmple E",
   "isGloballyGenerated E",
   "separatesJets E"
  },
  PARA{},
  "The toric Chern character can be computed:",
  EXAMPLE {
   "toricChernCharacter E"
  },
  "which associates to each maximal cone (its rays put into matrices) the corresponding components.",
  PARA{},
  "The restrictions of the bundle to the torus invariant curves can be computed:", 
  EXAMPLE {
   "restrictToInvCurves E"
  },
  "Here, in all three cases, the restriction splits into ", TEX ///$\mathcal{O}_{\mathbb P^1}(2) \oplus \mathcal{O}_{\mathbb P^1}(1)$///, ".",
  
  PARA{},
  "Most methods of ", TT "PositivityToricBundles", " support the option ", TT "Verbosity", ". ",
  "So by adding ", TT "Verbosity => n", " with n a positive integer to the arguments of a method, hopefully useful insight about the course of the calculation is provided.",
 
  Caveat => {
  "The description of a toric variety and a toric vector bundle by filtrations involves the choice of signs. ", TT "PositivityToricBundles", " follows the same choice of signs as ", TT "ToricVectorBundles", ", which are",
  UL {
   {"the fan associated to a polytope will be generated by inner normals,"},
   {"the filtrations for describing a toric vector bundle are increasing."},
  },
  "Unfortunately, the above cited articles use decreasing filtrations and, moreover, [HMP], [P] and [RJS] use outer normals.",
  PARA{},
  "Another warning concerns the toric variety: the methods of ", TT "PositivityToricBundles", " implicitly assume that the variety is complete (to apply the results of [HMP] and [P]) and in addition smooth (for [RJS]). For non-complete or singular toric varieties, methods might break or results might become meaningless."},

  SeeAlso => {"Polyhedra::Polyhedra", "ToricVectorBundles::ToricVectorBundles"}
  }


document {
  Key => {groundSet, (groundSet, ToricVectorBundleKlyachko)},
  Headline => "computes the ground set of a matroid associated to a toric vector bundle",
  Usage => "g = groundSet E",
  Inputs => {
    "E" => ToricVectorBundleKlyachko },
  Outputs => {
    "g" => List => {"containing elements of ground set as one column matrices"} },
  Consequences => { {"The result of ", TT "groundSet", " will be stored as a ", TO cacheValue, " in ", TT "E.cache#groundSet", ". It will be used by other methods."}
   },

  TEX///
  Given a toric vector bundle $\mathcal E$ in Klyachko's description on a toric variety $X = TV(\Sigma)$, 
  it is encoded by increasing filtrations $E^{\rho}(j)$  for each ray $\rho \in \Sigma(1)$.
  To these filtrations we can associated the set $L(\mathcal E)$ of intersections $\cap_{\rho} E^{\rho} (j_{\rho})$, where $(j_{\rho})_\rho$ runs over all tuples in $\mathbb Z^{\Sigma(1)}$.
  This set $L(\mathcal E)$ is ordered by inclusion and there is a unique matriod $M(\mathcal E)$ associated to it, see [RJS, Proposition 3.1].
  ///,
  TT "groundSet", " computes the ground set (i.e. building blocks) of this matroid.",
  
  EXAMPLE {
   "E = tangentBundle(projectiveSpaceFan 2)",
   "groundSet E"
  },
  
  "With the ground set, one can compute the parliament of polytopes using ", TO parliament, " or compute the set of compatible bases using ", TO compatibleBases, ".",

  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", parliament, compatibleBases}
  }

document {
  Key => {parliament, (parliament, ToricVectorBundleKlyachko) },
  Headline => "computes the parliament of polytopes to a toric vector bundle",
  Usage => "p = parliament E",
  Inputs => {
   "E" => ToricVectorBundleKlyachko,
   "g" => List  => {"containing elements of ground set as one column matrices"} },
  Outputs => {
   "p" => HashTable => {"whose keys are elements of the ground set and the value of a key is a ", TO Polyhedron } },
  Consequences => { {"The result of ", TT "parliament", " will be stored as a ", TO cacheValue, " in ", TT "E.cache#parliament", ". It will be used by other methods."}
   },
  
  "Given a toric vector bundle in Klyachko's description, ", 
  TT "parliament", " computes its parliament of polytopes as introduced in [RJS, Section 3].",

  EXAMPLE {
   "E = tangentBundle(projectiveSpaceFan 2)",
   "p = parliament E",
   "applyValues(p, vertices)"
  },

  "If the toric variety is two-dimensional, then the result can be visualised using ", TO drawParliament2Dtikz, ". ",
  TT "parliament", " calles internally the method ", TO groundSet, ".",

  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", groundSet, drawParliament2Dtikz}
  }


document {
  Key => {drawParliament2Dtikz, (drawParliament2Dtikz,ToricVectorBundleKlyachko,String), DrawCohomology, DrawChernCharacter},
  Headline => "visualises the parliament of polytopes for a vector bundle on a toric surface using TikZ",
  Usage => "drawParliament2Dtikz(E,file)",
  Inputs => {
   "E" => ToricVectorBundleKlyachko,
   "file" => String => {"containing name of file, where picture will be written"} },
  Consequences => {
   {"The file with name ", TT "file", " will contain a TikZ picture, ready to be included into a TeX file."} },

  "Given a toric vector bundle in Klyachko's description on a toric surface, ",
  TT "drawParliament2Dtikz", " writes to a file a TikZ picture visualising the ", TO parliament, ". ",
  "Additionally, the picture may contain also information about the cohomology of the vector bundle as computed by ", TO cohomology, " from the ", TO ToricVectorBundle, " package and the toric Chern character as computed by ", TO toricChernCharacter, " and ", TO graphToricChernCharacter, ".",
  PARA{},
  "Extending the example from ", TO parliament, " by the command ", 
  PRE ///i5 : drawParliament2Dtikz(E,p,"picture.tikz")///,
  "produces the file ", TT "picture.tikz", ". ",
  "Embedded into a TeX file, this finally leads to a nice picture of the parliament.",
  PARA{},
  "By setting the options ", TT "DrawChernCharacter", " and ", TT "DrawCohomology", " to ", TT "true", " or ", TT "false", ", one can decide whether the picture also includes information about the cohomology or toric Chern character, respectively. By default, both are set to ", TT "true", ".",


  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", parliament}
  }

document {
  Key => { compatibleBases, (compatibleBases, ToricVectorBundleKlyachko) },
  Headline => "computes compatible bases for a toric vector bundle",
  Usage => "b = compatibleBases E",
  Inputs => {
   "E" => ToricVectorBundleKlyachko },
  Outputs => {
   "b" => HashTable => {"whose keys are maximal cones and the value of a key is a ", TO matrix} },
  Consequences => { {"The result of ", TT "compatibleBases", " will be stored as a ", TO cacheValue, " in ", TT "E.cache#compatibleBases", ". It will be used by other methods."}
   },

  "Given a toric vector bundle in Klyachko's description, ", TT "compatibleBases", " computes for each maximal cone of the underlying toric variety a compatible basis of the filtrations, see [RJS, Section 3] for details.",
  BR{},
  TT "compatibleBases", " calles internally the method ", TO groundSet, ".",
  PARA{},
  "The compatible bases serve as input for the computation of the toric Chern character by ", TO toricChernCharacter, ".",

  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", groundSet, toricChernCharacter}
  }


document {
  Key => { toricChernCharacter, (toricChernCharacter, ToricVectorBundleKlyachko)},
  Headline => "computes the toric Chern character of a toric vector bundle",
  Usage => "c = toricChernCharacter E",
  Inputs => { 
   "E"  => ToricVectorBundleKlyachko },
  Outputs => {
   "c" => HashTable => {"whose keys are maximal cones and the value of a key is a list of one column matrices"} },
  Consequences => { {"The result of ", TT "toricChernCharacter", " will be stored as a ", TO cacheValue, " in ", TT "E.cache#toricChernCharacter", ". It will be used by other methods."}
   },

  "Given a toric vector bundle in Klyachko's description, ", TT "toricChernCharacter", " computes its toric Chern character as introduced in [P]. ",
  BR{},
  TT "toricChernCharacter", " calles internally the method ", TO compatibleBases, ".",

  EXAMPLE {
   "E = tangentBundle(projectiveSpaceFan 2)",
   "toricChernCharacter E"
  },


  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", compatibleBases, graphToricChernCharacter}
  }

document {
  Key => { graphToricChernCharacter, (graphToricChernCharacter, ToricVectorBundleKlyachko)},
  Headline => "computes the lines connected the components of the toric Chern character of a toric vector bundle",
  Usage => "g = graphToricChernCharacter E",
  Inputs => { 
   "E"  => ToricVectorBundleKlyachko },
  Outputs => {
   "g" => HashTable => {"whose keys are curve cones and the value of a key is a pair of the connected components"} },
  Consequences => { {"The result of ", TT "graphToricChernCharacter", " will be stored as a ", TO cacheValue, " in ", TT "E.cache#graphToricChernCharacter", ". It will be used by other methods."}
   },

  "Given a toric vector bundle in Klyachko's description, ", TT "graphToricChernCharacter", " connects the component of its toric Chern character. ",
  BR{},
  TT "graphToricChernCharacter", " calls internally the method ", TO toricChernCharacter, ".",

  EXAMPLE {
   "E = tangentBundle(projectiveSpaceFan 2)",
   "c = toricChernCharacter E",
   "g = graphToricChernCharacter E"
  },

  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", groundSet, compatibleBases, toricChernCharacter}
  }

document {
  Key => {separatesJets, isGloballyGenerated, isVeryAmple, (separatesJets, ToricVectorBundleKlyachko), (isGloballyGenerated, ToricVectorBundleKlyachko), (isVeryAmple, ToricVectorBundleKlyachko) },
  Headline => "computes up to which order a toric vector bundle separates jets",
  Usage => "l = separateJets E \nb = isGloballyGenerated E \nb = isVeryAmple E",
  Inputs => { 
   "E"  => ToricVectorBundleKlyachko },
  Outputs => {
   "l" => ZZ,
   "b" => Boolean },
  Consequences => { {"The result of ", TT "separatesJets", " will be stored as a ", TO cacheValue, " in ", TT "E.cache#separatesJets", ". It will be used by other methods."}
   },

  "Given a toric vector bundle in Klyachko's description, ", TT "separatesJets", " determines up to which order the vector bundle separates jets. ",
  "Note that a toric vector bundle is globally generated or very ample, if it separates 0-jets or 1-jets, respectively, see [RJS, Theorem 1.2, 6.2 and 6.5]. ",
  "Hence, the methods ", TT "isGloballyGenerated", " and ", TT "isVeryAmple", " only ask whether ", TT "separatesJets", " returns a non-negative or positive integer, respectively. ",
  BR{},
  "If the vector bundle is not even globally generated, then ", TT "separatesJets", " returns the value ", TT "-1", ". ",
  BR{},
  TT "separatesJets", " calls internally the methods  ",  TO parliament, " and ", TO toricChernCharacter, "; ",
  "whereas ", TT  "isGloballyGenerated", " and ", TT "isVeryAmple", " are simple checks on the output of ", TT "separatesJets", ".",

  EXAMPLE {
   "E = tangentBundle(projectiveSpaceFan 2)",
   "separatesJets E",
   "isGloballyGenerated E",
   "isVeryAmple E"
  },
  "In this example, the vector bundle ", TEX ///$\mathcal E$///, " separates 1-jets, hence is very ample.",

  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", parliament, compatibleBases, toricChernCharacter}
  }

document {
  Key => { restrictToInvCurves, isNef, isAmple, (restrictToInvCurves, ToricVectorBundleKlyachko), (isNef, ToricVectorBundleKlyachko), (isAmple, ToricVectorBundleKlyachko) },
  Headline => "computes the restrictions of a toric vector bundle to the torus invariant curves",
  Usage => "l = restrictToInvCurves E \nb = isNef E \nb = isAmple E",
  Inputs => { 
   "E"  => ToricVectorBundleKlyachko },
  Outputs => {
   "l" => HashTable,
   "b" => Boolean },
  Consequences => { {"The result of ", TT "restrictToInvCurves", " will be stored as a ", TO cacheValue, " in ", TT "E.cache#restrictionsToInvCurves", ". It will be used by other methods."}
   },

  "Given a toric vector bundle in Klyachko's description, ", TT "restrictToInvCurves", " computes its restrictions to the torus invariant curves, which are isomorphich to a direct sum of line bundles ", TEX ///$\mathbb P^1$///, ". ",
  "Recall that on ", TEX ///$\mathbb P^1$///, " any vector bundle splits into ", TEX ///$\oplus_i O_{\mathbb P^1}(a_i)$///, ". Therefore ", TT "restrictToInvCurves", " returns a hash table with keys the invariant curves and as values lists with the integers ", TEX ///$a_i$///, ". ",
  BR{},
  "By [HMP, Theorem 2.1], if all these integers", TEX ///$a_i$///, " are non-negative or positive, the original toric vector bundle is nef or ample. ",
  "Hence, the methods ", TT "isNef", " and ", TT "isAmple", " check exactly that.",
  BR{},
  TT "restrictToInvCurves", " calls internally the method ", TO toricChernCharacter, "; ",
  "whereas ", TT  "isNef", " and ", TT "isAmple", " are simple checks on the output of ", TT "restrictToInvCurves", ".",


  EXAMPLE {
   "E = tangentBundle(projectiveSpaceFan 2)",
   "restrictToInvCurves E",
   "isNef E",
   "isAmple E"
  },
  "In this example we see that the vector bundle is ample, as all integers are positive.",


  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", parliament, compatibleBases, toricChernCharacter}
  }




---------------------------------------------------------------------------
-- TESTS
---------------------------------------------------------------------------
-- We test against the examples from [RJS]
-- CAVEAT: 1) The data is copied from [RJS], but many signs are different,
--         as [RJS] and ToricVectorBundle (and therefore PositivityToricBundles)
--         follow different sign conventions.
--         2) When setting up the vector bundles using ToricVectorBundles,
--         I have no direct influence on the (internal) order of rays.
--         Fortunately, it seems that the order is always chosen in the same way.
--         If this changes at some point in the future, all tests will fail.

-- Test 0
-- [RJS, Example 3.7]

TEST ///
-- This is [RJS, Example 3.7]
-- auxillary methods
V = toricVectorBundle(3,hirzebruchFan 0);
rays V
-- output:
-- {| 0  |, | 0 |, | -1 |, | 1 |}
--  | -1 |  | 1 |  | 0  |  | 0 |
Vbasis = { 
 matrix{{0,0,1},{1,0,0},{0,1,0}},   -- for (0,-1)
 matrix{{0,0,1},{1,0,0},{0,1,0}},   -- for (0,1)
 matrix{{1,1,0},{0,0,1},{1,0,0}},   -- for (-1,0)
 matrix{{1,1,0},{1,0,0},{0,0,1}} }; -- for (1,0)
Vfiltration = {
 matrix{{-2,-1,0}},  -- for (0,-1)
 matrix{{-2,-1,0}},  -- for (0,1)
 matrix{{-1,0,1}},   -- for (-1,0)
 matrix{{-1,0,1}} }; -- for (1,0)
V = addBase(V,Vbasis);
V = addFiltration(V,Vfiltration);


p = parliament V;
assert( 
  set apply(values p, q -> set entries transpose lift(vertices q,ZZ)) ===
  set { set {{0,0}}, set {{1,0}}, set {{-1,0}}, set {} } );


c = toricChernCharacter V;
assert(
  set apply(values c, l -> set entries transpose fold(l, (i,j) -> i|j)) ===
  set { set {{-1,0},{0,-2},{1,-1}},
        set {{1,0},{-1,-2},{0,-1}},
        set {{1,0},{-1,2},{0,1}},
        set {{0,2},{-1,0},{1,1}} } );
///


TEST ///
-- This is [RJS, Example 3.8] for d=2
-- auxillary methods
V = tangentBundle(projectiveSpaceFan 2);


p = parliament V;
assert( 
  set apply(values p, q -> set entries transpose lift(vertices q,ZZ)) ===
  set { set {{0,0},{-1,1},{-1,0}},
        set {{0,0},{1,-1},{0,-1}}, 
        set {{0,0},{1,0},{0,1}} } );


c = toricChernCharacter V;
assert(
  set apply(values c, l -> set entries transpose fold(l, (i,j) -> i|j)) ===
  set { set {{1,0},{1,-1}},
        set {{-1,1},{0,1}},
        set {{-1,0},{0,-1}} } );

assert(isGloballyGenerated V);
assert(isVeryAmple V);

assert(isNef V);
assert(isAmple V);

///

TEST ///
-- This is [RJS, Example 4.2] 
-- auxillary methods
V = toricVectorBundle(3, projectiveSpaceFan 2);
rays V
--     {| -1 |, | 0 |, | 1 |}
--      | -1 |  | 1 |  | 0 |
Vbasis = { 
 matrix{{1,0,0},{-1,1,0},{0,-1,1}},  -- for (-1,-1)
 matrix{{0,0,1},{0,1,0},{1,0,0}},    -- for (0,1)
 matrix{{1,0,0},{0,1,0},{0,0,1}} };  -- for (1,0)

Vfiltration = {
 matrix{{-3,-2,1}},  -- for (-1,-1)
 matrix{{-3,0,2}},   -- for (0,1)
 matrix{{-4,0,1}} }; -- for (1,0)

V = addBase(V,Vbasis);
V = addFiltration(V,Vfiltration);


p = parliament V;
assert( 
  set apply(values p, q -> set entries transpose lift(vertices q,ZZ)) ===
  set { set {{-3,2},{-4,2},{-4,3}},
        set {{1,2},{0,2},{0,3}},
        set {},
        set {{2,0},{1,0},{1,1}},
        set {{2,-3},{1,-3},{1,-2}} } );


c = toricChernCharacter V;
assert(
  set apply(values c, l -> set entries transpose fold(l, (i,j) -> i|j)) ===
  set { set {{1,2},{2,0},{2,-3}},
        set {{-4,3},{0,3},{1,1}},
        set {{-4,2},{0,0},{1,-3}} } );

assert(not isGloballyGenerated V);
assert(not isVeryAmple V);

assert(isNef V);
assert(isAmple V);


///

TEST ///
-- This is [RJS, Example 4.4] 
-- auxillary methods
V = toricVectorBundle(2, hirzebruchFan 1);
rays V
-- {| 0  |, | -1 |, | 0 |, | 1 |}
--  | -1 |  | 1  |  | 1 |  | 0 |
Vbasis = { 
 matrix{{1,1},{1,0}},   -- for (0,-1)
 matrix{{0,1},{1,0}},   -- for (-1,1)
 matrix{{1,0},{0,1}},   -- for (0,1)
 matrix{{1,0},{0,1}} }; -- for (1,0)

Vfiltration = {
 matrix{{-3,1}},   -- for (0,-1)
 matrix{{-5,0}},   -- for (-1,1)
 matrix{{-3,-2}},  -- for (0,1)
 matrix{{-4,2}} }; -- for (1,0)

V = addBase(V,Vbasis);
V = addFiltration(V,Vfiltration);

p = parliament V;
assert( 
  set apply(values p, q -> set entries transpose lift(vertices q,ZZ)) ===
  set { set {{-1,-1},{-3,-3},{-4,-3},{-4,-1}},
        set {{3,3},{2,2},{2,3}},
        set {{4,-1},{3,-2},{2,-2},{2,-1}} } );

c = toricChernCharacter V;
assert(
  set apply(values c, l -> set entries transpose fold(l, (i,j) -> i|j)) ===
  set { set {{2,-2},{-4,-3}},
        set {{3,-2},{-3,-3}},
        set {{4,-1},{3,3}},
        set {{2,3},{-4,-1}} } );

assert(isGloballyGenerated V);
assert(isVeryAmple V);

assert(isNef V);
assert(isAmple V);

///

TEST ///
-- This is [RJS, Example 6.4] 
-- auxillary methods
V = toricVectorBundle(3, projectiveSpaceFan 2);
rays V
--     {| -1 |, | 0 |, | 1 |}
--      | -1 |  | 1 |  | 0 |
Vbasis = { 
 matrix{{1,0,0},{-1,1,0},{0,-1,1}},  -- for (-1,-1)
 matrix{{0,0,1},{0,1,0},{1,0,0}},    -- for (0,1)
 matrix{{1,0,0},{0,1,0},{0,0,1}} };  -- for (1,0)


Vfiltration = {
 matrix{{-4,-3,-1}}, -- for (-1,-1)
 matrix{{-2,0,2}},   -- for (0,1)
 matrix{{-2,1,2}} }; -- for (1,0)

V = addBase(V,Vbasis);
V = addFiltration(V,Vfiltration);

p = parliament V;
assert( 
  set apply(values p, q -> set entries transpose lift(vertices q,ZZ)) ===
  set { set {{-1,2},{-2,2},{-2,3}},
        set {{2,2},{1,2},{1,3}},
        set {{1,0}},
        set {{3,0},{2,0},{2,1}},
        set {{3,-2},{2,-2},{2,-1}} } );

c = toricChernCharacter V;
assert(
  set apply(values c, l -> set entries transpose fold(l, (i,j) -> i|j)) ===
  set { set {{2,2},{3,0},{3,-2}},
        set {{-2,3},{1,3},{2,1}},
        set {{-2,2},{1,0},{2,-2}} } );

assert(isGloballyGenerated V);
assert(not isVeryAmple V);

assert(isNef V);
assert(isAmple V);
 
///


TEST ///
-- Test with a randomized vector bundle

r = 2 + random 4

F = directProduct(projectiveSpaceFan 1, hirzebruch r)


E = randomDeformation(tangentBundle F,4)

rays E
tw = toList apply( 1 .. # rays E, i-> random 3)
E = twist(E, tw)

base E

filtration E

isVectorBundle E


recursionLimit = 1000
gs = groundSet E

p = parliament E;
par = unique entries transpose fold(flatten apply(values p, latticePoints), (i,j) -> i|j)

c = toricChernCharacter E

degs = unique degrees HH^0 E

assert( set par === set degs )

cList  = apply(values c, l -> apply(l, v -> flatten entries transpose v))

w = findWeights E

wList = apply(w, l -> apply(l, m -> entries transpose m))

assert( #cList == #wList )

for i in 0 ..< #cList do (
 found := -1;
 for j in 0 ..< #wList do (
  if member(cList_i,wList_j) then (
   found = j;
   break;
  )
 );
 assert( found >= 0 );
 wList = drop(wList,{found,found});
)

///


end

