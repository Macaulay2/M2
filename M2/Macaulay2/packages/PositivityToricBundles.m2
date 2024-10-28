--*- coding: utf-8 -*-

------------------------------------------------------------------------------
-- PURPOSE : Check positivity of toric vector bundles
-- PROGRAMMER : Andreas Hochenegger
------------------------------------------------------------------------------

newPackage("PositivityToricBundles",
           Headline => "check positivity of toric vector bundles",
           Version => "1.9",
           Date => "August, 2024",
           Authors => { 
            {Name => "Andreas Hochenegger",
             Email => "andreas.hochenegger@polimi.it"}},
           Keywords => {"Toric Geometry"},
           Configuration =>{},
	   PackageImports => {},
           PackageExports => {"ToricVectorBundles"}
          )

export {
        "groundSet", 
        "parliament",
        "compatibleBases",
        "isLocallyWeil",
        "isLocallyFree",
        "cartierInd",
        "toricChernCharacter",
        "graphToricChernCharacter",
        "separatesJets",
        "isGloballyGenerated",
        "restrictToInvCurves",
        "isNef",
        "isAmple",
        "drawParliament2Dtikz",
        "wellformedBundleFiltrations",
-- Options
        "Verbosity",
        "DrawCohomology",
        "DrawChernCharacter"
        }

-- cacheValues
protect basesSortedByFiltrations 
protect filtrationFlags
protect posetTvb
protect restrictionsToInvCurves
protect isLW
protect isLF
-- Options
protect DrawCohomology
protect DrawChernCharacter
protect Verbosity
protect preferredGenerators

---------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--
-- Copyright 2024 Andreas Hochenegger
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
-- flags, getColumns, primitive, 
------------------------------------------------------------------------------

flags = method()
flags (ToricVectorBundleKlyachko) := (cacheValue symbol filtrationFlags) ( tvb -> (
  hashTable for rho in rays tvb list (
   filtSteps := flatten entries (filtration tvb)#rho;
   rayFlag := for i in unique filtSteps list (
    ((base tvb)#rho)_(positions(filtSteps, j->j<=i))
   );
   rayFlag = sort(rayFlag, mat -> numgens source mat);
   rho => rayFlag
 )
))

getColumns = method ()
getColumns (Matrix) := mat -> toList apply( 0..<numgens source mat, i->mat_i )
getColumns (Module) := M -> apply( getColumns gens M, c -> image matrix c)

primitive = method ()
primitive (Matrix) := mat -> (
 m := 1;
 if instance(mat_(0,0),QQ) then
  m = lcm apply(flatten entries mat, denominator);
 matmod := m*mat;
 m = gcd flatten entries matmod;
 lift(1/m*promote(matmod,QQ),ZZ)
)

-- is there a better way?
cartesianProduct2 = (L1,L2) -> flatten apply(L1, l1 -> apply(L2, l2 -> {l1,l2}))
cartesianProductNested = L -> fold(L, cartesianProduct2)
inductiveFlatten = (L,i) -> if i<=0 then L else ( {L#0} | inductiveFlatten (L#1,i-1))
cartesianProduct = L -> (
 if #L==1 then return apply(L#0, l-> {l}); -- border case
 n := #L-2;
 apply( cartesianProductNested L, l -> inductiveFlatten(l,n))
)

poset = method()
poset (ToricVectorBundleKlyachko) := (cacheValue symbol posetTvb)  (tvb -> (
 -- do all possible intersections (over QQ)
 intersections := apply( cartesianProduct values flags tvb, L -> intersect( apply(L, l -> image promote(l,QQ) ) ) );
 -- remove 0-dimensional spaces and duplicates 
 intersections = toList set select( intersections, V -> rank V > 0 );
 intersections
))

-- MAIN METHOD: groundSet ----------------------------------------

-- PURPOSE : Given a toric vector bundle in Klyachko's description,
--           compute the ground set of the associated matroid
--           IMPORTANT: Due to the implementation, elements might appear several times
--   INPUT : 'tvb', a ToricVectorBundleKlyachko
--  OUTPUT : ground set (list of nx1-matrices)
groundSet = method( Options => true )
groundSet (ToricVectorBundleKlyachko) :=  {Verbosity => 0, preferredGenerators => {}} >> opts -> (cacheValue groundSet)  (tvb -> (
 if opts#Verbosity>0 then << "METHOD: groundSet" << endl;
 intersections := poset tvb;
 if opts#Verbosity>0 then << "Poset of proper linear subspaces of E: " << endl << apply(intersections,gens) << endl;
 G := toList set select( intersections, V -> rank V == 1 );
 if opts#Verbosity>0 then << "Initialize G with the one-dimensional subspaces: " << endl << apply(G,gens) << endl;
 for k in 2..rank tvb do (
  Vs := select( intersections, V -> rank V == k );
  if opts#Verbosity>0 then << "The " << k << "-dimensional subspaces: " << endl << apply(Vs,gens) << endl;
  for V in Vs do (
   GinV := select(G, g -> isSubset(g, V) );
   if opts#Verbosity>0 then << "V = " << gens V << " contains " << apply(GinV,gens) << endl;
   sumGinV := sum append(GinV, image map(QQ^(rank tvb),QQ^1,0)); -- works also if GinV empty
   if rank sumGinV < rank V then (
    newGs := {};
    generatorsToChoose := {};
    if #(opts#preferredGenerators) > 0 then (
     generatorsToChoose = select( apply(opts#preferredGenerators, g -> image promote(g,QQ)), g -> isSubset(g, V));
    )
    else (
     generatorsToChoose = toList getColumns V;
    );
    if opts#Verbosity>0 then << "generators in V: " << apply(generatorsToChoose, gens) << endl;
    generatorsToChoose = select( generatorsToChoose, v -> not isSubset(v, sumGinV));
    if opts#Verbosity>0 then << "generators not in " << apply(GinV,gens) << ": " << apply(generatorsToChoose, gens) << endl;
    while rank(sumGinV + sum append(newGs, image map(QQ^(rank tvb),QQ^1,0))) < rank V do (
     if rank(sumGinV+sum append(newGs, image map(QQ^(rank tvb),QQ^1,0))+generatorsToChoose#0) > rank(sumGinV+sum append(newGs, image map(QQ^(rank tvb),QQ^1,0))) then (
      if opts#Verbosity>0 then << "add generator: " << gens generatorsToChoose#0 << endl;
      newGs = append(newGs, generatorsToChoose#0);
     );
     generatorsToChoose = drop(generatorsToChoose,1);
    );
    G = G | newGs;
    if opts#Verbosity>0 then << "Updated G to " << apply(G,gens) << endl;
   )
   else (
    if opts#Verbosity>0 then << "is already generated by G." << endl;
   )
  )
 );
 -- return to ZZ
 apply(toList set G, g -> primitive gens g)
))

------------------------------------------------------------------------------
-- METHOD: parliament
------------------------------------------------------------------------------
-- AUXILIARY METHODS FOR parliament:
-- polytopeTVB
------------------------------------------------------------------------------


polytopeTVB = method( Options => true )
polytopeTVB (ToricVectorBundleKlyachko, Matrix) := {Verbosity => 0} >> opts -> ( (tvb,e) -> (
 if opts#Verbosity>0 then << "Calculate polytope for e = " << e << endl;
 filtSteps := hashTable for rho in rays tvb list (
  rho => unique flatten entries (filtration tvb)#rho
 );
 M := transpose matrix { apply( rays tvb, rho ->  (filtSteps#rho)#(position((flags tvb)#rho, f -> isSubset(image promote(e,QQ), image promote(f,QQ))))) };
 rhoMat := transpose fold( rays tvb, (i,j) -> i|j);
 M = promote(M,QQ);
 rhoMat = promote(rhoMat,QQ);
 if opts#Verbosity>0 then << "the equations for the polytope are:" << endl <<  rhoMat << "*x >= " << M << endl;
 pol := polyhedronFromHData(-rhoMat,-M);
 if opts#Verbosity>0 then << "the associated polytope has vertices:" << endl << vertices pol << endl;
 pol
))

-- Main method: parliament ---------------------------------------------------

-- PURPOSE : Given a toric vector bundle in Klyachko's description
--           and the ground set of the associated matroid,
--           compute the parliament of polytopes [RJS]
--   INPUT : 'tvb', a ToricVectorBundleKlyachko
--  OUTPUT : parliament of polytopes, hash table (ground set => parliament)
parliament = method( Options => true )
parliament(ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (cacheValue parliament)( tvb -> (
 if opts#Verbosity>0 then << "METHOD: parliament" << endl;
 gs := groundSet(tvb, Verbosity=>(opts#Verbosity-1));
 hashTable for e in gs list ( e => polytopeTVB(tvb,e, Verbosity=>opts#Verbosity) )
))

------------------------------------------------------------------------------
-- METHOD: compatibleBases
------------------------------------------------------------------------------
-- AUXILIARY METHODS FOR compatibleBases:
-- restrictToAffine, compatibleBasis
------------------------------------------------------------------------------


restrictToAffine = method( Options => true )
restrictToAffine (ToricVectorBundleKlyachko, Matrix) := {Verbosity => 0} >> opts -> ( (tvb,cone) ->
(
 if opts#Verbosity>0 then << "METHOD: restrictToAffine" << endl;
 E := toricVectorBundle(rank tvb, fan coneFromVData cone);
 E = addFiltration( E, toList apply(rays E, rho -> (filtration tvb)#rho));
 E = addBase( E, toList apply(rays E, rho -> (base tvb)#rho));
 E
))

-- PURPOSE: Given the possible flags (as computed by possibleFlags) for rays of maximal cone
--          compute a common set of basis vectors
--   INPUT: 'tvb', toric vector bundle
--          'sigma', maximal cone
--  OUTPUT: compatible base (matrix of vectors)
compatibleBasis = method( Options => true )
compatibleBasis (ToricVectorBundleKlyachko, Matrix) := {Verbosity => 0} >> opts -> ( (tvb, sigma) -> (
 if opts#Verbosity>0 then << "METHOD: compatibleBasis" << endl;
 E := restrictToAffine(tvb,sigma, Verbosity=>opts#Verbosity);
 if opts#Verbosity>0 then << "details of bundle restricted to " << sigma << endl << details E << endl;
 fold(groundSet (E, Verbosity=>opts#Verbosity-1, preferredGenerators=>groundSet tvb), (i,j) -> i|j)
))


-- MAIN METHOD: compatibleBases ----------------------------------------------

-- PURPOSE: Given a toric vector bundle in Klyachko's description
--          compute list of compatible bases as in [RJS, Section 3]
--   INPUT: 'tvb', toric vector bundle
--  OUTPUT: hash table: max cone => compatible basis
compatibleBases = method( Options => true )
compatibleBases (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (cacheValue symbol compatibleBases) (tvb -> (
 if opts#Verbosity>0 then << "METHOD: compatibleBases" << endl;
 maxcones := apply(maxCones tvb, rays);
 hashTable for sigma in maxcones list (sigma => compatibleBasis (tvb , sigma, Verbosity=>opts#Verbosity ))
))

------------------------------------------------------------------------------
-- isLocallyWeil
------------------------------------------------------------------------------

-- PURPOSE: Given a toric reflexive sheaf in Klyachko's description
--          check whether it is locally Weil, that is, locally a direct sum of reflexive sheaves of rank 1
--   INPUT: 'tvb', toric vector bundle
--  OUTPUT: true or false
isLocallyWeil = method( Options => true )
isLocallyWeil (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (cacheValue isLW) (tvb -> (
 if opts#Verbosity>0 then << "METHOD: isLocallyWeil" << endl;
 cbs := compatibleBases (tvb, Verbosity=>opts#Verbosity );
 isVB := applyValues(cbs, b -> numgens source b == rank tvb);
 if opts#Verbosity>0 then (
   << "cones where sheaf is not locally Weil:" << endl;
   for cb in pairs cbs do if not cb#1 then << cb#0 << endl;
 );
 all(values isVB, i->i)
))

------------------------------------------------------------------------------
-- toricChernCharacter
------------------------------------------------------------------------------

-- PURPOSE : Given a toric vector bundle in Klyachko's description and
--           given compatible bases as computed by compatibleBases,
--           compute the toric Chern character as introduced in [Payne] 
--   INPUT : 'tvb', toric vector bundle
--  OUTPUT : hash table: max cone => points of toric Chern character.
toricChernCharacter = method( Options => true )
toricChernCharacter (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (cacheValue toricChernCharacter) ( tvb -> (
 compBases := compatibleBases(tvb); 
 if opts#Verbosity>0 then << "METHOD: toricChernCharacter" << endl;
 filtSteps := hashTable for rho in rays tvb list (
  rho => unique flatten entries (filtration tvb)#rho
 );
 cI := 1; -- cartierIndex
 result := hashTable for cb in pairs compBases list (
  maxcone := apply( 0..<numgens source cb_0, i->matrix (cb_0)_i );
  if opts#Verbosity>0 then << "For maximal cone sigma: " << endl << maxcone << endl;
  base := apply( 0..<numgens source cb_1, i->matrix (cb_1)_i );
  if opts#Verbosity>0 then << "the compatible basis is: " << endl << base << endl;
  cb_0 => for b in base list (
   RHS := for rho in maxcone list (
    i := position((flags tvb)#rho, f -> isSubset(image promote(b,QQ), image promote(f,QQ)));
    filtSteps#rho#i
   );
   sol := solve(transpose promote(cb_0,QQ), transpose promote(matrix {RHS},QQ));
   if opts#Verbosity>0 then << "Solve equation: " << endl << transpose cb_0 << " *x= " << transpose matrix {RHS} << endl << "Solution: " << sol << endl;
   cI = lcm(cI, lcm( apply(flatten entries sol, denominator) ) );
   --if all(flatten entries sol, e -> liftable(e,ZZ)) then
   -- sol = lift(sol,ZZ); 
   sol
  )
 );
 tvb.cache.cartierInd = cI;
 if cI == 1 then (
  tvb.cache.isLF = true;
  result = applyValues(result, c -> apply(c, m -> lift(m,ZZ)));
 )
 else (
  tvb.cache.isLF = false;
  if opts#Verbosity>0 then << "Cartier index is: " << cI << endl;
 );
 result
))

isLocallyFree = method()
isLocallyFree (ToricVectorBundleKlyachko) :=  tvb -> (
 if not isLocallyWeil tvb then return false;
 if not tvb.cache.?isLF then   
  toricChernCharacter tvb;
 tvb.cache.isLF
)

cartierInd = method()
cartierInd (ToricVectorBundleKlyachko) := tvb -> (
 if not tvb.cache.?cartierInd then
  toricChernCharacter tvb;
 tvb.cache.cartierInd
)

-- PURPOSE : Given a toric vector bundle in Klyachko's description and
--           its toric Chern character,
--           connect components in adjacent maximal cones by lines
--   INPUT : 'tvb', toric vector bundle
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

separatesJetsLocally = method( Options => true )
separatesJetsLocally (ToricVectorBundleKlyachko,Cone) := {Verbosity => 0} >> opts -> (tvb,sigma) -> (
 if opts#Verbosity>0 then << "METHOD: separatesJetsLocally" << endl;
 sigmaMat := rays sigma;
 if opts#Verbosity>0 then << "for the maximal cone sigma " << sigmaMat << endl;
 uSigma := apply((toricChernCharacter tvb)#sigmaMat, c -> promote(c,QQ));
 uSigmaSet := toList set uSigma;
 uSigmaMult := apply(uSigmaSet, u -> number(uSigma, w -> u==w));
 if opts#Verbosity>0 then << "the u(sigma) are " << uSigmaSet <<  " with multiplicities " << uSigmaMult << endl;
 par := parliament tvb;
 parVertices := applyValues(par, pol -> apply(getColumns vertices pol, c -> matrix promote(c,QQ)) );
 if opts#Verbosity>0 then << "the parliament is " << parVertices << endl;
-- [RJS, Thm 6.2, condition (i)]
 uSigmaAreVertices := hashTable apply(uSigmaSet, u -> u => applyValues(parVertices, pol -> member(u, pol)));
 uSigmaAreVerticesNumber := applyValues( applyValues(uSigmaAreVertices, values), trueFalse -> number(trueFalse, i->i==true));
 -- check whether all uSigma are vertices of as many polytopes as their multiplicity
 if not all(0 ..< #uSigmaSet, i-> uSigmaAreVerticesNumber#(uSigmaSet#i) >= uSigmaMult#i ) then 
 (
  if opts#Verbosity>0 then << << "on sigma not globally generated." << endl;
  return -infinity;
 );
-- [RJS, Thm 6.2, condition (ii+iii)]
 sigmaDual := dualCone sigma;
 -- gives u=> {e's} such that u vertex of P(e) and u+sigma^vee contains P(e)
 --uSigmaPE := applyPairs(uSigmaAreVertices, (u,eAV) -> (u, keys selectPairs(eAV, (e,AV) -> AV and contains(convexHull u + sigmaDual, par#e) ) ));
 uSigmaPE := applyPairs(uSigmaAreVertices, (u,eAV) -> (u, keys hashTable select(pairs eAV, (e,AV) -> AV and contains(convexHull u + sigmaDual, par#e) ) ));
 uSigmaPEnumber := applyValues(uSigmaPE, Ps -> #Ps);
 -- [RJS, Thm 6.2, condition (iv)]
 if min values uSigmaPEnumber == 0 then (
  if opts#Verbosity>0 then << "there is no polytope with suitable local structure for at least one u(sigma): " << applyValues(uSigmaPE, l -> apply(l, e -> parVertices#e)) << endl;
  if opts#Verbosity>0 then << << "on sigma not globally generated." << endl;
  return -infinity;
 );
 if opts#Verbosity>0 then << "the u(sigma) appear as vertices (and with locally correct structure) of " << applyValues(uSigmaPE, l -> apply(l, e -> parVertices#e)) << endl;
 oneFaces := facesAsCones(tvb#"dimension of the variety"-1, sigmaDual);
 -- calculate the intersection of the edges of u+sigmaDual with P(e)
 edges := applyPairs(uSigmaPE, (u,es) -> (u, apply(es, e->  apply(oneFaces, f-> intersection(convexHull u + f, par#e)))));
 -- transform result into matrices whose columns are the vertices of the edges
 edges = applyValues(edges, edgeSet -> apply(edgeSet, polSet -> apply(polSet, pol -> matrix vertices pol )));
 if opts#Verbosity>0 then << "the intersections of u+sigmaDual with P(e) have vertices " << edges << endl;
 -- calculate the lattice length and take for each P(e) the minimal one
 edgeLengths := applyValues(edges, edgeSet -> apply(edgeSet, matSet -> min apply(matSet, mat -> gcd flatten entries (mat_0 - mat_(numgens source mat-1)))));
 -- [RJS, Thm 6.2, condition (iv)]
 -- check which combinations of P(e)s give a basis
 checkBaseOfMatroid := apply(cartesianProduct values uSigmaPE, es -> rank fold(es, (i,j)->i|j) == rank tvb);
 -- for all bases compute the minimal edge number, take maximum among all
 l := max apply(select(pack(2, mingle(checkBaseOfMatroid , cartesianProduct values edgeLengths)), i -> i#0), i -> min i#1);
 if opts#Verbosity>0 then (
  if l >= 0 then ( << "separates " << l << "-jets"  << endl ) else ( << "on sigma not globally generated." << endl )
 );
 l
)


-- PURPOSE : Given a toric vector bundle in Klyachko's description,
--           its parliament bases and toric Chern character
--           compute the maximal l such that the bundle separates l-jets
--   INPUT : 'tvb', toric vector bundle
--  OUTPUT :  Integer, -infinity if not separates any l-jets, otherwise l
separatesJets = method( Options => true )
separatesJets (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (cacheValue separatesJets) ( tvb -> (
 if opts#Verbosity>0 then << "METHOD: separatesJets" << endl;
 min( apply( maxCones tvb, sigma -> separatesJetsLocally(tvb,sigma, Verbosity=>opts#Verbosity) ) )
))


------------------------------------------------------------------------------
-- METHODS: isGloballyGenerated, isVeryAmple
------------------------------------------------------------------------------


-- PURPOSE : Given a toric vector bundle in Klyachko's description,
--           its parliament, compatible bases and toric Chern character
--           check if the vector bundle is globally generated, using [RJS, Thm. 1.2]
--   INPUT : 'tvb', toric vector bundle
--  OUTPUT :  'true' if globally generated, otherwise 'false'
isGloballyGenerated = method( Options => true )
isGloballyGenerated (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> (tvb) -> 
if separatesJets(tvb, Verbosity=>opts#Verbosity) >= 0 then true else false;

-- PURPOSE : Given a toric vector bundle in Klyachko's description,
--           its parliament, compatible bases and toric Chern character
--           check if the vector bundle is very ample, using [RJS, Cor. 6.7]
--   INPUT : 'tvb', toric vector bundle
--  OUTPUT :  'true' if very ample, otherwise 'false'
--isVeryAmple = method( Options => true ) -- already defined in Polyhedra
isVeryAmple (ToricVectorBundleKlyachko) := {Verbosity => 0} >> opts -> tvb -> separatesJets(tvb, opts) >= 1
  
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

-- PURPOSE: Draw a two-dimensional parliament of polytopes using tikz
--   INPUT: 'tvb', toric vector bundle,
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


--- METHOD: wellformedBundleFiltrations -----------------------------------

-- PURPOSE: ensures ascending entries in the filtration matrices of a toric vector bundle
--   INPUT: 'tvb', toric vector bundle
--  OUTPUT: a toric vector bundle, whose filtration matrices have ascending entries

wellformedBundleFiltrations = method ()
wellformedBundleFiltrations (ToricVectorBundleKlyachko) := tvb -> (
 fMTlist := applyValues(filtration tvb, f -> sort flatten entries f );
 fMT := applyValues(fMTlist, f -> matrix {f});
 fMTunique := applyValues(fMTlist, unique);
 permutations := applyPairs(fMTunique, (rho,f)-> rho => apply(f, i-> positions(flatten entries (filtration tvb)#rho, j->i==j)) );
 bT := applyPairs(base tvb, (rho,m) -> rho => fold(apply(permutations#rho, i -> m_i), (i,j) -> i|j));
 -- the following is taken from ToricVectorBundles#makeVBKlyachko
 fT := hashTable apply(pairs fMT, p -> (
               L := flatten entries p#1;
               L1 := sort unique L;
               p#0 => hashTable ({(min L1-1) => {}} | apply(L1, l -> l => positions(L,e -> e == l)))));
 new ToricVectorBundleKlyachko from {
               "ring" => tvb#"ring",
               "rayTable" => tvb#"rayTable",
               "baseTable" => bT,
               "filtrationMatricesTable" => fMT,
               "filtrationTable" => fT,
               "ToricVariety" => tvb#"ToricVariety",
               "number of affine charts" => tvb#"number of affine charts",
               "dimension of the variety" => tvb#"dimension of the variety",
               "rank of the vector bundle" => tvb#"rank of the vector bundle",
               "number of rays" => tvb#"number of rays",
               symbol cache => new CacheTable}
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

  "The following example computes the positivity for the tangent sheaf of ", TEX ///$\mathbb P^2$///, ":",
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
   {"the filtrations for describing a toric vector bundle are increasing and that the filtration steps are stored in that way, see ", TO "wellformedBundleFiltrations", "."},
  },
  "Unfortunately, the above cited articles use decreasing filtrations and, moreover, [HMP], [P] and [RJS] use outer normals.",
  PARA{},
  "Contrary to what the name suggests, ", TO ToricVectorBundle, " may very well encode a toric reflexive sheaf, which is not necessarily locally free, that is, not necessarily a vector bundle. Some methods work also for toric reflexive sheaves, but this is not guaranteed.",
  PARA{},
  "Another warning concerns the toric variety: the methods of ", TT "PositivityToricBundles", " implicitly assume that the variety is complete (to apply the results of [HMP] and [P]) and in addition smooth (for [RJS]). For non-complete or singular toric varieties, methods might break or results might become meaningless."},

  SeeAlso => {"Polyhedra::Polyhedra", "ToricVectorBundles::ToricVectorBundles"},
  Contributors => {"The author of the package wants to thank Brett Nasserden and Alexandre Zotine for reporting bugs."}
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
  This set $L(\mathcal E)$ is ordered by inclusion and there is a unique matroid $M(\mathcal E)$ associated to it, see [RJS, Proposition 3.1].
  ///,
  TT "groundSet", " computes the ground set (i.e. building blocks) of this matroid.",
  
  EXAMPLE {
   "E = tangentBundle(projectiveSpaceFan 2)",
   "groundSet E"
  },
  
  "With the ground set, one can compute the parliament of polytopes using ", TO parliament, " or compute the set of compatible bases using ", TO compatibleBases, ".",

  Caveat => {"This method works for any toric reflexive sheaf on any toric variety."},

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

  EXAMPLE {
   "P112 = ccRefinement transpose matrix {{1,0},{0,1},{-1,-2}};",
   "D = {1,0,0};",
   "L = toricVectorBundle(1, P112, toList(3: matrix{{1_QQ}}), apply( D, i-> matrix{{-i}}));",
   "details L",
   "apply(values parliament L, vertices)",
  },


  "If the toric variety is two-dimensional, then the result can be visualised using ", TO drawParliament2Dtikz, ". ",
  TT "parliament", " calls internally the method ", TO groundSet, ".",

  Caveat => {"This method works for any toric reflexive sheaf on any toric variety."},

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
  TT "compatibleBases", " calls internally the method ", TO groundSet, ".",
  PARA{},
  "The compatible bases serve as input for the computation of the toric Chern character by ", TO toricChernCharacter, ".",

  Caveat => {"This method works for any toric reflexive sheaf (see ", TO isLocallyWeil, " for an example where the sheaf is not locally free) on a toric variety, whose fan is covered by cones of maximal dimension."},

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
  Consequences => { {"The result of ", TT "toricChernCharacter", " will be stored as a ", TO cacheValue, " in ", TT "E.cache#toricChernCharacter", ". ", BR{},
                     "As a side product, this method checks whether the sheaf given is locally free", " this will be stored as a ", TO cacheValue, " in ", TT "E.cache#isLF", ". This value is returned by the method ", TO "isLocallyFree", ". ", BR{},
                     "Additionally,  ", TT "toricChernCharacter", " determines the Cartier index, which will be stored as a ", TO cacheValue, " in ", TT "E.cache#cartierInd", ". This value is returned by the method ", TO "cartierInd", "." }},

  "Given a toric vector bundle in Klyachko's description, ", TT "toricChernCharacter", " computes its toric Chern character as introduced in [P]. ",
  BR{},
  TT "toricChernCharacter", " calls internally the method ", TO compatibleBases, ".",

  EXAMPLE {
   "E = tangentBundle(projectiveSpaceFan 2)",
   "toricChernCharacter E"
  },

  Caveat => {"This method works for a toric reflexive sheaf which is locally Weil (see ", TO isLocallyFree, " for an example if the sheaf is locally Weil but not locally free) on a toric variety, whose fan is covered by simplicial cones of maximal dimension."},


  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", compatibleBases, graphToricChernCharacter, isLocallyFree, cartierInd}
  }

document {
  Key => { isLocallyWeil, (isLocallyWeil, ToricVectorBundleKlyachko) },
  Headline => "checks whether a toric reflexive sheaf is locally Weil",
  Usage => "isLW = isLocallyWeil E",
  Inputs => {
   "E"  => ToricVectorBundleKlyachko },
  Outputs => {
   "isLW" => Boolean => {"true if the reflexive sheaf is locally Weil, false otherwise"} },
  Consequences => { {"The result of ", TT "isLocallyWeil", " will be stored as a ", TO cacheValue, " in ", TT "E.cache#isLW", "."} },

  "Contrary to what the name suggests, ", TO ToricVectorBundle, " may well encode a toric reflexive sheaf that is not locally Weil (as soon as the toric variety has dimension at least three). ", TT "isLocallyWeil", " permits to check whether a toric reflexive sheaf is locally Weil, that is, locally a direct sum of reflexive sheaves of rank one. If the toric variety is smooth, this is equivalent to being locally free, that is, a vector bundle.",
  BR{},
  TT "isLocallyWeil", " calls internally the method ", TO compatibleBases, ", if all the bases have as many elements as the rank of the sheaf, then it is locally Weil.",

  EXAMPLE {
   "A3 = fan coneFromVData matrix {{1,0,0},{0,1,0},{0,0,1}};",
   "filtMat = apply( { {{1,0,0},{0,1,0},{0,0,1}}, {{0,1,0},{1,0,0},{0,0,1}}, {{1,1,0},{1,0,0},{0,0,1}} }, matrix);",
   "filtStep = apply( { {{0,1,1}}, {{0,1,1}}, {{0,1,1}} }, matrix);",
   "E = toricVectorBundle (3,A3,filtMat,filtStep);",
   "details E",
   "isLocallyWeil E",
   "compatibleBases E"
  },

  Caveat => {"This method works for any toric reflexive sheaf on a toric variety, whose fan is covered by cones of maximal dimension."},

  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", compatibleBases, isLocallyFree }
}

document {
  Key => { isLocallyFree, (isLocallyFree, ToricVectorBundleKlyachko) },
  Headline => "checks whether a toric reflexive sheaf is locally free",
  Usage => "isLF = isLocallyFree E",
  Inputs => {
   "E"  => ToricVectorBundleKlyachko },
  Outputs => {
   "isLF" => Boolean => {"true if the reflexive sheaf is locally free, false otherwise"} },
  Consequences => { {"The result of ", TT "isLocallyFree", " will be stored as a ", TO cacheValue, " in ", TT "E.cache#isLF", "."} },

  "Contrary to what the name suggests, ", TO ToricVectorBundle, " may well encode a toric reflexive sheaf that is not locally free. ", TT "isLocallyFree", " permits to check whether a toric reflexive sheaf is locally free, that is, locally a direct sum of invertible sheaves, that is, a vector bundle.",
  BR{},
  TT "isLocallyFree", " calls internally the methods ", TO isLocallyWeil, " and ", TO toricChernCharacter, ".",

  EXAMPLE {
   "A3 = fan coneFromVData matrix {{1,0,0},{0,1,0},{0,0,1}};",
   "filtMat = apply( { {{1,0},{0,1}}, {{0,1},{1,0}}, {{1,1},{1,0}} }, matrix);",
   "filtStep = apply( { {{0,1}}, {{0,1}}, {{0,1}} }, matrix);",
   "E = toricVectorBundle (2,A3,filtMat,filtStep);",
   "details E",
   "isLocallyFree E",
  },

  EXAMPLE {
   "P112 = ccRefinement transpose matrix {{1,0},{0,1},{-1,-2}};",
   "D = {1,0,0};",
   "L = toricVectorBundle(1, P112, toList(3: matrix{{1_QQ}}), apply( D, i-> matrix{{-i}}));",
   "details L",
   "isLocallyWeil L",
   "toricChernCharacter L",
   "isLocallyFree L"
  },

  Caveat => {"This method works for any toric reflexive sheaf on a toric variety, whose fan is covered by cones of maximal dimension."},

  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", toricChernCharacter, isLocallyWeil }
}


document {
  Key => { cartierInd, (cartierInd, ToricVectorBundleKlyachko) },
  Headline => "computes the Cartier index",
  Usage => "i = cartierInd E",
  Inputs => {
   "E"  => ToricVectorBundleKlyachko },
  Outputs => {
   "i" => ZZ => {"Cartier index of E"} },
  Consequences => { {"The result of ", TT "cartierInd", " will be stored as a ", TO cacheValue, " in ", TT "E.cache#cartierInd", "."} },

  "Contrary to what the name suggests, ", TO ToricVectorBundle, " may well encode a toric reflexive sheaf that is not locally free. ", TT "cartierInd", " computes the Cartier index, that is, the smallest non-negative integer i such that the pullback of the bundle under the i-th toric Frobenius becomes locally free. In case of the reflexive sheaf of a Weil divisor D, this is the smallest i such that iD is Cartier. This method works well only on simplicial toric varieties.",
  BR{},
  TT "cartierInd", " calls internally the method ", TO toricChernCharacter, ".",

  EXAMPLE {
   "P112 = ccRefinement transpose matrix {{1,0},{0,1},{-1,-2}};",
   "D = {1,0,0};",
   "L = toricVectorBundle(1, P112, toList(3: matrix{{1_QQ}}), apply( D, i-> matrix{{-i}}));",
   "details L",
   "cI = cartierInd L",
   "isLocallyFree L",
   "L2 = toricVectorBundle(1, P112, toList(3: matrix{{1_QQ}}), apply( cI*D, i-> matrix{{-i}}));",
   "isLocallyFree L2"
  },

  Caveat => {"This method works for a toric reflexive sheaf, which is locally Weil (see ", TO isLocallyWeil, "),  on a toric variety, whose fan is covered by cones of maximal dimension."},

  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", toricChernCharacter }
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
  "If the vector bundle is not even globally generated, then ", TT "separatesJets", " returns the value ", TT "-infinity", ". ",
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
  Caveat => {"These methods work for toric vector bundles on a complete simplicial toric variety.",
             BR{},
             "[RJS, Theorem 6.2, condition (iv)] is not checked, which might give a wrong result in special cases (too many polytopes of the parliament share common vertices). If this happens, the method ", TT "separatesJets", " will print a warning. In these cases, also the results of ", TT "isGloballyGenerated", " and ", TT "isVeryAmple", " might be wrong."},

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

  "Given a toric vector bundle in Klyachko's description, ", TT "restrictToInvCurves", " computes its restrictions to the torus invariant curves, which are isomorphic to a direct sum of line bundles ", TEX ///$\mathbb P^1$///, ". ",
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
  Caveat => {"These methods work for toric vector bundles on a complete simplicial toric variety."},


  SeeAlso => {"ToricVectorBundles::ToricVectorBundleKlyachko", parliament, compatibleBases, toricChernCharacter}
  }



document {
  Key => { wellformedBundleFiltrations, (wellformedBundleFiltrations, ToricVectorBundleKlyachko) },
  Headline => "produces the same toric vector bundle, but where the filtration steps are stored in matrices with ascending entries.",
  Usage => "F = wellformedBundleFiltrations E",
  Inputs => { 
   "E"  => ToricVectorBundleKlyachko },
  Outputs => {
   "F" => ToricVectorBundleKlyachko },

  "A toric vector bundle in Klyachko's description as used in ", TO "ToricVectorBundles", " is given by ascending filtrations. Unfortunately, the filtration steps are not always stored in an ascending way by certain methods, like ", TO "ToricVectorBundles::dual(ToricVectorBundle)", " or ", TO "ToricVectorBundles::tensor(ToricVectorBundle,ToricVectorBundle)", " (and maybe others). This may cause problems in some methods of ", TT "PositivityToricBundles", ", for example in ", TO "toricChernCharacter", ". This method ensures that the filtration steps are stored in an ascending way and therefore, that the methods of this package can be used safely.",

  EXAMPLE {
    "T = tangentBundle projectiveSpaceFan 2",
    "E = T ** (dual T)",
    "details E",
    "toricChernCharacter E"
  },
  "Note that the filtration steps of E are neither ascending nor descending. The method ", TO "toricChernCharacter", " produces nonsense here: ", TEX ///$\mathcal{O}_{\mathbb P^2}$///, " is a direct summand of E, so 0 has to appear in the toric Chern character.",

  EXAMPLE {
    "F = wellformedBundleFiltrations E",
    "details F",
    "toricChernCharacter F"
  },
  "By passing to ascending filtration steps, the result becomes correct.",

  Caveat => {"This method works for any toric reflexive sheaf."},

  SeeAlso => {"ToricVectorBundles::dual(ToricVectorBundle)", "ToricVectorBundles::tensor(ToricVectorBundle,ToricVectorBundle)"}
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
TEST ///
-- This is [RJS, Example 3.7]
-- auxiliary methods
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

-- Test 1
TEST ///
-- This is [RJS, Example 3.8] for d=2
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

-- Test 2
TEST ///
-- This is [RJS, Example 3.8] for d=3
V = tangentBundle(projectiveSpaceFan 3);


p = parliament V;
assert( 
  set apply(values p, q -> set entries transpose lift(vertices q,ZZ)) ===
  set { set {{0,0,0},{-1,0,1},{-1,1,0},{-1,0,0}},
        set {{0,0,0},{1,-1,0},{0,-1,1},{0,-1,0}}, 
        set {{0,0,0},{1,0,-1},{0,1,-1},{0,0,-1}}, 
        set {{0,0,0},{1,0,0},{0,1,0},{0,0,1}} } );


c = toricChernCharacter V;
assert(
  set apply(values c, l -> set entries transpose fold(l, (i,j) -> i|j)) ===
  set { set {{1,0,0},{1,-1,0},{1,0,-1}},
        set {{0,1,0},{-1,1,0},{0,1,-1}},
        set {{0,0,1},{-1,0,1},{0,-1,1}},
        set {{-1,0,0},{0,-1,0},{0,0,-1}} } );

assert(isGloballyGenerated V);
assert(isVeryAmple V);

assert(isNef V);
assert(isAmple V);
///

-- Test 3
TEST ///
-- This is [RJS, Example 4.2] 
-- auxiliary methods
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

-- Test 4
TEST ///
-- This is [RJS, Example 4.4] 
-- auxiliary methods
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

-- Test 5
TEST ///
-- This is [RJS, Example 6.4] 
-- auxiliary methods
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

-- Test 6
TEST ///
-- Test with a randomized vector bundle on 3-dim variety

r = 2 + random 4
F = directProduct(projectiveSpaceFan 1, hirzebruch r)
E = randomDeformation(tangentBundle F,4)

while not isLocallyWeil E do (
 E = randomDeformation(tangentBundle F,4)
)

tw = toList apply( 1 .. # rays E, i-> random 3)
E = twist(E, tw)

applyValues(filtration E, entries)
applyValues(base E, entries)

gs = groundSet E
p = parliament E;
par = unique entries transpose fold(flatten apply(values p, latticePoints), (i,j) -> i|j)

c = toricChernCharacter E

degs = unique degrees HH^0 E

assert( set par === set degs )

cList  = apply(values c, l-> fold(l,(i,j)->i|j))
wList = findWeights E

assert( #cList == #wList )

-- assumes that both lists have equal length
areEqualListsModPerm = (L1,L2) -> (
  if #L1 == 0 then return true; --implicit: #L2==0
  pos := positions(L2, l-> l==L1#0);
  if #pos == 0 then return false;
  return areEqualListsModPerm(drop(L1,{0,0}),drop(L2,{pos#0,pos#0}))
)

getColumns := mat -> toList apply( 0..<numgens source mat, i->mat_i )

foundList = for i in 0 ..< #cList list (
 found := -1;
 for j in 0 ..< #wList do (
  cChars := getColumns cList_i;
  for k in 0 ..< #(wList_j) do (
   wChars := getColumns (wList_j)_k;
   if areEqualListsModPerm(cChars,wChars) then (
    found = j;
    break;
   )
  )
 );
 found
)

assert(all(foundList, i->i>=0))
///

-- Test 7
TEST ///
-- Test with a randomized vector bundle of rank 3 on hirzebruch

r = 0 + random 5

X = hirzebruch r

rk=3

while true do (
 FiltMat = for i to 3 list matrix {{random(ZZ^rk,ZZ^rk)}};
 if min apply(FiltMat, rank) == rk then break
)
FiltMat
FiltStep = for i to 3 list matrix{sort toList apply(0..<rk, i-> random(-5,5))}
apply(FiltMat,entries)
apply(FiltStep,entries)

E = toricVectorBundle(rk, X, FiltMat, FiltStep)

cB = compatibleBases E

tCC = toricChernCharacter E

cList = apply(values tCC,  l-> fold(l,(i,j)->i|j))
wList = findWeights E

assert(#cList == #wList)

getColumns := mat -> toList apply( 0..<numgens source mat, i->mat_i )

-- assumes that both lists have equal length
areEqualListsModPerm = (L1,L2) -> (
  if #L1 == 0 then return true; --implicit: #L2==0
  pos := positions(L2, l-> l==L1#0);
  if #pos == 0 then return false;
  return areEqualListsModPerm(drop(L1,{0,0}),drop(L2,{pos#0,pos#0}))
)

foundList = for i in 0 ..< #cList list (
 found := -1;
 for j in 0 ..< #wList do (
  cChars := getColumns cList_i;
  for k in 0 ..< #(wList_j) do (
   wChars := getColumns (wList_j)_k;
   if areEqualListsModPerm(cChars,wChars) then (
    found = j;
    break;
   )
  )
 );
 found
)

assert(all(foundList, i->i>=0))
///

-- Test 8
TEST ///
-- Test with a randomized vector bundle of rank 4 on hirzebruch

r = 0 + random 3

X = hirzebruch r

rk=4

while true do (
 FiltMat = for i to 3 list matrix {{random(ZZ^rk,ZZ^rk)}};
 if min apply(FiltMat, rank) == rk then break
)
FiltMat
FiltStep = for i to 3 list matrix{sort toList apply(0..<rk, i-> random(-5,5))}
apply(FiltMat,entries)
apply(FiltStep,entries)

E = toricVectorBundle(rk, X, FiltMat, FiltStep)

cB = compatibleBases E

tCC = toricChernCharacter E

cList = apply(values tCC,  l-> fold(l,(i,j)->i|j))
wList = findWeights E

assert(#cList == #wList)

getColumns := mat -> toList apply( 0..<numgens source mat, i->mat_i )

-- assumes that both lists have equal length
areEqualListsModPerm = (L1,L2) -> (
  if #L1 == 0 then return true; --implicit: #L2==0
  pos := positions(L2, l-> l==L1#0);
  if #pos == 0 then return false;
  return areEqualListsModPerm(drop(L1,{0,0}),drop(L2,{pos#0,pos#0}))
)

foundList = for i in 0 ..< #cList list (
 found := -1;
 for j in 0 ..< #wList do (
  cChars := getColumns cList_i;
  for k in 0 ..< #(wList_j) do (
   wChars := getColumns (wList_j)_k;
   if areEqualListsModPerm(cChars,wChars) then (
    found = j;
    break;
   )
  )
 );
 found
)

assert(all(foundList, i->i>=0))
///

-- Test 9
TEST ///
-- Test whether the filtration steps obtained from the toric Chern character are correct
-- Such a test would have failed before version 1.8, 
-- because of a bug in the internal method flags:
-- the method made an assumption on the form how the filtration steps are ordered 
-- in the bundles generated by the package ToricVectorBundles. 
-- Usually true, this assumption does not apply, if the bundle arises 
-- by using the method dual of ToricVectorBundles (e.g. cotangent bundles).

E = dual tangentBundle projectiveSpaceFan 2

getCols = mat -> toList apply( 0..<numgens source mat, i->mat_i )

filtE = applyValues(filtration E, filt -> flatten entries filt);
raysE = keys filtE;
filtFromTCC := applyPairs( toricChernCharacter E, (cone,us) -> 
 cone => (
  filtRay := for ray in getCols cone list sort apply(us, u -> ( (transpose matrix ray)*u)_(0,0))
 )
);

applyPairs(filtFromTCC, (cone, filts) -> (
  cone => for ray in getCols cone do
           assert isMember( sort filtE#(matrix ray), filts)
 )
)
///

-- Test 10
TEST ///
-- the methods dual, tensor (and maybe others?) from ToricVectorBundles
-- may produce a ToricVectorBundleKlyachko whose matrices containing the filtration steps
-- have not ascending entries.
-- The method wellformedBundleFiltrations (added in version 1.9) ensures ascending entries.
-- The following test fails when omitting this method.
T = tangentBundle projectiveSpaceFan 2
E = wellformedBundleFiltrations( T ** (dual T))
F = wellformedBundleFiltrations((dual T) ** T)

origin = matrix map(ZZ^2,ZZ^1,0)

assert( all(values toricChernCharacter E, L -> isMember(origin, L)) )
assert( all(values toricChernCharacter F, L -> isMember(origin, L)) )
///

end

