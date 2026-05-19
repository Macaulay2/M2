newPackage(
    "SimplicialModules",
     AuxiliaryFiles => true,
     Version => "0.1",
     Date => "April 27, 2026",
     Authors => {
	{Name => "Keller VandeBogert", Email => "kvandebo@nd.edu", HomePage => "https://sites.google.com/view/kellervandebogert/home"},
	{Name => "Michael DeBellevue", Email => "michael.debellevue@gmail.com"}},
     Headline => "methods for working in the category of simplicial modules",
     Keywords => {"Homological Algebra", "Commutative Algebra"},
     PackageExports => {"Complexes", "SchurFunctors"}
    )

export {"SimplicialModule",
    "SimplicialModuleMap",
    "simplicialModule",
    --"combineSFactors",
    "forgetComplex",
    "forgetDegeneracy",
    "isSimplicialModule",
    "isSimplicialMorphism",
    "randomSimplicialMap",
    "tensorwithComponents",
    "topDegree",
    "exteriorInclusion",
    "extPower",
    "naiveNorm",
    "schurMap",
    "simplicialTensor",
    "symmetricQuotient",
    "tensorLES",
    --"CheckMap",
    "CheckComplex",
    "RememberSummands",
    "Degeneracy",
    "TopDegree",
    "ss",
    "complexLength",
    "CheckSum",
    "complexMap",
    "summandSurjection"
    }

-- normalize is owned and exported by SchurFunctors; SimplicialModules picks
-- it up automatically through PackageExports above and installs its own
-- normalize methods on simplicial modules in Normalization.m2.




------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- **CODE** --
------------------------------------------------------------------------------
------------------------------------------------------------------------------

load "./SimplicialModules/SimplicialMapUtilities.m2"
load "./SimplicialModules/SimplicialModule.m2"
load "./SimplicialModules/Normalization.m2"



-----------------------------------------------------------------------------
-- Documentation
-----------------------------------------------------------------------------

beginDocumentation()

load "./SimplicialModules/SimplicialModuleDOC.m2"



-----------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------

load "./SimplicialModules/SimplicialModuleTESTS1.m2"



-----------------------------------------------------------------------------
-- Development
-----------------------------------------------------------------------------

end--

uninstallPackage "SimplicialModules"
restart
debug installPackage "SimplicialModules"
debug loadPackage("SimplicialModules", LoadDocumentation => true, Reload => true)
debug needsPackage "SimplicialModules"
check SimplicialModules
viewHelp SimplicialModules

  Q = QQ[x_1..x_3];
  K = koszulComplex vars Q;
  isWellDefined simplicialModule(K, 4) --no degeneracy here
  S = simplicialModule(K,4, Degeneracy => true)
  S.dd**Q^1
  isWellDefined S
  Sdegen = simplicialModule(K,4,Degeneracy => true)
  S.dd
  --S.ss --should return error, no such key
  Sdegen.ss --this is the version that actually has degeneracy maps cached
  simplicialModule(K, 5)
  C = complex(Q^1, Base => 1)
  simplicialModule(C, 3)
  isWellDefined oo
  simplicialModule(complex(Q^2, Base => 2), 6, Degeneracy => true)
  isWellDefined oo --nice, now we can be confident the objects we are messing with actually make any sense
  oo.ss
  id_S
  phi = exteriorInclusion(S);
  prune phi
  prune coker phi
  K = koszulComplex {x_1,x_2}
  Sn = simplicialModule(K, 4, Degeneracy => true)
  phi = exteriorInclusion(K)
  isCommutative phi
  isWellDefined phi
  isCommutative prune phi
  isWellDefined prune phi
  sphi = exteriorInclusion(Sn);
  psphi = prune sphi;
  isWellDefined (source psphi).cache.pruningMap
  isCommutative (source psphi).cache.pruningMap 
  isSimplicialMorphism (source psphi).cache.pruningMap  --pruningMaps are well-defined morphisms
  
  S = simplicialModule(K,6)
  

  p = randomComplexMap(K, K, Degree => 1, InternalDegree => 1)
  sp = simplicialModule p
  isWellDefined sp
  isCommutative sp
  normalize(sp, CheckComplex => false)
  prune oo  ---reobtain p
  p' = randomComplexMap(K, K, Degree => -1)
  sp' = simplicialModule p'
  isWellDefined sp'
  isCommutative sp'
  normalize(sp', CheckComplex => false)
  prune oo --reobtain p'
  p = randomComplexMap(K, K, Degree => 1, Cycle => true, InternalDegree => 2)
  sp = simplicialModule p
  isWellDefined sp
  isCommutative sp
  normalize(sp, CheckComplex => false)
  prune oo  ---reobtain p
  diffs = simplicialModule K.dd
  isWellDefined diffs
  isCommutative diffs

  image(id_S) == S
  simplicialModule(id_K, 6) == id_S
  isSimplicialMorphism id_S
  simplicialModule(complex(Q^0), 6, Degeneracy => true) -- should be able to input 0 and get a well-defined output
  isWellDefined oo
  phi = randomSimplicialMap(S, S, Cycle => true, InternalDegree => 1)
  isWellDefined phi
  isCommutative phi

  bS = basis(0, forgetComplex S)
  prune bS.dd
  isWellDefined bS
  isWellDefined prune bS
  bid = basis(0, id_(forgetComplex S))
  isWellDefined bid
  isCommutative bid
  isWellDefined prune bid
  isCommutative prune bid

  fS = forgetComplex S
  fS.?ss

  bS = basis(1, forgetComplex S)
  isWellDefined bS
  prune bS.dd
  isWellDefined oo
  prune bS.ss
  isWellDefined oo
  normalize bS
  prune oo
  prune basis(1, K)

  bS = basis(3, forgetComplex S)
  isWellDefined bS
  prune bS.dd;
  isWellDefined oo
  prune bS.ss;
  isWellDefined oo
  normalize bS
  prune oo
  prune basis(3, koszulComplex vars Q)

  tS = truncate(1, S)
  isWellDefined tS
  prune tS
  isWellDefined prune tS
  tS = truncate(1, fS)
  isWellDefined tS
  prune tS
  isWellDefined prune tS
  isCommutative truncate(2, id_S)
  isCommutative truncate(2, id_fS)

  K = koszulComplex {x_1,x_2}
  isWellDefined (Sc = schurMap({1,1}, K))
  prune ext(2, K)
  prune schurMap({2,1}, K, TopDegree => 4)
