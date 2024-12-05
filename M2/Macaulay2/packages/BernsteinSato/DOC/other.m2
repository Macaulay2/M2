----------------------------------------------------------------------------
-- (better docs needed)
----------------------------------------------------------------------------

document {
     Key => {localizeCharacteristicCycle, (localizeCharacteristicCycle, List, RingElement), (localizeCharacteristicCycle, Ideal, RingElement)},
     Headline => "the characteristic cycle of the localized $D$-module",
     Usage => "localizeCharacteristicCycle(cc,f), localizeCharacteristicCycle(I,cc)",
     Inputs => {
	  "cc" => List => {"the characteristic cycle of a regular holonomic ", TEX "D-module $M$"},
	  "I" => Ideal => {"representing an `simple' ", TT "cc"}   
	  },
     Outputs => {
     	  "List" => TEX "the characteristic cycle of the localized module $M_f = M[f^{-1}]$" 
	  },
     PARA {"Provided a characteristic cycle in the form ", TT "{I_1 => m_1, ..., I_k => m_k}", 
     " with associated prime ideals ", TEX "I_1,...,I_k", " and the multiplicities ", TEX "m_1,...,m_k", 
     " of ", TEX "M", " along them, the routine computes the characteristic cycle of ", TEX "M_f", "."},
     PARA {"The method is based on a geometric formula given by V.Ginsburg in ", 
     	  EM "Characteristic varieties and vanishing cycles, Invent. Math. 84 (1986), 327--402.", " and 
	  reinterpreted by J.Briancon, P.Maisonobe and M.Merle in ", 
	  EM "Localisation de systemes differentiels, stratifications de
  	  Whitney et condition de Thom, Invent. Math. 117 (1994), 531--550", "."}, 
     EXAMPLE lines ///
     	  A =  QQ[x_1,x_2,a_1,a_2]
	  cc = {ideal A => 1} -- the characteristic ideal of R = CC[x_1,x_2] 
	  cc1 = localizeCharacteristicCycle(cc,x_1)   -- cc of R_{x_1}
	  cc12 = localizeCharacteristicCycle(cc1,x_2) -- cc of R_{x_1x_2}
	  ///,
     Caveat => {"The module has to be a regular holonomic complex-analytic module; 
	  while the holomicity can be checked by ", 
	  TO "isHolonomic", " there is no algorithm to check the regularity."},
     SeeAlso => {pruneCechComplexCC,populateCechComplexCC}
     }

document {
     Key => {(pruneCechComplexCC, MutableHashTable), pruneCechComplexCC},
     Headline => "reduction of the Cech complex that produces characteristic 
     cycles of local cohomology modules",
     Usage => "pruneCechComplexCC M",
     Inputs => {
     	  "M" => {"the output of ", TO "populateCechComplexCC"}
	  },
     Outputs => {
     	  MutableHashTable
	  },
     "The function reduces the Cech complex skeleton produced by ", TO "populateCechComplexCC", 
     " leaving the pieces of the characteristic cycles of the chains that together constitute 
     the characteristic cycles of the local cohomology modules.",    
     EXAMPLE lines ///
W =  QQ[x_1..x_6, a_1..a_6];
I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, 0, 0}});
cc = {ideal W => 1};
///,
     Caveat => {"The module has to be a regular holonomic complex-analytic module; 
	  while the holomicity can be checked by ", 
	  TO "isHolonomic", " there is no algorithm to check the regularity."},
     SeeAlso => {localizeCharacteristicCycle,populateCechComplexCC}
     }

document {
     Key => {(populateCechComplexCC, Ideal, List), populateCechComplexCC},
     Headline => "Cech complex skeleton for the computation of the characteristic 
     cycles of local cohomology modules",
     Usage => "populateCechComplexCC(I,cc)",
     Inputs => {
	  "I" => {"at which the local cohomology modules ", TEX "H^i_I(M)", " are computed."},
	  "cc" => {"the characteristic cycle of a regular holonomic module ", TEX "M"} 
	  },
     Outputs => {
     	  MutableHashTable => {"with entries corresponding to the direct summands of the chains in the Cech complex"}
	  },
     "For the ideal ", TEX "I=(f_1,...,f_k)", " the routine computes the characteristic cycles of the localized modules ", 
     TEX "M_{f_{i_1},...,f_{i_k}}", " and places them in the corresponding places in the Cech complex.",
     EXAMPLE lines ///
W =  QQ[x_1..x_6, a_1..a_6];
I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, 0, 0}});
cc = {ideal W => 1};
///,
     Caveat => {"The module has to be a regular holonomic complex-analytic module; 
	  while the holomicity can be checked by ", 
	  TO "isHolonomic", " there is no algorithm to check the regularity."},
     SeeAlso => {localizeCharacteristicCycle,pruneCechComplexCC}
     }

document {     
     Key => {(logCohomology,RingElement),logCohomology},
     Headline => "logarithmic cohomology groups in two variables",
     Usage => "logCohomology f",
     Inputs => {
	  "f" => {"polynomial in two variables"}
	  },
     Outputs => {
	  HashTable => {"with entries 
	       {VResolution, Input, TransferCycles, CohomologyGroups, PreCycles, OmegaRes, LocalizeMap, BFunction}
	       "} 
	  },
     "For a polynomial ", TEX "f", " in two variables executes the algorithm described in
     Castro-Jimenez and Takayama \"The Computation of the Logarithmic Cohomology for Plane Curves\" (arXiv:0712.0001).",
     EXAMPLE lines ///
S=QQ[x,y];
f=x*y*(x-y);
logCohomology(f)
///,
     SeeAlso => {deRham}
     }

document { -- local?
     Key => {ExternalProduct, (ExternalProduct,ChainComplex,ChainComplex), (ExternalProduct,Module,Module)},
     Headline => "external product of modules or complexes"
     }
document { -- local?
     Key => [ExternalProduct,TwistMap],
     Headline => "indicates whether TwistMap should be computed"
     }
document {
     Key => TwistMap,
     Headline => "indicates whether TwistMap should be computed"
     }

document {
     Key => twistMap,
     Headline => "a key attached by ExternalProduct",
     "see ", TO "ExternalProduct"
     }
document {
     Key => twistInvMap,
     Headline => "a key attached by ExternalProduct",
     "see ", TO "ExternalProduct"
     }
document {
     Key => (projMap1),
     Headline => "a key attached by ExternalProduct",
     "see ", TO "ExternalProduct"
     }
document {
     Key => (projMap2),
     Headline => "a key attached by ExternalProduct",
     "see ", TO "ExternalProduct"
     }
