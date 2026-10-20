----------------------------------------------------------------------------
-- (better docs needed)
----------------------------------------------------------------------------

--- old format (commented out) ---
-- document {
--      Key => {localizeCharacteristicCycle, (localizeCharacteristicCycle, List, RingElement), (localizeCharacteristicCycle, Ideal, RingElement)},
--      Headline => "the characteristic cycle of the localized $D$-module",
--      Usage => "localizeCharacteristicCycle(cc,f), localizeCharacteristicCycle(I,cc)",
--      Inputs => {
-- 	  "cc" => List => {"the characteristic cycle of a regular holonomic ", TEX "D-module $M$"},
-- 	  "I" => Ideal => {"representing an `simple' ", TT "cc"}
-- 	  },
--      Outputs => {
--      	  "List" => TEX "the characteristic cycle of the localized module $M_f = M[f^{-1}]$"
-- 	  },
--      PARA {"Provided a characteristic cycle in the form ", TT "{I_1 => m_1, ..., I_k => m_k}",
--      " with associated prime ideals ", TEX "I_1,...,I_k", " and the multiplicities ", TEX "m_1,...,m_k",
--      " of ", TEX "M", " along them, the routine computes the characteristic cycle of ", TEX "M_f", "."},
--      PARA {"The method is based on a geometric formula given by V.Ginsburg in ",
--      	  EM "Characteristic varieties and vanishing cycles, Invent. Math. 84 (1986), 327--402.", " and
-- 	  reinterpreted by J.Briancon, P.Maisonobe and M.Merle in ",
-- 	  EM "Localisation de systemes differentiels, stratifications de
--   	  Whitney et condition de Thom, Invent. Math. 117 (1994), 531--550", "."},
--      EXAMPLE lines ///
--      	  A =  QQ[x_1,x_2,a_1,a_2]
-- 	  cc = {ideal A => 1} -- the characteristic ideal of R = CC[x_1,x_2]
-- 	  cc1 = localizeCharacteristicCycle(cc,x_1)   -- cc of R_{x_1}
-- 	  cc12 = localizeCharacteristicCycle(cc1,x_2) -- cc of R_{x_1x_2}
-- 	  ///,
--      Caveat => {"The module has to be a regular holonomic complex-analytic module;
-- 	  while the holomicity can be checked by ",
-- 	  TO "isHolonomic", " there is no algorithm to check the regularity."},
--      SeeAlso => {pruneCechComplexCC,populateCechComplexCC}
--      }

doc ///
  Key
    localizeCharacteristicCycle
    (localizeCharacteristicCycle, List, RingElement)
    (localizeCharacteristicCycle, Ideal, RingElement)
  Headline
    the characteristic cycle of the localized $D$-module
  Usage
    localizeCharacteristicCycle(cc, f)
    localizeCharacteristicCycle(I, cc)
  Inputs
    cc:List
      the characteristic cycle of a regular holonomic $D$-module $M$
    I:Ideal
      representing a simple @TT "cc"@
  Outputs
    :List
      the characteristic cycle of the localized module $M_f = M[f^{-1}]$
  Description
    Text
      Provided a characteristic cycle in the form @TT "{I_1 => m_1, ..., I_k => m_k}"@
      with associated prime ideals $I_1,\ldots,I_k$ and the multiplicities $m_1,\ldots,m_k$
      of $M$ along them, the routine computes the characteristic cycle of $M_f$.

      The method is based on a geometric formula given by V. Ginsburg in
      [@TO2 ("WeylAlgebras :: Works Cited", "Gin86")@] and reinterpreted by
      J. Brian\c{c}on, P. Maisonobe and M. Merle in [@TO2 ("WeylAlgebras :: Works Cited", "BMM94")@].
    Example
      A = QQ[x_1,x_2,a_1,a_2]
      cc = {ideal A => 1} -- the characteristic ideal of R = CC[x_1,x_2]
      cc1 = localizeCharacteristicCycle(cc,x_1)   -- cc of R_{x_1}
      cc12 = localizeCharacteristicCycle(cc1,x_2) -- cc of R_{x_1x_2}
  Caveat
    The module has to be a regular holonomic complex-analytic module;
    while the holonomicity can be checked by @TO "isHolonomic"@
    there is no algorithm to check the regularity.
  SeeAlso
    pruneCechComplexCC
    populateCechComplexCC
///

--- old format (commented out) ---
-- document {
--      Key => {(pruneCechComplexCC, MutableHashTable), pruneCechComplexCC},
--      Headline => "reduction of the Cech complex that produces characteristic
--      cycles of local cohomology modules",
--      Usage => "pruneCechComplexCC M",
--      Inputs => {
--      	  "M" => {"the output of ", TO "populateCechComplexCC"}
-- 	  },
--      Outputs => {
--      	  MutableHashTable
-- 	  },
--      "The function reduces the Cech complex skeleton produced by ", TO "populateCechComplexCC",
--      " leaving the pieces of the characteristic cycles of the chains that together constitute
--      the characteristic cycles of the local cohomology modules.",
--      EXAMPLE lines ///
-- W =  QQ[x_1..x_6, a_1..a_6];
-- I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, 0, 0}});
-- cc = {ideal W => 1};
-- ///,
--      Caveat => {"The module has to be a regular holonomic complex-analytic module;
-- 	  while the holomicity can be checked by ",
-- 	  TO "isHolonomic", " there is no algorithm to check the regularity."},
--      SeeAlso => {localizeCharacteristicCycle,populateCechComplexCC}
--      }

doc ///
  Key
    (pruneCechComplexCC, MutableHashTable)
    pruneCechComplexCC
  Headline
    reduction of the Cech complex that produces characteristic cycles of local cohomology modules
  Usage
    pruneCechComplexCC M
  Inputs
    M:MutableHashTable
      the output of @TO "populateCechComplexCC"@
  Outputs
    :MutableHashTable
  Description
    Text
      The function reduces the Cech complex skeleton produced by @TO "populateCechComplexCC"@
      leaving the pieces of the characteristic cycles of the chains that together constitute
      the characteristic cycles of the local cohomology modules.
    Example
      W = QQ[x_1..x_6, a_1..a_6];
      I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, 0, 0}});
      cc = {ideal W => 1};
      M = populateCechComplexCC(I, cc);
      M#{0,1}
      pruneCechComplexCC M;
      M#{0,1}
  Caveat
    The module has to be a regular holonomic complex-analytic module;
    while the holonomicity can be checked by @TO "isHolonomic"@
    there is no algorithm to check the regularity.
  SeeAlso
    localizeCharacteristicCycle
    populateCechComplexCC
///

--- old format (commented out) ---
-- document {
--      Key => {(populateCechComplexCC, Ideal, List), populateCechComplexCC},
--      Headline => "Cech complex skeleton for the computation of the characteristic
--      cycles of local cohomology modules",
--      Usage => "populateCechComplexCC(I,cc)",
--      Inputs => {
-- 	  "I" => {"at which the local cohomology modules ", TEX "H^i_I(M)", " are computed."},
-- 	  "cc" => {"the characteristic cycle of a regular holonomic module ", TEX "M"}
-- 	  },
--      Outputs => {
--      	  MutableHashTable => {"with entries corresponding to the direct summands of the chains in the Cech complex"}
-- 	  },
--      "For the ideal ", TEX "I=(f_1,...,f_k)", " the routine computes the characteristic cycles of the localized modules ",
--      TEX "M_{f_{i_1},...,f_{i_k}}", " and places them in the corresponding places in the Cech complex.",
--      EXAMPLE lines ///
-- W =  QQ[x_1..x_6, a_1..a_6];
-- I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, 0, 0}});
-- cc = {ideal W => 1};
-- ///,
--      Caveat => {"The module has to be a regular holonomic complex-analytic module;
-- 	  while the holomicity can be checked by ",
-- 	  TO "isHolonomic", " there is no algorithm to check the regularity."},
--      SeeAlso => {localizeCharacteristicCycle,pruneCechComplexCC}
--      }

doc ///
  Key
    (populateCechComplexCC, Ideal, List)
    populateCechComplexCC
  Headline
    Cech complex skeleton for the computation of the characteristic cycles of local cohomology modules
  Usage
    populateCechComplexCC(I, cc)
  Inputs
    I:Ideal
      at which the local cohomology modules $H^i_I(M)$ are computed
    cc:List
      the characteristic cycle of a regular holonomic module $M$
  Outputs
    :MutableHashTable
      with entries corresponding to the direct summands of the chains in the Cech complex
  Description
    Text
      For the ideal $I = (f_1,\ldots,f_k)$ the routine computes the characteristic cycles of the localized modules
      $M_{f_{i_1},\ldots,f_{i_k}}$ and places them in the corresponding places in the Cech complex.
    Example
      W = QQ[x_1..x_6, a_1..a_6];
      I = minors(2, matrix{{x_1, x_2, x_3}, {x_4, 0, 0}});
      cc = {ideal W => 1};
      populateCechComplexCC(I, cc)
  Caveat
    The module has to be a regular holonomic complex-analytic module;
    while the holonomicity can be checked by @TO "isHolonomic"@
    there is no algorithm to check the regularity.
  SeeAlso
    localizeCharacteristicCycle
    pruneCechComplexCC
///

--- old format (commented out) ---
-- document {
--      Key => {(logCohomology,RingElement),logCohomology},
--      Headline => "logarithmic cohomology groups in two variables",
--      Usage => "logCohomology f",
--      Inputs => {
-- 	  "f" => {"polynomial in two variables"}
-- 	  },
--      Outputs => {
-- 	  HashTable => {"with entries
-- 	       {VResolution, Input, TransferCycles, CohomologyGroups, PreCycles, OmegaRes, LocalizeMap, BFunction}
-- 	       "}
-- 	  },
--      "For a polynomial ", TEX "f", " in two variables executes the algorithm described in
--      Castro-Jimenez and Takayama \"The Computation of the Logarithmic Cohomology for Plane Curves\" (arXiv:0712.0001).",
--      EXAMPLE lines ///
-- S=QQ[x,y];
-- f=x*y*(x-y);
-- logCohomology(f)
-- ///,
--      SeeAlso => {deRham}
--      }

doc ///
  Key
    (logCohomology, RingElement)
    logCohomology
  Headline
    logarithmic cohomology groups in two variables
  Usage
    logCohomology f
  Inputs
    f:RingElement
      polynomial in two variables
  Outputs
    :HashTable
      The logarithmic cohomology groups (key @TT "CohomologyGroups"@) along with
      intermediate data from the computation: @TT "VResolution"@, @TT "Input"@,
      @TT "TransferCycles"@, @TT "PreCycles"@, @TT "OmegaRes"@, @TT "LocalizeMap"@,
      and @TT "BFunction"@.
  Description
    Text
      For a polynomial $f$ in two variables executes the algorithm described in
      [@TO2 ("WeylAlgebras :: Works Cited", "CJT01")@].
    Example
      S = QQ[x,y];
      f = x*y*(x-y);
      logCohomology(f)
  SeeAlso
    deRham
///

--- old format (commented out) ---
-- document { -- local?
--      Key => {ExternalProduct, (ExternalProduct,Complex,Complex), (ExternalProduct,Module,Module)},
--      Headline => "external product of modules or complexes"
--      }

doc ///
  Key
    ExternalProduct
    (ExternalProduct, Complex, Complex)
    (ExternalProduct, Module, Module)
  Headline
    external product of modules or complexes
  Usage
    ExternalProduct(M, N)
    ExternalProduct(F, G)
  Inputs
    M:Module
      a quotient module over a Weyl algebra $W_1$
    N:Module
      a quotient module over a Weyl algebra $W_2$
    F:Complex
      a complex over a Weyl algebra $W_1$
    G:Complex
      a complex over a Weyl algebra $W_2$
    TwistMap=>Boolean
      if @TT "true"@ (only valid when $W_1 = W_2$), additionally attaches
      twist maps to the diagonal; see @TO twistMap@
  Outputs
    :Module
    :Complex
      the external tensor product over the Weyl algebra $W = W_1 \otimes W_2$.
      Projection maps back to the factors are attached as @TO projMap1@ and @TO projMap2@.
  Description
    Text
      Forms the external product of two $D$-modules (or complexes thereof).
      The result lives over a new Weyl algebra $W$ that combines the variables
      of both inputs.

      When @TT "TwistMap => true"@ is supplied (and both inputs share the
      same Weyl algebra), additional data is attached to $W$ for working with
      the diagonal: see @TO twistMap@ and @TO twistInvMap@.
///

--- old format (commented out) ---
-- document { -- local?
--      Key => [ExternalProduct,TwistMap],
--      Headline => "indicates whether TwistMap should be computed"
--      }

doc ///
  Key
    [ExternalProduct, TwistMap]
  Headline
    indicates whether TwistMap should be computed
///

--- old format (commented out) ---
-- document {
--      Key => TwistMap,
--      Headline => "indicates whether TwistMap should be computed"
--      }

doc ///
  Key
    TwistMap
  Headline
    indicates whether TwistMap should be computed
///

--- old format (commented out) ---
-- document {
--      Key => twistMap,
--      Headline => "a key attached by ExternalProduct",
--      "see ", TO "ExternalProduct"
--      }

doc ///
  Key
    twistMap
  Headline
    a key attached by ExternalProduct
  Description
    Text
      The twist map $W \to W$ to the diagonal, attached to the new Weyl
      algebra by @TO ExternalProduct@ when @TT "TwistMap => true"@.
///

--- old format (commented out) ---
-- document {
--      Key => twistInvMap,
--      Headline => "a key attached by ExternalProduct",
--      "see ", TO "ExternalProduct"
--      }

doc ///
  Key
    twistInvMap
  Headline
    a key attached by ExternalProduct
  Description
    Text
      The inverse of @TO twistMap@, attached to the new Weyl algebra by
      @TO ExternalProduct@ when @TT "TwistMap => true"@.
///

--- old format (commented out) ---
-- document {
--      Key => (projMap1),
--      Headline => "a key attached by ExternalProduct",
--      "see ", TO "ExternalProduct"
--      }

doc ///
  Key
    projMap1
  Headline
    a key attached by ExternalProduct
  Description
    Text
      The projection map from the combined Weyl algebra $W = W_1 \otimes W_2$
      to its first factor $W_1$, attached by @TO ExternalProduct@.
///

--- old format (commented out) ---
-- document {
--      Key => (projMap2),
--      Headline => "a key attached by ExternalProduct",
--      "see ", TO "ExternalProduct"
--      }

doc ///
  Key
    projMap2
  Headline
    a key attached by ExternalProduct
  Description
    Text
      The projection map from the combined Weyl algebra $W = W_1 \otimes W_2$
      to its second factor $W_2$, attached by @TO ExternalProduct@.
///
