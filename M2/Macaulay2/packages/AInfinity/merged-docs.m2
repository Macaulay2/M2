doc ///
Key
 (sAInfAlg,HashTable)
 (sAInfMod,HashTable,HashTable)
Headline
 use global higher products as typed algebra and module structures
Usage
 B = sAInfAlg p
 G = sAInfMod(p,h)
Description
 Text
  Given a complete algebra table p from @TO aInfinity@ and a complete module table h over it, these constructors reuse the computed operations in the shifted convention. The algebra needs its unshifted ambient resolution for the augmentation. The module constructor checks the module identities over the supplied algebra. All operation slots with nonzero source and target must be present in the input tables, including zero matrices. For a packaged dual h, supply its opposite algebra under the key "algebra". Keep the original table for its normalization shift.
 Example
  S = QQ[x,y]
  R = S/ideal(x^2,x*y)
  p = aInfinity R
  h = aInfinity(p,coker vars R)
  B = sAInfAlg p
  G = sAInfMod(p,h)
  checkStasheff B and checkStasheff G
SeeAlso
 aInfinity
 checkStasheff
 dualAInfinityStructure
///
doc ///
Key
 (burkeResolution,Module,ZZ,Ideal)
Headline
 construct a Burke resolution at a specified rational point
Usage
 F = burkeResolution(M,n,p)
Inputs
 M:Module
 n:ZZ
  a nonnegative homological degree
 p:Ideal
  a rational closed point in the coefficient ring R of M or its ambient polynomial ring over a field
Outputs
 F:Complex
  a complex over R in degrees zero through n
Description
 Text
  This overload constructs a complex resolving the stalk of M at p. It returns a zero complex when the stalk vanishes.
  The resulting matrices live over R. Their homology can have support away from p.
  With @TT "Check => true"@ the computation verifies that consecutive differentials compose to zero over R and that the interior positive homology vanishes at p.
  A call with n equal to zero retains just the labeled degree-zero module. The local model can contain contractible direct summands.
  The chosen quotient ideal is recorded as @TT "F.cache#\"AInfinityModelPoint\""@.
  The computation leaves the ambient point selected by @TT "setMaxIdeal"@ intact. The two-argument overload uses the global construction.
 Example
  S = QQ[x,y];
  R = S/ideal(x*(x-1),y);
  M = R^1/ideal(R_0-1);
  F = burkeResolution(M,3,ideal vars S,Check=>true);
  apply(4,i -> rank F_i)
  G = burkeResolution(M,3,ideal(S_0-1,S_1),Check=>true);
  ann coker G.dd_1
SeeAlso
 burkeResolution
///
