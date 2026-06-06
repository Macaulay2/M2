doc ///
Node
  Key
    eulerSequence
   (eulerSequence, ProjectiveVariety)

-- Note: TruncateDegree is set by Ext^ZZ(CoherentSheaf, SumOfTwists)
Node
  Key
    yonedaSheafExtension
   (yonedaSheafExtension, Matrix)
  SeeAlso
    (Ext, ZZ, CoherentSheaf, CoherentSheaf)

Node
  Key
    idealSheafSequence
   (idealSheafSequence, ProjectiveVariety)
  Headline
    ideal sheaf sequence of a projective variety
  Usage
    idealSheafSequence X
  Inputs
    X:ProjectiveVariety
      The projective variety for which to construct the ideal sheaf sequence
  Outputs
    :Complex
      The ideal sheaf sequence of the projective variety
  Description
    Text
      This method computes the ideal sheaf sequence of the projective variety $X$. The sequence is given by $$0 \to \mathcal{I}_X \to \mathcal{O}_{\mathbb{P}^n} \to \mathcal{O}_{\mathbb{P}^n}/\mathcal{I}_X \to 0,$$ where $\mathcal{I}_X$ is the ideal sheaf of $X$, $\mathcal{O}_{\mathbb{P}^n}$ is the structure sheaf of the ambient projective space, and $\mathcal{O}_{\mathbb{P}^n}/\mathcal{I}_X$ is the quotient sheaf.
    Text
      As an example, consider the projective variety defined by the equation $x^4 + y^4 + z^4 = 0$. The ideal sheaf sequence of this variety is computed.
    Example
      X = Proj QQ[x,y,z]/(x^4+y^4+z^4)
      seq = idealSheafSequence X
  SeeAlso
    idealSheaf
    ProjectiveVariety
  
///
