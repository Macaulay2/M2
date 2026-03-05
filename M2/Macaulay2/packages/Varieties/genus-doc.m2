--- status: draft
--- author(s): Decker, Popescu
--- notes: 

document { 
     Key => genus,
     Headline => "arithmetic genus",
     SeeAlso => {genera, euler}
     }

 doc ///
 Node
  Key
    (genus, CoherentSheaf)
    (genus, Module)
    (genus, Ideal)
  Headline
    the arithmetic genus of a coherent sheaf
  Usage
    genus F
  Inputs
    F:CoherentSheaf
  Outputs
    :ZZ
  Description
    Text
      This function computes the arithmetic genus of the coherent sheaf @TT "F"@. If $m$ denotes the dimension
      of the support of @TT "F"@, the arithmetic genus is defined here as $(-1)^m(\chi(X,F) - 1)$.
      Here @TT "F"@ may be defined on a closed subspace $X$ of projective space
      or more generally of a weighted projective space.
    Text
      If the input is a graded module @TT "M"@,
      then the genus of the associated sheaf @TT "M^~"@ is computed.
    Text
      If the input is a homogeneous ideal @TT "I"@ in a graded ring $R$,
      then the genus of $\mathrm{Proj}(R/I)$ (that is, of its structure sheaf) is computed.
    Text
      For example, the nodal cubic curve has arithmetic genus 1:
    Example
      V = Proj(QQ[x,y,z]/ideal(y^2*z-x^2*(x+z)));
      genus OO_V^1
  SeeAlso
    (euler,CoherentSheaf)
    (genus,ProjectiveVariety)
    genera

 Node
  Key
    (genus, ProjectiveVariety)
  Headline
    the arithmetic genus of a projective variety
  Usage
    genus X
  Inputs
    X:ProjectiveVariety
  Outputs
    :ZZ
  Description
    Text
      This function computes the arithmetic genus of a projective scheme @TT "X"@. If $m$ denotes the dimension
      of @TT "X"@, the arithmetic genus is defined here as $(-1)^m(\chi(X,O) - 1)$.
      Here @TT "X"@ may be a closed subspace $X$ of projective space
      or more generally of a weighted projective space.
    Text
      For example, the nodal cubic curve has arithmetic genus 1:
    Example
      V = Proj(QQ[a,b,c]/ideal(b^2*c-a^2*(a+c)));
      genus V
    Text
      The Fano model of a Reye-type Enriques surface in projective 5-space.
    Example
      R = ZZ/101[x_0..x_5];
      M = random(R^4, R^{4:-1});
      I = minors(3, M+transpose(M));
      V = Proj(R/I);
      genus V
  SeeAlso
    (euler,CoherentSheaf)
    genera

 Node
  Key
    (genus, Ring)
  Headline
    the arithmetic genus of Proj of a graded ring
  Usage
    genus R
  Inputs
    R:Ring
  Outputs
    :ZZ
  Description
    Text
      This function computes the arithmetic genus of the projective scheme $X = \mathrm{Proj}(R)$.
      If $m$ denotes the dimension
      of @TT "X"@, the arithmetic genus is defined here as $(-1)^m(\chi(X,O) - 1)$.
      Here @TT "X"@ may be a closed subspace $X$ of projective space
      or more generally of a weighted projective space.
    Text
      For example, the cuspidal cubic curve has arithmetic genus 1:
    Example
      R = QQ[x,y,z]/ideal(y^2*z-x^3);
      genus R
  SeeAlso
    (euler,CoherentSheaf)
    genera
///
