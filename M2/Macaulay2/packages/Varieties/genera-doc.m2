--- status: draft
--- author(s): Decker, Popescu
--- notes: 

document { 
     Key => genera,
     Headline => "list of the successive linear sectional arithmetic genera",
     SeeAlso => {genus,hilbertPolynomial,euler}
     }

doc ///
Node
  Key
    (genera, CoherentSheaf)
    (genera, Module)
  Headline
    the linear sectional arithmetic genera of a coherent sheaf
  Usage
    genera F
  Inputs
    F:CoherentSheaf
  Outputs
    :List
  Description
    Text
      This function computes the list of successive generic linear sectional arithmetic genera
      of a coherent sheaf @TT "M"@ on a closed subscheme $X$ of projective space.
      Writing $m$ for the dimension of the support $X$ of @TT "M"@
      and $X_i$ for a general codimension-$i$ linear section of $X$,
      the $i$th entry in the list is $(-1)^{m-i}(\chi(X_i, M|_{X_i}) - 1)$.
    Example
      V = Proj(ZZ/101[x_0..x_2]);
      M = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})
      genera M
  SeeAlso
    (euler,CoherentSheaf)
    genus

Node
  Key
    (genera, Ideal)
  Headline
    the linear sectional arithmetic genera of the zero locus of an ideal
  Usage
    genera I
  Inputs
    I:Ideal
  Outputs
    :List
  Description
    Text
      This function computes the list of successive generic linear sectional arithmetic genera
      of the zero locus of an ideal @TT "I"@ in a graded ring $R$ (generated in degree 1).
      Writing $m$ for the dimension of the zero locus $X$ of @TT "I"@ in $\mathrm{Proj}(R)$
      and $X_i$ for a general codimension-$i$ linear section of $X$,
      the $i$th entry in the list is $(-1)^{m-i}(\chi(X_i, O) - 1)$.
    Text
      For example, we take a complete intersection surface of type $(2,3)$ in projective 4-space.
      Its hyperplane section is a canonical curve of genus 4:
    Example
      R = ZZ/101[x_0..x_4];
      I = ideal random(R^1, R^{-2,-3});
      genera I
  SeeAlso
    (euler,CoherentSheaf)
    genus

Node
  Key
    (genera, ProjectiveVariety)
  Headline
    the linear sectional arithmetic genera of a projective variety
  Usage
    genera X
  Inputs
    X:ProjectiveVariety
  Outputs
    :List
  Description
    Text
      This function computes the list of successive generic linear sectional arithmetic genera
      of a closed subscheme @TT "X"@ of projective space.
      Writing $m$ for the dimension of @TT "X"@
      and $X_i$ for a general codimension-$i$ linear section of @TT "X"@,
      the $i$th entry in the list is $(-1)^{m-i}(\chi(X_i, O) - 1)$.
    Text
      For example, we take a complete intersection surface of type $(2,3)$ in projective 4-space.
      Its hyperplane section is a canonical curve of genus 4:
    Example
      R = ZZ/101[x_0..x_4];
      X = Proj(R/(ideal random(R^1, R^{-2,-3})));
      genera X
  SeeAlso
    (euler,CoherentSheaf)
    genus

Node
  Key
    (genera, Ring)
  Headline
    the linear sectional arithmetic genera of Proj of a graded ring
  Usage
    genera R
  Inputs
    R:Ring
  Outputs
    :List
  Description
    Text
      This function computes the list of successive generic linear sectional arithmetic genera
      of the projective scheme $X=\mathrm{Proj}(R)$, for $R$ an algebra generated in degree 1.
      Writing $m$ for the dimension of @TT "X"@
      and $X_i$ for a general codimension-$i$ linear section of $X$,
      the $i$th entry in the list is $(-1)^{m-i}(\chi(X_i, O) - 1)$.
    Text
      For example, we take a complete intersection surface of type $(2,3)$ in projective 4-space.
      Its hyperplane section is a canonical curve of genus 4:
    Example
      R = ZZ/101[x_0..x_4];
      I = ideal random(R^1, R^{-2,-3});
      genera (R/I)
  SeeAlso
    (euler,CoherentSheaf)
    genus
    ///
