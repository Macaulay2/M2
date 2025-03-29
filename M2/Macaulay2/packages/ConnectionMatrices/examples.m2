doc ///
Key
    "Examples from particle physics and cosmology"
    "examples from physics"
Description
  Text
    The following $D$-ideal corresponds to the $D$-ideal behind the cosmological correlator of the two-site chain as in Equation (11) in [FPSW].
  Example
    w = {1,1,1};
    D = makeWeylAlgebra(frac(QQ[ϵ])[x,y,z], w);
    delta1 = (x^2-z^2)*dx^2+2*(1-ϵ)*x*dx-ϵ*(1-ϵ);
    delta2 = (y^2-z^2)*dy^2+2*(1-ϵ)*y*dy-ϵ*(1-ϵ);
    delta3 = (x+z)*(y+z)*dx*dy-ϵ*(x+z)*dx-ϵ*(y+z)*dy+ϵ^2;
    h = x*dx+y*dy+z*dz-2*ϵ;
    I = ideal(delta1+delta3, delta2+delta3, h)
  Text
    First, we check that the system has finite holonomic rank using @TO holonomicRank@.
  Example
    assert(holonomicRank I == 4)
  Text
    Then, we compute the system in connection form and verify that it meets the integrability conditions.
  Example
    elapsedTime P = connectionMatrices I;
    elapsedTime assert isIntegrable P
    netList P
  Text
    Next, we use @TO2(gaugeMatrix, "gauge matrix")@ for changing base to a base given by
    suitable set of standard monomials, and compute the @TO2(gaugeTransform, "gauge transform")@
    with respect to this gauge matrix.
  Example
    F = baseFractionField D;
    B = {1_D,dx,dy,dx*dy};
    elapsedTime g = gaugeMatrix(I, B);
    elapsedTime P1 = gaugeTransform(g, P);
    elapsedTime assert isIntegrable P1
    netList P1
  Text
    Now we are ready to perform the gauge transform from this basis to the $\epsilon$-factorized form.
  Example
    changeEps = transpose((1/(2*z*ϵ^2)) * matrix {
	    {2*z*ϵ^2, -ϵ^2*(x-z), -ϵ^2*(y-z), -ϵ^2*(x+y)},
	    {0, ϵ*(x^2-z^2), 0, ϵ*(x+y)*(x+z)},
	    {0, 0, ϵ*(y^2-z^2), ϵ*(x+y)*(y+z)},
	    {0, 0, 0, -(x+y)*(x+z)*(y+z)}})
    elapsedTime P2 = gaugeTransform(changeEps, P1);
    elapsedTime assert isIntegrable P2
    netList P2
  Text
    Finally, we verify that only the last system is in the $\epsilon$-factorized form using @TO isEpsilonFactorized@.
  Example
    assert isEpsilonFactorized(P2, ϵ)
    assert not isEpsilonFactorized(P1, ϵ)
    assert not isEpsilonFactorized(P, ϵ)
References
  [@HREF("https://arxiv.org/pdf/2410.14757","FPSW")@]  C. Fevola, G. L. Pimentel, A.-L. Sattelberger, and T. Westerdijk. Algebraic Approaches to Cosmological Integrals. Preprint arXiv:2410.14757. To appear in {\em Le Matematiche}.

  [@HREF("https://link.springer.com/article/10.1007/s11005-024-01835-7","HPSZ")@] J. Henn, E. Pratt, A.-L. Sattelberger, and S. Zoia. $D$-Module Techniques for Solving Differential Equations behind Feynman Integrals. {\em Letters in Mathematical Physics}, 114(28), 2024.
SeeAlso
  "A GKZ system"
///

doc ///
Key
    "A GKZ system"
    "examples from GKZ systems"
Description
  Text
      Consider the GKZ system representing the Gauss hypergeometric function
      as in [@HREF("https://link.springer.com/book/10.1007/978-3-662-04112-3","SST")@, Example 1.2.9].
  Example
    D = makeWeylAlgebra(frac(QQ[a,b,c])[x_1..x_4]);
    I = ideal(dx_2*dx_3 - dx_1*dx_4,x_1*dx_1 - x_4*dx_4 + 1 - c,x_2*dx_2 + x_4*dx_4 + a,x_3*dx_3 + x_4*dx_4 + b);
    assert(2 == holonomicRank I)
    standardMonomials I
  Text
    [@HREF("https://link.springer.com/book/10.1007/978-3-662-04112-3","SST")@, Example 1.4.23] computes the connection matrices
    for this system with constants $a=1/2,b=1/2,c=1$. Using the @TO connectionMatrices@ function,
    we can find the system for arbitrary constants.
  Example
    A = connectionMatrices I;
    isIntegrable A
    netList(Boxes => false, VerticalSpace => 1, apply(4, i -> i+1 => A#i))
  Text
    Substituting the constants, we note that example in [SST] contains a small misprint.
  Example
    netList(Boxes => false, VerticalSpace => 1,
	apply(4, i -> i+1 => sub(A#i, {a => 1/2, b => 1/2, c => 1})))
  Code
      D := makeWeylAlgebra(frac(QQ[a,b,c])[x_1..x_4]);
      I := ideal(
	  dx_2*dx_3 - dx_1*dx_4,
	  x_1*dx_1 - x_4*dx_4 + 1 - c,
	  x_2*dx_2 + x_4*dx_4 + a,
	  x_3*dx_3 + x_4*dx_4 + b);
      A := connectionMatrices I;
      UL apply(4, i -> concatenate_("$A_"|i+1|"=") substring_1 tex sub(A#i, {a => 1/2, b => 1/2, c => 1}))
References
  [@HREF("https://link.springer.com/book/10.1007/978-3-662-04112-3","SST")@] M. Saito, B. Sturmfels, and N. Takayama. {\em Gröbner Deformations of Hypergeometric Differential Equations}. Volume 6 of {\em Algorithms and Computation in Mathematics}. Springer, 2000.
SeeAlso
  "Examples from particle physics and cosmology"
///
