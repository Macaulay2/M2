doc ///
Key
    "Cosmological correlator for the 2-site chain"
Description
  Text
    Differential equations for correlation functions in cosmology are studied in [ABHJLP].
    Therein, a basis of master integrals is constructed from the underlying hyperplane arrangement
    via canonical forms in the setup of positive geometries, which results in a matrix differential
    equation for the cosmological correlator that is in $\varepsilon$-factorized form.
    For the $2$-site chain (mathematically, the path graph on $2$ vertices), the underlying
    $D$-ideal was investigated in [FPSW].

    Here we revisit the $D$-ideal $I = \langle \nabla_1+\nabla_3,\nabla_2+\nabla_3,H\rangle$ (see Equation (11) in [FPSW]),
    and carry out the gauge transformation to write the connection matrices in $\varepsilon$-factorized form.
    This form is especially useful, as it allows for the construction of formal power series solutions
    in the variable $\varepsilon$ of such systems via the ``path-ordered exponential formalism.’’
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
    elapsedTime A = connectionMatrices I;
    elapsedTime assert isIntegrable A
    netList A
  Text
    Next, we use @TO2(gaugeMatrix, "gauge matrix")@ for changing base to a base given by
    suitable set of standard monomials, and compute the @TO2(gaugeTransform, "gauge transform")@
    with respect to this gauge matrix.
  Example
    F = baseFractionField D;
    B = {1_D,dx,dy,dx*dy};
    elapsedTime g = gaugeMatrix(I, B);
    elapsedTime A1 = gaugeTransform(g, A);
    elapsedTime assert isIntegrable A1
    netList A1
  Text
    Now we are ready to perform the gauge transform from this basis to the $\epsilon$-factorized form.
  Example
    changeEps = transpose((1/(2*z*ϵ^2)) * matrix {
	    {2*z*ϵ^2, -ϵ^2*(x-z), -ϵ^2*(y-z), -ϵ^2*(x+y)},
	    {0, ϵ*(x^2-z^2), 0, ϵ*(x+y)*(x+z)},
	    {0, 0, ϵ*(y^2-z^2), ϵ*(x+y)*(y+z)},
	    {0, 0, 0, -(x+y)*(x+z)*(y+z)}});
    elapsedTime A2 = gaugeTransform(changeEps, A1);
    elapsedTime assert isIntegrable A2
    netList A2
  Text
    Finally, we verify that only the last system is in the $\epsilon$-factorized form using @TO isEpsilonFactorized@.
  Example
    assert isEpsilonFactorized(A2, ϵ)
    assert not isEpsilonFactorized(A1, ϵ)
    assert not isEpsilonFactorized(A, ϵ)
References
  [@HREF("https://arxiv.org/pdf/2410.14757","FPSW")@]  C. Fevola, G. L. Pimentel, A.-L. Sattelberger, and T. Westerdijk. Algebraic Approaches to Cosmological Integrals. Preprint arXiv:2410.14757. To appear in {\em Le Matematiche}.

  [@HREF("https://arxiv.org/pdf/2312.05303", "ABHJLP")@]   N. Arkani-Hamed, D. Baumann, A. Hillman, A. Joyce, H. Lee, and G. L. Pimentel. Differential Equations for Cosmological Correlators. Preprint arXiv:2312.05303.
SeeAlso
  "Massless one-loop triangle Feynman diagram"
  "Gauss' hypergeometric function"
///

doc///
Key
    "Massless one-loop triangle Feynman diagram"
Description
  Text
    Consider the annihilating $D$-ideal of a massless one-loop triangle Feynman integral, appearing in Equation (5.14) in [HPSZ]. In what follows, we write this system of PDEs in connection form.
  Example
    D = makeWeylAlgebra(QQ[x,y],{1,1});
    Q1 = x^2*dx^2+2*x*y*dx*dy+(y-1)*y*dy^2+3*x*dx+(3*y-1)*dy+1;
    Q2 = x*dx^2-y*dy^2+dx-dy;
    I = ideal(Q1,Q2);
  Text
    We can check that the ideal has holonomic rank $4$ and compute the standard monomials of the Gröbner basis of $RI$.
  Example
    assert( 4 == holonomicRank I )
    standardMonomials I
  Text
    Finally, we can compute the connection matrices.
  Example
    elapsedTime A = connectionMatrices I;
    elapsedTime assert isIntegrable A
    netList A
References
  [@HREF("https://link.springer.com/article/10.1007/s11005-024-01835-7","HPSZ")@] J. Henn, E. Pratt, A.-L. Sattelberger, and S. Zoia. $D$-Module Techniques for Solving Differential Equations behind Feynman Integrals. {\em Letters in Mathematical Physics}, 114(28), 2024.
SeeAlso
  "Cosmological correlator for the 2-site chain"
  "Gauss' hypergeometric function"
///

doc ///
Key
    "Gauss' hypergeometric function"
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
  -- Example
  --   netList(Boxes => false, VerticalSpace => 1,
  -- 	apply(4, i -> i+1 => sub(A#i, {a => 1/2, b => 1/2, c => 1})))
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
  "Cosmological correlator for the 2-site chain"
  "Massless one-loop triangle Feynman diagram"
///
