
///
   Consequences
    Item
   Description
    Text
      Code
      Pre
    Example
      CannedExample
   Subnodes
   Caveat
   SeeAlso
///

doc ///
  Key 
      "Canonical Series Tutorial"
  Headline
      Computing series solutions to regular holonomic systems
  Description
    Text
      If D/I is a regular holonomic D-module, the solutions of the system of differential equations
      I can be written as Nilsson series (Puiseux series with logarithms). 
      The constructive version of this result is the canonical series method
      [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST, Sections 2.5, 2.6")@].
      In this tutorial, we illustrate an implementation of this method.
      
    Text
      If the input ideal I is not regular, this method is not guaranteed to produce convergent
      series, or even holonomicRank(I) formal power series solutions of I. There currently exists
      no computational method to verify whether D/I is a regular holonomic D-module. 
      In the case of GKZ systems, regularity has been characterized in terms of the input matrix.
    
    Text 
      Contains the following functions: 
      
    Text 
      Currently, this contains the computation of exponents with respect to a weight vector. Completing the canonical series computation is in the future.
      To compute the exponents for a D-ideal I with respect to w, do as follows. 
      Compute the initial ideal of I with respect to w. 
      Introduce the subring of D consisting of polynomials in 
      $\theta_1 = x_1 \partial_1, ... , \theta_n= x_n \partial_n$. This is a commutative polynomial subring of D, 
      referred to here as thetaRing.
      The indicial ideal of I with respect to w is produced by extending the initial ideal to the ring of 
      differential operators with rational function coefficients, and contract to thetaRing. The exponents of I
      with respect to w are the roots of the indicial ideal, counted with multiplicities. 

    Example
     needsPackage "Dmodules"
     R1 = QQ[z]
     W1 = makeWA R1
     a=1/2
     b=3
     c=5/3
     J = ideal(z*(1-z)*dz^2+(c-(a+b+1)*z)*dz-a*b) -- the Gauss hypergeometric equation, exponents 0, 1-c 
     cssExpts(J,{1})
     inw(J,{-1,1})
     distraction(oo,QQ[s])
     factor oo_0
     c=1  -- Now we have a single exponent of multiplicity 2
     J = ideal(z*(1-z)*dz^2+(c-(a+b+1)*z)*dz-a*b)
     cssExpts(J,{1})
     cssExptsMult(J,{1})
    Text  
      The first step is to rewrite elements of the initial ideal in a terms of the thetaRing, in a way that will allow us to easily
      extend and contract see [SST] 
    Example
      R2 = QQ[x_1..x_3]
      W2 = makeWA R2
      gens W2
      thetaRing = QQ[t_1,t_2,t_3] -- sets variable names t_i = x_i\dx_i
      f1= x_1*dx_1 -- this element already belongs to thetaRing
      genToDistractionGens(f1,thetaRing)  -- checks out
      f2 = x_1^3*dx_1^3 -- this is a descending factorial in the theta variables
      genToDistractionGens(f2,thetaRing)
      factor oo	  -- now it looks like a descending factorial
      f = x_1^2*x_2^2*x_3*dx_1*dx_2^2*dx_3^2
      genToDistractionGens(f,thetaRing)
    Text
      Here is an example that can be continued when more functions are implemented.
      This is worked out as [page 138, ex 3.5.3, SST]. 
    Example
      A = matrix{{1,1,1},{0,1,2}}
      I = gkz(A,{10,8})
      holonomicRank(I)
      cssExpts(I,{1,0,0})
    Text
      In this case, the series corresponding to the exponent 
      (2,8,0) is logarithm-free (actually, this is a hypergeometric polynomial),
      while the series corresponding to (0,12,-2) has logarithms. 
      [SST, page 138] has the polynomial, and four terms of the logarithmic series.
      
    
///


doc ///
   Key
     genToDistractionGens
     (genToDistractionGens, RingElement, Ring)
   Headline
     the image in the thetaRing of a torus-fixed element in a Weyl algebra
   Usage
     genToDistractionGen(f,thetaRing)
   Inputs
     f:RingElement 
       in a Weyl algebra D of the form x^u Dx^v
     thetaRing:Ring 
       that is a stand in for the theta ring inside D
   Outputs
     :List  
       in thetaRing that is the result of applying [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Lemma 2.3.1] to f.
   Description
    Text
      This function rewrites a monomial $x^u \partial^v$ as a product $x^a p(\theta) \partial^b$,
      where $\theta_i = x_i \partial_i$ for $i = 1,\dots, n$.
      This is a step in a procedure for checking that D-ideal is torus-fixed, and is 
      used in the isTorusFixed routine.
      Code
      Pre
    Example
      R = QQ[x_1..x_4]
      W = makeWA R
      describe W
///

doc ///
   Key
     distraction
     (distraction, Ideal, Ring)
   Headline
     the image in the thetaRing of a torus-fixed ideal in a Weyl algebra
   Usage
     distraction(I,thetaRing)
   Inputs
     I:
       Ideal in a WeylAlgebra that is torus-fixed
   Outputs 
     :Ideal 
       that results from intersecting I with the thetaRing of D, as in 
       [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Lemma 2.3.1] to I.
   Description
    Text
      This computes the distraction of a torus fixed D-ideal [SST, Corollary 2.3.5].
      This is necessary to compute indicial ideals [SST, Theorem 2.3.9, Corollary 2.3.5]; 
      the roots of the indicial ideals are the exponents of the starting terms of canonical series
      solutions of regular holonomic D-ideals.
      Code
      Pre
    Example
      R1 = QQ[z]
      W1 = makeWA R1
      a=1/2
      b=3
      c=5/3
      J = ideal(z*(1-z)*dz^2+(c-(a+b+1)*z)*dz-a*b) -- the Gauss hypergeometric equation, exponents 0, 1-c 
      cssExpts(J,{1})
      K = inw(J,{-1,1})
      distraction(K,QQ[s])
///

doc ///
   Key
     indicialIdeal
     (indicialIdeal, Ideal, List)
   Headline
     the image in the thetaRing of an indicial ideal in a Weyl algebra
   Usage
     indicialIdeal(I,w)
   Inputs
     I:
       Ideal in a WeylAlgebra that is torus-fixed
     w:
       List in the numbers of variables, to yield a weight vector (-w,w) in the Weyl algebra
   Outputs 
     :Ideal 
       that results from intersecting with the thetaRing of D with the result of extending 
       the ideal in_(-w,w)(I) with the rational function 
       field in the Weyl algebra variables, as in 
       [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Theorem 2.3.9] to I.
   Description
    Text
      This computes the distraction of a torus fixed D-ideal [SST, Corollary 2.3.5].
      This is necessary to compute indicial ideals [SST, Theorem 2.3.9, Corollary 2.3.5]; 
      the roots of the indicial ideals are the exponents of the starting terms of canonical series
      solutions of regular holonomic D-ideals.
    Example
      R1 = QQ[z]
      W1 = makeWA R1
      a=1/2
      b=3
      c=5/3
      J = ideal(z*(1-z)*dz^2+(c-(a+b+1)*z)*dz-a*b) -- the Gauss hypergeometric equation, exponents 0, 1-c 
      cssExpts(J,{1})
      K = inw(J,{-1,1})
      distraction(K,QQ[s])
      -- FIXME
      -- indicialIdeal(J,{-11}) -- returns the same answer as the previous distraction command
///

doc ///
   Key
     cssExpts
     (cssExpts, Ideal, List)
   Headline
     the exponents of the canonical series solutions of I in the direction of a weight vector
   Usage
     cssExpts(I,w)
   Inputs
     I:
       holonomic ideal in a Weyl algebra D
     w:
       List of (generic) weights for I, of length half the number of variables in D
   Outputs
     :List 
       of exponents of the exponents of the canonical series solutions of I 
       in the direction of (-w,w), as in 
       [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Theorem 2.3.11].
   Description
    Text 
      There are examples in the tutorial that can be moved here.
      Code
      Pre
    Example
      R1 = QQ[z]
      W1 = makeWA R1
      a=1/2
      b=3
      c=5/3
      J = ideal(z*(1-z)*dz^2+(c-(a+b+1)*z)*dz-a*b) -- the Gauss hypergeometric equation, exponents 0, 1-c 
      cssExpts(J,{1})
///

doc ///
   Key
     cssExptsMult
     (cssExptsMult, Ideal, List)
   Headline
     the exponents (and multiplicities) of the canonical series solutions of I in the direction of a weight vector
   Usage
     cssExptsMult(I,w)
   Inputs
     I:
       holonomic ideal in a Weyl algebra D
     w:
       List of (generic) weights for I, of length half the number of variables in D
   Outputs
     :List 
       of exponents of the starting exponents of the canonical series solutions of I 
       in the direction of (-w,w), as in 
       [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Theorem 2.3.11], 
       together with their multiplicities. 
   Description
    Text
      There are examples in the tutorial that can be moved here.
    Example
      R1 = QQ[z]
      W1 = makeWA R1
      a=1/2
      b=3
      c=5/3
      J = ideal(z*(1-z)*dz^2+(c-(a+b+1)*z)*dz-a*b) -- the Gauss hypergeometric equation, exponents 0, 1-c 
      cssExpts(J,{1})
      c=1  -- Now we have a single exponent of multiplicity 2
      J = ideal(z*(1-z)*dz^2+(c-(a+b+1)*z)*dz-a*b)
      cssExpts(J,{1})
      cssExptsMult(J,{1})
///

doc ///
   Key
     isTorusFixed
     (isTorusFixed, Ideal)
   Headline
     checks if an ideal in a Weyl algebra is torus-fixed
   Usage
     isTorusFixed I
   Inputs
     I:Ideal
       in a WeylAlgebra
   Outputs
     :Boolean 
       true if I is torus-fixed, false if not
   Description
    Text
       There is a natural action of the n-dimensional algebraic torus on $D$ 
       where $t \in (\mathbb{C}^*)^n$ acts on $\partial_i$ as $t_i\partial_i$ and on 
       $x_i$ as $t_i^{-1}x_i$. The function isTorusFixed verifies whether a D-ideal is 
       invariant under this action. 
       
       See [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@],
       just before Lemma 2.3.1.
    Example
       W = makeWA(QQ[x_1,x_2])
       b = 2
       I = ideal(x_1*dx_1*(x_1*dx_1+b), x_1*dx_1*(x_2*dx_2+b),
	   x_2*dx_2*(x_1*dx_1+b), x_2*dx_2*(x_2*dx_2+b))
       isTorusFixed I
///


doc ///
   Key
     solveFrobeniusIdeal
     (solveFrobeniusIdeal, Ideal)
   Headline
     solving Frobenius ideals
   Usage
     solveFrobeniusIdeal I
   Inputs
     I:
       a Frobenius ideal which is m-primary
   Outputs
     :List
       a list of polynomials in logarithms of the variable
   Description
    Text
      See [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Algorithm 2.3.14].

      Here is [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Example 2.3.16]:
    Example
      R = QQ[t_1..t_5];
      I = ideal(R_0+R_1+R_2+R_3+R_4, R_0+R_1-R_3, R_1+R_2-R_3, R_0*R_2, R_1*R_3);
      netList solveFrobeniusIdeal I
///
