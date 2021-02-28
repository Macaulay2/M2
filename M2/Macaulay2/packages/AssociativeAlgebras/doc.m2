-- warnings still to fix:

undocumented {sequenceToVariableSymbols,
	      isFreeAlgebraOrQuotient,
	      (isFreeAlgebraOrQuotient, Ring),
	      (ambient, FreeAlgebra),
	      (net, FreeAlgebra),
	      (symbol _, FreeAlgebra, ZZ),
	      (degreesRing, FreeAlgebra),
	      (describe, FreeAlgebra),
	      (expression, FreeAlgebra),
	      (isWellDefined, FreeAlgebra),
	      (toString, FreeAlgebra),
	      (toExternalString, FreeAlgebra),
	      (NewFromMethod, FreeAlgebra, List),
	      (coefficientRing, FreeAlgebra)}

-- TODO: Update this default page 
doc ///
    Key
        AssociativeAlgebras
    Headline
        Noncommutative algebra computations
    Description
        Text
            This code is in active development.  Currently 2-sided
            Groebner bases (up to some degree) are implemented, and
            most features of @TO "NCAlgebra"@ are available.  The package 
            {\tt NCAlgebra} uses curly braces to define non-commutative rings
            whereas this package uses angle bar lists.
        Text
            This package implements natively (i.e., in the Macaulay2
            engine) non-commutative rings (associative algebras),
            their (2-sided) Groebner bases, bases, and a number of
            other features.
        Example
            R = ZZ/32003<|a,b,c|>
            I = ideal(2*a*b + 3*b*a + 5*c^2,
                2*b*c + 3*c*b + 5*a^2,
                2*c*a + 3*a*c + 5*b^2)
            gbI = NCGB(I, 6);
            netList (ideal gbI)_*
            A = R/I -- only uses the Groebner basis already constructed, so only valid in degrees <= 6
            ncBasis(3, A)
    Caveat
        Not yet fully functional.  This package is a current work in
        progress.  The interface will change (even basic things like
        Groebner bases and the basis command still need to be hooked
        into the Macaulay2 groebnerBasis, basis commands), and more
        functionality is expected to be added.
    SeeAlso
        "Defining a noncommutative ring"
        "Basic operations on noncommutative algebras"
        NCGB
        ncBasis
///

doc ///
   Key
      "Defining a noncommutative ring"
   Description
      Text
         A noncommutative ring is a @ TO Ring @ of subclass @ TO FreeAlgebra @ or @ TO FreeAlgebraQuotient @.
      Text
         In addition to defining a ring as a quotient of a @ TO FreeAlgebra @, some common ways to create
	 noncommutative rings include @ TO skewPolynomialRing @ and @ TO oreExtension @.      
      
         Let's consider a three dimensional Sklyanin algebra.  We first define the free algebra on the
	 variables x,y,z:
      Example
         A = QQ<|x,y,z|>
      Text
         Then input the defining relations, and put them in an ideal:
      Example
	 f = y*z + z*y - x^2
	 g = x*z + z*x - y^2
	 h = z^2 - x*y - y*x
     	 I = ideal{f,g,h}
      Text
         Next, we will define the quotient ring (as well as try a few functions on this new ring).
	 Note that when the quotient ring is defined, Macaulay2 computes the Groebner basis
	 of I (out to a certain degree, should the Groebner basis be infinite).
      Example
	 B=A/I
	 generators B
	 numgens B
	 isCommutative B
	 coefficientRing B
      Text
	 As we can see, $x$ is now an element of the quotient $B$.
      Example
         x
      Text
         If we define a new ring containing x, x is now part of that new ring.  For example,
	 we can use the following command to define the (-1)-skew polynomial ring on the
	 variables x,y,z,w:
      Example
      	 C = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w}) 
         x
      Text
         We can 'go back' to B using the command @ TO (use, Ring) @.
      Example
	 use B
	 x
	 use C
      Text
         We can also create an Ore extension.  First define a @ TO RingMap @ with @ TO map @.
      Example
	 sigma = map(C,C,{y,z,w,x})
      Text
         Then call the command @ TO oreExtension @.
      Example
	 D = oreExtension(C,sigma,a)
	 generators D
	 numgens D
   SeeAlso
      "Basic operations on noncommutative algebras"
///

doc ///
   Key
      normalAutomorphism
      (normalAutomorphism,RingElement)
   Headline
      Computes the automorphism determined by a normal homogeneous element
   Usage
      normalAutomorphism x
   Inputs
      x : RingElement
          a homogeneous normal element
   Outputs
      : RingMap
   Description
      Text
         Let x be a homogeneous element in a noncommutative ring R. If x is normal then x determines
	 a graded ring automorphism f of R by x*a = f(x)*a.  This method returns this 
	 automorphism as a RingMap. 
      Example
         A = QQ<|a,b,c|>
	 I = ideal {a*b+b*a,a*c+c*a,b*c+c*b}
	 B = A/I
	 sigma = map(B,B,{b,c,a})
	 C = oreExtension(B,sigma,w)
      Text
         By construction, w is normal, and the normalizing automorphism is sigma
	 extended to C sending w to itself.  It follows that therefore w^2 is also
	 normal whose automorphism is the square of sigma extended to C in a similar
	 way.  We verify these facts with the following commands:
      Example
	 isNormal w^2        
	 phi = normalAutomorphism w^2
	 matrix phi
	 matrix (sigma * sigma)
   SeeAlso
      normalElements	 
///
--- TODO: isWellDefined sigma was included above, but doesn't work at the moment.

doc ///
   Key
      (isNormal, RingElement)
   Headline
      Determines if an element of a noncommutatie ring is normal
   Usage
      isNormal x
   Inputs
      x : RingElement
   Outputs
      : Boolean
   Description
      Text
         Given an element x in a noncommutative ring R, this method returns
	 true if Rx=xR.
      Example
         A = QQ<|a,b,c|>
	 I = ideal {a*b+b*a,a*c+c*a,b*c+c*b}
	 B = A/I
	 sigma = map(B,B,{b,c,a})
	 C = oreExtension(B,sigma,w)
	 isCentral w
	 isNormal w      
   SeeAlso
      isCentral
      normalElements
///
-- TODO: removed until isWellDefined is fixed
-- isWellDefined sigma

doc ///
   Key
      normalElements
      (normalElements, FreeAlgebraQuotient, ZZ, Symbol)
   Headline
      Finds normal elements
   Usage
      normalElements(A,n,x)
   Inputs
      A : FreeAlgebraQuotient
      n : ZZ
      x : Symbol
   Outputs
      : List
   Description
      Text
         Let b_1,...,b_n be a monomial basis for a noncommutative Ring A in degree d. We assume A
	 is generated by elements a_1,...,a_k of degree 1. A homogeneous element r in A
	 is normal if a_i*r is in the span of the r*a_j for all i.
      Text
         Using the input symbols x and y, we define the "normal variety" to be the 
	 set of common solutions to the equations  
	 x_j*a_i*b_j = y_j1*b_j*a_1+...+y_jk*b_j*a_k 
	 for all i and j. Saturating the ideal at each x_i we extract polynomial equations
	 the x_i must satisfy for the element x_1*b_1+...+x_n*b_n to be normal in A.
      Text
         Before computing the normal variety, this method checks for normal monomials
	 in degree n. These are returned first to reduce the complexity of the problem.
	 Then the method computes the variety and returns its components. The equations
         the method returns are given in terms of the indexed variable x. The indices are
	 basis monomials in degree n.
      Text
         The following example is a 3-dimensional Sklyanin algebra.
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 ncBasis(2,B)
	 normalElements(B,2,r)
      Text
         The normal elements in degree 2 are x^2, y^2 and z^2. The basis
	 calculation shows y^2 and z^2 are normal forms in B. The normalElements
	 method first checks all basis monomials using @ TO isNormal @. In this case
	 it finds y^2 and z^2 are normal and returns this information. However,  
	 x^2 is not a normal form expression. The normal form of x^2 is y*z+z*y. In 
	 the second phase of the calculation, the method returns generators of the
	 ideal describing the normal elements (excluding the normal monomials). We
	 see the coefficients of basis monomials z*x and y*x must be 0 and the 
	 coefficients of y*z and z*y must be equal. These equations identify
	 x^2 = y*z+z*y as a normal element of degree 2.
      Example
         normalElements(B,3,t)
	 g = -y^3-z*y*x+y*z*x+z^3
	 isCentral g
      Text
         In degree 3, there are no normal monomials, so the first part of the return
	 value is the empty list.  The second coordinate of the return value is a single
	 matrix whose entries determine the equations that show that the only normal element
	 of degree 3 (up to scaling) is the central element g.
///

doc ///
   Key
      (normalElements, RingMap, ZZ)
   Headline
      Finds elements normalized by a ring map
   Usage
      normalElements(f,n)
   Inputs
      f : RingMap
      n : ZZ
          a homogeneous degree in which to search for normal elements
   Outputs
      : Matrix
   Description
      Text
         A normal element x in a non-commutative ring R determines an automorphism f of R by
	 a*x=x*f(a). Conversely, given a ring endomorphism, we may ask if any x
	 satisfy the above equation for all a. 
      Text
         Given a ring map f and a degree n, this method returns solutions to 
	 the equations a*x=x*f(a) for all generators a of R.
      Example
         B = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w})
	 sigma = map(B,B,{y,z,w,x})
	 C = oreExtension(B,sigma,a)
	 sigmaC = map(C,C,{y,z,w,x,a})
	 normalElements(sigmaC,1)
         normalElements(sigmaC,2)
         normalElements(sigmaC * sigmaC,2)
         normalElements(sigmaC * sigmaC * sigmaC, 3)
///

doc ///
   Key
      "Basic operations on noncommutative algebras"
   Description
      Text 
         The AssociativeAlgebras package contains a number of methods for studying noncommutative
	 rings - primarily graded rings. The following three extended examples 
	 highlight the capabilites of the package. 
      Text
         Our first example concerns a three-dimensional Sklyanin algebra. This example is
	 a PI-ring. We define the ring as a quotient of the tensor algebra on three
	 generators by the two-sided ideal generated by the three elements listed.
      Example
         A = QQ<|x,y,z|>
	 f = y*z + z*y - x^2
	 g = x*z + z*x - y^2
	 h = z^2 - x*y - y*x
	 B = A/ideal{f,g,h}
      Text
         It is known that this algebra has a unique (up to rescaling) central element 
	 of degree 3. We can verify this claim computationally using @ TO centralElements @
	 and check that the element is regular to a given degree. See @ TO isLeftRegular @.
      Example
         centralElements(B,3)
	 j = z^3+y*z*x-z*y*x-y^3
	 isCentral j
	 apply(5,i->isLeftRegular(j,i+1))
      Text
         In fact, we can see that j is (up to scaling) the only normal element of degree 3.
	 See the discussion above for interpreting the output of @ TO normalElements @.
      Example
         normalElements(B,3,n)
	 ncBasis(3,B)
      Text
         The user can create noncommutative rings in ways other than specifying a
	 presentation. For our second example, consider a skew polynomial ring on four
	 generators, where generators skew-commute (but are not nilpotent). See
	 @ TO skewPolynomialRing @ for more details.
      Example
         C = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w})
      Text
         Let us briefly note that the user can also define a skew polynomial ring 
	 with coefficients in a commutative ring.
      Example
         R = QQ[q]/ideal{q^4+q^3+q^2+q+1}
	 B = skewPolynomialRing(R,q,{x,y,z,w})
	 x*y == q*y*x         
      Text
         Returning to the main example, we can define a graded Ore extension of C 
	 by specifying an automorphism.
	 The function @ TO map @ is used to define a ring map. Note that ring maps 
	 are linear and multiplicative by definition but are not assumed to be  well-defined. 
      Example
         use C
         sigma = map(C,C,{y,z,w,x})
	   -- isWellDefined sigma
      Text
         We form the Ore extension of C by sigma. See @ TO oreExtension @.
      Example         
         D = oreExtension(C,sigma,a)
      Text
         The new generator a is normal and regular in D. Regularity (on the left or right)
	 can be checked one homogeneous degree at a time. See @ TO isLeftRegular @. 
	 Thus a determines a graded automorphism f:D->D via a*r=f(r)*a.
      Example
         isNormal a
	 apply(5,i-> isLeftRegular(a,i+1))
         sigmaD = normalAutomorphism a
      Text
	 Given an automorphism, one can check to see which elements it normalizes
	 in any given degree.
      Example       
         normalElements(sigmaD,1)
	 normalElements(sigmaD,2)
      Text
         One can check for the presence of normal elements more generally. In our
	 example, since a is normal, a^2 will also be normal. It is the only normal
	 monomial of degree 2. A complete description of the normal elements in a
	 given degree is given by @ TO normalElements @. 
      Example
         normalElements(D,2,P)
      Text
         Each component of the "normal variety" is a set of polynomial equations which must
	 be satisfied by the coefficients of the monomial basis for an element expressed
	 in that basis to be normal. In this case, the basis of D in degree 2 is
      Example
         ncBasis(2,D)	 
      Text
         The output of normalElements tells us that in order for a degree 2 element of D
	 to be normal, it must be an expression in powers of the generators, and that the
	 coefficients of these powers must satisfy the six nontrivial equations listed.
      Example
         isNormal (x^2+z^2-y^2-w^2)	 
      Text
         Of course, one has been able to define (graded) commutative algebras in Macaulay2 for
	 a long time.  Specifying graded commutativity is usually done with the
	 @ TO SkewCommutative @ option when creating the ring.  The user can convert such rings (and
	 their quotients) to a @ TO FreeAlgebraQuotient @ by using the command @ TO toFreeAlgebraQuotient @:
      Example
         E' = QQ[x,y,z,w,SkewCommutative=>true]
	 E = toFreeAlgebraQuotient E'
	 f = map(E,C,gens E)
	 use C
	 f x^2       
	 use E
	 x^2 == 0
///

doc ///
   Key
      quadraticClosure
      (quadraticClosure,Ideal)
      (quadraticClosure,FreeAlgebra)
      (quadraticClosure,FreeAlgebraQuotient)
   Headline
      Creates the subideal generated by quadratic elements of a given ideal
   Usage
      quadraticClosure I
   Inputs
      I : Ideal
   Outputs
      : Ideal
        the quadratic closure of I
   Description
      Text
         The quadratic closure of an ideal in a FreeAlgebra is the ideal
	 generated by the generators of I of degree at most 2.  Commonly used with 
	 @ TO homogDual @ in the case where the ideal generators are homogeneous of
	 degree greater than 1.
	 
	 If the input is an FreeAlgebraQuotient, the method is applied to the defining
	 ideal of the quotient ring and the corresponding quotient ring is returned.
	 At the moment, quotients of quotients are not implemented, and the ambient
	 ring of the input FreeAlgebraQuotient is assumed to be a FreeAlgebra.
	 
	 This method is commonly used in conjunction with @ TO homogDual @.
      Example
         A = QQ<|x,y,z|>
	 I = ideal{x*z-z*x, y*z, x*y^2-y^2*x, x^3*y-y*x^3}
	 J = quadraticClosure I
   SeeAlso
      homogDual
///

doc ///
   Key
      homogDual
      (homogDual,Ideal)
      (homogDual,FreeAlgebra)
      (homogDual,FreeAlgebraQuotient)
   Headline
      Computes the dual of a pure homogeneous ideal
   Usage
      homogDual I
   Inputs
      I : Ideal
	  or a @ TO FreeAlgebraQuotient @.
   Outputs
      : Ideal
           or an @ TO FreeAlgebraQuotient @
   Description
      Text
         The homogeneous dual of a pure (i.e. generators all in the
	 same degree) ideal I in a FreeAlgebra A is generated by the
	 orthogonal complement to the generators of I under the
	 natural pairing on the generating subspace of A and its
	 linear dual. Though technically the dual ideal belongs to the
	 tensor algebra on the dual space of generators, this method
	 returns the dual ideal in the same FreeAlgebra
	 
	 If the input is a FreeAlgebraQuotient ring, the method is
	 applied to the defining ideal of the quotient and the
	 corresponding quotient ring is returned.
	 
	 Commonly used in conjunction with @ TO quadraticClosure @.
      Example
         A = QQ<|x,y,z|>
	 I = ideal{x*z-z*x, y*z, x*y^2-y^2*x, x^3*y-y*x^3}
	 J = quadraticClosure I
         J' = homogDual J
   SeeAlso
      quadraticClosure
///

doc ///
   Key
      (symbol /, FreeAlgebra, Ideal)
      FreeAlgebraQuotient
   Headline
      Type of a noncommutative ring
   Description
      Text
         This is the type of a quotient of a tensor algebra by a two-sided ideal.
    
         At this point, one cannot define quotients of quotients.
///

doc ///
   Key
      FreeAlgebra
   Headline
      Type of a free algebra
   Usage
      A = QQ<|x,y|>
   Description
      Text
         This is the type of a free algebra over a commutative
	 ring R (i.e. a tensor algebra over R).
      Example
         A = QQ<|x,y|>
///

doc ///
   Key
      ncBasis
      (ncBasis, InfiniteNumber, InfiniteNumber, Ring)
      (ncBasis, List, InfiniteNumber, Ring)
      (ncBasis, InfiniteNumber, List, Ring)
      (ncBasis, ZZ, Ring)
      (ncBasis, List, Ring)
      (ncBasis, ZZ, ZZ, Ring)
      (ncBasis, InfiniteNumber, ZZ, Ring)
      (ncBasis, ZZ, InfiniteNumber, Ring)
      (ncBasis, Ring)
      (ncBasis, List, List, Ring)
      [ncBasis, Limit]
   Headline
      Returns a basis of an noncommutative ring in specified degrees.
   Usage
      bas = ncBasis(d,e,B)
   Inputs
      d : ZZ
          or @ TO List @
	  or @ TO InfiniteNumber @
      e : ZZ
          or @ TO List @
	  or @ TO InfiniteNumber @
      B : Ring
   Outputs
      bas : Matrix
   Description
      Text
         This command returns a basis (or minimal generating set, if
	 the ground ring is not a field), of a graded noncommutative
         ring.
      Example
         A = QQ<|x,y,z|>
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ideal{p,q,r}
	 B = A/I
	 bas = ncBasis(4,B)
///
--- TODO: Not sure actually what is returned if the ground ring is not a field

doc ///
   Key
      leftMultiplicationMap
      (leftMultiplicationMap,RingElement,ZZ)
      (leftMultiplicationMap,RingElement,ZZ,ZZ)
      (leftMultiplicationMap,RingElement,List,List)
      rightMultiplicationMap
      (rightMultiplicationMap,RingElement,ZZ)
      (rightMultiplicationMap,RingElement,ZZ,ZZ)
      (rightMultiplicationMap,RingElement,List,List)
   Headline
      Computes a matrix for left or right multiplication by a homogeneous element
   Usage
      leftMultiplicationMap(r,n) or leftMultiplicationMap(r,n,m) or leftMultiplicationMap(r,fromBasis,toBasis)
   Inputs
      r : RingElement
      n : ZZ
          the homogeneous degree for the source of the map
      m : ZZ
      	  the homogeneous degree for the target of the map
      fromBasis : List
                a list of monomials of the same homogeneous degree
      toBasis : List
                  a list of monomials of homogeneous degree deg(r) larger than the degree of the toBasis
   Outputs
      : Matrix
   Description
      Text
         These methods return a matrix over the coefficient ring of the noncommutative ring to which r
	 belongs. The matrix represents left or right multiplication by r. Most commonly, 
	 the user will enter the ring element (required to be homogeneous) and a degree n.
	 The result is the matrix of the map A_n -> A_n+d where d is the degree of r.
	 The matrix is computed relative to the monomial basis obtain using 
	 @ TO (ncBasis, ZZ, Ring) @. 
	 
	 Alternatively, the user can enter sets of independent monomials to serve as a
	 basis for the domain and co-domain of the maps. The method left or right 
	 multiplies r by the fromBasis and converts to coordinates via @ TO coefficients @
	 and the toBasis.
	 
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 L = leftMultiplicationMap(x,2)
	 kernel L
	 isRightRegular(x,2)
      Text
         If the element is not regular, you can use these methods to compute the 
	 annihilators in particular degrees.
      Example
	 C = QQ<|x,y|>
	 D = C/ideal{x^2+x*y,y^2}
	 isRightRegular(x,1)
	 L = leftMultiplicationMap(x,1)
	 M=matrix gens kernel L
	 ncBasis(1,D)*M
///

doc ///
   Key
      isLeftRegular
      (isLeftRegular,RingElement,ZZ)
      isRightRegular
      (isRightRegular,RingElement,ZZ)
   Headline
      Determines if a given (homogeneous) element is regular in a given degree
   Usage
      isLeftRegular(x,n) or isRightRegular(x,n)
   Inputs
      x : RingElement
      n : ZZ
          the degree in which regularity is checked.
   Outputs
      : Boolean
   Description
      Text
         Given an element x in an noncommutative ring, isLeftRegular returns true if a*x=0 implies
	 a=0 for all a in the specified homogeneous degree n. Likewise isRightRegular
	 returns true if x*a=0 implies a=0 for all elements a of degree n. The
	 method calls @ TO leftMultiplicationMap @ or @ TO rightMultiplicationMap @ as
	 appropriate and checks the kernel in the specified degree. 
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 g = z^3 + y*z*x - z*y*x - y^3
         isLeftRegular(g,6)
	 
	 C = QQ<|x,y|>
	 D = C/ideal{x^2+x*y,y^2}
	 isLeftRegular(x,1)
	 isRightRegular(x,1)
	 
   SeeAlso
      leftMultiplicationMap
      rightMultiplicationMap         
///

doc ///
   Key
      isCentral
      (isCentral,RingElement)
   Headline
      Determines if an element is central
   Usage
      isCentral x or isCentral(x,ncgb)
   Inputs
      x : RingElement
   Outputs
      : Boolean
   Description
      Text
         This method checks to see if a given noncommutative ring element is central.
      Example
        B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	g = z^3 + y*z*x - z*y*x - y^3
	h = x^2 + y^2 + z^2
        isCentral h
        isCentral g
   SeeAlso
      centralElements
///

doc ///
   Key
      centralElements
      (centralElements, Ring, ZZ)
   Headline
      Finds central elements in a given degree
   Usage
      centralElements(A,n)
   Inputs
      A : Ring
      n : ZZ
          the homogeneous degree in which to compute central elements
   Outputs
      : Matrix
   Description
      Text
         If the given noncommutative ring has central elements of the specified degree, this method
	 returns a basis for the space of central elements in that degree.
      Example
        B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	centralElements(B,2)
        centralElements(B,3)
///

doc ///
   Key
      oreExtension
      (oreExtension,Ring,RingMap,RingMap,RingElement)
      (oreExtension,Ring,RingMap,RingMap,Symbol)
      (oreExtension,Ring,RingMap,RingElement)
      (oreExtension,Ring,RingMap,Symbol)
      [oreExtension, Degree]
   Headline
      Creates an Ore extension of a noncommutative ring
   Usage
      oreExtension(A,sigma,delta,x) or oreExtension(A,sigma,x)
   Inputs
      A : Ring
      sigma : RingMap
      delta : RingMap
      x : RingElement
          or a @ TO Symbol @
   Outputs
      : QuotientRing
   Description
      Text
         This method calls @ TO oreIdeal @ and returns the associated
	 Ore extension as an FreeAlgebraQuotient.
      Example
         B = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w})
	 sigma = map(B,B,{y,z,w,x})
	 C = oreExtension(B,sigma,a)
   SeeAlso
      oreIdeal
///

doc ///
   Key
      oreIdeal
      (oreIdeal,Ring,RingMap,RingMap,RingElement)
      (oreIdeal,Ring,RingMap,RingMap,Symbol)
      (oreIdeal,Ring,RingMap,RingElement)
      (oreIdeal,Ring,RingMap,Symbol)
      [oreIdeal, Degree]
   Headline
      Creates the defining ideal of an Ore extension of a noncommutative ring
   Usage
      oreIdeal(A,sigma,delta,x) or oreIdeal(A,sigma,x)
   Inputs
      A : Ring
      sigma : RingMap
      delta : RingMap
      x : RingElement
          or a @ TO Symbol @
   Outputs
      : Ideal
   Description
      Text
         Given a ring A, an Ore extension of A by x is the quotient of the free
	 extension A<x> by the relations x*a - sigma(a)*x-delta(a) where sigma
	 is an automorphism of A and delta is a sigma-derivation. This method returns
	 the defining ideal (in the appropriate tensor algebra) of an Ore extension
	 of A by x. The current version assumes the sigma-derivation delta is 0, but
	 this is to be fixed soon.
      Example
         B = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w})
	 sigma = map(B,B,{y,z,w,x})
	 C = oreIdeal(B,sigma,a)
   SeeAlso
      oreExtension
///

doc ///
   Key
      threeDimSklyanin
      (threeDimSklyanin,Ring,List)
      (threeDimSklyanin,Ring,List,List)
      [threeDimSklyanin,DegreeLimit]
   Headline
      Defines a three-dimensional Sklyanin with given parameters
   Usage
      threeDimSklyanin(R,params,varList)
   Inputs
      R       : Ring
      params  : List
      varList : List
      DegreeLimit => ZZ
   Outputs
      : Ring
   Description
      Text
         This method constructs a three dimensional Sklyanin algebra with parameters from
	 the params list, and variables from varList
	 (see @ HREF{"http:////arxiv.org//abs//1107.2953","here"} @).
	 If either list is not length three, then an error is thrown.  The generic 
	 such algebra does not have a  finite Groebner basis, so the optional parameter
	 DegreeLimit has been defaulted to 6.  If only one list is provided, it is used
	 for the variable names, and a random choice for each parameter is chosen.
      
         The following example is a PI algebra, and has a finite Groebner basis.
      Example
         B = threeDimSklyanin(ZZ/101,{1,1,-1},{x,y,z})
         NCGB(ideal B,5)
      Text
         This is not generically true, however:
      Example
         C = threeDimSklyanin(ZZ/101,{2,3,5},{a,b,c})
	 NCGB(ideal C,5)
      Text
         In all cases, there is a degree three central regular element (a formula
	 for which is given in the paper referenced above).
      Example
         centralElements(B,3)
	 centralElements(C,3)
      Text
         These algebras also all AS-regular and as such have the same Hilbert
	 series as a commutative polynomial algebra in three variables, as we can see here:
      Example
         apply(8, i -> numgens source ncBasis(i,C))
	 apply(8, i -> binomial(i+2,2))
///

doc ///
   Key
      fourDimSklyanin
      (fourDimSklyanin,Ring,List)
      (fourDimSklyanin,Ring,List,List)
      [fourDimSklyanin,DegreeLimit]
   Headline
      Defines a four-dimensional Sklyanin with given parameters
   Usage
      fourDimSklyanin(R,params,varList)
   Inputs
      R       : Ring
      params  : List
      varList : List
      DegreeLimit => ZZ
   Outputs
      : FreeAlgebraQuotient
   Description
      Text
         This method constructs a four dimensional Sklyanin algebra with parameters from
	 the params list, and variables from varList
	 (see @ HREF{"https://www.math.washington.edu/~smith/Research/Skly-survey.pdf","here"} @).
	 If either list is not the appropriate length, then an error is thrown.  The generic 
	 such algebra has a fairly complicated Groebner basis, so the optional parameter
	 DegreeLimit may be provided to limit the maximum of a generator of a Groebner basis
	 found.  This value has been defaulted to 6.  If only one list is provided, it is used
	 for the variable names, and a random choice for each parameter (satisfying the nondegeneracy
	 condition given below) is chosen.
      
      	 In order to not get a degenerate example, one should ensure that the
	 parameters provided satisfy \alpha + \beta + \gamma + \alpha\beta\gamma = 0.
	 This method does not check this condition, since the degenerate examples are
	 of interest as well.  If no parameters are provided, however a generic choice
	 of \alpha,\beta and \gamma satisfying the equation above are selected.
      Example
         C = fourDimSklyanin(ZZ/32003,{a,b,c,d})
      Text
         In all nondegenerate cases, there is are two central elements of degree two which form
	 a regular sequence on the four dimensional Sklyanin (this was proven by Paul
	 Smith and Toby Stafford in a paper in Compositio).
      Example
         centralElements(C,2)
      Text
         These algebras also all AS-regular and as such have the same Hilbert
	 series as a commutative polynomial algebra in four variables, as we can see here:
      Example
         apply(8, i -> numgens source ncBasis(i,C))
	 apply(8, i -> binomial(i+3,3))
///

-- change name...
doc ///
   Key
      toCommRing
      (toCommRing,FreeAlgebra)
      (toCommRing,FreeAlgebraQuotient)
      [toCommRing,SkewCommutative]
   Headline
     Compute the abelianization of a Ring and returns a Ring.
   Usage
     S = toCommRing R 
   Inputs
      R : FreeAlgebraQuotient
          or @ TO FreeAlgebra @
      SkewCommutative => Boolean
   Outputs
     S : Ring
   Description
      Text
         This method takes a noncommutative ring and returns the quotient of a commutative polynomial
	 ring (or an exterior algebra, if SkewCommutative=>true) on the same generators 
	 by the defining relations of the input ring. 
      Example
         A = skewPolynomialRing(QQ,(-1)_QQ,{w,x,y,z})
	 x*y-y*x
	 w^2
         B = toCommRing(A)
	 x*y
	 w^2
	 C = toCommRing(A,SkewCommutative=>true)
	 x*y-y*x
	 w^2
   SeeAlso
      toFreeAlgebraQuotient
///

doc ///
   Key
      toFreeAlgebraQuotient
      (toFreeAlgebraQuotient,Ring)
   Headline
      Converts a Ring to a noncommutative ring
   Usage
     S = toFreeAlgebraQuotient R
   Inputs
     R : Ring
   Outputs
     S : FreeAlgebraQuotient
   Description
      Text
         This function converts commutative rings and quotients of 
	 exterior algebras (i.e. quotients of @ TO PolynomialRing @) to a ring of
	 type FreeAlgebraQuotient.  
      Example
         R = QQ[a,b,c,d, SkewCommutative=>{2,3}]
	 I = ideal(a*d-b*c)
         S = R/I
	 S' = toFreeAlgebraQuotient(S)
	 ideal S'
   SeeAlso
      toCommRing
///

doc ///
   Key
      skewPolynomialRing
      (skewPolynomialRing,Ring,Matrix,List)
   Headline
      Defines a skew polynomial ring via a skewing matrix
   Usage
      B = skewPolynomialRing(R,M,L)
   Inputs
      R : Ring
      M : Matrix
      L : List
   Outputs
      B : FreeAlgebraQuotient
   Description
      Text
         This method constructs a skew polynomial ring with
	 coefficients in the ring R and generators from the list L. A
	 valid input matrix is a square matrix over R with at least #L
	 rows such that M_{ij} = M_{ji}^{(-1)} and M_{ii}=1. The
	 relations of the resulting ring have the form g_i*g_j -
	 M_{ij}*g_j*g_i.
      Example
         R = QQ[q]/ideal{q^4+q^3+q^2+q+1}
	 M = matrix{{1,q,q},{q^4,1,1},{q^4,1,1}}
         B = skewPolynomialRing(R,M,{x,y,z})
         x*y == q^4*y*x
	 N = matrix{{1,1,1,1},{1,1,1,1},{1,1,1,1},{1,1,1,1}}
	 C = skewPolynomialRing(QQ,promote(N,QQ), {a,b,c,d})
         isCommutative C
         isCommutative B
         Bop = oppositeRing B
         y*x == q^4*x*y
   SeeAlso
     oppositeRing
///

doc ///
   Key
      (skewPolynomialRing,Ring,RingElement,List)
      (skewPolynomialRing,Ring,QQ,List)
      (skewPolynomialRing,Ring,ZZ,List)
   Headline
      Defines a skew polynomial ring via a scaling factor
   Usage
      skewPolynomialRing(R,f,L)
   Inputs
      R : Ring
      f : RingElement
          or an integer or a rational number
      L : List
   Outputs
      : FreeAlgebraQuotient
   Description
      Text
         This method constructs a skew polynomial ring with coefficient ring R
	 and generators elements of L. The relations all have the form a*b - f*b*a
	 where a and b are in L. If R is a Bergman coefficient ring, an NCGroebnerBasis
	 is computed for B.      
      Example
         R = QQ[q]/ideal{q^4+q^3+q^2+q+1}
         A = skewPolynomialRing(R,promote(2,R),{x,y,z,w})
         x*y == 2*y*x
         B = skewPolynomialRing(R,q,{x,y,z,w})
         x*y == q*y*x
         Bop = oppositeRing B
         y*x == q*x*y

         C = skewPolynomialRing(QQ,2_QQ, {x,y,z,w})         
         x*y == 2*y*x
	 D = skewPolynomialRing(QQ,1_QQ, {x,y,z,w})
         isCommutative C
         isCommutative D
         Cop = oppositeRing C
         y*x == 2*x*y
   SeeAlso
       oppositeRing
       skewPolynomialRing      
///

doc ///
   Key
      oppositeRing
      (oppositeRing,FreeAlgebra)
      (oppositeRing,FreeAlgebraQuotient)
   Headline
      Creates the opposite ring of a noncommutative ring
   Usage                    
      Aop = oppositeRing A  
   Inputs
      A : FreeAlgebraQuotient
          or @ TO FreeAlgebra @
   Outputs        
      Aop : FreeAlgebraQuotient
            or @ TO FreeAlgebra @
   Description
      Text 
         Given a noncommutative ring A, this creates a noncommutative
	 ring whose defining ideal is generated by the "opposites" -
	 elements whose noncommutative monomial terms have been
	 reversed - of the generators of the defining ideal of A.
      Example
          R = QQ[q]/ideal{q^4+q^3+q^2+q+1}
          A = skewPolynomialRing(R,q,{x,y,z,w}) 
	  x*y == q*y*x
          Aop = oppositeRing A
	  y*x == q*x*y 
   SeeAlso
      skewPolynomialRing		
///

doc ///
   Key
      ncHilbertSeries
      (ncHilbertSeries, FreeAlgebraQuotient)
      (ncHilbertSeries, FreeAlgebra)
      [ncHilbertSeries, Order]
   Headline
      Computes the Hilbert series of a noncommutative ring
   Usage
     hseries = ncHilbertSeries(A)
   Inputs
     A : FreeAlgebraQuotient
         or @ TO FreeAlgebra @
   Outputs
       : Expression
         or @ TO RingElement @ 
   Description
      Text
         This method computes the Hilbert series of a graded
	 noncommutative ring.  If the ring is defined over a field
	 (and potentially not standard graded), then a basis is
	 computed and the generating function of the degrees of that
	 basis is returned.  The degree to which one computes the
	 Hilbert series is controlled with the Order option.  The
	 output is returned as either an expression (if a rational
	 representation can be found using @ TO toRationalFunction @)
         or as an element of the @ TO degreesRing @ of the input.
      Example
	 A = QQ<|x,y,z|>
	 ncHilbertSeries(A,Order=>10)
	 A = QQ<|x,y,z,Degrees=>{1,2,3}|>
	 ncHilbertSeries(A,Order=>10)
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 ncHilbertSeries(B,Order=>10)
///

doc ///
   Key
      endomorphismRingIdeal
      (endomorphismRingIdeal,Module,Symbol)
   Headline
      Find the relations of an endomorphism ring
   Usage
      I = endomorphismRingIdeal(M,X)
   Inputs
      M : Module
      X : Symbol
          the base name for the indexed variables serving as generators for the output ring
   Outputs
      I : Ideal
      	  in a FreeAlgebra with variables with base name X
   Description
      Text
         This method computes the multiplication table of the endomorphism ring
	 of a module $M$ over a commutative ring $R$, and returns this information
	 as an ideal.  Since Groebner bases do not (yet!) work for a FreeAlgebra with
	 coefficients in a commutative ring, minimizing these generators and relations
	 must be performed by the user.  This will be fixed in a future release.
      Example
         Q = QQ[a,b,c]
         R = Q/ideal{a*b-c^2}
         kRes = res(coker vars R, LengthLimit=>7)
         M = coker kRes.dd_5
         I = endomorphismRingIdeal(M,X)
      Text
         The endomorphisms corresponding to the variables chosen are cached in the
	 ideal returned by this method, and can be accessed using the key "EndomorphismRingIdealGens".
      Example
         maps = I.cache#"EndomorphismRingIdealGens"
	 assert(maps_0*maps_2 == maps_3)
///

doc ///
   Key
      NCReductionTwoSided
      (NCReductionTwoSided, RingElement, List)
      (NCReductionTwoSided, RingElement, Ideal)
      (NCReductionTwoSided, RingElement, Matrix)
      (NCReductionTwoSided, Matrix, List)
      (NCReductionTwoSided, Matrix, Ideal)
      (NCReductionTwoSided, Matrix, Matrix)
   Headline
      Reduces the entries of an Matrix with respect to an ideal
   Usage
      L = NCReductionTwoSided(M,I)
   Inputs
     M : Matrix
     I : Ideal
   Outputs
     L : Matrix
   Description
      Text
         This command reduces the entries of a RingElement or Matrix
	 with respect to an Ideal or a single row matrix.  A Groebner
	 basis is not computed for the input ideal.  If one wishes to
	 use this function for normal forms, one must first call @ TO
	 NCGB @ and pass the result to this function.
      Example
         A = QQ<|x,y,z|>
	 f = y*z + z*y - x^2
	 g = x*z + z*x - y^2
	 h = z^2 - x*y - y*x
	 I = ideal {f,g,h}
	 Igb = NCGB(I,10)
	 NCReductionTwoSided(x^4,I)
	 NCReductionTwoSided(x^4,Igb)
///

doc ///
   Key
     leftQuadraticMatrix
     (leftQuadraticMatrix,List)
     (leftQuadraticMatrix,Ideal)
     rightQuadraticMatrix
     (rightQuadraticMatrix,List)
     (rightQuadraticMatrix,Ideal)
   Headline
     Factors the quadratic ideal on the left or on the right.
   Usage
     M = leftQuadraticMatrix I
   Inputs
     I : Ideal
     	 or @ TO List @
   Outputs
     M : Matrix
   Description
      Text
        This function expresses the generators of the quadratic ideal
	I as a product of a row or column vector of the variables times
	a matrix with linear entries.
      Example
        R = ZZ/32003 <|x_4,x_1,x_2,x_3|>
	I = ideal {x_3^2 - x_1*x_2, x_4^2 - x_2*x_1, x_1*x_3 - x_2*x_4,
                   x_3*x_1 - x_2*x_3, x_1*x_4 - x_4*x_2, x_4*x_1 - x_3*x_2}
	lQ = leftQuadraticMatrix I
	rQ = rightQuadraticMatrix I
	d = matrix {{x_4,x_1,x_2,x_3}}
	e = matrix transpose {{x_4,x_1,x_2,x_3}}
	NCReductionTwoSided(ncMatrixMult(d,rQ),I)
	NCReductionTwoSided(ncMatrixMult(lQ,e),I)
      Text
      	We can perform these products over the quotient to verify that the
	composite is zero there.
      Example
	S = R/I
	(lQS,dS) = (sub(lQ,S),sub(d,S));
	(rQS,eS) = (sub(rQ,S),sub(e,S));
	ncMatrixMult(dS,rQS)
	ncMatrixMult(lQS,eS)
///

doc ///
   Key
     ncMatrixMult
     (ncMatrixMult, Matrix, Matrix)
   Headline
     Correctly multiplies matrices from noncommutative rings.
   Usage
     L = ncMatrixMult(M,N)
   Inputs
     M : Matrix
     N : Matrix
   Outputs
     L : Matrix
   Description
      Text
        This function is provided as a temporary band-aid for
	matrix multiplication over a noncommutative ring,
	as well as a reminder that it must be repaired eventually.
      Example
      	A = QQ<|x,y|>
	M = matrix {{x}}
	N = matrix {{y}}
	M*N
	assert(ncMatrixMult(M,N) == matrix {{x*y}})
///

doc ///
   Key
     freeAlgebra
     (freeAlgebra,Ring,BasicList)
     UseVariables
   Headline
     Create a FreeAlgebra
   Usage
     A = freeAlgebra(R,xs)
   Inputs
     R : Ring
     xs : BasicList
         containing the variables, and any options
   Outputs
     A : FreeAlgebra
   Description
      Text
        This function creates a free algebra over $R$ with variables
	from the BasicList xs.  Options are also passed as part of the BasicList.
	The variables are not in scope after a call to this function by default.
	If you wish to have them in scope, one may @ TO use @ the return value,
	or pass the option true to UseVariables.
      Example
        A = freeAlgebra(QQ,{x,y,z})
	--assert(class x == Symbol)  Somehow the x is leaking out still...
	use A
	assert(x == A_0)
      Text
	Other options are @ TO Degrees @, @ TO DegreeRank @, @ TO
	Weights @, and @ TO Heft @ which use the same syntax and play
	the same role as in the case of a commutative polynomial ring.
        
	In particular, to create noncommutative elimination orders, one must
	use @ TO Weights @ that are chosen accordingly.  The following
	example is the graph ideal of the ring homomorphism from
	$\mathbb{Q}\langle a,b,c\rangle$ to $\mathbb{Q}\langle x,y\rangle$
	satisfying $a \mapsto xyx$, $b \mapsto yxy$ and $c \mapsto xy$.
      Example
	B = freeAlgebra(QQ,{x,y,a,b,c,Weights=>{1,1,0,0,0},Degrees=>{1,1,3,3,2}})
        I = ideal {a - x*y*x, b - y*x*y, c - x*y}
	Igb = NCGB(I,10)
      Text
      	This general construction is automated in @ TO ncGraphIdeal @ and @ TO ncKernel @.
///

doc ///
   Key
     ncGraphIdeal
     (ncGraphIdeal,RingMap)
   Headline
     Compute the graph ideal of a ring map between noncommutative rings.
   Usage
     I = ncGraphIdeal f
   Inputs
     f : RingMap
   Outputs
     I : Ideal
   Description
      Text
        This function creates the graph ideal of a ring map between noncommutative
	rings.  It creates the free product of the source and target, and forms
	the ideal generated by $v - f(v)$ for all variables $v$ in the source.
      Example
      	A = QQ<|a,b,c|>
	B = QQ<|x,y|>
	f = map(B,A,{x*y*x,y*x*y,x*y})
	I = ncGraphIdeal f
	Igb = NCGB(I,10)
      Text
      	Those generators of the Groebner basis that involve only the variables in
	the domain are a Groebner basis of the kernel of the ring map.
///

doc ///
   Key
     ncKernel
     (ncKernel,RingMap)
     [ncKernel,DegreeLimit]
     [ncKernel,Strategy]
   Headline
     Compute the graph ideal of a ring map between noncommutative rings.
   Usage
     I = ncKernel f
   Inputs
     f : RingMap
   Outputs
     I : Ideal
   Description
      Text      
        This function computes (a Groebner basis of) the kernel of a
	ring map between noncommutative rings.  
      Example
      	A = QQ<|a,b,c|>
	B = QQ<|x,y|>
	f = map(B,A,{x*y*x,y*x*y,x*y})
	K = ncKernel f
      Text
        The generators returned by this function are in fact a Groebner basis
	of the kernel, so it may not be a minimal generating set.
      Text
      	The @ TO DegreeLimit @ and @ TO Strategy @ options are forwarded on
	to the call to the Groebner basis routine @ TO NCGB @.
///

doc ///
   Key
     toRationalFunction
     (toRationalFunction, List)
   Headline
     Attempt to find a rational function representation.
   Usage
     output = toRationalFunction coeffs
   Inputs
     coeffs : List
   Outputs
     output : Sequence
   Description
     Text
       This function attempts to find a rational function representation
       of the (ordinary) generating function given by the list of integers
       input in coeffs.  The return value is an ordered triple, given by the numerator
       of the rational function, the denominator of the rational function,
       and an expression representing the quotient of these two items.
       
       If no rational representation could be found, null is returned.
     Example
       toRationalFunction {1,3,6,10,15,21}
       toRationalFunction {1,3,6,10,15,21,28}
       toRationalFunction apply(10, i -> binomial(i+3,3))
     Text
       This method is used in the @ TO ncHilbertSeries @ method, but may also
       be used to find rational expressions of Poincare series:
     Example
       A = QQ[x,y]/ideal{x^2,x*y}
       kRes = res(coker vars A, LengthLimit => 10);
       kBetti = apply(10, i -> numcols kRes.dd_i)
       toRationalFunction kBetti
///

doc ///
   Key
     pointScheme
     (pointScheme,FreeAlgebraQuotient,Symbol)
   Headline
     Compute the point scheme of the quadratic algebra B
   Usage
     I = pointScheme B
   Inputs
     B : FreeAlgebraQuotient
   Outputs
     I : Ideal
   Description
    Text
      This method computes the ideal defining the point scheme of the
      (assumed to be Artin-Schelter regular) algebra B.  This amounts
      to computing the ideal of maximal minors of the left quadratic
      matrix corresponding to the generators of the defining ideal of B.
      
      The point scheme parameterizes the point modules over the algebra B.
      A $B$-point module is a module $M$ that is generated in degree zero
      and whose Hilbert function is one for every nonnegative integer.
      In the commutative case, the point scheme of a graded ring $R$ generated
      in degree one is simply $\operatorname{Proj}(R)$, so this object serves
      as a way to introduce geometry in a noncommutative context.

      A straightforward calculation shows that a skew commutative polynomial
      ring in two variables still has point scheme given by $\mathbb{P}^1$,
      for example:
    Example
      S = skewPolynomialRing(QQ,(-1)_QQ,{x_1,x_2})
      P = pointScheme(S,a)
    Text
      In higher variables, one gets smaller point schemes, however.  Indeed, the point scheme
      of the skew polynomial ring in four variables is a two-dimensional reducible scheme
      given by a union of six lines.
    Example
      S = skewPolynomialRing(QQ,(-1)_QQ,{x_1..x_4})
      P = pointScheme(S,a)
      netList minimalPrimes P
    Text
      Three-dimensional Sklyanin algebras were shown by Artin-Tate-Van den Bergh
      to generically have point scheme given by a smooth elliptic curve, a defining
      equation of which we may obtain using this method.
    Example
      S = threeDimSklyanin (frac(QQ[a,b,c]),{a,b,c},{x,y,z}, DegreeLimit => 3)
      P = pointScheme(S,X)
    Text
      The genericity condition $(3abc)^3 \neq (a^3 + b^3 + c^3)^3$ is somewhat visible
      here.  Some non-generic Sklyanin algebras are still AS-regular:
    Example     
      S = threeDimSklyanin (QQ,{1,1,-2},{x,y,z})
      P = pointScheme(S,X)
      netList minimalPrimes P
    Text
      But as you can see, the point scheme is not a smooth elliptic curve.  In fact,
      if we add a cube root of unity to the base ring, we can see that it is a union of
      three lines (although M2 has trouble detecting this outright):
    Example
      R = QQ[zz,X_1,X_2,X_3]
      PP = sub(P,R) + ideal {zz^2 + zz + 1}
      minPP = minimalPrimes PP; netList minPP
      minPP / degree
    Text
      Indeed, this Sklyanin algebra is isomorphic to a skew polynomial algebra
      in three variables if the base field contains a cube root of unity.
///

doc ///
   Key
     NCGB
     (NCGB, Ideal)
     (NCGB, Ideal, ZZ)
     [NCGB,Strategy]
   Headline
     Compute a two-sided Groebner basis of an ideal to a specified degree
   Usage
     Igb = NCGB(I,n)
   Inputs
     I : Ideal
     n : ZZ
   Outputs
     Igb : Matrix
   Description
     Text
       This method performs a two-sided Groebner basis calculation of the ideal
       $I$ to the degree $n$ given.  Possible strategies are "Naive", "F4" and "F4Parallel".
       If no integer is given, the Groebner basis is computed to twice the maximal degree
       of a generator.  As usual, one must take care not to provide too high of
       a degree here, as Groebner bases may be infinite in the noncommutative case.
       
       The current state of the algorithm requires the FreeAlgebra to be defined over
       a field, and the "F4" or "F4Parallel" strategies require the base ring to be
       either QQ, ZZ/p or GF(q).
     Example
       A = QQ<|x,y,z|>
       I = ideal { x*y + y*x - 2*z^2,
	           y*z + z*y - 2*x^2,
		   z*x + x*z - 2*y^2}
       Igb = NCGB(I,10)
///

doc ///
   Key
     lineSchemeFourDim
     (lineSchemeFourDim,FreeAlgebraQuotient,Symbol)
   Headline
     Compute the line scheme of a four-dimensional AS regular algebra
   Usage
     L = lineSchemeFourDim B
   Inputs
     B : FreeAlgebraQuotient
   Outputs
     L : Ideal
   Description
     Text
       This method computes the scheme that parameterizes the set of
       line modules over an AS-regular algebra B due to Shelton and Vancliff.
       More precisely, it computes the image of this scheme under the Plucker
       embedding.
     
       As a first example, we see that the line scheme of the commutative
       polynomial ring is just the image of the Grassmannian Gr(4,2) in $\mathbb{P}^5$:
     Example
       S = skewPolynomialRing(QQ,1_QQ,{x_1,x_2,x_3,x_4})
       L = lineSchemeFourDim(S,M);
       netList minimalPrimes L
     Text
       Next, we compute the line scheme of a (-1)-skew polynomial ring.
       We see that it is a union of four planes and three quadric surfaces.
     Example
       S = skewPolynomialRing(QQ,(-1)_QQ,{x_1,x_2,x_3,x_4})
       L = lineSchemeFourDim(S,M);
       netList minimalPrimes L
     Text
       Finally, we consider the following AS-regular algebra of dimension four.
       Its line scheme is dimension one and degree 20, and is a union of
       10 conics.
     Example
       R = QQ <|x_4,x_1,x_2,x_3|>
       I = ideal {x_3^2 - x_1*x_2, x_4^2 - x_2*x_1, x_1*x_3 - x_2*x_4, x_3*x_1 - x_2*x_3, x_1*x_4 - x_4*x_2, x_4*x_1 - x_3*x_2}
       Igb = NCGB(I, 10);
       S = R/I
       L = lineSchemeFourDim(S,M);
       netList minimalPrimes L
///

-*

restart
needsPackage "AssociativeAlgebras"

doc ///
   Key

   Headline

   Usage

   Inputs

   Outputs

   Description
      Text

      Example

///

doc ///
  Key
    freeProduct
    (freeProduct, Ring, Ring)
  Headline
    Define the free product of two algebras
  Usage
    C = freeProduct(A,B)
  Inputs
    A : Ring
    B : Ring
  Outputs
    C : FreeAlgebraQuotient
        or @ TO FreeAlgebra @
  Description
    Text
       This function returns the free product of the algebras A and B.
    Example
       A = QQ<|x,y,z|>
       B = skewPolynomialRing(QQ,(-1)_QQ, {a,b,c})
       C = freeProduct(A,B)
///

doc ///
  Key
    qTensorProduct
    (qTensorProduct,Ring,Ring,ZZ)
    (qTensorProduct,Ring,Ring,QQ)
    (qTensorProduct,Ring,Ring,RingElement)
  Headline
    Define the (q-)commuting tensor product
  Usage
    C = qTensorProduct(A,B,q)
  Inputs
    A : Ring
    B : Ring
    q : RingElement
  Outputs
    C : FreeAlgebraQuotient
  Description
    Text
       This function returns the algebra that contains A and
       B as a subalgebra, with the commutation law on the 
       images of A and B given by a*b = q*b*a for all a in A and b in B.
       In the case of A ** B, q = 1.
    Example
       A = QQ<|x,y|>
       B = skewPolynomialRing(QQ,(-1)_QQ, {a,b})
       C = qTensorProduct(A,B,-1_QQ)
       ideal C
       D = A ** B
       ideal D
///
*-
