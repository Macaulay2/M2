doc ///
	Key
		UnstableGrothendieckWittClass
		(net, UnstableGrothendieckWittClass)
		(texMath, UnstableGrothendieckWittClass)
	Headline
		a new type intended to capture an element of the unstable Grothendieck-Witt group of a field or finite étale algebras over a field 
	Description
		Text
			An @TT("UnstableGrothendieckWittClass")@ object is a type of @TO2(HashTable, "HashTable")@ intended to capture an element of the unstable Grothendieck-Witt group of a field or finite étale algebras over a field. The unstable Grothendieck-Witt group is defined to be $\text{GW}^{u}(k)=\text{GW}(k)\times_{k^{\times}/(k^{\times})^{2}}k^{\times}/$, where $\text{GW}(k)$ is the Grothendieck-Witt ring considered as an Abelian group under addition and multiplication in the respective factors. Objects of type @TT("UnstableGrothendieckWittClass")@ encode the data of a matrix and nonzero scalar defined over the same field or finite étale algebra, corresponding to the Gram matrix of the $\text{GW}(k)$ factor and the $k^{\times}$ factor, respectively, subject to the condition that the determinant of the Gram matrix agrees the non-zero element of the field or finite étale algebra up to multiplication by a square.

			An @TT("UnstableGrothendieckWittClass")@ object can be built from a symmetric @TO2(matrix, "matrix")@ and a non-zero element of the base field or finite étale algebra using the @TO2(makeGWuClass,"makeGWuClass")@ method.
			
		Example
			M = matrix(QQ, {{0,1},{1,0}})
			alpha = makeGWuClass(M, -4)
			class alpha
		Text

		Example
			beta0 = makeGWClass matrix(QQ, {{0,1},{1,0}})
			beta = makeGWuClass(beta0, -4)
			class beta
		Text
			Alternatively, we can construct a @TT("UnstableGrothendieckWittClass")@ object by applying @TT("makeGWuClass")@ to @TT("M")@ or @TT("beta0")@, which will automatically use the determinant of the matrix as the non-zero element of the base field or finite étale algebra.
		Example
			gamma = makeGWuClass(M)
			class gamma
		Text

		Example
			delta = makeGWuClass(beta0)
			class delta
		Text
			The underlying matrix of an @TT("UnstableGrothendieckWittClass")@ object can be accessed using the @TO2(getMatrix,"getMatrix")@ method which is the Gram matrix of the symmetric bilinear form represented by the @TT("UnstableGrothendieckWittClass")@ object. This matrix is a symmetric matrix over the base algebra of the @TT("GrothendieckWittClass")@ object. The underlying scalar of an @TT("UnstableGrothendieckWittClass")@ object can similarly be accessed using the @TO2(getScalar,"getScalar")@ method. 
		Example
			getMatrix delta
			getScalar delta
		Text
			The algebra over which can be retrieved using the @TO2(getAlgebra,"getAlgebra")@ method. If further the @TT("GrothendieckWittClass")@ object is over a field, then the field can be retrieved using the @TO2(getBaseField,"getBaseField")@ method.
		Example
			getAlgebra delta
			getBaseField delta
		Text
			As in the case of Grothendieck-Witt classes, it is often useful to have a diagonal representative of the $\text{GW}(k)$ factor of an @TT("UnstableGrothendieckWittClass")@. The method @TO2(getDiagonalClass,"getDiagonalClass")@ diagonalizes the underlying matrix of the class over the base algebra of the @TT("UnstableGrothendieckWittClass")@ object. The result is a new @TT("UnstableGrothendieckWittClass")@ with $\text{GW}(k)$ factor a diagonal Gram matrix. The entire class is stored in the cache for quick recovery later on.
		Example
			diagonalClass = getDiagonalClass beta;
			beta.cache.getDiagonalClass
	SeeAlso
		makeGWuClass
		getAlgebra
		getBaseField
		getMatrix
		getScalar
		getDiagonalClass
///

doc ///
	Key
		makeGWuClass
		(makeGWuClass, Matrix)
		(makeGWuClass, GrothendieckWittClass)
		(makeGWuClass, Matrix, Number)
		(makeGWuClass, Matrix, RingElement)
		(makeGWuClass, GrothendieckWittClass, Number)
		(makeGWuClass, GrothendieckWittClass, RingElement)
	Headline
		constructor for unstable Grothendieck-Witt classes
	Usage
		makeGWuClass(M)
		makeGWuClass(beta)
		makeGWuClass(M, a)
		makeGWuClass(beta, a)
	Inputs
		M: Matrix
			a symmetric matrix over a field or finite étale algebra
		beta: GrothendieckWittClass
			a Grothendieck-Witt class over a field or finite étale algebra
		a: Number
			a non-zero element of the base field or finite étale algebra of the matrix or Grothendieck-Witt class
		a: RingElement
			a non-zero element of the base field or finite étale algebra of the matrix or Grothendieck-Witt class
	Outputs
		: UnstableGrothendieckWittClass
			an unstable Grothendieck-Witt class with the Gram matrix of the input matrix or Grothendieck-Witt class and the non-zero element of the base field or finite étale algebra
	Description
		Text
			The unstable Grothendieck-Witt class is represented by the data of a Grothendieck-Witt class and a non-zero element of the base field or finite étale algebra such that the determinant of the Gram matrix of the Grothendieck-Witt class must agree with the square of the non-zero element of the base field or finite étale algebra up to multiplication by a square. 
			
			The method @TT("makeGWuClass")@ can be applied to a symmetric matrix or a Grothendieck-Witt class together with the data of a scalar, and it will automatically use the determinant of the matrix or Grothendieck-Witt class as the non-zero element of the base field or finite étale algebra if no such element is provided.
		Example
			M = matrix(QQ, {{0,1},{1,0}})
			alpha = makeGWuClass(M, -9)
			class alpha
		Example
			beta0 = makeGWClass M
			beta = makeGWuClass(beta0, -9)
			class beta
		Example
			gamma = makeGWuClass M
			class gamma
		Example
			delta = makeGWuClass beta0
			class delta
		Text
			Over the complex numbers, real numbers, rational numbers, or finite fields of characteristic not 2, the constructor @TT("makeGWuClass")@ verifies that the scalar, if provided, agrees with the determinant of the Gram matrix of the Grothendieck-Witt class or matrix up to multiplication by a square. Over arbitrary finite étale algebras over fields, the constructor will permit the construction of an unstable Grothendieck-Witt class with any nonzero scalar and warn the user to verify that the scalar agrees with the determinant of the matrix representative of the stable part up to squares.
		Example
			R = QQ[x]/(x^2 + 1);
			P = matrix(R, {{1,0}, {0,x}});
			makeGWuClass(P, x^3)
	Caveat
		Over an arbitrary finite étale algebra over a field, any nonzero algebra element can be used as the scalar in the construction of an unstable Grothendieck-Witt class. In cases when the user provides a scalar different from the determinant of the Gram matrix of the Grothendieck-Witt class or matrix, the user must manually verify that the scalar agrees with the determinant of the Gram matrix of the Grothendieck-Witt class or matrix up to multiplication by a square. In this case, a warning is printed: @TT("Warning, the function is not able to verify if the determinant of M and a agree up to squares.")@. 
	SeeAlso
		UnstableGrothendieckWittClass
		GrothendieckWittClass
		getMatrix
		getScalar
		getAlgebra
		getBaseField
///

doc ///
	Key
		getAlgebra
		(getAlgebra, GrothendieckWittClass)
		(getAlgebra, UnstableGrothendieckWittClass)
	Headline
		returns the algebra over which a stable or unstable Grothendieck-Witt class is defined
	Usage
		getAlgebra(beta)
	Inputs
		beta: GrothendieckWittClass
			a Grothendieck-Witt class
		beta: UnstableGrothendieckWittClass
			an unstable Grothendieck-Witt class
	Outputs
		: Ring
			the algebra over which the Grothendieck-Witt class is defined
	Description
		Text
			Given a @TT("GrothendieckWittClass")@ or @TT("UnstableGrothendieckWittClass")@ object @TT("beta")@, this method returns the algebra over which the Grothendieck-Witt class is defined. This algebra is either a field or a finite étale algebra over a field.
		Example
			R = QQ[x]/(x^2 + 1);
			beta = makeGWClass matrix(R, {{1,2},{2,x}});
			getAlgebra beta
	SeeAlso
		GrothendieckWittClass
		UnstableGrothendieckWittClass
		getBaseField
		getMatrix
		getScalar
///

doc ///
	Key
		getBaseField
		(getBaseField, GrothendieckWittClass)
		(getBaseField, UnstableGrothendieckWittClass)
	Headline
		returns the base field of a stable or unstable Grothendieck-Witt class
	Usage
		getBaseField(beta)
	Inputs
		beta: GrothendieckWittClass
			a Grothendieck-Witt class
		beta: UnstableGrothendieckWittClass
			an unstable Grothendieck-Witt class
	Outputs
		: Ring
			the base field of the Grothendieck-Witt class
	Description
		Text
			Given a @TT("GrothendieckWittClass")@ or @TT("UnstableGrothendieckWittClass")@ object @TT("beta")@, this method returns the base field of the Grothendieck-Witt class if the Grothendieck-Witt class is defined over a field. 
		Example
			R = QQ[x]/(x^2 + 1);
			beta = makeGWClass matrix(R, {{1,2},{2,x}});
			getBaseField beta
	Caveat
		This method will return the base field of the (unstable) Grothendieck-Witt class if it is defined over a finite separable extension of either the rational numbers or a finite field of characteristic not 2. The method verifies that the zero ideal of the @TO2(getAlgebra,"getAlgebra")@ of the (unstable) Grothendieck-Witt class is prime using @TO2(isPrime,"isPrime")@, which only works for ideals in polynomial rings or quotients of polynomial rings over a prime field. In particular, this method may fail even when the (unstable) Grothendieck-Witt class is indeed defined over a field. In these cases, the user can apply @TO2(toField, "toField")@ to the algebra of the (unstable) Grothendieck-Witt class to obtain the base field.
	SeeAlso
		GrothendieckWittClass
		UnstableGrothendieckWittClass
		getAlgebra
		getMatrix
		getScalar
		toField
///

doc ///
	Key
		getMatrix
		(getMatrix, GrothendieckWittClass)
		(getMatrix, UnstableGrothendieckWittClass)
	Headline
		returns the Gram matrix of a stable or unstable Grothendieck-Witt class
	Usage
		getMatrix(beta)
	Inputs
		beta: GrothendieckWittClass
			a Grothendieck-Witt class
		beta: UnstableGrothendieckWittClass
			an unstable Grothendieck-Witt class
	Outputs
		: Matrix
			the Gram matrix of the Grothendieck-Witt class or unstable Grothendieck-Witt class
	Description
		Text
			Given a @TT("GrothendieckWittClass")@ or @TT("UnstableGrothendieckWittClass")@ object @TT("beta")@, this method returns the Gram matrix of the Grothendieck-Witt class or unstable Grothendieck-Witt class. The Gram matrix is a symmetric matrix over the algebra of the Grothendieck-Witt class or unstable Grothendieck-Witt class.
		Example
			M = matrix(QQ, {{0,1},{1,0}})
			getMatrix makeGWClass M
			getMatrix makeGWuClass(M, -4)
	SeeAlso
		GrothendieckWittClass
		UnstableGrothendieckWittClass
		getAlgebra
		getBaseField
		getScalar
		makeGWClass
		makeGWuClass
///

doc ///
	Key
		getScalar
		(getScalar, UnstableGrothendieckWittClass)
	Headline
		returns the non-zero scalar of an unstable Grothendieck-Witt class
	Usage
		getScalar(beta)
	Inputs
		beta: UnstableGrothendieckWittClass
			an unstable Grothendieck-Witt class
	Outputs
		: RingElement
			the non-zero scalar of the unstable Grothendieck-Witt class
	Description
		Text
			Given an @TT("UnstableGrothendieckWittClass")@ object @TT("beta")@, this method returns the non-zero scalar of the unstable Grothendieck-Witt class. This scalar is an element of the base field or finite étale algebra of the unstable Grothendieck-Witt class.
		Example
			M = matrix(QQ, {{0,1},{1,0}})
			getScalar makeGWuClass(M, -4)
	SeeAlso
		UnstableGrothendieckWittClass
		getAlgebra
		getBaseField
		getMatrix
		makeGWuClass
///

doc ///
    Key
        addGWu
        (addGWu, UnstableGrothendieckWittClass,UnstableGrothendieckWittClass)     
    Headline
        the direct sum for  two unstable Grothendieck-Witt Classes
    Usage
        addGWu(beta, gamma)
    Inputs
        beta : UnstableGrothendieckWittClass
            the isomorphism class of a non-degenerate symmetric bilinear form represented by a matrix @TT("M")@ together with a scalar @TT("s")@
        gamma : UnstableGrothendieckWittClass
            the isomorphism class of a non-degenerate symmetric bilinear form represented by a matrix @TT("N")@ together with a scalar @TT("t")@

    Outputs
        : UnstableGrothendieckWittClass
            the isomorphism class of the direct sum of the bilinear forms represented by the matrices @TT("M")@  and @TT("N")@, and the scalar is the product @TT("st")@
    Description
        Text
            This computes the direct sum of the Grothendieck-Witt classes @TT("beta")@ and @TT("gamma")@.
        Example
            M = matrix(QQ, {{2,1},{1,2}})
            N = matrix(QQ, {{1,2},{2,6}})
            beta = makeGWuClass M
            gamma = makeGWuClass N
            addGWu(beta, gamma)
///

doc ///
	Key
		addGWuDivisorial
		(addGWuDivisorial, List, List)
	Headline
		the divisorial sum of local degrees of a rational function
	Usage
		addGWuDivisorial(L1, L2)
	Inputs
		L1: List
			a list of unstable Grothendieck-Witt classes representing unstable local degrees
		L2: List
			a list of elements of the base field corresponding to the roots at which the unstable local degrees of @TT("L1")@ are computed
	Outputs
		: UnstableGrothendieckWittClass
			the unstable Grothendieck-Witt class representing the divisorial sum of the local degrees with respect to the divisor determined by @TT("L2")@
	Description
		Text
			Let $f/g:\mathbb{P}^{1}_{k}\to\mathbb{P}^{1}_{k}$ be a pointed rational function with zeroes $\{r_{1},\dots,r_{n}\}$ and $\{\beta_{1},\dots,\beta_{n}\}$ the unstable local $\mathbb{A}^{1}$-degrees at the $r_{i}$. The unstable global $\mathbb{A}^{1}$-degree of the rational function is not computed as the @TO2(addGWu, "addGWu")@ of the local unstable degrees, but as the divisorial sum [I+24].

			The following example computes the divisorial sum of the rational function $\frac{x^{2}+x-2}{3x+5}$ over $\mathbb{Q}$ where the lists of unstable Grothendieck-Witt classes are given by $\{(\langle \frac{1}{3}\rangle, \frac{1}{3}), (\langle \frac{8}{3}\rangle, \frac{8}{3})\}$ and $\{-2, 1\}$.
		Example
			M1 = matrix(QQ, {{1/3}})
			alpha = makeGWuClass(M1)
			M2 = matrix(QQ, {{8/3}})
			beta = makeGWuClass(M2)
			addGWuDivisorial({alpha, beta}, {-2, 1})
	References
		[I+24] J. Igieobo, et. al., "Motivic configurations on the line," @TT("arXiv: 2411.15347")@, 2024.
	SeeAlso
		UnstableGrothendieckWittClass
		getGlobalUnstableA1Degree
		getLocalUnstableA1Degree
///

doc ///
	Key
		getGWClass
		(getGWClass, UnstableGrothendieckWittClass)
	Headline
		returns the Grothendieck-Witt class of the stable part of an unstable Grothendieck-Witt class
	Usage
		getGWClass(beta)
	Inputs
		beta: UnstableGrothendieckWittClass
			an unstable Grothendieck-Witt class
	Outputs
		: GrothendieckWittClass
			the Grothendieck-Witt class of the stable part of the unstable Grothendieck-Witt class
	Description
		Text
			Given an @TT("UnstableGrothendieckWittClass")@ object @TT("beta")@, this method returns the Grothendieck-Witt class of the stable part of the unstable Grothendieck-Witt class. 
		Example
			M = matrix(QQ, {{0,1},{1,0}})
			alpha = makeGWuClass(M, -9)
			getGWClass alpha
	SeeAlso
		UnstableGrothendieckWittClass
		makeGWuClass
		getMatrix
///

