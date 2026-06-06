doc ///
	Key
		GrothendieckWittClass
		(net, GrothendieckWittClass)
		(texMath, GrothendieckWittClass)
	Headline
		a new type intended to capture the isomorphism class of an element of the Grothendieck-Witt ring of a field or finite étale algebras over a field 
	Description
		Text
			A @TT("GrothendieckWittClass")@ object is a type of @TO2(HashTable, "HashTable")@ encoding the isomorphism class of a non-degenerate symmetric bilinear form $V\times V\to k$ over a field or finite étale algebra over a field.

			Given any basis $e_1,\ldots,e_n$ for $V$ as a $k$-vector space, we can encode the symmetric bilinear form $\beta$ by how it acts on basis elements. That is, we can produce a matrix $\left(\beta(e_i,e_j)\right)_{i,j}$. This is called a Gram matrix for the symmetric bilinear form. A change of basis produces a congruent Gram matrix, so thus a matrix represents a symmetric bilinear form uniquely up to matrix congruence.

			A @TT("GrothendieckWittClass")@ object can be built from a symmetric @TO2(matrix, "matrix")@ over a field using the @TO2(makeGWClass,"makeGWClass")@ method.
		Example
			R = QQ[x]/(x^2 + 1);
			beta = makeGWClass matrix(R, {{1,2},{2,x}});
			class beta
		Text
			The underlying matrix of a @TT("GrothendieckWittClass")@ object can be accessed using the @TO2(getMatrix,"getMatrix")@ method which is the Gram matrix of the symmetric bilinear form represented by the @TT("GrothendieckWittClass")@ object. This matrix is a symmetric matrix over the base algebra of the @TT("GrothendieckWittClass")@ object which can be retrieved using the @TO2(getAlgebra,"getAlgebra")@ method. If further the @TT("GrothendieckWittClass")@ object is over a field, then the field can be retrieved using the @TO2(getBaseField,"getBaseField")@ method.
		Example
			getMatrix beta
			getAlgebra beta
			getBaseField beta
		Text
			For computational purposes, it is often useful to have a @TT("GrothendieckWittClass")@ diagonalize the Gram-matrix representative of the symmetric bilinear form. This can be done using the @TO2(getDiagonalClass,"getDiagonalClass")@ method. The diagonalization is done over the base algebra of the @TT("GrothendieckWittClass")@ object, and the result is a new @TT("GrothendieckWittClass")@ object with a diagonal Gram matrix which is stored in the cache for quick recovery later on. 
		Example
			diagonalClass = getDiagonalClass beta;
			beta.cache.getDiagonalClass
		Text
			We additionally have the following methods which can be applied to Grothendieck-Witt classes:

			@UL(
			(TO2(getRank,"getRank"),": returns the rank of a form,"),
			(TO2(getSignature,"getSignature"),": returns the signature of a form over the real numbers or rational numbers,"),
			(TO2(getIntegralDiscriminant,"getIntegralDiscriminant"),": returns an integral representative for the discriminant of a form over the rational numbers,"),
			(TO2(getHasseWittInvariant,"getHasseWittInvariant"),": returns the Hasse-Witt invariant for a form over the rational numbers at a particular prime,"),
			(TO2(getAnisotropicDimension,"getAnisotropicDimension"),": returns the anisotropic dimension of a form,"),
			(TO2(getAnisotropicPart,"getAnisotropicPart"),": returns the anisotropic part of a form,"),
			(TO2(getSumDecomposition,"getSumDecomposition"),": returns a simplified diagonal representative of a form,"),
			(TO2(getSumDecompositionString,"getSumDecompositionString"),": returns a string to quickly read a form,"),
			)@

			and Boolean methods for Grothendieck-Witt classes:

			@UL(
			(TO2(isIsotropic,"isIsotropic"),": returns whether the form is isotropic,"),
			(TO2(isAnisotropic,"isAnisotropic"),": returns whether the form is anisotropic."),
			)@
	SeeAlso
		makeGWClass
		getAlgebra
		getBaseField
		getMatrix
		getDiagonalClass
///

doc ///
	Key
		makeGWClass
		(makeGWClass, Matrix)
		(isWellDefinedGW, Matrix)
	Headline
		the Grothendieck-Witt class of a symmetric matrix
	Usage
		makeGWClass M
	Inputs
		M: Matrix
			a non-singular symmetric matrix defined over a field or finite étale algebra of characteristic not 2
	Outputs
		: GrothendieckWittClass
			the isomorphism class of the non-degenerate symmetric bilinear form represented by @TT("M")@
	Description
		Text
			Given a symmetric matrix @TT("M")@, this command outputs an object of type @TT("GrothendieckWittClass")@. This output has the representing matrix @TT("M")@ and the base field of the matrix stored in its @TO2(CacheTable,"CacheTable")@.
		Example
			R = QQ[x]/(x^2 + 1);
			M = matrix(R, {{1,2},{2,x}});
			beta = makeGWClass M
	SeeAlso
		GrothendieckWittClass
		getMatrix
		getBaseField
		getAlgebra
///

doc ///
	Key
		addGW
		(addGW, GrothendieckWittClass, GrothendieckWittClass)
	Headline
		the direct sum of two Grothendieck-Witt classes
	Usage
		addGW(beta1, beta2)
	Inputs
		beta1: GrothendieckWittClass
			a Grothendieck-Witt class
		beta2: GrothendieckWittClass
			a Grothendieck-Witt class
	Outputs
		gamma: GrothendieckWittClass
			a Grothendieck-Witt class representing the direct sum of the two input classes
	Description
		Text
			Given two @TT("GrothendieckWittClass")@ objects @TT("beta1")@ and @TT("beta2")@, this method computes the direct sum
			of the two classes, which is a new @TT("GrothendieckWittClass")@ object representing the isomorphism class of the direct sum of the two symmetric bilinear forms represented by the input classes. The resulting class has a Gram matrix which is the direct sum of the Gram matrices of the two input classes.
		Example
			R = QQ[x]/(x^2 + 1);
			beta1 = makeGWClass matrix(R, {{1,2},{2,x}});
			beta2 = makeGWClass matrix(R, {{3,4},{4,5}});
			addGW(beta1, beta2)
	SeeAlso
		GrothendieckWittClass
		makeGWClass
		multiplyGW
///

doc ///
	Key
		multiplyGW
		(multiplyGW, GrothendieckWittClass, GrothendieckWittClass)
	Headline
		the tensor product of two Grothendieck-Witt classes
	Usage
		multiplyGW(beta1, beta2)
	Inputs
		beta1: GrothendieckWittClass
			a Grothendieck-Witt class
		beta2: GrothendieckWittClass
			a Grothendieck-Witt class
	Outputs
		gamma: GrothendieckWittClass
			a Grothendieck-Witt class representing the tensor product of the two input classes
	Description
		Text
			Given two @TT("GrothendieckWittClass")@ objects @TT("beta1")@ and @TT("beta2")@, this method computes the tensor product
			of the two classes, which is a new @TT("GrothendieckWittClass")@ object representing the isomorphism class of the tensor product of the two symmetric bilinear forms represented by the input classes. The resulting class has a Gram matrix which is the tensor product of the Gram matrices of the two input classes.
		Example
			R = QQ[x]/(x^2 + 1);
			beta1 = makeGWClass matrix(R, {{1,2},{2,x}});
			beta2 = makeGWClass matrix(R, {{3,4},{4,5}});
			multiplyGW(beta1, beta2)
	SeeAlso
		GrothendieckWittClass
		makeGWClass
		addGW
///
