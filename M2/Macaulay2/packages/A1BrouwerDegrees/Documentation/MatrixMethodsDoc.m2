doc ///
	Key
		diagonalizeViaCongruence
		(diagonalizeViaCongruence, Matrix)
	Headline
		diagonalizes a symmetric matrix via congruence
	Usage
		diagonalizeViaCongruence M
	Inputs
		M : Matrix
			a symmetric matrix over any field or finite étale algebras over a field
	Outputs
		: Matrix
			a diagonal matrix congruent to @TT("M")@
	Description
		Text
			Given a symmetric matrix @TT("M")@ over any field or finite étale algebra over a field, this command gives a diagonal matrix congruent to @TT("M")@. Note that the order in which the diagonal terms appear is not specified. 
		Example
			R = QQ[x]/(x^2 - 1)
			M = matrix(R, {{1,2},{2,x}});
			diagonalizeViaCongruence M
	SeeAlso
		getDiagonalClass
		getDiagonalEntries
///