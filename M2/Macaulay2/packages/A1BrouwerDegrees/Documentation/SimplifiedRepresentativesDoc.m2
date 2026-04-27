doc ///
	Key
		getDiagonalClass
		(getDiagonalClass, GrothendieckWittClass)
		(getDiagonalClass, UnstableGrothendieckWittClass)
	Headline
		produces a diagonalized form for any (unstable) Grothendieck-Witt class, with simplified terms on the diagonal
	Usage
		getDiagonalClass beta
	Inputs
		beta: GrothendieckWittClass
			over a field or finite étale algebra over a field
		beta: UnstableGrothendieckWittClass
			over a field or finite étale algebra over a field
	Outputs
		: GrothendieckWittClass
			a form isomorphic to $\beta$ with a diagonal Gram matrix
		: UnstableGrothendieckWittClass
			a class isomorphic to $\beta$ with a diagonal Gram matrix
	Description
		Text
			Given a symmetric bilinear form, this method uses the @TO2(diagonalizeViaCongruence, "diagonalizeViaCongruence")@ command in order to produce a diagonal symmetric bilinear form isomorphic to $\beta$, with reduced square classes appearing as the diagonal entries when $\beta$ is defined over $\mathbb{C}$, $\mathbb{R}$, $\mathbb{Q}$, or finite field of characteristic not 2. In the case of an @TT("UnstableGrothendieckWittClass")@ the function applies @TO2(diagonalizeViaCongruence, "diagonalizeViaCongruence")@ to the stable part.
		Example
			M = matrix(QQ, {{9,1,7,4},{1,10,3,2},{7,3,6,7},{4,2,7,5}});
			beta = makeGWuClass M;
			getDiagonalClass beta
			getDiagonalClass makeGWClass M
		Text
			Note that the @TO2(GrothendieckWittClass, "GrothendieckWittClass")@ and @TO2(UnstableGrothendieckWittClass, "UnstableGrothendieckWittClass")@ type caches diagonal versions of a form once they have been computed. We can recover this quickly in the following way.
		Example
			beta.cache.getDiagonalClass
	SeeAlso
		diagonalizeViaCongruence
		getDiagonalEntries
///

doc ///
	Key
		getDiagonalEntries
		(getDiagonalEntries, GrothendieckWittClass)
	Headline
		extracts a list of diagonal entries for a GrothendieckWittClass
	Usage
		getDiagonalEntries beta
	Inputs
		beta: GrothendieckWittClass
			over a field or finite étale algebra over a field
	Outputs
		L: List
			of elements $a_i$ of the field or finite étale algebra over a field, where $i = 1,\dots,n $, such that $\beta \cong \langle a_1,\ldots,a_n\rangle$
	Description
		Text
			Given a diagonal form, @TT("getDiagonalEntries")@ will extract the elements along the diagonal.
		Example
			beta = makeGWClass matrix(QQ, {{3,0,0},{0,2,0},{0,0,7}});
			getDiagonalEntries beta
		Text
			If the form is not given with a diagonal representative, this method will first diagonalize it.
		Example
			gamma = makeGWClass matrix(RR, {{0,0,1},{0,1,0},{1,0,0}});
			getDiagonalEntries gamma
	SeeAlso
		getDiagonalClass
		diagonalizeViaCongruence
///
