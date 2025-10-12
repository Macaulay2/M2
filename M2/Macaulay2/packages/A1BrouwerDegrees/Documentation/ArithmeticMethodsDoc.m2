doc ///
	Key
		getPadicValuation
		(getPadicValuation, ZZ, ZZ)
		(getPadicValuation, QQ, ZZ)
	Headline
		p-adic valuation of a rational number
	Usage
		getPadicValuation(a, p)
	Inputs
		a: QQ
			a non-zero rational number in $\mathbb{Q}_p$
		p: ZZ
			a prime number
	Outputs
		: ZZ
			$n$ where $a=up^n$ and $u$ is a unit in $\mathbb{Z}_p$
	Description
		Text
			This function computes the $p$-adic valuation of a rational number $a$, which is the exponent of the prime $p$ in the factorization of $a$ in $\mathbb{Q}_p$. It returns the integer $n$ such that $a=up^{n}$, where $u$ is a unit in $\mathbb{Z}_p$.
		Example
			a = 363/7;
			getPadicValuation(a, 11)
		Text
			We have that $363/7 = \frac{3\cdot 11^{2}}{7}$, so the $11$-adic valuation is $2$.
///

doc ///
	Key
		getLocalAlgebraBasis
		(getLocalAlgebraBasis, List, Ideal)
	Headline
		produces a basis for a local finitely generated algebra over a field or finite étale algebra
	Usage
		getLocalAlgebraBasis(L, p)
	Inputs
		L: List
			of polynomials $f=(f_1, \dots ,f_n)$ over the same ring
		p: Ideal
			a prime ideal of an isolated zero
	Outputs
		: List
			of basis elements of the local algebra $Q_p(f)$
	Description
		Text
			Given an endomorphism of affine space, $f=(f_1,\dots ,f_n)$, given as a list of polynomials called $L$ and the prime ideal of an isolated zero, this command returns a list of basis elements of the local algebra $Q_p(f):=k[x_{1},\dots,x_{n}]_{\mathfrak{m}_{p}}/(f_{1},\dots,f_{n})$ where $\mathfrak{m}_{p}$ is the maximal ideal corresponding to the closed point $p$ by computing a normal basis for $(I:(I:p^{\infty}))$ (see [S02, Proposition 2.5]).
		Example
			QQ[x,y];
			f = {x^2 + 1 - y, y};
			p = ideal(x^2 + 1, y);
			getLocalAlgebraBasis(f, p)
	References
		[S02] B. Sturmfels, "Solving Systems of Polynomial Equations," American Mathematical Society, 2002.
	SeeAlso
		getLocalA1Degree
///

doc ///
	Key
		getSylvesterMatrix
		(getSylvesterMatrix, RingElement, RingElement)
	Headline
		computes the Sylvester matrix of two polynomials in one variable
	Usage
		getSylvesterMatrix(f, g)
	Inputs
		f: RingElement
			a polynomial in one variable over a ring
		g: RingElement
			a polynomial in one variable over a ring
	Outputs
		: RingElement
			the resultant of f and g
	Description
		Text
			The @ITALIC("Sylvester matrix")@ of two polynomials $f$ and $g$ in one variable is a matrix whose rows are formed by the coefficients of the polynomials $f$ and $g$ shifted by their degrees and is used to compute the resultant of the two polynomials [CLO15, Chapter 3.6, Definition 2].

			Given two polynomials $f$ and $g$
			$$f(x)=a_{n}x^{n}+a_{n-1}x^{n-1}+\dots+a_{1}x+a_{0}$$
			$$g(x)=b_{m}x^{m}+b_{m-1}x^{n-1}+\dots+b_{1}x+b_{0}$$
			the Sylvester matrix of $f$ and $g$ is defined to be the $(n+m)\times(n+m)$-matrix 
			$$\begin{bmatrix}
			a_{0} & 0 & \dots & 0 & b_{0} & 0 & \dots & 0 \\
			a_{1} & a_{0} & \dots & 0 & b_{1} & b_{0} & \dots & 0 \\
			\vdots & \vdots & \ddots & \vdots & \vdots & \vdots & \ddots & \vdots \\
			a_{n} & a_{n-1} & \dots & a_{0} & b_{m} & b_{m-1} & \dots & b_{0} \\
			0 & a_{n} & \ddots & \vdots & 0 & b_{m} & \ddots & \vdots \\
			\vdots & \vdots & \ddots & \vdots & \vdots & \vdots & \ddots & \vdots \\
			0 & 0 & \dots & a_{n} & 0 & 0 & \dots & b_{m}
			\end{bmatrix}.$$
		Example
			QQ[x];
			f = x^5 + 3*x^4 - 8*x^3 + 4*x^2 - 2*x + 1;
			g = x^2 + 7*x - 1;
			getSylvesterMatrix(f, g)
	References
		[CLO15] D. Cox, J. Little, and H. O'Shea, "Ideals, Varieties, and Algorithms," Springer, 2015.
	SeeAlso
		getResultant
///

doc ///
	Key
		getResultant
		(getResultant, RingElement, RingElement)
	Headline
		computes the resultant of two polynomials in one variable
	Usage
		getResultant(f, g)
	Inputs
		f: RingElement
			a polynomial in one variable over a ring
		g: RingElement
			a polynomial in one variable over a ring
	Outputs
		: RingElement
			the resultant of f and g
	Description
		Text
			The resultant of two polynomials $f$ and $g$ in one variable is a polynomial expression in the coefficients of $f$ and $g$ that vanishes if and only if $f$ and $g$ have a common root. It can be computed as the determinant of the @TO2(getSylvesterMatrix, "Sylvester matrix")@ of $f$ and $g$ [CLO15, Chapter 3.6, Definition 2].
		Example
			QQ[x];
			f = x^5 + 3*x^4 - 8*x^3 + 4*x^2 - 2*x + 1;
			g = x^2 + 7*x - 1;
			getResultant(f, g)
	References
		[CLO15] D. Cox, J. Little, and H. O'Shea, "Ideals, Varieties, and Algorithms," Springer, 2015.
	SeeAlso
		getSylvesterMatrix
///