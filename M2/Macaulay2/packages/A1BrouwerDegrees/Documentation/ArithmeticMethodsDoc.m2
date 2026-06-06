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

