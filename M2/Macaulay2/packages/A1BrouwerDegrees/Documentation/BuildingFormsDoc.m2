doc ///
	Key
		makeDiagonalForm
		(makeDiagonalForm, Ring, RingElement)
		(makeDiagonalForm, Ring, Number)
		(makeDiagonalForm, Ring, Sequence)
		(makeDiagonalForm, InexactFieldFamily, RingElement)
		(makeDiagonalForm, InexactFieldFamily, Number)
		(makeDiagonalForm, InexactFieldFamily, Sequence)
	Headline
		the Grothendieck-Witt class of a diagonal form
	Usage
		makeDiagonalForm(k, a)
		makeDiagonalForm(k, L)
	Inputs
		k: Ring
			a field or finite étale algebra over a field
		a: RingElement
			any element in the field or finite étale algebra over a field
		L: Sequence
			of elements in the field or finite étale algebra $a_{i}$ where $i = 1,\dots, n$
	Outputs
		: GrothendieckWittClass
			the diagonal form $\langle a_{1},\ldots,a_{n}\rangle$ in the Grothendieck-Witt ring of the field or finite étale algebra
	Description
		Text
			Given a sequence of elements $a_{1},\ldots,a_{n}$, we can form the diagonal form $\langle a_{1},\ldots,a_{n}\rangle$ defined to be the block sum of each of the rank one forms $\langle a_{i} \rangle \colon k \times k \to k,$ $(x,y) \mapsto a_{i} xy$.
		Example
			makeDiagonalForm(QQ, (3,5,7))
		Text
			Inputting a ring element, an integer, or a rational number instead of a sequence will produce a rank one form instead. For instance:
		Example
			makeDiagonalForm(GF(29), 5/13)
			makeDiagonalForm(RR, 2)
	SeeAlso
		getDiagonalClass
		diagonalizeViaCongruence
		getDiagonalEntries
///

doc ///
	Key
		makePfisterForm
		(makePfisterForm, Ring, RingElement)
		(makePfisterForm, Ring, Number)
		(makePfisterForm, Ring, Sequence)
		(makePfisterForm, InexactFieldFamily, RingElement)
		(makePfisterForm, InexactFieldFamily, Number)
		(makePfisterForm, InexactFieldFamily, Sequence)
	Headline
		the Grothendieck-Witt class of a Pfister form
	Usage
		makePfisterForm(k, a)
		makePfisterForm(k, L)
	Inputs
		k: Ring
			a field of characteristic not 2
		a: RingElement
			any element in the field
		L: Sequence
			of elements in the field $a_{i}$ where $i = 1,\dots, n$
	Outputs
		: GrothendieckWittClass
			the Pfister form $\langle\langle a_{1},\ldots,a_{n}\rangle\rangle$ in the Grothendieck-Witt ring of the field
	Description
		Text
			Given a sequence of elements $a_{1},\ldots,a_{n} \in k$, we can form the Pfister form $\langle\langle a_{1},\ldots,a_{n}\rangle\rangle$ defined to be the rank $2^{n}$ form defined as the product $\langle 1, -a_{1}\rangle \otimes \cdots \otimes \langle 1, -a_{n}\rangle$.
		Example
			makePfisterForm(QQ, (2,6))
		Text
			Inputting a ring element, an integer, or a rational number instead of a sequence will
			produce a one-fold Pfister form instead. For instance:
		Example
			makePfisterForm(GF(13), -2/3)
			makePfisterForm(CC, 3)
///

doc ///
	Key
		makeHyperbolicForm
		(makeHyperbolicForm, Ring)
		(makeHyperbolicForm, Ring, ZZ)
		(makeHyperbolicForm, InexactFieldFamily)
		(makeHyperbolicForm, InexactFieldFamily, ZZ)
	Headline
		the Grothendieck-Witt class of a hyperbolic form
	Usage
		makeHyperbolicForm(k)
		makeHyperbolicForm(k, n)
	Inputs
		k: Ring
			a field or finite étale algebra over a field
		n: ZZ
			an even number, giving an optional rank $n$ for a totally hyperbolic form
	Outputs
		: GrothendieckWittClass
			the hyperbolic form $\mathbb{H} = \langle 1, -1\rangle$ in the Grothendieck-Witt ring or the totally hyperbolic form $\left(\frac{n}{2}\right)\mathbb{H}$ if an optional rank $n$ is specified
	Description
		Text
			By default outputs the rank two hyperbolic form over the input algebra. 
		Example
			makeHyperbolicForm(GF(7))
		Text
			Specifying a rank yields a copy of sums of the rank two hyperbolic form. Only even rank inputs are accepted.
		Example
			makeHyperbolicForm(RR, 4)
	SeeAlso
		isAnisotropic
		getSumDecomposition
		getSumDecompositionString
///

doc ///
	Key
		makeDiagonalUnstableForm
		(makeDiagonalUnstableForm, Ring, RingElement)
		(makeDiagonalUnstableForm, Ring, Number)
		(makeDiagonalUnstableForm, Ring, Sequence)
		(makeDiagonalUnstableForm, InexactFieldFamily, RingElement)
		(makeDiagonalUnstableForm, InexactFieldFamily, Number)
		(makeDiagonalUnstableForm, InexactFieldFamily, Sequence)
	Headline
		the unstable Grothendieck-Witt class of a diagonal matrix
	Usage
		makeDiagonalUnstableForm(k, a)
		makeDiagonalUnstableForm(k, L)
	Inputs
		k: Ring
			a field or finite étale algebra over a field
		a: RingElement
			any element in the field or finite étale algebra over a field
		L: Sequence
			of elements in the field or finite étale algebra $a_{i}$ where $i = 1,\dots, n$
	Outputs
		: UnstableGrothendieckWittClass
			the unstable Grothendieck-Witt class represented by the diagonal form $\langle a_{1},\ldots,a_{n}\rangle$ in the unstable Grothendieck-Witt group of the field or finite étale algebra
	Description
		Text
			Given a sequence of elements $a_{1},\ldots,a_{n}$, we can form the diagonal form $\langle a_{1},\ldots,a_{n}\rangle$ defined to be the block sum of each of the rank one forms $\langle a_{i} \rangle \colon k \times k \to k,$ $(x,y) \mapsto a_{i} xy$.
		Example
			makeDiagonalUnstableForm(QQ, (3,5,7))
		Text
			Inputting a ring element, an integer, or a rational number instead of a sequence will produce a rank one form instead. For instance:
		Example
			makeDiagonalUnstableForm(GF(29), 5/13)
			makeDiagonalUnstableForm(RR, 2)
	SeeAlso
		getDiagonalClass
		diagonalizeViaCongruence
///

doc ///
	Key
		makeHyperbolicUnstableForm
		(makeHyperbolicUnstableForm, Ring)
		(makeHyperbolicUnstableForm, Ring, ZZ)
		(makeHyperbolicUnstableForm, InexactFieldFamily)
		(makeHyperbolicUnstableForm, InexactFieldFamily, ZZ)
	Headline
		the unstable Grothendieck-Witt class of a hyperbolic form
	Usage
		makeHyperbolicUnstableForm(k)
		makeHyperbolicUnstableForm(k, n)
	Inputs
		k: Ring
			a field or finite étale algebra over a field
		n: ZZ
			an even number, giving an optional rank $n$ for a totally hyperbolic form
	Outputs
		: UnstableGrothendieckWittClass
			the unstable Grothendieck-Witt class represented by the hyperbolic form $\mathbb{H} = \langle 1, -1\rangle$ in the unstable Grothendieck-Witt group or the totally hyperbolic form $\left(\frac{n}{2}\right)\mathbb{H}$ if an optional rank $n$ is specified
	Description
		Text
			By default outputs the rank two hyperbolic form over the input algebra.
		Example
			makeHyperbolicUnstableForm(GF(7))
		Text
			Specifying a rank yields a copy of sums of the rank two hyperbolic form. Only even rank inputs are accepted.
		Example
			makeHyperbolicUnstableForm(RR, 4)
	SeeAlso
		getSumDecomposition
		getSumDecompositionString
///