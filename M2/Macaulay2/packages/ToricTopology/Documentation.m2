-- -*- coding: utf-8 -*-
doc ///
	Key
		ToricTopology
	Headline
		homological computations in toric topology
	Description
		Text
			ToricTopology computes algebraic invariants of three families of spaces
			built from combinatorial data in toric topology: moment-angle complexes,
			quasitoric manifolds, and small covers.

			A moment-angle complex is determined by a simplicial complex $K$.
			A quasitoric manifold or small cover is determined by $K$ together with a
			characteristic matrix $\lambda$. Here $K$ is the boundary of a simplicial
			polytope, or more generally a triangulated sphere, and every maximal minor of
			$\lambda$ indexed by a facet of $K$ has determinant $\pm 1$. Use
			@TO isValidChar@ to check this condition.

			The package can compute:

			@UL {
				{"cohomology rings"},
				{"equivariant cohomology"},
				{"Betti numbers"},
				{"characteristic classes: Stiefel-Whitney and Chern"}
			}@

			It also has constructors for standard examples such as
			@TO complexProjectiveSpace@, @TO realProjectiveSpace@,
			@TO hessenbergVariety@ and @TO AFPVariety@.

			Additionally, it provides methods for computation of some of these
			invariants for normal toric varieties.
	SeeAlso
		NormalToricVarieties
///

doc ///
	Key
		SmallCover
	Headline
		the class of all small covers
	Description
		Text
			A small cover is represented by a simplicial complex K and a matrix
			which is characteristic for K.
	SeeAlso
		QuasiToricManifold
///

doc ///
	Key
		QuasiToricManifold
	Headline
		the class of all quasitoric manifolds
	Description
		Text
			A quasitoric manifold is represented by a simplicial complex K and
			a matrix which is characteristic for K.
	SeeAlso
		SmallCover
///

doc ///
	Key
		MomentAngleComplex
	Headline
		the class of all moment-angle complexes
	Description
		Text
			Given a simplicial complex $K$ on $m$ vertices, the moment-angle
			complex $\mathcal{Z}_K$ is a cellular complex constructed as a
			union of certain products of disks and circles: $$\mathcal{Z}_K =
			\bigcup_{\sigma \in K} \left( (D^2)^\sigma \times (S^1)^{[m]
			\setminus \sigma} \right).$$ These spaces admit a natural action of
			the torus $T^m = (S^1)^m$. Non-singular toric varieties (not
			necessarily complete) are homotopy equivalent to partial quotients
			of moment-angle complexes by freely acting subtori of $T^m$. Thus,
			moment-angle complexes are an important class of spaces studied in
			toric topology. Their topological properties can be determined from
			the combinatorics of the underlying simplicial complex.  This
			package implements methods to determine some of these properties.
			A moment-angle complex is a special case of a polyhedral product.
	SeeAlso
		--NormalToricVariety
		QuasiToricManifold
		SmallCover
///

doc ///
	Key
		isValidChar
		(isValidChar,SimplicialComplex,Matrix)
	Headline
		whether a matrix is characteristic for a simplicial complex
	Usage
		isValidChar(K,lambda)
	Inputs
		K:SimplicialComplex
		lambda:Matrix
	Outputs
		:Boolean
	Description
		Text
			Checks whether lambda is characteristic for K.
	SeeAlso
///

doc ///
	Key
		smallCover
		(smallCover,SimplicialComplex,Matrix)
	Headline
		create a small cover
	Usage
		smallCover(K,lambda)
	Inputs
		K:SimplicialComplex
		lambda:Matrix
	Outputs
		:SmallCover
	Description
		Text
			Create a small cover over K with characteristic matrix lambda.  If
			lambda is not characteristic for K, then an error is raised.  The
			entries of lambda are automatically converted to ZZ/2 entries, if
			they are not already so.
		Text
			The following example illustrates creating the 2-torus as a small
			cover over the unit square.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[a..d]
			K = simplicialComplex {a*b, b*c, c*d, d*a}
			lambda = matrix{{1, 0, 1, 0}, {0, 1, 0, 1}}
			X = smallCover(K, lambda)
	SeeAlso
///

doc ///
	Key
		quasiToricManifold
		(quasiToricManifold,SimplicialComplex,Matrix)
	Headline
		create a quasitoric manifold
	Usage
		quasiToricManifold(K,lambda)
	Inputs
		K:SimplicialComplex
		lambda:Matrix
	Outputs
		:QuasiToricManifold
	Description
		Text
			Create a quasitoric manifold over K with characteristic matrix
			lambda.  If lambda is not characteristic for K, an error is
			raised.
		Text
			The following example creates the 2-dimensional complex projective
			space as a quasitoric manifold.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[a..c]
			K = simplicialComplex {a*b, b*c, c*a}
			lambda = matrix{{1, 0, -1}, {0, 1, -1}}
			X = quasiToricManifold(K, lambda)
	SeeAlso
///

doc ///
	Key
		momentAngleComplex
		(momentAngleComplex,SimplicialComplex)
	Headline
		create a moment-angle complex
	Usage
		momentAngleComplex(K)
	Inputs
		K:SimplicialComplex
	Outputs
		:MomentAngleComplex
	Description
		Text
			Create a moment-angle complex over the simplicial complex K.
		Text
			This example creates a moment-angle complex over the simplicial
			complex consisting of two disjoint vertices.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[x,y]
			K = simplicialComplex {x, y}
			Z = momentAngleComplex K
	SeeAlso
///

doc ///
	Key
		equivariantCohomology
		(equivariantCohomology,NormalToricVariety)
		(equivariantCohomology,MomentAngleComplex)
	Headline
		compute the equivariant cohomology of a toric space
	Usage
		equivariantCohomology(X)
		equivariantCohomology(Z)
	Inputs
		X:NormalToricVariety
		Z:MomentAngleComplex
	Outputs
		:Module
			the torus-equivariant cohomology of X or Z, as a module over a polynomial
			ring
	Description
		Text
			Compute the equivariant cohomology of certain classes of spaces
			with torus actions: normal toric varieties and moment-angle
			complexes. The equivariant cohomology is computed as a module over a
			polynomial ring in which the coefficients are dependent on the
			context.
			For normal toric varieties, the underlying ring is the polynomial
			ring $\mathbb{Q}[t_1, \ldots, t_r]$ where $r$ is the dimension of the
			toric variety.  For moment-angle complexes, the underlying ring is
			the polynomial ring $k[x_1, \ldots, x_m]$ where $k$ is the coefficient
			ring of the polynomial ring over which the underlying simplicial
			complex was created.
		Text
			The equivariant cohomology of a moment-angle complex is free over
			the polynomial ring when the simplicial complex is a full simplex.
		Example
			needsPackage "SimplicialComplexes";
			R = QQ[x,y,z];
			K = simplicialComplex {x*y*z};
			Z = momentAngleComplex K
			M = equivariantCohomology Z
			isFreeModule M
		Text
			If there is any missing simplex, then the equivariant cohomology is
			not free.
		Example
			needsPackage "SimplicialComplexes";
			R = QQ[x,y,z];
			K = simplicialComplex {x*y, y*z, x*z};
			Z = momentAngleComplex K
			M = equivariantCohomology Z
			isFreeModule M
		Text
			We can also compute the equivariant cohomology of a normal toric
			variety.  In the example below, we compute the equivariant
			cohomology of $\mathbb{CP}^2$ with respect to the standard torus
			($T^2$) action, in rational coefficients.
			Recall that $H^*_T(\mathbb{CP}^2)$ is a free module over
			$\mathbb{Q}[t_1, t_2]$.
		Example
			needsPackage "NormalToricVarieties";
			rayList = {{1, 0}, {0, 1}, {-1, -1}};
			coneList = {{0, 1}, {1, 2}, {0, 2}};
			X = normalToricVariety (rayList, coneList)
			M = equivariantCohomology X
			isFreeModule M
		Text
			Another example is $\mathbb{CP}^1 \times \mathbb{CP}^1$
			with two fixed points removed. Its equivariant cohomology
			is not free (see @TO AFPVariety@).
		Example
			needsPackage "NormalToricVarieties";
			rayList = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};
			coneList = {{0, 1}, {2, 3}};
			X = normalToricVariety (rayList, coneList)
			M = equivariantCohomology X
			isFreeModule M
///

doc ///
	Key
		cohomologyRing
		(cohomologyRing,SmallCover)
		(cohomologyRing,QuasiToricManifold)
	Headline
		compute the cohomology ring of a small cover or quasitoric manifold
	Usage
		cohomologyRing(N)
		cohomologyRing(M)
	Inputs
		N:SmallCover
		M:QuasiToricManifold
	Outputs
		:QuotientRing
	Description
		Text
			Compute the cohomology ring of a small cover (over ZZ/2) or
			quasitoric manifold (over ZZ).
	SeeAlso

///

doc ///
	Key
		stiefelWhitney
		(stiefelWhitney,SmallCover)
	Headline
		compute the Stiefel-Whitney classes of a small cover
	Usage
		stiefelWhitney(N)
	Inputs
		N:SmallCover
	Outputs
		:List
	Description
		Text
			Compute the Stiefel-Whitney classes of a small cover.
			The output is a list of elements in the cohomology ring of N.
		Example
			stiefelWhitney realProjectiveSpace 2
			stiefelWhitney realProjectiveSpace 3
			stiefelWhitney hessenbergVariety 2
	SeeAlso
		cohomologyRing
///

doc ///
	Key
		(chern,QuasiToricManifold)
	Headline
		compute the Chern classes of a quasitoric manifold
	Usage
		chern(M)
	Inputs
		M:QuasiToricManifold
	Outputs
		:List
	Description
		Text
			Compute the Chern classes of a quasitoric manifold.
			The output is a list of elements in the cohomology ring of M.
		Example
			X = complexProjectiveSpace 3
			chern X
	SeeAlso
		cohomologyRing
///

doc ///
	Key
		bettiSmallCover
		(bettiSmallCover,ZZ,SmallCover)
		(bettiSmallCover,SmallCover)
	Headline
		(deprecated) compute the Betti numbers of a small cover
	Usage
		bettiSmallCover(k,N)
		bettiSmallCover(N)
	Inputs
		k:ZZ
		N:SmallCover
	Outputs
		:ZZ
		:List
	Description
		Text
			Compute the rational Betti numbers of a small cover.  If a
			dimension k is specified, then only the k-th Betti number of N is
			computed.  If no dimension is specified, all the Betti numbers
			between 0 and the dimension of N are computed.
	SeeAlso
		(betti, SmallCover)
///

doc ///
	Key
		bettiQTM
		(bettiQTM,ZZ,QuasiToricManifold)
		(bettiQTM,QuasiToricManifold)
	Headline
		(deprecated) compute the Betti numbers of a quasitoric manifold
	Usage
		bettiQTM(k,M)
		bettiQTM(M)
	Inputs
		k:ZZ
		M:QuasiToricManifold
	Outputs
		:ZZ
		:List
	Description
		Text
			Compute the Betti numbers of a quasitoric manifold.  If a
			dimension k is specified, then only the k-th Betti number of M is
			computed.  If no dimension is specified, all the Betti numbers
			between 0 and the dimension of M are computed.
	SeeAlso
		(betti, QuasiToricManifold)
///

doc ///
	Key
		(betti, SmallCover)
		(betti, ZZ, SmallCover)
	Headline
		compute the Betti numbers of a small cover
	Usage
		betti(k,X)
		betti(X)
	Inputs
		X:SmallCover
		Weights=>
			ignored
		Minimize=>
			ignored
	Outputs
		b:Nothing
			the k-th Betti number of X if k is provided, otherwise a @TO List@ of all Betti numbers
	Description
		Text
			This method computes the rational Betti numbers of a small cover
			using @HREF("https://arxiv.org/abs/1302.2342", "the formula of
			Suciu–Trevisan")@. If a dimension k is specified, then only the
			k-th Betti number of X is computed. If no dimension is specified,
			all the Betti numbers between 0 and m are computed where m is the
			dimension of the small cover X.
		Text
			The small cover over a square with the characteristic matrix
			$\begin{pmatrix}1 & 1 & 0 & 1 \\ 0 & 1 & 1 & 1\end{pmatrix}$
			is the Klein bottle, as indicated by its Betti numbers.
		Example
			needsPackage "SimplicialComplexes";
			R = QQ[a..d];
			K = simplicialComplex {a*b, b*c, c*d, d*a};
			lambda = matrix{{1, 1, 0, 1}, {0, 1, 1, 1}};
			X = smallCover(K, lambda)
			betti X
///

doc ///
	Key
		(betti, QuasiToricManifold)
		(betti, ZZ, QuasiToricManifold)
	Headline
		compute the Betti numbers of a quasitoric manifold
	Usage
		betti(k,X)
		betti(X)
	Inputs
		X:QuasiToricManifold
		Weights=>
			ignored
		Minimize=>
			ignored
	Outputs
		b:Nothing
			the k-th Betti number of X if k is provided, otherwise a @TO List@ of all Betti numbers
	Description
		Text
			This method computes the Betti numbers of a quasitoric manifold
			using @HREF("http://dx.doi.org/10.1215/S0012-7094-91-06217-4", "the
			classical formula of Davis–Januszkiewicz")@.  If a dimension k is
			specified, then only the k-th Betti number of X is computed. If no
			dimension is specified, all the Betti numbers between 0 and 2m are
			computed where 2m is the dimension of the quasitoric manifold X.
		Text
			The quasitoric manifold over a triangle with the characteristic matrix
			$\begin{pmatrix}1 & 0 & -1 \\ 0 & 1 & -1\end{pmatrix}$
			is the complex projective plane $\mathbb{CP}^2$,
			as indicated by its Betti numbers.
		Example
			needsPackage "SimplicialComplexes";
			R = QQ[a..c];
			K = simplicialComplex {a*b, b*c, c*a};
			lambda = matrix{{1, 0, -1}, {0, 1, -1}};
			X = quasiToricManifold(K, lambda)
			betti X
///

doc ///
	Key
		(betti, MomentAngleComplex)
		(betti, ZZ, MomentAngleComplex)
	Headline
		compute the Betti numbers of a moment-angle complex
	Usage
		betti(k,X)
		betti(X)
	Inputs
		X:MomentAngleComplex
		Weights=>
			ignored
		Minimize=>
			ignored
	Outputs
		b:Nothing
			the k-th Betti number of X if k is provided, otherwise a @TO List@ of all Betti numbers
	Description
		Text
			This method computes the Betti numbers of a moment-angle complex
			using @HREF("https://arxiv.org/abs/math/0407189", "the theorem of
			Baskakov–Buchstaber–Panov")@. If a dimension k is specified, then
			only the k-th Betti number of X is computed. If no dimension is
			specified, all the Betti numbers between 0 and 2m are computed
			(where m is the number of vertices in the underlying simplicial
			complex of the moment-angle complex).
		Text
			The moment-angle complex corresponding to the simplicial complex
			consisting of two disjoint vertices is homeomorphic to $S^3$, the
			3-sphere, as indicated by its Betti numbers.
		Example
			needsPackage "SimplicialComplexes";
			R = QQ[x,y];
			K = simplicialComplex {x, y};
			Z = momentAngleComplex K
			betti Z
		Text
			Let $\mathcal{Z}_K$ be the moment-angle complex corresponding to the
			simplicial complex consisting of 3 vertices, with an edge and a
			disjoint vertex. By Hochster's formula, its third cohomology
			$H^3(\mathcal{Z}_K)$ will have rank $2$. We can verify this as
			follows:
		Example
			needsPackage "SimplicialComplexes";
			R = QQ[x..z];
			K = simplicialComplex {x, y*z};
			Z = momentAngleComplex K
			betti (3, Z)
		Text
			The moment-angle complex corresponding to the boundary $\partial \Delta^2$
			of the 2-simplex is homeomorphic to $S^5$, as reflected by its
			Betti numbers.
		Example
			needsPackage "SimplicialComplexes";
			R = QQ[x..z];
			K = simplicialComplex {x*y, y*z, x*z};
			Z = momentAngleComplex K
			betti Z
///

doc ///
	Key
		(betti, NormalToricVariety)
		(betti, ZZ, NormalToricVariety)
	Headline
		compute the Betti numbers of a normal toric variety
	Usage
		betti(k,X)
		betti(X)
	Inputs
		X:NormalToricVariety
		Weights=>
			ignored
		Minimize=>
			ignored
	Outputs
		b:Nothing
			the k-th Betti number of X if k is provided, otherwise a @TO List@ of all Betti numbers
	Description
		Text
			This method computes the Betti numbers of a normal toric variety
			using @HREF("https://arxiv.org/abs/math/0308253", "the result of
			Franz")@. If a dimension k is specified, then only the k-th Betti
			number of X is computed. If no dimension is specified, all the
			Betti numbers between 0 and 2m are computed where m is the complex
			dimension of the normal toric variety X.
		Text
			Consider the normal toric variety $\mathbb{CP}^1 \times \mathbb{CP}^1$
			with two fixed points $([1:0], [1:0])$ and $([0:1], [0:1])$ removed.
			Its Betti numbers can be computed as follows:
		Example
			needsPackage "NormalToricVarieties";
			rayList = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};
			coneList = {{1, 2}, {0, 3}};
			X = normalToricVariety (rayList, coneList)
			betti X
///

doc ///
	Key
		[betti, Weights]
		[betti, Minimize]
	Headline
		options ignored by the toric-space methods of betti
	Description
		Text
			The methods @TO (betti, SmallCover)@, @TO (betti,
			QuasiToricManifold)@, @TO (betti, MomentAngleComplex)@ and @TO
			"ToricTopology :: betti(NormalToricVariety)"@ accept the options of
			@TO betti@ but ignore them.
///

doc ///
	Key
		bettiMAC
		(bettiMAC,ZZ,MomentAngleComplex)
		(bettiMAC,MomentAngleComplex)
	Headline
		(deprecated) compute the Betti numbers of a moment-angle complex
	Usage
		bettiMAC(k,Z)
		bettiMAC(Z)
	Inputs
		k:ZZ
			(optional)
		Z:MomentAngleComplex
	Outputs
		:ZZ
			the k-th Betti number of the moment-angle complex (if k is provided)
		:List
			of all Betti numbers
	Description
		Text
			This method computes the Betti numbers of a moment-angle complex
			using the theorem of Baskakov–Buchstaber–Panov. If a dimension k is
			specified, then only the k-th Betti number of Z is computed. If no
			dimension is specified, all the Betti numbers between 0 and 2m are
			computed (where m is the number of vertices in the underlying
			simplicial complex).
	SeeAlso
		(betti, MomentAngleComplex)
///

doc///
	Key
		(euler,MomentAngleComplex)
	Headline
		compute the Euler characteristic of a moment-angle complex
	Usage
		euler(Z)
	Inputs
		Z:MomentAngleComplex
	Outputs
		:ZZ
			the Euler characteristic of the moment-angle complex
	Description
		Text
			This method computes the Euler characteristic of moment-angle
			complexes.
		Text
			The Euler characteristic of a moment-angle complex is $0$ if the
			underlying simplicial complex is not a full simplex.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[a..d]
			K0 = simplicialComplex {a*b, b*c, c*d, d*a}
			Z0 = momentAngleComplex K0
			euler Z0
			K1 = simplicialComplex {a*b*c*d}
			Z1 = momentAngleComplex K1
			euler Z1
///

doc///
	Key
		eulerMAC
		(eulerMAC,MomentAngleComplex)
	Headline
		(deprecated) compute the Euler characteristic of a moment-angle complex
	Usage
		eulerMAC(Z)
	Inputs
		Z:MomentAngleComplex
	Outputs
		:ZZ
			the Euler characteristic of the moment-angle complex
	Description
		Text
			This method computes the Euler characteristic of moment-angle
			complexes.
	SeeAlso
		(euler, MomentAngleComplex)
///

doc ///
	Key
		realProjectiveSpace
		(realProjectiveSpace,ZZ)
	Headline
		real projective space of dimension n
	Usage
		realProjectiveSpace(n)
	Inputs
		n:ZZ
	Outputs
		:SmallCover
	Description
		Text
			Real projective space of dimension n, as a small cover.
	SeeAlso
		complexProjectiveSpace
///

doc ///
	Key
		complexProjectiveSpace
		(complexProjectiveSpace,ZZ)
	Headline
		complex projective space of dimension n
	Usage
		complexProjectiveSpace(n)
	Inputs
		n:ZZ
	Outputs
		:QuasiToricManifold
	Description
		Text
			Complex projective space of dimension n, as a quasitoric manifold.
	SeeAlso
		realProjectiveSpace
///

doc ///
	Key
		hessenbergVariety
		(hessenbergVariety,ZZ)
	Headline
		create a real Hessenberg variety
	Usage
		hessenbergVariety(n)
	Inputs
		n:ZZ
	Outputs
		:SmallCover
	Description
		Text
			This function creates a real Hessenberg variety of dimension n,
			associated to the (n+1)-permutahedron, as a small cover. It is
			known that the $k$-th Betti number of an $n$-dimensional real
			Hessenberg variety is given by $A_{2k} {n + 1 \choose 2k}$, where
			$A_{2k}$ is the @HREF("https://oeis.org/A000364",
			"Euler secant number")@.
		Example
			X = hessenbergVariety 3
			betti X
	SeeAlso
///

doc ///
	Key
		AFPVariety
		(AFPVariety, ZZ, ZZ)
	Headline
		non-equivariantly formal toric varieties
	Usage
		AFPVariety(d, ord)
	Inputs
		d:ZZ
		ord:ZZ
	Outputs
		:NormalToricVariety
	Description
		Text
			A class of smooth toric varieties whose equivariant cohomology
			is torsion-free but not free is described and studied in
			@HREF("https://doi.org/10.1090/S0002-9947-2014-06165-5", "the work
			of Allday–Franz–Puppe")@ (hence the name). These varieties are
			described by their complex dimension d, and the syzygy order
			ord of their equivariant cohomology. Note that we must have 0 <
			ord < d. For ord = 0, the equivariant cohomology is not
			torsion-free and for ord = d, the equivariant cohomology is free.
			These cases are not supported.  The variety is obtained by
			removing two appropriately chosen fixed points from
			$(\mathbb{CP}^1)^d$.
		Text
			The AFPVariety(2, 1) is $\left(\mathbb{CP}^1\right)^2 - \{([1:0],
			[1:0]), ([0:1], [0:1])\}$.
		Example
			X = AFPVariety (2, 1)
			betti X
		Text
			The AFPVariety(4, 2) is $\left(\mathbb{CP}^1\right)^4 - \{([1:0],
			[1:0], [1:0], [1:0]), ([0:1], [0:1], [0:1], [1:0])\}$.
		Example
			X = AFPVariety (4, 2)
			betti X
	SeeAlso
///
