-- -*- coding: utf-8 -*-
doc ///
	Key
		ToricTopology
	Headline
		homological computations in toric topology
	Description
		Text
			ToricTopology is a package for computing with quasitoric
			manifolds, small covers and moment-angle complexes.

			A quasitoric manifold (or small cover) is entirely determined by a
			pair consisting of a simplicial complex K and a matrix lambda which
			is characteristic for K.

			If K has n vertices, we can think of its k-faces as sets of
			integers between 1 and n. A matrix lambda is characteristic for K
			if all maximal minors of lambda indexed by the facets of  K have
			determinant equal to 1 or -1.
	SeeAlso
		--NormalToricVarieties
///

doc ///
	Key
		SmallCover
	Headline
		the class of all small covers
	Description
		Text
			A small cover is represented by a simplicial complex K and matrix
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
			matrix which is characteristic for K.
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
			union of certain products of disks and circles: $$\mathcal{Z_K} =
			\bigcup_{\sigma \in K} \left( (D^2)^\sigma \times (S^1)^{[m]
			\setminus \sigma} \right).$$ These spaces admit a natural action of
			the torus $T^m = (S^1)^m$. Non-singular toric varieties (not
			necessarily complete) are homotopy equivalent to partial quotients
			of moment-angle complexes by freely acting subtori of $T^m$. Thus,
			moment-angle complexes are an important class of spaces studied in
			Toric Topology. Their topological properties can be determined from
			the combinatorics of the underlying simplicial complex.  This
			package implements methods to determine some of these properties.
			A moment-angle complex is a special case of polyhedral products.
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
			lambda is not characteristic for K, then an error is returned.  The
			entries of lambda are automatically converted to ZZ/2 entries, if
			they not already so.
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
			returned.
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
			Create a moment-angle complex with simplicial complex K.
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
		compute the equivariant cohomology of a toric space (normal toric variety and moment-angle complex)
	Usage
		equivariantCohomology(X)
		equivariantCohomology(Z)
	Inputs
		X:NormalToricVariety
		Z:MomentAngleComplex
	Outputs
		:Module
			the torus equivariant cohomology of Z, as a module over polynomial
			ring
	Description
		Text
			Compute the equivariant cohomology of certain classes of spaces with
			torus actions: normal toric varieties and  moment-angle complex.
			The equivariant cohomology is computed as a module over
			polynomial ring in which the coefficients are dependent
			on the context.
			For normal toric varieties, the underlying ring is the polynomial
			ring QQ[t_1, .. t_r] where r is the dimension of the toric variety.
			For moment-angle complexes, the underlying ring is the polynomial
			ring k[x_1, ..., x_m] where k is the coefficient ring of the
			polynomial ring over which the underlying simplicial complex was
			created.
		Text
			The equivariant cohomology of a moment-angle complex is free over the polynomial ring
			when the simplicial complex is a full simplex.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[x,y,z]
			K = simplicialComplex {x*y*z}
			Z = momentAngleComplex K
			M = equivariantCohomology Z
			isFreeModule M
		Text
			If there is any missing simplex, then the equivariant cohomology is
			not free.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[x,y,z]
			K = simplicialComplex {x*y, y*z, x*z}
			Z = momentAngleComplex K
			M = equivariantCohomology Z
			isFreeModule M
		Text
			We can also compute the equivariant cohomology of a normal toric variety.
			In the example below, we compute the equivariant cohomology
			of $\mathbb{CP}^2$ with respect to the standard torus ($T^2$) action.
		Example
			needsPackage "NormalToricVarieties"
			rayList = {{1, 0}, {0, 1}, {}}
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
		chern
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
		(deprecated) compute the betti numbers of a small cover
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
			Compute the rational Betti numbers of a small cover.  If a dimension k is
			specified, then only the k-th Betti number of N is computed.  If no
			dimension is specified, all the Betti numbers between 0 and the
			dimension of N are computed.
	SeeAlso

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
///

doc ///
	Key
		betti
		(betti, ZZ, SmallCover)
		(betti, SmallCover)
		(betti, ZZ, QuasiToricManifold)
		(betti, QuasiToricManifold)
		(betti, ZZ, MomentAngleComplex)
		(betti, MomentAngleComplex)
		(betti, ZZ, NormalToricVariety)
		(betti, NormalToricVariety)
	Headline
		compute the Betti numbers of a toric space
	Usage
		betti(k,X)
		betti(X)
	Inputs
		k:ZZ
			(optional)
		X:SmallCover
		X:QuasiToricManifold
		X:MomentAngleComplex
		X:NormalToricVariety
	Outputs
		:ZZ
			the k-th Betti number of the moment angle complex (if k is provided)
		:List
			of all Betti numbers
	Description
		Text
			This method computes the Betti numbers of a toric space (a space
			with a torus action, ie, small cover, quasitoric manifold,
			moment-angle complex and normal toric variety).  For small-covers,
			the formula of Suciu-Trevisan (https://arxiv.org/abs/1302.2342) is
			used.  For quasitoric manifolds, the classical formula of
			Davis-Januszkiewicz
			(http://dx.doi.org/10.1215/S0012-7094-91-06217-4) is used. For
			moment-angle complexes, the theorem of Baskakov-Buchstaber-Panov
			(https://arxiv.org/abs/math/0407189) is used. For normal toric
			varieties, the result of Franz (https://arxiv.org/abs/math/0308253)
			is used. If a dimension k is specified, then only the k-th Betti
			number of X is computed. If no dimension is specified, all the
			Betti numbers between 0 and 2m are computed (where m is either the
			dimension of the small-cover, quasitoric manifold or normal toric
			variety X, or it is the number of vertices in the underlying
			simplicial complex of the moment-angle complex).
		Text
			The small cover over a square with the characteristic matrix
			$\begin{pmatrix}1 & 1 & 0 & 1 \\ 0 & 1 & 1 & 1\end{pmatrix}$
			is the Klein bottle, as indicated by its betti numbers.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[a..d]
			K = simplicialComplex {a*b, b*c, c*d, d*a}
			lambda = matrix{{1, 1, 0, 1}, {0, 1, 1, 1}}
			X = smallCover(K, lambda)
			betti X
		Text
			The quasitoric manifold over a triangle with the characteristic matrix
			$\begin{pmatrix}1 & 0 & -1 \\ 0 & 1 & -1\end{pmatrix}$
			is the complex projective plane $\mathbb{CP}^2$,
			as indicated by its betti numbers.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[a..c]
			K = simplicialComplex {a*b, b*c, c*a}
			lambda = matrix{{1, 0, -1}, {0, 1, -1}}
			X = quasiToricManifold(K, lambda)
			betti X
		Text
			The moment-angle complex corresponding to the simplicial complex
			consisting of two disjoint vertices is homeomorphic to $S^3$, the
			3-sphere as indicated by its Betti numbers.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[x,y]
			K = simplicialComplex {x, y}
			Z = momentAngleComplex K
			betti Z
		Text
			Let $\mathcal{Z}_K$ be the moment-angle corresponding to the
			simplicial complex consisting on 3 vertices, with an edge and a
			disjoint vertex. By Hochster's formula, its third cohomology
			$H^3(\mathcal{Z}_K)$ will have rank $2$. We can verify this as
			follows,
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[x..z]
			K = simplicialComplex {x, y*z}
			Z = momentAngleComplex K
			betti (3, Z)
		Text
			The moment-angle corresponding to the boundary $\partial \Delta^2$
			of the 2-simplex is homeomorphic to $S^5$, as reflected by its
			Betti numbers.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[x..z]
			K = simplicialComplex {x*y, y*z, x*z}
			Z = momentAngleComplex K
			betti Z
		Text
			The consider the normal toric variety $\mathbb{CP}^1 \times \mathbb{CP}^1$
			with two fixed points $([1:0], [1:0])$ and $([0:1], [0:1])$ removed.
			Its betti numbers can be computed as follows:
		Example
			needsPackage "NormalToricVarieties"
			rayList = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}}
			coneList = {{1, 2}, {0, 3}}
			X = normalToricVariety (rayList, coneList)
			betti X
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
			the k-th Betti number of the moment angle complex (if k is provided)
		:List
			of all Betti numbers
	Description
		Text
			This method computes the Betti numbers of a moment-angle complex
			using the theorem of Baskakov-Buchstaber-Panov. If a dimension k is
			specified, then only the k-th Betti number of Z is computed. If no
			dimension is specified, all the Betti numbers between 0 and 2m are
			computed (where m is the number of vertices in the underlying
			simplicial complex).
///

doc///
	Key
		euler
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
		Hessenberg variety associated to the n-permutahedron
	Usage
		hessenbergVariety(n)
	Inputs
		n:ZZ
	Outputs
		:SmallCover
	Description
		Text
			Hessenberg variety associated to the n-permutahedron, as small
			cover.
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
			Normal toric variety whose equivariant cohomology is not free but
			is torsion-free (as described in
			https://doi.org/10.1090/S0002-9947-2014-06165-5). These varieties
			are described by their complex dimension $d$, and the syzygy order
			$ord$ of their equivariant cohomology. Note that we must have 1 <
			ord < d. For ord = 0, the equivariant cohomology is not
			torsion-free and for ord = d, the equivariant cohomology is free.
			So we don't handle those cases.  The variety is obtained by
			removing two appropriately chosen fixed points from
			$(\mathbb{CP}^1)^d$.
		Text
			The AFPVarity(2, 1) is $(\mathbb{CP^1})^2 - \{([1:0], [1:0]),
			([0:1], [0:1])\}$.
		Example
			X = AFPVariety (2, 1)
			betti X
	SeeAlso
///
