
-*-----------------------------------------------------------------------------------------
Documentation for "GKMVarieties.m2"

Authors: Christopher Eur, Ritvik Ramkumar
-----------------------------------------------------------------------------------------*-


beginDocumentation()


doc ///
	Key
		GKMVarieties
	Headline
		computations with GKM varieties and moment graphs
	Description
		Text
			A GKM variety is a variety $X$, often assumed to be smooth and complete, with an 
			action of an algebraic torus $T$ satisfying the following conditions:
			(i) $X$ is equivariantly formal with respect to the the action of $T$,
			(ii) $X$ has finitely many $T$-fixed points, and (iii) $X$ has finitely
			many one-dimensional $T$-orbits.  The data of the zero and one dimensional
			$T$-orbits of $X$ define the moment graph of $X$, with which one can carry out
			$T$-equivariant cohomology and $T$-equivariant $K$-theory computations via
			the method of localization.
			This package provides methods for these computations in Macaulay2.
			
		Text
			For mathematical background see:
			
			@UL{
			{"[BM01] T. Braden and R. MacPherson.  From moment graphs to intersection cohomology.  Math. Ann. 321 (2001), 533-551."},
			{"[BGH02] E. Bolker, V. Guillemin, and T. Holm.  How is a graph like a manifold?  arXiv:math/0206103."},
			{"[CDMS18] A. Cameron, R. Dinu, M. Michalek, and T. Seynnaeve.  Flag matroids: algebra and geometry.  arXiv:1811.00272."},
			{"[DES20] R. Dinu, C. Eur, and T. Seynnaeve.  K-theoretic Tutte polynomials of morphisms of matroids.  arXiv:math/2004.00112."},
			{"[FS12] A. Fink and S. Speyer.  K-classes for matroids and equivariant localization.  Duke Math. J. 161 (2012), no. 14, 2699-2723."},
			{"[GKM98] M. Goresky, R. Kottwitz, and R. MacPherson.  Equivariant cohomology, Koszul duality, and the localization theorem. Invent. Math. 131 (1998), no. 1, 25-83."},
			{"[RK03] I. Rosu.  Equivariant K-theory and equivariant cohomology.  With an Appendix by I. Rosu and A. Knutson.  Math. Z. 243 (2003), 423-448."},
			{"[Tym05] J. Tymoczko.  An introduction to equivariant cohomology and homology, following Goresky, Kottwitz, and MacPherson.  Contemp. Math. 388 (2005), 169-188."},
			{"[VV03] G. Vezzosi and A. Vistoli.  Higher algebraic K-theory for actions of diagonalizable groups. Invent. Math. 153 (2003), no. 1, 1–44."}
			}@

		Text
			@SUBSECTION "Contributors"@
		Text
			The following people have contributed code, improved existing code, or enhanced the documentation:
			@HREF("https://www.mis.mpg.de/combag/members/tim-seynnaeve.html","Tim Seynnaeve")@.
		
	SeeAlso
		"Example: generalized flag varieties"
		"Example: smooth toric varieties"

		

///



doc ///
	Key
		"Example: generalized flag varieties"
	Description
		Text
			Let $G$ be a reductive complex Lie group and $P$ a 
			parabolic subgroup containing a maximal torus $T$.  The generalized flag variety $G/P$ is a GKM variety 
			with the action of $T$.  This package allows users to create a generalized flag variety for classical Lie types 
			($A$, $B$, $C$, and $D$) as a @TO "GKMVariety"@ with conventions explicitly laid out as follows.

		Text
			For type $A_{n-1}$, the group $G$ is $GL_{n}$, and the torus $T$ is $diag(t_1, \ldots, t_n)$, the group of invertible diagonal matrices.

		Text
			For type $B_n$, the group $G$ is $SO_{2n+1}$, where we set the standard symmetric bilinear form on $\mathbb C^{2n+1}$ 
			to be is given by the matrix
			$$\begin{pmatrix} 0 & I_n & 0 \\ I_n & 0 & 0 \\ 0 & 0 & 1 \end{pmatrix}$$
			and the torus $T$ is $diag(t_1, \ldots,t_n, t_1^{-1}, \ldots, t_n^{-1}, 1)$.

		Text
			For type $C_n$, the group $G$ is $Sp_{2n}$, where we set the standard alternating bilinear form on 
			$\mathbb C^{2n}$ to be given by the matrix
			$$\begin{pmatrix} 0 & -I_n \\ I_n & 0 \end{pmatrix}$$
			and the torus $T$ is $diag(t_1, \ldots,t_n, t_1^{-1}, \ldots, t_n^{-1})$.

		Text
			For type $D_n$, the group $G$ is $SO_{2n}$, where we set the standard symmetric bilinear form on 
			$\mathbb C^{2n}$ to be given by the matrix
			$$\begin{pmatrix} 0 & I_n \\ I_n & 0 \end{pmatrix}$$
			and the torus $T$ is $diag(t_1, \ldots,t_n, t_1^{-1} \ldots, t_n^{-1})$.

		Text
			In all the cases, the standard action of $(\mathbb C^*)^m$ on $\mathbb C^m$ is defined by $(t_1, \ldots, t_m) \cdot (x_1, \ldots, x_m) = (t_1^{-1}x_1, \ldots, t_m^{-1}x_m)$.  

		Text
			Let $\{w_1, \ldots, w_n\}$ be a set of fundamental weights, which for classical Lie types are explicitly set to be 
			as follows:

			($A_{n-1}$): $\{w_1, \ldots, w_n\}= \{e_1, e_1+e_2, \ldots , e_1+e_2+\cdots+e_{n-1}\}$

			($B_n$): $\{w_1, \ldots, w_n\}= \{e_1, e_1+e_2, \ldots , e_1+\cdots+e_{n-1}, (1/2)(e_1+\cdots e_n)\}$

			($C_n$): $\{w_1, \ldots, w_n\}= \{e_1, e_1+e_2, \ldots , e_1+\cdots+e_{n-1}, e_1 + \cdots +e_n\}$

			($D_n$): $\{w_1, \ldots, w_n\}= \{e_1, e_1+e_2, \ldots , e_1+\cdots+e_{n-2}, (1/2)(e_1+\cdots+e_{n-2} +e_{n-1}- e_{n}), (1/2)(e_1+\cdots+e_{n-2}+e_{n-1}+e_n)\}$

		Text
			For a sequence $(a_1, \ldots, a_n)\in \mathbb N^n$ of nonnegative integers, 
			let $I = \{i \mid a_i \neq 0\}$ and $P_I$ the corresponding parabolic subgroup of $G$.
			Then the generalized flag variety $G/P_I$ is embedded in the irreducible representation of $G$ 
			with the highest weight $a_1w_1 + \cdots a_nw_n$.
			These generalized flag varieties can be created as a @TO "GKMVariety"@ using the method 
			@TO generalizedFlagVariety@.  For instance, the Grassmannian $Gr(2,4)$ of
			2-dimensional subspaces in $\mathbb C^4$, embedded in $\mathbb P^5$ by the usual Plücker embedding,
			can be created as follows.

		Example
			Gr24 = generalizedFlagVariety("A",3,{2})
			peek Gr24

		Text
			The @TO MomentGraph@ of $Gr(2,4)$ is the 1-skeleton of the hypersimplex $\Delta(2,4)$, a.k.a. the octahedron.

		Example
			G = momentGraph Gr24
			underlyingGraph G

		Text
			The line bundle $O(1)$ on $Gr(2,4)$, corresponding to its Plücker embedding, can be accessed by @TO (ampleKClass, GKMVariety)@.
			The method @TO (euler, KClass)@ computes its Lefschetz trace (a.k.a. equivariant Euler characteristic),
			which in this case is the Laurent polynomial in the character ring of the torus $T$ 
			whose terms correspond to be weights of the second exterior power of the standard representation of $GL_4$.

		Example
			O1 = ampleKClass Gr24 --the O(1) bundle on Gr24 via its Plücker embedding
			euler O1

		Text
			If $Gr(2,4)$ is embedded differently, say by the line bundle $O(2)$ instead, the Lefschetz trace changes 
			accordingly, and its coefficients record the multiplicities of the associated weight spaces in the second
			symmetric power of the second exterior power of the standard representation of $GL_4$.

		Example
			euler (O1^2)

		Text
			The Schubert decomposition of $Gr(2,4)$, and more generally the Bruhat decomposition of $G/P$, can be accessed 
			by the method @TO (bruhatOrder, GKMVariety)@, which outputs the poset of the Bruhat order.  Moreover, the Schubert
			varieties can be created via the method @TO generalizedSchubertVariety@.

		Example
			P1 =  bruhatOrder Gr24
			Sch = generalizedSchubertVariety(Gr24,{set{1,2}})
			P2 = bruhatOrder Sch
			--{P1,P2}/displayPoset --to view the posets in pdf

		Text
			The "forgetful" map from the complete flag variety $Fl(4)$ to $Gr(2,4)$, 
			given by forgetting the subpsaces in the complete flag except for the 2-dimensional one, can be created as 
			a @TO EquivariantMap@ by the method @TO flagMap@.

		Example
			Fl4 = generalizedFlagVariety("A",3,{1,2,3},Gr24.characterRing) --Fl(4) with the torus having the same character ring as Gr24
			f = flagMap(Fl4,Gr24)
			Fl4 === f.source and Gr24 === f.target

		Text
			As $Fl(4)$ is a $BiProj$ of vector bundles on $Gr(2,4)$, the (derived) pushforward of the structure sheaf 
			of $Fl(4)$ is the structure sheaf of $Gr(2,4)$ since the higher direct images vanish under the forgetful map.

		Example
			(trivialKClass Gr24) === (pushforward f)(trivialKClass Fl4)

		Text
			For type $C$, the following example features the isotropic Grassmannian $SpGr(2,6)$ consisting of 
			2-dimensional subspaces in $\mathbb C^6$ that are isotropic with respect to the standard alternating form.
			The vertices of its moment graph can be considered as the vertices of the cuboctahedron.

		Example
			SpGr26 = generalizedFlagVariety("C",3,{2})
			peek SpGr26
			momentGraph SpGr26

		Text
			The second fundamental representation of $Sp_{6}$ is 14-dimensional with 12 extremal weights.

		Example
			euler ampleKClass SpGr26

		Text
			For type $B$, the following example features the isotropic Grassmannian $SOGr(2,5)$ consisting of
			3-dimensional subspaces in $\mathbb C^5$ that are isotropic with respect to the standard symmetric form.
			Its moment graph is the a complete graph on 4 vertices.
			Note that Spin groups and their representations are not implemented, so for the type $B_n$ the coefficient
			$a_n$ need be a multiple of 2.

		Example
			SOGr25 = generalizedFlagVariety("B",2,{2,2}) --inputing {2} instead of {2,2} results in error: spin groups not implemented yet
			peek SOGr25
			euler ampleKClass SOGr25

		Text
			For type $D$, the following example features the isotropic Grassmannian $SOGr(3,8)$ consisting of
			3-dimensional subspaces in $\mathbb C^8$ that are isotropic with respect to the standard symmetric form.

		Example
			SOGr38 = generalizedFlagVariety("D",4,{3,4})
			SOGr38.points

		Text
			Similarly as in type $B$, Spin groups are not implemented, so the two connected components of 
			$SOGr(4,8)$ need be separatedly created in the following way.

		Example
			SOGr48odd = generalizedFlagVariety("D",4,{3,3})
			SOGr48odd.points
			SOGr48even = generalizedFlagVariety("D",4,{4,4})
			SOGr48even.points

	Caveat
		Does not check for low-dimensional isogeneis.  For instance, always use type $D_n$ with $n\geq 4$ to be safe.

	SeeAlso
		generalizedFlagVariety
		flagMap
		GKMVariety
		makeGKMVariety





///




doc ///
	Key
		"Example: smooth toric varieties"
	Description
		Text
			A toric variety is an integral variety containing an open dense algebraic torus. If the 
			toric variety is smooth (or simplicial) it is naturally a GKM variety:
			Let $X$ be a smooth toric variety 
			and $U$ be an affine chart whose associated character lattice is generated by elements of
			weights $a_1,\dots, a_m$. Then $(\mathbb C^*)^n$ acts on $U$ by 
			$t \cdot (x_1,\dots, x_n) = (t^{-a_1}x_1,\dots, t^{-a_n}x_n)$.
			We caution that this package uses the outer normals instead of inner normals.
			
		Text	
			The method @TO "normalToricVariety"@ from the package @TO "NormalToricVarieties"@ allows the 
			user to construct smooth toric varieties. To convert it to a GKM variety we use the method
			@TO "makeGKMVariety"@. Here is an example with $X = Bl_p\mathbb P^2$, the blow-up of $\mathbb P^2$ at a point,
			which is also the first Hirzebruch surface.
		Example
			FF1 = hirzebruchSurface 1;
			X = makeGKMVariety FF1;
			peek FF1
			peek X

   		Text
			If a GKM variety $X$ was originally constructed from @TO "normalToricVariety"@ we can convert it
			back to a toric variety.
		Example
			Y = normalToricVariety(X); -- X defined in the previous example above
			Y === FF1
		Text
			Continuing this example, the following shows how to convert a torus-invariant divisor constructed using
			@TO ToricDivisor@ to a @TO KClass@.

		Example
			antiK = - toricDivisor(FF1) -- the anti-canonical class on FF1
			TantiK = makeKClass(X,antiK)
			isWellDefined TantiK

		Text
			Since the toric variety $X = Bl_p\mathbb P^2$ is Gorenstein Fano, 
			with its anticanonical embedding in $\mathbb P^8$, the equivariant Euler characteristic of
			the anticanonical divisor is the sum of the characters of the sections of the associated line bundle.

		Example
			euler TantiK

		Text
			We caution the following difference in convention:
			Projective $n$-space $\mathbb P^n$ as a @TO NormalToricVariety@ constructed using @TO "toricProjectiveSpace"@
			is acted upon by an $n$-dimensional torus. However, as a @TO GKMVariety@
			constructed using @TO "projectiveSpace"@, it is acted upon by an $(n+1)$-dimensional torus.
		Example
			X = makeGKMVariety toricProjectiveSpace 2; -- the torus is C^2
	   		Y = projectiveSpace 2; -- the torus is C^3
			peek X
			peek Y		

	SeeAlso
		(makeGKMVariety, NormalToricVariety)
		projectiveSpace
		normalToricVariety
		(makeKClass, GKMVariety, ToricDivisor)

///


doc ///
	Key
		(normalToricVariety, GKMVariety)
	Headline
		converts a GKM variety back into a toric variety
	Usage
		Y = normalToricVariety X
	Inputs
		X:GKMVariety
	Outputs
		Y:NormalToricVariety		
	Description			
		Text	
			If $X$ is a GKM variety that was originally constructed using @TO "normalToricVariety"@, then
			this method reverts $X$ to a @TO "NormalToricVariety"@.
			
		Example
			X = toricProjectiveSpace 2;
			Y = makeGKMVariety X
			assert(normalToricVariety Y === X)

	SeeAlso
		makeGKMVariety
		normalToricVariety

///




doc ///
	Key
		GKMVariety
	Headline
		the class of all GKM varieties
	Description
		Text
			A @TO GKMVariety@ $X$ is a @TO MutableHashTable@ representing a GKM variety $X$ with an action of a torus $T$.
			Its keys include:
			
			@UL{
			{TT "points", ", whose value is a list representing the torus-fixed points of ", TEX "$X$"},
			{TT "characterRing", ", whose value is a ring representing the character ring of ", TEX "$T$"},
			{TT "momentGraph", ", whose value is the ", TO "MomentGraph", " of ", TEX "$X$"},
			{TT "charts", ", whose value is a ", TO "HashTable", " representing the (negatives of) characters of the torus action 
			on each torus-invariant affine chart around a torus-fixed point. ", " The keys of ", TT "X.charts", " are ", TT "X.points", 
			" and the values are lists consisting of lists of integers."}
			}@

		Text
			Every @TO GKMVariety@ created by methods in this package has at least the two keys @TT "points"@ and @TT "characterRing"@.
			The following example is the projective space $\mathbb P^2$ as a @TO GKMVariety@.

		Example
			PP2 = projectiveSpace 2
			peek PP2

		   			
	SeeAlso
		makeGKMVariety
		"Example: generalized flag varieties"
		"Example: smooth toric varieties"
		





///


doc ///
	Key
		makeGKMVariety
		(makeGKMVariety, List, Ring)
		(makeGKMVariety, List, List, Ring)
		(makeGKMVariety, MomentGraph)
		(makeGKMVariety, MomentGraph, Ring)
		(makeGKMVariety, NormalToricVariety)
		(makeGKMVariety, NormalToricVariety,Ring)
		(makeGKMVariety, KClass)
	Headline
		constructs a GKM variety
	Usage
		X = makeGKMVariety(L,R)
		X = makeGKMVariety(L,M,R)
		X = makeGKMVariety(G)
		X = makeGKMVariety(G,R)
		X = makeGKMVariety(R)
		X = makeGKMVariety(Y,R)
		X = makeGKMVariety(C)
	Inputs
		L:List
			of torus-fixed points of $X$
		M:List
			of lists; the i-th list consists of the (negatives of) characters of the 
			action of the torus on a torus-invariant affine chart around the torus-fixed point 
			corresponding to L_i
		G:MomentGraph
			representing the one dimensional torus-orbits of $X$
		R:Ring
			representing the character ring of the torus acting on $X$
		Y:NormalToricVariety
		C:KClass
	Outputs
		X:GKMVariety
	Description
		Text
			The minimum data needed to create a @TO "GKMVariety"@ are the set of torus-fixed points
			and the character ring. Here is an example with projective space	
		Example
			L = {0,1,2,3};
			R = makeCharacterRing 4
			X = makeGKMVariety(L,R)
		Text	
			If necessary, we can add the (negatives of) characters of the action of the torus on each 
			torus-invariant chart of $X$. Note that the i-th entry of the list below corresponds to
			the i-th entry of L.
		Example
			M = {{{-1, 1, 0, 0}, {-1, 0, 1, 0}, {-1, 0, 0, 1}},
				{{1, -1, 0, 0}, {0, -1, 1, 0}, {0, -1, 0, 1}},
				{{1, 0, -1, 0}, {0, 1, -1, 0}, {0, 0, -1, 1}},
				{{1, 0, 0, -1}, {0, 1, 0, -1}, {0, 0, 1, -1}}};
			Y = makeGKMVariety(L,M,R);
			peek Y
		Text
			To produce one of the generalized flag varieties we use the method @TO generalizedFlagVariety@
			Here is an example of the Lagrangian Grassmannian $SpGr(2,4)$ consisting of 2-dimensional subspaces
			in $\mathbb C^4$ that are isotropic with respect to the standard alternating form.

		Example
			SpGr24 = generalizedFlagVariety("C",2,{2})
			peek SpGr24
		
		Text
			Here is the complete flag variety of $Sp_4$.

		Example
			SpFl4 = generalizedFlagVariety("C",2,{1,2})
			peek SpFl4
		
		Text
			The following example produces the Orthogonal Grassmannian $SOGr(2,5)$ from its
			moment graph.

		Example
			V = {{set {0, 1}}, {set {0, "1*"}}, {set {"0*", 1}}, {set {"0*", "1*"}}};
			edgs = {{{set {"0*", 1}}, {set {"0*", "1*"}}},
				{{set {0, "1*"}}, {set {"0*", "1*"}}},
				{{set {0, "1*"}}, {set {"0*", 1}}},
				{{set {0, "1*"}}, {set {0, 1}}},
				{{set {0, 1}}, {set {"0*", "1*"}}},
				{{set {0, 1}}, {set {"0*", 1}}}};
			wghts = {{0,-1},{-1,0},{-1,1},{0,1},{-1,-1},{-1,0}}
			E = hashTable(apply(edgs, v -> (v,wghts)));
			t = symbol t; H = QQ[t_0, t_1]
			G = momentGraph(V,E,H);
			Z = makeGKMVariety(G);
			peek Z			

	Caveat
		This function does not check if X is a valid GKM variety.
	
	SeeAlso
		(symbol **, GKMVariety, GKMVariety)
		projectiveSpace
		generalizedFlagVariety
		(map, GKMVariety, GKMVariety, List)
///


doc ///
	Key
		(symbol **, GKMVariety, GKMVariety)
	Headline
		product of GKM varieties
	Usage
		X ** Y
	Inputs
		X:GKMVariety
		Y:GKMVariety
	Outputs
		X:GKMVariety
			product of X and Y
	Description
		Text
			Given two GKM varieties $X$ and $Y$ with an action of a common torus $T$, the
			product is $X \times Y$ with the structure of a GKM variety given by the 
			diagonal action of $T$. This method constructs $X \times Y$ as a @TO "GKMVariety"@.
			To speed up computation, this method does not automatically cache
			the moment graph of $X \times Y$. The user can cache this using the method
 			@TO"MomentGraph ** MomentGraph"@.
		Text
			The following example exhibits the product of $\mathbb P^1$ with
			the Lagrangian Grassmannian SpGr(2,4). 
		Example
			R = makeCharacterRing 2;
			X = projectiveSpace(1,R);
			Y = generalizedFlagVariety("C",2,{2},R);
			Z = X ** Y;
			peek Z
		Text
			We can cache the moment graph of $Z$ as follows:
		Example
			G = momentGraph X;
			H = momentGraph Y;
			momentGraph(Z, G** H);
			peek Z
			
			
				 	
	SeeAlso
		makeGKMVariety

	
///




doc ///
	Key
		KClass
	Headline
		the class of all equivariant K-classes
	Description
		Text
			For $X$ a GKM variety with an action of a torus $T$ whose character ring is $R$,
			a $T$-equivariant $K$-class $C \in K_T^0(X)$ of is encoded by its image in $K_T^0(X^T) = \prod_{x\in X^T} R$,
			under the injective restriction map $K_T^0(X) \to K_T^0(X^T)$.
			See [Corollary 5.12; VV03] or [Corollary A.5; RK03] for details.

		Text
			A @TO "KClass"@ C is a @TO "HashTable"@
			consisting of two keys:

			@UL{
			{TT "variety", ", whose value is a ", TO "GKMVariety", " of which C is a K-class of"},
			{TT "KPolynomials", ", whose value is a ", TO "HashTable", "; its keys are ", TT "X.points", " and the values are
			Laurent polynomials in the character ring representing the values of the K-class under the restriction map."}
			}@


	SeeAlso
		makeKClass
		(isWellDefined, KClass)
		pushforward
		pullback
		(euler, KClass)

///



doc ///
	Key
		makeKClass
		(makeKClass, GKMVariety, List)
	Headline
		constructs an equivariant K-class
	Usage
		C = makeKClass(X,L)
	Inputs
		X:GKMVariety
		L:List
			of Laurent polynomials corresponding to each torus-fixed point
		D:ToricDivisor	
	Outputs
		C:KClass
	Description
		Text
			This method creates a @TO KClass@ given a @TO GKMVariety@ @TT "X"@ and a list @TT "L"@ of Laurent polynomials in its
			character ring.  The order of Laurent polynomials in the list must correspond to the order of the list
			of torus-fixed points @TT "X.points"@.

		Text
			The following example is the class of $O(1)$ on the projective space $\mathbb P^3$.
			
		Example
			PP3 = projectiveSpace 3;
			R = PP3.characterRing;
			L = gens R
			C = makeKClass(PP3,L) --the class of O(1) on PP3
			C === ampleKClass PP3
			isWellDefined C
	Caveat
		This function does not check if X defines a GKM variety - see 
		@TO2{(isWellDefined, KClass), "isWellDefined"}@.
	
	SeeAlso
		(isWellDefined, KClass)
		(symbol *, KClass, KClass)
		(symbol +, KClass, KClass)
		pullback
		pushforward
///

doc ///
	Key
		(makeKClass, GKMVariety, ToricDivisor)
	Headline
		create the KClass from a ToricDivisor
	Usage
		C = makeKClass(X,D)
	Inputs
		X:GKMVariety
			created from a @TO NormalToricVariety@
		D:ToricDivisor
	Outputs
		C:KClass
	Description
		Text
			If a GKM variety $X$ also admits a structure of a @TO NormalToricVariety@,
			then the following example shows how to obtain the @TO KClass@ of any 
			@TO ToricDivisor@ on $X$. 
		Example
			X = toricProjectiveSpace 3;
			D = toricDivisor({1,0,0,0},X) -- the class of O(1) on P^3
			Y = makeGKMVariety X; -- The torus is C^3 not C^4
			C = makeKClass(Y,D)
			assert(isWellDefined C)
			peek C
	Caveat
		Toric vector bundles are yet to be imported.
	SeeAlso
		makeKClass
		KClass
		(normalToricVariety, GKMVariety)
		(makeGKMVariety, NormalToricVariety)
///


doc ///
	Key
		(isWellDefined, KClass)
	Headline
		whether the input is a well-defined equivariant K-class
	Usage
		isWellDefined C
	Inputs
		C:KClass
	Outputs
		:Boolean
			whether or not a list of Laurent polynomials satisfies edge compatibility condition
			--prints out the edges of the moment graph for which @TT "C"@ fails the compatibility condition
	Description
		Text
			If $\{f_x \mid x\in X^T\}$ is a collection of Laurent polynomials in the
			character ring $\mathbb Z[T_0, \ldots, T_n]$ of the torus $T$ acting on a @TO GKMVariety@ $X$, one per each torus-fixed point, representing an element $C$ of $K_T^0(X^T)$,
			then $C$ is in the image of $K_T^0(X)$ under the injective restriction map $K_T^0(X)\to K_T^0(X^T)$ if and only if
			it satisfies the following "edge compatibility condition":

			For each one-dimensional $T$-orbit-closure in $X$ with boundary points $x$ and $x'$, one has
			$$f_x \equiv f_{x'} \ \mod \ 1 - T^{\lambda(x,x')}$$
			where $\lambda(x,x')$ is the character of the action of $T$ on the
			one-dimensional orbit.
			See [Corollary 5.12; VV03] or [Corollary A.5; RK03] for details.

		Example
			PP3 = projectiveSpace 3
			isWellDefined ampleKClass PP3 --the O(1) class on PP3 is well-defined
			badC = makeKClass(PP3, reverse gens PP3.characterRing) --reverse the order of Laurent polynomials defining the O(1) class
			isWellDefined badC --no longer well-defined

	Caveat
		A @TO MomentGraph@ must be defined on the @TO GKMVariety@ on which the @TO KClass@ is a $K$-class of.

	SeeAlso
		KClass
		makeKClass		


///

doc ///
	Key
		(symbol *, KClass, KClass)
	Headline
		computes the product of two equivariant K-classes
	Usage
		C1 * C2
	Inputs
		C1:KClass
		C2:KClass
	Outputs
		:KClass
			the product of C1 and C2
	Description
		Text
			This method computes the product of two equivariant $K$-classes.

		Example
			Gr24 = generalizedFlagVariety("A",3,{2}); --the Grassmannian of projective lines in projective 3-space
			O1 = ampleKClass Gr24 -- the O(1) bundle on Gr24 as an equivariant K-class
			O2 = O1 * O1
			peek O2

	SeeAlso
		makeKClass
		(symbol ^, KClass, ZZ)
		(symbol +, KClass, KClass)
///

doc ///
	Key
		(symbol ^, KClass, ZZ)
	Headline
		computes powers of an equivariant K-classes
	Usage
		C^n
	Inputs
		C:KClass
		n:ZZ
	Outputs
		:KClass
			the n-th power of C
	Description
		Text
			This method computes the $n$-th power of an equivariant $K$-class $C$.

		Example
			Gr24 = generalizedFlagVariety("A",3,{2}); --the Grassmannian of projective lines in projective 3-space
			O1 = ampleKClass Gr24 -- the O(1) bundle on Gr24 as an equivariant K-class
			O2 = O1^2
			peek O2
			Oneg1 = O1^(-1)
			peek Oneg1

	Caveat
		$n$ is allowed to be negative only when $C$ is a line bundle, or a direct sum of copies of a line bundle.

	SeeAlso
		makeKClass
		(symbol *, KClass, KClass)
		(symbol +, KClass, KClass)
///


doc ///
	Key
		(symbol +, KClass, KClass)
	Headline
		computes the sum of two equivariant K-classes
	Usage
		C1 + C2
	Inputs
		C1:KClass
		C2:KClass
	Outputs
		:KClass
			the sum of C1 and C2
	Description
		Text
			This method computes the sum of two equivariant $K$-classes.

		Example
			Gr24 = generalizedFlagVariety("A",3,{2}); --the Grassmannian of projective lines in projective 3-space
			O1 = ampleKClass Gr24 -- the O(1) bundle on Gr24 as an equivariant K-class
			E = O1 + (O1*O1)
			peek E

	SeeAlso
		makeKClass
		(symbol *, KClass, KClass)
///

doc ///
	Key
		generalizedFlagVariety
		(generalizedFlagVariety, String, ZZ, List)
		(generalizedFlagVariety, String, ZZ, List, Ring)
	Headline
		makes a generalized flag variety as a GKM variety
	Usage
		X = generalizedFlagVariety(LT,d,L)
		X = generalizedFlagVariety(LT,d,L,R)
	Inputs
		LT:String
			one of "A", "B", "C", or "D"
		d:ZZ
			the dimension of the root system
		L:List
			of integers strictly between 1 and d (inclusive)
		R:Ring
			the character ring of the torus acting on the generalized flag variety

	Outputs
		X:GKMVariety
			representing the corresponding generalized flag variety

	Description
		Text
			Let $G$ be the Lie group corresponding to $LT_d$, and
			let $w = a_1w_1 + \cdots + a_dw_d$ be a nonnegative $\mathbb Z$-linear combination of fundamental weights 
			in the root system of type $LT_d$, where $a_i$ is the number of times $i$ appears in the list $L$.
			(See @TO "Example: generalized flag varieties"@ for conventions regarding classical Lie groups and 
			their root systems).
			This method outputs the GKM variety representing the generalized flag variety $G/P$ embedded in the irreducible 
			representation of $G$ with the highest weight $w$.

		Text
			The following example features the Lagrangian Grassmannian $LGr(2,4)$ of 2-dimensional subspaces
			in $\mathbb C^4$ that are isotropic under the standard alternating form.  Its @TO MomentGraph@ is a complete 
			graph on 4 vertices.

		Example
			LGr24 = generalizedFlagVariety("C",2,{2})
			peek LGr24
			momentGraph LGr24
			euler ampleKClass LGr24

	Caveat
		Spin groups have not been implemented.

	SeeAlso
		"Example: generalized flag varieties"
		flagMap


///



doc ///
	Key
		EquivariantMap
	Headline
		the class of all equivariant morphisms between GKM varieties
	Description
		Text
			Given two GKM varieties $X$ and $Y$, an equivariant morphism from $X$ to $Y$
			induces a map from the torus-fixed points of $X$ to the torus-fixed points of $Y$.
			 			
		Text
			A @TO "EquivariantMap"@ C is a @TO "HashTable"@
			consisting of three keys:

			@UL{
			{TT "source", ", whose value is a ", TO "GKMVariety", " corresponding to the domain of f"},
			{TT "target", ", whose value is a ", TO "GKMVariety", " corresponding to the codomain of f"},
			{TT "ptsMap", ", whose value is a ", TO "HashTable", "; its keys are ", TT "X.points", " and the values are
			points of ", TT "Y.points", " that the key maps to."}
			}@
						
	SeeAlso
		(symbol **, EquivariantMap, EquivariantMap)
		(compose, EquivariantMap, EquivariantMap)
		(map, GKMVariety, GKMVariety, List)
		flagMap
		pullback
		pushforward
		(euler, KClass)
///


doc ///
	Key
		(symbol **, EquivariantMap, EquivariantMap)
	Headline
		computes the product of two equivariant morphisms
	Usage
		f ** g
	Inputs
		f:EquivariantMap
		g:EquivariantMap
	Outputs
		:EquivariantMap
			the product of f and g
	Description
		Text
			This method computes the cartesian product of two equivariant morphisms.

		Example
			R = makeCharacterRing 3;
			X = generalizedFlagVariety("A",2,{1,2},R);
			Y = generalizedFlagVariety("A",2,{1},R);
			f = flagMap(X,Y); -- the projection of Fl(1,2;3) onto Gr(2,3)
			h = f ** f
			peek h

	SeeAlso
		(map, GKMVariety, GKMVariety, List)
		flagMap
		(compose, EquivariantMap, EquivariantMap)
///



doc ///
	Key
		(compose, EquivariantMap, EquivariantMap)
	Headline
		computes the composition of two equivariant morphisms
	Usage
		compose(f,g)
	Inputs
		f:EquivariantMap
		g:EquivariantMap
	Outputs
		:EquivariantMap
			the composition of f and g
	Description
		Text
			This method computes the composition of two equivariant morphisms. The
			following example constructs the composition of two projection maps between
			standard flag varieties.

		Example
			R = makeCharacterRing 4;
			X = generalizedFlagVariety("A",3,{1,2,3},R);
			Y = generalizedFlagVariety("A",3,{2,3},R);
			Z = generalizedFlagVariety("A",3,{2},R);
			f = flagMap(X,Y); --the projection of Fl(1,2,3;4) onto Fl(2,3;4)
			g = flagMap(Y,Z); --the projection of Fl(2,3;4) onto Gr(2;4)
			h = compose(g,f)
			h === flagMap(X,Z)


	SeeAlso
		(symbol **, EquivariantMap, EquivariantMap)
		(map, GKMVariety, GKMVariety, List)
		flagMap
		
///





doc ///
	Key
		(map, GKMVariety, GKMVariety, List)
	Headline
		creates a EquivariantMap
	Usage
		f = map(X,Y,L)
	Inputs
		X:GKMVariety
			the source GKM variety of the map
		Y:GKMVariety
			the target GKM variety of the map
		L:List
			of pairs (x,y) where x and y are members of @TT "X.points"@ and @TT "Y.points"@, respectively
	Outputs
		f:EquivariantMap
	Description
		Text
			This method creates a @TO EquivariantMap@ given a GKM variety $X$, a GKM variety $Y$,
			and a list @TT "L"@ of pairs (x,y) where x and y are members of @TT "X.points"@ and @TT "Y.points"@ (respectively),
			indicating that the torus-fixed point x of X is sent to the torus-fixed point y of Y under the map.

		Text
			The following describes the projection from the third Hizerbruch surface to the projective 
			line.
			
		Example
			R = makeCharacterRing 2;
			F3 = makeGKMVariety(hirzebruchSurface 3,R);
			PP1 = projectiveSpace(1,R);
			L = {({0,1},set {0}), ({0,3}, set{0}), ({1,2}, set{1}), ({2,3}, set{1})};
			f = map(F3,PP1,L)
	Caveat
		This does not check that the morphism is well defined. In particular, it does not
		verify that the map on torus-fixed points is induced by a morphism of GKM varieties.						
	SeeAlso
		diagonalMap
		flagMap
		(pullback, EquivariantMap)
		pushforward
		(euler, KClass)
///

doc ///
	Key
		flagMap
		(flagMap, GKMVariety, GKMVariety)
	Headline
		creates equivariant maps between generalized flag varieties
	Usage
		f = flagMap(X,Y)
	Inputs
		X:GKMVariety
			the source generalized flag variety
		Y:GKMVariety
			the target generalized flag variety
	Outputs
		f:EquivariantMap
	Description
		Text
			Let $L =\{k_1,\dots,k_m\}$ be a set of ranks of linear subscpaces of $\mathbb C^n$ and consider
			a subset $L' \subseteq L$. Let $X = Fl(L; n)$ and $Y=Fl(L';n)$ be the associated generalized 
			flag varieties (if they exist). This method produces the canonical projection from $X$ to $Y$
			that forgets the linear subspaces having ranks $L \setminus L'$.
		Example
			R = makeCharacterRing 3
		   	X = generalizedFlagVariety("B",3,{1,2},R);
			Y1 = generalizedFlagVariety("B",3,{2},R);
			Y2 = generalizedFlagVariety("B",3,{1},R);
			peek flagMap(X,Y1)
			peek flagMap(X,Y2)
	SeeAlso
		(map, GKMVariety, GKMVariety, List)
		diagonalMap
		generalizedFlagVariety

///




doc ///
	Key
		(pullback, EquivariantMap)
	Headline
		computes the pullback map of equivariant K-classes of an equivariant map
	Usage
		pullback(f)
	Inputs
		f:EquivariantMap
	Outputs
		:FunctionClosure
			whose input is a @TO KClass@ on the target @TO GKMVariety@ of f and output is its pullback along f
	Description
		Text
			Given two GKM varieties $X$ and $Y$, this method computes the pullback of a @TO KClass@ on $Y$ 
			along an equivariant morphism $X \to Y$.

		Example
			R = makeCharacterRing 4;
			FlGr = generalizedFlagVariety("A",3,{1,2},R)
			Gr24 = generalizedFlagVariety("A",3,{2},R)
			f = flagMap(FlGr,Gr24)
			O1 = ampleKClass Gr24
			(pullback f)(O1)
			
	SeeAlso
		flagMap
		pushforward
///


doc ///
	Key
		pushforward
		(pushforward, EquivariantMap)
	Headline
		computes the pushforward map of equivariant K-classes of an equivariant map
	Usage
		pushforward(f)
	Inputs
		f:EquivariantMap
	Outputs
		:FunctionClosure
			whose input is a @TO KClass@ on the source @TO GKMVariety@ of f and output is its pushforward along f
	Description
		Text
			Given two GKM varieties $X$ and $Y$, this method computes the pushforward of a @TO KClass@ on $X$ 
			along an equivariant morphism $X \to Y$.

		Example
			R = makeCharacterRing 4;
			FlGr = generalizedFlagVariety("A",3,{1,2},R)
			Gr24 = generalizedFlagVariety("A",3,{2},R)
			f = flagMap(FlGr,Gr24)
			O1 = ampleKClass FlGr
			(pushforward f)(O1)

	SeeAlso
		flagMap
		(pullback, EquivariantMap)
		pushforward
		(euler, KClass)
///

doc ///
	Key
		(euler, KClass)
	Headline
		computes the equivariant Euler characteristic of an equivariant K-class
	Usage
		euler C
	Inputs
		C:KClass
	Outputs
		:RingElement
			in the character ring of the torus of the GKM variety on which C is defined
	Description
		Text
			This method computes the pushforward of a @TO KClass@ on a @TO GKMVariety@ $X$ along the structure map 
			$X \to pt$, where $pt$ is a point with trivial torus-action.
			
		Example
			PP3 = projectiveSpace 3
			O1 = ampleKClass PP3
			euler O1
	SeeAlso
		pushforward
		ampleKClass

///




doc ///
	Key
		projectiveSpace
		(projectiveSpace, ZZ)
		(projectiveSpace, ZZ, Ring) 
	Headline
		constructs projective space as a GKM variety
	Usage
		projectiveSpace n
		projectiveSpace(n,R)
	Inputs
		n:ZZ
		R:Ring
	Outputs
		:GKMVariety
	Description
		Text
			Given an integer $n$ this method constructs the n-dimensional projective space, $\mathbb P^n$, as a GKM variety. The action
			of $(\mathbb C^*)^{n+1}$ on $\mathbb P^n$ is defined by 
			$(t_0, \ldots, t_n) \cdot (x_0, \ldots, x_n) = (t_0^{-1}x_0, \ldots, t_n^{-1}x_n)$.

		Example
			PP4 = projectiveSpace 4;
			peek PP4
	SeeAlso
		flagMap
		generalizedFlagVariety
///



doc ///
	Key
		makeCharacterRing
		(makeCharacterRing, ZZ) 
	Headline
		constructs the character ring of a torus
	Usage
		makeCharacterRing n
	Inputs
		n:ZZ
	Outputs
		:Ring
			with n variables and inverses are allowed
	Description
		Text
			Given an integer n, this method outputs the character ring of T = $(\mathbb C^*)^n$.

		Example
			R = makeCharacterRing 4
			describe R

///


doc ///
	Key
		MomentGraph
	Headline
		the class of all moment graphs
	Description
		Text
			The moment graph of a GKM variety $X$ with an action of a torus $T$ has vertices
			corresponding to the $T$-fixed points $X^T$ 
			and edges corresponding to the one-dimensional $T$-orbits.  If $\{v_1,v_2\}$ is an edge and the 
			corresponding one-dimensional $T$-orbit closure is $\mathbb P^1$ where $v_1 = 0$ and $v_2 = \infty$, 
			then denote $m(v_1,v_2)$ to be the @EM "negative"@ of the character of the action of $T$ on 
			$\mathbb A^1 \subset \mathbb P^1$ (where $v_1 \in \mathbb A^1$).

		Text
			A @TO MomentGraph@ is a @TO HashTable@ with three keys:

			@UL{
			{TT "vertices", ", whose values represent the vertices of the moment graph"},
			{TT "edges", ", whose value is a ", TO "HashTable", "; its keys are pairs {a,b} of elements in ", 
			TT "vertices", " representing the edges of the moment graph, and the values are the characters ", TEX "$m(a,b)$"},
			{TT "HTpt", ", whose value is a ring representing the equivariant cohomology ring of a point"}
			}@

	Caveat
		Functionalities concerning intersection cohomology of sheaves on moment graphs, which had been 
		implemented before (see @HREF{"https://people.math.umass.edu/~braden/MG/index.html","MG: moment graph computations"}@),
		have not been imported into this package yet.

	SeeAlso
		momentGraph
		makeGKMVariety
		GKMVariety

///


doc ///
	Key
		momentGraph
		(momentGraph, List, HashTable, Ring)
	Headline
		creates a moment graph
	Usage
		G = momentGraph(L,E,H)
	Inputs
		L:List
			of vertices
		E:HashTable
			whose keys are lists of two vertices representing edges and values are characters of corresponding 
			1-dimensional orbits
		H:Ring
			a polynomial ring representing the equivariant cohomology ring of a point
	Outputs
		G:MomentGraph
	Description
		Text
			This method creates a @TO MomentGraph@ from the data of vertices, edges and their associated characters,
			and a ring representing the equivariant cohomology ring of a point (with trivial torus-action).
			The following example is the moment graph of the projective 2-space $\mathbb P^2$.

		Example
			V = {set{0}, set{1}, set{2}};
			E = hashTable {({set{0},set{1}},{-1,1,0}), ({set{0},set{2}},{-1,0,1}), ({set{1},set{2}},{0,-1,1})}
			t = symbol t; H = QQ[t_0..t_2]
			G = momentGraph(V,E,H)
			peek G
			underlyingGraph G

	SeeAlso
		MomentGraph
		(underlyingGraph, MomentGraph)
		(momentGraph, GKMVariety)

///

doc ///
	Key
		(momentGraph, GKMVariety)
	Headline
		view the moment graph of a GKM variety
	Usage
		G = momentGraph(X)
	Inputs
		X:GKMVariety
	Outputs
		G:MomentGraph
			if a moment graph is defined for the @TO GKMVariety@ X
	Description
		Text
			If a @TO MomentGraph@ has been defined for a @TO GKMVariety@ X, this method method returns the moment graph, 
			and returns error otherwise.
		Example
			momentGraph generalizedFlagVariety("A",3,{2})
	SeeAlso
		(momentGraph, GKMVariety, MomentGraph)
///

doc ///
	Key
		(momentGraph, GKMVariety, MomentGraph)
	Headline
		define a moment graph for a GKM variety
	Usage
		momentGraph(X,G)
	Inputs
		X:GKMVariety
		G:MomentGraph
	Outputs
		:null
	Description
		Text
			This methods sets a given @TO MomentGraph@ G to be the moment graph of a @TO GKMVariety@ X.
			If a moment graph was already defined for X, then overwrites it and prints that it has done so.
		Example
			R = makeCharacterRing 4
			X = makeGKMVariety({set{0},set{1},set{2},set{3}},R)
			X.?momentGraph
			PP3 = projectiveSpace 3
			G = momentGraph PP3
			momentGraph(X,G)
			X.?momentGraph
			momentGraph X
			momentGraph(X,G)
	SeeAlso
		(momentGraph, GKMVariety)
		(momentGraph, List, HashTable, Ring)

///

doc ///
	Key
		(underlyingGraph, MomentGraph)
	Headline
		the underlying (undirected) graph of a moment graph
	Usage
		underlyingGraph(G)
	Inputs
		G:MomentGraph
	Outputs
		:Graph
	Description
		Text
			This method outputs the underlying undirected @TO Graph@ of a moment graph.
		Example
			G = momentGraph projectiveSpace 3
			underlyingGraph G
	SeeAlso
		MomentGraph

///


doc ///
	Key
		(symbol **, MomentGraph, MomentGraph)
	Headline
		the product of two moment graphs
	Usage
		G ** H
	Inputs
		G:MomentGraph
		H:MomentGraph
	Outputs
		:MomentGraph
	Description
		Text
			Let $G$ and $H$ be moment graphs associated to the GKM varieties $X$ and $Y$, respectively. This function produces
			the moment graph of $X ** Y$; the latter is a GKM variety via the diagonal action of the torus.
		Example
			G = momentGraph projectiveSpace 1;
			H = momentGraph generalizedFlagVariety("C",2,{2}); -- The isotropic Grassmannian SpGr(2,4)
			J = G ** H;
			peek J
	SeeAlso
		MomentGraph
		(symbol **, GKMVariety, GKMVariety)

///



doc ///
	Key
		ampleKClass
		(ampleKClass, GKMVariety)
		(ampleKClass, GKMVariety, KClass)
	Headline
		the class of an ample line bundle
	Usage
		ampleKClass(X)
	Inputs
		X:GKMVariety
		C:KClass
	Outputs
		:KClass
	Description
		Text
			If $X$ is a GKM variety with a distinguished ample equivariant line bundle, this method returns the @TO KClass@ of 
			the line bundle. If no such line bundle is defined, it allows the user to construct one.
		Text
			The following example describes the ample line bundle on the Lagrangian Grassmannian $SpGr(2,4)$. The line bundle
			is precisely the pullback of O(1) under the Plücker embedding $SpGr(2,4) \to \mathbb P^4$.
		Example
			SpGr24 = generalizedFlagVariety("C",2,{2})
			O1 = ampleKClass SpGr24
			peek O1

	SeeAlso
		makeKClass
		generalizedFlagVariety
///


doc ///
	Key
		orbitClosure
		(orbitClosure, GKMVariety, Matrix)
	   	[orbitClosure, RREFMethod]
	Headline
		computes the equivariant K-class of a torus orbit closure of a point in a generalized flag variety
	Usage
		C = orbitClosure(X,M)
	Inputs
		X:GKMVariety
		M:Matrix
			representing a point in a generalized flag variety
	Outputs
		C:KClass
	Description
		Text
			Let $X$ be a generalized flag variety parameterizing flags of linear subspaces of dimensions $\{r_1, ... , r_k\}$ 
			in $\mathbb C^n$ with $1 <= r_1 < \cdots < r_k$. Then a point $p$ of $X$ can be identified with a matrix $M$ of 
			size  $r_k \times n$ such that the first $r_i$ rows of $M$ spans a subspace of dimension $r_i$. Given $X$ and
			such a matrix $M$ representing the point $p$, this method computes the equivariant K-class of the closure of
			the torus orbit of $p$. 
		

		Text
			The following example computes the torus orbit closure of a point in the standard Grassmannian $Gr(2,4)$
			and in the Lagrangian Grassmannian $SpGr(2,4)$. 

		Example
			M = matrix(QQ,{{1,0,1,2},{0,1,2,1}})
			X1 = generalizedFlagVariety("A",3,{2})
			X2 = generalizedFlagVariety("C",2,{2})
			C1 = orbitClosure(X1,M)
			C2 = orbitClosure(X2,M)
			peek C1
			peek C2	
		Text
			In type "A", the equivariant K-class of the orbit closure of a point coincides with that of its flag matroid.

		Example
			X = generalizedFlagVariety("A",3,{1,2})
			Mat = random(QQ^2,QQ^4)
			C = orbitClosure(X,Mat)
			FM = flagMatroid(Mat,{1,2})
			C' = makeKClass(X,FM)
			C === C'
		Text
			In type "D", the orthogonal Grassmannian $SOGr(n,2n)$ has two connected components. To compute the
			torus orbit closure of a point $p$ it suffices to restrict to either 
			$SOGr(n,n;2n)$ or $SOGr(n-1,n-1;2n)$, depending on which component $p$ is located in; see the last example
			in @TO "Example: generalized flag varieties"@ for more details. Here is an example with $n=4$:
		Example
			R = makeCharacterRing 4
			X1 = generalizedFlagVariety("D",4,{4,4},R)
			X2 = generalizedFlagVariety("D",4,{3,3},R)
			A = matrix{{1,3,-2,-1/4},{-1,-1,19,-61/4},{0,1,19,-73/4},{2,0,22,-89/4}};
			B = matrix(QQ,{{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,16}});
			M = A | B
			assert(A* transpose(B)  + B *transpose(A) == 0) -- verifying that M is isotropic
			C1 = orbitClosure(X1,M)
			C2 = orbitClosure(X2,M)
			peek C1 
			peek C2 -- since the point corresponding to M lies in X1, C2 is just the empty class i.e. 0
		Text
			By default the option RREFMethod is set to false. In this case the method produces the torus orbit closure by
			only computing the minors of the matrix. If the option RREFMethod is set to true, the method row reduces
			the matrix instead of computing its minors.
		Example
			X = generalizedFlagVariety("A",3,{1,2,3})
			Mat = random(QQ^3,QQ^4)
			time C = orbitClosure(X,Mat)	
			time C = orbitClosure(X,Mat, RREFMethod => true)
			
	SeeAlso
		generalizedFlagVariety
		(makeKClass, GKMVariety, FlagMatroid)
///


doc ///
	Key
		cellOrder
		(cellOrder, MomentGraph)
		(cellOrder, GKMVariety)
	Headline
		the poset of a stratification of a GKM variety
	Usage
		P = cellOrder(G)
		P = cellOrder(X)
	Inputs
		G:MomentGraph
			on which a cell order has been defined already
		X:GKMVariety
			whose moment graph has a cell order defined already
	Outputs
		P:Poset
	Description
		Text
			If a moment graph $G$ arises from a (possibly singular) GKM variety $X$ with an equivariant stratification,
			with each strata having a unique torus-fixed point, the vertices of $G$
			(which correspond to the torus-fixed point of $X$)
			form a poset where $v_1 \leq v_2$ if the closure of the stratum corresponding to $v_1$ contains that of $v_2$.
			The following example features the Schubert variety of projective lines in $\mathbb P^3$ meeting a distinguished line.
			The poset of its stratification by smaller Schubert cells is a subposet of the Bruhat poset.
		Example
			Gr24 = generalizedFlagVariety("A",3,{2})
			X = generalizedSchubertVariety(Gr24, {set{0,2}})
			cellOrder X

	SeeAlso
		(cellOrder, MomentGraph, Poset)
		bruhatOrder


///

doc ///
	Key
		(cellOrder, MomentGraph, Poset)
	Headline
		define a cell order on a moment graph
	Usage
		cellOrder(G,P)
	Inputs
		G:MomentGraph
		P:Poset
			whose ground set is the vertices of G
	Description
		Text
			Defines a @TO Poset@ $P$ to be a cell order on the @TO MomentGraph@ $G$.  Overwrites if there was 
			one already defined on $G$.
		CannedExample
			i2 : PP3 = projectiveSpace 3

			o2 = a GKM variety with an action of a 4-dimensional torus

			o2 : GKMVariety

			i3 : cellOrder PP3
			stdio:3:1:(3): error:  no cell order defined on this moment graph

			i4 : V = (momentGraph PP3).vertices

			o4 = {set {0}, set {1}, set {2}, set {3}}

			o4 : List

			i5 : P = poset(V, {{V_0,V_1},{V_1,V_2},{V_2,V_3}})

			o5 = P

			o5 : Poset

			i6 : cellOrder(momentGraph PP3, P)

			i7 : cellOrder PP3

			o7 = P

			o7 : Poset
	SeeAlso
		cellOrder
		bruhatOrder

///


doc ///
	Key
		bruhatOrder
		(bruhatOrder, GKMVariety)
	Headline
		computes the Bruhat order on a generalized flag variety
	Usage
		P = bruhatOrder(X)
	Inputs
		X:GKMVariety
			a generalized flag variety or Schubert variety
	Outputs
		P:Poset
			the Bruhat order
	Description
		Text
			Given a @TO GKMVariety@ $X$ created by either @TO generalizedFlagVariety@ or @TO generalizedSchubertVariety@, 
			computes and returns the Bruhat order corresponding to the Bruhat decomposition.  The resulting poset is 
			cached in $X$, and can be accessed by either @TO cellOrder@ or @TO bruhatOrder@.
		CannedExample
			i1 : Fl3 = generalizedFlagVariety("A",2,{1,2})

			o1 = a GKM variety with an action of a 3-dimensional torus

			o1 : GKMVariety

			i2 : cellOrder Fl3
			stdio:2:1:(3): error:  no cell order defined on this moment graph

			i2 : P = bruhatOrder Fl3

			o2 = P

			o2 : Poset

			i3 : #(coveringRelations P) == 8

			o3 = true

			i4 : cellOrder Fl3

			o4 = P

			o4 : Poset
	SeeAlso
		cellOrder
		generalizedFlagVariety
		generalizedSchubertVariety

///



doc ///
	Key
		generalizedSchubertVariety
		(generalizedSchubertVariety, GKMVariety, Thing)
	Headline
		create a generalized Schubert variety
	Usage
		Y = generalizedSchubertVariety(X,pt)
	Inputs
		X:GKMVariety
			obtained by @TO generalizedFlagVariety@
		pt:Thing
			an element in @TT "X.points"@
	Outputs
		Y:GKMVariety
			the generalized Schubert variety corresponding to pt which is the union of all Bruhat cells 
			corresponding to elements in @TT "X.points"@ that are bigger than pt in the Bruhat order
	Description
		Text
			This method creates a @TO GKMVariety@ that represent a generalized Schubert variety of a generalized flag variety.
			The following example is the Schubert variety of projective lines in $\mathbb P^3$ meeting a distinguished point.
		Example
			X = generalizedSchubertVariety(generalizedFlagVariety("A",3,{2}),{set{0,3}})

	SeeAlso
		bruhatOrder
		generalizedFlagVariety

///



doc ///
	Key
		charts
		(charts, GKMVariety)
		(charts, GKMVariety, List)
	Headline
		outputs the torus-invariant affine charts of a GKM variety
	Usage
		charts X
		charts(X,L)
	Inputs
		X:GKMVariety
		L:List
			of lists
	Outputs
		H:HashTable
			whose keys are the torus-fixed points of X and values are the (negatives) of characters of the 
			torus action on a torus-invariant affine chart around the corresponding point.
	Description
		Text
			Assume $X$ is a GKM-variety for which there exists a contracting torus-invariant affine chart around each
			torus-fixed point.  For instance, generalized flag varieties and smooth toric varieties have this property.
			This returns a @TO HashTable@ whose keys are the torus-fixed points of 
			$X$ and the values are the negatives of characters of the torus action on the associated 
			contracting affine chart.
			
		Text
			The following example describes the charts of the isotropic Grassmannian $SpGr(2,6)$.
		Example
			X = generalizedFlagVariety("C",3,{2});
			X.charts

		Text
		   If $X$ does not have its charts stored, we can manually cache it as follows.
		Example
		   R = makeCharacterRing 2;
		   X = makeGKMVariety({{0,1},{0,3},{1,2},{2,3}},R);
		   peek X
		   L = {{{-1,0},{0,-1}},{{-1,0},{0,1}},{{-3,-1},{1,0}},{{1,0},{3,1}}};
		   charts(X,L);
		   peek X
		   peek makeGKMVariety hirzebruchSurface 3

	SeeAlso
		makeGKMVariety
		generalizedFlagVariety

	
///


doc ///
	Key
		diagonalMap
		(diagonalMap, GKMVariety)
	Headline
		constructs the diagonal morphism
	Usage
		diagonalMap X
	Inputs
		X:GKMVariety
	Outputs
		:EquivariantMap
	Description
		Text
			Given a GKM variety $X$ this method constructs a @TO EquivariantMap@ representing the diagonal 
			morphism $X \to X \times X$. Note that $X \times X$ is a GKM variety via the diagonal action of the torus.

		Example
			X = generalizedFlagVariety("A",3,{2}); -- The Grassmannian Gr(2,4)
			f = diagonalMap X;
			peek f

	SeeAlso
		(map, GKMVariety, GKMVariety, List)
		
///

doc ///
	Key
		lieType
		(lieType, GKMVariety)
	Headline
		outputs the Lie type of a generalized flag variety
	Usage
		lieType X
	Inputs
		X:GKMVariety
	Outputs
		:String
			of the form "A", "B", "C" or "D"
	Description
		Text
			If $X$ is a generalized flag variety or a generalized Schubert variety constructed using the method
			@TO generalizedFlagVariety@ or @TO generalizedSchubertVariety@, this method outputs the Lie type of $X$. 

		Example
			X = generalizedFlagVariety("A",3,{2}); -- The Grassmannian Gr(2,4)
			Y = generalizedFlagVariety("B",2,{1}); -- The Orthogonal Grassmannian SOGr(1,5)
			lieType(X)
			lieType(Y)
		Text
			If the GKM variety is not a generalized flag variety or a generalized Schubert variety, prints error.
	Caveat
		The method @TO projectiveSpace@ dose not cache the Lie type.						
	SeeAlso
		generalizedFlagVariety
		
///


doc ///
	Key
		affineToricRing
		(affineToricRing, List)
		(affineToricRing, Matrix)
	Headline
		computes the toric ring associated to a monomial map
	Usage
		affineToricRing L
		affineToricRing M
	Inputs
		L:List
			of lists; each list corresponds to a vector in $\mathbb Z^n$
		M:Matrix
			with each column corresponding to a vector in $\mathbb Z^n$
	Outputs
		:Ring
	Description
		Text
			Given a list $\{v_1,...,v_d\}$ of vectors in $\mathbb Z^n$ this function computes
			the toric ring $R/I$ where $R$ is the polynomial ring $\mathbb{Q}[x_1,\dots,x_d]$
			with $x_i$ having degree $v_i$ and $I$ is the associated toric ideal. In particular
			$I$ is the kernel of the map $R \to \mathbb{Q}[y_1,\dots,y_n]$ defined by
			$x_i \mapsto \mathbb y^{v_i}$.
		Example
			L = {{2,0},{1,1},{0,2}};
			X = affineToricRing L; -- The singular quadric in A^3	 	
			I = ideal X
			hilbertSeries I  
		
///


doc ///
	Key
		trivialKClass
		(trivialKClass, GKMVariety)
	Headline
		the equivariant K-class of the structure sheaf
	Usage
		trivialKClass X
	Inputs
		X:GKMVariety
	Outputs
		:KClass
	Description
		Text
			Given a GKM variety $X$ this function computes the @TO KClass@ of the
			structure sheaf $O_X$. In terms of the localization map, $O_X$ corresponds
			to the constant function $1$.
		Example
			X = projectiveSpace 3;
			C = trivialKClass X;
			peek C
		
///

doc ///
	Key
		FlagMatroid
	Headline
		the class of all flag matroids
	Description
		Text
			A flag matroid $\mathbf M$ is an ordered list $\{M_1, \ldots, M_k\}$ of @TO Matroid@s on a common ground set 
			such that $M_i$ is a matroid quotient of $M_{i+1}$ for all $i=1, \ldots, k-1$.  The matroids $M_i$'s are called 
			the "constituent" matroids of the flag matroid $\mathbf M$.  The class @TO FlagMatroid@ is a @TO HashTable@ with 
			two keys:

			@UL{
			{TT "groundSet", ", whose value is a ", TO "Set", " representing the common ground set of the constituent matroids"},
			{TT "constituents", ", whose value is a ", TO "List", " of ", TO "Matroid", "s"}
			}@

	Caveat
		Flag matroids are the first examples beyond ordinary matroids of a more general combinatorial family 
		known as Coxeter matroids.  Coxeter matroids have not been implemented yet.
	SeeAlso
		(flagMatroid, List)
		(flagMatroid, Matrix, List)
		(makeKClass, GKMVariety, FlagMatroid)

///

doc ///
	Key
		flagMatroid
		(flagMatroid, List)
		(flagMatroid, Matrix, List)
	Headline
		construct a flag matroid
	Usage
		FM = flagMatroid(ML)
		FM = flagMatroid(A,L)
	Inputs
		ML:List
			of @TO Matroid@s on a common ground set
		A:Matrix
		L:List
			of integers between 1 and the number of rows of A (inclusive)
	Outputs
		FM:FlagMatroid
	Description
		Text
			Given a list $ML$ of matroids on a common ground set, this method stores the data as a @TO FlagMatroid@.
		Example
			ML = {uniformMatroid(2,6),matroid completeGraph 4}
			FM = flagMatroid(ML)
			isWellDefined FM

		Text
			For $A$ an $r\times n$ matrix over a field and $L = \{r_1, \ldots, r_k}$ a list of integers,
			let $M_i$ be the @TO Matroid@ defined by the columns of the matrix obtained by the first $r_i$ rows of $A$.
			These matroids form a flag matroid $\mathbf M = \{M_1, \ldots, M_k\}$.
			This method creates this @TO FlagMatroid@.
		Example
			A = random(QQ^2,QQ^4)
			FM = flagMatroid(A,{1,2})

	Caveat
		When a list of matroids is given as input, this method does not check if the flag matroid is well-defined.
	SeeAlso
		(isWellDefined, FlagMatroid)
		FlagMatroid

///



doc ///
	Key
		(isWellDefined, FlagMatroid)
	Headline
		check if a flag matroid is well-defined
	Usage
		isWellDefined(FM)
	Inputs
		FM:FlagMatroid
	Outputs
		:Boolean
	Description
		Text
			A @TO FlagMatroid@ with constituent matroids $\{M_1, \ldots, M_k\}$ is well-defined if $M_i$ is 
			a matroid quotient of $M_{i+1}$ (i.e. every flat of $M_i$ is a flat of $M_{i+1}$) for all $i = 1, \ldots, k-1$.
		Example
			FM = flagMatroid {uniformMatroid(2,4),uniformMatroid(3,4)}
			isWellDefined FM
			FMbad = flagMatroid {uniformMatroid(2,4), uniformMatroid(1,2)++uniformMatroid(2,2)}
			isWellDefined FMbad
	SeeAlso
		FlagMatroid
		(flagMatroid, List)

///


doc ///
	Key
		(bases, FlagMatroid)
	Headline
		compute the bases of a flag matroid
	Usage
		B = bases(FM)
	Inputs
		FM:FlagMatroid
	Outputs
		B:List
	Description
		Text
			An ordered list $\{B_1, \ldots, B_k\}$ of sets is a basis of a flag matroid $\mathbf M = \{M_1, \ldots, M_k\}$ 
			if $B_i$ is a basis of $M_i$ and $B_i \subseteq B_{i+1}$ for all $i$.  This method computes the bases of a 
			flag matroid.
		Example
			FM = flagMatroid {uniformMatroid(2,4),uniformMatroid(3,4)}
			bases FM
	SeeAlso
		FlagMatroid

///

doc ///
	Key
		(latticePoints, FlagMatroid)
	Headline
		lattice points of a base polytope of a flag matroid
	Usage
		P = latticePoints(FM)
	Inputs
		FM:FlagMatroid
	Outputs
		P:List
			of lists of integers representing the lattice points
	Description
		Text
			For a basis $B= \{B_1, \ldots, B_k\}$ of a flag matroid $M$ (see @TO (bases, FlagMatroid)@), 
			let $e_B$ be the sum over $i = 1, \ldots, k$ of the indicator vectors of $B_i$.
			The base polytope of a flag matroid $M$ 
			is the convex hull of $e_B$ as $B$ ranges over all bases of $M$.  This method computes the lattice points 
			of the base polytope of a flag matroid, exploiting the strong normality property as proven in [CDMS18]. 
		Example
			FM = flagMatroid {uniformMatroid(1,4),uniformMatroid(2,4)}
			P = latticePoints FM
		Text
			In terms of equivariant K-theory, the lattice points of the base polytope of a flag matroid is 
			equal to the integer-point transform of the equivariant Euler characteristic (see @TO (euler, KClass)@) of the @TO KClass@ 
			defined by the flag matroid shifted by the $O(1)$ bundle on the (partial) flag variety.
		Example
			X = generalizedFlagVariety("A",3,{1,2})
			FM = flagMatroid {uniformMatroid(1,4),uniformMatroid(2,4)}
			C = makeKClass(X,FM)
			chiCO1 = euler(C * ampleKClass X)
			set P === set exponents chiCO1
	SeeAlso
		(bases, FlagMatroid)
		(euler, KClass)

///

doc ///
	Key
		(makeKClass, GKMVariety, FlagMatroid)
	Headline
		the equivariant K-class of a flag matroid
	Usage
		C = makeKClass(X,FM)
	Inputs
		X:GKMVariety
			of @TO lieType@ "A" created by @TO generalizedFlagVariety@ 
		FM:FlagMatroid
	Outputs
		C:KClass
	Description
		Text
			A flag matroid of whose constituent matroids have ranks $r_1, \ldots, r_k$ and ground set size $n$ defines a 
			@TO KClass@ on the (partial) flag variety $Fl(r_1,\ldots, r_k;n)$.  When the flag matroid arises 
			from a matrix representing a point on the (partial) flag variety, this equivariant K-class coincides with 
			that of the structure sheaf of its torus orbit closure.
			See [CDMS18] or [DES20].
		Example
			X = generalizedFlagVariety("A",2,{1,2})
			A = matrix{{1,2,3},{0,2,3}}
			FM = flagMatroid(A,{1,2})
			C1 = makeKClass(X,FM)
			C2 = orbitClosure(X,A)
			C1 === C2

	SeeAlso
		(latticePoints, FlagMatroid)
		orbitClosure
		flagGeomTuttePolynomial

///

doc ///
	Key
		flagGeomTuttePolynomial
		(flagGeomTuttePolynomial, FlagMatroid)
	Headline
		computes the flag-geometric Tutte polynomial of flag matroids
	Usage
		flagGeomTuttePolynomial(FM)
	Inputs
		FM:FlagMatroid
	Outputs
		:RingElement
			a polynomial in variables $x,y$
	Description
		Text
			This method computes the flag-geometric Tutte polynomial of a @TO FlagMatroid@, defined via a push-pull of the 
			@TO KClass@ of the flag matroid.  See Definition 6.1 of [DES20].
			The following is the example 8.24 in [CDMS18].
		Example
			FM = flagMatroid {uniformMatroid(1,3),uniformMatroid(2,3)}
			flagGeomTuttePolynomial FM
		Text
			The following example negatively answers Conjecture 9.2 of [CDMS18], which had conjectured that all coefficients 
			of the flag-geometric Tutte polynomial of a flag matroid are nonnegative.
		CannedExample
			i1 : FM = flagMatroid {uniformMatroid(1,5),uniformMatroid(3,5)}

			o1 = a flag matroid with rank sequence {1, 3} on 5 elements 

			o1 : FlagMatroid

			i2 : flagGeomTuttePolynomial FM

			      3 4    3 3     2 4    3 2    2 3       4    3      2 2       3     4    3     2        2  
			o2 = x y  + x y  + 2x y  + x y  - x y  + 3x*y  + x y + 6x y  + 9x*y  + 4y  + x  + 3x y + 3x*y  +
			     ---------------------------------------------------------------------------------------------
			      3
			     y

			o2 : ZZ[x, y]
		Text
			Here is another counterexample but one where no constituent matroids have rank 1 or corank 1.
		CannedExample
			i1 : FM = flagMatroid {uniformMatroid(2,6),uniformMatroid(4,6)}

			o1 = a flag matroid with rank sequence {2, 4} on 6 elements 

			o1 : FlagMatroid

			i2 : time flagGeomTuttePolynomial FM -- used 691.322 seconds

			       4 4     4 3     3 4     4 2     3 3     2 4     4       3 2      2 3       4     4      3 
			o2 = x y  + 2x y  + 2x y  + 3x y  - 6x y  + 3x y  + 4x y + 18x y  + 18x y  + 4x*y  + 5x  + 14x y
			      --------------------------------------------------------------------------------------------
			           2 2        3     4     3     2        2     3
			      + 18x y  + 14x*y  + 5y  + 2x  + 6x y + 6x*y  + 2y

			o2 : ZZ[x, y]

		Text
			When the flag matroid has a single constituent (i.e. is a matroid), it agrees with the usual Tutte polynomial.
		Example
			M = matroid graph{{a,b},{b,c},{c,a},{a,d}}
			flagGeomTuttePolynomial flagMatroid {M}, tuttePolynomial M
	Caveat
		The computation often does not finish within a reasonable time (< 10 min) if the ground set is bigger than 5.
	SeeAlso
		FlagMatroid
		(makeKClass, GKMVariety, FlagMatroid)

///

doc ///
	Key
		setIndicator
		(setIndicator, Set, ZZ)
	Headline
		computes the signed indicator vector of an admissible set
	Usage
		setIndicator(T,n)
	Inputs
		T:Set
		n:ZZ
	Outputs
		:List
			corresponding to a vector in $\mathbb Z^n$
	Description
		Text
			Let $S$ be a set consisting of elements $s$, where $s$ is either equal to $i$ or $i^*$ with $0 \leq i \leq n-1$.
			The set $S$ is said to be admissible if for any integer $i$, not both $i$ and $i^{*}$ are contained in $S$.
			This method produces the signed indicator vector of $S$. In particular, the @TO "setIndicator"@ of $S$ is  
			$\sum c_ie_i \in \mathbb Z^n$ where $c_i = 1$ if $i \in T$, $c_i = -1$ if $i^{*} \in T$ and $0$ otherwise.
		Example
			S1 = set{1,2,4,5};
			S2 = set{1,"2*"};
			setIndicator(S1,7)
			setIndicator(S2,3)
		Text
			If the set is not admissible it produces an error.
		CannedExample
			i1 : S3 = set{1,"1*","2*",3}		

			o1 = set {1, 1*, 2*, 3}

			o1 : Set

			i2 : setIndicator(S3,4)
			stdio:2:1:(3): error:  the signed subset is not admissible 
	SeeAlso
		generalizedFlagVariety

///




undocumented {
	(net, GKMVariety),
	(net, MomentGraph),
	makeHTpt,
	characterRing,
	constituents,
	KPolynomials,
	HTpt,
	points,
	ptsMap,
	RREFMethod,
	tHilbNumer,
	toCharacterRing,
	toFraction,
	(toFraction, RingElement, RingElement, Ring),
	unastrsk,
	(net, KClass),
	(net, EquivariantMap),
	(net, FlagMatroid),
	signedPermutations
}










-*--documentation template
doc ///
	Key
	Headline
	Usage
	Inputs
	Outputs
	Description
		Text
			Blah
		Example
			X = 1
	Caveat
	SeeAlso

///
--*-

