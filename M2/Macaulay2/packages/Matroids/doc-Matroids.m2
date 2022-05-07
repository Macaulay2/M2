
beginDocumentation()

-- Documentation --
-- <<docTemplate
doc ///
	Key
		Matroids
	Headline
		a package for computations with matroids
	Description
		Text
			A matroid is a combinatorial structure abstracting the notion of 
			(linear algebraic, graph-theoretic) independence. This package 
			provides methods to perform computations with matroids in 
			Macaulay2.

			This package provides capabilities for converting between various 
			representations of matroids, creating linear and graphic 
			matroids from a matrix or graph, forming and detecting existence 
			of minors, computing Tutte polynomials, and additional 
			functions for applications of matroids to areas like optimization
			and convex geometry.

			Matroids are stored as pairs (E, B) of a ground set E and a list 
			B of bases, which are maximal independent subsets of the ground
			set. Internally, a ground set of size n is always identified with
			the set $\{0, ..., n-1\}$, and thus all subsets of the ground set
			(e.g. bases, circuits, flats) are also treated as subsets of 
			$\{0, ..., n-1\}$ (for more, cf. @TO groundSet@). However, the 
			actual elements of the ground set are allowed to be arbitrary 
			(e.g. integers, symbols, vectors, edges in a graph), and can be
			accessed by @TO2{(symbol _, Matroid, List), "taking subscripts"}@.
			
		Example
			M = matroid({a, matrix{{-1.2},{3.78}}, x, set{4,6}, -9}, {{a, x}, {x, -9}})
			peek M
			M_{0,1,4}
			peek restriction(M, set{1,2,3})
			circuits M
			netList flats M
			tuttePolynomial M
			
		Text
			A matroid can be specified by its bases, nonbases, circuits, 
			from a matrix, graph, or ideal, or via a collection of predefined
			matroids. For more on how to construct a matroid, see 
			@TO matroid@.

			{\bf Reference} Oxley, Matroid Theory, second edition. Oxford University Press, 2011.
		Text
			@SUBSECTION "Contributors"@
		Text
			The following people have generously contributed code, improved existing 
			code, or enhanced the documentation:
			Aaron Dall,
			Chris Eur,
			Matthew Mastroeni,
			Jason McCullough,
			Tianyi Zhang.
///

doc ///
	Key
		Matroid
	Headline
		the class of all matroids
	Description
		Text
			To see how to specify a matroid, see @TO matroid@.

			In this package, the ground set of the matroid is always 
			(internally) assumed to be a set of the form $\{0, ..., n-1\}$.
			This means that although the actual elements of the ground set
			can be arbitrary, all subsets of the ground set are specified by
			their indices, i.e. as a subset of $\{0, ..., n-1\}$ (this
			includes bases, circuits, flats, loops, etc.). 
			
			For convenience, the user can specify a subset of the ground set
			either by indices (which are integers between 0 and n-1), or as 
			actual elements. If indices are used, they should be given as a 
			@TO Set@, and if elements are used, they should be given as a 
			@TO List@.
			
			One can use the function @TO indicesOf@ to convert elements of
			the ground set to their indices. Conversely, use 
			@TO2{(symbol _, Matroid, List), "subscripts"}@
			to obtain the elements from their indices. 
			
			A recommended way to circumvent this distinction between indices 
			and elements is to make the elements of M equal to integers from 0 
			to n-1, in which case an element is equal to its index in M.groundSet.
			
			For more on this package-wide convention, see @TO groundSet@.
		
		Example
			U24 = uniformMatroid(2, 4)
			U24 == dual U24
			ideal U24
			peek U24
			tuttePolynomial U24
			N = U24 / {0}
			areIsomorphic(N, uniformMatroid(1, 3))
		Text
			
			Many computations performed in this package are 
			cached in order to speed up subsequent 
			calculations (as well as avoiding redundancy). 
			These include the @TO circuits@, @TO flats@, 
			@TO2{(ideal, Matroid), "ideal"}@,
			@TO2{(rank, Matroid, Set), "rank function"}@, and
			@TO2{(tuttePolynomial, Matroid), "Tutte polynomial"}@
			of a matroid, and are stored in the 
			@TO CacheTable@ of the matroid. Since the cache is
			a @TO MutableHashTable@, the user can also manually
			cache data (e.g. if it has been computed in a previous
			session), which can greatly speed up computation.
			
		Example
			R10 = specificMatroid "R10"
			keys R10.cache
			time isWellDefined R10
			time fVector R10
			keys R10.cache
			time fVector R10
///

doc ///
	Key
		matroid
		(matroid, List, List)
		(matroid, List)
		(matroid, Matrix)
		(matroid, Graph)
		(matroid, Ideal)
		(matroid, List, MonomialIdeal)
		Loops
		[matroid, Loops]
		ParallelEdges
		[matroid, ParallelEdges]
	Headline
		constructs a matroid
	Usage
		M = matroid(E, B)
		M = matroid(E, C, EntryMode => "circuits")
		M = matroid(B)
		M = matroid(A)
		M = matroid(G)
		M = matroid(I)
	Inputs
		E:List
			a ground set
		B:List
			a list of bases
		C:List
			a list of circuits
		A:Matrix
			whose column vectors form the ground set
		G:Graph
			whose edges form the ground set
		I:Ideal
			a squarefree monomial ideal defining the independence complex
	Outputs
		M:Matroid
	Description
		Text
			The default representation of a matroid in this package is by 
			its ground set and list of bases. 
			
		Example
			M = matroid({a,b,c,d}, {{a,b},{a,c}})
			peek M
		Text

			One can create a matroid by specifying its @TO circuits@ or 
			@TO nonbases@ instead, using the option
			@TO2{[matroid, EntryMode], "EntryMode"}@. 
			Regardless of the 
			value of EntryMode, the bases 
			are automatically computed in the process of creation.
			
		Example
			M = matroid({a,b,c,d},{}, EntryMode => "nonbases") -- defaults to uniform matroid of full rank
			peek M
			N = matroid({a,b,c,d}, {{b,c}}, EntryMode => "circuits")
			peek N
		Text	
		
			If no ground set is provided, the ground set is taken to be the 
			(sorted) union of the bases/nonbases/circuits. 
			
		Example
			M = matroid {{a,b},{a,c}}
			peek M
		Text

			If a matrix is provided, then the realizable matroid on the 
			columns of the matrix is returned. The ground set consists of
			columns of the matrix, and independence is determined by the
			method @TO rank@ (which allows flexibility over general rings
			understood by M2).
		
		Example
			M = matroid random(ZZ^3, ZZ^5)
			peek M
		Text
		
			If a graph is provided, then the graphic matroid is returned.
			The ground set consists of edges in the graph, and circuits 
			are precisely the (minimal) cycles in the graph. 
				
		Example
			M = matroid completeGraph 3
			peek M
		Text
		
			One can use the optional arguments Loops and ParallelEdges
			to specify loops and parallel edges for the graph, respectively
			(as the @TO Graphs@ package does not currently provide 
			functionality for loops or parallel edges). These options are
			intended only for use with graphic matroids. ParallelEdges should
			be given as a list of edges (which are two-element sets of the
			form set$\{i,j\}$ where i, j are vertices in G), and Loops should
			be given as a list of vertices where the loops are based.
			
		Example
			M = matroid(completeGraph 3, ParallelEdges => {set{0,1},set{0,1},set{1,2}}, Loops => {0,2})
			peek M
			circuits M
		Text
			
			If a squarefree monomial ideal is provided, corresponding to a 
			simplicial complex $\Delta$ via the Stanley-Reisner
			correspondence, then the matroid with 
			@TO2{(independenceComplex, Matroid), "independence complex"}@
			$\Delta$ is returned. The ground set consists of the variables in
			the ring of the ideal.
			
		Example
			R = QQ[x_0..x_4]
			I = monomialIdeal (x_0*x_1*x_3,x_1*x_2*x_4,x_0*x_2*x_3*x_4)
			M = matroid I
			peek M

	Caveat
		This function does not check if (E,B) defines a matroid - see 
		@TO2{(isWellDefined, Matroid), "isWellDefined"}@.
	
		The bases are not stored as sets of elements of M - rather, the 
		indices (with respect to the ground set) are stored instead. For more,
		see @TO groundSet@.
	SeeAlso
		(isWellDefined, Matroid)
		bases
		indicesOf
		specificMatroid
///

doc ///
	Key
		(symbol ==, Matroid, Matroid)
	Headline
		whether two matroids are equal
	Usage
		M == N
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Boolean
			whether the two matroids are equal
	Description
		Text
			Two matroids are considered equal if they have the same 
			set of (indexed) bases and same size grounds sets (in 
			particular, the ground sets need not be identical). This 
			happens iff the identity permutation is an isomorphism.

			The strong comparison operator === should not be used, as 
			bases (and ground sets) are internally stored as lists rather 
			than sets, so the same matroid with a different ordering on 
			the list of bases (or ground set) will be treated as different 
			under ===. (One might try to sort the list of bases, but 
			this is potentially time-consuming, as the list of bases can 
			grow rapidly with the size of the ground set.)
			
		Example
			M = matroid completeGraph 3
			peek M
			N = uniformMatroid(2, 3)
			peek N
			M == N
			M === N
			AG32 = specificMatroid "AG32" -- identically self-dual
			AG32 == dual AG32
			AG32 === dual AG32
			V = specificMatroid "vamos" -- self-dual, but not identically so
			V == dual V
			areIsomorphic(V, dual V)
	SeeAlso
		(isomorphism, Matroid, Matroid)
///

doc ///
	Key
		[matroid, EntryMode]
	Headline
		select method of specifying matroid
	Usage
		matroid(..., EntryMode => "bases")
		matroid(..., EntryMode => "nonbases")
		matroid(..., EntryMode => "circuits")
	Description
		Text
			A matroid is determined by its set of bases, i.e. maximal 
			(with respect to inclusion) independent sets, which are all 
			of the same size (namely, the rank of the matroid). However, 
			many interesting matroids have relatively few dependencies, 
			and thus it may be easier to specify the matroid by its 
			@TO nonbases@, i.e. dependent subsets of the ground set, with 
			size equal to the rank of the matroid.

			Similarly, a matroid can be specified by its @TO circuits@, i.e. 
			minimal dependent sets. This is done e.g. when creating 
			a graphical matroid.
			
			If EntryMode is not specified, then the default value is assumed, 
			which is EntryMode => "bases".
			
		Example
			M = matroid({{0,1,2}, {3,4,5}}, EntryMode => "circuits") -- bowtie graph / 2 disjoint K3's
			bases M
			F7 = matroid({{0,1,6},{0,2,4},{0,3,5},{1,2,5},{1,3,4},{2,3,6},{4,5,6}}, EntryMode => "nonbases")
			F7 == specificMatroid "fano"
	SeeAlso
		matroid
		nonbases
		circuits
///

doc ///
	Key
		(isWellDefined, Matroid)
	Headline
		whether the input is a well-defined matroid
	Usage
		isWellDefined M
	Inputs
		M:Matroid
	Outputs
		:Boolean
			whether or not a set of subsets satisfies the circuit elimination axiom
	Description
		Text
			If E is a set and C is a collection of subsets of E such that 
			(i) no two elements of C are comparable, and (ii): for C1, C2 in
			C and $e \in C1 \cap C2$, there exists $C3 \in  
			C$ with $C \subseteq (C1 \cup C2) - e$, then C is the
			set of circuits of a matroid on E. Property (ii) is called the
			circuit elimination axiom, and these characterize the collections
			of subsets of E which can be circuits for a matroid on E. This
			method verifies if the circuit elimination axiom 
			holds for the given input, and additionally whether the input has 
			the correct keys and data types that an object of type Matroid
			has.
			
		Example
			isWellDefined matroid({a,b,c,d},{{a,b},{c,d}})
			isWellDefined matroid({a,b,c,d},{{a,b},{a,c}})
			isWellDefined matroid({{1,2,3},{1,4,5},{2,3,4,5},{2,3,6,7},{4,5,6,7}}, EntryMode =>"circuits") -- the Escher "matroid"
			isWellDefined matroid({{1,2,3},{1,4,5},{1,6,7},{2,3,4,5},{2,3,6,7},{4,5,6,7}}, EntryMode =>"circuits")
			isWellDefined matroid random(ZZ^3, ZZ^5)
			isWellDefined matroid completeGraph 4
			isWellDefined uniformMatroid(4, 5)
		Text
		
			A theorem of Terai and Trung states that a monomial ideal 
			is the Stanley-Reisner ideal for (the independence complex of) 
			a matroid iff all symbolic powers is Cohen-Macaulay (indeed, 
			this happens iff the 3rd symbolic power is Cohen-Macaulay). 
			This can be verified as follows:
			
		Example
			R = QQ[x_0..x_3]
			I = monomialIdeal(x_0*x_1, x_0*x_2, x_3)
			isWellDefined matroid I
			symbolicCube = intersect apply(irreducibleDecomposition I, P -> P^3)
			(codim symbolicCube, pdim betti res symbolicCube)
			
///

doc ///
	Key
		groundSet
		(groundSet, Matroid)
	Headline
		(internal) ground set
	Usage
		groundSet M
		M.groundSet
	Inputs
		M:Matroid
	Outputs
		:Set
			of integers starting from 0
	Description
		Text
			Returns the internal representation of the ground set.
			
			Important: read the following if you encounter warnings/errors 
			when specifying subsets of a matroid (e.g. 
			restriction/deletion/contraction, ranks of subset, etc.)
			
			For a matroid M, there are 2 main differences between 
			M.groundSet and the elements of M (given by 
			@TO2{(symbol _*, Matroid), "M_*"}@). First is data 
			types: M.groundSet is a @TO Set@, and M_* is a @TO List@. 
			Second, M.groundSet always consists of integers from 0 to n-1, 
			where n is the number of elements of M; on the other hand,
			the elements of M themselves can be arbitrary (e.g. symbols, 
			matrices, edges in a graph, etc.).
			
			Thus, one can think of M.groundSet as the
			set of indices of the elements in the list M_*: the first
			element of M has index 0, corresponding to the 
			element 0 in M.groundSet; the second element of M 
			has index 1, etc. 
			
			The key point is that all sets associated to the structure of a 
			matroid - bases, circuits, flats, etc. - are subsets of 
			M.groundSet (not M_*). In particular, they are also of class
			@TO Set@ (although a collection of them is usually a @TO List@),
			and are also indexed from 0 to n-1. (An exception here is loops
			and coloops, which are given as a list of indices, rather than
			single-element sets).
			
			A recommended way to circumvent this distinction between indices 
			and elements is to use $\{0, ..., n-1\}$ as the actual elements 
			of M, in which case an element is equal to its index in
			M.groundSet. Most methods in this package will accept either a
			list of elements or a set of indices, and if the elements of M
			are $\{0, ..., n-1\}$, then functionally there will be no
			difference between inputting lists or sets.
			
			In summary: @TO2{List, "lists"}@ are used for elements in M, and 
			given as sublists of M_*, while @TO2{Set, "sets"}@ are used
			for indices, and given as subsets of M.groundSet.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			peek M
			M.groundSet
			M_*
			bases M
			(bases M)#0
			circuits M
			flats M
			loops M
			coloops M
		Text
			
			Note in particular the types of the various outputs above.
			
			The following illustrates how to perform operations with a 
			specified subset of M.groundSet. In the final example, a list of
			indices is given, which goes against the conventions above, but
			the elements of the list are treated (correctly) as indices, and
			if {\tt debugLevel} is greater than 0, then a warning is printed.
			
		Example
			N1 = M | {a,c,d}
			N2 = M | set{0,2,3}
			N1 == N2
			debugLevel = 1
			N3 = M | {0,2,3} -- gives a warning, but attempts to treat 0 as an index
			N3 == N2
	SeeAlso
		(symbol _, Matroid, List)
		indicesOf
		relabel
///

doc ///
	Key
		(symbol _, Matroid, List)
		(symbol _, Matroid, Set)
		(symbol _, Matroid, ZZ)
		(symbol _*, Matroid)
	Headline
		elements of matroid
	Usage
		M_S
		M_i
		M_*
	Inputs
		M:Matroid
		S:List
			or @TO2{Set, "set"}@, of indices in M.groundSet
	Outputs
		:List
			of elements of M
	Description
		Text
			Converts a list or set of indices to the list of elements of the 
			matroid with those indices. The inverse of this 
			function is @TO indicesOf@.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			M_2
			M_{0,2,3}
			B = (bases M)#0
			M_B
		Text
		
			If used with the operator @TO2{symbol _*, "_*"}@, then the 
			list of all elements in M is returned. This is useful in 
			conjunction with @TO apply@, to iterate over all elements in 
			a matroid.
			
		Example
			F7 = specificMatroid "fano"
			M4 = matroid completeGraph 4
			all(F7_*, x -> areIsomorphic(F7 \ {x}, M4))
	Caveat
		There are important differences between this method and
		@TO groundSet@: see that page for more details.
	SeeAlso
		groundSet
		indicesOf
///

doc ///
	Key
		indicesOf
		(indicesOf, List, List)
		(indicesOf, Matroid, List)
	Headline
		indices of a sublist
	Usage
		indicesOf(E, L)
		indicesOf(M, L)
	Inputs
		E:List
		M:Matroid
		L:List
			a list of sublists of E, or a sublist of M_*
	Outputs
		:List
			of indices
	Description
		Text
			This method has two typical-use cases. The first case is
			to convert sublists of a list E to their corresponding indices. 
			For technical reasons, the accepted input is a list L of 
			sublists of E, and the output is a list of sets of indices, 
			one set for each sublist in L. Note that the order of elements
			in the sublist is lost, when viewed as a set.
			
		Example
			indicesOf(toList(a..z) | toList(0..9), {{m,a,c,a,u,l,a,y,2},{i,s},{f,u,n}})
		Text
		
			The second case is with a matroid as input. Here the ambient 
			list E is taken as the ground set of the matroid 
			(i.e. E = M_*), and L should be a list of elements of M 
			(not a list of lists); in this case the inverse of this method 
			is given by @TO2{(symbol _, Matroid, List), "taking subscripts"}@
			with respect to M.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			B = {a, c}
			S = indicesOf(M, B)
			M_S == B
		Text
			
			In this case, if L is not a sublist of M_*, then a warning is 
			printed, and L itself is returned. This is done so that if a user 
			inputs a list of indices, then it will be interpreted as a set of 
			indices. For more on this, cf. @TO groundSet@.
	SeeAlso
		groundSet
		(symbol _, Matroid, List)
		relabel
///

doc ///
	Key
		(ideal, Matroid)
	Headline
		Stanley-Reisner (circuit) ideal of matroid
	Usage
		ideal M
	Inputs
		M:Matroid
	Outputs
		:MonomialIdeal
			the Stanley Reisner ideal of the independence complex,
			also called the circuit ideal
	Description
		Text
			The @TO2{(independentSets, Matroid), "independent sets"}@
			of a matroid M form a simplicial complex (i.e., are 
			downward closed), called the 
			@TO2{(independenceComplex, Matroid), "independence complex"}@ 
			of M. Via the Stanley-Reisner correspondence, the independence 
			complex of M corresponds uniquely to a squarefree monomial ideal, 
			which is the output of this method. 
			
			The minimal generators of the ideal correspond to minimal 
			non-faces of the simplicial complex. As the faces of the
			independence complex are precisely the independent sets, the
			minimal non-faces are exactly the minimal dependent sets, i.e.
			the @TO circuits@ of M.
			
			The facets of the simplicial complex correspond to @TO bases@ of 
			M, and thus also to irreducible components of the ideal of M;
			which are in bijection with the minimal generators of the
			Alexander dual ideal via taking complements.
			
			Internally, the ideal of the matroid is an important complete 
			invariant, and is heavily used in many algorithms in this package.
			Accordingly, once the ideal of a matroid is computed, it is 
			cached in the @TO CacheTable@ of the matroid, which speeds up any 
			algorithm which requires the ideal as part of the input.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			circuits M
			ideal M
			J = dual ideal M
			J_*/indices
			bases M
			betti res ideal matroid completeGraph 4
			
	SeeAlso
		circuits
		bases
		(independenceComplex, Matroid)
///

doc ///
	Key
		bases
		(bases, Matroid)
	Headline
		bases of matroid
	Usage
		bases M
	Inputs
		M:Matroid
	Outputs
		:List
			of bases
	Description
		Text
			Returns a list of bases of the matroid. The basis elements 
			are represented as sets of nonnegative integers, which are 
			the indices of the elements in the ground set that make up 
			a basis element. To get the subset of the ground set 
			corresponding to a set of indices, use 
			@TO2{(symbol _, Matroid, List), "subscripts"}@.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			bases M
			M_((bases M)#0)
		Text
		
			In this package, bases are the only structure that is always 
			computed upon creation of a matroid. Additional invariants 
			(such as the @TO2{(ideal, Matroid), "ideal"}@ or 
			@TO circuits@) are not precomputed, but are cached after 
			being computed once. This allows for maximum speed in 
			methods that need to call the 
			@TO2{matroid, "constructor function"}@ many, many times: 
			e.g. @TO2{(tuttePolynomial, Matroid), "tuttePolynomial"}@
			and @TO hasMinor@.
	SeeAlso
		nonbases
		(independentSets, Matroid)
		(symbol _, Matroid, List)
///

doc ///
	Key
		nonbases
		(nonbases, Matroid)
	Headline
		nonbases of matroid
	Usage
		nonbases M
	Inputs
		M:Matroid
	Outputs
		:List
			of nonbases
	Description
		Text
			In any matroid, all basis elements have the same cardinality 
			r (which is the rank of the matroid). The nonbases of a matroid 
			are the subsets of the ground set of cardinality r that are 
			dependent. 

			Just as with @TO bases@ and @TO circuits@, nonbases are 
			stored via their indices (rather than the elements of the ground 
			set themselves).
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			nonbases M
	SeeAlso
		bases
///

doc ///
	Key
		circuits
		(circuits, Matroid)
	Headline
		circuits of matroid
	Usage
		circuits M
	Inputs
		M:Matroid
	Outputs
		:List
			of circuits
	Description
		Text
			The circuits of a matroid are the minimal dependent 
			subsets of the ground set.

			Just as with @TO bases@ and @TO nonbases@, circuits 
			are stored via their indices (rather than the elements of the 
			ground set themselves).
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			circuits M
	SeeAlso
		(ideal, Matroid)
		fundamentalCircuit
		loops
///

doc ///
	Key
		fundamentalCircuit
		(fundamentalCircuit, Matroid, List, Thing)
		(fundamentalCircuit, Matroid, Set, ZZ)
	Headline
		fundamental circuit of independent set
	Usage
		fundamentalCircuit(M, I, e)
	Inputs
		M:Matroid
		I:Set
			of indices, or a @TO2{List, "list"}@ of elements in M, 
			which is an independent set
		e:ZZ
			an index, or an element in M, such that 
			$I \cup \{e\}$ is dependent
	Outputs
		:Set
			the fundamental circuit of e with respect to I
	Description
		Text
			If I is an independent set I, and e is an element such
			that $I \cup \{e\}$ is dependent (in particular
			e is not in I), then there is a unique circuit contained in 
			$I \cup \{e\}$, called the fundamental circuit of e 
			with respect to I, which moreover contains e.
			Every circuit is the fundamental circuit of some element 
			with respect to some basis.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			circuits M
			fundamentalCircuit(M, {a,c}, b)
			fundamentalCircuit(M, set{0,2}, 1)
			fundamentalCircuit(M, set{0,2}, 3)
		Text
			
			This method does not perform any checks (e.g. 
			whether $I$ is independent, or if $e$ is not in $I$). 
			If $I \cup \{e\}$ is independent, then 
			(if debugLevel is greater than 0) a warning is
			printed, and @TO null@ is returned. In the example below, 
			the elements with indices 2 and 3 are parallel (indeed, both 
			are equal to the column vector (1, 1)). Thus in general it is 
			safer to refer to a subset by its indices, rather than its elements.
			
		Example
			M = matroid matrix{{1,0,1,1},{0,1,1,1}}
			circuits M
			M_2
			M_2 == M_3
			(try fundamentalCircuit (M, M_{1,2}, M_3)) === null
			fundamentalCircuit (M, set{1,2}, 3)
	SeeAlso
		circuits
		(independentSets, Matroid)
		isDependent
///

doc ///
	Key
		loops
		(loops, Matroid)
	Headline
		loops of matroid
	Usage
		loops M
	Inputs
		M:Matroid
	Outputs
		:List
			the loops of M
	Description
		Text
			The loops of a matroid are the one-element @TO circuits@.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			loops M
			all(loops M, l -> member(set{l}, circuits M))
			loops(M/(set loops M)) == {}
	SeeAlso
		coloops
		circuits
///

doc ///
	Key
		coloops
		(coloops, Matroid)
	Headline
		coloops of matroid
	Usage
		coloops M
	Inputs
		M:Matroid
	Outputs
		:List
	Description
		Text
			The coloops of a matroid M are the loops of the 
			@TO2{(dual, Matroid), "dual"}@ matroid. 
			The set of coloops of M equals both the intersection of the 
			@TO bases@ of M, and the complement of the union of the 
			@TO circuits@ of M.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			circuits M
			C = set coloops M
			C === M.groundSet - fold(circuits M, (a, b) -> a + b)
			C === fold(bases M, (a, b) -> a*b)
			M_C
			D = dual M; peek D
			coloops matroid completeGraph 4 == {}
	SeeAlso
		loops
		(dual, Matroid)
///

doc ///
	Key
		(independentSets, Matroid, ZZ)
		(independentSets, Matroid)
	Headline
		independent subsets of a matroid
	Usage
		independentSets(M, s)
		independentSets M
	Inputs
		M:Matroid
		s:ZZ
	Outputs
		:List
			of independent sets in M
	Description
		Text
			A subset of the ground set is called independent if it is 
			contained in a @TO2{bases, "basis"}@, or equivalently, 
			does not contain a @TO2{circuits, "circuit"}@.
			This method returns certain independent subsets of the ground 
			set, depending on the input:
			
			If an integer $s$ is provided, then all independent subsets
			of size $s$ in $M$ are returned.
			
			If a subset $S$ of the ground set of $M$ is given, then all
			maximal independent subsets of $S$ are returned.
			
			If neither a size $s$ nor a subset $S$ is given, then all
			independent sets of M is returned (which may be quite large).
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			independentSets(M, 2)
			independentSets(M, set{1,2,3})
			netList independentSets M
			V = specificMatroid "vamos"
			I3 = independentSets(V, 3)
			#I3
	SeeAlso
		bases
		isDependent
		(independenceComplex, Matroid)
///

doc ///
	Key
		isDependent
		(isDependent, Matroid, Set)
		(isDependent, Matroid, List)
	Headline
		whether a subset is dependent
	Usage
		isDependent(M, S)
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
	Outputs
		:Boolean
			whether S is dependent in M
	Description
		Text
			This method checks if the given subset of the ground set
			is dependent, i.e. contains a @TO2{circuits, "circuit"}@.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			isDependent(M, {a,b})
			isDependent(M, {d})
	SeeAlso
		(independentSets, Matroid)
///

doc ///
	Key
		(rank, Matroid)
	Headline
		rank of a matroid
	Usage
		rank M
	Inputs
		M:Matroid
	Outputs
		:ZZ
	Description
		Text
			The rank of a matroid is the common size of a(ny) 
			basis of M. This is a basic numerical invariant of a matroid.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			rank M
	SeeAlso
		(rank, Matroid, Set)
///

doc ///
	Key
		(rank, Matroid, Set)
		(rank, Matroid, List)
	Headline
		rank of a subset of a matroid
	Usage
		rank(M, S)
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
	Outputs
		:ZZ
	Description
		Text
			The rank of a subset S of a matroid is the size of a maximal 
			independent subset of S. The map 2^E $\to \mathbb{N}$, 
			S $\mapsto$ rank(S), is called the rank function, and 
			completely determines the matroid.
			
			Once the rank of a given subset is computed, it is cached in the
			matroid (under {\tt M.cache#"ranks"}), so future computations of
			the rank of the same set are (essentially) instant.
			
			The user may choose to install a custom rank function for a 
			matroid (which should take in a list of integers (corresponding
			to a subset of @TO2{groundSet, "M.groundSet"}@), and output an
			integer), under {\tt M.cache#"rankFunction"}.
			This is done automatically when a matroid is constructed from 
			a matrix or graph, or with @TO setRepresentation@.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			for s in subsets M_* do print(toString s | " has rank " | rank_M s)
	SeeAlso
		(rank, Matroid)
///

doc ///
	Key
		closure
		(closure, Matroid, List)
		(closure, Matroid, Set)
	Headline
		closure of a subset of a matroid
	Usage
		closure(M, S)
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
	Outputs
		:List
	Description
		Text
			The closure of a subset S of a matroid (E, B) is the set 
			cl(S) := {x $\in$ E : rank(S) = rank(S $\cup \{x\}$) }.
			The closure operator 2^E -> 2^E, S $\mapsto$ cl(S), 
			completely determines the matroid (indeed, the maximal proper
			closed sets - i.e. @TO2{(hyperplanes, Matroid), "hyperplanes"}@ -
			already determine the matroid).
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			for s in subsets M_* do print(toString s | " has closure " | toString closure_M s)
			F = flats M
			all(F, f -> closure_M f === f)
	SeeAlso
		flats
///

doc ///
	Key
		flats
		(flats, Matroid)
		(flats, Matroid, ZZ)
		(flats, Matroid, ZZ, String)
	Headline
		flats of a matroid
	Usage
		flats M
		flats(M, r)
		flats(M, r, s)
	Inputs
		M:Matroid
		r:ZZ
			a target (co)rank (optional)
		s:String
			either "rank" or "corank"
	Outputs
		:List
	Description
		Text
			A flat, or closed subset, of a matroid is a subset A of 
			the ground set which equals its @TO closure@. The 
			set of flats, partially ordered by inclusion, forms a lattice, 
			called the @TO2{latticeOfFlats, "lattice of flats"}@.
			This is an important invariant of the matroid: one can
			recover the matroid from the lattice of flats, and for
			simple matroids (i.e. matroids whose circuits all have size
			>= 3), the isomorphism type of the lattice is already a
			complete invariant.
			
			If a target rank r is provided, then this method returns
			the list of all rank r flats of M.
			
			If a target corank r is provided along with the mode "corank", 
			then this method computes 
			all intersections of r distinct hyperplanes. This is guaranteed to 
			contain all flats of rank = rank M - r (cf. Oxley, Prop. 1.7.8), 
			and may be useful if the lattice of flats is large, and only the 
			upper portion is required (such as in the Scum theorem).
			
		Example
			M = uniformMatroid(4, 6)
			netList flats M
			flats(M, 1)
			flats(M, 2, "corank")
		
		Text
			In general, this method computes flats by iteratively 
			intersecting @TO2{(hyperplanes, Matroid), "hyperplanes"}@ 
			of M. Thus if hyperplanes of M have 
			been precomputed, then this function is typically much 
			faster.
			
		CannedExample
			i4 : M = matroid completeGraph 7

			o4 = a matroid of rank 6 on 21 elements
			
			o4 : Matroid
			
			i5 : time #hyperplanes M
			     ‐‐ used 4.98437 seconds
			
			o5 = 63
			
			i6 : time #flats M
			     ‐‐ used 0.515625 seconds
			
			o6 = 877
	SeeAlso
		closure
		(hyperplanes, Matroid)
		(fVector, Matroid)
		latticeOfFlats
///

doc ///
	Key
		hyperplanes
		(hyperplanes, Matroid)
	Headline
		hyperplanes of a matroid
	Usage
		hyperplanes M
	Inputs
		M:Matroid
	Outputs
		:List
	Description
		Text
			The hyperplanes of a matroid are the flats of rank 
			equal to rank M - 1. The complements of the hyperplanes 
			are precisely the @TO circuits@ of the 
			@TO2{(dual, Matroid), "dual"}@ matroid (which is indeed 
			how this method computes hyperplanes), and thus 
			a matroid is determined by its hyperplanes.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			hyperplanes M
	SeeAlso
		flats
		(rank, Matroid)
///

doc ///
	Key
		latticeOfFlats
		(latticeOfFlats, Matroid)
	Headline
		lattice of flats of a matroid
	Usage
		latticeOfFlats M
	Inputs
		M:Matroid
	Outputs
		:Poset
	Description
		Text
			The lattice of flats of a matroid M is the set of flats of M, 
			partially ordered by containment; i.e. $F1 \le F2$ if F1 is 
			contained in F2. The lattice of flats of a matroid is a geometric
			lattice: i.e. it is atomic (every element is a join of atoms =
			rank 1 elements) and semimodular ($h(x) + h(y) \ge h(x \vee y) + 
			h(x \wedge y)$ for any x, y, where h is the height function =
			maximum length of a chain from 0, and all maximal chains have 
			the same length). Conversely, every geometric lattice is the
			lattice of flats of a matroid.
			
			If M and N are @TO2{(isSimple, Matroid), "simple matroids"}@
			(i.e. no loops or parallel classes) with isomorphic lattice of 
			flats, then M and N are isomorphic.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			latticeOfFlats M
		Text
		
			One can also view the lattice of flats, using @TO displayPoset@
			provided by the @TO Posets@ package (the option
			@TO SuppressLabels@ may be useful).
	SeeAlso
		flats
		(rank, Matroid)
		(fVector, Matroid)
///

doc ///
	Key
		(fVector, Matroid)
	Headline
		f-vector of a matroid
	Usage
		fVector M
	Inputs
		M:Matroid
	Outputs
		:HashTable
	Description
		Text
			The f-vector of a matroid M is the invariant (f_0, f_1, ..., f_r),
			where f_i is the number of @TO2{(rank, Matroid, Set), "rank"}@ 
			i @TO flats@ of M, and r is the @TO2{(rank, Matroid), "rank"}@ 
			of M. Note that f_0 = f_r = 1, as the set of @TO loops@ 
			is the unique flat of rank 0, and the ground set is the unique
			flat of maximal rank.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			fVector M
			fVector matroid completeGraph 4
	Caveat
		This is not the same as the f-vector of the 
		@TO2{(independenceComplex, Matroid), "independence complex"}@
		of the matroid M, which counts the number of independent sets of 
		a given size. To do this instead, use "fVector independenceComplex M".
	SeeAlso
		flats
		(rank, Matroid)
		latticeOfFlats
///

doc ///
	Key
		(dual, Matroid)
	Headline
		dual matroid
	Usage
		dual M
	Inputs
		M:Matroid
	Outputs
		:Matroid
			the dual matroid of M
	Description
		Text
			The dual matroid of a matroid M has the same ground set 
			as M, and bases equal to the complements of bases of M.
			
			Duality is a fundamental operation in matroid theory: 
			for nearly any property/operation of matroids, there
			is a corresponding dual version, usually denoted with the
			prefix "co-". For instance, coloops are loops of the dual,
			and contraction is dual to deletion.
			
			In this package, every dual matroid is created as a 
			matroid-dual matroid pair, and each is cached as the dual
			of the other. Often the ideal of the dual matroid has a 
			significantly different number of generators, so many 
			algorithms in this package will use an equivalent check 
			for the ideal with fewer generators.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			D = dual M
			peek D
			M == dual D
			loops D == coloops M
			hyperplanes M === apply(circuits D, C -> D.groundSet - C)
		Text
			
			A matroid that is 
			@TO2{(areIsomorphic, Matroid, Matroid), "isomorphic"}@ 
			to its dual is called self-dual; and a matroid that is 
			@TO2{(symbol ==, Matroid, Matroid), "equal"}@ to its 
			dual is called identically self-dual.
			
		Example
			V8plus = specificMatroid "V8+"
			V8plus == dual V8plus
			V = relaxation(V8plus, set{4,5,6,7})
			V == dual V
			areIsomorphic(V, dual V)
		Text
		
			If a matroid has a @TO2{storedRepresentation, "representation"}@
			stored, then this function will attempt to automatically compute
			a representation for the dual (whether this works depends on 
			whether @TO reducedRowEchelonForm@ is implemented for the 
			underlying ring of the matrix).
			
		Example
			F7 = specificMatroid fano
			getRepresentation F7
			M = dual F7
			getRepresentation M
///

doc ///
	Key
		restriction
		(restriction, Matroid, Set)
		(restriction, Matroid, List)
		(symbol |, Matroid, Set)
		(symbol |, Matroid, List)
	Headline
		restriction to subset of matroid
	Usage
		restriction(M, S)
		M | S
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
	Outputs
		:Matroid
			the restriction M | S
	Description
		Text
			The restriction of M to S, M | S, has ground set S and 
			independent sets equal to the independent sets of M 
			contained in S.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			N = M | {a,b,d}
			peek N
			N == M | set{0,1,3}
	SeeAlso
		deletion
		minor
///

doc ///
	Key
		deletion
		(deletion, Matroid, Set)
		(deletion, Matroid, List)
		(symbol \, Matroid, Set)
		(symbol \, Matroid, List)
	Headline
		deletion of subset of matroid
	Usage
		deletion(M, S)
		M \ S
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
	Outputs
		:Matroid
			the deletion M \ S
	Description
		Text
			The deletion M \ S is obtained by restricting to the 
			complement of S.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			N = M \ {a}
			peek N
			N == M \ set{0}
	SeeAlso
		restriction
		contraction
		minor
///

doc ///
	Key
		contraction
		(contraction, Matroid, Set)
		(contraction, Matroid, List)
		(symbol /, Matroid, Set)
		(symbol /, Matroid, List)
	Headline
		contraction of subset of matroid
	Usage
		contraction(M, S)
		M / S
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
	Outputs
		:Matroid
			the contraction M / S
	Description
		Text
			The contraction of M by S is given by M/S := (M* \ S)*, 
			where * stands for dual, and \ is deletion.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			N = M / {c}
			peek N
			N == M / set{2}
	SeeAlso
		deletion
		(dual, Matroid)
		minor
///

doc ///
	Key
		minor
		(minor, Matroid, List, List)
		(minor, Matroid, Set, Set)
	Headline
		minor of matroid
	Usage
		minor(M, X, Y)
	Inputs
		M:Matroid
		X:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
		Y:Set
			of indices, or a @TO2{List, "list"}@ of elements in M, disjoint from X
	Outputs
		:Matroid
			the minor M / X \ Y
	Description
		Text
			The minor M / X \ Y of M is given by contracting X and 
			deleting Y from M. The resulting matroid is independent 
			of the order in which deletion and contraction is done. If 
			X (or Y) is a set (of indices in M.groundSet), then X is identified 
			with the sublist of elements of M with indices in X: 
			cf. @TO groundSet@ for more on this package-wide convention.
			
		Example
			M = matroid random(ZZ^3,ZZ^6)
			M_*
			M.groundSet
			(X, Y) = (set{3}, set{0,1})
			(X1, Y1) = (M_X, M_Y)/toList
			N = minor(M, X, Y)
			peek N
			N == minor(M, X1, Y1)
		Text	
			
			Note that there is potential ambiguity for the second argument - 
			namely, whether or not Y is treated with respect to the ground
			set of M or M / X (which are different). This method assumes that
			the indices of Y (and X) are taken with respect to the ground set
			of M. 
			
			If one already has the indices Y0 of Y in M / X (or the indices
			X0 of X in M \ Y), one can simply use the notation M 
			@TO2{contraction, "/"}@ X @TO2{deletion, "\\"}@ Y0 
			(or (M \ Y) / X0). Thus this method serves purely as a 
			convenience, to save the user the (trivial) task of computing Y0
			from Y.
			
			If X and Y are not disjoint, then an error is thrown (thus one
			should @TO2{(symbol -, Set, Set), "subtract"}@ X from Y
			beforehand).
			
		Example
			M5 = matroid completeGraph 5
			M5.groundSet
			N = minor(M5, set{8}, set{3,4,9})
			areIsomorphic(N, matroid completeGraph 4)
			N == (M5 \ set{3,4,9}) / set{6} -- after deleting 3,4 (and 9), index 8 -> 6
			N == M5 / set{8} \ set{3,4,8} -- after contracting 8, index 9 -> 8
			(try minor(M5, set{8}, set{3,4,8,9})) === null
			minor(M5, set{8}, set{3,4,8,9} - set{8})
	SeeAlso
		deletion
		contraction
		hasMinor
///

doc ///
	Key
		hasMinor
		(hasMinor, Matroid, Matroid)
		[hasMinor, Strategy]
	Headline
		whether a matroid has a given minor
	Usage
		hasMinor(M, N)
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Boolean
			whether N is a minor of M
	Description
		Text
			Determines if N is a minor of M, i.e. can be obtained 
			from M by a contraction followed by a deletion. Since 
			deletion and contraction by disjoint subsets commute, 
			every sequence of deletion and contraction operations 
			can be written as a single contraction and a single deletion.
			
			Many families of matroids can be defined by a 
			list of forbidden minors: i.e. a matroid M is in the family
			iff M does not have any of the forbidden minors as a minor. 
			For instance, a matroid is representable over F_2 iff it does
			not have U_{2,4} as a minor, i.e. U_{2,4} is the (sole)
			forbidden minor for @TO2{isBinary, "binary matroids"}@.
			
			There are 2 strategies for this method: the default (and 
			generally fastest) method is ``Strategy => "flats"'', which 
			can be sped up by precomputing the 
			@TO2{(fVector, Matroid), "fVector"}@ of M. Otherwise, all
			independent/coindependent sets of a certain size/rank are
			iterated over.
			
		Example
			(M4, M5, M6) = (4,5,6)/completeGraph/matroid
			hasMinor(M4, uniformMatroid(2,4))
			time hasMinor(M6, M5)
	SeeAlso
		minor
		isBinary
///

doc ///
	Key
		isBinary
		(isBinary, Matroid)
	Headline
		whether a matroid is representable over F_2
	Usage
		isBinary M
	Inputs
		M:Matroid
	Outputs
		:Boolean
			whether M is binary
	Description
		Text
			Determines if M is a binary matroid, i.e. is representable
			over the field $F_2$ of 2 elements.
			
			A matroid is representable over F_2 iff it does
			not have U_{2,4} as a minor. However, this method does
			not go through @TO hasMinor@, for efficiency reasons:
			rather it checks whether the symmetric difference of any 2
			distinct circuits is dependent.

			Note: in general, determining representability is a difficult
			computational problem. For instance, assuming access to 
			an independence oracle, it is known that the problem of 
			determining whether a matroid is binary cannot be solved 
			in polynomial time.
			
		Example
			M5 = matroid completeGraph 5
			isBinary M5
			U48 = uniformMatroid(4, 8)
			isBinary U48
	SeeAlso
		hasMinor
		getRepresentation
///

doc ///
	Key
		relaxation
		(relaxation, Matroid, Set)
		(relaxation, Matroid, List)
		(relaxation, Matroid)
	Headline
		relaxation of matroid
	Usage
		relaxation(M, S)
		relaxation M
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M,
			which is a circuit-hyperplane of M
	Outputs
		:Matroid
			the relaxation of M by S
	Description
		Text
			Let M = (E, B) be a matroid with bases B. If there is a 
			subset S of E that is both a @TO2{circuits, "circuit"}@ 
			and a @TO2{(hyperplanes, Matroid), "hyperplane"}@ of 
			M, then the set $B \cup \{S\}$ is the set of bases
			of a matroid on E, called the relaxation of M by S.
			
			If no set S is provided, then this function will take S 
			to be a random circuit-hyperplane (the first in lexicographic
			order). If no circuit-hyperplanes exist, then an error is 
			produced.
			
			Many interesting matroids arise as relaxations of other
			matroids: e.g. the non-Fano matroid is a relaxation of the
			@TO2{specificMatroid, "Fano matroid"}@, and the 
			non-Pappus matroid is a relaxation of the Pappus matroid.
			
		Example
			P = specificMatroid "pappus"
			NP = specificMatroid "nonpappus"
			NP == relaxation(P, set{6,7,8})
///

doc ///
	Key
		(symbol +, Matroid, Matroid)
	Headline
		union of matroids
	Usage
		M + N
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Matroid
			the sum, or union, of M and N
	Description
		Text
			The union of M and N has ground set equal to the 
			union of those of M and N, and independent sets given
			by pairwise unions of independent sets of M and N.
			
		Example
			M = uniformMatroid(2,4) + uniformMatroid(1,4)
			peek M
			M == uniformMatroid(3, 4)
		Text
			
			When the ground sets of M and N are disjoint, this is
			the @TO2{(symbol ++, Matroid, Matroid), "direct sum"}@
			of M and N. Beware of order when using 
			@TO2{(symbol ==, Matroid, Matroid), "=="}@ though:
			
		Example
			M0 = uniformMatroid(2, 4) + matroid completeGraph 4
			M0 == uniformMatroid(2, 4) ++ matroid completeGraph 4
			M1 = matroid completeGraph 4 ++ uniformMatroid(2, 4)
			M0 == M1
			areIsomorphic(M0, M1)
		Text
			
			Matroid union is an important operation in combinatorial
			optimization, and via duality, is related to the problem of
			matroid intersection.
			
			With the operation of union, one can work with transversal
			matroids and gammoids. A matroid is transversal iff it is a union
			of rank 1 matroids; strict gammoids are precisely the duals of
			transversal matroids, and gammoids are restrictions of strict
			gammoids. In general the problem of determining if a given
			matroid is a gammoid is difficult.
			
			A union of two uniform matroids is again uniform, but a union 
			of two graphic matroids need not be binary:
			
		Example
			M0 = matroid({a,b,c,d}, {{a},{b},{c}})
			M1 = matroid({a,b,c,d}, {{b},{c},{d}})
			M0 + M1 == uniformMatroid(2,4)
			F7 = specificMatroid "fano"
			NF = specificMatroid "nonfano"
			all({F7 + NF, F7 + F7, NF + NF}, M -> M == uniformMatroid(6, 7))
		Text
		
			One potential caveat: the ground set of M must not have repeated 
			elements. If this is not the case, the user MUST rename
			elements of M so that they become distinct. Of course, this needs
			to be done for both M and N, and one should also keep track of
			which elements of M and N are meant to be the same after the
			renaming (otherwise the entire point of taking unions, as
			opposed to direct sums, is lost).
			
			In the example below, M contains the vector {1,1} twice.
			Macaulay2 has no way of distinguishing the repeated vectors, so
			the second occurrence of {1,1} is relabelled to the symbol d (of
			course, if the symbol d also happened to be an element of N, then
			a different label would have to be chosen).
			
		Example
			A = matrix{{0,1,1,1},{0,0,1,1}}
			M = matroid A
			M_*
			unique M_*
			M0 = matroid(M_{0,1,2} | {d}, bases M)
			M == M0
			B = matrix{{0,1,2},{0,1,2}}
			N = matroid B
			U = M0 + N
			peek U
			U_*
	SeeAlso
		(symbol ++, Matroid, Matroid)
		relabel
///

doc ///
	Key
		(symbol ++, Matroid, Matroid)
	Headline
		direct sum of matroids
	Usage
		M ++ N
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Matroid
			the direct sum of M and N
	Description
		Text
			The direct sum of M and N is a matroid with ground set equal 
			to the disjoint union of the ground sets of M and N, and bases 
			equal to the union of bases of M and N.
			
		Example
			S = uniformMatroid(2,3) ++ uniformMatroid(1,3)
			peek S
			S_*
			(S ++ uniformMatroid(1, 3))_*
	Caveat
		The elements of the ground set of the direct sum will receive
		placeholders to ensure disjointness (as evidenced by the 
		elements of S being ordered pairs above). As this method is 
		binary, repeated applications of this function will result in nested 
		placeholders. Since the bases are stored as indices, the 
		bases of M will not change, but those of N will be shifted up by 
		the size of the ground set of M.
	SeeAlso
		(symbol +, Matroid, Matroid)
		(components, Matroid)
///

doc ///
	Key
		(components, Matroid)
	Headline
		connected components of matroid
	Usage
		components M
	Inputs
		M:Matroid
	Outputs
		:List
			the connected components of M
	Description
		Text
			Define an equivalence relation ~ on the ground set of M 
			by e ~ f if e = f or $\{e,f\}$ is contained in a circuit. The 
			equivalence classes under ~ are the connected components 
			of M. A matroid is the direct sum of its connected components.
			
		Example
			M = matroid graph({{0,1},{0,2},{1,2},{3,4},{4,5}})
			C = components M
			areIsomorphic(M, fold(C, (a, b) -> a ++ b))
			G = graph({{0,1},{0,2},{0,3},{0,4},{1,2},{3,4}})
			isConnected G
			components matroid G
	Caveat
		As the examples above show, the connected components of 
		the graphic matroid M(G) need not be the same as the connected 
		components of the graph G (indeed, for any graph G, there exists 
		a connected graph H with M(G) isomorphic to M(H)).
	SeeAlso
		circuits
		(symbol ++, Matroid, Matroid)
///

doc ///
	Key
		(isConnected, Matroid)
	Headline
		whether a matroid is connected
	Usage
		isConnected M
	Inputs
		M:Matroid
	Outputs
		:Boolean
			whether M is connected
	Description
		Text
			A matroid M is called connected if for every pair of distinct 
			elements f, g in M, there is a circuit containing both of them.
			This turns out to be equivalent to saying that there does not 
			exist an element e in M with rank({e}) + rank(M - {e}) = rank(M)
			(note that <= always holds by submodularity of the rank function).
			
			This method checks connectivity using the first definition above.
			The second definition generalizes to higher connectivity - cf.
			@TO is3Connected@. In the language of higher connectivity,
			a matroid is connected (in the sense of the two definitions above)
			if and only if it is 2-connected, i.e. has no 1-separation.
			
			To obtain the connected components of a matroid, use
			@TO2{(components, Matroid), "components"}@.
			
		Example
			M = matroid graph({{0,1},{0,2},{1,2},{3,4},{4,5}})
			isConnected M
			C = components M
			all(C, isConnected)
	SeeAlso
		(components, Matroid)
		is3Connected
///

doc ///
	Key
		is3Connected
		(is3Connected, Matroid)
	Headline
		whether a matroid is 3-connected
	Usage
		is3Connected M
	Inputs
		M:Matroid
	Outputs
		:Boolean
			whether M is 3-connected
	Description
		Text
			A matroid M is called m-connected if M has no k-separations for 
			k < m (see @TO getSeparation@ for the definition of a 
			k-separation). Thus a matroid is 3-connected if it has no 
			2-separations (or 1-separations). 
			
		Example
			U1 = uniformMatroid(1, 4)
			isConnected U1
			is3Connected U1
			is3Connected matroid completeMultipartiteGraph {3,3}
	SeeAlso
		(isConnected, Matroid)
		getSeparation
		sum2
///

doc ///
	Key
		getSeparation
		(getSeparation, Matroid, ZZ)
	Headline
		finds a k-separation of a matroid
	Usage
		getSeparation(M, k)
	Inputs
		M:Matroid
		k:ZZ
	Outputs
		:Set
			a k-separation of M, if one exists, or @TO null@ if none exists
	Description
		Text
			For a matroid M on a ground set E, and k >= 1,
			a (2-)partition (X, E - X) of E(M) is called a k-separation of M
			if |X| >= k, |E - X| >= k, and 
			rank(X) + rank(E - X) - rank(M) <= k-1.
			The separation is called minimal if either |X| = k or |E - X| = k.
			
			This method computes a k-separation of M, if one exists. 
			If no k-separation of M exists, then @TO null@ is returned.
			
			Efficiency is achieved by using special structure of 
			k-separations: if (X, E - X) is a minimal k-separation (and no 
			m-separation with m < k exists) with |X| = k, then X is either an
			independent cocircuit or a coindependent circuit. On the other
			hand, if (X, E - X) is a nonminimal separation with |E - X|
			minimal, then X is both a flat and a coflat. In particular, if
			the ranks of all flats have been previously computed (e.g. via 
			@TO2{(fVector, Matroid), "fVector"}@), then this method should
			finish quickly.
			
			For k = 1, it is generally more efficient to use 
			@TO2{(components, Matroid), "components"}@ and 
			@TO2{(isConnected, Matroid), "isConnected"}@ than this
			method.
			
		Example
			G = graph({{0,1},{1,2},{2,3},{3,4},{4,5},{5,6},{6,0},{0,2},{0,3},{0,4},{1,3},{3,5},{3,6}})
			M = matroid G
			getSeparation(M, 2)
	SeeAlso
		(isConnected, Matroid)
		is3Connected
///

doc ///
	Key
		seriesConnection
		(seriesConnection, Matroid, Matroid)
	Headline
		series connection of two matroids
	Usage
		seriesConnection(M, N)
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Matroid
			the series connection of M and N with basepoint 0
	Description
		Text
			This function returns the series connection of two given
			matroids M and N (cf. Oxley, Section 7.1).
			
			It is always assumed that the common basepoint
			of M and N is the first element in the respective ground sets,
			i.e. the element with index 0. (To form a series connection 
			using a different basepoint, one can first @TO relabel@ 
			M and/or N.)
			
			This method includes series extensions as a special case: 
			a series extension of M is a series connection of M with U(1,2).
			
		Example
			G = graph({{0,1},{1,2},{2,3},{3,4},{4,5},{5,6},{6,0},{0,2},{0,3},{0,4},{1,3},{3,5},{3,6}})
			M = matroid G
			seriesConnection(M, uniformMatroid(1,2))
	SeeAlso
		parallelConnection
		sum2
///

doc ///
	Key
		parallelConnection
		(parallelConnection, Matroid, Matroid)
	Headline
		parallel connection of two matroids
	Usage
		parallelConnection(M, N)
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Matroid
			the parallel connection of M and N with basepoint 0
	Description
		Text
			This function returns the parallel connection of two given
			matroids M and N (cf. Oxley, Section 7.1). Parallel 
			connection is dual to @TO seriesConnection@: namely, the parallel 
			connection of M and N is the dual of the series connection of 
			M* and N*.
			
			It is always assumed that the common basepoint
			of M and N is the first element in the respective ground sets,
			i.e. the element with index 0. (To form a parallel connection 
			using a different basepoint, one can first @TO relabel@ 
			M and/or N.)
			
		Example
			G = graph({{0,1},{1,2},{2,3},{3,4},{4,5},{5,6},{6,0},{0,2},{0,3},{0,4},{1,3},{3,5},{3,6}})
			M = matroid G
			parallelConnection(M, uniformMatroid(1,2))
	SeeAlso
		seriesConnection
		sum2
///

doc ///
	Key
		sum2
		(sum2, Matroid, Matroid)
	Headline
		2-sum of matroids
	Usage
		sum2(M, N)
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Matroid
			the 2-sum of M and N with basepoint 0
	Description
		Text
			This function returns the 2-sum of two given
			matroids M and N (cf. Oxley, Section 7.1).
			
			It is always assumed that the common basepoint
			of M and N is the first element in the respective ground sets,
			i.e. the element with index 0. (To form a 2-sum using a 
			different basepoint, one can first @TO relabel@ M and/or N.)
			Moreover, it is necessary that the basepoint 0 is not a 
			@TO2{loops, "loop"}@ or @TO2{coloops, "coloop"}@ in 
			either M or N. Under these assumptions,
			the 2-sum of M and N is equal to the @TO contraction@ of the 
			@TO seriesConnection@ of M and N by 0 (or alternatively, the
			deletion of the @TO parallelConnection@ of M and N by 0).
			
			The operation of 2-sum is important in higher matroid 
			connectivity: a @TO2{(isConnected, Matroid), "connected"}@
			matroid is @TO2{is3Connected, "3-connected"}@
			iff it cannot
			be expressed as a 2-sum of smaller matroids.
			
		Example
			M = sum2(specificMatroid "fano", uniformMatroid(2,4))
			isConnected M
			is3Connected M
	SeeAlso
		seriesConnection
		is3Connected
		getSeparation
		(isConnected, Matroid)
///

doc ///
	Key
		getRepresentation
		(getRepresentation, Matroid)
	Headline
		retrieves stored representation
	Usage
		getRepresentation M
	Inputs
		M:Matroid
	Outputs
		:Thing
			a representation of M (either a matrix or a graph)
	Description
		Text
			For a matroid created from a matrix, this function 
			provides a user-friendly way to access the original matrix.
			Similarly, for a matroid created from a (simple) graph,
			this function returns the graph used to create the matroid.
			
			Note that some constructions applied to a matroid M 
			(such as taking dual, or minors) will automatically compute 
			an induced representation (if a representation of M exists),
			which can be viewed with this function.
			
		Example
			A = random(QQ^3,QQ^5)
			M = matroid A
			A == getRepresentation M
			K4 = completeGraph 4
			M4 = matroid K4
			getRepresentation M4 === K4
			N = M / set{0}
			getRepresentation N
	SeeAlso
		setRepresentation
		matroid
///

doc ///
	Key
		setRepresentation
		(setRepresentation, Matroid, Matrix)
		storedRepresentation
	Headline
		stores user-defined representation
	Usage
		setRepresentation(M, A)
	Inputs
		M:Matroid
		A:Matrix
	Outputs
		:Matroid
			with a stored representation
	Description
		Text
			This function provides a way for the user to specify a 
			representation of a matroid (given by a matrix).
			The matrix is cached in the matroid 
			(under M.cache.storedRepresentation), and can be retrieved
			using @TO getRepresentation@.
			
			This function will also create a custom rank function for 
			the matroid, using the given representation (this is also
			cached in the matroid, under M.cache.rankFunction). 
			This can lead to faster computations of rank.
			
		Example
			M = uniformMatroid(4, 6)
			A = random(QQ^4,QQ^6)
			setRepresentation(M, A)
			getRepresentation M
			keys M.cache
			elapsedTime fVector M
	SeeAlso
		getRepresentation
		matroid
///

doc ///
	Key
		relabel
		(relabel, Matroid, HashTable)
		(relabel, Matroid, List)
		(relabel, Matroid)
	Headline
		relabel a matroid
	Usage
		relabel(M, H)
		relabel(M, L)
		relabel M
	Inputs
		M:Matroid
		H:HashTable
		L:List
	Outputs
		:Matroid
			a matroid isomorphic to M
	Description
		Text
			This method yields another matroid isomorphic to the input
			matroid M with labels specified by a hash table, or list of
			options. The keys of the hash table should be (a subset of) 
			$\{0, ..., n-1\}$ where n is the number of elements in the 
			@TO groundSet@ of M. Since any unused elements are assumed to
			remain unchanged, the set of values should coincide with the set
			of keys.
			
			Alternatively, the images of $\{0, ..., n-1\}$ (in order) can be
			specified (as a list, whose elements are not options). If neither
			a list nor hash table is provided, then a random permutation is
			chosen.
			
		Example
			Q6 = specificMatroid "Q6"
			M = relabel_Q6 {3 => 0, 0 => 3}
			(nonbases Q6, nonbases M)
			(M == Q6, areIsomorphic(M, Q6))
			nonbases relabel Q6
	SeeAlso
		(isomorphism, Matroid, Matroid)
		(areIsomorphic, Matroid, Matroid)
///

doc ///
	Key
		getIsos
		(getIsos, Matroid, Matroid)
	Headline
		all isomorphisms between two matroids
	Usage
		getIsos(M, N)
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:List
			of all isomorphisms between M and N
	Description
		Text
			This method computes all 
			@TO2{(areIsomorphic, Matroid, Matroid), "isomorphisms"}@ 
			between M and N: in particular, this method returns an 
			empty list iff M and N are not isomorphic. 
			
			To compute only a single isomorphism, use
			@TO2{(isomorphism, Matroid, Matroid), "isomorphism"}@.
			To test if two matroids are isomorphic, use
			@TO2{(areIsomorphic, Matroid, Matroid), "areIsomorphic"}@.
			
			To save space, the isomorphisms are given as lists (as opposed
			to hash tables). One way to interpret the output of this method 
			is: given two isomorphic matroids, this method returns a 
			permutation representation of the automorphism group of 
			that matroid, inside the symmetric group on the ground set.
			
		Example
			M = matroid({a,b,c},{{a,b},{a,c}})
			U23 = uniformMatroid(2,3)
			getIsos(M, U23) -- not isomorphic
			getIsos(M, M)
			getIsos(U23, U23) -- the full symmetric group S3
		Text
			
			We can verify that the Fano matroid (the projective plane 
			over the field of two elements) has automorphism group of 
			order 168, and give a permutation representation for this
			nonabelian simple group (= PGL(3, F_2)) inside the 
			symmetric group S_7:
			
		Example
			F7 = specificMatroid "fano"
			time autF7 = getIsos(F7, F7);
			#autF7
	SeeAlso
		(isomorphism, Matroid, Matroid)
		quickIsomorphismTest
		(areIsomorphic, Matroid, Matroid)
///

doc ///
	Key
		(isomorphism, Matroid, Matroid)
	Headline
		computes an isomorphism between isomorphic matroids
	Usage
		isomorphism(M, N)
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:HashTable
			an isomorphism between M and N
	Description
		Text
			This method computes a single
			@TO2{(areIsomorphic, Matroid, Matroid), "isomorphism"}@ 
			between M and N, if one exists, and returns @TO null@
			if no such isomorphism exists. 
			
			The output is a @TO HashTable@, where the keys are elements
			of the @TO groundSet@ of M, and their corresponding values 
			are elements of (the ground set of) N.
			
			To obtain all isomorphisms between two matroids, use
			@TO getIsos@.
			
		Example
			M = matroid({a,b,c},{{a,b},{a,c}})
			isomorphism(M, uniformMatroid(2,3)) -- not isomorphic
			(M5, M6) = (5,6)/completeGraph/matroid
			minorM6 = minor(M6, set{8}, set{4,5,6,7})
			time isomorphism(M5, minorM6)
			isomorphism(M5, M5)
			N = relabel M6
			time phi = isomorphism(N,M6)
	SeeAlso
		getIsos
		quickIsomorphismTest
		(areIsomorphic, Matroid, Matroid)
///

doc ///
	Key
		quickIsomorphismTest
		(quickIsomorphismTest, Matroid, Matroid)
	Headline
		quick checks for isomorphism between matroids
	Usage
		quickIsomorphismTest(M, N)
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:String
			either "true" or "false" or "Could be isomorphic"
	Description
		Text
			This method performs relatively quick tests to determine 
			whether or not two matroids are isomorphic. A result of "false"
			is definitive proof that the matroids are not isomorphic, a result
			of "true" is definitive proof that the matroids are isomorphic,
			and a result of "Could be isomorphic" is evidence that 
			the matroids may be isomorphic (although there are nonisomorphic
			matroids which cannot be detected by this method).
			
			If "true" or "false" is returned, use @TO value@ to convert to a
			@TO Boolean@.
			
		Example
			M0 = matroid(toList(a..z)/toString,{{"m","a","t","r","o","i","d"}})
			M1 = matroid(toList(0..25), {{random(ZZ),23,15,12,19,20,11}})
			quickIsomorphismTest(M0, M1)
			quickIsomorphismTest(matroid random(ZZ^5,ZZ^8), uniformMatroid(5, 8))
			quickIsomorphismTest(uniformMatroid(5, 9), uniformMatroid(4, 9))
			M0 = matroid graph({{a,b},{b,c},{c,d},{d,e},{e,f},{f,g},{f,h},{c,h},{c,f},{a,g},{d,g}})
			M1 = matroid graph({{a,b},{b,c},{c,d},{d,e},{e,f},{f,g},{f,h},{c,h},{c,f},{a,g},{a,h}})
			R = ZZ[x,y]; tuttePolynomial(M0, R) == tuttePolynomial(M1, R)
			time quickIsomorphismTest(M0, M1)
			value oo === false
	SeeAlso
		(isomorphism, Matroid, Matroid)
		getIsos
		(areIsomorphic, Matroid, Matroid)
		
///

doc ///
	Key
		(areIsomorphic, Matroid, Matroid)
	Headline
		whether two matroids are isomorphic
	Usage
		areIsomorphic(M, N)
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Boolean
			true if the matroids are isomorphic, false otherwise
	Description
		Text
			Two matroids are isomorphic if there is a bijection between 
			their ground sets which induces a bijection between bases,
			or equivalently, circuits (which is what this package actually
			checks, since there are often fewer circuits than bases). 
			
			This method first runs @TO quickIsomorphismTest@,
			then @TO2{(isomorphism, Matroid, Matroid), "isomorphism"}@ 
			if the tests are inconclusive.
			
		Example
			M = matroid({a,b,c},{{a,b},{a,c},{b,c}})
			areIsomorphic(M, uniformMatroid(2,3))
			M0 = matroid({a,b,c},{{a,b},{a,c}})
			areIsomorphic(M, M0)
	Caveat
		Isomorphism of matroids should not be confused with equality: cf.
		@TO2{(symbol ==, Matroid, Matroid), "=="}@ for more details.
	SeeAlso
		(isomorphism, Matroid, Matroid)
		getIsos
		quickIsomorphismTest
///

doc ///
	Key
		(tuttePolynomial, Matroid)
		(tuttePolynomial, Matroid, Ring)
	Headline
		Tutte polynomial of a matroid
	Usage
		tuttePolynomial M
	Inputs
		M:Matroid
	Outputs
		:RingElement
			the Tutte polynomial of M
	Description
		Text
			The Tutte polynomial is an invariant of a matroid that is 
			universal with respect to satisfying a deletion-contraction 
			recurrence. Indeed, one way to define the Tutte polynomial 
			of a matroid is: if $M$ is a matroid consisting of $a$ loops 
			and $b$ coloops, then $T_M(x, y) = x^ay^b$, and if 
			$e \in M$ is neither a loop nor a coloop, then 
			$T_M(x, y) := T_{M \backslash e}(x, y) + T_{M/e}(x, y)$, where 
			M\e is the @TO deletion@ of M with respect to $\{e\}$, and 
			M/e is the @TO contraction@ of M with respect to $\{e\}$. 
			Many invariants of a matroid can be determined by substituting
			values into its Tutte polynomial - cf. @TO tutteEvaluate@.
			
		Example
			tuttePolynomial matroid completeGraph 4
			tuttePolynomial specificMatroid "nonpappus"
	SeeAlso
		tutteEvaluate
		(characteristicPolynomial, Matroid)
		chromaticPolynomial
		deletion
		contraction
///

doc ///
	Key
		tutteEvaluate
		(tutteEvaluate, Matroid, Thing, Thing)
	Headline
		evaluate Tutte polynomial
	Usage
		tutteEvaluate(M, a, b)
	Inputs
		M:Matroid
		a:Thing
		b:Thing
	Outputs
		:Thing
			the evaluation of the Tutte polynomial of M at (a, b)
	Description
		Text
			Provides a user-friendly method to evaluate the Tutte 
			polynomial at given values (i.e. calls @TO sub@ with the 
			correct variables in the ring of the Tutte polynomial). 
			For example, if M has Tutte polynomial T, then 
			T(1,1) is the number of bases of M.
			
		Example
			M = uniformMatroid(2, 4)
			tutteEvaluate(M, 1, 1)
		Text
		
			If M = M(G) is the graphic matroid of a graph G, then 
			T(2, 1) counts the number of spanning forests of G,
			and T(2, 0) counts the number of acyclic orientations
			of G.
			
		Example
			M = matroid completeGraph 5
			tutteEvaluate(M, 2, 1)
			tutteEvaluate(M, 2, 0)
	SeeAlso
		(tuttePolynomial, Matroid)
///

doc ///
	Key
		(characteristicPolynomial, Matroid)
	Headline
		computes characteristic polynomial of a matroid
	Usage
		characteristicPolynomial M
	Inputs
		M:Matroid
	Outputs
		:RingElement
			the characteristic polynomial of M
	Description
		Text
			The characteristic polynomial is a particular specialization 
			of the Tutte polynomial. If M is a matroid of rank r with Tutte 
			polynomial T(x, y), then the characteristic polynomial of M 
			is given by (-1)^r * T(1 - x, 0).

			This function computes the characteristic polynomial as an 
			evaluation of the Tutte polynomial. If the Tutte polynomial of 
			the matroid has already been computed, then this function 
			should return the characteristic polynomial instantaneously.
			
		Example
			M = matroid completeGraph 4
			T = tuttePolynomial M
			factor characteristicPolynomial M
			
	Caveat
		If M = M(G) is a graphic matroid, then the characteristic polynomial 
		of M and the chromatic polynomial of G differ by a factor of x^k, 
		where k is the number of connected components of the graph G.
	SeeAlso
		(tuttePolynomial, Matroid)
		chromaticPolynomial
///

doc ///
	Key
		chromaticPolynomial
		(chromaticPolynomial, Graph)
	Headline
		computes chromatic polynomial of a graph
	Usage
		chromaticPolynomial G
	Inputs
		G:Graph
	Outputs
		:RingElement
			the chromatic polynomial of G
	Description
		Text
			The chromatic polynomial is an invariant of a graph 
			that counts the number of vertex colorings. The value 
			of this polynomial at a natural number n is the number 
			of ways to color the vertices of G using at most n colors, 
			such that no adjacent vertices have the same color. 

			This method computes the chromatic polynomial as a 
			multiple of the characteristic polynomial of the graphic 
			matroid. Indeed, if M = M(G) is the graphic matroid 
			corresponding to a graph G, then the chromatic 
			polynomial of G equals the characteristic polynomial of 
			M times x^k, where k is the number of connected 
			components of G (which is distinct from the number of 
			@TO2{(components, Matroid), "components"}@ of M).
			
		Example
			factor chromaticPolynomial cycleGraph 7
			factor characteristicPolynomial matroid cycleGraph 7
		Text
			
			The Four Color Theorem states that if G is a 
			planar graph, then its chromatic polynomial has 
			value > 0 at n = 4. In accordance with this, we see that
			K5 is not planar (on the other hand, note that K_{3,3} is 
			bipartite, hence 2-colorable):
		
		Example
			factor chromaticPolynomial completeGraph 5
	SeeAlso
		(tuttePolynomial, Matroid)
		(characteristicPolynomial, Matroid)
///

doc ///
	Key
		(isSimple, Matroid)
	Headline
		whether a matroid is simple
	Usage
		isSimple M
	Inputs
		M:Matroid
	Outputs
		:Boolean
			whether M is a simple matroid
	Description
		Text
			A matroid is simple if it has no 
			@TO loops@ or parallel classes;
			equivalently, it has no 
			@TO circuits@ of size <= 2.
			
			Among the class of simple matroids, the 
			@TO2{latticeOfFlats, "lattice of flats"}@
			is a complete invariant. Every matroid has 
			a unique @TO2{simpleMatroid, "simplification"}@
			which has the same lattice of flats.
			
		Example
			isSimple matroid completeGraph 3
			M = matroid(completeGraph 3, ParallelEdges => {set{0,1},set{0,1},set{1,2}}, Loops => {0,2})
			isSimple M
			S = simpleMatroid M
			isSimple S
			latticeOfFlats M == latticeOfFlats S
		Text
		
			Note that the @TO2{(dual, Matroid), "dual"}@ 
			of a simple matroid may not be simple:
			
		Example
			U = uniformMatroid(2, 2)
			isSimple U
			isSimple dual U
	SeeAlso
		simpleMatroid
///

doc ///
	Key
		simpleMatroid
		(simpleMatroid, Matroid)
	Headline
		simple matroid associated to a matroid
	Usage
		S = simpleMatroid M
	Inputs
		M:Matroid
	Outputs
		:Matroid
			the simple matroid associated to M
	Description
		Text
			The simple matroid associated to a matroid M
			is obtained from M by deleting all @TO loops@, 
			and all but one element from each parallel class.
			
			In a simple matroid, the 
			@TO2{latticeOfFlats, "lattice of flats"}@ has the
			empty set as minimal element, and all atoms
			are singletons.
			
		Example
			M = uniformMatroid(0, 2) ++ uniformMatroid(1, 2) ++ uniformMatroid(2, 4)
			isSimple M
			S = simpleMatroid M
			latticeOfFlats M == latticeOfFlats S
			select(flats S, f -> rank(S, f) <= 1)
			AG32 = affineGeometry(3, 2)
			
	SeeAlso
		(isSimple, Matroid)
///

doc ///
	Key
		getCycles
		(getCycles, Graph)
	Headline
		find cycles of graph
	Usage
		getCycles(G)
	Inputs
		G:Graph
	Outputs
		:List
			a list of (simple) cycles of G
	Description
		Text
			A cycle of G is a connected, 2-regular subgraph of 
			G (i.e. every vertex has degree 2). This method 
			returns all cycles of length at least 3, as ordered 
			lists of vertices (for cycles of length 1 or 2, see 
			the options Loops and ParallelEdges at @TO matroid@). 
			This method is used to create the graphic matroid:
			the output is in bijection with the circuits of the graphic 
			matroid (excluding loops and parallel edges).
			
		Example
			getCycles completeGraph 4
///

doc ///
	Key
		basisIndicatorMatrix
		(basisIndicatorMatrix, Matroid)
	Headline
		matrix of basis polytope
	Usage
		basisIndicatorMatrix M
	Inputs
		M:Matroid
	Outputs
		:Matrix
	Description
		Text
			The matroid (basis) polytope of a matroid on n elements lives in 
			R^n, and is the convex hull of the indicator vectors of the bases.
			
			For uniform matroids, the basis polytope is precisely the 
			hypersimplex:
		
		Example
			U24 = uniformMatroid(2, 4)
			A = basisIndicatorMatrix U24
		Text
		
			In order to obtain an actual polytope object in M2, one
			must take the convex hull of the columns of the indicator matrix,
			which is provided by either the Polyhedra or OldPolyhedra package:
			
		Example
			needsPackage "Polyhedra"
			P = convexHull A
			vertices P
		Text

			The Gelfand-Goresky-MacPherson-Serganova (GGMS) theorem
			characterizes which polytopes are basis polytopes for a matroid: 
			namely, each edge is of the form $e_i - e_j$ for some $i, j$,
			where $e_i$ are the standard basis vectors.
			
		-- Example
			-- M = matroid({{0,1},{0,2},{0,3},{1,2},{2,3}})
			-- n = #M.groundSet
			-- P = polytope M
			-- E = Polyhedra$faces(n - 2, P)/Polyhedra$vertices -- edges of P
			-- all(E, e -> sort flatten entries(e_0 - e_1) == ({-1} | toList(n-2:0) | {1})) -- GGMS criterion
	SeeAlso
		bases
		(independenceComplex, Matroid)
///

doc ///
	Key
		(independenceComplex, Matroid)
	Headline
		independence complex of matroid
	Usage
		independenceComplex M
	Inputs
		M:Matroid
	Outputs
		:SimplicialComplex
	Description
		Text
			The independence complex of a matroid is the simplicial 
			complex associated (via the Stanley-Reisner correspondence) 
			to the circuit ideal of the matroid (which is a squarefree 
			monomial ideal). This method uses the @TO SimplicialComplexes@
			package to return an object of type @TO SimplicialComplex@.
			
		Example
			M = matroid({{0,1},{0,2},{0,3},{1,2},{2,3}})
			independenceComplex M
	SeeAlso
		(independentSets, Matroid)
		(ideal, Matroid)
		(basisIndicatorMatrix, Matroid)
///

doc ///
	Key
		maxWeightBasis
		(maxWeightBasis, Matroid, List)
	Headline
		maximum weight basis using greedy algorithm
	Usage
		maxWeightBasis(M, w)
	Inputs
		M:Matroid
		w:List
			a weight function
	Outputs
		:Set
			a maximum-weight basis obtained by the greedy algorithm
	Description
		Text
			For a matroid M on ground set E, a weight function on M is 
			a function $w : E -> \mathbb{R}$, extended to all subsets of 
			E by setting $w(X) := \sum_{x\in X} w(x)$. The greedy 
			algorithm for finding a maximum-weight independent subset 
			of E starts with the empty set, and proceeds by successively 
			adding elements of E of maximum weight, which together with 
			the elements already added, form an independent set.

			In this method, a weight function is specified by its list of 
			values on E. Thus if $E = \{e_1, ..., e_n\}$, then w is
			represented as the list $\{w(e_1), ..., w(e_n)\}$.

			Matroids can be characterized via the greedy algorithm as follows:
			a set $\mathcal{I}$ of subsets of E is the set of independent
			sets of a matroid on E iff $\mathcal{I}$ is nonempty, downward
			closed, and for every weight function $w : E -> \mathbb{R}$, the
			greedy algorithm returns a maximal member of $\mathcal{I}$ of 
			maximum weight.
			
		Example
			M = matroid completeGraph 4
			bases M
			w1 = apply(M_*, e -> (toList e)#1)
			maxWeightBasis(M, w1)
			w2 = rsort w1
			maxWeightBasis(M, w2)
			
///

doc ///
	Key
		idealChowRing
		(idealChowRing, Matroid)
	Headline
		the defining ideal of the Chow ring
	Usage
		idealChowRing M
	Inputs
		M:Matroid
	Outputs
		:Ideal
			the defining ideal of the Chow ring of M
	Description
		Text
			The Chow ring of M is the ring R := QQ[x_F]/(I1 + I2), 
			where $I1 = (\sum_{i_1\in F} x_F - \sum_{i_2\in F} x_F : i_1, i_2 
			\in M)$ and $I2 = (x_Fx_{F'} : F, F' incomparable)$, 
			as $F$ runs over all proper nonempty flats of $M$. This is the 
			same as the Chow ring of the toric variety associated to the 
			Bergman fan of M. This ring is an Artinian standard graded 
			Gorenstein ring, by a result of Adiprasito, Katz, and Huh: cf. 
			https://arxiv.org/abs/1511.02888, Theorem 6.19.
			
			This method returns the defining ideal of the Chow ring, 
			which lives in a polynomial ring with variable indices equal to 
			the flats of M. To work with these subscripts, use 
			"last baseName v" to get the index of a variable v. For more 
			information, cf. @TO "Working with Chow rings of matroids"@.
			
		Example
			M = matroid completeGraph 4
			I = idealChowRing M
			basis comodule I
			(0..<rank M)/(i -> hilbertFunction(i, I))
			betti res minimalPresentation I
			apply(gens ring I, v -> last baseName v)
	SeeAlso
		latticeOfFlats
		cogeneratorChowRing
		"Working with Chow rings of matroids"
///

doc ///
	Key
		cogeneratorChowRing
		(cogeneratorChowRing, Matroid)
	Headline
		cogenerator of the Chow ring of a matroid
	Usage
		cogeneratorChowRing M
	Inputs
		M:Matroid
	Outputs
		:RingElement
			the dual socle generator of the Chow ring of M
	Description
		Text
			If R is an Artinian Gorenstein k-algebra, then the Macaulay 
			inverse system of R is generated by a single polynomial (in 
			dual/differential variables), called the cogenerator (or dual 
			socle generator) of R. By a result of Adiprasito, Katz, and 
			Huh, the Chow ring of a matroid M is always Gorenstein. 
			This function computes the cogenerator of the Chow ring of M, 
			which is also called the volume polynomial of M. Note that 
			this is a very fine invariant of M - indeed, this single 
			polynomial can recover the entire Chow ring of M, and thus most
			of the lattice of flats of M.
			
		Example
			M = matroid completeGraph 4
			I = idealChowRing M;
			betti I
			F = cogeneratorChowRing M
			T = ring F
			diff(gens((map(T, ring I, gens T)) I), F)
	SeeAlso
		latticeOfFlats
		idealChowRing
///

doc ///
	Key
		"Working with Chow rings of matroids"
	Description
		Text
			This documentation page contains various tips for 
			effectively working with Chow rings of matroids within
			this package. We take the graphic matroid of the 
			complete graph on 4 vertices as the running example:
			
		Example
			M = matroid completeGraph 4
			I = idealChowRing M;
		Text
		
			As seen from above, the output of @TO idealChowRing@
			is an @TO Ideal@, rather than a @TO Ring@. One can
			get the ambient polynomial ring, as well as the associated
			quotient ring:
			
		Example
			R = ring I
			S = R/I
		Text
		
			Next, one often wants to access and perform computations
			with elements in the quotient ring. The variables in the 
			ambient ring of the ideal of the Chow ring are indexed by 
			flats of the matroid, which retains useful information but 
			makes the variables themselves difficult to access. However,
			as with any ring in Macaulay2, one can always access variables
			using subscripts:
			-- using @TO2{(symbol _, Ring, ZZ), "subscripts"}@:
			
		Example
			R_0
			S_1
			S_5*S_6
		Text
			
			Notice that elements of $S$ are already rewritten in the 
			normal form modulo the ideal of the Chow ring. 
			
			One can access the flat corresponding to a given variable as 
			follows:
			
		Example
			R_7
			last baseName R_7
		Text
		
			It is also possible to access variables via their flats by 
			creating an auxiliary @TO HashTable@:
			
		Example
			chowVars = hashTable apply(#gens R, i -> last baseName R_i => S_i)
			chowVars#{5} * chowVars#{0,5}
	SeeAlso
		idealChowRing
///

doc ///
	Key
		uniformMatroid
		(uniformMatroid, ZZ, ZZ)
	Headline
		uniform matroid
	Usage
		U = uniformMatroid(k, n)
	Inputs
		k:ZZ
		n:ZZ
	Outputs
		:Matroid
			the uniform matroid of rank k on n elements
	Description
		Text
			The uniform matroid of rank k has as bases all 
			size k subsets. The ground set is $\{0, ..., n-1\}$.
			
		Example
			U35 = uniformMatroid(3,5)
			peek U35
	SeeAlso
		specificMatroid
///

doc ///
	Key
		affineGeometry
		(affineGeometry, ZZ, ZZ)
	Headline
		affine geometry of rank n+1 over F_p
	Usage
		M = affineGeometry(n, p)
	Inputs
		n:ZZ
			the dimension of the ambient vector space
		p:ZZ
			a prime
	Outputs
		:Matroid
			the affine geometry of rank n+1 over F_p
	Description
		Text
			The affine geometry of rank n+1 over F_p is the matroid
			whose ground set consists of all vectors in a vector 
			space over F_p of dimension n, where independence 
			is given by affine independence, i.e. vectors are dependent 
			iff there is a linear combination equaling zero in which 
			the coefficients sum to zero (equivalently, the vectors
			are placed in the hyperplane x_0 = 1 in a vector space
			of dimension n+1, with ordinary linear
			independence in the larger space). 
			
		Example
			M = affineGeometry(3, 2)
			M === specificMatroid "AG32"
			circuits M
			getRepresentation M
	SeeAlso
		projectiveGeometry
		specificMatroid
///

doc ///
	Key
		projectiveGeometry
		(projectiveGeometry, ZZ, ZZ)
	Headline
		projective geometry of dimension n over F_p
	Usage
		M = projectiveGeometry(n, p)
	Inputs
		n:ZZ
			the dimension of the projective space
		p:ZZ
			a prime
	Outputs
		:Matroid
			the projective geometry of dimension n over F_p
	Description
		Text
			The projective geometry of dimension n over F_p is the 
			matroid whose ground set consists of points in an 
			n-dimensional projective space over F_p. The matroid
			structure is precisely the 
			@TO2{simpleMatroid, "simple matroid"}@ associated
			to the realizable matroid of (F_p)^(n+1) (i.e. all vectors in an 
			(n+1)-dimensional vector space over F_p) - the origin
			(being a loop) has been removed, and a representative
			is chosen for all parallel classes (= lines).
			
			Note that projective space has a stratification into affine
			spaces (one of each smaller dimension). In particular,
			deleting any hyperplane from PG(n, p) gives AG(n, p).
			
		Example
			PG22 = projectiveGeometry(2, 2)
			PG22 == specificMatroid "fano"
			A = transpose sub(matrix toList(((3:0)..(3:2-1))/toList), ZZ/2) -- all vectors in (ZZ/2)^3
			areIsomorphic(PG22, simpleMatroid matroid A)
			PG32 = projectiveGeometry(3, 2)
			getRepresentation PG32
			H = first hyperplanes PG32
			areIsomorphic(affineGeometry(3, 2), PG32 \ H)
	SeeAlso
		affineGeometry
		specificMatroid
///

doc ///
	Key
		thetaMatroid
		(thetaMatroid, ZZ)
	Headline
		theta matroid
	Usage
		M = thetaMatroid n
	Inputs
		n:ZZ
	Outputs
		:Matroid
			the theta matroid
	Description
		Text
			The family of theta matroids appears in Oxley, p. 663.
			For a given n, thetaMatroid n has 2n elements and rank n.
			
			A notable feature of this family is that thetaMatroid n is 
			representable over a field iff the field has at least n - 1
			elements (a property also shared by uniformMatroid(2, n)).
			
		Example
			M = thetaMatroid 3
			areIsomorphic(M, matroid completeGraph 4)
	SeeAlso
		specificMatroid
///

doc ///
	Key
		spike
		(spike, ZZ)
		(spike, ZZ, List)
		binarySpike
		(binarySpike, ZZ)
	Headline
		spike matroid
	Usage
		M = spike r
		M = spike(r, L)
		M = binarySpike r
	Inputs
		r:ZZ
		L:List
			of circuits
	Outputs
		:Matroid
			a (tipped) r-spike
	Description
		Text
			The family of spikes appears in Oxley, p. 661-662.
			For a given r, every r-spike has 2r+1 elements and rank r.
			The ground set consists of a tip 0, and r legs {0, 1, 2}, 
			{0, 3, 4}, ..., {0, 2*r-1, 2*r} (each of which is a circuit).
			
			Deleting the tip 0 gives a matroid of rank r on 2r elements
			called a tipless r-spike.
			
			The optional input L should be a list of additional circuits,
			subject to certain additional conditions to be a spike.
			If no additional circuits are provided, then the spike is
			called free.
			
			Out of all possible r-spikes, there is a a unique one which is
			binary (i.e. representable over the field of 2 elements): 
			this is returned by the function @TO binarySpike@.
			
		Example
			M = binarySpike 5
			getRepresentation M
			N = M \ set{0}
			areIsomorphic(N, dual N)
			N1 = (spike 5) \ set{0}
			N1 == dual N1
	SeeAlso
		specificMatroid
///

doc ///
	Key
		swirl
		(swirl, ZZ)
	Headline
		swirl matroid
	Usage
		M = swirl r
	Inputs
		r:ZZ
	Outputs
		:Matroid
			a free swirl
	Description
		Text
			The family of swirls appears in Oxley, p. 664.
			The rank-r free swirl has 2r elements and rank r.
			
		Example
			areIsomorphic(swirl 3, uniformMatroid_3 6)
			M = swirl 4
			betti ideal M
			M == dual M
			getSeparation(M, 3)
	SeeAlso
		specificMatroid
///

doc ///
	Key
		wheel
		(wheel, ZZ)
		whirl
		(whirl, ZZ)
	Headline
		wheels/whirls
	Usage
		M = wheel r
		M = whirl r
	Inputs
		r:ZZ
	Outputs
		:Matroid
			a rank r wheel or whirl
	Description
		Text
			The families of wheels and whirls appears in Oxley, p. 659-660.
			The rank-r wheel is the graphic matroid of the 
			@TO2{wheelGraph, "wheel graph"}@ with r outer vertices, and has 
			2r elements.
			The rank-r whirl is the unique relaxation of the rank-r wheel
			(and thus also has 2r elements).
			
		Example
			M = wheel 3
			M == matroid wheelGraph 4
			N = whirl 3
			areIsomorphic(N, relaxation M)
	SeeAlso
		wheelGraph
		specificMatroid
///

doc ///
	Key
		specificMatroid
		(specificMatroid, String)
		(specificMatroid, Symbol)
	Headline
		creates built-in matroid
	Usage
		specificMatroid(S)
	Inputs
		S:String
			or symbol, the name of the matroid
	Outputs
		:Matroid
	Description
		Text
			Returns one of the named matroids below.
		Code
			UL {
				"U24",
				"C5",
				"P6",
				"Q6",
				"fano",
				"nonfano",
				"V8+",
				"vamos",
				"pappus",
				"nonpappus",
				"nondesargues",
				"betsyRoss",
				"AG32",
				"AG32'",
				"F8",
				"J",
				"L8",
				"O7",
				"P6",
				"P7",
				"P8",
				"P8=",
				"Q3(GF(3)*)",
				"Q6",
				"Q8",
				"R6",
				"R8",
				"R9",
				"R9A",
				"R9B",
				"R10",
				"R12",
				"S8",
				"S5612",
				"T8",
				"T12"
			}
		Text
			The matroids provided in this function (together with the
			infinite families given by the functions
			@TO affineGeometry@,
			@TO projectiveGeometry@,
			@TO binarySpike@,
			@TO spike@,
			@TO swirl@,
			@TO wheel@,
			@TO whirl@,
			@TO thetaMatroid@,
			@TO uniformMatroid@)
			includes all "interesting matroids" listed
			in Oxley, p. 639 - 664 (except for the general Dowling geometry).
		
			Many of these matroids are interesting for their 
			(non-)representability or duality properties:
		Code
			UL {
				"U24 is the uniform matroid of rank 2 on 4 elements, i.e. the
				4 point line, and is the unique forbidden minor for 
				representability over the field of 2 elements",
				"The Fano matroid F7 is the matroid of the projective plane 
				over F_2, and is representable only in characteristic 2. 
				The non-Fano matroid is a relaxation of F7, and is
				representable only in characteristic not equal to 2.",
				"The Pappus matroid is an illustration of Pappus' theorem.
				By the same token, the non-Pappus matroid is a relaxation
				which is not representable over any field.",
				"The Vamos matroid V, which is a relaxation of V8+, is the
				smallest (size) matroid which is not representable over any
				field - indeed, it is not even algebraic. 
				V8+ is identically self-dual, while V is isomorphic to its 
				dual.",
				"AG32 is the affine geometry corresponding to a
				3-dimensional vector space over F_2, and is identically 
				self-dual, with circuits equal to its hyperplanes. 
				A relaxation of AG32 is the smallest matroid not 
				representable over any field, with fewer basis elements 
				than V.",
				"R9A and R9B (along with their duals) are the only matroids 
				on <= 9 elements that are not representable over any field,
				although their foundations do not have $1$ as a fundamental
				element.",
				"R10 is a rank 5 matroid on 10 elements, which is the unique
				splitter for the class of regular matroids.",
				"The Betsy Ross matroid is a matroid which is representable
				over the Golden Mean partial field"
			}
		Example
			F7 = specificMatroid "fano"
			all(F7_*, x -> areIsomorphic(matroid completeGraph 4, F7 \ {x}))
			AG32 = specificMatroid "AG32"
			getRepresentation AG32
			AG32 == dual AG32
			R10 = specificMatroid "R10"
			getRepresentation R10
			areIsomorphic(R10 \ set{0}, matroid completeMultipartiteGraph {3,3})
	Caveat
		Notice that the ground set is a subset of $\{0, ..., n-1\}$
		rather than $\{1, ..., n\}$.
	SeeAlso
		matroid
		allMatroids
///

doc ///
	Key
		allMatroids
		(allMatroids, ZZ)
		(allMatroids, ZZ, ZZ)
	Headline
		returns all n-element matroids of rank r
	Usage
		allMatroids n
		allMatroids(n, r)
	Inputs
		n:ZZ
			the size of the ground set
		r:ZZ
			the target rank
	Outputs
		:List
			of matroids on n elements
	Description
		Text
			This method returns a list of matroids on n elements of rank r,
			for small n (currently, n <= 9). This list is complete for
			isomorphism types of rank r matroids on n elements, i.e. every
			matroid on n elements of rank r is 
			@TO2{(areIsomorphic, Matroid, Matroid), "isomorphic"}@ to a 
			unique matroid in this list.
			
			This function will silently switch inputs so that the rank r
			is the smaller of the two inputs (i.e. allMatroids(3,6) and 
			allMatroids(6,3) return the same output). If no rank r is
			provided, then all matroids on n elements are returned.
			
			One can perform many verifications using this method:
			
		CannedExample
			i1 : L = allMatroids 5; #L

			o2 = 38
			
			i3 : all(L, isWellDefined)
			
			o3 = true
			
			i4 : all(subsets(L, 2), S -> quickIsomorphismTest(S#0, S#1) == "false")
			
			o4 = true
			
			i5 : tally(L/fVector/values)
			
			o5 = Tally{{1, 1} => 5              }
				{1, 2, 1} => 6
				{1, 3, 1} => 4
				{1, 3, 3, 1} => 4
				{1, 4, 1} => 2
				{1, 4, 4, 1} => 3
				{1, 4, 6, 1} => 2
				{1, 4, 6, 4, 1} => 2
				{1, 5, 1} => 1
				{1, 5, 5, 1} => 1
				{1, 5, 6, 1} => 1
				{1, 5, 8, 1} => 1
				{1, 5, 8, 5, 1} => 1
				{1, 5, 10, 1} => 1
				{1, 5, 10, 7, 1} => 1
				{1, 5, 10, 10, 1} => 1
				{1, 5, 10, 10, 5, 1} => 1
				{1} => 1
			
			o5 : Tally
			
			i6 : smallMatroids = flatten apply(6, i -> allMatroids i); -- all matroids on < 6 elements
			
			i7 : #smallMatroids
			
			o7 = 70
///

doc ///
	Key
		allMinors
		(allMinors, Matroid, Matroid)
	Headline
		returns all minors of one matroid in another
	Usage
		allMinors(M, N)
	Inputs
		M:Matroid
			the ambient matroid
		N:Matroid
			the candidate minor
	Outputs
		:List
			of pairs (S, T), such that M / S \ T is isomorphic to N
	Description
		Text
			This method returns a list of all possible ways to realize N 
			as a minor of M. The output is a list of pairs (S, T) of subsets 
			of the ground set of M such that 
			@TO2{minor, "minor(M, S, T)"}@
			is isomorphic to N.
			
			In fact, S will be an independent subset of M, of size = 
			rank M - rank N, and T will be a coindependent subset of
			M, of size = #((M/S).groundSet) - #N.groundSet, which is 
			disjoint from S.
			
			The output of this method should be the empty list iff the 
			output of @TO hasMinor@ is false (for the same input).
			
		Example
			V = specificMatroid "vamos"
			U25 = uniformMatroid(2,5)
			elapsedTime L = allMinors(V, U25);
			#L
			netList L_{0..4}
			all(L, pair -> areIsomorphic(U25, minor(V, pair#0, pair#1)))
	SeeAlso
		minor
		hasMinor
///

doc ///
	Key
		toSageMatroid
		(toSageMatroid, Matroid)
		fromSageMatroid
		(fromSageMatroid, String)
	Headline
		Sage format for matroid
	Usage
		toSageMatroid M
	Inputs
		M:Matroid
	Outputs
		:String
	Description
		Text
			This method returns a string which can be recognized as a 
			matroid in the Sage matroids package. 
			
			Currently this function is purely for demonstrative purposes,
			using lowercase alphabet letters for the ground set (thus limited
			to matroids on <= 26 elements), and using a basis description.
			
			The inverse functionality is provided by the function 
			fromSageMatroid.
			
		Example
			V = specificMatroid "vamos"
			s = toSageMatroid V
			class s
			fromSageMatroid s === V
///

undocumented {
	(net, Matroid),
	(independentSets, Matroid, Set),
	(independentSets, Matroid, List)
}
