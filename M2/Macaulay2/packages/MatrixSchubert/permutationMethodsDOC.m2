doc ///
    Key
        isPerm
        (isPerm, List)
    Headline
        whether a list is a permutation in 1-line notation
    Usage
        isPerm w
    Inputs
        w:List
    Description
        Text
            Given a list of length $n$, checks if the entries of the permutation are the integers from 1 to $n$.
        Example
            w = {5,3,4,6,1,2}
            isPerm w

            v = {4,3,2}
            isPerm v
///

doc ///
    Key
        permToMatrix
        (permToMatrix, List)
    Headline
       converts a permutation in 1-line notation into a permutation matrix
    Usage
        permToMatrix w
    Inputs
        w:List
    Description
        Text
            Given a permutation in 1-line notation, produces the permutation matrix with 1's in location $(i,w_i)$.
        Example
            w = {7,2,5,8,1,3,6,4}
            permToMatrix w

            v = {1,6,9,2,4,7,3,5,8}
            permToMatrix v
///

doc ///
    Key
        lastDescent
        (lastDescent, List)
    Headline
       finds the location of the last descent of a permutation
    Usage
        lastDescent w
    Inputs
        w:List
    Description
        Text
            Given a non-identity permutation in 1-line notation, finds the location of its last descent, i.e., the greatest $i$ so that $w_{i+1}<w_i$.
        Example
            w = {7,2,5,8,1,3,6,4}
            lastDescent w

            v = {1,6,9,2,4,7,3,5,8}
            lastDescent v
///

doc ///
    Key
        firstDescent
        (firstDescent, List)
    Headline
       finds the location of the first descent of a permutation
    Usage
        firstDescent w
    Inputs
        w:List
    Description
        Text
            Given a non-identity permutation in 1-line notation, finds the location of its first descent, i.e., the least $i$ so that $w_{i+1}<w_i$.
        Example
            w = {7,2,5,8,1,3,6,4}
            firstDescent w

            v = {1,6,9,2,4,7,3,5,8}
            firstDescent v
///

doc ///
    Key
        descentSet
        (descentSet, List)
    Headline
       the descent set of a permutation
    Usage
        descentSet w
    Inputs
        w:List
    Description
        Text
            Given a permutation in 1-line notation, lists the location of its descents, i.e., the least $i$ so that $w_{i+1}<w_i$.
        Example
            w = {7,2,5,8,1,3,6,4}
            descentSet w
///

doc ///
    Key
        permLength
	(permLength, List)
    Headline
    	to find the length of a permutation in 1-line notation.
    Usage
        permLength w
    Inputs
    	w:List
    Description
    	Text
	 Given a permutation in 1-line notation returns the Coxeter length of the permutation.
	Example
    	    w = {2,5,4,1,3}
	    permLength w

	    
///

doc ///
    Key
        inverseOf
	(inverseOf, List)
    Headline
    	to return the inverse of a permutation in 1-line notation.
    Usage
        inverseOf w
    Inputs
    	w:List
    Description
    	Text
	 Given a permutation in 1-line notation returns the inverse of the permutation in 1-line notation.
	Example
    	 w = {2,5,4,1,3}
	 inverseOf w
///

doc ///
    Key
        longestPerm
	(longestPerm, ZZ)
    Headline
    	to return the longest permutation of length n
    Usage
        longestPerm n
    Inputs
    	n:ZZ
    Description
    	Text
	    Given an integer $n$, returns the permutation $\{n,n-1,...2,1\}$.
	Example
    	    longestPerm 7
///

-*
doc ///
    Key
	(getOneReducedWord, List)
        getOneReducedWord
    Headline
        given a permutation in 1-line notation, finds one reduced word
    Usage
        getOneReducedWord w
    Inputs
    	w:List
    Description
    	Text
	    This is a stub.
///
*-

doc ///
    Key
        toOneLineNotation
        (toOneLineNotation, List, ZZ)
	(toOneLineNotation, Matrix)
    Headline
	rewrites a transposition or permutation matrix in 1-line notation
    Usage
        toOneLineNotation(perm, maxIdx)
	toOneLineNotation(A)
    Inputs
    	perm:List
        maxIdx:ZZ
	A:Matrix
    Outputs
        :List
    Description
    	Text
	    Converts a permutation matrix or list of transpositions $(a,b)$ to 1-line notation.
            {\tt maxIdx} is the $n$ for which to regard {\tt perm} as an 
            element of $S_n$, the symmetric group on $n$ letters.
        Example
            perm = {2,4}
            maxIdx = 5
            toOneLineNotation(perm, maxIdx)
	Example
            toOneLineNotation(matrix{{0,1,0},{1,0,0},{0,0,1}})
///

doc ///
    Key
        composePerms
        (composePerms, List, List)
    Headline
        computes the composition of two permutations
    Usage
        composePerms(u,v)
    Inputs
        u:List
        v:List
    Outputs
        :List
    Description
        Text
            Computes the composition of two permutations, $u$ and $v$, as $u*v$.
            Note that the permutations must be written as a list in 1-line notation and must both be permutations of (the same) $n$ letters.
        Example
            u = {2,3,4,1}
            v = {4,3,2,1}
            composePerms(u,v)

            u = {1,2,3,4,5}
            v = {3,5,2,1,4}
            composePerms(u,v)

            u = {3,5,2,1,4}
            v = {1,2,3,4,5}
            composePerms(u,v)
///


doc ///
    Key
        isPatternAvoiding
        (isPatternAvoiding, List, List)
    Headline
        whether a permutation avoids certain patterns, e.g. $2143$-avoiding or $312$- and $231$-avoiding
    Usage
        isPatternAvoiding(w, pattern)
    Inputs
        w:List
        pattern:List
    Outputs
        :Boolean
    Description
        Text
            Given a permutation, checks if the permutation is pattern-avoiding, e.g. $2143$-avoiding or $1432$-avoiding.
            For example, a permutation $w$ is $2143$-avoiding if there does not exist indices $i < j < k < l$
            such that $w_j < w_i < w_l < w_k$.
        Example
            w = {7,2,5,8,1,3,6,4};
            pattern2143 = {2,1,4,3};
            isPatternAvoiding(w, pattern2143)

            v = {2,3,7,1,5,8,4,6};
            pattern1432 = {1,4,3,2};
            isPatternAvoiding(v, pattern1432)

            isPatternAvoiding({3,1,2},{3,1,2})
///


doc ///
    Key
        isVexillary
        (isVexillary, List)
    Headline
        whether a permutation is vexillary, i.e. 2143-avoiding
    Usage
        isVexillary w 
    Inputs
        w:List
    Outputs
        :Boolean
    Description
        Text
            Given a permutation in 1-line notation, checks if the permutation is vexillary, i.e. $2143$-avoiding.
            A permutation $w$ is $2143$-avoiding if there do not exist indices $i < j < k < l$
            such that $w_j < w_i < w_l < w_k$.
        Example
            w = {7,2,5,8,1,3,6,4}
            isVexillary w

            v = {1,6,9,2,4,7,3,5,8}
            isVexillary v
///

doc ///
    Key
        avoidsAllPatterns
        (avoidsAllPatterns, List, List)
    Headline
        whether a permutation avoids all of the given patterns
    Usage
        avoidsAllPatterns(perm, patterns)
    Inputs
        perm:List
        patterns:List
    Outputs
        :Boolean
    Description
        Text
            Given a permutation in one-line notation, and a list of patterns checks if the permutation avoids every pattern.
	Example 
	    w = {7,2,5,8,1,3,6,4}
	    patterns = {{2,1,4,3},{1,4,3,2}}
	    avoidsAllPatterns(w,patterns)
///

doc ///
    Key
        isCartwrightSturmfels
        (isCartwrightSturmfels, List)
    Headline
        whether a permutation is Cartwright-Sturmfels
    Usage
        isCartwrightSturmfels w
    Inputs
        w:List
    Outputs
        :Boolean
    Description
        Text
            Given a permutation in 1-line notation, checks if the permutation is Cartwright-Sturmfels.  By [CDG22], the matrix
	    Schubert variety $X_w$ is Cartwright-Sturmfels if and only if $w$ avoids all of the patterns 
	    $\{12543, 13254, 13524, 13542, 21543, 125364, 125634, 215364, 215634, 315264, 315624, 315642\}$.
	    
	     @UL {
            {"[CDG22] A. Conca, E. De Negri, and E. Gorla, ",
            HREF("https://arxiv.org/abs/2108.10115", EM "Radical generic initial ideals"),
            ", Vietnam J. Math. 50 (2022), no. 3, 807-827."}
            }@
	    
        Example
            w = {7,2,5,8,1,3,6,4}
            isCartwrightSturmfels w

            v = {1,6,9,2,4,7,3,5,8}
            isCartwrightSturmfels v
///

doc ///
    Key
        isCDG
        (isCDG, List)
    Headline
        whether a permutation is CDG
    Usage
        isCDG(perm)
    Inputs
        perm:List
    Outputs
        :Boolean
    Description
        Text
            Given a permutation in 1-line notation, checks if the permutation is CDG.  We say that a permutation $w$ is CDG 
	    if a certain modification (see [Kle23] for precise description) of the Fulton generators of the Schubert determinantal
	    ideal $I_w$ form a diagonal Gröbner basis.  By [Kle23], $w$ is CDG if and only if $w$ avoids all of the patterns
	    $\{13254, 21543, 214635, 215364, 215634, 241635, 315264, 4261735\}$.
	    
	     @UL {
            {"[Kle23] P. Klein, ",
            HREF("https://arxiv.org/abs/2008.01717", EM "Diagonal degenerations of matrix Schubert varieties"),
            ", Algebr. Comb. 6 (2023), no. 4, 1073-1094."}
            }@
	    
        Example
            w = {7,2,5,8,1,3,6,4}
            isCDG w

            v = {1,6,9,2,4,7,3,5,8}
            isCDG v
///

doc ///
    Key
        rajcode
        (rajcode, List)
    Headline
      finds the Rajchgot code of a permutation
    Usage
        rajcode w
    Inputs
        w:List
    Description
        Text
            Given a permutation in 1-line notation, finds its Rajchgot code, as defined in [PSW].
	    
	    @UL {{"[PWS]: O. Pechenik, A. Weigandt, and D. Speyer, \"Castlenuovo--Mumford regularity of matrix Schubert varieties\" (see ", arXiv "2111.10681", ")."},}@
	    
        Example
            w = {7,2,5,8,1,3,6,4}
	    rajcode w
	    
            v = {1,6,9,2,4,7,3,5,8}
            rajcode v
///

doc ///
    Key
        rajIndex 
        (rajIndex, List)
    Headline
      finds the Rajchgot index of a permutation
    Usage
        rajIndex w
    Inputs
        w:List
    Description
        Text
            Given a permutation in 1-line notation, finds its Rajchgot index, as defined in [PSW].
	    
	    @UL {{"[PWS]: O. Pechenik, A. Weigandt, and D. Speyer, \"Castlenuovo--Mumford regularity of matrix Schubert varieties\" (see ", arXiv "2111.10681", ")."},}@
	    
        Example
            w = {7,2,5,8,1,3,6,4}
	    rajIndex w
	    
            v = {1,6,9,2,4,7,3,5,8}
            rajIndex v
///





doc ///
    Key 
        grothendieckPolynomial
        (grothendieckPolynomial, List)
	[grothendieckPolynomial, Algorithm]
	Algorithm
    Headline
        computes the Grothendieck polynomial of a permutation 
    Usage
        grothendieckPolynomial w
    Inputs
        w:List
	Algorithm => String
	    algorithm "PipeDream" also available
    Description
        Text
            Given a permutation in 1-line notation, finds its Grothenieck polynomial.  Two algorithms are impliemented: DividedDifference (which is the default) and PipeDream.
	    
	Example
	    w = {2,1,4,3}
	    time grothendieckPolynomial w
	    time grothendieckPolynomial (w,Algorithm=>"PipeDream")
	    
///

doc ///
    Key 
        schubertPolynomial
        (schubertPolynomial, List)
	[schubertPolynomial, Algorithm]
    Headline
        computes the Schubert polynomial of a permutation 
    Usage
        schubertPolynomial w
    Inputs
        w:List
	Algorithm => String
	    algorithm "Transition" also available
    Description
        Text
            Given a permutation in 1-line notation, finds its (single) Schubert polynomial.  Two algorithms are impliemented: DividedDifference (which is the default) and Transition
	    (which makes use of the transition equations for Schubert polynomials).
	    
        Example 
	    w = {2,1,5,4,3}
	    schubertPolynomial w
	    schubertPolynomial (w,Algorithm=>"Transition")
///

doc ///
    Key 
        doubleSchubertPolynomial
        (doubleSchubertPolynomial, List)
    Headline
        computes the double Schubert polynomial of a permutation 
    Usage
        doubleSchubertPolynomial w
    Inputs
        w:List
    Description
        Text
            Given a permutation in 1-line notation, finds its double Schubert polynomial. This is implemented via the transition equations for double Schubert polynomials.
        Example 
            w = {2,1,5,4,3}
            doubleSchubertPolynomial w
///
-*
doc ///
    Key 
        dividedDifference
	(dividedDifference, RingElement, ZZ)
	[dividedDifference, Operator]
	Operator
    Headline
        the divided Difference operator of a polynomial
    Usage
    	dividedDifference(f,n)
    Inputs
    	f:RingElement
	    a polynomial in $n$ variables
	n:ZZ
	Operator =>  
    Description
        Text
            This is a stub
///
*-

undocumented {
    (symbol==, PipeDream, List),
    (describe, PipeDream),
    (net, PipeDream),
    -- (texMath, PipeDream),
    (toString, PipeDream),
    (toExternalString, PipeDream),
    }

doc ///
    Key
        PipeDream
    Headline
        the class representing a pipe dream
    Description
        Text
            A pipe dream is stored as a square array containing "+" and "/" symbols.
	    The "+" symbols are interpreted as crossing tiles and the "/" are interpreted as bump tiles.
	    Starting on the left edge, the path starting at row $i$ will end at column $w_i$.
        Example
	    L = {{"+", "/", "/"}, {"+", "/", "/"}, {"/", "/", "/"}}
	    D = PipeDream L
	    L == toList D
    Subnodes
        pipeDreams
        pipeDreamsNonReduced
///

doc ///
    Key 
        pipeDreams
        (pipeDreams, List)
    Headline
        computes the set of reduced pipe dreams corresponding to a permutation
    Usage 
        pipeDreams w
    Inputs
        w:List
    Description
        Text
            Given a permutation in one line notation, finds the set of reduced pipe dreams. Each element of the output is a square array containing "+" and "/" symbols. The "+" symbols are interpreted as crossing tiles and the "/" are interpreted as bump tiles. Starting on the left edge, the path starting at row $i$ will end at column $w_i$. This function only returns reduced pipe dreams (i.e. pipe dreams for which each pair of pipes crosses at most once).
        Example
            w = {2,1,4,3,6,5};
            (pipeDreams w)_0
    SeeAlso
        "pipeDreamsNonReduced"
///

doc ///
    Key 
        pipeDreamsNonReduced
        (pipeDreamsNonReduced, List)
    Headline
        computes the set of all pipe dreams corresponding to a permutation
    Usage 
        pipeDreams w
    Inputs
        w:List
    Description
        Text
            Given a permutation in one line notation, finds the set of  pipe dreams. Each element of the output is a square array containing "+" and "/" symbols. The "+" symbols are interpreted as crossing tiles and the "/" are interpreted as bump tiles. Starting on the left edge, the path starting at row $i$ will end at column $w_i$. This function returns all pipe dreams of w, including those containing pairs of pipes that cross more than once.
        Example
            w = {2,1,4,3,6,5};
            (pipeDreamsNonReduced w)_1
    SeeAlso
        "pipeDreams"
///

doc ///
    Key 
        ASMToMonotoneTriangle
        (ASMToMonotoneTriangle, Matrix)
    Headline
        converts an ASM to a monotone triangle
    Usage
        ASMToMonotoneTriangle A
    Inputs
        A:Matrix
    Outputs
        :List
    Description
        Text
            Converts an alternating sign matrix (ASM) to a monotone triangle according to the bijection described in [HR].
            More precisely, suppose $A$ is an ASM.
            The unique monotone triangle $T=(T_0,\ldots,T_n)$ corresponding to $A$ has the property that $T_m$ is the (ordered) set of column indices in which $\sum_{i=1}^m A_m $ 
	    has an entry of $1$, where $A_m$ denotes the $m$th row of $A$.
            See [HR] for more details.

            @UL {{"[HR]: Z. Hamaker and V. Reiner, \"Weak Order and Descents for Monotone Triangles\" (see ", arXiv "1809.10571", ")."},}@
        Example
            A = matrix{{0,1,0,0,0,0},{0,0,0,1,0,0},{1,-1,1,-1,0,1},{0,0,0,1,0,0},{0,1,0,-1,1,0},{0,0,0,1,0,0}}
            netList ASMToMonotoneTriangle A
///

doc ///
    Key 
        monotoneTriangleToASM
        (monotoneTriangleToASM, List)
    Headline
        converts a monotone triangle to an ASM
    Usage
        monotoneTriangleToASM M
    Inputs
        M:List
    Outputs
        :Matrix
    Description
        Text
            Converts an monotone triangle to an alternating sign matrix (ASM) according to the bijection described in [HR].
            More precisely, suppose $T=(T_0,\ldots,T_n)$ is a monotone triangle.
            The unique ASM $A$ corresponding to $T$ is given by $A_m = \mathbb{1}_{T_m} - \mathbb{1}_{T_{m-1}}$, where $A_m$ denotes the $m$th row of $A$ and $\mathbb{1}_{T_i}$
	    is the is a vector of length $n$ whose entries are $1$ in the positions whose indices appear in $T_i$ and $0$ otherwise.
            See [HR] for more details.
	    
	    This function does not check that what you've given it is actually a monotone triangle before attempting to convert to an ASM.

            @UL {{"[HR]: Z. Hamaker and V. Reiner, \"Weak Order and Descents for Monotone Triangles\" (see ", arXiv "1809.10571", ")."},}@
        Example
            M = {{}, {2}, {2, 4}, {1, 3, 6}, {1, 3, 4, 6}, {1, 2, 3, 5, 6}, {1, 2, 3, 4, 5, 6}}
            monotoneTriangleToASM M
///
