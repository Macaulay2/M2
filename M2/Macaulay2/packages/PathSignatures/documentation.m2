beginDocumentation()


-- Node 
--     Key
--     Inputs
--         z: ZZ -- The number of variables in the algebra, usually the ambient dimension of the path
--         CoefficientRing => Ring -- The coefficients ring, by default the rationals.
--     Outputs
--         A: NCRing --The associative free polynomial algebra on the letters LT_1, ..., Lt_z
--     Headline
--         create a free associative algebra on a given number of generators
--     Usage
--         wordAlgebra(z)
--     Description
--         Text
--             Creates the free associative polynomial algebra on the letters Lt_1,$ \dots$, Lt_z.
--         Example
--             z = 5;
--             A = wordAlgebra(z)
--             gens A
--     SeeAlso
--         wordAlgebra


-------------------------------
--INTERFACENCAlgebra
-------------------------------
doc ///

Node
    Key
        NCRingElement
    Headline
        exported from the NCAlgebra package, used to encode tensors
Node
    Key
        NCPolynomialRing
    Headline
        exported from the NCAlgebra package, used to encode tensor algebras
Node
    Key 
        ambient
        (ambient, NCPolynomialRing)
    Headline
        exported from the NCALgebra package.

Node
    Key
        (symbol _, Array, NCPolynomialRing)
    Inputs
        a : Array -- In the form [i_1,...i_k] where 0 < i_j< d+1, where d is the number of generators of R
        R : NCPolynomialRing 
    Outputs
        w: NCRingElement -- The element of R corresponding to R_(i_1)*...R_(i_k)
    Headline
        create a word from an array
    Usage
        w = a_R
    Description
        Text
            This method allows to create a monomial in a free associative algebra $R$. It follows the same 
            convention of @TO wordFormat@. Let $Lt_1,\dots, Lt_d$ be the generators of $R$, then an array of 
            integers $[i_1,\dots, i_k]$ such that $0<i_l<d+1, \forall 1\leq l\leq k$ yields the element 
            $$Lt_{i_1} Lt_{i_2}\cdot\dots\cdot Lt_{i_{k-1}} Lt_{i_k}$$
            of $R$.
        Example
            d = 5;
            R = wordAlgebra(d); -- create a free associative algebra over Lt_1,..., Lt_d
            a = new Array from for i from 1 to d list i; -- The array [1,...,d]
            f = a_R  -- The word associated to a
            g = product gens R -- The word Lt_1*...Lt_d
            f === product gens R
    SeeAlso
        wordAlgebra
        wordFormat
Node 
    Key 
        wordFormat
        (wordFormat, NCRingElement)
    Headline
        display a tensor in word notation
    Description
        Text
            A more readable display of tensors can be obtained through the following convention. Let $Lt_1,\dots, Lt_d$ be the generators of $R$, then an array of 
            integers $[i_1,\dots, i_k]$ such that $0<i_l<d+1, \forall 1\leq l\leq k$ yields the decomposable tensor
            $$Lt_{i_1} Lt_{i_2}\cdot\dots\cdot Lt_{i_{k-1}} Lt_{i_k}$$
            of $R$. This notation can then be extended linearly to any tensor.
        Example
            R = wordAlgebra(2);
            f = ([1,2]_R ** [1,2]_R) -- shuffle product of Lt_1*Lt_1 with itself displayed as a non commutative polynomial
            f // wordFormat -- f displayed in the above notation
    SeeAlso
        wordAlgebra
        (symbol _, Array, NCPolynomialRing)

Node 
    Key
        wordString
        (wordString, NCRingElement)
    Headline
        a string representing a word in wordFormat
    Inputs
        w : NCRingElement
    Outputs
        s : String -- A string representing the word 2 in word format
    Usage
        w // wordString
    Description
        Text
            Returns a string representing the word as described in @TO wordFormat@.
        Example
            R = wordAlgebra(3);
            w = [1,2,3]_R + 2 * [3,2,1]_R; w // wordFormat
            w // wordString
    SeeAlso
        wordAlgebra
        wordFormat
        (symbol _, Array, NCPolynomialRing)

Node
    Key 
        tensorArray
        (tensorArray, NCRingElement)
        (tensorArray, NCRingElement, ZZ)
        (symbol @, NCRingElement, ZZ)
    Headline
        k-th level component of a tensor.
    Inputs
        s : NCRingElement 
        k : ZZ --The level to extract
    Outputs
        sk : List -- The k-th level of s, as a multi-dimensional array
        L : List -- The list of all levels of s, as multi-dimensional arrays
    Usage
        sk = tensorArray(s,k)
        sk = s@k
        L = tensorArray(s)
    Description
        Text
            Returns the $k$-level component of a tensor as multi-dimensional array, represented by a nested @TO List@.
        Example
            R = QQ[t];
            X = polyPath({t,t^2});
            sig(X,2)@2 -- signature matrix at depth 2
            A = sig(X,3)@3 -- third level signature as multi-dimensional array
            i = 0; j = 0; k = 0;
            A#i#j#k == (Lt_(i+1)*Lt_(j+1)*Lt_(k+1))@(sig(X, 3)) --The coefficient of (Lt_1)^3 in sig(X, 3)
        Text
            If the integer $k$ is not provided, returns a list of all non trivial levels (in nested list form).
        Example
            L = tensorArray(sig(X, 3))
            L#2 == A
    SeeAlso
        (symbol @, NCRingElement, NCRingElement)


///

TEST ///
d = 5;
R = wordAlgebra(d);
a = new Array from for i from 1 to d list i;
f = a_R  
g = product gens R 
assert(f === product gens R)    
///

TEST ///
R = wordAlgebra(2);
f = ([1,2]_R  +  2 * [2,1]_R)
assert (f == Lt_1 * Lt_2  +  2 * Lt_2 * Lt_1)   
///

TEST ///
R = wordAlgebra(3);
w = [1,2,3]_R + 2 * [3,2,1]_R; w // wordFormat
assert ((w // wordString) == "2 [3, 2, 1] + [1, 2, 3]")  
///

TEST ///
R = QQ[t];
X = polyPath({t,t^2});
assert(sig(X,2)@2 == {{1/2, 2/3} , {1/3, 1/2}})
assert(sig(X,3)@3 == {{{1/6 , 1/4}, {1/6 , 4/15}}, {{1/12 , 2/15}, {1/10 , 1/6}}})
///

TEST ///
R = QQ[t];
X = polyPath({t,t^2});
A = sig(X, 2)
assert( tensorArray(A, 2) == A @ 2);
///
-------------------------------
--TYPES
-------------------------------
doc ///

Node
    Key
        Path
        (symbol _, Path, List)
        (symbol _, Path, ZZ)
        (symbol _, Path, Sequence)
        (symbol ^, Path, ZZ)
        (getDimension, Path)
        getDimension
        (dim, Path)
        (getNumberOfPieces, Path)
        getNumberOfPieces
        (getPieces, Path)
        getPieces
        (getCoefficientRing, Path)
        getCoefficientRing
        (coefficientRing, Path)
        (net, Path)
    Description
        Text
            A polynomial path is a map $[0,1] \to \mathbb R^d$ whose coordinate functions are given by polynomials. A piecewise polynomial path is a concatenation of polynomial paths.
        Text
            To create a polynomial path, use @TO polyPath@, which takes a list of polynomials as input. These can be given as elements of a commutative polynomial ring with one generator or directly in @TO2 {"Macaulay2Doc :: listForm", "listForm"}@.
        Example
            R = QQ[t];
            X = polyPath({t,2*t^2,3*t^3})
            Y = polyPath({{({1},1)},{({2},2)},{({3},3)}})
        Text
            While the polynomials must be chosen from a polynomial ring with one generator, the coefficient ring of the polynomials can be chosen arbitrarily.
        Example
            R = QQ[a][t]; --QQ[a,t] will not work!
            X = polyPath({t,2*a*t^2,3*a^2*t^3})
        Text
            An important special case of polynomial paths are linear paths. These can be constructed directly from their increment using @TO linPath@.
        Example
            Y = linPath({2,3,4})
        Text
            Paths can be concatenated using @TO (symbol **, Path, Path)@. This concatenation is formal: the new Path object encodes the polynomial pieces and their order, but no parametrization is chosen. The concatenation @TO (symbol **, Path, Path)@ will automatically select a bigger coefficient ring for all polynomial pieces if an obvious choice is available.
        Example
            Z = X ** Y
        Text
            A piecewise linear path can be constructed directly from the increments of its segments using @TO pwLinPath@.
        Example
            A = matrix {{1,2,3},{2,3,4},{4,5,6}};
            W = pwLinPath(A)
        Text
            To read out the ambient dimension of a path, use @TO (getDimension, Path)@ or @TO (dim, Path)@. To get the pieces of the path in @TO2 {"Macaulay2Doc :: listForm", "listForm"}@ use @TO (getPieces, Path)@. To get the coefficient ring of the coordinate polynomials, use @TO (getCoefficientRing, Path)@ or @TO (coefficientRing, Path)@. Finally, @TO (getNumberOfPieces, Path)@ returns the number of pieces of the path.
        Example
            getDimension(Z) --The ambient dimension of the path
            getPieces(Z) --The polynomial pieces of the path, in listForm
            getCoefficientRing(Z) -- The coefficient ring of the polynomial components of the path
            getNumberOfPieces(Z) -- The number of polynomial pieces of the path
        Text
            To extract the pieces of a concatenated path one can use @TO (symbol _, Path, ZZ)@, @TO (symbol _, Path, List)@ and @TO (symbol _, Path, Sequence)@.
        Example
            Z_0
            Z2 = Z^2
            Z2_{0,-1}
        Text
            When considering the set of paths modulo tree-like equivalence, concatenation turns it into a groupoid. Use @TO (symbol ^, Path, ZZ)@ to compute powers and the inverse of a path in this groupoid. Note that the inverse of a path is just given by reversing its parametrization.
        Example
            X^4
            X^(-1)

    SeeAlso
        polyPath
        linPath

Node 
    Key
        concatPath
        (concatPath, Path, Path)
        (symbol **, Path, Path)
    Headline
        concatenation of paths
    Usage 
        concatPath(X,Y)
        X**Y
    
    Description
        Text
            This allows for concatenation of paths. The concatenation is formal, no parametrization is chosen.
        Example
            R = QQ[t];
            X = polyPath({t,t^2}) ** polyPath({t^3 + 3*t, t^2 - 1})
    SeeAlso
        polyPath
        linPath
Node
    Key
        (substitute,Path, Ring)
    Headline
        changes the coefficient ring of a path
    Usage
        Y = substitute(X,R)
    Inputs
        X: Path
        R: Ring -- an algebra over the coefficient ring of the polynomials defining X
    Outputs
        Y: Path -- a path with the same pieces as X but whose coordinate functions are now polynomials with coefficients in R
    Description
        Text
            Tries to substitute the coefficients of the polynomials defining the given path into the given ring, producing a new path.
        Example
            R = QQ[t];
            X = polyPath({t,t^2})
            coefficientRing X
            A = QQ[a];
            Y = substitute(X,A)
            coefficientRing Y
Node
    Key
        polyPath
        (polyPath,List)
    Headline
        constructor of single piece polynomial path
    Usage
        polyPath(polyPathList)
    Inputs
        polyPathList: List --A list of elements of the same ring, the components of the polynomial path.
    Outputs
        X: Path
    Description
        Text
            Takes as input a list of polynomials in the same ring. Constructs a @TO Path@ object with one piece equal to the list of normalForm 
            of the polynomial components of the path given in input. Automatically sets the dimension attribute of the
            object to the length of the list given as input.
        Example
            R = QQ[t];
            X = polyPath({t,t^2})
            X // getDimension 
    SeeAlso
        Path
        linPath
        (symbol **, Path, Path)

Node 
    Key
        linPath
        (linPath, List)
    Headline
        constructor of single piece polynomial path
    Usage
        linPath(v)
    Inputs
        v: List -- A list of elements of a ring, the endpoints of the linear path.
    Outputs
        X: Path
    Description
        Text
            Takes as input a list of elements in the same ring. Constructs a @TO Path@ object with one piece equal to the list given in input. Automatically sets the dimension attribute of the
            object to the length of the list given as input.
        Example
            R = QQ[x_1..x_5];
            X = linPath({x_1, x_2, x_3, x_4, x_5^2})
            X // getDimension
    SeeAlso
        Path
        polyPath
        (symbol **, Path, Path)
Node 
    Key
        pwLinPath
        (pwLinPath,Matrix)
    Headline
        constructor of a piecewise linear path from a matrix
    Inputs
        pwlMatrix: Matrix -- A matrix containing on its columns the articulation points (or increments) of the path
    Outputs
        X: Path --A piecewise linear path of dimension the number of rows of the input and with the same number of pieces as the columns of the input
    Usage
        pwLinPath(pwlMatrix)
    Description
        Text
            Creates a piecewise linear @TO Path@ whose increments are the columns of the given matrix.
        Example
            M = id_(QQ^3)
            pwLinPath(M)
    SeeAlso
        Path
///

TEST ///
R = QQ[t];
X = polyPath({t,2*t^2,3*t^3})
Y = polyPath({{({1},1)},{({2},2)},{({3},3)}})
assert((getPieces X == getPieces Y) and (dim X == dim Y) and (getNumberOfPieces X == getNumberOfPieces Y))
Z = X ** Y
assert (getPieces Z_0 == {{{({1}, 1)}, {({2}, 2)}, {({3}, 3)}}})
assert( (getPieces (Z ^ 2)) == (getPieces(Z ** Z)))
assert( getPieces(Z) == getPieces(Z_{0, -1}))
assert( getPieces(X^(-1)) == {{{({1}, -1)}, {({2}, 2), ({1}, -4)}, {({3}, -3), ({2}, 9), ({1}, -9)}}})
///

TEST ///
X = pwLinPath(matrix({{1,0,0},{0,1,0},{0,0,1}}))
Y = linPath({1,0,0}) ** linPath({0,1,0}) ** linPath({0,0,1})
assert((getPieces X == getPieces Y) and (dim X == dim Y) and (getNumberOfPieces X == getNumberOfPieces Y))
///


-------------------------------
--ALGEBRA
-------------------------------

doc ///

Node
    Key
        adjointWord
        (adjointWord, NCRingElement, NCPolynomialRing, List)
    Headline
        image of a word through the shuffle algebra homomorphism induced by a polynomial map 
    Inputs 
        g : NCRingElement -- the word to compute the image of
        T : NCPolynomialRing --the shuffle algebra of the image
        L : List -- the components of the polynomial map. each entry should be a polynomial
    Usage
        adjointWord (g, T, L)
    Description
        Text
            This computes the image of g through the map $M_p$ described in Theorem 1 and Theorem 7 of @HREF("#ref1","[1]")@. Its importance is evidenced by
            Theorem 2 of the same paper: 
        Text
            Let $X:[0,1]\rightarrow \mathbb{R}^d$ be a piecewise continuously differentiable path
            with $X(0) = 0$ and let $p : \mathbb{R}^n \rightarrow \mathbb{R}^m$ be a polynomial map with $p(0) = 0$. Then, for
            all $w ∈ T(\mathbb{R}^m)$ one has $$  \sigma(p(X)) = M_p^*(\sigma(X))$$
            (where $\sigma(X)$ is the signature of $X$ and $M_p^*$ is the dual map of $M_p$).
        Text 
            As a use example, we verify this in a particular case. First we define a transformation of affine spaces and create the word algebra
            where our tensors live
        Example
            S = QQ[x,y]; 
            p = {x^2,x*y,y^2} -- A map of affine spaces, the degree 2 Veronese morphism R^2 -> R^3

            wA2 = wordAlgebra(2); -- signatures of paths in dimension 2 
            wA3 = wordAlgebra(3); -- signatures of paths in dimension 3
        Text
            Then we define a path in the domain space and explicitly compute its image under the polynomial map above:
        Example
            R = QQ[t];
            X = polyPath({t,t^2}) -- A path in 2 dimensional space
            PP = apply(p, q -> sub(q, {x=>t, y=>t^2})); 
            Y = polyPath(PP) -- the transformed path in 3 dimensional space
        Text
            Finally we compute the signature of the transformed path along the @TO sgnVolTensor@ tensor of $\mathbb{R}^3$ and verify the formula in Theorem 2 above:
        Example
            vol = sgnVolTensor(wA3); vol // wordFormat -- consider the signed volume in R^3 and display it in word format
            adw = adjointWord(vol, wA2, p); adw // wordFormat -- we compute its image through the induced homomorphism on algebras
            sig(Y, vol)  -- the signed volume of the transformed path
            sig(X, adw)  -- is given by evaluating at adw for the original path.

    References
        @LABEL("[1]","id" => "ref1")@ @HREF {"https://doi.org/10.1007/s13366-020-00493-9","Signatures of paths transformed by polynomial maps (doi.org/10.1007/s13366-020-00493-9)"} @
Node
    Key
        shuffle
        (symbol **, NCRingElement, NCRingElement)
        (symbol ⧢, NCRingElement, NCRingElement)
        (shuffle, NCRingElement, NCRingElement)
    Headline
        shuffle product of two words
    Inputs
        w1: NCRingElement 
        w2: NCRingElement
        R: NCPolynomialRing --The non-commutative polynomial where the operation ought to be carried out
    Outputs
        v: NCRingElement --The shuffle product of w1 and w2
    Usage
        v = shuffle(w1, w2, R)
    Description
        Text
            We start with the mathematical definition of this operation, based on @HREF("#ref1","[1]")@. 
            Consider $T(\mathbb{R}^d)$, the free algebra on the symbols $1,\dots, d$. Denote 
            by $\bullet$ its concatenation product and by $e$ the neutral element with respect to the concatenation (the empty word). The shuffle product of two words is defined 
            recursively as follows. Let $w, w_1, w_2$ be three words and $a, b$ bet two letters, i.e.
            $w, w_1, w_2\in T(\mathbb{R}^d)$ and $a,b\in \{1,\dots, d\}$. Then the shuffle product $\char"29E2$ is defined to be
            $$ e \char"29E2 w := w =: w\char"29E2 e$$
            and 
            $$(w_1\bullet a) \char"29E2 (w_2\bullet b) = (w_1 \char"29E2 (w_2\bullet b))\bullet a + ((w_1\bullet a)\char"29E2 w_2)\bullet b$$
        Text
            The easiest way to compute a shuffle product is through @TO (symbol **, NCRingElement, NCRingElement)@ which is equivalent to @TO (shuffle, NCRingElement, NCRingElement)@:
        Example
            R = wordAlgebra(2); -- create a free associative algebra over two letters Lt_1, Lt_2
            f = [1,2]_R; -- [i_1,...,i_k]_R defines a word
            wordFormat f -- display the polynomial in word notation
            f = ([1,2]_R ** [1,2]_R); -- compute the shuffle product
            f // wordFormat --display the result in word notation
    References
                @LABEL("[1]","id" => "ref1")@ @HREF {"https://doi.org/10.1007/s13366-020-00493-9","Signatures of paths transformed by polynomial maps (doi.org/10.1007/s13366-020-00493-9)"} @
    SeeAlso
        wordAlgebra
        wordFormat
        (symbol _, Array, NCPolynomialRing)


Node
    Key
        wordAlgebra
        (wordAlgebra, ZZ)
        (wordAlgebra, List)
        
    Headline
        create a free algebra over a given alphabet
    Description
        Text
            In this package, tensors are represented as elements of free associative algebras, using the package @TO2 {"NCAlgebra :: NCAlgebra", "NCAlgebra"}@.
            More precisely, the free associative algebra on the alphabet $\{\texttt 1,...,\texttt d\}$ is isomorphic to the tensor algebra $T(\mathbb R^d)$ via the algebra homomorphism induced by $\texttt i \mapsto e_i$. This allows us to interpret tensors as non-commutative polynomials, or equivalently, linear combinations of words.
            Given an alphabet $l$, the free associative algebra over it can be obtained by using @TO wordAlgebra@, where the letter corresponding to $x \in l$ is represented by $\texttt{Lt}_x$.
        Example
            d = 5;
            l1 = {getSymbol "a", getSymbol "b", getSymbol "c"};
            A = wordAlgebra(l1)
            gens A

            l2 = toList(1..d);
            B = wordAlgebra(l2)
            gens B
        Text
            The algebra B in the example can also be directly obtained for a given d using @TO (wordAlgebra, ZZ)@:
        Example
            B = wordAlgebra(d);
            gens B
        Text
            By default, @TO wordAlgebra@ creates a non-commutative algebra over @TO2{"Macaulay2Doc :: QQ", "QQ"}@. The coefficient ring can be changed via the CoefficientRing option:
        Example
            coefficientRing B
            C = wordAlgebra(d, CoefficientRing => CC)
            coefficientRing C
        Text
            An element of the algebra can be obtained by using the generator symbols, or (more conveniently) by using word notation, see @TO (symbol _, Array, NCPolynomialRing)@:
        Example
            d = 5;
            R = wordAlgebra(d); -- create a free associative algebra over two letters Lt_1, Lt_2
            f = 2 * [1,d]_R - [2,d]_R  -- [i_1,...,i_k]_R defines a word.
            f === 2 * Lt_1 * Lt_d - Lt_2 * Lt_d
        Text
            Note that for two words (equivalently, monomials) $\texttt{w}$ and $\texttt{v}$, $\texttt{w} * \texttt{v}$ is the concatenation.
        Text
            To display a non-commutative polynomial in word notation, one can use @TO wordFormat@ or @TO wordString@:
        Example
            f^3 // wordFormat
            f^3 // wordString
        Text
            There are more interesting algebraic structures on non-associative algebras; of particular importance in the context of path signatures is the shuffle product and the half-shuffle product:
        Example
            a = [1]_R ** [2]_R; wordFormat a -- the shuffle product
            b = [1,2]_R ** [3,4]_R; wordFormat b
            c = [1]_R >> [2,3]_R; wordFormat c -- the half-shuffle product
        Text
            See @TO (symbol **, NCRingElement, NCRingElement)@ and @TO (symbol >>, NCRingElement, NCRingElement)@ for more information on the shuffle and half-shuffle product.
    SeeAlso
        (symbol _, Array, NCPolynomialRing)

Node
    Key
        halfshuffle
        (halfshuffle, NCRingElement, NCRingElement)
        (symbol >>, NCRingElement, NCRingElement)
    Headline
        compute the half-shuffle of an ordered pair of words
    Inputs
        f : NCRingElement 
        g : NCRingElement 
    Outputs
        h : NCRingElement -- the half-shuffle f>>g of the two words
    Usage
        h = f >> g
    Description
        Text
            We start with the mathematical definition, based on @HREF("#ref1","[1]")@ (where the operation is called {\em right half-shuffle}). Let 
            $T^{\geq 1}(\mathbb{R}^d)$ be the vector space spanned by the non empty words on $d$ letters. Then the half shuffle $>>$ is defined 
            recursively to be $$ w >> i := wi$$ for $w$ a word and $i$ a letter and $$ w >> vi := (w >> v + v >> w)\bullet i$$ for $w, v$ words 
            and $i$ a letter, where $\bullet$ is the concatenation product on words. 
        Text
            As stated in the reference, the @TO shuffle@ on non empty words can be seen as a symmetrization of the half-shuffle. As a usage example, we verify this in a particular instance.
        Example
            R = wordAlgebra(3);
            w = [1]_R
            v = [1,2,3]_R
            s = w ** v --shuffle product of w, v
            hsSymm = (w >> v) + (v >> w)--half-shuffle product symmetrization of w, v
            s == hsSymm


    References
        @LABEL("[1]","id" => "ref1")@ @HREF {"https://doi.org/10.1007/s13366-020-00493-9","Signatures of paths transformed by polynomial maps (doi.org/10.1007/s13366-020-00493-9)"} @
    SeeAlso
        shuffle
        (symbol _, Array, NCPolynomialRing)

Node
    Key
        lie
    Headline
        lie bracket of two elements
    Inputs
        a : NCRingElement
        b : NCRingElement
    Outputs
        c : NCRingElement --The commutator of a, b
    Usage 
        c = lie(a, b)
    Description
        Text
            A non-commutative algebra $(R, +, \cdot)$ is naturally a Lie algebra with the commutator $a\cdot b - b\cdot a$ as the Lie bracket. The commutator of two non-commutative polynomials can be computed as follows:
        Example
            R = wordAlgebra (3);
            a = [1]_R
            b = [2]_R
            lie(a,b)
Node 
    Key
        lieBasis
        (lieBasis, Array, NCPolynomialRing)
        (lieBasis, List, NCPolynomialRing)
    Headline
        basis element corresponding to a Lyndon word in a Lie algebra
    Inputs
        w : Array -- a Lyndon word in @TO wordFormat@ notation
        R : NCPolynomialRing -- The algebra where the output lives
    Outputs
        b : NCRingElement --An element of R
    Usage
        b = lieBasis(w, R)
    Description
        Text
            A word $l$ on the alphabet $\{1,\dots, d\}$ is a {\em Lyndon word} if it is strictly smaller, in lexicographic order, than all of its rotations.
            To any Lyndon word we can associate an iterated Lie bracketing $b(l)\in T(\mathbb{R}^d)$ defined iteratively as follows. If $l$ is a letter $i\in \{1,\dots, d\}$
            we simply define $$ b(i) := e_i$$
            where as ever $e_i$ is the $i-th$ vector in the canonical basis of $\mathbb{R}^d$. For the length of $l$ greater than 1 we define $$
            b(I) := [b(I_1), b(I_2)]$$
            where $I_1, I_2$ are such that their concatenation $I_1 I_2$ is $I$ and $I_2$ is the longest Lyndon word appearing as a proper right factor 
            of $I$. 
        Text
            This method computes $b(l)$ for a given Lyndon word. To illustrate its usage, we replicate Example 4.9 of the reference paper.
        Example
            R = wordAlgebra(2);
            lieBasis([1,1,1,2], R) == [1,1,1,2]_R - 3 * [1,1,2,1]_R + 3 * [1,2,1,1]_R - [2,1,1,1]_R
            lieBasis([1,1,2,2], R) == [1,1,2,2]_R - 2 * [1,2,1,2]_R + 2 * [2,1,2,1]_R - [2,2,1,1]_R
            lieBasis([1,2,2,2], R) == [1,2,2,2]_R - 3 * [2,1,2,2]_R + 3 * [2,2,1,2]_R - [2,2,2,1]_R
        Text
            The word can also be given as a @TO List@.
        Example
            lieBasis({1,1,1,2}, R) // wordFormat

    References
        @HREF {"https://doi.org/10.1017/fms.2019.3", "Varieties Of Signature Tensors (doi.org/10.1017/fms.2019.3)"}@
Node
    Key
        lyndonWords
        (lyndonWords, ZZ, ZZ)
    Headline
        compute all Lyndon words of at most a given length on a given number of letters
    Inputs
        d : ZZ --The number of letters
        k : ZZ --The maximum length
    Outputs
        L : List -- of all Lyndon words of length at most k in d letters
    Usage
        L = lyndonWords(d, k)
    Description
        Text
            A word $l$ on the alphabet $\{1,\dots, d\}$ is a {\em Lyndon word} if it is strictly smaller, in lexicographic order, than all of its rotations.
        Text
            This method generates a list containing all Lyndon words of length at most $k$ on $d$ letters. The Lyndon words are given as @TO Array@ in the same convention
            of @TO wordFormat@. 
        Example
            words = lyndonWords (2,3)
            R = wordAlgebra(2);
            apply(words, i-> i_R)
Node
    Key
        lyndonShuffle
        (lyndonShuffle, NCRingElement)
    Usage
        f = lyndonShuffle(T)
    Inputs
        T : NCRingElement
        f : HashTable -- representing the shuffle polynomial in Lyndon words
    Headline
        compute the representation of a tensor as a shuffle polynomial in Lyndon words
    Description
        Text
            The free associative algebra $k \langle \texttt{1}, \dots, \texttt{d} \rangle$ is isomorphic to the free commutative algebra over the Lyndon words when equipped with the shuffle product $\char"29E2$. This method represents the corresponding isomorphism
            $$k \langle \texttt{1}, \dots, \texttt{d} \rangle_{\char"29E2} \to k[x_w \ | \ w \text{ Lyndon}].$$
        Example
            A3 = wordAlgebra(3);
            T = [3,2,1]_A3;
            f = lyndonShuffle(T)
        Text
            The polynomial is represented as in @TO2 {"Macaulay2Doc::standardForm","standard form"}@, with the variable index replaced by the respective Lyndon word. One easily obtains an actual polynomial from this:
        Example
            var = new Array from apply(lyndonWords(3,3), i->x_i)
            R = QQ var;
            pol = sum(pairs f, (term,coef) -> coef * product(pairs term, (word,ex)-> x_word^ex))
        Text
            Indeed, we check:
        Example
            ([1]_A3**[2]_A3**[3]_A3 - [1]_A3**[2,3]_A3 - [1,2]_A3**[3]_A3 + [1,2,3]_A3) // wordFormat
        Text
            In general, one can "shuffle out" a polynomial like $\texttt{pol}$ in two steps as follows:
        Example
            polh = applyKeys(standardForm pol, i-> applyKeys(i,j-> last baseName R_j));
            sum(pairs polh, (term,coef) -> coef * fold(flatten apply(pairs term, (word,ex)-> toList (ex:(word_A3))),(i,j)->i**j)) // wordFormat
Node
    Key
        inner
        (symbol @, NCRingElement, NCRingElement)
        (inner, NCRingElement, NCRingElement)
    Usage
        c = inner(f,g)
        c = f @ g
    Inputs
        f : NCRingElement
        g : NCRingElement
    Headline
        compute the inner product of two tensors.
    Description
        Text
            The inner product of two words $\mathtt{w}$ and $\mathtt{v}$ in a free associative algebra is defined as $1$ if $\mathtt{w} = \mathtt{v}$ and $0$ otherwise. This extends bilinearly to an inner product on the whole associative algebra.
        Text
            The inner product can be used to access the coefficient of a tensor over a single word.
        Example
            R = wordAlgebra(3);
            t = 2*[1,2,3]_R + [2,3,1]_R + 4*[3,3,3,3]_R; t //wordFormat
            [1,2,3]_R @ t
            [3,3,3,3]_R @ t
            ([1,2,3]_R @ t) == (t @ [1,2,3]_R)
        Text
            It is also a convenient way to evaluate linear combinations of tensor entries:
        Example
            A = CAxisTensor(3,R);
            vol = sgnVolTensor(R);
            A @ vol -- the signed volume of the canonical axis path in 3 dimensions.

Node
    Key
        matrixAction
        (symbol *, Matrix, NCRingElement)
        (matrixAction, Matrix, NCRing, NCRing)
        (matrixAction, Matrix, NCRingElement, NCRing)
    Headline
        Diagonal matrix action on a tensor. 
    Inputs
        M : Matrix -- A matrix acting on a tensor
        f : NCRingElement -- The tensor to act on
        A : NCRing -- The domain of the action
        B : NCRing -- The codomain of the action
    Outputs
        F : NCRingMap -- The action A->B of M 
        Mf : NCRingElement -- The action of M on f
    Usage
        Mf = M * f -- Computes the matrix action on f in a new algebra
        F = matrixAction(M, A, B) -- Gives the map A->B corresponding to the action
        Mf = matrixAction(M, f, B) -- Computes the matrix action on f, as an element in B
    Description
        Text
            Let $\mathtt{A} := R\langle\mathtt{e_1}, \dots, \mathtt{e_a}\rangle$ be a free associative algebra on $\mathtt{a}$ letters and $\mathtt{B}:= \mathtt{R}\langle\mathtt{f_1}, \dots, \mathtt{f_b}\rangle$ be a free associative algebra on $\mathtt{b}$ letters, both with coefficient in a ring $\mathtt{R}$.
            Let $\mathtt{M}$ be a $\mathtt{b}\times \mathtt{a}$ matrix with entries in $\mathtt{R}$. Then the matrix action of $\mathtt{M}$ from $\mathtt{A}$ to $\mathtt{B}$ is defined on the generators of $\mathtt{A}$ by $$
            \mathtt{e_j}\mapsto \sum_{i=1}^{\mathtt{a}}\mathtt{M}_{ij}\mathtt{f_i}, \,\,\forall 0<j\leq \mathtt{a}, 
            $$
            and then extended by linearity to the whole of $\mathtt{A}$.
        Text
            For example if we consider $\mathtt{A}= \mathbb{Q}\langle \mathtt{e_1},\mathtt{e_2}, \mathtt{e_3}, \mathtt{e_4}\rangle$, $\mathtt{B}= \mathbb{Q}\langle \mathtt{f_1},\mathtt{f_2}, \mathtt{f_3}\rangle$ and the matrix $$
            \mathtt{M} := \begin{pmatrix}
            0 & 0 & 1 & 1\\
            0 & 1 & 0 & 0\\
            1 & 0 & 0 & 1
            \end{pmatrix}$$
            we get the map defined by $\mathtt{e_1}\mapsto \mathtt{f_3}$, $\mathtt{e_2}\mapsto \mathtt{f_2}$, $\mathtt{e_3}\mapsto \mathtt{f_1}$, $\mathtt{e_4}\mapsto \mathtt{f_1}+\mathtt{f_3}$. The action of $\mathtt{M}$ on $\mathtt{w}:=\mathtt{e_1}\mathtt{e_2}+2\mathtt{e_4}$ is $$
            \mathtt{M*w} = \mathtt{f_3 f_2}+ 2 \mathtt{f_1}+2 \mathtt{f_3}
            $$
        Text
            To get the map from $\mathtt{A}$ to $\mathtt{B}$ use @TO (matrixAction, Matrix, NCRing, NCRing)@.
        Example
            M = matrix {{0,0,1,1}, {0,1,0,0}, {1,0,0,1}}
            A = wordAlgebra(4);
            B = wordAlgebra(3);
            F = matrixAction(M, A, B)
        Text
            To compute the action of $\mathtt{M}$ on a tensor $\mathtt{w}$ as an element of $\mathtt{B}$ use @TO (matrixAction, Matrix, NCRingElement, NCRing)@.
        Example
            w = [1,2]_A + 2* [4]_A
            Mw = matrixAction(M, w, B)
            F(w) == Mw
        Text
            To compute the action of $\mathtt{M}$ on a tensor $\mathtt{w}$ in a new algebra created automatically, use @TO (symbol *, Matrix, NCRingElement)@.
        Example
            M * w


Node
    Key
        tensorExp
        (tensorExp, NCRingElement,ZZ)
    Headline
        Compute a component of the exponential of a tensor.
    Description
        Text
            Let $T((\mathbb{R}^d))$ denote the dual of the tensor algebra on $\mathbb{R}^d$, i.e., the space $\prod_k (\mathbb{R}^d)^{\otimes k}$. Given $x \in T(\mathbb{R}^d)$ its exponential is
            $$\exp(x) := \sum_{k \geq 0} \frac{1}{k!} x^{\otimes k} \in T((\mathbb{R}^d)).
            $$
            $\texttt{tensorExp(x,k)}$ computes the degree $k$ component of $\exp(x)$.

            If the constant term of the input is not $0$, the exponential can not be expressed with algebraic coefficients. To avoid this case, the method is only implemented for tensors with constant term equal to $0$.
        Example
            R = wordAlgebra(2);
            x = [1]_R + [1,2]_R
            tensorExp(x, 2)
    Caveat
        The method is implemented only for tensors with constant term $0$.
    SeeAlso
        lieBasis
        tensorLog
    -- References
    --      @HREF {"https://doi.org/10.1017/fms.2019.3", "Varieties Of Signature Tensors (doi.org/10.1017/fms.2019.3)"}@

Node
    Key
        tensorLog
        (tensorLog, NCRingElement,ZZ)
    Headline
        Compute a component of the logarithm of a tensor.
    Description
        Text
            Let $T((\mathbb{R}^d))$ denote the dual of the tensor algebra on $\mathbb{R}^d$, i.e., the space $\prod_k (\mathbb{R}^d)^{\otimes k}$. Given $x \in T(\mathbb{R}^d)$ with constant term $1$, its logarithm is
            $$\log(x) := - \sum_{k \geq 0} \frac{1}{k} (1-x)^{\otimes k} \in T((\mathbb{R}^d)).
            $$
            $\texttt{tensorLog(x,k)}$ computes the degree $k$ component of $\log(x)$.
        Example
            A2 = wordAlgebra(2);
            x = 1 + [1]_A2 + 1/2 * [1,1]_A2 + 1/6 * [1,1,1]_A2;
            sum(0..3, i -> tensorLog(x, i))
    Caveat
        The method is implemented only for tensors with constant term $1$.
    SeeAlso
        tensorExp
    -- References
    --      @HREF {"https://doi.org/10.1017/fms.2019.3", "Varieties Of Signature Tensors (doi.org/10.1017/fms.2019.3)"}@

Node
    Key
        sgnVolTensor
        (sgnVolTensor, NCPolynomialRing)
    Headline 
        The signed volume tensor in a free associative algebra.
    Description
        Text
            Given a free associative algebra $K \langle \mathtt{1}, \dots, \mathtt{d} \rangle$ over a field $K$, the {\em signed volume tensor} is
            $$\frac{1}{d!}\sum_{\sigma} (-1)^{\operatorname{sign}(\sigma)} \mathtt{\sigma(1)}\dots \mathtt{\sigma(d)}$$
            where the sum is taken over all permutations of ${1, \dots, d}$. Under the isomorphism $\mathbb R \langle \mathtt{1}, \dots, \mathtt{d} \rangle \to T(\mathbb{R}^d), \ \mathtt{i} \mapsto e_i^*$ it corresponds to the determinant $e_1^* \wedge \dots \wedge e_d^*$.
        Text
            This method computes the signed volume tensor of the given @TO NCPolynomialRing@. The output is in the same ring.
        Example
            d = 3;
            R = wordAlgebra(d)
            sgnVolTensor(R) // wordFormat
        Text
            The paper @HREF("#ref1","[1]")@ explores under what conditions the value of the signature at the signed volume tensor computes the volume of the convex hull of a path. One instance where this is true is the case of canonical axis paths (see @TO CAxisTensor@).
            For example, for $\mathtt{d}=3$ the convex hull of the canonical axis path in $\mathbb{R}^{\mathtt{d}}$ is a tetrahedron, whose volume is $\frac{1}{6}$. We verify this.
        Example
            X = linPath({1,0,0})**linPath({0,1,0})**linPath({0,0,1})
            R = wordAlgebra(3) -- where the signature of X lives
            v = sgnVolTensor(R); 
            sig(X, v) -- computes the signature of X at the signed volume tensor



    References
        @LABEL("[1]","id" => "ref1")@ @HREF {"https://doi.org/10.1007/978-3-031-38271-0_45", "Convex Hulls of Curves: Volumes and Signatures (doi.org/10.1007/978-3-031-38271-0_45)"}@

Node
    Key
        tensorParametrization
        (tensorParametrization, NCRingElement)
        [tensorParametrization, CoefficientRing]
        [tensorParametrization, VarWordTable]
        VarWordTable
    Headline
        Constructs the morphism that maps a word to its coefficient in the given tensor.
    Inputs
        T : NCRingElement -- the tensor encoding the parametrization
        CoefficientRing => QQ -- the coefficient ring of the domain polynomial ring
        VarWordTable => null -- a hash table that assigns variables of a free polynomial ring to words. If provided, the ring is used as the domain of the map
    Outputs
        m : RingMap -- the parametrization encoded by T
    Usage
        m = tensorParametrization(f)
    Description
        Text
            Given some $k$-algebra $A$, a tensor $f = \sum_{\mathtt{w} \in I} a_w \mathtt{w} \in A \langle\texttt{1},\ldots,\texttt{d}\rangle$ can be viewed as a morphism $\mathrm{Spec} \ A \to k^I$. This method constructs the associated map of coordinate rings, that is, the morphism
            $k[b_w \ | \ w\in I] \to A$ that maps $b_\mathtt{w}$ to $a_\mathtt{w}$. Here $k$ is the $\texttt{baseRing}$ of $A$. If $\texttt{CoefficientRing}$ is provided, it replaces $k$. If $\texttt{VarWordTable}$ is provided, the map above is precomposed with the morphism induced by the assignments in the hash table.
        Example
            R = QQ[a,b];
            A2 = wordAlgebra(2, CoefficientRing => R);
            T = a^2 * [1,1]_A2 + a * b * [1,2]_A2 + b^2 * [2,2]_A2
            tensorParametrization(T); wordFormat T
            S = QQ[x,y,z];
            vwtable = hashTable({(x,[1,1]),(y,[1,2]),(z,[2,2])})
            m = tensorParametrization(T, VarWordTable => vwtable)
            kernel m
        Text
            For a key use example see @TO "Computing Path Varieties"@.
Node
    Key
        (antipode, NCRingElement)
    Headline
        The antipode of a tensor 
    Description
        Text
            Consider a free associative algebra $\mathtt{R}$ on the letters $\mathtt{1}, \dots, \mathtt{d}$. We define the {\em antipode} map $\mathtt{a}:\mathtt{R}\rightarrow \mathtt{R}$ first on a word $w:=\mathtt{i_1}\cdot\dots \cdot\mathtt{i_k}$ to be $$
            \mathtt{a}(w) := (-1)^k \mathtt{i_k}\cdot\dots\cdot \mathtt{i_1}$$
            and then extending it by linearity to the whole algebra.
        Example
            R = wordAlgebra(3)
            f = [1,2,3]_R + 2* [3,2]_R; f//wordFormat
            antipode(f) //wordFormat
///

TEST ///
S = QQ[x,y]; 
p = {x^2,x*y,y^2} -- A map of affine spaces, the degree 2 Veronese morphism R^2 -> R^3
wA2 = wordAlgebra(2); -- signatures of paths in dimension 2 
wA3 = wordAlgebra(3);
R = QQ[t];
X = polyPath({t,t^2}) -- A path in 2 dimensional space
PP = apply(p, q -> sub(q, {x=>t, y=>t^2})); 
Y = polyPath(PP)
vol = sgnVolTensor(wA3); vol // wordFormat -- consider the signed volume in R^3 and display it in word format
adw = adjointWord(vol, wA2, p); adw // wordFormat -- we compute its image through the induced homomorphism on algebras
assert( sig(Y, vol) == sig(X, adw))
///

TEST ///
R = wordAlgebra(2);
f = ([1,2]_R ** [1,2]_R)
assert (f == 2 * Lt_1 * Lt_2 * Lt_1 * Lt_2  +  4 * Lt_1 * Lt_1 * Lt_2 * Lt_2)  
///

TEST ///
l1 = {getSymbol "a", getSymbol "b", getSymbol "c"};
A = wordAlgebra(l1)
assert( gens A == {Lt_a, Lt_b, Lt_c} )

l2 = toList(1..5);
B = wordAlgebra(l2)
assert(gens B == {Lt_1, Lt_2, Lt_3, Lt_4, Lt_5})

C = wordAlgebra(5);
assert( {Lt_1, Lt_2, Lt_3, Lt_4, Lt_5} == gens C)

assert(instance(1_(coefficientRing B), QQ))
C = wordAlgebra(5, CoefficientRing => CC)
assert(instance(1_(coefficientRing C), CC))
///

TEST ///
R = wordAlgebra(5)
c = [1]_R >> [2,3]_R
assert(c == [2,1,3]_R + [1,2,3]_R)
///

TEST ///
R = wordAlgebra(3);
w = [1]_R
v = [1,2,3]_R
s = w ** v --shuffle product of w, v
hsSymm = (w >> v) + (v >> w) --half-shuffle product symmetrization of w, v
assert(s == hsSymm)
///

TEST ///
R = wordAlgebra (3);
a = [1]_R
b = [2]_R
assert(lie(a,b) == -1*[2,1]_R + [1,2]_R)
///

TEST ///
R = wordAlgebra(2);
assert(lieBasis([1,1,1,2], R) == [1,1,1,2]_R - 3 * [1,1,2,1]_R + 3 * [1,2,1,1]_R - [2,1,1,1]_R)
assert(lieBasis([1,1,2,2], R) == [1,1,2,2]_R - 2 * [1,2,1,2]_R + 2 * [2,1,2,1]_R - [2,2,1,1]_R)
assert(lieBasis([1,2,2,2], R) == [1,2,2,2]_R - 3 * [2,1,2,2]_R + 3 * [2,2,1,2]_R - [2,2,2,1]_R)
assert(lieBasis({1,1,1,2}, R) == lieBasis([1,1,1,2], R))
///

TEST ///
words = lyndonWords (2,3)
R = wordAlgebra(2);
assert(words == {[1], [1, 1, 2], [1, 2], [1, 2, 2], [2]})
apply(words, i-> i_R)
assert(apply(words, i-> i_R) == {Lt_1 , Lt_1^2 * Lt_2 , Lt_1 *  Lt_2 , Lt_1 * Lt_2^2  , Lt_2 })
///

TEST ///
A3 = wordAlgebra(3);
T = [3,2,1]_A3;
f = lyndonShuffle(T)
var = new Array from apply(lyndonWords(3,3), i->x_i)
R = QQ var;
assert(sum(pairs f, (term,coef) -> coef * product(pairs term, (word,ex)-> x_word^ex)) == x_[1] * x_[2] * x_[3]  -  x_[1] * x_[2,3]  -  x_[1,2] * x_[3]  +  x_[1,2,3])
assert(([1]_A3**[2]_A3**[3]_A3 - [1]_A3**[2,3]_A3 - [1,2]_A3**[3]_A3 + [1,2,3]_A3) == [3,2,1]_A3)
///

TEST ///
R = wordAlgebra(3);
t = 2*[1,2,3]_R + [2,3,1]_R + 4*[3,3,3,3]_R; 
assert([1,2,3]_R @ t == 2)
assert([3,3,3,3]_R @ t == 4)
assert(([1,2,3]_R @ t) == (t @ [1,2,3]_R))
///

TEST ///
R = wordAlgebra(3)
A = CAxisTensor(3,R);
vol = sgnVolTensor(R);
assert(A @ vol == 1/6)
///

TEST ///
M = matrix {{0,0,1,1}, {0,1,0,0}, {1,0,0,1}}
A = wordAlgebra(4);
B = wordAlgebra(3);
F = matrixAction(M, A, B)
assert(F([1]_A) == [3]_B and F([2]_A) == [2]_B and F([3]_A) == [1]_B and F([4]_A) == [1]_B + [3]_B)
w = [1,2]_A + 2* [4]_A
Mw = matrixAction(M, w, B)
assert(F(w) == Mw)
assert(M * w //wordString == Mw //wordString)
///

TEST ///
R = wordAlgebra(2);
P = [1,2]_R + [1]_R
assert(tensorExp(P, 2) == [1,2]_R  +  1/2 * [1,1]_R)
///

TEST ///
R = wordAlgebra(3)
v = sgnVolTensor(R); 
assert(v == -1/6 * [3, 2, 1]_R  +  1/6 * [3, 1, 2]_R  +  1/6 * [2, 3, 1]_R  -  1/6 * [2, 1, 3]_R  -  1/6 * [1, 3, 2]_R  +  1/6 * [1, 2, 3]_R)

X = linPath({1,0,0})**linPath({0,1,0})**linPath({0,0,1})
assert(v @ sig(X, 3)  == 1/6)
///

TEST ///
A = wordAlgebra(2)
T = sgnVolTensor(A)
F = tensorParametrization(T)
S = gens source F
assert(F(S#0) == 1/2 and F(S#1) == -1/2)
///

TEST ///
R = wordAlgebra(3)
f = [1,2,3]_R + 2* [3,2]_R; 
assert(antipode(f) == - [3, 2, 1]_R  +  2 * [2,3]_R)  
///
----------------------------------
--SIGNATURES
---------------------------------

doc ///
Node
    Key
        sig
        (sig, Path, List)
        (sig, Path, NCRingElement)
        (sig, Path, ZZ)
        (sig, Path, ZZ, NCRing)
    Headline
        compute the signature of a piecewise polynomial path.
    Description
        Text
            Given a @TO Path@ $X(t):[0,1]\rightarrow \mathbb{R}^d$, its signature is the linear form $\sigma: T((\mathbb{R}^d)^*)\rightarrow \mathbb{R}$ on the tensor algebra of the dual of 
            $\mathbb{R}^d$, whose image on a decomposable tensor $\alpha_1\otimes \dots\otimes \alpha_k$ is the iterated integral $$
            \alpha_1\otimes \dots\otimes \alpha_k\overset{\sigma}{\mapsto} \int_0^1\int_0^{t_k}\dots\int_0^{t_2}\partial(\alpha_1 X)\dots \partial (\alpha_k X) d t_1\dots dt_k.
            $$
            This form does not depend on the parametrization of $X$.
            In this package, we identify $T((\mathbb{R}^d)^*)$ with the free associative algebra over the alphabet $\{\texttt{1},\dots,\texttt{d}\}$ via $\texttt{i} \mapsto e_i^*$ where $e_1^*, \dots, e_d^*$ is the dual of the canonical basis of $\mathbb{R}^d$. See also @TO wordAlgebra@.
        Example
            d = 4;
            R = QQ[t];
            X = polyPath(for i from 1 to d list t^i) -- the moment path in dimension d
            A = wordAlgebra(d) -- create the free associative algebra over d letters
            w = (new Array from (1..d))_A -- the word 1..d
        Text
            In the example above, the word $\texttt{w}$ corresponds to the simple tensor $e_1^*\otimes\dots  \otimes e_d^*$.
            The signature of $X$ on this word can be computed using @TO (sig, Path, NCRingElement)@.
        Example 
            sig(X, w)
            sig(X,[1]_A)
            sig(X,[2,3]_A)
        Text
            One can also compute the {\em k-th level} signature tensor for $k\in \mathbb{N}$ by using @TO (sig, Path, ZZ)@. This returns the tensor as a non-commutative polynomial in symbols Lt_1, ..., Lt_d.
        Example
            T = sig(X, 2)
            T // wordFormat
        Text
            Note however that neither the symbols nor the ring of this polynomial are made available to the user, in particular they can not be added or multiplied. To obtain the tensor as a NCPolynomial in a given NCRing, use @TO (sig, Path, ZZ,  NCRing)@ instead. The following example demonstrates Chen's identity:
        Example
            T = 1 + sig(X, 1, A) + sig(X, 2, A);
            S = T * T;
            Ma = matrix (S@2) -- the second component of S as a matrix
            Y = X ** X -- X concatenated with itself
            Mb = matrix (sig(Y, 2, A)@2) -- the signature matrix of Y
            (Ma == Mb)

Node
    Key
        CAxisTensor
        (CAxisTensor, ZZ, NCPolynomialRing)
    Headline
        the signature tensor of the canonical axis path at a given level
    Inputs
        k : ZZ -- the level of the signature tensor to compute
        R : NCPolynomialRing -- the output tensor space
    Outputs
        s : NCRingElement -- an element of R; the k-th level signature of the canonical axis path in $\mathbb R^d$, where $d$ is the number of generators of R
    Usage
        s = CAxisTensor(k, R)
    Description
        Text
            The {\em canonical axis path} in $\mathbb{R}^d$ is the path from $(0, \dots, 0)$ to $(1, \dots, 1)$ given by $d$ linear steps 
            in the unit directions $e_1, \dots, e_d$, in this order. The $k$-th level signature tensor of such a path has a combinatorial closed-form description (see the reference below) and can be obtained as follows:
        Example
            d = 2;
            k = 3;
            R = wordAlgebra(d);
            Cd = CAxisTensor(k, R); Cd // wordFormat -- k-th level signature of the canonical axis path in R^d
        Text
            To expand on the example, we verify that the result agrees with the one obtained from @TO sig@.
            Notice that the matrix of increments for the canonical axis path in dimension $d$ is 
            the $d \times d$ identity matrix.
        Example
            M = id_(QQ^d); -- identity matrix
            CAxisPath = pwLinPath(M) -- the canonical axis path in dimension d
            Cd2 = sig(CAxisPath, k); Cd2 // wordFormat -- the k-th level signature
    References
        @HREF {"https://doi.org/10.1017/fms.2019.3", "Varieties Of Signature Tensors (doi.org/10.1017/fms.2019.3)"}@

Node
    Key
        CMonTensor
        (CMonTensor, ZZ, NCPolynomialRing)
    Headline
        the signature tensor of the canonical monomial path at a given level
    Inputs
        k : ZZ -- the level of the signature tensor to compute
        R : NCPolynomialRing -- the output tensor space
    Outputs
        s : NCRingElement -- an element of R; the k-th level signature of the canonical monomial path in $\mathbb R^d$, where $d$ is the number of generators of R
    Usage
        s = CAxisTensor(k, R)
    Description
        Text
            The {\em canonical monomial path} in $\mathbb{R}^d$ is the path from $(0, \dots, 0)$ to $(1, \dots, 1)$ given by $t\mapsto (t, t^2, \dots, t^d)$. Its $k$-th level signature has a closed-form description (see the reference below) and can be obtained as follows:
        Example
            d = 2;
            k = 3;
            R = wordAlgebra(d);
            Cd = CMonTensor(k, R); Cd // wordFormat --k-th level signature of the canonical monomial path in R^d
        Text
            To expand on the example, we verify that the result agrees with the one obtained from @TO sig@.
        Example
            R=QQ[t];
            CMonPath = polyPath(for i from 1 to d list t^i) -- The canonical axis path in dimension d
            Cd2 = sig(CMonPath, k); Cd2 // wordFormat --The k-th level signature
    References
        @HREF {"https://doi.org/10.1017/fms.2019.3", "Varieties Of Signature Tensors (doi.org/10.1017/fms.2019.3)"}@


///

TEST ///
d = 4;
R = QQ[t];
X = polyPath(for i from 1 to d list t^i) -- the moment path in dimension d
A = wordAlgebra(d) -- create the free associative algebra over d letters
w = (new Array from (1..d))_A -- the word 1..d
assert(sig(X, w) == 2/15)
assert(sig(X, [1]_A) == 1)
assert(sig(X,[2,3]_A) == 3/5)

T = 1 + sig(X, 1, A) + sig(X, 2, A);
S = T * T;
Ma = matrix (S@2) -- the second component of S as a matrix
Y = X ** X -- X concatenated with itself
Mb = matrix (sig(Y, 2, A)@2) -- the signature matrix of Y
assert(Ma == Mb)
///

TEST ///
M = id_(QQ^3); -- identity matrix
CAxisPath = pwLinPath(M) -- the canonical axis path in dimension d
Cd2 = sig(CAxisPath, 2)
R = wordAlgebra(3);
Cd = CAxisTensor(2, R);
assert ( Cd // wordString == Cd2 // wordString)
///

TEST ///
R = wordAlgebra(3);
Cd = CMonTensor(2, R);
R=QQ[t];
CMonPath = polyPath(for i from 1 to 3 list t^i) -- The canonical axis path in dimension d
Cd2 = sig(CMonPath, 2);
assert (Cd //wordString == Cd2 //wordString)
///

----------------------------------
--SIGNATURE VARIETIES
---------------------------------

TEST ///
d=2; k=3; m=2;
R = QQ[a_(1,1)..a_(d,m)];
Ma = transpose genericMatrix(R,m,d);
A2 = wordAlgebra(m, CoefficientRing => R);
CAx = CAxisTensor(k, A2);
DAx = Ma * CAx; 
parAx = tensorParametrization(DAx,CoefficientRing => QQ); --Parametrization of L_{2,3,2}
I = ker parAx;
assert (dim I == 4)
assert (degree I == 6) --Variety has affine dimension 4 and degree 6 
///

TEST ///
d=2; k=4; m=2;
R = QQ[a_(1,1)..a_(d,m)];
Ma = transpose genericMatrix(R,m,d);
A2 = wordAlgebra(m, CoefficientRing => R);
CMon = CAxisTensor(k, A2);
DMon = Ma * CMon; 
parMon = tensorParametrization(DMon,CoefficientRing => QQ); --Parametrization of P_{2,4,2}
I = ker parMon;
assert (dim I == 4)
assert (degree I == 24) --Variety has affine dimension 4 and degree 24
///



