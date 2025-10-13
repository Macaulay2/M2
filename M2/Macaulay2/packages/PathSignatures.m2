newPackage("PathSignatures",
         Version => "1.0",
         Authors => {{Name => "Felix Lotter", HomePage => "https://felixlotter.gitlab.io"}, {Name => "Oriol Reig"}, {Name => "Angelo El Saliby"}, {Name => "Carlos Amendola"}},
         Headline => "working with algebraic path signatures",
         AuxiliaryFiles => true,
         Keywords => {"Applied Algebraic Geometry"},
         PackageExports => {"NCAlgebra", "Permutations"}
);
export {
    --types
    "Path",
    --methods
    "sig",
    "polyPath",
    "linPath",
    "pwLinPath",
    "concatPath",
    "matrixAction",
    "CAxisTensor",
    "CMonTensor",
    "tensorParametrization",
    "wordAlgebra",
    "sgnVolTensor",
    "shuffle",
    "halfshuffle",
    "wordFormat",
    "wordString",
    "getDimension",
    "getPieces",
    "getCoefficientRing",
    "getNumberOfPieces",
    -- symbols
    "adjointWord",
    "tensorArray",
    "inner",
    "lyndonWords",
    "lie",
    "lieBasis",
    "tensorExp",
    "tensorLog",
    "lyndonShuffle",
    "VarWordTable"
    -- "type",
    -- "pieces",
    -- "dimension",
    -- "numberOfPieces",
    -- "bR"
};

exportFrom("NCAlgebra",{"NCRingElement", "NCPolynomialRing"})

--------------------------------------------
--Include interface for NCAlgebra
load "./PathSignatures/interfaceNCAlgebra.m2"

--Include tensor algebra
load "./PathSignatures/algebra.m2"

--Include types
load "./PathSignatures/types.m2"

--Include signatures
load "./PathSignatures/signatures.m2"

--Include documentation
load "./PathSignatures/documentation.m2"
--------------------------------------------

----------------------------------------------------
--Landing page and case use documentation
----------------------------------------------------
beginDocumentation()
 doc ///
 Node
    Key
        PathSignatures
    Headline
        a package for working with signatures of algebraic paths
    Description
        Text
            {\em PathSignatures} is a package for studying the signature of piecewise polynomial paths.
        Text
            The package heavily simplifies the process of obtaining data related to signature varieties, e.g. as in @HREF("#ref1","[1]")@. See @TO "Computing Path Varieties"@.
        Text
            A polynomial path is a path $X: [0,1] \to \mathbb R^d$ whose coordinate functions are given by polynomials. A piecewise polynomial path is a path $X: [0,1] \to \mathbb R^d$ which is polynomial on each interval in a partition of $[0,1]$.
        Text
            Given such a path $X$, its signature is the linear form $\sigma: T((\mathbb{R}^d)^*)\rightarrow \mathbb{R}$ on the tensor algebra of the dual of 
            $\mathbb{R}^d$, whose image on a decomposable tensor $\alpha_1\otimes \dots\otimes \alpha_k$ is the iterated integral $$
            \alpha_1\otimes \dots\otimes \alpha_k\overset{\sigma}{\mapsto} \int_0^1\int_0^{t_k}\dots\int_0^{t_2}\partial(\alpha_1 X)\dots \partial (\alpha_k X) d t_1\dots dt_k.
            $$
            This form is invariant under translation, reparametrization and tree-like equivalence of $X$ and characterizes $X$ uniquely up to these relations.
        Text
            In this package, we identify $T((\mathbb{R}^d)^*)$ with the free associative algebra over the alphabet $\{\texttt{1},\dots,\texttt{d}\}$ via $\texttt{i} \mapsto e_i^*$ where $e_1^*, \dots, e_d^*$ is the dual of the canonical basis of $\mathbb{R}^d$. For example, the word $\texttt{12}$ corresponds to $e_1^* \otimes e_2^*$.
        Text
            It is easy to create a polynomial path:
        Example
            R = QQ[t];
            X = polyPath({t + t^2, t^3})
        Text
            A piecewise polynomial path is obtained by concatenating polynomial paths:
        Example
            Y = X ** X
        Text
            Any @TO2 {"NCAlgebra::NCPolynomialRing", "NCPolynomialRing"}@ can serve as a tensor algebra. Use @TO wordAlgebra@ to quickly create one in variables $\texttt{Lt}_i$. The package introduces a convenient notation for words in this algebra.
        Example
            A2 = wordAlgebra(2)
            [1,2]_A2 -- the word 12.
        Text
            To evaluate the signature of $\mathtt{X}$ at a tensor $\mathtt{w}$, use @TO sig@. The following computes the @ITALIC "signed volume"@ of the path; also see @TO sgnVolTensor@.
        Example
            sig(X,[1,2]_A2-[2,1]_A2)
        Text
            @TO sig@ can also be used to obtain the @ITALIC "$k$-th level signature tensor"@. Use @TO wordFormat@ or @TO tensorArray@ to display the tensor in a nicer way.
        Example
            T = sig(X,2)
            T // wordFormat
            T // tensorArray
        Text
            The package allows for the computation of signatures for parametrized families of paths.
        Example
            S = QQ[a,b,c]
            R = S[t]
            X = polyPath({a*t+b*t^2,c*t^3})
            Y = X ** X
            sig(X, sgnVolTensor(A2))
    References
        @LABEL("[1]","id" => "ref1")@ Améndola, C., Friz, P., & Sturmfels, B. (2019, January). Varieties of signature tensors. In Forum of Mathematics, Sigma (Vol. 7, p. e10). Cambridge University Press.
            
           
Node
    Key
        "Computing Path Varieties"
    Description
        Text
            @TO PathSignatures@ simplifies the computation of varieties coming from signature tensors. We showcase this in a number of examples:
    Subnodes
        "Polynomial paths of degree m"
        "A family of paths on a cone"
        "The universal variety and toric coordinates"
Node
    Key
        "Polynomial paths of degree m"
    Description
        Text
            We reproduce a computation from @HREF("#ref1","[1]")@.
            We briefly recall the setting for this computation. We are interested in paths $X:[0,1]\rightarrow \mathbb{R}^\mathtt{d}$ whose coordinates are polynomials of degree $\mathtt{m}$. These can be 
            represented by a $\mathtt{d}\times \mathtt{m}$ matrix with real entries whose coordinates are determined by the expressions 
            $$ X_i(t) = x_{i,1}t+x_{i,2}t^2+\dots+ x_{i,m}t^m$$
            When we restrict to the $\mathtt{k}$-th level signature of $X$, $\sigma:= \sigma^{(\mathtt{k})}(X)$, each of its coordinates $\sigma_{i_1, \dots, i_{\mathtt{k}}}$ is a homogeneous 
            polynomial of degree $\mathtt{k}$ in the $\mathtt{d}\cdot \mathtt{m}$ unknowns $x_{i,j}$ corresponding to the matrix representation of the path $X$. Let $\mathtt{CMon}$ be the canonical monomial path
            in $\mathbb{R}^{\mathtt{m}}$ introduced in @TO CMonTensor@. Then $\sigma$ can be computed through the @TO matrixAction@ of $X$ on $\sigma^{(k)}(\mathtt{CMon})$.
            Moreover, we can view the entries of $X$ as coordinates in the projective space $\mathbb{P}^{\mathtt{d}\cdot \mathtt{m}-1}$ over some algebraically closed field $\mathbb{K}$ containing 
            $\mathbb{R}$. Then the matrix action just described gives rise to a rational map $$
            \sigma^{(\mathtt{k})}:\mathbb{P}^{\mathtt{d}\cdot \mathtt{m}-1}\rightarrow \mathbb{P}^{\mathtt{d}^{\mathtt{k}}-1}$$
            determined by $X\mapsto \sigma^{(\mathtt{k})}(X)$, of degree $\mathtt{k}$. The {\em polynomial signature variety}, denoted $\mathcal{P}_{\mathtt{d},\mathtt{k},\mathtt{m}}$, is then defined to be 
            the Zariski closure of the image of this map (informally, the closure of the space of all tensors of order $\mathtt{k}$ that arise as signatures of paths of the specified type), while its homogeneous prime ideal is called 
            {\em polynomial signature ideal} and denoted $P_{{\mathtt{d},\mathtt{k},\mathtt{m}}}$.
        Text
            We set up the procedure to compute the ideal $P_{\mathtt{d},\mathtt{k},\mathtt{m}}$.
        Example
            d = 2; k = 3; m = 2;
        Text
            We create a ring $\mathtt{R}$ with $\mathtt{d}\cdot\mathtt{m}$ variables, corresponding to the entries of a path. Then we create the free algebra $\mathtt{A}$ over $\mathtt{R}$ with $\mathtt{m}$ generators, where we can compute the $\mathtt{k}$-th level signature of the canonical monomial path in $\mathbb{R}^{\mathtt{m}}$.
        Example
            R = CC[x_1..x_(d*m)]; --x_1, ..., x_(d*m) are the entries of the degree m paths in d-dimensional space, seen as matrices
            A = wordAlgebra(m, CoefficientRing => R); --Signatures in m-dimensional space, where the signature of CMon lives.
            sigmaCMon = CMonTensor(k, A); sigmaCMon // wordFormat -- The 2nd level signature of CMon
        Text
            Next we create the @TO genericMatrix@ with $\mathtt{d}\times \mathtt{m}$ variables.
        Example
            M = genericMatrix (R, d, m)
        Text
            Finally we compute the matrix action on $\sigma^{(k)}(\mathtt{CMon})$ with @TO (symbol *, Matrix, NCRingElement)@, and then compute the corresponding map on rings using @TO tensorParametrization@.
        Example 
            f = M * sigmaCMon; 
            sigVarietyParam = tensorParametrization(f, CoefficientRing => CC);
        Text
            Now that we have the map, any tool for implicitization can be used. We compute its dimension and degree with @TO2 {"NumericalImplicitization::NumericalImplicitization", "NumericalImplicitization"}@.
        Example
            needsPackage "NumericalImplicitization";
            numericalImageDim(sigVarietyParam,ideal 0_R) 
            numericalImageDegree(sigVarietyParam,ideal 0_R, Verbose => false) 
        Text
            This agrees with the result in Table 3 of @HREF("#ref1","[1]")@, where dimension and degree of the corresponding projective variety is computed.
    References
        @LABEL("[1]","id" => "ref1")@ Améndola, C., Friz, P., & Sturmfels, B. (2019, January). Varieties of signature tensors. In Forum of Mathematics, Sigma (Vol. 7, p. e10). Cambridge University Press.

Node
    Key
        "A family of paths on a cone"
    Description
        Text
            We consider the following family of polynomial paths of degree 6:
        Example
            S = QQ[a_1..a_6]
            R = S[t]
            u = a_1*t + a_2*t^2 + a_3*t^3;
            v = a_4*t + a_5*t^2 + a_6*t^3;
            X = polyPath({u^2 - v^2, 2*u*v, u^2 + v^2});
        Text
            Let us take a look at its signature matrix variety. We obtain its parametrization as follows:
        Example
            sigMatrix = sig(X,2);
            S = QQ[s_(1,1)..s_(3,3)]
            vwtable = hashTable apply(gens S, i-> (i, new Array from last baseName i))
            m = tensorParametrization(sigMatrix, VarWordTable => vwtable);
        Text
            Let us use numericalImplicitization to obtain information about the dimension of the image.
        Example
            needsPackage "NumericalImplicitization";
            Snum = CC[a_1..a_6];
            Rnum = Snum[t];
            unum = sub(u, Rnum);
            vnum = sub(v, Rnum);
            Xnum = polyPath({unum^2 - vnum^2, 2*unum*vnum, unum^2 + vnum^2});
            sigMatrixnum = sig(Xnum,2);
            mnum = tensorParametrization(sigMatrixnum,CoefficientRing => CC);
            numericalImageDim(mnum,ideal 0_Snum)
        Text
            The universal variety has dimension 6, so we expect at least one additional relation. We use MultigradedImplicitization:
        Example
            needsPackage "MultigradedImplicitization";
            I = sub(ideal flatten values componentsOfKernel(2, m, Grading => matrix {toList(9:1)}), S);
            dim I
            isPrime I
            betti mingens I
            degree I
        Text
            We conclude that our variety is cut out by one linear relation and 6 quadrics. Let us take a look at the linear relation:
        Example
            lin = select(flatten entries gens I, i-> (degree i == {1}))
        Text
            We recognize it as a shuffle polynomial in the letters $\mathtt{1},\mathtt{2},\mathtt{3}$. It corresponds to the constraint $X(1) - X(0) \in V(x^2 + y^2 - z^2)$ for paths in our family.
        Text
            Recall that the universal variety is cut out by the 2-minors of the symmetric part of the matrix. We check if the linear relation is the only additional one on our path family:
        Example
            A = genericMatrix(S,3,3)
            univI = minors(2, A + transpose(A));
            I == univI + ideal lin

Node
    Key
        "The universal variety and toric coordinates"
    Description
        Text
            Recall that $K \langle \mathtt{1}, \ldots, \mathtt{d} \rangle$ is isomorphic to the free commutative algebra over the Lyndon words and if we grade each word by its length, the algebra homomorphism
            $$\phi: K[\mathtt{w} \ | \ \mathtt{w} \ \mathrm{Lyndon}] \cong K \langle \mathtt{1}, \ldots, \mathtt{d} \rangle, \ \mathtt{w} \mapsto \mathtt{w}$$
            is an isomorphism of graded vector spaces. The inverse $\psi$ of this isomorphism is computed by @TO lyndonShuffle@. Dually, we have the isomorphism of graded vector spaces
            $$\psi^*: K[\mathtt{w} \ | \ \mathtt{w} \ \mathrm{Lyndon}]^* \cong K \langle \mathtt{1}, \ldots, \mathtt{d} \rangle^*, \ \alpha \mapsto \alpha \circ \psi.$$
        Text
            We view the two vector spaces $K\langle \mathtt{1}, \ldots, \mathtt{d}\rangle^*$ and $K[\mathtt{w} \ | \ \mathtt{w} \ \mathrm{Lyndon}]^*$ as infinite-dimensional affine spaces. We are interested in the subset $\mathcal U_d$ of those points of $K\langle \mathtt{1}, \ldots, \mathtt{d}\rangle^*$ that define shuffle algebra homomorphisms. This is a Zariski closed set. As $\psi$ is an algebra homomorphism, they correspond to the points of $K[\mathtt{w} \ | \ \mathtt{w} \ \mathrm{Lyndon}]^*$ under $\psi^*$ that define algebra homomorphisms; these are parametrized by the points of the vector space $K^{\mathcal L}$, where $\mathcal L$ is the set of Lyndon words, via the map
            $$\eta: K^{\mathcal L} \to K[\mathtt{w} \ | \ \mathtt{w} \ \mathrm{Lyndon}]^*, \ x \mapsto ev_x.$$
            In particular, $\mathcal U_d$ is parametrized by $\psi^* \circ \eta, \ x \mapsto (\mathtt{w} \mapsto \psi(\mathtt{w})(x))$.
        
            Projecting $\mathcal U_d$ to the degree $k$ component, we obtain a subvariety of $((K^d)^{\otimes k})^* \cong (K^d)^{\otimes k}$, called the universal variety, $\mathcal U_{d,k}$. 
            
            As the map $\psi$ is compatible with the projection, $\psi^* \circ \eta$ restricts to a parametrization of $\mathcal U_{d,k}$: it is the image of the induced morphism
            $$K^{\mathcal L_k} \to (K\langle \mathtt{1}, \ldots, \mathtt{d}\rangle_k)^* \cong (K^d)^{\otimes k}, \ x \mapsto (\psi(\mathtt{w})(x) \cdot \mathtt{w})$$
            where $\mathcal L_k$ is the set of Lyndon words of length at most $k$.

            As usual, we can compute the ideal that cuts out the image variety as the kernel of the corresponding ring map $$K[x_{\mathtt w} \ | \ \mathtt{w} \text{ of length } k] \to K[y_{\mathtt w} \ | \ \mathtt{w} \text{ Lyndon of length } \leq k].$$ This map can be computed via @TO lyndonShuffle@. Let us do this in the example $d=3, k=3$.
        Example
            words = toList apply((3:1)..(3:3), i -> new Array from i);
            lwords = lyndonWords(3,3)
            R = QQ new Array from apply(words, i->x_i);
            Q = QQ new Array from (apply(lwords,i->y_i) | {Degrees => apply(lwords, i->length(i))});
            A3 = wordAlgebra(3);
            lpolyHT = apply(words, i -> lyndonShuffle(i_A3));
            lpols = apply(lpolyHT, f -> sum(pairs f, (term,coef) -> coef * product(pairs term, (word,ex)-> y_word^ex)));
            lpols_{0..4}
            m = map(Q,R,lpols);
        Text
            Let us compute the kernel.
        Example
            I = ker m;
            dim I
            degree I
            betti mingens I
            (mingens I)_(0,0)
        Text
            This agrees with the result in Table 2 of @HREF("#ref1","[1]")@.
        Text
            As $\phi$ is an isomorphism of graded vector spaces, we see that the variety $\mathcal U_{d,k}$ is parametrized by the vector of degree $k$ monomials in Lyndon words after a linear coordinate change on $(K^d)^{\otimes k}$. We can use this to simplify the computation of the universal variety.
        Example
            mons = flatten entries basis(3,Q);
            S = QQ[z_1..z_(length mons)];
            m = map(Q,S,mons);
            I = ker m;
            dim I
            degree I
            betti mingens I
        Text
            Let us compute the matrix of the coordinate change for $d=2, k=4$.
        Example
            words = toList apply((4:1)..(4:2), i-> new Array from i);
            lwords = lyndonWords(2,4);
            R = QQ new Array from apply(words,i->x_i);
            Q = QQ new Array from (apply(lwords,i->y_i) | {Degrees => apply(lwords, i->length(i))});
            A2 = wordAlgebra(2);
            lpolyHT = apply(words, i -> lyndonShuffle(i_A2));
            lpols = apply(lpolyHT, f -> sum(pairs f, (term,coef) -> coef * product(pairs term, (word,ex)-> y_word^ex)));
            mons = basis(4,Q)
            M = sub(matrix apply(lpols, i -> (flatten entries (coefficients(i, Monomials => mons))#1) ),QQ)
            M^(-1)
        Text
            Note that the coordinate change differs from the one described in Example 21 of @HREF("#ref2","[2]")@ as our toric parametrization does not arise from the exponential map on the Lie algebra. We can easily construct this coordinate change as well, by using @TO lieBasis@ and @TO tensorExp@:
        Example
            A2 = wordAlgebra(2, CoefficientRing => Q)
            lbasis = apply(lwords, i -> lieBasis(i,A2));
            lT = sum(0..length(lbasis)-1, i-> Q_i * lbasis_i);
            gT = tensorExp(lT,4);
            lpols = apply(words, i-> gT @ i_A2)
            M = sub(matrix apply(lpols, i -> (flatten entries (coefficients(i, Monomials => mons))#1) ),QQ);
            M^(-1)
        Text
            This is the matrix from Example 21 in @HREF("#ref2","[2]")@. Note that while we obtained the coordinate change by inverting the map that sends a word to its coefficient in the exponential (which is a linear combination of Lyndon word monomials), in @HREF("#ref2","[2]")@ the coordinate change is obtained directly without computing the exponential. Both strategies yield the same result by Lemma 18 in loc. cit..
        -- Text
        --     Let us compute a path variety after toric coordinate change.
        -- Example
        --     S = QQ[a_(1,1)..a_(3,2)]
        --     A2 = wordAlgebra(3, CoefficientRing => S)
        --     T = CAxisTensor(4,A2)
        --     A = genericMatrix(S,2,3)
        --     m = tensorParametrization(A * T, VarWordTable => hashTable apply(gens R, i-> (i, last baseName i)))
        --     cc = map(R,R,flatten entries (M^(-1) * (transpose basis(1,R))))
        --     mcc = m * cc;
        --     needsPackage "MultigradedImplicitization"
        --     I = ideal componentOfKernel({2},source mcc,mcc)
        --     J = ideal componentOfKernel({3},source mcc,mcc)
        --     dim (I+J)
        --     isPrime (I+J)
    References
        @LABEL("[1]","id" => "ref1")@ Améndola, C., Friz, P., & Sturmfels, B. (2019, January). Varieties of signature tensors. In Forum of Mathematics, Sigma (Vol. 7, p. e10). Cambridge University Press.

        @LABEL("[2]","id" => "ref2")@ Galuppi, F. (2019). The rough Veronese variety. Linear algebra and its applications, 583, 282-299.
///


endPackage;












