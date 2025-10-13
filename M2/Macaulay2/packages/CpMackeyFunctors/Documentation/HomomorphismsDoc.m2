doc ///
    Key
        MackeyFunctorHomomorphism
        (symbol -, MackeyFunctorHomomorphism)
        (symbol ==, MackeyFunctorHomomorphism, MackeyFunctorHomomorphism)
        (net, MackeyFunctorHomomorphism)
        (symbol *, ZZ, MackeyFunctorHomomorphism)
        (prune, MackeyFunctorHomomorphism)
        (symbol ^, MackeyFunctorHomomorphism, ZZ)
        (symbol -, MackeyFunctorHomomorphism, MackeyFunctorHomomorphism)
        (symbol +, MackeyFunctorHomomorphism, MackeyFunctorHomomorphism)
        (inverse, MackeyFunctorHomomorphism)
        (directSum,MackeyFunctorHomomorphism)
        (symbol ++, MackeyFunctorHomomorphism,MackeyFunctorHomomorphism)
    Headline
        the class of Mackey functor homomorphisms
    Description
        Text
            The @TO2(Type,"type")@ of a {\em Mackey functor homomorphism} (for definitions, see @TO("background on Mackey functors")@). A Mackey functor homomorphism can be constructed with an implementation of the @TO2((map,CpMackeyFunctor,CpMackeyFunctor,Matrix,Matrix),"map")@ method in this package. Given a Mackey functor homomorphism @TT("f")@, the data for this type can be accessed in the following way:

            @UL {
                (TT "target f", " -- yields the codomain/target of the homomorphism. See ", TO2((target, MackeyFunctorHomomorphism),"target")),
                (TT "source f", " -- yields the domain/source of the homomorphism. See ", TO2((source, MackeyFunctorHomomorphism),"source")),
                (TT "f.UnderlyingMap", " -- yields the induced ", TO2("Macaulay2Doc :: module homomorphisms","homomorphism")," on the ", TO2(CpMackeyFunctor,"underlying modules"),". See ", TO("UnderlyingMap")),
                (TT "f.FixedMap", " -- yields the induced ", TO2("Macaulay2Doc :: module homomorphisms","homomorphism")," on the ", TO2(CpMackeyFunctor,"fixed modules"), ". See ", TO("FixedMap")),
            }@

            ({\bf Warning}: the following method @TO("makeRandomMackeyFunctorHomomorphism")@ takes ({\em source,target}) as input, which follows the @TO("Hom")@ convention rather than the @TO("map")@ convention):
        Example
            M = makeRandomCpMackeyFunctor(2);
            N = makeRandomCpMackeyFunctor(2);
            f = makeRandomMackeyFunctorHomomorphism(M,N)
        Text
            There are a few ways to build Mackey functor homomorphisms, for example we can take the @TO2(id,"identity")@ on any Mackey functor
        Example
            id_(M)
        Text
            The only $0$-ary operation implemented for Mackey functor homomorphisms is:

            @UL{
                (TO2((isIsomorphism,MackeyFunctorHomomorphism),"isIsomorphism"), " checking if a morphism is an isomorphism")
            }@

            The unary, binary, and $n$-ary operations are as follows:

            @UL{
                (BOLD "inversion:", " if ", TEX"$f$", " is an ", TO2((isIsomorphism,MackeyFunctorHomomorphism),"isomorphism"), " we can invert it via ", TT"f^-1", " or using ", TT"inverse(f)"),
                (BOLD "negating:", " we can take the negative of any Mackey functor homomorphism as ", TT"-f"),
                (TO2("==","equality"), " of two Mackey functors homomorphisms, via ", TT"f==g"),
                (BOLD "powers:", " given an endomorphism of a Mackey functor ", TEX"$f\\colon M \\to M$", " we can take iterated composition of it as ", TT"f^n"),
                (BOLD "addition:", " we can add two Mackey functor homomorphisms with the same domain and codomain via ", TT"f+g"),
                (BOLD "subtraction:", " we can subtract two Mackey functor homomorphisms with the same domain and codomain via ", TT"f-g"),
                (BOLD "iterated addition:", " we can add a Mackey functor homomorphism ", TT"f", " with itself ", TT"n", " times via ", TT"n*f"),
                (BOLD "composition:", " we can compose two composable Mackey functor homomorphisms via ", TT"g*f"),
                (BOLD "block sum:", " given two Mackey functor homomorphisms ", TEX"$f\\colon M_1 \\to N_1$", " and ", TEX"$g\\colon M_2 \\to N_2$", " we can obtain their ", EM"block sum", " ", TEX"$f\\oplus g \\colon M_1 \\oplus M_2 \\to N_1 \\oplus N_2$", " as ", TT"f++g")
            }@

    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor

///
-- ^^ There must be a better way!

doc ///
    Key
        FixedMap
    Headline
        the fixed-point level of a Mackey functor homomorphism
    Usage
        F.FixedMap
    SeeAlso
        "background on Mackey functors"
        MackeyFunctorHomomorphism
///

doc ///
    Key
        UnderlyingMap
    Headline
        the underlying level of a Mackey functor homomorphism
    Usage
        F.UnderlyingMap
    SeeAlso
        "background on Mackey functors"
        MackeyFunctorHomomorphism
///

doc ///
    Key
        (map,CpMackeyFunctor, CpMackeyFunctor, Matrix, Matrix)
        [map,Degree]
        [map,DegreeLift]
        [map,DegreeMap]
    Headline
        constructs a map between two Mackey functors
    Usage
        map(N,M,U,F)
    Inputs
        N : CpMackeyFunctor
        M : CpMackeyFunctor
        U : Matrix
        F : Matrix
    Outputs
        : MackeyFunctorHomomorphism
            the morphism of Mackey functors which is F at the fixed point level and U at the underlying level.
    Description
        Text
            A morphisms of $C_p$-Mackey functors consists of a group homomorphism on the fixed and underlying levels which commutes with the transfer, restriction, and conjugation morphisms. If @TO("assertLevel")@ is greater than 0, this method will check if the input data yields a well-defined homomorphism (and will throw an error if not).
        Example
            map(makeComplexRepresentationMackeyFunctor 3, makeBurnsideMackeyFunctor 3, matrix {{1}}, matrix {{1,1},{0,1},{0,1}})
    SeeAlso
        "background on Mackey functors"
        MackeyFunctorHomomorphism
///

doc ///
    Key
        (map, CpMackeyFunctor, CpMackeyFunctor, ZZ)
    Headline
        constructs the zero map between two Mackey functors, or a multiple of the identity map
    Usage
        map(N,M,0)
        map(M,M,n)
    Inputs
        N : CpMackeyFunctor
            target
        M : CpMackeyFunctor
            source
        n : ZZ
    Outputs
        : MackeyFunctorHomomorphism
            the map $n*\mathrm{id}$ from M to N, if such a thing exists.
    SeeAlso
        "background on Mackey functors"
        MackeyFunctorHomomorphism
        (map,CpMackeyFunctor, CpMackeyFunctor, Matrix, Matrix)
///


doc ///
    Key
        (source, MackeyFunctorHomomorphism)
    Headline
        returns the source of a Mackey functor homomorphism
    Usage
        source(F)
    Inputs
        F : MackeyFunctorHomomorphism
    Outputs
        : CpMackeyFunctor
            the source of the homomorphism F.
    Description
        Text
            A homomorphism between Mackey functors has a source and a target. This method returns the source.
        Example
            source(complexLinearizationMap(5))
    SeeAlso
        "background on Mackey functors"
        MackeyFunctorHomomorphism
///

doc ///
    Key
        (target, MackeyFunctorHomomorphism)
    Headline
        returns the target of a Mackey functor homomorphism
    Usage
        target(F)
    Inputs
        F : MackeyFunctorHomomorphism
    Outputs
        : CpMackeyFunctor
            the target of the homomorphism F.
    Description
        Text
            A homomorphism between Mackey functors has a source and a target. This method returns the target.
        Example
            target(complexLinearizationMap(5))
    SeeAlso
        "background on Mackey functors"
        MackeyFunctorHomomorphism
///

doc ///
    Key
        complexLinearizationMap
        (complexLinearizationMap, ZZ)
    Headline
        returns the complex linearization map for the prime p
    Usage
        complexLinearizationMap(p)
    Inputs
        p : ZZ
    Outputs
        : MackeyFunctorHomomorphism
            the linearization map from the Burnside Mackey functor to the complex representation functor for the prime p.
    Description
        Text
            Every $G$-set $X$ determines a complex representation with basis $X$, called the permutation representation of $X$.  This induces a map from the @TO2(makeBurnsideMackeyFunctor,"Burnside Mackey functor")@ to the @TO2(makeComplexRepresentationMackeyFunctor,"complex representation Mackey functor")@ called the linearization map.
        Example
            complexLinearizationMap(5)
    SeeAlso
        MackeyFunctorHomomorphism
        realLinearizationMap
///

doc ///
    Key
        realLinearizationMap
        (realLinearizationMap, ZZ)
    Headline
        returns the real linearization map for the prime p
    Usage
        realLinearizationMap(p)
    Inputs
        p : ZZ
    Outputs
        : MackeyFunctorHomomorphism
            the linearization map from the Burnside Mackey functor to the real representation functor for the prime p.
    Description
        Text
            Every $G$-set $X$ determines a real representation with basis $X$, called the permutation representation of $X$.  This induces a map from the Burnside Mackey functor to the real representation Mackey functor called the linearization map.
        Example
            realLinearizationMap(5)
    SeeAlso
        MackeyFunctorHomomorphism
        complexLinearizationMap
///

doc ///
    Key
        (symbol |, MackeyFunctorHomomorphism,MackeyFunctorHomomorphism)
    Headline
        Horizontal concatenation of Mackey functor homomorphisms
    Usage
        f | g
    Outputs
        : MackeyFunctorHomomorphism
            the horizontal concatenation of the homomorphisms $f$ and $g$.
    Inputs
        f : MackeyFunctorHomomorphism
        g : MackeyFunctorHomomorphism
    Description
        Text
            Given two Mackey functor homomorphisms $f : A \to C$ and $g : B \to C$ with the same codomain, $f | g$ is the homomorphism $(f,g) : A \oplus B \to C$.
        Example
            A = makeBurnsideMackeyFunctor 2;
            id_A | id_A
    SeeAlso
        MackeyFunctorHomomorphism
        (symbol ||, MackeyFunctorHomomorphism,MackeyFunctorHomomorphism)
///

doc ///
    Key
        (symbol ||, MackeyFunctorHomomorphism,MackeyFunctorHomomorphism)
    Headline
        Vertical concatenation of Mackey functor homomorphisms
    Usage
        f || g
    Outputs
        : MackeyFunctorHomomorphism
            the vertical concatenation of the homomorphisms $f$ and $g$.
    Inputs
        f : MackeyFunctorHomomorphism
        g : MackeyFunctorHomomorphism
    Description
        Text
            Given two Mackey functor homomorphisms $f : A \to B$ and $g : A \to C$ with the same domain, $f || g$ is the homomorphism $(f,g) : A \to B \oplus C$.
        Example
            A = makeBurnsideMackeyFunctor 2;
            id_A || id_A
    SeeAlso
        MackeyFunctorHomomorphism
        (symbol |, MackeyFunctorHomomorphism,MackeyFunctorHomomorphism)
///

doc ///
    Key
        (isIsomorphism, MackeyFunctorHomomorphism)
    Headline
        checks if a Mackey functor homomorphism is an isomorphism
    Usage
        isIsomorphism F
    Inputs
        F : MackeyFunctorHomomorphism
    Outputs
        : Boolean
            whether $F$ is an isomorphism
    Description
        Text
            Checks whether a @TO2(MackeyFunctorHomomorphism,"Mackey functor homomorphism")@ is an isomorphism.
    SeeAlso
        "background on Mackey functors"
        MackeyFunctorHomomorphism
///


doc ///
    Key
        isTrivialMackeyFunctor
        (isTrivialMackeyFunctor,CpMackeyFunctor)
    Headline
        checks if a Mackey functor is trivial or not
    Usage
        isTrivialMackeyFunctor M
    Inputs
        M : CpMackeyFunctor
    Outputs
        : Boolean
            whether $M$ is trivial or not
    Description
        Text
            Checks whether $M$ is {\em trivial} (meaning isomorphic to the @TO2(makeZeroMackeyFunctor,"zero Mackey functor")@).

    SeeAlso
        "background on Mackey functors"
        makeZeroMackeyFunctor
///

doc ///
    Key
        (inducedMap, CpMackeyFunctor, CpMackeyFunctor)
        (inducedMap, CpMackeyFunctor, CpMackeyFunctor, MackeyFunctorHomomorphism)
        (inducedMap, CpMackeyFunctor, CpMackeyFunctor, Matrix, Matrix)
        [inducedMap,Degree]
        [inducedMap,Verify]
    Headline
        tries to induce a natural map between two Mackey functors
    Usage
        inducedMap(N,M)
        inducedMap(N,M,F)
        inducedMap(N,M,u,f)
    Inputs
        N : CpMackeyFunctor
            target
        M : CpMackeyFunctor
            source
    Outputs
        : MackeyFunctorHomomorphism
            a Mackey functor homomorphism from M to N
    Description
        Text
            If a homomorphism is given (either as a @TO(MackeyFunctorHomomorphism)@ or as a pair of matrices), this method will try to induce a map between the provided Mackey functors. If no homomorphism is given, the identity morphism on the @TO2((source,MackeyFunctorHomomorphism),"source")@ is used. A common use for this method to produce the natural inclusion of a subobject or the natural projection to a quotient object.
        Example
            f := complexLinearizationMap(3);
            inducedMap(coker f, target f)
    SeeAlso
        MackeyFunctorHomomorphism
        inducedMap
///