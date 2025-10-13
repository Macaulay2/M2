doc ///
    Key
        CpMackeyFunctor
        (symbol ==, CpMackeyFunctor, CpMackeyFunctor)
        (directSum,CpMackeyFunctor)
        (net, CpMackeyFunctor)
    Headline
        The type of Cp Mackey functors
    Description
        Text
            This documents the @TO2(Type,"type")@ of $C_p$-Mackey functors. A $C_p$-Mackey functor is encoded as a @TO2(HashTable, "hash table")@ with keys through which we can pull various parts of the data.
        Text
            The keys available (internally) in the hash table are as follows, together with their external {\tt get} methods:

            @UL {
                (TT ".PrimeOrder", " yields the ", TO2(ZZ,"order"), " of the cyclic group over which we're working. See ", TO("PrimeOrder")),
                (TT ".Underlying",
                " encodes the ", TO "module", " ",
                TEX"$M(C_p/e)$", ". See ", TO("Underlying")),
                (TT ".Fixed", " encodes the ",TO "module", " ", TEX"$M(C_p/C_p)$",". See ", TO("Fixed")),
                (TT ".Res"," encodes the restriction ", TO2("Macaulay2Doc :: module homomorphisms","homomorphism")," ",
                TEX"$M(C_p/C_p) \\to M(C_p/e)$",". See ", TO("Res")),
                (TT ".Trans",
                " encodes the transfer ", TO2("Macaulay2Doc :: module homomorphisms","homomorphism")," ",
                TEX"$M(C_p/e) \\to M(C_p/C_p)$",". See ", TO("Trans")),
                (TT ".Conj",
                " encodes the conjugation ", TO2("Macaulay2Doc :: module homomorphisms","homomorphism")," ",
                TEX"$M(C_p/e) \\to M(C_p/e)$",". See ", TO("Conj")),
            }@

            {\bf Constructing new Mackey functors:} See @TO("constructing examples of Mackey functors")@.

            {\bf Operations with Mackey functor types:} One of the first operations to know about is @TO2((prune,CpMackeyFunctor),"pruning")@ a $C_p$-Mackey functor. This basically takes the data of a Mackey functor and attempts to simplify it and make it more reader-friendly. Pruning is the only unary operation on $C_p$-Mackey functors that we consider. The $0$-ary (@TO("Boolean")@-valued) operations are

            @UL{
                (TO("isTrivialMackeyFunctor")," checks if a Mackey functor is trivial"),
                (TO("isCohomological")," checks if a Mackey functor is ", EM "cohomological")
            }@

            For binary (and $n$-ary) operations, there are a few, including:

            @UL {
                (TO2("==","equality"), " of two Mackey functors, via ", TT"M==N"),
                ("the ", TO2(boxProduct,"box product"), " of two Mackey functors, via  ", TT "M**N"),
                ("the ", TO2(directSum,"direct sum")," of two Mackey functors, as ", TT"M++N"),
                ("the ", TO2(internalHom,"internal hom"), " of two Mackey functors")
            }@

            {\bf Well-definedness:} The global variable @TO("assertLevel")@ controls whether the methods that construct Mackey functors will check whether the input data is well-defined. If @TO("assertLevel")@ is set to 0 or lower, then no checks are made, and the methods will return a Mackey functor (which may be malformed) regardless of the input data. If @TO("assertLevel")@ is set to 1 or higher, then the methods will check whether the input data yields a well-defined Mackey functor, and if not, they will throw an error.
    SeeAlso
        "background on Mackey functors"
        "list of common Mackey functors"
        MackeyFunctorHomomorphism
///

doc ///
    Key
        makeCpMackeyFunctor
        (makeCpMackeyFunctor,ZZ,Matrix,Matrix,Matrix)
    Headline
        make a Cp Mackey functor
    Usage
        makeCpMackeyFunctor(p,R,T,C)
    Inputs
        p : ZZ
            a prime number
        R : Matrix
            a restriction homomorphim
        T : Matrix
            a transfer homomorphim
        C : Matrix
            a conjugation homomorphim
    Outputs
        : CpMackeyFunctor
    Description
        Text
            Builds an instance of the @TO("CpMackeyFunctor")@ type from the data of a prime number and three matrices, encoding restriction, transfer, and conjugation, respectively. If @TO("assertLevel")@ is greater than 0, the method will verify whether the input data yields a valid Mackey functor.
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///

doc ///
    Key
        (prune,CpMackeyFunctor)
    Headline
        prune a CpMackeyFunctor
    Description
        Text
            Outputs a more user-friendly version of a @TO2(CpMackeyFunctor,"Mackey functor")@.
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///

doc ///
    Key
        isCohomological
        (isCohomological,CpMackeyFunctor)
    Headline
        checks if a CpMackeyFunctor is cohomological
    Usage
        isCohomological M
    Inputs
        M : CpMackeyFunctor
    Outputs
        : Boolean
            whether $M$ is {\em cohomological} or not
    Description
        Text
            A Mackey functor $M$ defined over $C_p$ is said to be {\em cohomological} if the composite of restriction followed by transfer is identical to multiplication by $p$ as a map from the @TO2(Fixed,"fixed module")@ of $M$ to itself. We consider the {\em category of cohomological Mackey functors} to be the full subcategory on cohomological Mackey functors. An important result is the following:

            {\bf Theorem:} [TW95, 16.3] Every cohomological Mackey functor is a module over the @TO2(makeFixedPointMackeyFunctor,"fixed point Mackey functor")@ $\underline{\ZZ}$, and every @TO2(MackeyFunctorHomomorphism,"homomorphism")@ between cohomological Mackey functors is a $\underline{\ZZ}$-module homomorphism.

            Another way to say this is that the category of cohomological Mackey functors is the abelian subcategory $\text{Mod}_{\underline{\ZZ}}\subseteq \text{Mack}_{C_p}$. Carrying out computations (for instance Ext and Tor) internal to the abelian subcategory $\text{Mod}_{\underline{\ZZ}}$ leads to what are called @TO2(ExtCoh,"cohomological Ext")@ and @TO2(TorCoh,"cohomological Tor")@.

            {\bf References:}

            @UL{
                ("[TW95] J. Thevenaz & P. Webb, ", EM"The structure of Mackey functors", " Trans. Amer. Math. Soc., 1995.")
            }@

    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///

doc ///
    Key
        drawVerticalCpMackeyFunctor
        (drawVerticalCpMackeyFunctor,CpMackeyFunctor)
    Headline
        reorients the net of a Mackey functor vertically
    Usage
        drawVerticalCpMackeyFunctor M
    Inputs
        M : CpMackeyFunctor
    Outputs
        : Net
    Description
        Text
            Reorients the @TO2(Net,"net")@ of a Mackey functor vertically
        Example
            M = makeRandomCpMackeyFunctor(2)
            drawVerticalCpMackeyFunctor M
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///


doc ///
    Key
        PrimeOrder
    Headline
        the prime order of the group over which a Mackey functor is defined
    Usage
        M.PrimeOrder
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///

doc ///
    Key
        Underlying
    Headline
        the underlying module of a Mackey functor
    Usage
        M.Underlying
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///

doc ///
    Key
        Fixed
    Headline
        the fixed module of a Mackey functor
    Usage
        M.Fixed
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///

doc ///
    Key
        Res
    Headline
        the restriction homomorphism in a Mackey functor
    Usage
        M.Res
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///

doc ///
    Key
        Trans
    Headline
        the transfer homomorphism in a Mackey functor
    Usage
        M.Trans
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///

doc ///
    Key
        Conj
    Headline
        the conjugation homomorphism in a Mackey functor
    Usage
        M.Conj
    SeeAlso
        "background on Mackey functors"
        CpMackeyFunctor
///