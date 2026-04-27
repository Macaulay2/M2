doc ///
    Key
        "the abelian category of Mackey functors"
    Headline
        background on the category of Mackey functors and how to code within it
    Description
        Text
            {\bf The category of Mackey functors:} Fixing a finite group $G$ (always cyclic of prime order in this package), we obtain a {\em category} $\text{Mack}_G$, whose objects are @TO2(CpMackeyFunctor,"Mackey functors")@ and whose morphisms are @TO2(MackeyFunctorHomomorphism,"Mackey functor homomorphisms")@. Given any Mackey functor $M$ we can take its {\em identity homomorphism} as follows:

        Example
            M = makeRandomCpMackeyFunctor(3,GenBound=>2);
            id_M

        Text
            @TO2((symbol *, MackeyFunctorHomomorphism,MackeyFunctorHomomorphism),"Composition")@  of Mackey functor homomorphisms is done exactly the same as for composition of maps. If $f\colon M \to N$ and $g \colon N \to Q$ are two Mackey functor homomorphisms, their composite $g\circ f \colon M \to Q$ can be accessed as @TT("g*f")@.

        Text
            {\bf The abelian category structure:} Given any two Mackey functors $M$ and $N$, we can form the set $\text{Hom}_{\text{Mack}_G}(M,N)$ of homomorphisms from $M$ to $N$. Since we can @TO2((symbol ++, MackeyFunctorHomomorphism,MackeyFunctorHomomorphism),"add")@ any two homomorphisms, we have that $\text{Hom}_{\text{Mack}_G}(M,N)$ is really an @TO2((Hom,CpMackeyFunctor,CpMackeyFunctor) ,"abelian group of homomorphisms")@. Composition is moreover $\ZZ$-linear, hence we can say that the category $\text{Mack}_G$ is {\em pre-additive}.

        Example
            Hom(makeRandomCpMackeyFunctor(2,GenBound=>2),makeRandomCpMackeyFunctor(2,GenBound=>2))
            prune oo

        Text
            Given any two Mackey functors, we can take their @TO2(directSum,"direct sum")@ and obtain another Mackey functor. This is a {\em finite biproduct} on the category $\text{Mack}_G$, in other words it is both a product and a coproduct. There is a zero object for this biproduct, namely the @TO2(makeZeroMackeyFunctor,"zero Mackey functor")@. This gives $\text{Mack}_G$ what is often called a {\em semiadditive} structure.

            Moreover, any Mackey functor homomorphism admits a @TO2((kernel,MackeyFunctorHomomorphism),"kernel")@ and a @TO2((cokernel,MackeyFunctorHomomorphism),"cokernel")@. All this data we've collected so far is often called a {\em pre-abelian} structure. Finally, one can prove that images and coimages agree, hence $\text{Mack}_G$ is an {\em abelian category}.

            {\bf Enough projectives:} The category $\text{Mack}_{G}$ has {\em exactly two} projective objects, which we denote by $\underline{A}$ and $\underline{B}$. These are the @TO2(makeBurnsideMackeyFunctor, "Burnside Mackey functor")@ and the @TO2(makeUnderlyingFreeMackeyFunctor,"underlying free Mackey functor")@, respectively. The category $\text{Mack}_G$ has {\em enough projectives}, meaning in particular that we can take projective resolutions in order to compute ext and tor groups.

            {\bf Free resolutions:} Given any Mackey functor we can take its {\em free resolution}. The projective dimension of an object of $\text{Mack}_G$ is generally infinite, so we cannot provide all of the data of a resolution -- nevertheless we can provide a computation that holds in a range. This is accomplished using the @TO(resolution)@ (aka @TO(res)@) method, which inputs a Mackey functor $M$ and an integer $n$, and outputs a free resolution of the form
            \[M \leftarrow F_0 \leftarrow F_1 \leftarrow \cdots \leftarrow F_n\]

        Example
            M = prune makeRandomCpMackeyFunctor(3,GenBound=>2);
            class res(M,2)
            length resolution(M,2)
        Text
            In the notation above, the element @TT("res(M,2)")@ will be a @TO2(List,"list")@ of differentials $(M \leftarrow F_0, F_0 \leftarrow F_1, F_1 \leftarrow F_2)$, where each entry in the list is a @TO2(MackeyFunctorHomomorphism,"Mackey functor homomorphism")@.

            When a resolution is computed, this is @TO2(cache,"cached")@ in the Mackey functor itself, and can be accessed as @TT("M.ProjRes")@. This makes computations less costly, and moreover the cache helps us expand resolutions. For instance if we have already computed @TT("res(M,10)")@ and want to compute @TT("res(M,20)")@, it will access the cached 10-term resolution to shorten the computation for the 20-term resolution.

            {\bf The monoidal structure:} The category $\text{Mack}_G$ furthermore has a {\em symmetric monoidal structure}, given by the @TO2(boxProduct,"box product")@ of two Mackey functors. Taking the box product admits a right adjoint, in other words there is an @TO2(internalHom,"internal hom")@, which is a functor

            \[\underline{\text{Hom}} \colon \text{Mack}_G^{\text{op}} \times \text{Mack}_G \to \text{Mack}_G,\]

            whose @TO2(Fixed,"fixed point module")@ is the @TO2((Hom,CpMackeyFunctor,CpMackeyFunctor),"abelian group of homomorphisms")@ from $M$ to $N$. The category $\text{Mack}_G$ is {\em closed symmetric monoidal}, meaning the internal hom and box product fit together into a natural isomorphism of abelian groups, natural in any three Mackey functors $M$, $N$, and $P$:
            \[\text{Hom}_{\text{Mack}_G}(M \square N, P) \cong \text{Hom}_{\text{Mack}_G}(M, \underline{\text{Hom}}(N,P)).\]



            {\bf Ext and Tor:} With free resolutions in mind, we can define internal @TO2((Ext,ZZ,CpMackeyFunctor,CpMackeyFunctor),"ext")@ and @TO2((Tor,ZZ,CpMackeyFunctor,CpMackeyFunctor),"tor")@ groups as the derived functors of the internal hom and box product, respectively:

            \[\underline{\text{Ext}}^i(-,M):= \mathbf{R}^i \underline{\text{Hom}}_{\text{Mack}_G}(-,M),\]

            \[\underline{\text{Tor}}^i(-,M) := \mathbf{L}_i (- \square M).\]

            These are {\em internal} incarnations of $\text{Ext}$ and $\text{Tor}$. We can recover the abelian groups $\text{Ext}^i(M,N)$ and $\text{Tor}_i(M,N)$ as the @TO2(Fixed,"fixed point module")@ of the internal Ext and Tor.

    SeeAlso
        "background on Mackey functors"
        "constructing examples of Mackey functors"
        "list of common Mackey functors"
        "explicit applications of the CpMackeyFunctors package"

///

doc ///
    Key
        (symbol *,MackeyFunctorHomomorphism,MackeyFunctorHomomorphism)
    Headline
        composition of Mackey functor homomorphisms
    Usage
        f * g
    Inputs
        f : MackeyFunctorHomomorphism
        g : MackeyFunctorHomomorphism
    Outputs
        : MackeyFunctorHomomorphism
            the composition $f \circ g$
    Description
        Text
            Given two @TO2(MackeyFunctorHomomorphism,"Mackey functor homomorphisms")@ $f$ and $g$, we can form their composition, which is a new @TO2(MackeyFunctorHomomorphism,"Mackey functor homomorphism")@.
    SeeAlso
        "background on Mackey functors"
        MackeyFunctorHomomorphism
///

doc ///
    Key
        (symbol ++,CpMackeyFunctor,CpMackeyFunctor)
    Headline
        direct sum of Cp-Mackey functors
    Usage
        M ++ N
    Inputs
        M : CpMackeyFunctor
        N : CpMackeyFunctor
    Outputs
        : CpMackeyFunctor
            the direct sum of $M$ and $N$
    Description
        Text
            Given two @TO2((CpMackeyFunctor),"Cp-Mackey Functors")@ $M$ and $N$, we can form their {\em direct sum}, defined by taking the levelwise direct sum of the underlying and fixed modules.
    SeeAlso
        "the abelian category of Mackey functors"
        MackeyFunctorHomomorphism

///

doc ///
    Key
        (kernel, MackeyFunctorHomomorphism)
        [kernel,DegreeLimit]
        [kernel,Strategy]
        [kernel,SubringLimit]
    Headline
        kernel of a Mackey functor homomorphism
    Usage
        kernel f
    Inputs
        f : MackeyFunctorHomomorphism
    Outputs
        : CpMackeyFunctor
            the kernel of the homomorphism f
    SeeAlso
        "the abelian category of Mackey functors"
        MackeyFunctorHomomorphism
///

doc ///
    Key
        (cokernel, MackeyFunctorHomomorphism)
    Headline
        cokernel of a Mackey functor homomorphism
    Usage
        cokernel f
    Inputs
        f : MackeyFunctorHomomorphism
    Outputs
        : CpMackeyFunctor
            the cokernel of the homomorphism f
    SeeAlso
        "the abelian category of Mackey functors"
        MackeyFunctorHomomorphism
///
