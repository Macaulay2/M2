doc ///
Node
    Key
        (Hom, CpMackeyFunctor, CpMackeyFunctor)
        [Hom,DegreeLimit]
        [Hom,MinimalGenerators]
        [Hom,Strategy]
    Headline
        computes the Hom group between two Cp Mackey functors
    Usage
        Hom(M,N)
    Inputs
        M : CpMackeyFunctor
        N : CpMackeyFunctor
    Outputs
        : Module
            the abelian group of homomorphisms $\text{Hom}_{\text{Mack}_{C_p}}(M,N)$
    Description
        Text
            Given two Mackey functors $M$ and $N$, the set of Mackey functor homomorphisms from $M$ to $N$ forms an abelian group under pointwise addition.
        Example
            M = makeBurnsideMackeyFunctor(2);
            N = makeUnderlyingFreeMackeyFunctor(2);
            Hom(M,N)
    SeeAlso
        "the abelian category of Mackey functors"
        MackeyFunctorHomomorphism
        (Hom, CpMackeyFunctor, MackeyFunctorHomomorphism)
        (Hom, MackeyFunctorHomomorphism, CpMackeyFunctor)
        internalHom
Node
    Key
        (Hom, CpMackeyFunctor, MackeyFunctorHomomorphism)
        (Hom, MackeyFunctorHomomorphism, CpMackeyFunctor)
    Headline
        computes the induced map on Hom groups
    Usage
        Hom(M, f)
        Hom(f, N)
    Inputs
        M : CpMackeyFunctor
        f : MackeyFunctorHomomorphism
    Outputs
        : Matrix
            the induced map on Hom groups $\mathrm{Hom}(M,f)$ or $\mathrm{Hom}(f,N)$
    Description
        Text
            $\mathrm{Hom}$ is a functor which is contravariant in the first argument and covariant in the second argument. This method implements the functoriality on morphisms.
        Example
            M = makeBurnsideMackeyFunctor(2);
            N = makeUnderlyingFreeMackeyFunctor(2);
            f = makeRandomMackeyFunctorHomomorphism(M,N);
            Hom(M,f)
            Hom(f,N)
    SeeAlso
        "the abelian category of Mackey functors"
        MackeyFunctorHomomorphism
        (Hom, CpMackeyFunctor, CpMackeyFunctor)
        internalHom
///