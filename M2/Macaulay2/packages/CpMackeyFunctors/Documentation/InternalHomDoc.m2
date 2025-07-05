doc ///
    Key
        InternalHom
        (InternalHom, CpMackeyFunctor, CpMackeyFunctor)
    Headline
        returns the internal hom Mackey functor between two Mackey functors.
    Usage
        InternalHom(N,M)
    Inputs
        M : CpMackeyFunctor
        N : CpMackeyFunctor
    Outputs
        : CpMackeyFunctor
            the internal hom Mackey functor from M to N.
    Description
        Text
            Given any two Mackey functors $M$ and $N$, we can form their {\em internal hom}, which is a Mackey functor which we denote by $\underline{\text{Hom}}(M,N)$. For example:
        Example
            InternalHom(makeRealRepresentationMackeyFunctor 3, makeComplexRepresentationMackeyFunctor 3)
        Text
            The underlying @TO2((Hom, CpMackeyFunctor,CpMackeyFunctor),"group of homomorphisms")@ between any two Mackey functors can be recovered as the @TO2(Fixed,"fixed module")@ key of the internal hom.
    SeeAlso
        "the abelian category of Mackey functors"
        boxProduct
///

doc ///          
    Key
        (InternalHom, CpMackeyFunctor, MackeyFunctorHomomorphism)
        (InternalHom, MackeyFunctorHomomorphism, CpMackeyFunctor)
    Headline
        returns the induced map on an internal hom.
    Usage
        InternalHom(F,M)
        InternalHom(M,F)
    Inputs
        M : CpMackeyFunctor
        F : MackeyFunctorHomomorphism
    Outputs
        : MackeyFunctorHomomorphism
            the induced map on internal hom Mackey functors
    Description
        Text
            The internal hom of Mackey functors is functorial in each variable.  This method returns the induced maps.
        Example
            prune InternalHom(makeRealRepresentationMackeyFunctor(3), complexLinearizationMap(3))
    SeeAlso
        "the abelian category of Mackey functors"
        InternalHom
///
