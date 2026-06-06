doc ///
    Key
        "background on Mackey functors"
    Headline
        a brief mathematical intro to the theory of Mackey functors
    Description
        Text
            {\em Mackey functors} were introduced by Andreas Dress [D71] as a way to encode a system of abelian groups indexed along conjugacy classes of a subgroup, with homomorphisms between them behaving analogously to restriction and induction of representations. They are ubiquitous in mathematics, appearing in representation theory, group cohomology, equivariant cohomology of $G$-spaces, algebraic $K$-theory of group rings, algebraic number theory (where they go by the name {\em modulations}), and many other contexts.

            {\bf Assumption:} In this package, we will work with cyclic groups of prime order. The reason for this reduction is the simple subgroup structure of $C_p$, which reduces our data structure dramatically. Furthermore, the theory of $C_p$-Mackey functors is already highly complicated - for example no classification result is known.

            {\bf Definition}: Let $p$ be a prime. A $C_p${\em-Mackey functor} is the data of two abelian groups $M(C_p/e)$ (called the {\em underlying}) and $M(C_p/C_p)$ (called the {\em fixed}), together with three abelian group homomorphisms, called {\em restriction}, {\em transfer}, and {\em conjugation}, respectively:

            \[\text{res} \colon M(C_p/C_p) \to M(C_p/e),\]
            \[\text{tr} \colon M(C_p/e) \to M(C_p/C_p),\]
            \[\text{conj} \colon M(C_p/e) \to M(C_p/e)\]

            subject to the following axioms:

            @OL{
                (TEX "$\\text{conj}$", " is an automorphism of order dividing ", TEX"$p$", " (encoding an action of the cyclic group ", TEX"$C_p$", " on ", TEX"$M(C_p/e)$", ")"),
                (TEX "$\\text{conj}\\circ\\text{res} = \\text{res}$", " and ", TEX"$\\text{tr}\\circ\\text{conj} = \\text{tr}$"),
                (TEX "$\\text{res}(\\text{tr}(x)) = \\sum_{i=0}^{p-1}\\text{conj}^{i}(x)$", " for every ", TEX"$x\\in M(C_p/e)$")
            }@

            {\bf Example}: The easiest example is when all the abelian groups are the trivial group, and hence all maps are trivial. This is called the @TO2(makeZeroMackeyFunctor,"zero Mackey functor")@.

            {\bf Example}: A prototypical example has underlying module $\ZZ$ (i.e. the complex representation ring of the trivial group) and fixed module given by the complex representation ring of $C_p$, with transfer and restriction coming from restriction and transfer of $C_p$-representations. This is called the @TO2(makeComplexRepresentationMackeyFunctor,"representation Mackey functor")@.

            {\bf Note:} For general examples and their constructors, see @TO("constructing examples of Mackey functors")@ and for some common Mackey functors used over the group $C_p$, see @TO("list of common Mackey functors")@.

            We frequently use $M$ as shorthand for all the data $(M(C_p/e), M(C_p/C_p), \text{res},\text{tr},\text{conj})$.

            {\bf Definition:} If $M$ and $N$ are both $C_p$-Mackey functors, a @TO2(MackeyFunctorHomomorphism,"Mackey functor homomorphism")@ $f \colon M \to N$ is the data of a morphism $f_{C_p/e} \colon M(C_p/e) \to N(C_p/e)$ and $f_{C_p/C_p} \colon M(C_p/C_p) \to N(C_p/C_p)$ which commute with transfer, restriction, and conjugation. Explicitly (if we decorate things like $\text{res}$ with a subscript to indicate which Mackey functor they are coming from) we mean that the following relations must hold:

            @OL{
                (TEX "$\\text{res}_N\\circ f_{C_p/C_p} = f_{C_p/e}\\circ \\text{res}_M$"),
                (TEX "$\\text{tr}_N \\circ f_{C_p/e} = f_{C_p/C_p} \\circ \\text{tr}_M$"),
                (TEX "$\\text{conj}_N\\circ f_{C_p/e} = f_{C_p/e} \\circ \\text{conj}_M$")
            }@

        Text
            {\bf References:}

            @UL{
                ("[D71] A. Dress, ", EM"Notes on the theory of representations of finite groups. Part I: The Burnside ring of a finite group and some AGN-applications.", " Bielefeld."),
                ("[W00] P. Webb, ", EM"A guide to mackey functors", " Handbook of Algebra, 2000.")
            }@

    SeeAlso
        "the abelian category of Mackey functors"
        "constructing examples of Mackey functors"
        "list of common Mackey functors"
        "explicit applications of the CpMackeyFunctors package"
///