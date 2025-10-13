doc ///
   Key
     "parallelism in engine computations"
     "numTBBThreads"
     ParallelizeByDegree
   Headline
     parallelism in engine computations
   Description
     Text
       Some computations in the engine can run using multiple cores on
       your computer. Currently, this includes computation of minimal
       betti diagrams, non-minimal resolutions, and Gröbner bases of
       2-sided ideals in associative algebras, all in the graded case,
       over a finite field.  Also included is (one of the algorithms for) the computation of Gröbner
       bases in polynomial rings over finite fields, whether graded or not.
     Text
       The variable {\tt numTBBThreads} controls the number of cores used by Macaulay2.
       The default value (zero) means the system can choose an appropriate number
       of cores (often the maximum available).  Note that the default
       behavior is to use multiple cores.

       In @TO minimalBetti@, and in @TO "Complexes::freeResolution"@
       with the {\tt Strategy => Nonminimal} option, more aggressive
       parallelism that sometimes uses a lot of memory but can
       sometimes produce answers in less time can be enabled using the
       {\tt ParallelizeByDegree} boolean option.

       For examples, we show some simple examples of computation
       which, for larger size problems, might benefit from using
       parallelism.  Note that in each of these cases, the default is
       to use all available CPU cores for computation. For these
       particularly simple examples, the overhead for using multiple
       cores is non-trivial with respect to the total computation
       time.  Significant speedup is achieved when the time to gauss
       reduce the requisite matrices is large compared to creating
       these matrices.
     Example
       numTBBThreads
       I = Grassmannian(1, 6, CoefficientRing => ZZ/101)
       S = ring I
       elapsedTime minimalBetti I
       I = ideal I_*;
       elapsedTime minimalBetti(I, ParallelizeByDegree => true)
       I = ideal I_*;
       numTBBThreads = 1
       elapsedTime minimalBetti(I)
     Example
       needsPackage "Complexes"
       numTBBThreads = 0
       I = ideal I_*;
       elapsedTime freeResolution(I, Strategy => Nonminimal)
       numTBBThreads = 1
       I = ideal I_*;
       elapsedTime freeResolution(I, Strategy => Nonminimal)
     Text
       Gröbner bases (based on a linear algebra method, e.g.
       Faugere's F4 algorithm, are also parallelized.  Note: the MGB
       Strategy of groebnerBasis is not currently parallelized.
     Example
       numTBBThreads = 0
       S = ZZ/101[a..g]
       I = ideal random(S^1, S^{4:-5});
       elapsedTime groebnerBasis(I, Strategy => "F4");
       numTBBThreads = 1
       I = ideal I_*;
       elapsedTime groebnerBasis(I, Strategy => "F4");
       numTBBThreads = 10
       I = ideal I_*;
       elapsedTime groebnerBasis(I, Strategy => "F4");
     Text
       For Gröbner basis computation in associative algebras,
       @TT "ParallelizeByDegree"@ is not relevant.  In this case, use
       @TT "numTBBThreads"@ to control the amount of parallelism.
     Example
       needsPackage "AssociativeAlgebras"
       numTBBThreads = 0
       C = threeDimSklyanin(ZZ/101,{2,3,5},{a,b,c})
       I = ideal C
       elapsedTime NCGB(I, 22);
       I = ideal I_*
       numTBBThreads = 1
       elapsedTime NCGB(I, 22);
   SeeAlso
     minimalBetti
     "OldChainComplexes::resolution"
     "Complexes::freeResolution"
     groebnerBasis
     "AssociativeAlgebras::NCGB"
     "parallel programming with threads and tasks"
///
