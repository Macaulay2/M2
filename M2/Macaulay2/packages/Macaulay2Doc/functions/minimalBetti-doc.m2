doc ///
   Key
      minimalBetti
     (minimalBetti,Ideal)
     (minimalBetti,Module)
     [minimalBetti,DegreeLimit]
     [minimalBetti,LengthLimit]
     [minimalBetti,Weights]
     [minimalBetti,ParallelizeByDegree]
   Headline
     minimal betti numbers of (the minimal free resolution of) a homogeneous ideal or module
   Usage
     B = minimalBetti I
     B = minimalBetti(I, DegreeLimit=>d, LengthLimit=>len, Weights=>h)
   Inputs
     I:{Ideal,Module}
       a homogeneous ideal or module in a singly graded polynomial ring or skew commuting polynomial ring over a finite prime field
     DegreeLimit=>ZZ
       if given, only compute enough to determine the Betti diagram up to and including the row labelled {\tt d}
     LengthLimit=>ZZ
       if given, only compute enough to determine the Betti diagram up to and including the column labelled {\tt len}
     Weights=>List
       see @TO [betti, Weights]@
     ParallelizeByDegree => Boolean
       Use additional parallelism to compute different (homological degree, internal degree) pairs in
       parallel when possible. To control parallelism, see @TO "parallelism in engine computations"@
   Outputs
     :BettiTally
   Description
    Text
      Given a singly-graded module, this function computes the minimal betti numbers of the module.
      If the input is an ideal $I \subset S$, it computes the minimal betti numbers of $S^1/I$.

      The algorithm used is based on the @TO FastNonminimal@ algorithm,
      except that the complex is not constructed, resulting in a smaller memory footprint and often reduced computation time.
    Example
      I = Grassmannian(1,6, CoefficientRing => ZZ/101);
      S = ring I      
      elapsedTime C = minimalBetti I
    Text
      One can compute smaller parts of the Betti table, by using @TO DegreeLimit@ and/or @TO LengthLimit@.
    Example
      I = ideal I_*;
      elapsedTime C = minimalBetti(I, DegreeLimit=>2)
      I = ideal I_*;
      elapsedTime C = minimalBetti(I, DegreeLimit=>1, LengthLimit=>5)
      I = ideal I_*;
      elapsedTime C = minimalBetti(I, LengthLimit=>5)
    Text
      This function computes only as much of the non-minimal resolution as needed
      to compute the desired Betti numbers.  Further calls will generally not recompute
      previously computed parts of the resolution, except that if you ask for 
      a longer resolution than previously, it currently will recompute the resolution.
      This behavior might change in later releases.
    Text
      If one has already computed the non-minimal free resolution using 
      @TO FastNonminimal@, then one can use @TO [betti,Minimize]@, except that it doesn't currently
      have support for {\tt DegreeLimit} and {\tt LengthLimit}, and probably still computes more
      than is needed (it is still experimental).
   Caveat
     Only works over finite prime field.
     If the ideal or module is a non-homogeneous or multi-homogeneous object,
     then this function will result in an error.
   SeeAlso
     betti
     "OldChainComplexes :: betti(...,Minimize=>...)"
     "OldChainComplexes :: resolution"
     "OldChainComplexes :: resolution(...,FastNonminimal=>...)"
     "Complexes :: Nonminimal"
///

