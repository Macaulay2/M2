-- TODO: move the rest here from doc10.m2

doc ///
   Key
     FastNonminimal
     [resolution, FastNonminimal]
   Headline
     compute a non-minimal graded free resolution
   Usage
     C = res(I, FastNonminimal => true, ...)
   Inputs
     I:Ideal
       or @ofClass Matrix@ or @ofClass Module@, in a polynomial ring
       or skew commuting polynomial ring, over a finite prime field
   Outputs
     C:ChainComplex
   Description
    Text
      Given an inhomogeneous,  singly-graded, or multi-graded ideal or module, this function
      computes a non-minimal free resolution.  If the 
      input is an ideal $I \subset S$, it computes a non-minimal
      resolution of $S^1/I$.
      
      A key benefit of this function is that it allows
      a much faster method for computing the 
      betti numbers of the {\bf minimal} free resolution.
      If that is your only interest (i.e. you don't need the complex itself), 
      instead use @TO minimalBetti@.  However, @TO "minimalBetti"@ currently only works for
      single gradings, not multi-gradings.
    Example
      I = Grassmannian(1,6, CoefficientRing => ZZ/101);
      S = ring I      
      elapsedTime C = res(I, FastNonminimal => true)
      elapsedTime C1 = res ideal(I_*)
      betti(C, Minimize => true) == betti C1
    Text
      For a non-minimal resolution, @TO betti@ gives the actual Betti
      numbers, and using the @TO [betti,Minimize]@ option gives the ranks
      in a minimal resolution (which is itself not computed).
    Example
      betti C
      betti(C, Minimize => true)
    Text

      As mentioned above, if you are just interested in the minimal betti numbers of the ideal or
      module, then use @TO minimalBetti@, as it avoids construction of the
      non-minimal free resolution.
    Example
      minimalBetti I
    Text

      If the resolution is not large, this function can be slower than
      the usual function @TO resolution@.  But for larger examples, 
      if one is only interested in the betti numbers, this function
      can be hundreds or thousands of times faster.      
    Text
      
      If the input module is not graded, or is multi-graded, this function still works. However,
      @TO "minimalBetti"@ does not work in these cases.  In the inhomogeneous case, the 
      returned free resolution is often highly non minimal. Of course, there is no notion of minimal
      resolution in this case, but one can use @TO "PruneComplex::pruneComplex"@ to clean up the
      returned complex.
    Example
      R = ZZ/101[a..f]
      I = ideal"a3-ab-c2,abc-d2-1, b3-b2-b"
      C = res(I, FastNonminimal => true)
      needsPackage "PruneComplex"
      pruneComplex C
    Text
      If one has a specific Gröbner basis on which one wants to base the Schreyer resolution,
      use @TT "Strategy => 5"@.  This will not check that the input forms a Gröbner basis, but
      if it does not, then the function will either produce non-sensical answers, or fail.
    Example
      R = ZZ/101[a..c,x_1..x_9, MonomialOrder=>{3,9}]
      I = ideal(a^2 - b^2 - x_1 * a*c - x_2 * b*c - x_3 * c^2,
                 a*b - x_4 * a*c - x_5 * b*c - x_6 *c^2,
                 b^2 - x_7 * a*c - x_8 * b*c - x_9 *c^2
                 )
      C = res(I, FastNonminimal => true, Strategy=>5)
      C.dd
    Text
      Note that {\tt Strategy=>4} or {\tt Strategy=>5} implies FastNonminimal.
    Example
      C1 = res(ideal I_*, Strategy=>5)
      C1.dd
    
   Caveat
     Released in M2 1.9, still experimental.  Only works over finite prime fields. Uses quite a lot of memory.
     For inhomogeneous ideals or modules, the monomial order must be a degree order.  For multi-graded
     ideals or modules, @TO "minimalBetti"@ is not yet implemented.
   SeeAlso
     minimalBetti
     betti
     [betti,Minimize]
     resolution
     "PruneComplex::pruneComplex"
///

-- Known bug: if I is inhomogeneous, in a poly ring with a non-degree monomial order,
--  then the following line can actually crash.
--    C = res(ideal I_*, FastNonminimal => true) -- crash!!
