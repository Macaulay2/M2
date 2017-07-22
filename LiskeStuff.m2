
doc ///
  Key
    isReduction
    (isReduction, Ideal, Ideal)
  Headline
    tests if the second ideal is the reduction of the first
  Usage
    isReduction(I,J)
  Inputs
    I:Ideal
    J:Ideal
  Outputs
    :ZZ
      true if J is a reduction of I, false otherwise
  Description
    Text
      isReduction takes a pair of ideals $I,J$, and determines if $J$ is a reduction of $I$, that is
      $J\subset I$ and $I^{n+1}=JI^n$ for some $n\geq 0$. The minimal such $n$ is called the reduction number and can be found using @TO reductionNumber@.

     
      See the book 
      Huneke, Craig; Swanson, Irena: Integral closure of ideals, rings, and modules. 
      London Mathematical Society Lecture Note Series, 336. Cambridge University Press, Cambridge, 2006.
      for further information.
    Example
      setRandomSeed()
      kk = ZZ/101;
      S = kk[a..c];
      m = ideal vars S;
      i = (ideal"a,b")*m+ideal"c3"
      analyticSpread i
      j=minimalReduction i
      reductionNumber (i,j)
  Caveat
     It is possible for the routine to not finish in reasonable time, due to the
     probabilistic nature of the routine.  What happens is that 
     the routine @TO minimalReduction@ occasionally, but rarely, returns an ideal
     which is not a minimal reduction.  In this case, the routine goes into an infinite loop.
     This will be addressed in a future version
     of the package.  In the meantime, simply interrupt the routine, and restart the
     computation.
  SeeAlso
    analyticSpread
    minimalReduction
    whichGm
///