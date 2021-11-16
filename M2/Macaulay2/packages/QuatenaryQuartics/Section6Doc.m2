doc ///
  Key
    "Finding the Betti stratum of a given quartic"
  Headline
    the 19 Betti strata
  Description
    Text
      There are 19 Betti strata of quartics forms in four variables.
      Given a quartic, the stratum that it lives on is determined by the Betti diagram of the
      ideal of quadrics in the inverse system, except for [300] and [441].  The function
      @TO (quarticType, RingElement)@ uses this information, together with a finer analysis to
      determine which stratum those cases live on.  The cases [300a] and [300b] are more
      difficult to separate, as it depends on the exact rank of the quartic.  So this function
      returns [300ab] in this case.
      
      In any case, if we know a set of points which compute the rank, then that ideal and
      the quadratic part completely determine which stratum the quartic is on.
    Example
      kk = ZZ/101;
      R = kk[x_0..x_3];
      HT = bettiStrataExamples R;
      netList prepend(
          {"type", "I = ideal of points", "quadrics in I", "Fperp", "doubling of points"},
          sort for k in keys HT list (
          I := pointsIdeal((HT#k)_0);
          Q := ideal super basis(2, I);
          F := quartic (HT#k)_0;
          {k, minimalBetti I,
              minimalBetti Q,
              minimalBetti inverseSystem F,
              minimalBetti doubling(8, I)}
          ))
  SeeAlso
    "[QQ]"
    (quarticType, RingElement)
    inverseSystem
    minimalBetti
///


doc ///
  Key
    "Noether-Lefschetz examples"
  Headline
    examples from Section 6.2 in [QQ]
  Description
    Text
      We give examples of specific quartics interesting in Noether-Lefschetz loci for K3 surfaces,
      and where they fit in the Betti classification.
    Example
      kk = ZZ/101;
      R = kk[x_0..x_3];
    Text
      The first example illustrates Corollary 6.18.
    Example
      Q618 = (x_0^2+x_1^2+x_2^2+x_3^2)^2+x_0^4+x_1^4+x_2^4+x_3^4
      minimalBetti inverseSystem Q618
      quarticType Q618
    Text
      We illustrate Remark 6.19, considering a double quadric:
    Example
      Q619 = (x_0^2+x_1^2+x_2^2+x_3^2)^2
      minimalBetti inverseSystem Q619
    Text
      Next, we illustrate Remark 6.20. The first example is that of the Vinberg most singular K3 surface.  This is of type [331].
    Example
      Q620V = x_0^4-x_1*x_2*x_3*(x_1+x_2+x_3)
      minimalBetti inverseSystem Q620V
      quarticType Q620V
    Text
      The second example illustrating Remark 6.20 is that a general element of the Dwork pencil has type [000].
    Example
      Q620D = x_0^4+x_1^4+x_2^4+x_3^4-8*x_0*x_1*x_2*x_3
      minimalBetti inverseSystem Q620D
      quarticType Q620D
    Text
      The third example illustrating Remark 6.20 is that
      the K3 quartics $S_{t}\subset \mathbb{P}^{5}$ given by
      $$ x_{1}^{4}+\dots+x_{5}^{4}-t(x_{1}^{2}+\dots+x_{5}^{2})^{2}=x_{1}+\dots+x_{5}=0$$
      for general $t$ are of type [000]. However, $S_{0}$ is of type [550].
    Example
      x5=x_0+x_1+x_2+x_3
      Q = x_0^4+x_1^4+x_2^4+x_3^4+x5^4-random(kk)*(x_0^2+x_1^2+x_2^2+x_3^2+x5^2)^2;
      minimalBetti inverseSystem Q
      Q = x_0^4+x_1^4+x_2^4+x_3^4+x5^4;
      minimalBetti inverseSystem Q
      quarticType Q
  SeeAlso
    "[QQ]"
    (quarticType, RingElement)
    inverseSystem
    minimalBetti
///
