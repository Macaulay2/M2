doc ///
  Key
    "Doubling Examples"
  Headline
    Doubling of each type of set of points
  Description
    Text
      For each of the 19 strata, take a set of points computing the rank of a
      general quartic on that stratum.  From these, we have two ways of 
      obtaining an Artinian Gorenstein algebra of codimension and regularity both 4: 
      we can take the inverse system of the quartic, and we can take the doubling
      of the ideal of points.  These match in all but one stratum: case [300a], see
      @TO "Example Type [300a]"@ for more details about this case.
    Example
      kk = ZZ/101;
      R = kk[x_0..x_3];
      HT = bettiStrataExamples R;
      netList prepend(
          {"type", "I = ideal of points", "Fperp", "doubling of points"},
          sort for k in keys HT list (
          I := pointsIdeal((HT#k)_0);
          F := quartic (HT#k)_0;
          {k, minimalBetti I,
              minimalBetti inverseSystem F,
              minimalBetti doubling(8, I)}
          ))
  SeeAlso
      "[QQ]"
///

doc ///
  Key
    "Doubling Examples for ideals of 6 points"
  Headline
    For an ideal $I_{\Gamma}$ of six points we compute possible doublings of $I_{\Gamma}$. See Example 2.16 in [QQ] for details
  Description
    Text
      If $\Gamma$ does not contain four collinear points, then
      $S/I_{\Gamma}$ has regularity $2$.  The following computation
      shows that a random element of $Hom(\omega(-4),R/I_{\Gamma})$
      is injective. Therefore Corollary 2.15 gives the corresponding
      doublings.
    Example
      kk = ZZ/101;
      R = kk[x_0..x_3];
      HT = bettiStrataExamples R;
      netList for k in {"[420]","[430]","[441a]","[441b]"} list (
	      if doubling(8,pointsIdeal((HT#k)_0))===null then 
              {k, betti res pointsIdeal((HT#k)_0), "No injective map"}
          else 
              {k, betti res pointsIdeal((HT#k)_0), 
                  betti res doubling(8,pointsIdeal((HT#k)_0))}
          )
    Text
      Next, suppose $\Gamma$ is the set of six points span
      $\mathbb{P}^{3}$ and four are collinear. We first check that a random
      element of $Hom(\omega(-\gamma), R/I_{\Gamma})$ is not injective for $\gamma = 2$. When
      $\gamma\geq 3$, a general element is injective and we compute
      the Betti table of doubling of $I_{\Gamma}$ with a general
      element for $\gamma=3,4,5,6$.
    Example
      Mpts = randomPoints(R,4,2)|(randomPoints(R,2,4)||(randomPoints(R,2,4)*0));
      IGamma = pointsIdeal(Mpts);
      betti res IGamma
      netList for k in {2,3,4,5,6} list (
	  if doubling(k+4,IGamma)===null then {k, "No injective map"}
	  else {k, betti res doubling(k+4,IGamma)})
  SeeAlso
      "[QQ]"
///

doc ///
  Key
    "Computation of a doubling for each Betti table type"
  Headline
    See Proposition 2.18 in [QQ]
  Description
    Text
      We take point sets $\Gamma$ in the Hash table
      coming from @TO bettiStrataExamples@, and make a doubling of each $I_{\Gamma}$.
    Example
      kk = ZZ/101;
      R = kk[x_0..x_3];
      HT = bettiStrataExamples R;
      netList for k in keys HT list (
	  IGamma = pointsIdeal((HT#k)_0);
	  J = doubling(8, IGamma);
	  {k, betti res IGamma, betti res J}
	  )
  SeeAlso
      "[QQ]"
///

doc ///
  Key
    "Example Type [300a]"
  Headline
    An example of an apolar ideal of a quartic that cannot be obtained as a doubling of it's apolar set
  Description
    Text
      To get a quartic form $F$ of type [300a], we start with a point set $\Gamma$ which is a complete intersection of three quadric forms. Then we let $F$ be a general element in the space spanned by $v_{4}(\Gamma)\subset\mathbb{P}^{34}$.
    Example
      kk = ZZ/101;
      R = kk[x_0..x_3];
      HT = bettiStrataExamples(R);
      MGamma = (HT#"[300a]")_0
      linforms = flatten entries((vars R) * MGamma);
      F = sum for ell in linforms list random(kk)*ell^4
    Text
      We check the Betti table of $F^\perp$.
    Example
      Fperp = inverseSystem F;
      betti res Fperp
    Text
      Let $Q$ be the quadratic part of $F^{\perp}$. We check that $Q=I_{\Gamma}$. 
    Example
      Q = ideal super basis(2,Fperp);
      Q == pointsIdeal(MGamma)
    Text
      We know that $\Gamma$ is a minimal apolar set to $F$. The doubling of $I_{\Gamma}$ is always a complete intersection. Therefore, $F^{\perp}$ cannot be obtained as a doubling of $I_{\Gamma}$ in this case.
  SeeAlso
///

doc ///
  Key
    "Example Type [300b]"
  Headline
    An example of doubling construction
  Description
    Text
      To get a quartic form $F$ of type [300b], we start with a set of
      $7$ points and let $F$ be power sum of them.
    Example
      kk = ZZ/101;
      R = kk[x_0..x_3];
      HT = bettiStrataExamples(R);
      MGamma = (HT#"[300b]")_0
      F = quartic MGamma;
    Text
      We check the type of $F$.
    Example
      quarticType F
    Text
      The function @TO quarticType@ cannot distinguish between type [300a] and [300b].
      However, given {\tt MGamma}, we now
      check that $F$ is of type [300b].  Let $I_{\Gamma}$ be the
      ideal defining the $7$ points.
    Example
      Fperp = inverseSystem F;
      betti res Fperp
      IGamma = pointsIdeal MGamma;
      degree IGamma
      decompose IGamma -- 7 points, therefore the rank is at most 7
      betti res IGamma
    Text
      Let $Q$ be the quadratic part of $I_{\Gamma}$. We check that $Q$
      is a complete intersection. Performing Construction 2.17, we
      obtain a doubling of $I_{\Gamma}$, which equals $F^{\perp}$.
    Example
      Q = ideal super basis(2,IGamma);
      betti res Q
      Ip = Q:IGamma;
      betti res Ip
      v = random(2,(Fperp:Ip));
      Fperp == IGamma + v*Ip
  SeeAlso
      "[QQ]"
///

doc ///
  Key
    "Example Type [300c]"
  Headline
    The third family of type [300]
  Description
    Text
      To get a quartic form $F$ of type [300c], we start with a set of $7$ points, with $3$ of them in a line, and let $F$ be their power sum.
    Example
      kk = ZZ/101;
      R = kk[x_0..x_3];
      HT = bettiStrataExamples(R);
      MGamma = (HT#"[300c]")_0
      IGamma = pointsIdeal MGamma;
      F = quartic MGamma;
    Text
      We check the type of $F^{\perp}$ and see that 
      the quadratic part $Q$ of $F^{\perp}$ is not a complete intersection.
    Example
      quarticType F
    Example
      Fperp = inverseSystem F;
      betti res Fperp
      Q = ideal super basis (2,Fperp);
      betti res Q
    Text
      Now we construct a doubling of $I_{\Gamma}$, which is not necessary the same as $F^{\perp}$, but is of type [300c].
    Text
      Let $J$ be a subideal of $I_{\Gamma}$ which is a $(2,2,3)$ complete intersection.
    Example
      J = ideal(random(2,IGamma),random(2,IGamma),random(3,IGamma));
      betti res J
    Text
      The colon ideal $I_{p}=J:I_{\Gamma}$ is a set of $5$ points. Performing Construction 2.17, we can find a doubling of $I_{\Gamma}$, which is of type [300c].
    Example
      Ip = J : IGamma
      betti res (Fperp:Ip)
      l = random(1,R);
      betti res (IGamma+l*Ip)
  SeeAlso
      "[QQ]"
///


