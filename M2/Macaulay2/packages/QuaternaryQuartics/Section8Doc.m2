doc ///
  Key
    "Type [000], CY of degree 20"
  Headline
    lifting to a 3-fold with two singular points
  Description
    Text
      We construct a singular Calabi-Yau of dimension 3 with 2 nodes, of codimension 4,
      of degree 20.  This example cannot be smoothed.  However, there is the example of
      the determinantal ($3 \times 3$ minors of a $4 \times 4$ matrix of linear forms,
      which is a smooth Calabi-Yau 3-fold of degree 20.
    Example
      kk = ZZ/31
      U = kk[y0,y1,y2,y3,y4,y5,y6,y7];
      setRandomSeed 8327432
      U1 = vars U
      P6 = matrix{{y1,y2,y3,y4,y5,y6,y7}} -- The hyperplane y0 == 0
    Text
      The original example code
    Example
      F1 = P6 * random(kk^7,kk^5);
      F2 = P6 * random(kk^7,kk^5);
      F3 = P6 * random(kk^7,kk^5);
      fg = transpose F1 | transpose F2 | transpose F3 ;
      ff = matrix{{y0,0,0,0,0},{0,0,0,0,0},{0,0,0,0,0}};     
      fg = transpose fg + ff;    --3x5 matrix of rank 1 at (1:0:..:0), of rank 2 on fourfold Y

      Y = fg3 = minors(3,fg);
      fg2 = minors(2,fg);
      codim fg3
      codim fg2, degree fg2
      saturate fg2

      sf1 = fg^{0,1}
      sf2 = fg^{0,2}

      Z1 = sf12 = minors(2,sf1);--threefold rational scroll of degree 5 in Y
      Z2 = sf22 = minors(2,sf2);--threefold rational scroll of degree 5 in Y
      sf = trim intersect(Z1, Z2);-- threefold rational scroll over reducible conic of degree 10 in Y
      (dim Z1, degree Z1)

      betti sf  --in 11 cubics, so sf is in a cubic hypersurface section of Y

      k3 = ideal select(sf_*, f -> degree f == {3}); -- the cubics in the ideal
  
      k3=submatrix(gens sf,{0,1,2,3,4,5,6,7,8,9,10});  --the cubics in the ideal of sf
      k3=ideal flatten k3;

      X = cy = (k3:Z1):Z2; --the CY threefold, residual to sf in a cubic hypersurface section
      -- the following is about 10 times in computing X:
      -- elapsedTime X = cy=k3:sf;  --the CY threefold, residual to sf in a cubic hypersurface section
      
      betti X
      betti res X
      (dim X, degree X)
    Text
      Now we analyze the singularities of this Calabi-Yau.
      First, note that $X$ is singular at the point$(1, 0, \ldots, 0)$.
    Example
      J = jacobian X;
      minors(4, sub(J, matrix{{1_kk,0,0,0,0,0,0,0}})) == 0
      sub(X, matrix{{1_kk,0,0,0,0,0,0,0}})
    Text
      Now we find other singular points.  We would like to just compute the 
      singular locus as the $4 \time 4$ minors of the $8 \times 16$ Jacobian matrix, which is
      $127,400$ determinants, but this is pretty large.
      
      We find that there is exactly one more singular point, and that it is of multiplicity 2.
    CannedExample
      needsPackage "FastMinors" -- "FastLinAlg" in versions of M2 before 1.19
      setRandomSeed 500
      elapsedTime chooseGoodMinors(200, 4, J);
      J1 = cy + oo;
      elapsedTime gbJ1 = groebnerBasis(J1, Strategy => "F4");
      inJ1 = monomialIdeal leadTerm gbJ1;
      codim inJ1 == 7 -- so singular locus is finite
      degree inJ1 == 2
      (L1, d) = divideByVariable(gbJ1, U_7);
      L1 = ideal L1;
      pt2 = trim L1
    Text
      This implies that the singular locus is a finite set of points, and is at most 2
      points, and we know one of the points, and the other pointis off of the hyperplane $x_7 = 0$.
      
      However, we must check that this point is singular on $X$, as our ideal
      is only a subset of the ideal of the singular locus.
    CannedExample
      isSubset(cy, pt2)
      minors(4, J % pt2) == 0
    Text
      We now know that the singular locus consists of these 2 points, as the degree of $J_1$ is 2,
      so there can be no other components of dimension 0.
  SeeAlso      
    "[QQ]"
///

doc ///
  Key
    "Singularities of lifting of type [300b]"
  Headline
    The lifting of [300b] defined by biliaison acquires singularities in dimension 3
  Description
    Text
      We describe a way of lifting case [300b] by performing a biliaison construction. Let $X$  be a degree 7 residual component of an intersection of three quadrics containing a linear space of codimension 3. 
    Example 
      loadPackage "RandomIdeals"
      kk = ZZ/101;
      R = kk[x_0..x_7];
      T = random(R^1,R^{-1,-1,-1});
      I = ideal(T);
      J = randomElementsFromIdeal({2,2,2},I); 
      X=J:I;
    Text 
      Consider $SS$ the intersection of the two components of the complete intersection
    Example 
      SS=X+I;
      SingSS= radical ideal singularLocus saturate SS;
      degree SingSS
      dim SingSS
    Text 
      Perform a biliaison uo by degree 2 of $SS$ in $X$ and get $BT$ an AG variety of degree 17 and type [300b]
    Example
      JJ = randomElementsFromIdeal({3},SS);
      IDD=X+JJ;
      PP=IDD:SS;
      BB= randomElementsFromIdeal({5},PP);
      BU=BB+X;
      BT=BU:PP;
      degree BT
    Text 
      We look for singularities of $BT$. Since the computation of singular locus is too long, we just check the rank of the jacobian matrix in one of the components of $SingSS$ and get that $BT$ is singular in that locus.
    Example
      minors(4,(map(R/(decompose SingSS)_0,R)) (jacobian BT))
  SeeAlso      
    "[QQ]"
///

doc ///
  Key
    "Half canonical degree 20"
  Headline
    Computation which supports the proof of Proposition 8.4
  Description
    Text
      In the proof Proposition 8.4 we need to show that if Z is fifteen points in the plane defined by the (5x5)-minors of a general (5x6) matrix of linear forms, then there are no curves of degree 8, singular along Z.
    Example
      kk=ZZ/101
      S=kk[x0,x1,x2]
      M=random(S^5,S^{6:-1});--a random (5x6) matrix linear forms in P2
      m5=minors(5,M);--the ideal of 15 points 
      m52=saturate m5^2;--the square of the ideal of 15 points, saturated, with no forms of degree 8.
      betti res m52
    Text
      In Y, a smooth 4-fold in $\mathbb{P}^7$ defined as the rank 2 locus of (3x5) matrix M of linear forms, let S1 be the 3-fold scroll defined as the rank 1 locus of the submatrix M12 of M consisting of its two first rows, and let S2 be the 3-fold scroll defined as the rank 1 locus of the submatrix M23 of M consisting of its two last rows
      Then we show that every cubic hypersurface that contains both S1 and S2, contains all of Y.  
      Hence this is the case for any divisor on Y equivalent to S1+S2.
    Example
      kk=ZZ/101
      S=kk[x0,x1,x2,x3,x4,x5,x6,x7];
      M=random(S^3,S^{5:-1});--a random (3x5) matrix linear forms in P7
      m3=minors(3,M);--the ideal of a 4-fold Y,  
      M12=submatrix (M, {0,1},{0,1,2,3,4});
      M23=submatrix (M, {1,2},{0,1,2,3,4});
      S1=minors (2,M12);-- a divisor in Y of degree 5
      S2=minors (2,M23);-- another divisor equivalent to S1 
      S12=saturate intersect(S1,S2);--the divisor S1+S2 on Y
      betti res S12 --the ten cubics are the same as the ones in the ideal of Y                                                                                                                   
  SeeAlso      
    "[QQ]"
///
