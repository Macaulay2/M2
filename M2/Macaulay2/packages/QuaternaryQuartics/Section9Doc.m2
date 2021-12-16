doc ///
  Key
    "Type [210], CY of degree 18 via linkage"
  Headline
    lifting to a 3-fold with components of degrees 11, 6, 1
  Description
    Text
      We construct via linkage an arithmetically Gorenstein
      3-fold $X = X_{11} \cup X_1 \cup X_6 \subset \bf{P}^7$, 
      of degree 17, having Betti table of type [210]:
      For an artinian reduction $A_F$, the quadratic part of the ideal $F^\perp$ is the ideal
      of a plane and a line, and $F^\perp$ contains pencils of ideals of five points on a conic in the plane and three points on the line, 
      and a 3-dimensional family of ideals of six points in the plane and two points on the line.  In particular, $F^\perp$ contains the ideal of the intersection point
       of the line and the plane in addition to five points in the plane and two points on the line.
        
     We construct $X_{11}$ in a quadric in a P6,  $X_{6}$ in a quadric in a P5 and $X_{1}$ in the P4 of intersection of P5 and P6.
      In the construction the intersection $X\cup X'$ of a component $X$ with the other is an anticanonical divisor on $X$.
      
     $\phantom{WWWW}
      \begin{matrix}
         &0&1&2&3&4\\\text{total:}&1&11&20&11&1\\
         \text{0:}&1&\text{.}&\text{.}&\text{.}&\text{.}\\
         \text{1:}&\text{.}&2&1&\text{.}&\text{.}\\
         \text{2:}&\text{.}&9&18&9&\text{.}\\
         \text{3:}&\text{.}&\text{.}&1&2&\text{.}\\
         \text{4:}&\text{.}&\text{.}&\text{.}&\text{.}&1\\
      \end{matrix}
      $
    Example
      kk = QQ;
      U=kk[y0,y1,y2,y3,y4,y5,y6,y7];
      X1 = ideal(y0,y5,y6,y7); -- the component X1, a P3
      P3a = ideal(y0,y1,y2,y3); --another  P3  
      P4 = ideal(y0,y6,y7); --a P4
      P5 = ideal(y6,y7); --a P5
      P6 = ideal(y0); -- a P6
    Text
      Construct a $(2,2)$ complete intersection in $X_1$:
    Example
      CI22 = X1 + ideal(random(2,P3a),random(2,P3a));
    Text
      Compute the ideal of the union of $CI_{22}$ and the other $\PP^3$.
      Then compute a complete intersection of type $(1,2,2,2)$
      which contains $\PP^3_a$ and $CI_{22}$.
    Example
      CIP3 = intersect(CI22,P3a);  --the union of CI22 and P3a
      CI1222 = trim(
          ideal(y0) + 
          ideal(random(2,CIP3),random(2,CIP3),random(2,CIP3)));
    Text
      We take the residual in this complete intersection of the $\PP^3_a$.
    Example
      X7 = CI1222:P3a;   -- the 3-fold X7 linked (1,2,2,2) to PL
      (dim X7, degree X7)
    Text
    Example
      QX7 = ideal random(2,X7);--a quadric hypersurface that contains X7
      Z2a = X1 + QX7;-- quadric that contain X7 intersected with X1
      X7Z2 = intersect(Z2a,X7);-- the union of  Z2 and X7
    Text
    Example
      Z6 = P4 + QX7 + ideal(random(3,X7Z2));--a complete intersection 2,3 in P4
      X7Z6 = intersect(X7,Z6);  --the union of X7 and the complete intersection 2,3 in P4
      CI1233 = P6 + QX7 + ideal(random(3,X7Z6),random(3,X7Z6));--complete intersection 1233 containing  Z2a and a hyperplane section of X7
      X11 = CI1233:X7;  --a 3-fold of degree 11.
      Z4 = Z6:Z2a;--a Del Pezzo surface in P4
      Y2 = P5+ideal(random(2,Z4));--a quadric 4-fold in P5 that contain Z4
      Z2b = X1+Y2;-- another quadric surface in X1
      Z6b = intersect(Z4,Z2b);-- a complete intersection 2,3 different from Z6a, the union of Z4 and Z2b
      Y3 = P5 + ideal(random(3,Z6b));--a cubic 4-fold in P5 that contain Z6b
      X6 = Y2 + Y3;
      X18 = intersect(X11,X6,X1);--a AG 3-fold of degree 18, with betti table of type 210. 
    Example
      (dim X18, degree X18)
      betti res X18
  SeeAlso      
    "[QQ]"
///

doc ///
  Key
    "Type [310], CY of degree 17 via linkage"
  Headline
    lifting to a 3-fold with components of degrees 11, 6
  Description
    Text
      We construct via linkage an arithmetically Gorenstein
      3-fold $X = X_{6} \cup X_{11} \subset \bf{P}^7$, 
      of degree 17, having Betti table of type [310].
      For an artinian reduction $A_F$, the quadratic part of the ideal $F^\perp$ is the ideal
      of a conic and two independent points $p_1,p_2$, and $F^\perp$ contains a pencil of ideals of five points on the conic and the two fixed points $p_1,p_2$.  
      We construct $X_{11}$ in a quadric in a P6, and $X_{6}$ in a quadric in a P5.
      In the construction the intersection $X\cup X'$ of a component $X$ with the other is an anticanonical divisor on $X$.
       
       The Betti table is
      
      $\phantom{WWWW}
      \begin{matrix}
      &0&1&2&3&4\\
      \text{total:}&1&8&14&8&1\\
      \text{0:}&1&\text{.}&\text{.}&\text{.}&\text{.}\\
      \text{1:}&\text{.}&3&1&\text{.}&\text{.}\\
      \text{2:}&\text{.}&5&12&5&\text{.}\\
      \text{3:}&\text{.}&\text{.}&1&3&\text{.}\\
      \text{4:}&\text{.}&\text{.}&\text{.}&\text{.}&1\\
      \end{matrix}
      $

    Example
      kk = QQ;
      U = kk[y0,y1,y2,y3,y4,y5,y6,y7];
      P4a = ideal(y0,y1,y2); --a P4 
      P4b = ideal(y4,y5,y6); --another P4
      P5 = ideal(y4,y5);-- a P5 containing P4b
      P6 = ideal(y6);--a P6 containing P4b
    Example
      CI222 = ideal(random(2,P4a),random(2,P4a),random(2,P4a));--a complete intersection (2,2,2) containing P4a
      Y7 = CI222:P4a; -- a 4-fold of degree 7 linked (2,2,2) to a P4a
      CI23 = ideal(random(2,Y7),random(3,Y7));--a complete intersection (2,3) that contains Y7 
      Z6 = P4b + CI23; --a complete intersection (2,3) in P4b that contain the intersection of P4b and Y7
      X6 = P5 + ideal(random(2,Z6),random(3,Z6));-- a complete intersection (2,3) in P5 with hyperplane section Z6
      Y67 = intersect(Y7,Z6);--the union of the 4-fold Y7 and the Z6 
      Y18 = CI23 + random(3,Y67);--a complete intersection (2,3,3) that contains Y7 and Z6 
      Y11 = Y18:Y7;-- a 4-fold of degree 11 that contains Z6
      X11 = P6 + Y11; --a 3-fold of degree 11 that contains Z6
      X17 = intersect(X11,X6); -- a AG 3-fold in P7 of type (310)
    Example
      (dim X17, degree X17)
      betti res X17
  SeeAlso      
    "[QQ]"
///

doc ///
  Key
    "Type [331], CY of degree 17 via linkage"
  Headline
    lifting to a 3-fold with components of degrees 13 and 4
  Description
    Text
      We construct via linkage a 3-fold $X = X_{13} \cup X_4 \subset \bf{P}^7$, which is an
      arithmetically Gorenstein Calabi-Yau 3-fold of degree 17 in $\PP^7$.
       For an artinian reduction $A_F$, the quadratic part of the ideal $F^\perp$ is the ideal
      of a plane and a point $p$, and $F^\perp$ contains a family of ideals of six points in the plane and the fixed point $p$.  
      We construct $X_{13}$ in a web of cubics in a P6, and $X_{4}$ as a quartic in a P4.
      In the construction the intersection $X\cup X'$ of a component $X$ with the other is an anticanonical divisor on $X$.
       
     
      
      The Betti table is
      
      $\phantom{WWWW}
      \begin{matrix}
      &0&1&2&3&4\\\text{total:}&1&11&20&11&1\\\text{0:}&1&\text{.}&\text{.}&\text{.}&\text{.}\\
      \text{1:}&\text{.}&3&3&1&\text{.}\\\text{2:}&\text{.}&7&14&7&\text{.}\\\text{3:}&\text{.}&1&3&
      3&\text{.}\\\text{4:}&\text{.}&\text{.}&\text{.}&\text{.}&1\\
      \end{matrix}
      $

      This variety $X \subset \PP^7$ has two components, $X_{13}$ in a $\PP^6$ linked to a $\PP^3$ in the
      cubic pfaffians of a 7x7 skew matrix, and $X_4$, a quartic 3-fold in
      a $\PP^4$.  $X_{13}$ and $X_4$ intersect in a hyperplane section of $X_4$.
    Example
      kk = QQ;
      U = kk[y0,y1,y2,y3,y4,y5,y6,y7];
      u7 = ideal vars U;
      P6 = ideal(y0); -- a P6
      P4 = ideal(y1,y2,y3); --a P4
    Text
      We now choose a skew symmetric  $7 \times 7$ matrix of linear forms in $U$
      whose cubic pfaffians contain a $\PP^4$.
    Example
      q1=(gens u7)*random(kk^8,kk^7);
      q2=(gens u7)*random(kk^8,kk^7);
      q3=(gens P4)*random(kk^3,kk^7);
      q4=(gens P4)*random(kk^3,kk^7);
      q5=(gens P4)*random(kk^3,kk^7);
      q6=(gens P4)*random(kk^3,kk^7);
      q7=(gens P4)*random(kk^3,kk^7);
      F1=transpose q1|transpose q2|transpose q3|transpose q4|
         transpose q5|transpose q6|transpose q7;
      F = F1-transpose F1;--a skew symmetric 7x7 matrix ;

      Y14 = pfaffians (6,F); --a 4-fold of degree 14 that contain P4
    Text
      The variety defined by the cubic pfaffians is a 4-fold, has degree 14, 
      and contains the $\PP^4$.
    Example
      (dim Y14, degree Y14)
      isSubset(Y14, P4)
    Text
      We take the residual of the $\PP^4$ in $Y_{14}$.  This is a 4-fold of degree 13.
    Example
      Y13 = Y14:P4;--a 4-fold of degree 13 
      dim Y13
      degree Y13
    Text
      We intersect this variety with the $\PP^4$, obtaining a quartic hypersurface $X_4$
      in $\PP^4$.
    Example
      X4 = trim(Y13 + P4);
    Text
      Let $X_{13}$ be the 3-fold which is the intersection of $Y_{13}$ with the hyperplane $y_0 = 0$.
    Example
      X13 = trim(P6 + Y13);--  A 3-fold of degree 13 in  P6
      dim X13
      degree X13
    Text
      The union $X$ of $X_{13}$ and $X_4$ is a 3-fold 
      of degree 17 in $\PP^7$, with Betti table of type [331]
    Example
      X = intersect (X4,X13);  --A 3-fold of degree 17 in P7, the union of X13 and X4, with betti table of type 331
      dim X
      degree X
      betti res X
    Text
      The intersection of $X_4$ and $X_{13}$ is a quartic surface in a $\PP^3$,
      which is a hyperplane section of $X_4$.
    Example
      Z = trim(X13 + X4);
      dim Z -- 3, therefore a surface in projective space
      degree Z
      dim X4
      X4 + ideal(y0) == Z
  SeeAlso      
    "[QQ]"
///

doc ///
  Key
    "Type [420], CY of degree 16 via linkage"
  Headline
    lifting to an irreducible 3-fold
  Description
    Text
      We construct via linkage an arithmetically Gorenstein
      irreducible 3-fold $X = X_{16} \bf{P}^7$, 
      of degree 16, having Betti table of type [420].
       For an artinian reduction $A_F$, the quadratic part of the ideal $F^\perp$ is the ideal
      of six points on a twisted cubic curve.
      We construct $X_{16}$ as an anticanonical divisor in the fourfold intersection of a cubic scroll and quadric.
      
      The betti table is
      $\phantom{WWWW}
      \begin{matrix}
        &0&1&2&3&4\\
        \text{total:}&1&6&10&6&1\\
        \text{0:}&1&\text{.}&\text{.}&\text{.}&\text{.}\\
        \text{1:}&\text{.}&4&2&\text{.}&\text{.}\\
        \text{2:}&\text{.}&2&6&2&\text{.}\\
        \text{3:}&\text{.}&\text{.}&2&4&\text{.}\\
        \text{4:}&\text{.}&\text{.}&\text{.}&\text{.}&1\\
        \end{matrix}
      $

    Example
      --Construction of an AGCY of degree 16 in P7 with betti table 2.4  (420).  X16 is irreducible.  It lies in a cubic 5-fold scroll T3 and is singular
      --in its intersection with the vertex of this scroll
      kk=QQ;
      U=kk[y0,y1,y2,y3,y4,y5,y6,y7];
      P4=ideal(y0,y1,y2);--a P4
      T3=minors(2,matrix{{y0,y1,y2},{y3,y4,y5}});--a cubic 5-fold scroll with P4 as a ruling 
      X2=P4+ideal(random(2,U));-- a quadric 3-fold in T3
      X18=T3+ideal(random(2,X2),random(3,X2));--a 3-fold of degree 18 in T3 that contains X2
      X16=X18:X2;--a 3-fold of degree 16 in T3 with betti table of type 420
    Example
      (dim X16, degree X16)
      betti res X16
  SeeAlso      
    "[QQ]"
///

doc ///
  Key
    "Type [430], CY of degree 16 via linkage"
  Headline
    lifting to a 3-fold with components of degrees 10, 6
  Description
    Text
      We construct via linkage an arithmetically Gorenstein
      3-fold $X = X_{10} \cup X_{6} \subset \bf{P}^7$, 
      of degree 16, having Betti table of type [430].
       For an artinian reduction $A_F$, the quadratic part of the ideal $F^\perp$ is the ideal
      of a line and three independant points $p_1,p_2,p_3$, and $F^\perp$ contains pencils of ideals of three points on the line and the three fixed points $p_i$.
      In the pencil we find an ideal of four points in a plane and two on an independant line passing though one of the four points.    
      We construct $X_{10}$ in a pencil of quadrics in a P6, and $X_{6}$ as a complete intersection in a P5.
      In the construction the intersection $X\cup X'$ of a component $X$ with the other is an anticanonical divisor on $X$.
      
      The Betti table is
      $\phantom{WWWW}
      \begin{matrix}
      &0&1&2&3&4\\
      \text{total:}&1&7&12&7&1\\
      \text{0:}&1&\text{.}&\text{.}&\text{.}&\text{.}\\
      \text{1:}&\text{.}&4&3&\text{.}&\text{.}\\
      \text{2:}&\text{.}&3&6&3&\text{.}\\
      \text{3:}&\text{.}&\text{.}&3&4&\text{.}\\
      \text{4:}&\text{.}&\text{.}&\text{.}&\text{.}&1\\
      \end{matrix}
      $

    Example
      --A reducible AGCY 3-fold in P7, with three components, X1,X9,X6 of degrees 1,9,6.  X6 is a complete intersection (1,1,2,3), 
      --X10=X1+X9 is linked to a quadric 3-fold in (1,2,2,3), and X1+X9 intersects X6 in a hyperplane section of X6. 
      kk=QQ;
      U=kk[y0,y1,y2,y3,y4,y5,y6,y7];
      P4=ideal(y0,y1,y2);--a P4
      P5=ideal(y1,y2);--a P5
      X2=P4+ideal(random(2,U));--a quadric 3-fold in P4
      CI1223=ideal(y0)+ideal(random(2,P4),random(2,P4),random(3,X2)); --a complete intersection (1,2,2,3) that intersects  P4 in X2 and X1, a P3.
      X10=CI1223:X2;  --a reducible 3-fold X10 of degree 10 linked to the quadric threefold X2.  X10 is the union of X1 and  3-fold X9 of degree 9.
      Z6=X10+X2;----a complete intersection (1,1,1,2,3) surface  contained in X10
      X6=P5+ideal(random(2,Z6),random(3,Z6));-- X6 is a complete intersection (1,1,2,3) that intersects s10 in a hyperplane section of X6
      X16=intersect(X6,X10);--a 3-fold of degree 16 in P7 with betti table of type 430.  X16 is the union of X6, X1 and X9
      betti res X16 
  SeeAlso      
    "[QQ]"
///

doc ///
  Key
    "Type [441a], CY of degree 16"
  Headline
    lifting to a 3-fold with components of degrees 12, 4
  Description
    Text
      We construct via linkage an arithmetically Gorenstein
      3-fold $X = X_{12} \cup X_{4} \subset \bf{P}^7$, 
      of degree 16, having Betti table of type [441], and is on component [441a].
       For an artinian reduction $A_F$, the quadratic part of the ideal $F^\perp$ is the ideal
      of a conic and an independant point p, and $F^\perp$ contains pencils of ideals of five points on the conic and the point p.    
      So we construct $X_{12}$ in a pencil of codimension 3 varieties of degree 5 in a quadric in a P6, and $X_{4}$ in an independant P4.
      In the construction the intersection $X\cup X'$ of a component $X$ with the other is an anticanonical divisor on $X$.
      
      The Betti table is
      $\phantom{WWWW}
      \begin{matrix}
      &0&1&2&3&4\\
      \text{total:}&1&9&16&9&1\\
      \text{0:}&1&\text{.}&\text{.}&\text{.}&\text{.}\\
      \text{1:}&\text{.}&4&4&1&\text{.}\\
      \text{2:}&\text{.}&4&8&4&\text{.}\\
      \text{3:}&\text{.}&1&4&4&\text{.}\\
      \text{4:}&\text{.}&\text{.}&\text{.}&\text{.}&1\\
      \end{matrix}
      $

    Example
      --CY of degree 16 and betti diagram 2.6 (441a), with two components X12 og degree 12, in a quadric in a P6, and X4 a quartic 3-fold in a P4.
      --X4 and X12 intersect in a hyperplane section of X4
      kk=QQ;
      U=kk[y0,y1,y2,y3,y4,y5,y6,y7];
      P3=ideal(y0,y1,y2,y3);--a P3
      P4=ideal(y1,y2,y3);--a P4
      P6=ideal(y0);--a P6
      M24=matrix{{y1,y2,random(2,U),random(2,U)},{y2,y3,random(2,U),random(2,U)}};--a 2x4 matrix with two columns of linear and two of quadratic forms
      X12=P6+minors(2,M24);--a 3-fold of degree 12 in P6
      Z4=P3+X12;--a quartic surface in X12
      X4=P4+ideal(random(4,Z4));--a quartic 3-fold 
      X16=intersect(X12,X4);-- a 3-fold of degree 16 in P7 with betti table of type 441
      betti res X16
  SeeAlso      
    "[QQ]"
///

doc ///
  Key
    "Type [441b], CY of degree 16"
  Headline
    lifting to a 3-fold with components of degrees 8, 8
  Description
    Text
      We construct via linkage an arithmetically Gorenstein
      3-fold $X = X_{8} \cup X_{8}' \subset \bf{P}^7$, 
      of degree 16, having Betti table of type [441], on component [441b].
       For an artinian reduction $A_F$, the quadratic part of the ideal $F^\perp$ is the ideal
      of two skew lines, and $F^\perp$ contains pencils of ideals of three points on one line and three fix points on the other.   
      So we construct $X_{8}$ in the intersection of two cubics in a P5 and $X_{8}'$ in the intersection of two cubics in another P5.
      In the construction the intersection $X\cup X'$ of a component $X$ with the other is an anticanonical divisor on $X$.
      
      The Betti table is
      $\phantom{WWWW}
      \begin{matrix}
      &0&1&2&3&4\\
      \text{total:}&1&9&16&9&1\\
      \text{0:}&1&\text{.}&\text{.}&\text{.}&\text{.}\\
      \text{1:}&\text{.}&4&4&1&\text{.}\\
      \text{2:}&\text{.}&4&8&4&\text{.}\\
      \text{3:}&\text{.}&1&4&4&\text{.}\\
      \text{4:}&\text{.}&\text{.}&\text{.}&\text{.}&1\\
      \end{matrix}
      $

    Example
      --CY of degree 16  with betti table 2.6  (441b).  Two components X8a and X8b each of degree 8 and linked to a P3 in (1,1,3,3).
      --X8a and X8b intersect in a quartic surface in a P3
      kk=QQ;
      U=kk[y0,y1,y2,y3,y4,y5,y6,y7];
      P5a=ideal(y0,y1);--a P5 containing P3c
      P5b=ideal(y2,y3);--another P5 containing P3c
      P3=P5a+P5b;--the common P3 of P5a and P5b
      F0=matrix{{random(2,U),random(2,U)},{random(2,U),random(2,U)}};--a 2x2 matrix of quadrics,  
      --the quartic determinant restricts to a surface Z4 in P3
      F1=matrix{{y2},{y3}}|F0;--a 2x3 matrix, one columns of linear forms, and two of quadrics,
      X8a=P5a+minors(2,F1);--a 3-fold of degree 8 in P5a linked (1,1,3,3) to P3 
      F2=matrix{{y0},{y1}}|F0;--a 2x3 matrix, one columns of linear forms, and two of quadrics,
      X8b=P5b+minors(2,F2);--a 3-fold of degree 8 in P5b linked  (1,1,3,3) to P3
      X16=intersect(X8a,X8b);-- a 3-fold of degree 16 in P7 with betti table of type 441b
      --X8a and X8b intersect along Z4
      betti res X16
  SeeAlso      
    "[QQ]"
///

doc ///
  Key
    "Type [551], CY of degree 15 via linkage"
  Headline
    lifting to a 3-fold with components of degrees 11 and 4
  Description
    Text
      We construct via linkage a variety $X = X_{15} = X_{11} \cup X_{4} \subset \PP^7$, with Betti table
      
      $\phantom{WWWW}
      \begin{matrix}
      &0&1&2&3&4\\\text{total:}&1&7&12&7&1\\\text{0:}&1&\text{.}&\text{.}&\text{.}&\text{.}\\\text{1:}&\text{.}&5&5&1&\text{.}\\\text{2:}&\text
      {.}&1&2&1&\text{.}\\\text{3:}&\text{.}&1&5&5&\text{.}\\\text{4:}&\text{.}&\text{.}&\text{.}&\text{.}&1\\
      \end{matrix}  
      $
      
      For an artinian reduction $A_F$, the quadratic part of the ideal $F^\perp$ is the ideal
      of $\Gamma=\Gamma_4\cup p$, the union of a four points in a plane and one point outside.
      So we construct $X_{11}$ in the intersection of two quadrics in a P6 and $X_4$ in an independant P4.
      In the construction the intersection $X\cup X'$ of a component $X$ with the other is an anticanonical divisor on $X$.
      
      We start with a linear $\mathbb{P}^3 \subset \mathbb{P}^7$, take the linked ideal via a $(1,2,2,3)$ complete intersection
      containing the $\mathbb{P}^3$.
    Example
      kk = QQ;
      U = kk[y0,y1,y2,y3,y4,y5,y6,y7];
      P3 = ideal(y0,y1,y2,y3); --a P3
      CI = ideal(y0, random(2, P3), random(2, P3), random(3, P3));
      X11 = CI : P3; -- degree 11, codim 4.
      (codim X11, degree X11)
      s114 = X11 + P3; -- intersect X11 with the P3.  Take a random quartic in this ideal.
      X4 = ideal(y1,y2,y3, random(4, s114));
      X15 = intersect(X4, X11);
      assert isPrime(X4 + X15) -- a quartic in P^3.
      (dim X15, degree X15)
      betti resolution X15
  SeeAlso      
    "[QQ]"
///

doc ///
  Key
    "Type [562] with lifting of type I, a CY of degree 15 via linkage"
  Headline
    lifting to a 3-fold with components of degree 8, 7
  Description
    Text
      We construct via linkage an arithmetically Gorenstein
      3-fold $X = X_{8} \cup X_{7} \subset \bf{P}^7$, 
      of degree 15, with two components of degrees 7 and 8,
      having Betti table of type [562].   For an artinian reduction $A_F$, the ideal $F^\perp$ contains a pencil of ideals
      $I_\Gamma$, where $\Gamma=\Gamma_3\cup\Gamma_2$, the union of a three points in a line $L$ and two fixed points on a line $L'$ skew to $L$ .
      So we construct $X_8$ in the complete intersection of two cubics in a P5 and $X_7$ in a complete intersection $(2,4)$ in another P5.
      In the construction the intersection $X\cup X'$ of a component $X$ with the other is an anticanonical divisor on $X$.
      
      The betti table is
      $\phantom{WWWW}
      \begin{matrix}
          &0&1&2&3&4\\
          \text{total:}&1&9&16&9&1\\
          \text{0:}&1&\text{.}&\text{.}&\text{.}&\text{.}\\
          \text{1:}&\text{.}&5&6&2&\text{.}\\
          \text{2:}&\text{.}&2&4&2&\text{.}\\
          \text{3:}&\text{.}&2&6&5&\text{.}\\
          \text{4:}&\text{.}&\text{.}&\text{.}&\text{.}&1\\
          \end{matrix}
      $

      $X_{7}$ is a 3-fold of degree 7 linked via a $(2,4)$ complete
      intersection to a $\PP^3$ in a $\PP^5$.  The other component
      $X_8$ of $X$ is a 3-fold of degree 8 linked via a $(3,3)$
      complete intersection to another $\PP^3$ in another $\PP^5$.
      These are constructed do that $X_7$ and $X_8$ intersect in a
      quartic surface $Z_4$ in the $\PP^3$ which is the intersection
      of the span of $X_7$ and $X_8$.
    Example
      -- Construction of a reducible AG CY threefold of degree 15 in
      --  P7 with betti table (2.8) (562) the CY X15 is the union of a
      --  3-fold X7 (linked (2,4) to a P3 in a P5) and a 3-fold X8
      --  (linked (3,3) to a P3 in a P5), such that X7 and X8
      --  intersect in a quartic surface Z4 in the P3 intersection of
      --  the span of X7 and X8.
      kk=QQ;
      U=kk[y0,y1,y2,y3,y4,y5,y6,y7];
      P5c=ideal(y0,y1);  -- a P5
      P5a=ideal(y2,y3);  --P5 of S8
      P5b=ideal(y4,y5);   --P5 of S7
      P3ac=P5a+P5c;-- P3 intersection of P5a and P5c
      P3bc=P5b+P5c;
      P1=P5a+P5b+P5c; --a line L, the intersection of all three P5s
      F=matrix{{y0,random(2,U),random(2,P1)},{y1,random(2,U),random(2,P1)}};
    Example
      X8=P5a+minors(2,F);-- a 3-fold of degree 8 in P5a
      Z4=P5c+X8; -- a quartic surface in P3ac that contains L
      XY=P5c+ideal(random(2,intersect(P5a,P5b)),random(4,intersect(Z4,P5b)));
    Example
      X7=XY:P3bc;--a 3-fold of degree 7 in P5b
      (dim X7, degree X7)
      betti res X7    
    Example
      X15=intersect(X7,X8);--a 3-fold of degree 15 in P7 with betti table of type [562],  the union of X7 and X8.
      (dim X15, degree X15)
      betti res X15
  SeeAlso      
    "[QQ]"
///

doc ///
  Key
    "Type [562] with a lifting of type II, a CY of degree 15 via linkage"
  Headline
    lifting to a 3-fold with components of degrees 7, 4, 4
  Description
    Text
      We construct via linkage an arithmetically Gorenstein
      3-fold $X = X_7& \cup X_{4} \cup X_{4}' \subset \bf{P}^7$, 
      of degree 15, with components of degrees $7, 4, 4$,
      having Betti table of type [562]. For an artinian reduction $A_F$, the ideal $F^\perp$ contains a pencil of ideals
      $I_\Gamma$, where $\Gamma=\Gamma_3\cup p_1\cup p_2$, the union of a three points in a fixed line and two independant points outside the line.
      So we construct $X_7$ in the intersection of two cubics in a P5 and $X_4$ and $X_4'$ as quartics in an independant P4s.
      In the construction the intersection $X\cup (X'\cap X'')$ of a component $X$ with the two others is an anticanonical divisor on $X$.
      --, and lying on the
      --component [562b].
      
      The betti table is
      $\phantom{WWWW}
      \begin{matrix}
          &0&1&2&3&4\\
          \text{total:}&1&9&16&9&1\\
          \text{0:}&1&\text{.}&\text{.}&\text{.}&\text{.}\\
          \text{1:}&\text{.}&5&6&2&\text{.}\\
          \text{2:}&\text{.}&2&4&2&\text{.}\\
          \text{3:}&\text{.}&2&6&5&\text{.}\\
          \text{4:}&\text{.}&\text{.}&\text{.}&\text{.}&1\\
          \end{matrix}
      $

      $X_7$ is linked to a reducible quadric 3-fold $Y$ in a complete
      intersection $(1,1,3,3)$.  $X_4$ and $X_4'$ are quartic 3-folds
      that each intersect $X_7$ in a cubic surface, while they
      intersect each other in a plane.  The cubic surfaces are the
      intersection of $X_7$ with the components of $Y$, and the plane
      is the intersection of these components.
    Example
      kk=QQ;
      U=kk[y0,y1,y2,y3,y4,y5,y6,y7];
      P5=ideal(y0,y1);--a P5
      P3a=ideal(y0,y1,y2,y3);-- a P3
      P3b=ideal(y0,y1,y2,y4);-- another P3
      P4a=ideal(y0,y2,y3);-- a P4
      P4b=ideal(y1,y2,y4);-- a P4
      X2=ideal(y0,y1,y2,y3*y4);--a reducible quadric
      CI1133=P5+ideal(random(3,X2),random(3,X2));--a complete intersection (1,1,3,3) that contain X2.
      X7=CI1133:X2;  -- a 3-fold of degree 7, linked (1,1,3,3) to X2
    Example
      (dim X7, degree X7)
    Example
      Z3a=P3a+X7; -- a cubic surface
      Z4a=intersect(Z3a,P3b);-- the union of Z3a and P3b
      X4a=P4a+ideal(random(4,Z4a));-- a quartic 3-fold in P4 that contains a plane in P3b and the cubic surface Z3
    Example
      (dim X4a, degree X4a)
    Example
      Z3b=X7+P3b;-- a cubic surface
      Z4b=intersect(Z3b,P3a);-- the union of Z3b and P3b
      X4b=P4b+ideal(random(4,Z4b));-- a quartic 3-fold in P4 that contains a plane in P3b and the cubic surface Z3
    Example
      (dim X4b, degree X4b)
    Text
      The union $X = X_7 \cup X_4 \cup X_4' \subset \PP^7$ has Betti table [562].
    Example
      X15=intersect(X7,X4a,X4b);--a 3-fold of degree 15, with betti table of type [562], with three components, X7 of degree 7, and X4a and X4b of degree 4.
      (dim X15, degree X15)
      betti res X15
  SeeAlso      
    "[QQ]"
///
