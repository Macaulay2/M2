doc ///
  Key
    "Finding the possible betti tables for points in P^3 with given geometry"
  Headline
     Material from Section 3 of [QQ]
  Description
     Text
       The following code finds the ideal and betti table for a point configuration.
       The point configuration is given by a matrix whose column vectors are the 
       coordinates of the points. The command pointideal does this for a single point,
       and pointsideal does it for several points
     Example
       K = ZZ/101;
       R = K[x_0..x_3];
     Text
       We check this for some special configurations in P^3, first for a set of six points consisting
       of two sets of three collinear points, and second for seven points on a twisted cubic
     Example
       TwoSets3Points=transpose matrix{{1,0,0,0},{0,1,0,0},{1,1,0,0},{0,0,1,1},{0,0,1,0},{0,0,0,1}}**R
       I = pointsIdeal TwoSets3Points
       minimalBetti I
       SevenPointsOnTC=transpose matrix{{1,1,1,1},{1,2,4,8},{1,3,9,27},{1,4,16,64},{1,5,25,125},{1,6,36,216},{1,7,49,343}}**R
       J = pointsIdeal SevenPointsOnTC
       minimalBetti J
     Text
       Finally we check configurations of 3 to 10 generic points in P^3, note 3 points will have a linear form
     Example
       netList(pack(2,apply({3,4,5,6,7,8,9,10},i->(minimalBetti pointsIdeal random(R^4,R^i)))))
  SeeAlso
      "[QQ]"
///
