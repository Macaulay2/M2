doc ///
   Key
     submatrixByDegrees
   Headline
     submatrix consisting of rows and columns in an interval or box of degrees
   Usage
     submatrixByDegrees(m, targetBox, sourceBox)
   Inputs
     m:Matrix
       A matrix between free modules
     targetBox:Sequence
       A sequence of 2 degree vectors, {\tt (lodegree, hidegree)}.  All rows whose multi-degree is 
       greater than or equal (in each component) to lodegree, and less than or equal to hidegree, are selected
     sourceBox:Sequence
       Same as targetBox, except this governs which columns are selected
   Outputs
     :Matrix
       The submatrix of {\tt m} consisting of those rows (respectively columns) whose (multi-)degree lie in
       the box {\tt targetBox}, (respectively {\tt columnBox})
   Description
    Text
      If only one degree (as integer, or list of integers) is given for {\tt targetBox} or {\tt sourceBox}, then
      only rows or columns that match that exact degree are used.
      
      
    Example
      R = QQ[a..d];
      I = ideal"a2b-c3,abc-d3,ac2-bd2-cd2,abcd-c4"
      C = res I;
      m = C.dd_2
      submatrixByDegrees(m, 3, 6)
      submatrixByDegrees(m, (3,3), (6,7))
      submatrixByDegrees(m, (4,4), (7,7))
    Text
      For multidegrees, the interval is a box.
    Example
      S = QQ[a..d, Degrees=>{2:{1,0},2:{0,1}}];
      I = ideal(a*d^4, b^3, a^2*d^2, a*b*c*d^3)
      C = res I
      m = C.dd_2
      degrees target m
      degrees source m
      submatrixByDegrees(C.dd_2, ({2,2},{2,4}), ({2,2},{5,4}))
   Caveat
     The degrees are taken from the target and source free modules, 
       not from the matrix entries themselves.
   SeeAlso
     submatrix
///

TEST ///
  -- test of submatrixByDegrees
  R = QQ[a..d]
  I = ideal"a2b-c3,abc-d3,ac2-bd2-cd2,abcd-c4"
  C = res I
  submatrixByDegrees(C.dd_2, (3,3),(6,6))
  submatrixByDegrees(C.dd_2, ({3},{3}),({6},{6}))
  submatrixByDegrees(C.dd_2, ({4},{4}),({},{}))
  submatrixByDegrees(C.dd_2, ({3},{3}),({7},{7}))
  F = source C.dd_2
  -- rawSelectByDegrees(raw F, {-4}, {-3})
  -- rawSelectByDegrees(raw F, {}, {8})
///
