-- Author: Lily Silverstein
-- Rewritten August 2018

doc ///
 Key
  leadComponent
  (leadComponent, Matrix)
  (leadComponent, Vector)
 Headline
  the leading component(s) of a vector or matrix
 Usage
  leadComponent v
 Inputs
  v:Vector
   or @TO Matrix@
 Outputs
  :ZZ 
   or @TO List@ of integers, the largest index of a nonzero element of {\tt v}
 Description
  Example
   leadComponent vector{0,0,1,0,1}
   leadComponent vector{0,0,1,2,3,4,0}
   leadComponent matrix{{0,0,1}, {0,1,0}, {1,0,0}}
  Text
   {\em Leading} appears to be a bit of a misnomer here, since the index/indices return
   are the last, not the first, corresponding to nonzero elements.
   
   This command also works with vectors and matrices that contain polynomial
   ring elements.
  Example
   R = ZZ/5[a,b,c]; leadComponent vector{a*b^2,3*b*c^3,0}
 SeeAlso
  leadCoefficient
  leadMonomial
  leadTerm
  position
  positions
  select
  support
///
