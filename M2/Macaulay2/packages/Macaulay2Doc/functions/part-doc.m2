--- status: rewritten September 2018
--- author(s): Lily Silverstein
--- notes: 

-*
-- TODO
part(InfiniteNumber,InfiniteNumber,Number)
part(InfiniteNumber,ZZ,Number)
part(ZZ,InfiniteNumber,Number)
part(ZZ,Number)
part(ZZ,ZZ,Number)
*-

doc///
 Key
  part
  (part,List,RingElement)
  (part,ZZ,ZZ,VisibleList,RingElement)
  (part,InfiniteNumber,InfiniteNumber,RingElement)
  (part,InfiniteNumber,InfiniteNumber,VisibleList,RingElement)
  (part,InfiniteNumber,ZZ,RingElement)
  (part,InfiniteNumber,ZZ,VisibleList,RingElement)
  (part,Nothing,Nothing,RingElement)
  (part,Nothing,Nothing,VisibleList,RingElement)
  (part,Nothing,ZZ,RingElement)
  (part,Nothing,ZZ,VisibleList,RingElement)
  (part,ZZ,InfiniteNumber,RingElement)
  (part,ZZ,InfiniteNumber,VisibleList,RingElement)
  (part,ZZ,Nothing,RingElement)
  (part,ZZ,Nothing,VisibleList,RingElement)
  (part,ZZ,RingElement)
  (part,ZZ,VisibleList,RingElement)
  (part,ZZ,ZZ,RingElement)
 Headline
  select terms of a polynomial by degree(s) or weight(s)
 Usage
  part(deg, f)
  part(lo, hi, f)
  part(deg, f)
  part(lo, hi, wt, f)
  part(mdeg, f)
  part_deg f
 Inputs
  f:RingElement
   a polynomial
  deg:ZZ
   in which degree to select terms from {\tt f}
  lo:ZZ
   or @TO InfiniteNumber@, the lower bound of degrees to select
  hi:ZZ
   or @TO InfiniteNumber@, the upper bound of degrees to select
  wt:List
   (optionally) a list of integers to use as weights before selecting degrees
  mdeg:List
   a multidegree, in the case that {\tt f} is defined in a multigraded ring
 Outputs
  :RingElement
   the sum of all terms of {\tt f} matching the specified degree or range of degrees (after weighting, if weights given)
 Description
  Text
   To select terms of a single degree, use {\tt part(deg, f)}. An alternate syntax
   uses an underscore.
  Example
   R = QQ[x,y];
   f = (x+y+1)^4
   part(2, f)
   part_2 f
  Text
   To select terms within a range of degrees, use {\tt part(lo, hi, f)}. 
  Example
   part(1, 2, f) 
  Text
   In the next example, we apply the weights {\tt \{2,3\}} before selecting terms. In other words, the term
   {\tt x^ay^b} is considered to have degree {\tt 2a+3b}.
  Example
   part(6, {2,3}, f)
   part(6, 8, {2,3}, f)
  Text
   If the generators of the ring were defined to have non-unit degrees,
   the weights {\em override} those degrees.
  Example
   R = QQ[x,y, Degrees=>{2,3}];
   f = (x+y+1)^4
   part(2, f)
   part(2, {1,1}, f)
  Text
   By omitting {\tt lo} or {\tt hi}, but providing a comma indicating the omission, the range of degrees will
   be unbounded in the appropriate direction.
  Example
   S = QQ[a,b,c]
   g = (a - b*c + 2)^3
   part(4, , g)
   part(, 3, g)
   part(, 3, 1..3, g)
  Text
   Infinite numbers may also be given for the bounds.
  Example
   part(4, infinity, g)
   part(-infinity, 3, g)
   part(-infinity, infinity, 1..3, g)
  Text
   For {\bf multigraded rings}, use a list to specify a single multidegree in the first argument.
   The underscore syntax works here too.
  Example
   R = QQ[x,y,z, Degrees => {{1,0,0},{0,1,0},{0,0,1}}];
   f = (x+y+z)^3
   part({2,0,1}, f)
   part_{2,0,1} f
  Text
   A range of degrees cannot be asked for in the multigraded case.
   Polynomial rings over polynomial rings are multigraded, so either use a 
   multidegree or specify weights to avoid errors.
  Example
   R = QQ[a][x];
   h = (1+a+x)^3
   part(2, {1,0}, h)
   part(2, {0,1}, h)
   part({2,1}, h)
 SeeAlso
  degree
  monomials
  parts
  select
  someTerms
  terms
  "graded and multigraded polynomial rings"
  "manipulating polynomials"
///

  
