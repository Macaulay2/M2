

TEST /// -- youngTableau, size T, skewShape, trim skewShape, pad skewShape, standardize
         -- shape, innerShape, outerShape

  lam = new Partition from {4,1,0,-3,0,2,0,0}
  mu = new Partition from {1,0,0,2,-2,-1,-5,-1,0,0}
  entryList = toList(1..20)
  T = youngTableau(lam,mu,entryList)

  assert (toList T#"outerShape" == toList lam and toList T#"innerShape" == toList mu and T#values == entryList)
  assert(toList T#"outerShape" == toList outerShape T)
  assert(toList T#"innerShape" == toList innerShape T)
  assert(toList shape youngTableau lam == toList lam)
  assert (size T == 20)

  (lam',mu') = skewShape T
  assert (toList lam' == toList lam and toList mu' == toList mu)

  (lam'',mu'') = pad (lam',mu')
  assert (toList lam'' == {4,1,0,-3,0,2,0,0,0,0} and toList mu'' == {1,0,0,2,-2,-1,-5,-1,0,0})

  (lam'',mu'') = trim (lam',mu')
  assert (toList lam'' == {4,1,0,-3,0,2} and toList mu'' == {1,0,0,2,-2,-1,-5,-1})

  (lam'',mu'') = standardize (lam',mu')
  assert (toList lam'' == {4,1,0,-3,0,2,0,0} and toList mu'' == {1,0,0,2,-2,-1,-5,-1})

///

TEST /// -- mutableYoungTableau, tabloid
         -- verticalConcatenate

 lam = new Partition from {3,3,1}
 mu = new Partition from {1}
 entryList = {4, 5, 3, 2, 1, 6}

 assert(entries representative tabloid(lam,mu,entryList) == {4, 5, 1, 2, 3, 6})
 
 T = mutableYoungTableau(lam,mu,entryList)
 T_(1,0) = 9
 
 assert(toList entries T == {4, 5, 9, 2, 1, 6})
 assert(entries verticalConcatenate {T,T} == (toList entries T | toList entries T))
 
///

TEST /// -- numRows, numColumns, rowRange, columnRange, positionList, toIndex, toPosition
         -- T^i, T_j, T_(i,j), rowEntries, columnEntries

  lam = new Partition from {4,1,0,-3,0,2,0,0}
  mu = new Partition from {1,0,0,2,-2,-1,-5,-1,0,0}
  entryList = toList(1..20)
  T = youngTableau(lam,mu,entryList)

  assert(numRows T == max(# trim lam, # trim mu))
  assert(numColumns T == max(toList lam | toList mu) - min(toList lam | toList mu))
  assert(rowRange T == (0..(numRows T - 1)))
  assert(columnRange T == (min(toList lam | toList mu)..(max(toList lam | toList mu)-1)))

  for i in rowRange T do (
      for j in columnRange T do (
          assert(T^i#j == T_j#i)
          )
      )

  for i from 0 to size T - 1 do (
      assert(T_(toPosition(T,i)) == i + 1)
      )

  assert(rowEntries(T,3) == {5,6,7,8,9})
  assert(columnEntries(T,0) == {4,8,13})
  
///

TEST /// -- applyEntries, applyPositions
         -- boxContent, hookLength

  lam = new Partition from {6,5,5,4,2,1}
  mu = new Partition from {3,2,2}
  entryList = toList(1..(sum toList lam - sum toList mu))
  T = youngTableau(lam,mu,entryList)

  assert(entries applyEntries(T, theBox -> theBox^2) == apply(entries T, theBox -> theBox^2))
  assert(entries applyPositions(T, thePosition -> thePosition) == positionList T)

  assert(boxContent(3,1) == -2)
  assert(apply(positionList T, thePosition -> hookLength(thePosition,T)) == {6,4,1,5,4,2,4,3,1,6,4,2,1,3,1,1})
  
///

TEST /// -- rowStabilizer, columnStabilizer, readingWord

  lam = new Partition from {2,2,1}
  entryList = {1,4,2,5,3}
  T = youngTableau(lam, entryList)

  assert(# columnStabilizer T == 12)
  assert(# rowStabilizer T == 4)

  lam' = new Partition from {3,2}
  entryList' = {1,3,4,2,5}
  T' = youngTableau(lam', entryList')

  assert(readingWord T' == {2, 1, 5, 3, 4})

///

TEST /// -- youngTableau
         -- isWeaklyDecreasing, isNonnegative
         -- randomSemistandardTableau, randomStandardTableau, randomTabloid
         -- isCorner, isSemistandard, isStandard, isSkew
         -- representative
         -- shift, unshift

  lam = new Partition from {4,1,0,-3,0,2,0,0}
  mu = new Partition from {1,0,0,2,-2,-1,-5,-1,0,0}
  entryList = toList(1..20)
  T = youngTableau(lam,mu,entryList)

  lam' = new Partition from {6,5,3,1}
  T' = youngTableau(lam')

  assert(isSkew T)

  assert(isWeaklyDecreasing T == false)
  assert(isWeaklyDecreasing T' == true)

  assert(isNonnegative T == false)
  assert(isNonnegative T' == true)

  assert(isSemistandard randomSemistandardTableau lam')
  assert(isStandard randomStandardTableau lam')

  correctCorners = {false, false, false, false, false, true, false, false, false, false, true, false, false, true, true}
  assert(entries applyPositions(T',thePosition -> isCorner(thePosition,T')) == correctCorners)

  assert(instance(representative randomTabloid lam',YoungTableau))

  assert(T' == unshift shift T')
  
///

TEST /// -- allSemistandardTableaux, numSemistandardTableaux
         -- allStandardTableaux, numStandardTableaux
         -- allTalboids, numTabloids
         -- allSubPartitions

  lam = new Partition from {6,5,4,2,1}
  
  assert(numSemistandardTableaux(lam, #lam) == # allSemistandardTableaux(lam, #lam))

  lam = new Partition from {5,4,2,1}
  
  assert(numStandardTableaux(lam) == # allStandardTableaux(lam))

  lam = new Partition from {5,3,2}

  assert(numTabloids(lam) == # allTabloids(lam))

  assert(# allSubPartitions(new Partition from {4,3,1}, new Partition from {2,1}) == 16)
///

TEST /// -- youngDiagram, ferrersDiagram, verticalNet, horizontalNet

  T = youngTableau(new Partition from {4,3,1}, new Partition from {2,1}, {1,2,3,4,5})
  yd = youngDiagram T
  fd = ferrersDiagram T
  assert(class yd === Net and class fd === Net)
  assert((width yd, height yd) == (17, 2))
  assert((width fd, height fd) == (8, 1))
  ydp = youngDiagram(new Partition from {3,2})
  assert((width ydp, height ydp) == (13, 2))
  T1 = youngTableau(new Partition from {3,1})
  T2 = youngTableau(new Partition from {2})
  vn = verticalNet {T1, T2}
  hn = horizontalNet {T1, T2}
  assert(class vn === Net and class hn === Net)
  assert((width vn, height vn) == (13, 2))
  assert((width hn, height hn) == (22, 2))

///

TEST /// -- drawInnerShape (toggles whether net draws the inner shape)

  assert(drawInnerShape true === true)
  assert(drawInnerShape false === false)
  T = youngTableau(new Partition from {5,4,1}, new Partition from {2,1,1}, toList(1..6))
  drawInnerShape true
  netOn = toString net T
  drawInnerShape false
  netOff = toString net T
  assert(netOn != netOff)

///

TEST /// -- toPartitionChain

  T = youngTableau(new Partition from {3,2}, {1,1,2,2,3})
  chain = toPartitionChain T
  assert(class chain === Sequence)
  assert(apply(toList chain, p -> toList trim p) == {{}, {2}, {3,1}, {3,2}})
  assert(toList trim last chain == toList trim shape T)

///

TEST /// -- toIndex (inverse of toPosition, both argument orders)

  T = youngTableau(new Partition from {6,3,2}, new Partition from {2}, toList(10..18))
  assert(all(toList(0..size T - 1), k -> toIndex(toPosition(T,k),T) == k))
  assert(all(toList(0..size T - 1), k -> toIndex(T,toPosition(T,k)) == k))

///

TEST /// -- rowStabilizer, columnStabilizer (orders are Young-subgroup products)

  T = youngTableau(new Partition from {2,2,1}, {1,4,2,5,3})
  rs = rowStabilizer T
  cs = columnStabilizer T
  assert(# rs == product(toList rowRange T, i -> (# rowEntries(T,i))!))
  assert(# cs == product(toList columnRange T, j -> (# columnEntries(T,j))!))
  assert(# rs == # unique rs)
  assert(# cs == # unique cs)

///

TEST /// -- shift, unshift

  T = youngTableau(new Partition from {6,6,5,3,1})
  S = shift T
  assert(toList innerShape S == {0,1,2,3,4})
  assert(toList outerShape S == {6,7,7,6,5})
  assert(toList innerShape shift(T,2) == {2,3,4,5,6})
  assert(unshift S == T)
  assert(unshift(shift(T,2),2) == T)

///
