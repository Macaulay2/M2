

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
