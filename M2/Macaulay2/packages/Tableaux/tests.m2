

TEST /// -- skewTableau, size T, skewShape, truncate skewShape, pad skewShape, standardize

  lam = new Partition from {4,1,0,-3,0,2,0,0}
  mu = new Partition from {1,0,0,2,-2,-1,-5,-1,0,0}
  entryList = toList(1..20)
  T = skewTableau(lam,mu,entryList)

  assert (toList T#"outerShape" == toList lam and toList T#"innerShape" == toList mu and T#values == entryList)
  assert (size T == 20)

  (lam',mu') = skewShape T
  assert (toList lam' == toList lam and toList mu' == toList mu)

  (lam'',mu'') = pad (lam',mu')
  assert (toList lam'' == {4,1,0,-3,0,2,0,0,0,0} and toList mu'' == {1,0,0,2,-2,-1,-5,-1,0,0})

  (lam'',mu'') = truncate (lam',mu')
  assert (toList lam'' == {4,1,0,-3,0,2} and toList mu'' == {1,0,0,2,-2,-1,-5,-1})

  (lam'',mu'') = standardize (lam',mu')
  assert (toList lam'' == {4,1,0,-3,0,2,0,0} and toList mu'' == {1,0,0,2,-2,-1,-5,-1})

///

TEST /// -- numRows, numColumns, rowRange, columnRange, positionList, toIndex, toPosition
         -- T^i, T_j, T_(i,j), rowEntries, columnEntries

  lam = new Partition from {4,1,0,-3,0,2,0,0}
  mu = new Partition from {1,0,0,2,-2,-1,-5,-1,0,0}
  entryList = toList(1..20)
  T = skewTableau(lam,mu,entryList)

  assert(numRows T == max(# truncate lam, # truncate mu))
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

TEST /// -- youngTableau
         -- isWeaklyDecreasing, isNonnegative

  lam = new Partition from {4,1,0,-3,0,2,0,0}
  mu = new Partition from {1,0,0,2,-2,-1,-5,-1,0,0}
  entryList = toList(1..20)
  T = skewTableau(lam,mu,entryList)

  lam' = new Partition from {6,5,3,1}
  T' = youngTableau(lam')

  assert(isWeaklyDecreasing T == false)
  assert(isWeaklyDecreasing T' == true)

  assert(isNonnegative T == false)
  assert(isNonnegative T' == true)
  
///

TEST /// -- applyEntries, applyPositions
         -- boxContent, hookLength

  lam = new Partition from {6,5,5,4,2,1}
  mu = new Partition from {3,2,2}
  entryList = toList(1..(sum toList lam - sum toList mu))
  T = skewTableau(lam,mu,entryList)

  assert(entries applyEntries(T, theBox -> theBox^2) == apply(entries T, theBox -> theBox^2))
  assert(entries applyPositions(T, thePosition -> thePosition) == positionList T)

  assert(boxContent(3,1) == -2)
  assert(apply(positionList T, thePosition -> hookLength(thePosition,T)) == {6,4,1,5,4,2,4,3,1,6,4,2,1,3,1,1})
  
///

TEST /// -- allSemistandardTableaux, numSemistandardTableaux

  lam = new Partition from {6,5,4,2,1}
  
  assert(numSemistandardTableaux(lam, #lam) == # allSemistandardTableaux(lam, #lam))

///
