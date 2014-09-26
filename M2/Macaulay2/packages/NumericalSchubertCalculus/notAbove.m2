-- NotAboveLambda
--
-- Function that, given a partition
-- Lambda, it computes the partitions
-- that are not above lambda in the Bruhat order
-------------------------------
-- Input:
--    l -- (List) the partition Lambda
--    k,n --ZZ,ZZ the Grassmannian Gr(k,n)
--
-- Output:
--    NotAbove  -- List of brackets b such that the 
--                 corresponding partition m is not above l 
--
----------------------------- 
notAboveLambda = method()
notAboveLambda(List,ZZ,ZZ) := (lambda,k,n) ->(
  -- We Assume that lambda is not the zero partition {0,0,...,0}
  -- k-#lambda is how many zeroes we have
  L':=unique(lambda);
  pos'corners:= apply(L', l->position(lambda, i->i==l, Reverse=>true));
  maxElements:=apply(pos'corners, i->(  --the maximal elements of the ordered set notAbove
	  toList(i:(n-k))|toList(k-i:lambda_i-1)
	  ));
  print maxElements;
  notAbove := new MutableHashTable;
  addPartition := la -> (
      b := sum la;
      if not notAbove#?b then notAbove#b = {la} 
      else if not member(la, notAbove#b)
      then notAbove#b = notAbove#b | {la}; 
      );
  scan(apply(maxElements, la->select(la,t->t!=0)), addPartition);
  i := max keys notAbove;
  while i>=0 do (
      scan(notAbove#i, la->
	  for j in 0..<#la do
	  if j==#la-1 or la#j>la#(j+1) 
	  then if j==#la-1 and la#j == 1 then addPartition drop(la,-1) 
	  else addPartition replace(j,la#j-1,la) 
	  );
      i = i - 1;
      );
  apply(flatten values notAbove, la->partition2bracket(la,k,n))
  )
{*  
  
  maxKey:=0;
  scan(maxElements, m->(
	  sizem := sum(m);
	  if sizem > maxKey then maxKey=sizem;
	  if notAbove#?sizem then (
	      notAbove#sizem = append(notAbove#sizem,m) 
	      )else (
	      notAbove#sizem = {m});
	  ));
  scan(reverse (0..maxKey-1), i->(--we will start to populate notAbove from top to bottom
	  notAbove#i={};
	  scan(notAbove#(i+1), mu->(
		  ll:=append(mu,0);
		  scan(#ll,j->(
			  if ll#j>ll#(j+1) then(
			      take(mu,j)|{mu_j+1}|take(mu,-j+#mu)
			      notAbove#i=append(notAbove#i,
			      )
			  ));
		  ))
	      );
	  ));
  notAbove
)
end

--verifyLength(l,k)
*}