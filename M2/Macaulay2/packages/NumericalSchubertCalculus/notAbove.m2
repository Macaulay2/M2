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
NotAboveLambda = method()
NotAboveLambda(List,ZZ,ZZ) := (lambda,k,n) ->(
  -- We Assume that lambda is not the zero partition {0,0,...,0}
  -- k-#lambda is how many zeroes we have
  L':=unique(lambda);
  pos'corners:= apply(L', l->position(lambda, i->i==l, Reverse=>true));
  maxElements:=apply(pos'corners, i->(  --the maximal elements of the ordered set NotAbove
	  toList(i:(n-k))|toList(k-i:lambda_i-1)
	  ));
  NotAbove := new MutableHashTable;
  maxKey:=0;
  scan(maxElements, m->(
	  sizem := sum(m);
	  if sizem > maxKey then maxKey=sizem;
	  if NotAbove#?sizem then (
	      NotAbove#sizem = append(NotAbove#sizem,m) 
	      )else (
	      NotAbove#sizem = {m});
	  ));
  scan(reverse (0..maxKey-1), i->(--we will start to populate NotAbove from top to bottom
	  NotAbove#i={};
	  scan(NotAbove#(i+1), mu->(
		  ll:=append(mu,0);
		  scan(#ll,j->(
			  if ll#j>ll#(j+1) then(
			      take(mu,j)|{mu_j+1}|take(mu,-j+#mu)
			      NotAbove#i=append(NotAbove#i,
			      )
			  ));
		  ))
	      );
	  ));
  NotAbove
)
end

--verifyLength(l,k)