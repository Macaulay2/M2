skewSchubertVariety = method(TypicalValue=>Matrix)
skewSchubertVariety(ZZ,ZZ,List,List) := (k,n,l,m)->(
     -- k and n are the integers defining the Grassmanian G(k,n)
     -- l and m are partitions of n
     if #l < k then for i from 1 to k-#l do (l=l|{0}); -- makes sure l have size k
     if #m < k then for i from 1 to k-#m do (m=m|{0}); -- makes sure m have size k
     d:=(k*(n-k)-sum(l)-sum(m));  
     x:=symbol x;
     R:=QQ[x_1..x_d]; -- ring where the variables for the matrix live
     r:=0;
     matrix (
      	  for i from 1 to k list (
               for j from 1 to n list (
            	    if j==i+l_(k-i) then 1
            	    else if j>i+l_(k-i) and j<=(n-k+i-m_(i-1)) then ( 
		     	          r=r+1; 
		     	          x_r 
		     	         )
            	    else 0
         	     ) 
      	       )
      	  )
     )

/// 
restart
-- the code inside triple-slashes is treated as a string, therefore, is ignored by the "load" in the next line
load "pieri.m2"
--l={2,1,0}
--m={1,1,0}
skewSchubertVariety(3,7,{2,1},{1,1})
///