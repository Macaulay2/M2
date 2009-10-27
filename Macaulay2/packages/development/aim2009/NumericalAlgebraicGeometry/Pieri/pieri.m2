skewSchubertVariety = method(TypicalValue=>Matrix)
skewSchubertVariety(ZZ,ZZ,List,List) := (k,n,l,m)->(
     -- k and n are the integers defining the Grassmanian G(k,n)
     -- l and m are partitions of n
     if #l < k then for i from 1 to k-#l do (l=l|{0}); -- makes sure l have size k
     if #m < k then for i from 1 to k-#m do (m=m|{0}); -- makes sure m have size k
     d := (k*(n-k)-sum(l)-sum(m));  
     x := symbol x;
     R := QQ[x_1..x_d]; -- ring where the variables for the matrix live
     r := 0;
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
     
generateChildren = method(TypicalValue=>List)
generateChildren List := m -> (
 L := apply(#m, i->take(m,i)|{m#i+1}|drop(m,i+1));
 select(L, a->a==reverse sort a)
)

positionVariableChildren = method(TypicalValue=>ZZ)
positionVariableChildren(Sequence,List,List,List):=(kn,l,m,v)->(
   (k,n) := kn;
   i := maxPosition(v-m);
   t := apply(i+1, j->plus(n-k-m_(j)-l_(k-j-1)));
   sum(t)
)

insertSolution = method(TypicalValue=>List)
insertSolution(List,List) := (v,s) ->(
   i:=positionVariableChildren((k,n),l,m,v);
   take(s,i)|{0}|drop(s,i)
)
precookPieriHomotopy = method(TypicalValue=>List)
precookPieriHomotopy(Sequence,List,List,List) := (kn,l,m,M)->(
     -- k and n are the integers defining the Grassmanian G(k,n)
     -- l and m are partitions of n
     -- M is a list of children solutions {p,s} 
     -- where p is a permutation, s is a list of solutions
     (k,n) := kn;
     if #l < k then for i from 1 to k-#l do (l=l|{0}); -- makes sure l have size k
     if #m < k then for i from 1 to k-#m do (m=m|{0}); -- makes sure m have size k
     E := skewSchubertVariety(k,n,l,m);
     d := (k*(n-k)-sum(l)-sum(m));
     S := QQ[x_1..x_d];
   if #M == d then (
     P := 1..n;
     P = toList P;
     for j from 0 to k-1 do (
       i:= k-1-j;
      if i =!= n-k+i-m_(i) then (P=drop(P,{n-k+i-m_(i),n-k+i-m_(i)}))
      );
      G:=mutableMatrix(S,n-k,n);
      apply(#P, j->G_(j,P_(j)-1)=1);
      F=matrix E || sub(matrix G, ring E);
      return F;
   )
     else "wrong size of M"
)
/// 
restart
-- the code inside triple-slashes is treated as a string, therefore, is ignored by the "load" in the next line
load "pieri.m2"
--l={2,1,0}
--m={1,1,0}
skewSchubertVariety(3,7,{2,1},{1,1})
precookPieriHomotopy(3,7,{2,1},{1,1},{1,1,1,1,1,1,1})
///
