needsPackage ("NumericalAlgebraicGeometry", FileName=>"../../NumericalAlgebraicGeometry.m2", DebuggingMode=>true)

skewSchubertVariety = method(TypicalValue=>Matrix)
skewSchubertVariety(Sequence,List,List) := (kn,l,m)->(
     -- k and n are the integers defining the Grassmanian G(k,n)
     -- l and m are partitions of n
     (k,n):=kn;
     if #l < k then for i from 1 to k-#l do (l=l|{0}); -- makes sure l have size k
     if #m < k then for i from 1 to k-#m do (m=m|{0}); -- makes sure m have size k
     d := (k*(n-k)-sum(l)-sum(m));  
     R := QQ[vars(53..d+52)]; -- ring where the variables for the matrix live
     r := 0;
     matrix (
      	  for i from 1 to k list (
               for j from 1 to n list (
            	    if j==i+l_(k-i) then 1
            	    else if j>i+l_(k-i) and j<=(n-k+i-m_(i-1)) then ( 
		     	          r=r+1; 
		     	          R_(r-1) 
		     	         )
            	    else 0
         	     ) 
      	       )
      	  )
     )
     
generateChildren = method(TypicalValue=>List)
generateChildren(Sequence, List, List) := (kn, l, m) -> (
 (k,n):=kn;
 L := apply(#m, i->if n-k-l#(k-i-1)>=m#i+1 then take(m,i)|{m#i+1}|drop(m,i+1));
 select(L, a-> a=!=null and a==reverse sort a)
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
   i:=positionVariableChildren(kn,l,m,v);
   take(s,i)|{0}|drop(s,i)
)
precookPieriHomotopy = method(TypicalValue=>List)
precookPieriHomotopy(Sequence,List,List) := (kn,l,m)->(
     -- k and n are the integers defining the Grassmanian G(k,n)
     -- l and m are partitions of n
     (k,n) := kn;
     if #l < k then for i from 1 to k-#l do (l=l|{0}); -- makes sure l have size k
     if #m < k then for i from 1 to k-#m do (m=m|{0}); -- makes sure m have size k
     E := skewSchubertVariety(kn,l,m);
     d := (k*(n-k)-sum(l)-sum(m));
     S := QQ[x_1..x_d];
     T:= apply(#m, i->n-k+i-m#i);
      P:=toList(set toList(0..n-1)-T);
      G:=mutableMatrix(S,n-k,n);
      apply(#P, j->G_(j,P#j)=1);
      F=matrix E || sub(matrix G, ring E);
      return F;
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

launchSimpleSchubert = method()
launchSimpleSchubert(Sequence, List, List) := (kn,l,m)->(
   -- l and m are partitions of n
   (k,n) := kn;
   d := k*(n-k)-sum(l)-sum(m);
   G = apply(d, i->matrix apply(n-k,i->apply(n,j->random QQ)));
   H = new MutableHashTable from {};
   solveSimpleSchubert(kn,l,m)
   )
     
solveSimpleSchubert = method(TypicalValue=>List)
solveSimpleSchubert(Sequence, List,List) := (kn,l,m)->(
   -- l and m are partitions of n
   (k,n) := kn;
   d := k*(n-k)-sum(l)-sum(m);
   E := skewSchubertVariety(kn,l,m);
   if H#?(l,m) then(
   H # (l,m)
   )
   else if d == 1 then (
      -- solve linear equation
      H#(l,m) = solveEasy det (matrix E || sub(G#0, ring E))
   )
   else(
      L:=generateChildren(kn, l,m);
      start := flatten apply(L, p->(
         C := solveSimpleSchubert(kn,l,p);
         i := positionVariableChildren((k,n),l,m,p);
         apply(C,c->insert(i-1,0,c))
         )
      );
      S := apply(take(G,d-1), g->det( matrix E || sub(g, ring E))) | {det(precookPieriHomotopy(kn,l,m))};
      T := apply(take(G,d), g->det( matrix E || sub(g, ring E))); 
      newR := CC_53(monoid[gens ring first S]);
      S = S/(s->sub(s,newR));
      T = T/(t->sub(t,newR));
      assert all(start, s->norm sub(matrix{S},matrix{s}) < 1e-3);
      H#(l,m) = track(S,T,start,gamma=>exp(2*pi*ii*random RR)
      --,Software=>PHCpack
      ) / first
   )
)

solveEasy = method(TypicalValue=>QQ)
solveEasy(RingElement) := (p)->(
   R:=ring p;
   var:=support p;
   b:=part(0,p);
   a:=p_(var_(0));
   -- print(p,a,b);
   {{toCC lift((-b)/a, coefficientRing R)}}
)
