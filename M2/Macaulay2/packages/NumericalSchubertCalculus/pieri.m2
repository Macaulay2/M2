export {   
   "createRandomFlagsForSimpleSchubert",
   "solveSimpleSchubert"
   }
protect StartSolutions
protect Memoize

solutionsHash := new MutableHashTable;

-------------------------
-- Pieri Homotopy Code --
--------------------------
-- Authors: Anton Leykin
--          Abraham Martin del Campo
--
-- Date:  October 29, 2009
--
-- Last Update: August 11, 2010
------------------------------------
-- Littlewood-Richardson Homotopy --
------------------------------------
-- Authors: Anton Leykin
--          Abraham Martin del Campo
--          Frank Sottile
--          Jan Verschelde
--
-- Date: April 5, 2012
--
-- Last Update: October 16, 2015
------------------------------------

-- needsPackage "NumericalAlgebraicGeometry"
-- we need to use GAP's package


--------------------------------
-- Numerical Pieri Homotopies --
--------------------------------

---------------------
-- Generate partitions for 
-- Children problems of a 
-- partition 'm' 
---------------------
generateChildren = method(TypicalValue=>List)
generateChildren(Sequence, List, List) := (kn, l, m) -> (
     (k,n):=kn;
     L := apply(#m, i->if n-k-l#(k-i-1)>=m#i+1 then take(m,i)|{m#i+1}|drop(m,i+1));
     select(L, a-> a=!=null and a==reverse sort a)
)
-------------------
-- find the position where you need
-- to add a solution of the children problem
-- to have a solution of the parent problem
------------------
positionVariableChildren = method(TypicalValue=>ZZ)
positionVariableChildren(Sequence,List,List,List):=(kn,l,m,v)->(
   -- kn is a sequence (k,n)
   -- l, m are partitions
   -- v is a children partition of m 
   (k,n) := kn;
   i := maxPosition(v-m);
   t := apply(i+1, j->plus(n-k-m_(j)-l_(k-j-1)));
   sum(t)
)

-----------------------
-- precookPieri
--
-- creates a special matrix G_\mu
-- and attach it to E_{\mu\lambda}
----------------------- 
precookPieriHomotopy = method(TypicalValue=>List)
precookPieriHomotopy(Sequence,List,List) := (kn,l,m)->(
     -- k and n are the integers defining the Grassmanian G(k,n)
     -- l and m are partitions of n
     (k,n) := kn;
     l = verifyLength(l, k);
     m = verifyLength(m, k);
     E := skewSchubertVariety(kn,l,m);
     ------------
     -- d is the number of variables i.e. the codimension of the Schubert variety E_{l,m}
     ------------
     d := (k*(n-k)-sum(l)-sum(m));
     S := FFF[vars(53..d+52)];
     T:= apply(#m, i->n-k+i-m#i);
     -- P is a list with the indeces where the special flag has ones
     P:=toList(set toList(0..n-1)-T);
     G:=mutableMatrix(S,n-k,n);
     apply(#P, j->G_(j,P#j)=1);
     F:=matrix E || sub(matrix G, ring E);
     return F;
)

--------------------------
-- Given the partitions l, and m for the Grassmannian Gr(k,n)
-- it creates a flags as random numeric matrices G_1,...,G_m
-- for solving the system defined by these random matrices
-- using Homotopy Continuation
--------------------------
createRandomFlagsForSimpleSchubert = method( )
createRandomFlagsForSimpleSchubert(Sequence, List, List) := (kn,l,m)->(
	 (k,n) := kn;
	 l = verifyLength(l, k);
	 m = verifyLength(m, k);
   d := k*(n-k)-sum(l)-sum(m);
   --apply(d, i->matrix apply(n-k,i->apply(n,j->random FFF)))
   apply(d, i-> random(FFF^(n-k),FFF^n))
   )

     
solveSimpleSchubert = method(TypicalValue=>List)
solveSimpleSchubert(Sequence,List,List,List) := (kn,l,m,G)->(
   -- l and m are partitions of n
   -- G is a flag
   (k,n) := kn;
   l = verifyLength(l, k);
   m = verifyLength(m, k);
   d := k*(n-k)-sum(l)-sum(m);
   E := skewSchubertVariety(kn,l,m);
   if solutionsHash#?{l,m,G} then(
   	 solutionsHash # {l,m,G}
   )
   else if d == 1 then (
      -- solve linear equation
      solutionsHash#{l,m,G} = solveEasy det (matrix E || sub(G#0, ring E), Strategy=>Cofactor)
   )
   else(
      -- generate the children problems
      L:=generateChildren(kn, l,m);
      
      -- once the children problems are solved
      -- store solutions in "start" 
      start := flatten(apply(L, p->(
         C := solveSimpleSchubert(kn,l,p,G);
         i := positionVariableChildren((k,n),l,m,p);
         apply(C,c->insert(i-1,0,c))
         )
      ));
      ---- Create the start system S  ----
      S := apply(take(G,d-1), g->det( matrix E || sub(g, ring E),Strategy=>Cofactor)) | {det(precookPieriHomotopy(kn,l,m), Strategy=>Cofactor)};
      ---- Create the target system T ----
      T := apply(take(G,d), g->det( matrix E || sub(g, ring E), Strategy=>Cofactor)); 
      newR := FFF(monoid[gens ring first S]);
      S = S/(s->sub(s,newR));
      T = T/(t->sub(t,newR));
      ------------------------
      -- make sure your starting set of solutions are in fact solutions
      -- of the Starting system S
      ------------------------
      assert all(start, s->norm sub(matrix{S},matrix{s}) < 1e-3);
      solutionsHash#{l,m,G} = track(S,T,start,NumericalAlgebraicGeometry$gamma=>exp(2*pi*ii*random RR)) / coordinates;
      ---------------------
      ---- make sure that you got solutions of the Target System --
      ---------------------
      assert all(solutionsHash#{l,m,G}, s->norm sub(matrix{T},matrix{s}) < 1e-3);
      solutionsHash#{l,m,G}
      )
)


-----------------------------
---- function written to solve
---- a simple linear equation
solveEasy = method(TypicalValue=>CC)
solveEasy(RingElement) := (p)->(
   R:=ring p;
   var:=support p;
   b:=part(0,p);
   a:=p_(var_(0));
   -- print(p,a,b);
   {{toCC sub((-b)/a, coefficientRing R)}}
)


--------------------------------------
--- trackSimpleSchubert
--------------------------------------
---
--- A function to find solution from a specific instance 
--- of a Schubert problem using homotopy 
--- continuation starting from solving
--- another instance (hopefully easier) of
--- the Schubert problem, but with respect 
--- to a different flag
--------------------------------------

trackSimpleSchubert = method(TypicalValue=>List, Options=>{Memoize => false, StartSolutions=>null})
trackSimpleSchubert(Sequence, Sequence, List, List) := o->(kn,cond,G,F) ->(
   -- G is the start flag and F the target flag
   -- k and n are integers defining the Grassmannian G(k,n)
   (k,n) := kn;
   -- l and m are partitions of n
   (l,m) := cond;
   Sols:= (if o.StartSolutions === null then solveSimpleSchubert(kn,l,m,G) else o.StartSolutions);
   E := skewSchubertVariety(kn,l,m);
   Start:=apply(G, g->det( matrix E || sub(g, ring E),Strategy=>Cofactor));
   Target:=apply(F,f->det( matrix E || sub(f, ring E),Strategy=>Cofactor));
   Ret:=track(Start,Target,Sols,NumericalAlgebraicGeometry$gamma=>exp(2*pi*ii*random RR)) / coordinates;
   if o.Memoize then solutionsHash#{l,m,F} = Ret;  
   return Ret;
)
