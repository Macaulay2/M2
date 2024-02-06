export {   
   "solveSimpleSchubert"
   }
----------------
--Functions contained here but not exported:
----------------
-- generateChildren
-- positionVariableChildren
-- precookPieriHomotopy
-- solveEasy
-- trackSimpleSchubert

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
-- Last Update: October, 2016
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
-- generateChildren
---------------------
-- Generate partitions for 
-- Children problems of a 
-- partition 'm' 
---------------------
-- Input:
--    kn     - sequence of integers (k,n)
--    l,m    - partitions
-- Output:
--    a list of children partitions for m (i.e. multiplying m by one box)
----------------------
generateChildren = method(TypicalValue=>List)
generateChildren(Sequence, List, List) := (kn, l, m) -> (
     (k,n):=kn;
     L := apply(#m, i->if n-k-l#(k-i-1)>=m#i+1 then take(m,i)|{m#i+1}|drop(m,i+1));
     select(L, a-> a=!=null and a==reverse sort a)
)
-------------------
-- positionVariableChildren
-------------------
-- find the position where you need
-- to add a solution of the children problem
-- to have a solution of the parent problem
------------------
-- Input:
--    kn    - sequence of integers (k,n)
--    l,m,v - partitions where v is a children
--            partition of m
-- Output:
--    the position where m gets modified to get to v?
---------------------------
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
-- Input:
--       kn   - sequence of integers (k,n)
--       l,m  - partitions each given as a list
-- Output:
--       F    - an nxn matrix with the skewySchubert variety E
--              on top, and a matrix G of size n-k x n in the shape
--              of a specific Affine patch of Gr(n-k,n)
-----------------------
precookPieriHomotopy = method(TypicalValue=>List)
precookPieriHomotopy(Sequence,List,List) := (kn,l,m)->(
     -- k and n are the integers defining the Grassmannian G(k,n)
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
     -- P is a list with the indices where the special flag has ones
     P:=toList(set toList(0..n-1)-T);
     G:=mutableMatrix(S,n-k,n);
     apply(#P, j->G_(j,P#j)=1);
     F:=matrix E || sub(matrix G, ring E);
     return F;
)


-----------------------
-- solveInternalSimple
-----------------------
-- Uses Pieri homotopies to solve
-- a simple Schubert problem internally
-----------------------
-- Input:
--    kn   - a sequence of 2 integers (k,n) specifying Gr(k,n)
--    l,m  - Lists of two partitions indicating the non-simple Schubert conditions
--    G    - List of flags indicated by (n-k)xn matrices
------------------------
-- Note:   Solves the Schubert problem l,m,{1}^d in Gr(k,n)
--         w.r.t. the flags Id, rsort Id, G
------------------------
solveInternalSimple = method(TypicalValue=>List)
solveInternalSimple(Sequence,List,List,List) := (kn,l,m,G)->(
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
         C := solveInternalSimple(kn,l,p,G);
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

-------------------------
-- solveSimpleSchubert
------------------------
-- Input:
--    SchPblm  - a Schubert problem given as a list
--               of the form {(cond_List, flag_Matrix), ...}
--    k,n      - integers that specify the Grassmannian Gr(k,n)
-- Output:
--    A list of kxn-matrices that are solutions to the problem
-------------------------
-- NOTE:  This way to call it is using the same input
--        as the LR-Homotopies. Here flags are complete (invertible square matrices)
-------------------------
solveSimpleSchubert = method(TypicalValue=>List)
solveSimpleSchubert(List,ZZ,ZZ) := (SchPblm,k,n)->(
   -- SchPblm is a an instance of a Schubert problem, which is a list of pairs (c,F) with c a Schubert conditions and F a flag
   -- Check that it does indeed form a Schubert problem, and convert the conditions to partitions (if they were brackets)
   SchPblm = ensurePartitions(SchPblm,k,n);
   -- set aside the first two conditions
   twoconds := take(SchPblm,2);
   remaining'conditions'flags := drop (SchPblm,2);
   l1 := verifyLength(first first twoconds, k);
   l2 := verifyLength(first last twoconds, k); 
   F1:= promote(last first twoconds, FFF);
   F2:= promote(last last twoconds, FFF);
   simplConds := remaining'conditions'flags/first;
   remaining'flags := remaining'conditions'flags/last;   
   --Slns:={};
   -- checks if it is a valid Simple Schubert problem
   checkSimpleSchubertProblem({l1,l2}|simplConds, k,n);
   checkPartitionsOverlap := (l1+reverse l2)/(i->n-k-i);
   if min(checkPartitionsOverlap) < 0 then
      {}
   else(
       ID:= id_(FFF^n);
       LocalFlags1 := {F1,F2}; 
       LocalFlags2 := {ID,rsort ID}; -- maybe goes ID, resort ID
       At1t2 := moveFlags2Flags(LocalFlags1, LocalFlags2);
       A := first At1t2;
       Ainv := solve(A,ID);
       -- we update the given flags F3 ... Fm
       -- to F3' .. Fm' where Fi' = A*Fi
       new'remaining'flags := A*remaining'flags;
       --new'remaining'flags := apply(remaining'flags, F-> A*F);
       -- we take the first n-k columns and transpose
       -- because SimpleSchubert solves wtr rowSpan and not colSpan
       flagsForSimple:= apply(new'remaining'flags, F->transpose F_{0..n-k-1});
       Sols := solveInternalSimple((k,n),l2,l1,flagsForSimple);
       E:= skewSchubertVariety((k,n),l2,l1);
       apply(Sols, s->Ainv*(transpose sub(E,matrix{s})))
       ) 
)


---------------------------
--      solveEasy
-----------------------------
---- function written to solve
---- a simple linear equation
-----------------------------
-- Input:
--     p   - a linear polynomial a*X + b
-- Output:
--    a list with the solution {{-b/a}}
------------------------------
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
-- Input:
--      kn     - sequence of integers (k,n)
--      conds  - sequence with two partitions (l,m)
--      G, F   - List of flags, G is the starting set of Flags
--               and F is the target
---------------------------------------
trackSimpleSchubert = method(TypicalValue=>List, Options=>{Memoize => false, StartSolutions=>null})
trackSimpleSchubert(Sequence, Sequence, List, List) := o->(kn,cond,G,F) ->(
   -- G is the start flag and F the target flag
   -- k and n are integers defining the Grassmannian G(k,n)
   (k,n) := kn;
   -- l and m are partitions of n
   (l,m) := cond;
   Sols:= (if o.StartSolutions === null then solveInternalSimple(kn,l,m,G) else o.StartSolutions);
   E := skewSchubertVariety(kn,l,m);
   Start:=apply(G, g->det( matrix E || sub(g, ring E),Strategy=>Cofactor));
   Target:=apply(F,f->det( matrix E || sub(f, ring E),Strategy=>Cofactor));
   Ret:=track(Start,Target,Sols,NumericalAlgebraicGeometry$gamma=>exp(2*pi*ii*random RR)) / coordinates;
   if o.Memoize then solutionsHash#{l,m,F} = Ret;  
   return Ret;
)
