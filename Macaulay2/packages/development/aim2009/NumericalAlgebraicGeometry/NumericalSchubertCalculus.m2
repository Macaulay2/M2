newPackage(
        "NumericalSchubertCalculus",
        Version => "0.1", 
        Date => "October 29, 2009",
        Authors => {{Name => "", 
                  Email => "", 
                  HomePage => ""}},
        Headline => "a Macaulay2 package for using numerical methods in Schubert Calculus",
        DebuggingMode => true
        )

export {   
   skewSchubertVariety,
   solveSimpleSchubert,
   launchSimpleSchubert
   }

-------------------------
-- Pieri Homotopy Code --
--------------------------
-- Authors: Anton Leykin
--          Abraham Martin del Campo
--
-- Date:  October 29, 2009
-----------------------

needsPackage ("NumericalAlgebraicGeometry", FileName=>"../NumericalAlgebraicGeometry.m2", DebuggingMode=>true)

H := new MutableHashTable;

---------------------------
--  skewSchubertVariety  --
-- Creates Matrix E_{m,l} --
---------------------------
skewSchubertVariety = method(TypicalValue=>Matrix)
skewSchubertVariety(Sequence,List,List) := (kn,l,m)->(
     -- k and n are the integers defining the Grassmanian G(k,n)
     -- l and m are partitions of n
     (k,n):=kn;
     ----- make sure both partitions are of size k  ----
     if #l < k then x := for i to k-#l-1 list 0;
     l = l | x; -- makes sure l have size k
     if #m < k then x := for i to k-#l-1 list 0;
     m = m | x; -- makes sure m have size k
     ---------------------------------------------------
     d := (k*(n-k)-sum(l)-sum(m));  
     R := CC[vars(53..d+52)]; -- ring where the variables for the matrix lie
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


insertSolution = method(TypicalValue=>List)
insertSolution(List,List) := (v,s) ->(
   ---------------
   -- v is a children partition
   -- s is a solution of the children problem E(l,v)
   ------------------------
   i:=positionVariableChildren(kn,l,m,v);
   take(s,i)|{0}|drop(s,i)
)


-----------------------
-- precookPieri
-- creates a special matrix G_\mu
-- and attach it to E_{\mu\lambda}
----------------------- 
precookPieriHomotopy = method(TypicalValue=>List)
precookPieriHomotopy(Sequence,List,List) := (kn,l,m)->(
     -- k and n are the integers defining the Grassmanian G(k,n)
     -- l and m are partitions of n
     (k,n) := kn;
     if #l < k then x := for i to k-#l-1 list 0;
     l = l | x; -- makes sure l have size k
     if #m < k then x := for i to k-#l-1 list 0;
     m = m | x; -- makes sure m have size k
     E := skewSchubertVariety(kn,l,m);
     ------------
     -- d is the number of variables i.e. the codimension of the Schubert variety E_{l,m}
     ------------
     d := (k*(n-k)-sum(l)-sum(m));
     S := CC[vars(53..d+52)];
     T:= apply(#m, i->n-k+i-m#i);
     -- P is a list with the indeces where the special flag has 1's
     P:=toList(set toList(0..n-1)-T);
     G:=mutableMatrix(S,n-k,n);
     apply(#P, j->G_(j,P#j)=1);
     F=matrix E || sub(matrix G, ring E);
     return F;
)

--------------------------
-- Given the partitions l, and m for the Grassmannian Gr(k,n)
-- it creates a flags as random numeric matrices G_1,...,G_m
-- for solving the system defined by these random matrices
-- using Homotopy Continuation
--------------------------
createRandomFlagsForSimpleSchubert = method()
createRandomFlagsForSimpleSchubert(Sequence, List, List) := (kn,l,m)->(
   (k,n) := kn;
   d := k*(n-k)-sum(l)-sum(m);
   H = new MutableHashTable from {};
   apply(d, i->matrix apply(n-k,i->apply(n,j->random CC)))
   )

     
solveSimpleSchubert = method(TypicalValue=>List)
solveSimpleSchubert(Sequence,List,List,List) := (kn,l,m,G)->(
   -- l and m are partitions of n
   (k,n) := kn;
   d := k*(n-k)-sum(l)-sum(m);
   E := skewSchubertVariety(kn,l,m);
   if H#?(l,m) then(
   H # (l,m)
   )
   else if d == 1 then (
      -- solve linear equation
      H#(l,m) = solveEasy det (matrix E || sub(G#0, ring E), Strategy=>Cofactor)
   )
   else(
      -- generate the children problems
      L:=generateChildren(kn, l,m);
      
      -- once the children problems are solved
      -- store solutions in "start" 
      start := flatten apply(L, p->(
         C := solveSimpleSchubert(kn,l,p,G);
         i := positionVariableChildren((k,n),l,m,p);
         apply(C,c->insert(i-1,0,c))
         )
      );
      ---- Create the start system S  ----
      S := apply(take(G,d-1), g->det( matrix E || sub(g, ring E),Strategy=>Cofactor)) | {det(precookPieriHomotopy(kn,l,m), Strategy=>Cofactor)};
      ---- Create the target system T ----
      T := apply(take(G,d), g->det( matrix E || sub(g, ring E), Strategy=>Cofactor)); 
      newR := CC_53(monoid[gens ring first S]);
      S = S/(s->sub(s,newR));
      T = T/(t->sub(t,newR));
      ------------------------
      -- make sure your starting set of solutions are in fact solutions
      -- of the Starting system S
      ------------------------
      assert all(start, s->norm sub(matrix{S},matrix{s}) < 1e-3);
      H#(l,m) = track(S,T,start,gamma=>exp(2*pi*ii*random RR) 
      --,Software=>Bertini
      --,Software=>PHCpack
      ) / first;
      ---------------------
      ---- make sure that you got solutions of the Target System --
      ---------------------
      assert all(H#(l,m), s->norm sub(matrix{T},matrix{s}) < 1e-3);
      H#(l,m)
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


-------------------
-- Documentation --
-------------------

beginDocumentation()

doc ///
   Key
      skewSchubertVariety
   Headline
      skew Schubert variety (or Richardson variety) from partitions l and m
   Usage
      skewSchubertVariety(kn,l,m)
   Inputs
      kn:Sequence
         two integers denoting the Grassmannian Gr(k,n)
      l:List
      m:List
         partitions of n
   Outputs
      :Matrix
   Consequences
      Matrix with some indeterminate entries that parametrizes the skew Schubert variety 
   Description
      Text
         Creates the matrix $E_{l,m}$ that parametrizes the skew Schubert variety $Y_{l,m} = Y_l \cap Y_m$.
     Example
       -- for l = 2,1 and m = 1,1
       -- in Gr(3,7)
       skewSchubertVariety((3,7),{2,1},{1,1})
   SeeAlso
      solveSimpleSchubert
///

doc ///
   Key
      solveSimpleSchubert
   Headline
      Uses Pieri Homotopy continuation to solve simple Schubert problems
   Usage
      solveSimpleSchubert(kn,l,m,G)
   Inputs
      kn:Sequence
         two integers denoting the Grassmannian Gr(k,n)
      l:List
      m:List
         partitions of n
      G:List
         of fixed Flags G_1,...,G_d
   Outputs
      :List
         solutions of the simple Schubert Problem defined by l and m with respect to the flags G_1,...,G_d
   Description
      Text
         Given partitions l and m in the Grassmannian Gr(k,n), and a set of fixed flags G_1,...,G_d, where d=k*(k-n) - sum(l) - sum(m). The function solves the system taking the first $d-1$ flags, and replacing the last one for a simpler one G_m. Then it uses homotopy continuation to track the solutions of this simpler system to solutions of the original system.
         
         This function is used to solve Simple Schubert Problems, as described in the paper: 
         
         Leykin and Sottile, "Galois groups of Schubert problems via homotopy continuation", Mathematics of Computation, 78 (2009) 1749--1765.
     Example
       ---- Simple Schubert Problem
       (k,n) = (3,7)
       l = {2,1,0}
       m = {1,1,0}
       ----  Generate random flags G----
       d = k*(n-k)-sum(l)-sum(m);
       G = apply(d, i->matrix apply(n-k,i->apply(n,j->random CC)));
       ---------------------------------
       H = new MutableHashTable from {};
       solveSimpleSchubert((k,n),l,m)
   SeeAlso
       createRandomFlagsForSimpleSchubert, skewSchubertVariety
///

doc ///
   Key
      createRandomFlagsForSimpleSchubert
   Headline
      Create a list of flags with random numbers to solve a simple Schubert problem
   Usage
      createRandomFlagsForSimpleSchubert(kn,l,m)
   Inputs
      kn:Sequence
         two integers denoting the Grassmannian Gr(k,n)
      l:List
      m:List
         partitions of n
   Outputs
      :List
         random fixed flags
   Description
      Text
         Creates a list of d matrices with random numbers, where d = k*(n-k)-sum(m)-sum(l).
     Example
       -- for l = 2,1 and m = 1,1
       -- in Gr(3,7)
       createRandomFlagsForSimpleSchubert(3,7),{2,1,0},{1,1,0}
       G = oo
       skewSchubertVariety((3,7),{2,1},{1,1}, G)
   SeeAlso
      solveSimpleSchubert
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

