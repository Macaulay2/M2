-------------------
-- Package Header
-------------------
-- Copyright 2010 David W. Cook II
-- You may redistribute this file under the terms of the GNU General Public
-- License as published by the Free Software Foundation, either version 2
-- of the License, or any later version.

newPackage (
   "SimplicialDecomposability",
   Version => "1.0.6",
   Date => "20. June 2011",
   Authors => {{Name => "David Cook II",
                Email => "dwcook@eiu.edu",
                HomePage => "http://ux1.eiu.edu/~dwcook/"}},
   Headline => "various decomposability routines for simplicial complexes",
   Keywords => {"Combinatorial Commutative Algebra"},
   DebuggingMode => false,
   Certification => {
	"journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	"journal URI" => "http://j-sag.org/",
	"article title" => "Simplicial Decomposability",
	"acceptance date" => "2010-08-03",
	"published article URI" => "http://www.j-sag.org/Volume2/jsag-5-2010.pdf",
	"published code URI" => "http://www.j-sag.org/Volume2/SimplicialDecomposability.m2",
	"repository code URI" => "https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/SimplicialDecomposability.m2",
	"release at publication" => "d230ee4205bab933be32f3de7ae0ae0f52115c84",
	"version at publication" => "1.0.5",
	"volume number" => "2",
	"volume URI" => "http://www.j-sag.org/volume2.html"
	},
    PackageExports => {"SimplicialComplexes", "Posets"}
    )

-------------------
-- Exports
-------------------
export {      
   -- Methods
   "allFaces",
   "faceDelete",
   "fTriangle",
   "hTriangle",
   "hVector",
   "isDecomposable",
   "isSheddingFace",
   "isSheddingVertex",
   "isShellable",
   "isShelling",
   "isSimplex",
   "isVertexDecomposable",
   "shellingOrder",
   -- Options
   "Flag",
   "Permutation"
}

-------------------
-- Exported Code
-------------------

-- Returns all faces of a simplicial complex (except {}, the (-1)-face) up to a
-- given dimension
allFaces = method(TypicalValue => List)
allFaces SimplicialComplex := S -> (
   allFaces(dim S, S)
)
allFaces (ZZ, SimplicialComplex) := (k, S) -> (
   flatten for i from 0 to min(k, dim S) list faces(i, S)
)

-- Face Deletion: Remove all faces of a complex containing the given face.
faceDelete = method(TypicalValue => SimplicialComplex)
faceDelete (RingElement, SimplicialComplex) := (F, S) -> (
   simplicialComplex (monomialIdeal S + monomialIdeal F)
)

-- fTriangle is a generalisation of the fVector and is introduced
-- in Definition 3.1 in [BW-1].
fTriangle = method(TypicalValue => Tally)
fTriangle SimplicialComplex := S -> (
   -- (reverse) sort the facets by dimension
   F := rsort facets S;
   -- tally up (face degree, dimension) pairs and join the empty face)
   tally join({(dim S + 1, 0)}, for f in allFaces S list (faceDeg(f, F), first degree f))
)

-- hTriangle is a generalisation of the hVector and is introduced
-- in Definition 3.1 in [BW-1].
hTriangle = method(TypicalValue => Tally)
hTriangle SimplicialComplex := S -> (
   fT := fTriangle S;
   new Tally from new HashTable from flatten for i from 0 to dim S + 1 list for j from 0 to i list (
       si := sum(j+1, k -> (-1)^(j-k)*binomial(i-k,j-k)*(if fT#?(i,k) then fT#(i,k) else 0));
       if si != 0 then (i,j) => si else continue
   )
)


protect isVD

-- Determines whether or not a simplicial complex is k-decomposable, as
-- in Definition 3.6 in [Wo].
isDecomposable = method(TypicalValue => Boolean)
isDecomposable (ZZ, SimplicialComplex) := (k, S) -> (
   -- k must be nonnegative
   if k < 0 then return false;
   -- check the cache (vertex-decomposable => k-decomposable)
   if S.cache.?isVD and S.cache.isVD then return true;

   iskd := false;
   done := false;
   -- base case: simplices are k-decomposable for all nonnegative k
   if isSimplex S then ( iskd = done = true; );

   -- short-circuit via the h-Vector or h-Triangle, depending on purity
   if not done then (
       if isPure S then (
           if any(values hVector S, i -> i<0) then ( iskd = false; done = true; )
       )
       else (
           if any(values hTriangle S, i -> i < 0) then ( iskd = false; done = true; );
       );
   );

   -- Check for shedding faces which are also happy faces
   if not done then (
       iskd = any(allFaces(k, S), F ->
               isSheddingFace(F, S)
               and isDecomposable(k, link(S, F))
               and isDecomposable(k, faceDelete(F, S))
           );
   );

   -- update the cache (if necessary)
   -- 0-decomposable == vertex-decomposable, and a complex which is
   -- not k-decomposable is not j-decomposable for j <= k.
   if k == 0 or iskd == false then S.cache.isVD = iskd;
   iskd
)

-- Determines whether or not a face is a shedding face of a simplicial
-- complex, as introduced in Definition 3.1 in [Wo].
-- Uses Remark 3.2 in [Wo] for a simplification.
isSheddingFace = method(TypicalValue => Boolean)
isSheddingFace (RingElement, SimplicialComplex) := (F, S) -> (
   -- star is the simplicial complex generated by the faces containing F
   star := simplicialComplex (facets S)_(positions(first entries (matrix{facets S} % F), zero));
   DS := faceDelete(F, star);
   D := faceDelete(F, S);
   #((set facets DS) * (set facets D)) == 0
)

-- Determines whether or not a vertex is a shedding vertex of a simplicial
-- complex using Definition 11.1 in [BW-2].
isSheddingVertex = method(TypicalValue => Boolean)
isSheddingVertex (RingElement, SimplicialComplex) := (x, S) -> (
   L := link(S, x);
   D := faceDelete(x, S);
   #((set facets L) * (set facets D)) == 0 and isVertexDecomposable(D) and isVertexDecomposable(L)
)

-- Determines whether or not a simplicial complex is shellable by checking
-- for the existence of a shelling order.
isShellable = method(TypicalValue => Boolean)
isShellable SimplicialComplex := S -> (
   -- Vertex-decomposability implies shellability for the pure case (Theorem
   -- 2.8 in [PB]) and the non-pure case (Theorem 11.3 in [BW-2]).
   if S.cache.?isVD and S.cache.isVD then return S.cache.isVD;

   -- otherwise, look for a shelling order
   shellingOrder(S) != {}
)

-- Determines whether or not a list of faces is a shelling.
isShelling = method(TypicalValue => Boolean)
isShelling List := L -> (
   -- Check for squarefree monic monomials
   if any(L, f -> (size f != 1) or (max first exponents f > 1) or (leadCoefficient f != 1)) then return false;

   -- Sets with zero or one face are always shellings as they are simplices!
   if #L <= 1 then return true;

   -- Use Definition 2.1 in [BW-1] for non-pure shellability.
   if #unique apply(L, degree) > 1 then (
       -- Lemma 2.2 in [BW-1] shows dim L_0 == dim L, if L is a shelling
       if (max flatten apply(L, degree)) != first degree L_0 then return false;
       -- prime the loop
       S := fi := I := null;
       fa := set apply(drop(subsets support L_0, {0,0}), product);
       -- for each face in the list
       for i from 1 to #L - 1 do (
           -- get the next set of faces
           fi = set apply(drop(subsets support L_i, {0,0}), product);
           -- find the simplicial complex of the intersection
           I = toList(fa * fi);
           -- handle the empty intersection case separately
           if #I == 0 then (
               if first degree L_i != 1 then return false;
           )
           else (
               S = simplicialComplex I;
               -- check it is pure and properly dimensional
               if not isPure S or dim S != (first degree L_i - 2) then return false;
           );
           -- update the union new step
           fa = fa + fi;
       );
   )
   -- Use Definition III.2.1. in [St] for pure shellability.
   else (
       -- prime the loop
       f0 := T := null;
       f1 := set apply(subsets support L_0, product);
       -- for each face in the list
       for i from 1 to #L - 1 do (
           -- copy the last step
           f0 = f1;
           -- update with the new step
           f1 = f1 + set apply(subsets support L_i, product);
           -- make sure the minimal face is unique
           T = sort toList(f1 - f0);
           if any(drop(T, 1), m -> m // T_0 == 0) then return false;
       );
   );
   true
)

-- Determines whether or not a simplicial complex is a simplex.
isSimplex = method(TypicalValue => Boolean)
isSimplex SimplicialComplex := S -> (
   # facets S <= 1
)

-- Determines whether or not a simplicial complex is vertex (0-) decomposable.
-- Uses Definition 2.1 in [PB] for pure complexes.
-- Uses Definition 11.1 in [BW-2] for non-pure complexes.
isVertexDecomposable = method(TypicalValue => Boolean)
isVertexDecomposable SimplicialComplex := (cacheValue isVD) (S -> (
   -- base case: simplices are vertex-decomposable
   if isSimplex S then return true;

   -- pure case: vertex-decomposable => shellable => nonnegative h-Vector
   if isPure S then (
       if any(values hVector S, i -> i < 0) then return false;
   )
   -- non-pure case: Theorem 11.3 in [BW-2] shows vertex-decomposable implies
   -- shellable, hence nonnegative h-Triangle by Theorem 3.4 in [BW-1].
   else if any(values hTriangle S, i -> i < 0) then return false;

   -- Check for shedding vertices
   any(faces(0, S), x -> isSheddingVertex(x, S))
))

-- Attempts to find a shelling order of a simplicial complex.
shellingOrder = method(TypicalValue => List, Options => {Random => false, Permutation => {}})
shellingOrder SimplicialComplex := opts -> S -> (
   -- check the cache, if not trying to do it in an alternate manner
   if opts.Random == false and #opts.Permutation == 0 and S.cache.?shellingOrder then return S.cache.shellingOrder;
   if opts.Random == true and #opts.Permutation > 0 then error("shellingOrder: Options Random and Permutation are mutually exclusive.");

   -- Build the facet list, applying options if necessary.
   F := facets S;
   if #opts.Permutation > 0 then (
       tmp := sort unique opts.Permutation;
       if tmp != toList(0..#F-1) then error("shellingOrder: Option Permutation must be the same length as the number of facets and must be increasing consecutive integers.");
       F = F_(opts.Permutation);
   )
   else if opts.Random then F = random F;

   O := {};
   -- The pure case is easier, so separate it
   if isPure S then (
       -- Negatives in the h-Vector imply not shellable for pure complexes
       if any(values hVector S, i -> i < 0) then return {};
       -- Start the recursion
       O = recursivePureShell({}, F);
   )
   else (
       -- Negatives in the h-Triangle imply not shellable for all complexes,
       -- use Theorem 3.4 in [BW-1].
       if any(values hTriangle S, i -> i < 0) then return {};
       -- Lemma 2.6 in [BW-1] shows that if S is shellable, then
       -- there is a shelling with the facets in dimension order.
       -- Note: This sort preserves the order on F within a given dimension.
       F = F_(apply(sort apply(#F, i -> (-first degree F_i, i)), last));
       -- Start the recursion
       O = recursiveNonpureShell({}, rsort facets S);
   );

   -- cache & return
   S.cache.shellingOrder = O
)

-- Check if the grading on the ring defines a proper (dim(D)+1)-coloring on D. Used by fVector and hVector. Not exported.

isBalanced = (D) -> (
     d := dim D +1;
     m := true;
     if not d == #(degree first gens ring D) then (
         m = false;
     );
     apply(faces(1,D), f -> if max(degree f) > 1 then m = false);
     return m;
     );

-------------------
-- 27/12/2018 Lorenzo: some changes
-------------------

-- Modified hVector with the Flag option
hVector = method(TypicalValue => List, Options => {Flag => false})
hVector SimplicialComplex := opts -> D -> (
     I := ideal D;
     if not opts.Flag then (
         S := newRing(ring D, Degrees => {#(gens ring D):1});
         maptoS := map(S, ring D);
         I = maptoS(I);
     );
     N := poincare cokernel generators I;
     if opts.Flag then (
     if not isBalanced(D) then (
         stderr << "-- the grading does not correspond to a proper d-coloring." << endl;
         return new HashTable from {}
     );
         R := newRing(ring N, Degrees => apply(gens ring N, g -> apply(gens ring N, f -> if index(f) == index(g) then 1 else 0)));
         maptoR := map(R, ring N);
         N = maptoR(N);
     );
     if N == 0 then (
         new HashTable from {-1 => 0}
     )
     else (
         d := dim D + 1;
         apply(gens ring N, t -> while 0 == substitute(N, t => 1) do N = N // (1-t));
         supp := apply(flatten entries monomials(N), m -> degree m);
         allsubsets := apply(subsets(#(gens ring N)), s -> apply(toList(0..#(gens ring N)-1), l -> if member(l,s) then 1 else 0));
         flagh := L -> coefficient((flatten entries monomials part(L, N))#0, part(L, N));
     flagf := M -> sum(supp, m -> if all(m,M, (i,j) -> j >= i) then flagh(m) else 0);
     h := j -> sum(supp, s -> if sum(s)==j then flagh(s) else 0);
     f := j -> sum(0..j+1, i -> binomial(d-i, d-j-1)*h(i));
     if opts.Flag then (
         new HashTable from apply(supp, j -> j => flagh(j))
     )
     else new HashTable from apply(toList(0..d), j -> j => h(j))
     )
     )


-------------------
-- Local-Only Code
-------------------

-- Returns the cardinality of the largest face containing in the list F containing f.
-- The code assumes F is sorted in reverse order by dimension.
faceDeg = method(TypicalValue => ZZ)
faceDeg (RingElement, List) := (f, F) -> (
   s := support f;
   for i from 0 to #F-1 do if isSubset(s, support F_i) then return(first degree F_i);
   -- If it's not contained in any face, then give a reasonable number.
   infinity
)

-- Build up an non-pure shelling recursively.
-- Uses Definition 2.1 in [BW-1].
-- !! Assumes P is reverse sorted by dimension.
recursiveNonpureShell = method(TypicalValue => List)
recursiveNonpureShell (List, List) := (O, P) -> (
   -- if it's "obvious", then keep going
   OisShelling := true;
   if #O > 1 then (
       -- the previous step is a shelling, but is the newest step?
       fa := set allFaces simplicialComplex drop(O, -1);
       Oi := O_-1;
       fi := set apply(subsets support Oi, product);
       I := toList(fa * fi);
       -- handle the empty intersection case separately
       if #I == 0 then (
           OisShelling = (first degree Oi == 1);
       )
       else (
           S := simplicialComplex toList(fa * fi);
           -- check it is pure and properly dimensional
           OisShelling = (isPure S and dim S == (first degree Oi - 2));
       );
   );
   if OisShelling then (
       -- Nothing else to add: we're done
       if P == {} then return O;
       -- Recurse until success, if possible
       Q := {};
       d := degree P_0;
       for i from 0 to #P - 1 do (
           -- if the dimension of the face drops, then we can bail; see
           -- Lemma 2.6 in [BW-1],
           if degree P_i != d then return {};
           Q = recursiveNonpureShell(append(O, P_i), drop(P, {i,i}));
           if Q != {} then return Q;
       );
   );
   {}
)

-- Build up a pure shelling recursively.
-- Uses Definition III.2.1 in [St].
recursivePureShell = method(TypicalValue => List)
recursivePureShell (List, List) := (O, P) -> (
   -- if it's "obvious", then keep going
   OisShelling := true;
   if #O > 1 then (
       -- the previous step is a shelling, but is the newest step?
       f0 := set allFaces simplicialComplex drop(O, -1) + set {1};
       f1 := f0 + set apply(subsets support last O, product);
       T := sort toList(f1 - f0);
       OisShelling = all(drop(T, 1), m -> m // T_0 != 0);
   );
   if OisShelling then (
       -- Nothing else to add: we're done
       if P == {} then return O;
       -- Recurse until success, if possible
       Q := {};
       for i from 0 to #P - 1 do (
           Q = recursivePureShell(append(O, P_i), drop(P, {i,i}));
           if Q != {} then return Q;
       );
   );
   {}
)


-------------------
-- Documentation
-------------------
beginDocumentation()

doc ///
   Key
       SimplicialDecomposability
   Headline
       various decomposability routines for simplicial complexes.
   Description
       Text
           This package includes routines for vertex decomposability and
           shellability for arbitrary simplicial complexes as well as routines
           for k-decomposability.  Moreover, it can find a shelling order for
           a shellable simplicial complex.

           References:

           [BW-1] A. Bjoerner and M. Wachs, "Shellable nonpure complexes and
           posets, I," Trans. of the AMS 348 (1996), 1299--1327.

           [BW-2] A. Bjoerner and M. Wachs, "Shellable nonpure complexes and
           posets, II," Trans. of the AMS 349 (1997), 3945--3975.

           [MT] S. Moriyama and F. Takeuchi, "Incremental construction
           properties in dimension two: shellability, extendable shellability
           and vertex decomposability," Discrete Math. 263 (2003), 295--296.

           [PB] J. S. Provan and L. J. Billera, "Decompositions of Simplicial
           Complexes Related to Diameters of Convex Polyhedra," Math. of
           Operations Research 5 (1980), 576--594.

           [St] R. Stanley, "Combinatorics and Commutative Algebra," 2nd
           edition.  Progress in Mathematics, 41. Birkhaeuser Boston, Inc.
           Boston, MA, 1996.

           [Wo] R. Woodroofe, "Chordal and sequentially Cohen-Macaulay clutters,"
           arXiv:0911.4697v1.
///

doc ///
   Key
       allFaces
       (allFaces, SimplicialComplex)
       (allFaces, ZZ, SimplicialComplex)
   Headline
       returns all faces of a simplicial complex, up to a given dimension
   Usage
       allFaces S
       allFaces(k, S)
   Inputs
       k:ZZ
           the highest dimension to return ($dim S$ by default)
       S:SimplicialComplex
   Outputs
       L:List
           the list of all faces of $S$ (excluding the $(-1)$-dimensional face \{\})
           up to dimension $k$
   Description
       Example
           R = QQ[a..e];
           S = simplicialComplex {a*b*c*d*e};
           allFaces S
           allFaces(2, S)
   SeeAlso
       faces
       facets
///

doc ///
   Key
       faceDelete
       (faceDelete, RingElement, SimplicialComplex)
   Headline
       computes the face deletion for a simplicial complex
   Usage
       faceDelete(F, S)
   Inputs
       F:RingElement
           a face of $S$
       S:SimplicialComplex
   Outputs
       T:SimplicialComplex
           the simplicial complex of all faces in $S$ not containing the
           face $F$
   Description
       Example
           R = QQ[a..e];
           S = simplicialComplex {a*b*c*d*e};
           faceDelete(a, S)
           faceDelete(a*b*c, S)
           faceDelete(a*b*c*d*e, S) === skeleton(3, S)
   Caveat
       Do not confuse face deletion with normal deletion wherein the vertices
       of the given face are entirely removed.
   SeeAlso
       link
///

doc ///
   Key
       fTriangle
       (fTriangle, SimplicialComplex)
   Headline
       determines the f-Triangle of a simplicial complex
   Usage
       fTriangle S
   Inputs
       S:SimplicialComplex
   Outputs
       f:Tally
           the f-Triangle of $S$
   Description
       Text
           Definition 3.1 in [BW-1] defines the f-Triangle, a generalisation
           of the f-Vector, to have entries $f#(i,j)$ equal to the number of
           faces of $S$ with degree $i$ and dimension $j-1$.
           The degree of a face is the dimension of the largest face of $S$
           containing it, plus one.

           If $S$ is pure, then the last row is the traditional f-Vector
           and the remainder is zeros.
       Example
           R = QQ[a..e];
           fTriangle simplicialComplex {a*b*c, c*d*e, a*d, a*e, b*d, b*e}
           fTriangle simplicialComplex {a*b*c*d*e}
   SeeAlso
       fVector
       hTriangle
///

doc ///
   Key
       hTriangle
       (hTriangle, SimplicialComplex)
   Headline
       determines the h-Triangle of a simplicial complex
   Usage
       hTriangle S
   Inputs
       S:SimplicialComplex
   Outputs
       h:Tally
           the h-Triangle of $S$
   Description
       Text
           Definition 3.1 in [BW-1] defines the h-Triangle, a generalisation
           of the h-Vector. 

           If $S$ is pure, then the last row is the traditional h-Vector
           and the remainder is zeros.
       Example
           R = QQ[a..e];
           hTriangle simplicialComplex {a*b*c, c*d*e, a*d, a*e, b*d, b*e}
           hTriangle simplicialComplex {a*b*c*d*e}
   SeeAlso
       fTriangle
       hVector
///


doc ///
   Key
       isDecomposable
       (isDecomposable, ZZ, SimplicialComplex)
   Headline
       determines whether a simplicial complex is k-decomposable
   Usage
       isDecomposable(k, S)
   Inputs
       k:ZZ
       S:SimplicialComplex
   Outputs
       B:Boolean
           true if and only if $S$ is $k$-decomposable
   Description
       Text
           Definition 3.6 of [Wo] states that a simplicial complex $S$
           is $k$-decomposable if $S$ is either a simplex or there
           exists a shedding face $F$ of $S$ of dimension at most
           $k$ such that both the face deletion and link of $S$ by
           $F$ are again $k$-decomposable.
       Example
           R = QQ[a..f];
           isDecomposable(0, simplicialComplex {a*b*c*d*e*f})
           isDecomposable(2, simplicialComplex {a*b*c, b*c*d, c*d*e})
       Text
           The method checks the cache, if possible, to see if the complex is
           vertex-decomposable.
   SeeAlso
       faceDelete
       isSheddingFace
       isShellable
       isVertexDecomposable
       link
///

doc ///
   Key
       isSheddingFace
       (isSheddingFace, RingElement, SimplicialComplex)
   Headline
       determines whether a face of a simplicial complex is a shedding face
   Usage
       isSheddingFace(F, S)
   Inputs
       F:RingElement
           a face of $S$
       S:SimplicialComplex
   Outputs
       B:Boolean
           true if and only if $F$ is a shedding face of $S$
   Description
       Text
           Definition 3.1 in [Wo] states that a face $F$ of a simplicial
           complex $S$ is a shedding face if every face $G$ of the star
           of $S$ by $F$ satisfies the exchange property, that is,
           for every vertex $v$ of $F$ there is a vertex $w$ of
           the face deletion of $S$ by $G$ such that
           $(G \cup w) \setminus v$ is a face of $S$.
       Example
           R = QQ[a..e];
           T = simplicialComplex {a*b*c, b*c*d, c*d*e};
           isSheddingFace(b*d, T)
           isSheddingFace(b*c*d, T)
   SeeAlso
       faceDelete
       isDecomposable
       isShellable
       isVertexDecomposable
       link
///

doc ///
   Key
       isSheddingVertex
       (isSheddingVertex, RingElement, SimplicialComplex)
   Headline
       determines whether a vertex of a simplicial complex is a shedding vertex
   Usage
       isSheddingVertex(x, S)
   Inputs
       x:RingElement
           a vertex of $S$
       S:SimplicialComplex
   Outputs
       B:Boolean
           true if and only if $x$ is a shedding vertex of $S$
   Description
       Text
           Definition 11.1 of [BW-2] states that a shedding vertex $x$
           of a simplicial complex $S$ is a vertex such that the link and
           face deletion of $x$ from $S$ are vertex decomposable and
           share no common facets.
       Example
           R = QQ[a..f];
           S = simplicialComplex {a*b*c, c*d, d*e, e*f, d*f};
           isSheddingVertex(a, S)
           isSheddingVertex(f, S)
   SeeAlso
       faceDelete
       isVertexDecomposable
       link
///

doc ///
   Key
       isShellable
       (isShellable, SimplicialComplex)
   Headline
       determines whether a simplicial complex is shellable
   Usage
       isShellable S
   Inputs
       S:SimplicialComplex
   Outputs
       B:Boolean
           true if and only if $S$ is shellable
   Description
       Text
           The pure and non-pure cases are handled separately.  If $S$ is
           pure, then definition III.2.1 in [St] is used.  That is, $S$ is
           shellable if its facets can be ordered $F_1, ..., F_n$ so that
           the difference in the $j$-th and $j-1$-th subcomplex has a 
           unique minimal face, for $2 \leq j \leq n$.

           If $S$ is non-pure, then definition 2.1 in [BW-1] is used.  That is,
           a simplicial complex $S$ is shellable if the facets of $S$
           can be ordered $F_1, .., F_n$ such that the intersection of the
           faces of the first $j-1$ with the faces of the $F_j$ is
           pure and $dim F_j - 1$-dimensional.
       Example
           R = QQ[a..f];
           isShellable simplicialComplex {a*b*c*d*e}
           isShellable simplicialComplex {a*b*c, c*d*e}
           isShellable simplicialComplex {a*b*c, b*c*d, c*d*e}
           isShellable simplicialComplex {a*b*c, c*d, d*e, e*f, d*f}
           isShellable simplicialComplex {a*b*c, c*d, d*e*f}
   SeeAlso
       facets
       isDecomposable
       isShelling
       shellingOrder
///

doc ///
   Key
       isShelling
       (isShelling, List)
   Headline
       determines whether a list of faces is a shelling
   Usage
       isShelling L
   Inputs
       L:List
           a list of faces (i.e., squarefree monic monomials)
   Outputs
       B:Boolean
           true if and only if $L$ is shelling
   Description
       Text
           Determines if a list of faces is a shelling order of the
           simplicial complex generated by the list.

           Let $S$ be the simplicial complex generated by the list of facets
           $L$.  If $S$ is pure, then definition III.2.1 in [St] is used.
           That is, $L_1, .., L_n$ is a shelling order of $S$ if 
           the difference in the $j$-th and $j-1$-th subcomplex has a 
           unique minimal face, for $2 \leq j \leq n$.

           If $S$ is non-pure, then definition 2.1 in [BW-1] is used.  That is,
           $L_1, .., L_n$ is a shelling order if the intersection of the
           faces of the first $j-1$ facets with the faces of the $L_j$ is
           pure and $dim L_j - 1$-dimensional.
       Example
           R = QQ[a..e];
           isShelling {a*b*c, b*c*d, c*d*e}
           isShelling {a*b*c, c*d*e, b*c*d}
   SeeAlso
       isShellable
       shellingOrder
///

doc ///
   Key
       isSimplex
       (isSimplex, SimplicialComplex)
   Headline
       determines whether a simplicial complex is simplex
   Usage
       isSimplex S
   Inputs
       S:SimplicialComplex
   Outputs
       B:Boolean
           true if and only if $S$ is simplex
   Description
       Example
           R = QQ[a..d];
           isSimplex simplicialComplex {a*b*c*d}
           isSimplex simplicialComplex {a*b}
           isSimplex simplicialComplex {a*b, c*d}
   SeeAlso
       facets
///

doc ///
   Key
       isVertexDecomposable
       (isVertexDecomposable, SimplicialComplex)
   Headline
       determines whether a simplicial complex is vertex-decomposable
   Usage
       isVertexDecomposable S
   Inputs
       S:SimplicialComplex
   Outputs
       B:Boolean
           true if and only if $S$ is vertex-decomposable
   Description
       Text
           Vertex-decomposability is just zero-decomposability when $S$ is
           pure, see [PB].  When $S$ is non-pure, [BW-2] gives a generalisation:
           A complex $S$ is vertex decomposable if it is either a simplex or
           there exists a shedding vertex.
       Example
           R = QQ[a..f];
           isVertexDecomposable simplicialComplex {a*b*c*d*e}
           isVertexDecomposable skeleton(3, simplicialComplex {a*b*c*d*e})
           isVertexDecomposable simplicialComplex {a*b*c, c*d*e}
           isVertexDecomposable simplicialComplex {a*b*c, c*d, d*e, e*f, d*f}
       Text
           Whether the complex is vertex-decomposable is cached.
   SeeAlso
       isDecomposable
       isSheddingVertex
///

--The following documentation node is in a different documentation format
--to allow the nicer optional inputs styling.
document {
   Key => {
       shellingOrder,
       (shellingOrder, SimplicialComplex),
       [shellingOrder, Permutation],
       [shellingOrder, Random]
   },
   Headline => "finds a shelling of a simplicial complex, if one exists",
   Usage => "L = shellingOrder S",
   Inputs => {
        "S" => SimplicialComplex,
        Permutation => List => { "of integers from", TEX /// $0$ ///, "to one less than the number of facets which is applied to the facets before the recursive search for a shelling is executed."},
        Random => Boolean => "whether to use a random permutation to the facet list before the recursive search for a shelling is executed."
   },
   Outputs => { "L" => List => {"a shelling order of the facets of", TEX /// $S$///, ", if one exists"} },
   PARA { TEX ///If $S$ is pure, then definition III.2.1 in [St] is used.  That is,
           $S$ is shellable if its facets can be ordered $F_1, .., F_n$ so that
           the difference in the $j$-th and $j-1$-th subcomplex has a 
           unique minimal face, for $2 \leq j \leq n$. /// },
   PARA { TEX ///If $S$ is non-pure, then definition 2.1 in [BW-1] is used.  That is,
           a simplicial complex $S$ is shellable if the facets of $S$
           can be ordered $F_1, .., F_n$ such that the intersection of the
           faces of the first $j-1$ with the faces of the $F_j$ is
           pure and $dim F_j - 1$-dimensional./// },
   PARA { TEX ///This function attempts to build up a shelling order of $S$
           recursively.  In particular, a depth-first search is used to
           attempt to build up a shelling order from the bottom, that is,
           from the first facet in the order.///},
   PARA { TEX ///In the case when $S$ is non-pure, then the search is restricted
           to the maximal dimension facets remaining to be added.  This allows
           a shelling order in reverse dimension order to be returned whenever
           $S$ is indeed shellable.///},
   EXAMPLE lines ///
           R = QQ[a..f];
           shellingOrder simplicialComplex {a*b*c*d*e}
           shellingOrder simplicialComplex {a*b*c, b*c*d, c*d*e}
           shellingOrder simplicialComplex {a*b*c, c*d*e}
           shellingOrder simplicialComplex {a*b*c, c*d, d*e, e*f, d*f}
           shellingOrder simplicialComplex {a*b*c, c*d, d*e*f}
       ///
   ,
   PARA { "The options Random and Permutation can be used to try to find
           alternate shelling orders.  Random applies a random permutation
           to the facet list and Permutation applies a supplied permutation
           to the list.  In the non-pure case, the facets are subsequently
           ordered in reverse dimension order but retaining the ordering within
           dimensions."},
   PARA { "The options Random and Permutation are mutually exclusive."},
   EXAMPLE lines ///
           S = simplicialComplex {a*b*c, b*c*d, c*d*e, d*e*f};
           shellingOrder S
           shellingOrder(S, Random => true)
           shellingOrder(S, Permutation => {3,2,1,0})
       ///,
   PARA { "The shelling order is cached if it exists, however, if either option
           is used, then the cache is ignored."},
   SeeAlso => {
       "isShellable",
       "isShelling"
   }
}

--These are documented in the above node.
undocumented { "Random", "Permutation" }

document {
     Key => {hVector,(hVector,SimplicialComplex),[hVector,Flag]},
     Headline => "the h-vector of a simplicial complex",
     Usage => "h = hVector D",
     Inputs => {
     "D" => SimplicialComplex,
     Flag => Boolean => "the flag h-vector if the simplicial complex is
     properly defined over a multigraded ring."
     },
     Outputs => {
     "h" => {"such that ", TT "h#i",
     " is the ", TT "i-", "th entry of the h-vector of ", TT "D",
     " for an integer ", TT "0 <= i <= dim D+1", " or a squarefree degree ", TT "i", "."}
     },
     "The h-vector of the 4-simplex.",
     EXAMPLE {
     "R = ZZ[a..e];",
     "smplex = simplicialComplex{a*b*c*d*e}",
     "hVector smplex",
     },
     "A filled triangle with two edges attached to two vertices shows
     that the h-vector can have negative entries.",
     EXAMPLE {
     "R = ZZ[x_1..x_5];",
     "delta = simplicialComplex{x_1*x_2*x_3,x_2*x_4,x_3*x_5}",
     "hVector delta",
     },
     "The last example above can be considered in a ", TT "Z^3", "-graded
      ring. Then we can compute its flag h-vector.",
     EXAMPLE {
     "grading = {{1,0,0},{1,0,0},{1,0,0},{0,1,0},{0,0,1}};",
     "R = ZZ[x_1,x_2,x_3,y,z, Degrees => grading];",
     "gamma = simplicialComplex{x_1*y*z,x_2*y,x_3*z}",
     "hVector(gamma, Flag => true)",
     },
     Caveat => {
     "The option ", TT "Flag", " checks if the multigrading corresponds to a properly d-coloring of "
     , TT "D", ", where d is the dimension of ", TT "D", " plus one. If it is not the case the output
     is an empty HashTable."
     },
     SeeAlso => {SimplicialComplexes,
     faces}
     }

--These are documented in the above node.
undocumented { "Flag" }

-------------------
-- Tests
-------------------

-- Tests allFaces
TEST ///
R = QQ[a,b,c];
assert(allFaces simplicialComplex {a*b*c} === {a, b, c, a*b, a*c, b*c, a*b*c});
assert(allFaces simplicialComplex {a*b} === {a, b, a*b});
assert(allFaces simplicialComplex {a, b*c} === {a, b, c, b*c});
assert(allFaces(1, simplicialComplex {a*b*c}) === {a, b, c, a*b, a*c, b*c});
///

-- Tests of faceDelete
TEST ///
R = QQ[a..e];
S = simplicialComplex {a*b*c*d*e};
assert(faceDelete(a, S) === simplicialComplex {b*c*d*e});
assert(faceDelete(a*b*c, S) === simplicialComplex {b*c*d*e, a*c*d*e, a*b*d*e});
assert(faceDelete(a*b*c*d*e, S) === skeleton(3, S))
///

-- Tests of fTriangle
TEST ///
R = QQ[a..e];
-- pure complex
S = simplicialComplex {a*b*c*d*e};
fT = fTriangle S;
-- pure complexes should have all zeros
assert(not any(5, i -> any(i+1, j -> fT#?(i,j))));
-- except in the last row, where the should be the traditional fVector;
fV = fVector S;
assert(not any(6, i -> fV#i != fT#(5, i)));
-- non-pure complex: see Example 3.2 in [BW-1].
S = simplicialComplex {a*b*c, c*d*e, a*d, a*e, b*d, b*e};
fT = fTriangle S;
assert(not fT#?(0,0));
assert(not fT#?(1,0) and not fT#?(1,1));
assert(not fT#?(2,0) and not fT#?(2,1) and fT#(2,2) == 4);
assert(fT#(3,0) == 1 and fT#(3,1) == 5 and fT#(3,2) == 6 and fT#(3,3) == 2);
///

-- Tests of hTriangle
TEST ///
R = QQ[a..e];
-- pure complex
S = simplicialComplex {a*b*c*d*e};
hT = hTriangle S;
-- pure complexes should have all zeros 
assert(not any(5, i -> any(i+1, j -> hT#?(i,j))));
-- except in the last row, where the should be the traditional hVector;
assert(hT#(5,0) == 1 and all(5, i -> not hT#?(5,i+1)));
-- non-pure complex: see Example 3.2 in [BW-1].
S = simplicialComplex {a*b*c, c*d*e, a*d, a*e, b*d, b*e};
hT = hTriangle S;
assert(not hT#?(0,0)); 
assert(not hT#?(1,0) and not hT#?(1,1));
assert(not hT#?(2,0) and not hT#?(2,1) and hT#(2,2) == 4);
assert(hT#(3,0) == 1 and hT#(3,1) == 2 and hT#(3,2) == -1 and not hT#?(3,3));
///

-- Tests of hVector
TEST ///
R = QQ[a..e];
assert(values hVector simplicialComplex {a, b, c, d, e} === {1,4});
assert(values hVector simplicialComplex {a*b*c*d*e} === {1,0,0,0,0,0});
assert(values hVector simplicialComplex {a*b*c, b*c*d, c*d*e} === {1,2,0,0});
assert(values hVector simplicialComplex {a*b, b*c, c*d, d*e, b*d} === {1,3,1});
assert(values hVector simplicialComplex {a*b*c, c*d*e} === {1, 2, -1, 0});
assert(values hVector simplicialComplex {a, b*c, d*e} === {1, 3, -2});
///

-- Tests of isDecomposable
TEST ///
R = QQ[a..e];
S = simplicialComplex {a*b*c*d*e};
assert(isDecomposable(0, S));
assert(isDecomposable(0, skeleton(3,S))); -- prop 2.2 in [PB]
assert(isDecomposable(2, simplicialComplex {a*b*c, b*c*d, c*d*e}));
assert(not isDecomposable(2, simplicialComplex {a*b*c, c*d*e}));
///

-- Tests isDecomposable: see Examples V6F10-{1,6,7} in [MT].
TEST ///
R = QQ[a..f];
S1 = simplicialComplex {a*b*c, a*b*d, a*b*f, a*c*d, a*c*e, b*d*e, b*e*f, c*d*f, c*e*f, d*e*f};
S6 = simplicialComplex {a*b*c, a*b*d, a*b*e, a*c*d, a*c*f, b*d*e, b*e*f, c*d*f, c*e*f, d*e*f};
S7 = simplicialComplex {a*b*c, a*b*e, a*b*f, a*c*d, a*d*e, b*c*d, b*e*f, c*d*f, c*e*f, d*e*f};
assert(not isVertexDecomposable(S1));
assert(isDecomposable(1, S1));
assert(not isVertexDecomposable(S6));
assert(isDecomposable(1, S6));
assert(not isVertexDecomposable(S7));
assert(isDecomposable(1, S7));
///

-- Tests isSheddingFace
TEST ///
R = QQ[a..e];
S = skeleton(3, simplicialComplex {a*b*c*d*e});
assert(all(allFaces S, F -> isSheddingFace(F, S)));
T = simplicialComplex {a*b*c, b*c*d, c*d*e};
assert(isSheddingFace(a, T));
assert(isSheddingFace(e, T));
assert(isSheddingFace(b*d, T));
assert(not isSheddingFace(b, T));
assert(not isSheddingFace(c, T));
assert(not isSheddingFace(d, T));
assert(not isSheddingFace(b*c, T));
assert(not isSheddingFace(b*c*d, T));
///

-- Tests isSheddingVertex
TEST ///
R = QQ[a..f];
S = simplicialComplex {a*b*c, c*d, d*e, e*f, d*f};
assert(not isSheddingVertex(a, S));
assert(isSheddingVertex(f, S));
///

-- Tests of isShellable (and hence shellingOrder by invocation)
-- NB: shellingOrder can only be tested this way as a shelling order need not be unique.
TEST ///
R = QQ[a..f];
-- Extreme cases
assert(isShellable simplicialComplex {a*b*c*d*e});
assert(isShellable simplicialComplex monomialIdeal {a,b,c,d,e}); -- empty complex
-- The following are from [St], Example 2.2.
assert(isShellable simplicialComplex {a*b*c, b*c*d, c*d*e});
assert(not isShellable simplicialComplex {a*b*c, c*d*e});
-- The following are from [BW-1], Figure 1.
assert(isShellable simplicialComplex {a*b, c});
assert(not isShellable simplicialComplex {a*b, c*d});
assert(isShellable simplicialComplex {a*b*c, c*d, d*e, e*f, d*f});
assert(not isShellable simplicialComplex {a*b*c, c*d, d*e*f});
-- The following tests for the fix of a bug in version 1.0.5 found by Sam Kolins and
-- Gwyn Whieldon.  In particular, RP2 is not shellable, though removing a face should be.
RP2 = {a*b*d,a*c*d,a*c*f,a*b*e,a*e*f,b*c*f,b*d*f,b*c*e,c*d*e,d*e*f};
assert(not isShellable simplicialComplex RP2);
assert(not isShellable simplicialComplex drop(RP2, 1));
///

-- Tests of isShelling
TEST ///
R = QQ[a..f];
assert(isShelling {a*b*c*d*e*f});
-- The following are from [St], Example 2.2.
assert(isShelling {a*b*c, b*c*d, c*d*e});
assert(not isShelling {a*b*c, c*d*e});
-- The following are from [BW-1], Figure 1.
assert(isShelling {a*b, c});
assert(not isShelling {a*b, c*d});
assert(isShelling {a*b*c, c*d, d*e, e*f, d*f});
assert(not isShelling {a*b*c, c*d, d*e*f});
///

-- Tests of isSimplex
TEST ///
R = QQ[a..d];
-- Simplices
assert(isSimplex simplicialComplex {a*b*c*d});
assert(isSimplex simplicialComplex {a*b*c});
assert(isSimplex simplicialComplex {a*b});
assert(isSimplex simplicialComplex {a});
assert(isSimplex simplicialComplex monomialIdeal {a,b,c,d}); -- empty complex
-- Non-simplices
assert(not isSimplex simplicialComplex {a*b, b*c, c*d});
assert(not isSimplex simplicialComplex {a*b, b*c*d});
assert(not isSimplex simplicialComplex {a*b, c});
assert(not isSimplex simplicialComplex {a, b, c, d});
///

-- Tests of isVertexDecomposable
TEST ///
R = QQ[a..f];
S = simplicialComplex {a*b*c*d*e*f};
assert(isVertexDecomposable S);
assert(isVertexDecomposable skeleton(3, S)); -- prop 2.2 in Provan-Billera
-- The following are from [BW-1], Figure 1.
assert(isVertexDecomposable simplicialComplex {a*b, c});
assert(isVertexDecomposable simplicialComplex {a*b*c, c*d, d*e, e*f, d*f});
assert(not isVertexDecomposable simplicialComplex {a*b*c, c*d, d*e*f});
///

-- New tests for hVector
----------------------------------------------
-- Boundary of the 4-Cross-Polytope
----------------------------------------------

TEST ///
S = QQ[x_{1,1}..x_{2,4}, Degrees => {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}]
I = monomialIdeal(x_{1,1}*x_{2,1},x_{1,2}*x_{2,2},x_{1,3}*x_{2,3},x_{1,4}*x_{2,4})
D = simplicialComplex(I)
assert( (hVector(D))#2 == 6)
assert( (hVector(D, Flag => true))#{1,1,0,1} == 1)
///




end


restart
uninstallPackage "SimplicialDecomposability"
installPackage "SimplicialDecomposability"
check "SimplicialDecomposability"

restart
loadPackage "SimplicialDecomposability";
R = QQ[a..g];
D = simplicialComplex {c*e*g, b*e*g, a*e*g, b*d*g, a*d*g, c*e*f, b*e*f, a*e*f};
isPure D
linearQuotients = O -> for i from 1 to #O-1 list unique flatten for j from 0 to i-1 list (
          ImJ = set support O_i - set support O_j;
          for k from 0 to i - 1 list (
               ImK = set support O_i - set support O_k;
               if #ImK == 1 and isSubset(ImK, ImJ) then
               first toList ImK else continue));
O1 = shellingOrder D
linearQuotients O1
O2 = shellingOrder(D, Random => true)
linearQuotients O2
O3 = shellingOrder(D, Permutation => {3,2,1,0,4,5,6,7})
linearQuotients O3
isVertexDecomposable D
select(allFaces(0, D), v -> isSheddingVertex(v, D))
E = link(D, f); 
ideal E
select(allFaces(0, E), v -> isSheddingVertex(v, E))
F = link(E, c); 
ideal F
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=SimplicialDecomposability RemakePackages=true RerunExamples=true IgnoreExampleErrors=false"
-- End:
