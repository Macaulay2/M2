newPackage(
	"BoijSoederberg",
    	Version => "1.5", 
    	Date => "April 01, 2015",
    	Authors => { -- This Package was originally written by D. Eisenbud, F. Schreyer, and M. Stillman. 
	             -- Various revisions and updates were made by C. Gibbons and B. Stone.
	     {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "http://www.msri.org/~de/"},
	     {Name => "Frank Schreyer", Email => "schreyer@math.uni-sb.de"},
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike"},
	     {Name => "Courtney Gibbons", Email => "crgibbon@hamilton.edu", HomePage => "http://people.hamilton.edu/cgibbons/"},
	     {Name => "Branden Stone", Email => "bstone@adelphi.edu", HomePage => "http://math.adelphi.edu/~bstone/"}
	     }, 
    	Headline => "betti diagram operations useful for investigating the Boij-Soederberg conjectures",
    	DebuggingMode => false
    	)

export {
     "mat2betti", -- documented
     "lowestDegrees", -- documented
     "highestDegrees", -- documented
     "isPure", -- documented
     "makePureBetti", --documented
     "makePureBettiDiagram", --documented
     "pureBetti", -- documented
     "pureBettiDiagram", -- documented

     "pureCharFree", -- documented    
     "pureTwoInvariant", -- documented
     "pureWeyman", -- documented
     "pureAll", -- documented
     
     "randomSocleModule", -- documented
     "randomModule", -- documented
     
     "pureCohomologyTable", -- documented
     "facetEquation", -- documented
     "dotProduct", -- documented
     "supportFunctional", -- not written
     
     "bott", -- documented
     
     "CohomologyTally", -- documented
     "mat2cohom", -- not written
     
     -- Methods
     "decomposeBetti", -- documented
     "decomposeDegrees", -- documented
     "isMassEliminate", -- documented
     "eliminateBetti", -- documented
     "makeCI", -- documented

     -- Options -- documented     
     "EliminationSequence",     
     "TableEntries",     
     "LeastIntegerEntries",
     "HerzogKuhl",
     "RealizationModules",
     
     -- Types
     "BettiEliminationTally"
     }
-- Also defined here:
-- pdim BettiTally
-- decompose BettiTally      -- documented
-- matrix BettiTally (2 versions) -- documented

-- FIXES A BUG in pdim in M2
--projectiveDimension = B -> max apply ((keys B), i->i_0) 
--     max apply ((keys B), i->i_0);
pdim BettiTally := (B) -> (
     max apply(select(keys B,i->B#i != 0), i -> i#0))

-- Used in supportFunctional
hf = (degrange, M) -> apply(degrange, d -> hilbertFunction(d,M))

----------------------------
-- CohomologyTally ---------
----------------------------

-- TO IMPLEMENT:
--   matrix(CohomologyTally, ...)
--   cohomologyTable Sheaf (probably need HF for this) -- done in BGG0
--   BettiTally * CohomologyTally DONE
--   supportFunctional
-- TO DOCUMENT:
--   CohomologyTally
--   pureAll
--   mat2cohom
--   bott (needs improvement)

-- A CohomologyTally is a hash table C
-- s.t. C#{i,d} => h^i(E(d)) is the i-th cohomology in degree d of some sheaf E.

CohomologyTally = new Type of VirtualTally
CohomologyTally.synonym = "Cohomology tally"
CohomologyTally == CohomologyTally := (C,D) -> C === D
CohomologyTally ++ CohomologyTally := (C,D) -> merge(C,D,plus)
--CohomologyTally ** CohomologyTally := (C,D) -> combine(C,D,(j,k)->apply(j,k,plus),times,plus)
CohomologyTally ZZ := (C,n) -> applyKeys(C, (i,d) -> (i,d-n))
--dual CohomologyTally := (C) -> applyKeys(C,j -> apply(j,minus))
--regularity CohomologyTally := opts -> (C) -> (
--     if opts.Weights =!= null then C = betti(C,opts);
--     max apply(keys C, (i,d,h) -> h-i))
--CohomologyTally Array := (C,A) -> (
--      if # A =!= 1 then error "expected array of length 1";
--      n := A#0;
--      applyKeys(C,(i,d,h) -> (i-n,d,h)))

ZZ * CohomologyTally := (d,C) -> applyValues(C, v -> d*v)

rawCohomologyTallyFormat = v -> (
     -- v is a CohomologyTally
     v' := new MutableHashTable;
     scan(pairs v, (key,n) -> (
	       (i,h) := key;
	       h = h+i;	-- skew in the usual way
	       key = (h,i);
	       if v'#?key then v'#key = v'#key + n else v'#key = n;
	       ));
     v = v';
     k := keys v;
     fi := first \ k;
     la := (s -> s#1) \ k;
     mincol := min la;
     maxcol := max la;
     minrow := min fi;
     maxrow := max fi;
     v = transpose table(toList (minrow .. maxrow), reverse toList (mincol .. maxcol), (i,j) -> if v#?(i,j) then v#(i,j) else 0);
     leftside := splice {"", apply(reverse(mincol .. maxcol), i -> toString i | ":")};
     v = applyTable(v, bt -> if bt === 0 then "." else toString bt);
     v = prepend(toString \ toList (minrow .. maxrow), v);
     v = apply(leftside,v,prepend);
     v)

net CohomologyTally := v -> netList(rawCohomologyTallyFormat v, Alignment => Right, HorizontalSpace => 1, BaseRow => 1, Boxes => false)

BettiTally * CohomologyTally := (B,C) -> (
     sum apply(keys B, (i,md,d) -> sum apply(keys C, (j,k) -> 
	       if j > i and first md =!= -k then 0 else (-1)^(i-j) * B#(i,md,d) * C#(j,k)))
     )

CohomologyTally * BettiTally := (C,B) -> B*C

-- input is a matrix over ZZ or QQ
-- output is the corresponding BettiTally 
mat2betti = method()
mat2betti(Matrix,ZZ) := (M,lowDegree) -> (
     e := entries M;
     a := flatten apply(#e, i -> 
	  apply(#e#i, j -> (j, {i+j+lowDegree}, i+j+lowDegree) => if e#i#j != 0 then e#i#j else null));
     a = select(a, b -> b#1 =!= null);
     new BettiTally from a
     )
mat2betti Matrix := (M) -> mat2betti(M,0)

mat2cohom = method()
mat2cohom(Matrix,ZZ) := (M,lowDegree) -> (
     -- lowDegree is the degree of the first column
     e := entries M;
     n := #e-1; -- row indices are n-1 .. 0
     a := flatten apply(#e, i -> 
	  apply(#e#i, j -> (n-i, i+j+lowDegree-n) => if e#i#j != 0 then e#i#j else null));
     a = select(a, b -> b#1 =!= null);
     new CohomologyTally from a
     )

--- Test 1
TEST ///
M = matrix "1,0,0,0;
        0,4,4,1"
B = mat2betti oo 
assert (M == matrix B)

B2 = mat2betti(M,2)
assert(M == matrix B2)
///


--- Test 2
TEST ///
m = matrix "5,0,0,0,0;
     	0,1,1,0,0;
	0,0,0,1,2"
c = mat2cohom (oo,0)
///

matrix(BettiTally, ZZ, ZZ) := opts -> (B,lowestDegree, highestDegree) -> (
     c := pdim B + 1;
     r := highestDegree - lowestDegree + 1;
     M := mutableMatrix(ZZ,r,c);
     scan(pairs B, (i,v) -> (
	       if v != 0 then
	         M_(i_2-i_0-lowestDegree, i_0) = v;
	       ));
     matrix M
     )

matrix BettiTally := opts -> (B) -> (
     lo := min apply(keys B, i -> if B#i == 0 then infinity else i_2-i_0);
     hi := max apply(keys B, i -> if B#i == 0 then -infinity else i_2-i_0);
     matrix(B,lo,hi)
     )

matrix(BettiTally,ZZ) := opts -> (B,lo) -> (
     hi := max apply(keys B, i -> if B#i == 0 then -infinity else i_2-i_0);
     matrix(B,lo,hi)
     )


-- Test 3
TEST ///
R = ZZ/101[a..e]
I = ideal borel monomialIdeal"abc,ad3,e4"
B = betti res I
M = matrix "1,0,0,0,0,0;
     	    0,0,0,0,0,0;
	    0,5,6,2,0,0;
	    0,51,176,230,135,30"
assert(matrix(B,0,3) == M)
assert(matrix B == M)
B = pureBettiDiagram{0,3,4,7,9}
C = matrix B
assert(B == mat2betti C)

B = pureBettiDiagram{-1,0,3,4,7}
C = matrix B
assert(B == mat2betti(C,-1))
///

lowestDegrees = method()
lowestDegrees BettiTally := (B) -> (
     pd := pdim B;
     for i from 0 to pd list (
	  B1 := select(keys B, k -> k#0 == i and B#k != 0);
	  min apply(B1, k -> k#2)
	  ))

highestDegrees = method()
highestDegrees BettiTally := (B) -> (
     pd := pdim B;
     for i from 0 to pd list (
	  B1 := select(keys B, k -> k#0 == i and B#k != 0);
	  max apply(B1, k -> k#2)
	  ))

isPure = method()
isPure BettiTally := (B) -> lowestDegrees B == highestDegrees B


-- Test 4
TEST ///
matrix "1,0,0;
     	0,2,3"  
B = mat2betti oo
assert(isPure B)

matrix "1,0,0; 0,2,1; 0,1,1"
B2 = mat2betti oo
assert(not isPure B2)
///

-- Test 5
TEST ///
--load "BoijSoederberg.m2"
m=matrix"1,0,0;
0,1,1;
0,0,1"
B=mat2betti m
assert(lowestDegrees B == {0,2,3})
assert(highestDegrees B == {0,2,4})
m=matrix"1,0,0;
0,0,1;
0,0,1"
B=mat2betti m
assert(lowestDegrees B == {0,infinity,3})
assert(highestDegrees B == {0,-infinity,4})
///

-------------------------------------
-- Pure Betti diagrams --------------
-------------------------------------

makePureBetti = method(Options => {TableEntries => LeastIntegerEntries})
makePureBetti List := o -> Degs -> (
     c := # Degs;
     p := 1;
     for i from 1 to c-1 do (
       if Degs#i <= Degs#(i-1) then error "--makePureBetti: expected an increasing list of integers";
       for j from 0 to i-1 do p=p*(Degs_i-Degs_j)
       );
     if o.TableEntries == LeastIntegerEntries 
     then (
          D := for i from 0 to c-1 list (
          (-1)^i * product(i, j->Degs_j-Degs_i) * product(i+1..c-1, j->Degs_j-Degs_i)
          );
          Bettis := for i from 0 to c-1 list (p/D_i);
          Bettis = Bettis / gcd Bettis;
          apply(Bettis, x -> lift(x,ZZ))
     )
     else if o.TableEntries == HerzogKuhl 
     then (
          for i from 0 to c-1 list (
          1/(product(for j from 0 to i-1 list Degs#i-Degs#j) * product(for j from i+1 to c-1 list Degs#j-Degs#i))
          )
     )
     else if o.TableEntries == RealizationModules
     then (
          L := for i from 1 to c-1 list (
               binomial(Degs#i -1,Degs#i-Degs#(i-1) - 1)
          );
          tag := first sort keys makePureBettiDiagram(Degs,TableEntries=>HerzogKuhl);
          b0 := (product L)*(1/(makePureBettiDiagram(Degs,TableEntries=>HerzogKuhl))#tag);
          --returns an error if the first degree is not 0. need to fix.
          for i from 0 to c-1 list (
               b0/(product(for j from 0 to i-1 list Degs#i-Degs#j) * product(for j from i+1 to c-1 list Degs#j-Degs#i))
          )
     )
)

-- alias for previous function; same functionality as original
pureBetti = method( )
pureBetti List := (Degs) -> (
    makePureBetti(Degs)
    )


-- Similar to pureBettiDiagram but with options and M2 naming convention
-- this was done in order to preserve the old functionality and give 
-- the ability to add options to the method
makePureBettiDiagram = method(Options => {TableEntries => LeastIntegerEntries})
makePureBettiDiagram List := o -> (degs) -> (
     B := makePureBetti(degs, TableEntries => o.TableEntries);
     new BettiTally from apply(#degs, i -> (i,{degs#i},degs#i) => B#i)
)

-- alias for previous; same functionality as original
pureBettiDiagram = method()
pureBettiDiagram List := (degs) -> (
     makePureBettiDiagram(degs)
     )



 

---Methods for general use---

--  input: pure Betti table
-- output: degree sequence (or an error if the diagram isn't pure)
listPureDegrees = method();
listPureDegrees BettiTally := B -> (
     if lowestDegrees(B)==highestDegrees(B) then return highestDegrees(B)
     else return "Error: diagram is not pure."
     )


-------------------------------------
--- Elimination Orders --------------
-------------------------------------
BettiEliminationTally = new Type of BettiTally;     
     
--  input:  BettiTally of a Cohen-Macaulay Module
-- output:  Boolean Value, True if more than one betti dies in the
--     	    decompose algorithm     	    
-- caveat:  Prints a warning if not the "Generic Case"
isMassEliminate = method();
isMassEliminate BettiTally :=  B -> (
      local SCAN; local D; local LD;
            
      scan( values B, i -> if i != 1 then break print "-- Warning: Not Generic Case");
      
      D = decomposeDegrees(B,TableEntries=>HerzogKuhl);
      LD = apply(#D-1, i-> D#(i+1)#1-D#i#1 );
      
      SCAN = scan( LD, l -> if #positions( l, i -> i != 0 ) != 1 then break "true" );
      if SCAN === null
      then return false 
      else return true;
     )

--  input:  BettiTally of a Cohen-Macaulay Module
--     	    Cohen-Macualay Ideal
-- output:  List, if no mass elimination occurs, a list is given sequencing
--     	    the homological degree of the elimination of betti numbers
-- options: EliminationSequence => Boolean; default is false, thus the output is 
--     	    a BettiTally.  If true, only the EliminationSequence is returned.
eliminateBetti = method(Options =>{EliminationSequence => false});
eliminateBetti BettiTally := o -> B -> (
     local D; local LD;
     local C;local L; local LL; local P; local K; local p; local c;
     
     if isMassEliminate(B) == true then print"\n --MASS EXTINCTION!--";
     
     D = decomposeDegrees(B,TableEntries=>HerzogKuhl);
     LD = apply(#D-1, i-> D#(i+1)#1-D#i#1 );
     
     if o.EliminationSequence == true then return apply( LD, l -> positions( l, i -> i != 0) );
     
     c = pdim B + 1;
     p = #D;
               
     C = new MutableHashTable from B;
     
     L = prepend( {p}, eliminateBetti( B, EliminationSequence => true ) ); 
     LL = apply(c, j -> positions(L, l ->  any( l, i -> i == j  )  ) );
     P = flatten prepend ( p, apply(1..(#LL-1), i ->  append(LL#i, p ) ) );
     if last LL == {0} then P = delete(0,P);
     K = sort keys C;
     scan(#P, i -> C#(K#i) = P#i );
     return new BettiEliminationTally from C;
    )

eliminateBetti Ideal := o -> I -> (
     return eliminateBetti( betti res I, EliminationSequence => o.EliminationSequence );
     )
  
-- Test 6  
TEST ///
R = ZZ/8821[x,y,z,w]
I = ideal(x,y^4,z^8,w^9)
B = betti res I
eliminateBetti I
X = eliminateBetti B
assert(X#(0,{0},0) === 12)
assert(X#(2,{13},13) === 10)

assert(isMassEliminate B === false)

R = QQ[x,y,z,w]
I = ideal(x^2,y^4,z^5,w^7)
B = makeCI{2,4,5,7}
C = betti res (R^1/I)
assert(B===C)

J = ideal(x^4,y^5,z^7,w^9)
D = makeCI{4,5,7,9}
E = betti res(R^1/J)
assert(D===E)

assert(isMassEliminate(E)===true)
///
 
--  input: List of degrees (type of an artinian complete intersection)
--  output: BettiTally of such a complete intersection
makeCI = method();
makeCI List := degs ->  (
     Cc := #degs; 
     S := (ZZ/499)(monoid[vars(0..< Cc)]); -- BettiTally is independent of the field.
     G := toSequence(for i from 0 to (Cc-1) list S_i^(degs#i));
     I := ideal G;
     betti res (S^1/G)
     )


---- helper method, not for export ----
--  input:  BettiTally of a Cohen-Macaulay Module
-- output:  List, differnce of degree sequence in decomposeDegreesHK
degreeDiff = method();
degreeDiff BettiTally := B -> (
     local D; 
     D = decomposeDegrees (B, TableEntries => HerzogKuhl);
     return apply(#D-1, i-> D#(i+1)#1-D#i#1 );
)
     
-------------------------
-- end of patch (for now)
-------------------------  


-- Test 7

TEST ///
assert(pureBetti{0,1,2,3,4} == {1,4,6,4,1})
assert(makePureBetti{0,1,2,3,4} == pureBetti{0,1,2,3,4})
assert(makePureBetti({0,1,2,3,4}, TableEntries => HerzogKuhl) === {1/24,1/6,1/4,1/6,1/24})
assert(makePureBetti({0,1,2,3,4},TableEntries => RealizationModules) == {1,4,6,4,1})

B = pureBettiDiagram {0,1,2,3,4}
assert(B == mat2betti matrix "1,4,6,4,1")
C = makePureBettiDiagram {0,1,2,3,4}
assert(C == mat2betti matrix "1,4,6,4,1")
D = makePureBettiDiagram({0,1,2,3,4},TableEntries => LeastIntegerEntries)
assert(B === D)
assert(B === C)
E = makePureBettiDiagram({0,1,2,3,4}, TableEntries => HerzogKuhl)
assert(E === mat2betti matrix "1/24,1/6,1/4,1/6,1/24")
F = makePureBettiDiagram({0,1,2,3,4}, TableEntries => RealizationModules)
assert(F === mat2betti matrix "1/1,4,6,4,1")

B1=pureBetti{0,2,3,4}
assert (B1 == {1,6,8,3})
assert (makePureBetti{0,2,3,4} == B1)
D1=pureBettiDiagram {0,2,3,4}
assert (D1 == mat2betti matrix "1,0,0,0; 0,6,8,3")
assert (makePureBettiDiagram{0,2,3,4} == D1)
assert(makePureBetti({0,2,3,4}, TableEntries => HerzogKuhl) == {1/24,1/4,1/3,1/8})
C1 = makePureBettiDiagram({0,2,3,4}, TableEntries => HerzogKuhl)
assert(C1 == mat2betti matrix "1/24,0,0,0; 0,1/4,1/3,1/8")
E1 = makePureBettiDiagram({0,2,3,4}, TableEntries => RealizationModules)
assert(lift(E1,ZZ) == D1)

B2 = pureBetti {0,2,3,5}
assert(B2 == {1,5,5,1})
D2 = pureBettiDiagram {0,2,3,5}
m = matrix "1,0,0,0;
     	    0,5,5,0;
	    0,0,0,1"   
assert(D2 == mat2betti m)
assert(makePureBetti({0,2,3,5},TableEntries=> RealizationModules) == {4,20,20,4})
C2 = makePureBettiDiagram({0,2,3,5},TableEntries=> RealizationModules)
assert(lift(C2,ZZ) == mat2betti matrix "4,0,0,0; 0,20,20,0; 0,0,0,4")
///

---------------------------------------------
-- Decomposing a Betti diagram into pures --
---------------------------------------------

--input: list of rational numbers
--output true if the list is strictly increasing, else false.
isStrictlyIncreasing=L->(
     t:=true;
     for i from 0 to #L-2 do t=(t and (L_i<L_(i+1)));
     t)

-- Test 8
TEST ///
debug BoijSoederberg
L={1,4,5,9}
assert(isStrictlyIncreasing L)
L={1,4,5,9,9}
assert(not isStrictlyIncreasing L)
///

--input: a BettiTally or a similar hash table
--output: a triple, 
--First element: the first summand in the (conjectural) Boij-Soederberg decomposition
--second element: the multiplier
--third element: the result of subtracting it.
decompose1= B->(
     L:=lowestDegrees B;
     if not isStrictlyIncreasing L then error "not in the simplex of pure Betti diagrams";
--     C:=pureBettiDiagram L;
     C:=makePureBettiDiagram L;     
     ratio:=min apply(#L, i->(B#(i,{L_i}, L_i))/(C#(i,{L_i},L_i)));
     (C,ratio,merge(B,C, (i,j)->i-ratio*j))
     )
 
--  input: BettiTally
decompose2 = B -> (
     L:=lowestDegrees B;
     if not isStrictlyIncreasing L then error "not in the simplex of pure Betti diagrams";
     C:=makePureBettiDiagram( L,TableEntries=>RealizationModules);
     ratio:=min apply(#L, i->(B#(i,{L_i}, L_i))/(C#(i,{L_i},L_i)));
     (C,ratio,merge(B,C, (i,j)->i-ratio*j))
     ) 

--  input: BettiTally
decompose3 = B -> (    
     L:=lowestDegrees B;
     if not isStrictlyIncreasing L then error "not in the simplex of pure Betti diagrams";
     C:=makePureBettiDiagram( L,TableEntries=>HerzogKuhl);
     ratio:=min apply(#L, i->(B#(i,{L_i}, L_i))/(C#(i,{L_i},L_i)));
     (C,ratio,merge(B,C, (i,j)->i-ratio*j))
     )



-- Same as decompose but with options.
-- this was done in order to preserve the old functionality and give 
-- the ability to add options to the method.
-- We would like to replace 'decompse' with 'decomposeBetti'.
decomposeBetti = method(Options => {TableEntries => LeastIntegerEntries})
decomposeBetti BettiTally := o -> B -> (
    
    Components:={};
    B1:= new MutableHashTable from B;
    
     if o.TableEntries == LeastIntegerEntries 
     then (
     	 while min values B1 >= 0 and max values B1 > 0 do (
	     X:=decompose1(new BettiTally from B1);
	     B1=new MutableHashTable from X_2;
	     --change the type of the values in X_0 to ZZ
	     Y:=new BettiTally from apply(pairs X_0, i->{first i, lift(last i, ZZ)});
	     Components = append(Components, hold(X_1) * Y));
     	 sum Components
     )
     else if o.TableEntries == HerzogKuhl 
     then (
	 while min values B1 >= 0 and max values B1 > 0 do (
	     X3:=decompose3(new BettiTally from B1);
	     B1=new MutableHashTable from X3_2;
	     --change the type of the values in X3_0 to ZZ
	     Y3:=new BettiTally from apply(pairs X3_0, i->{first i,last i});
	     Components = append(Components, hold(X3_1) * Y3));
	 sum Components	 
     )
     else if o.TableEntries == RealizationModules
     then (
     	 while min values B1 >= 0 and max values B1 > 0 do (
	     X2:=decompose2(new BettiTally from B1);
	     B1=new MutableHashTable from X2_2;
	     --change the type of the values in X2_0 to ZZ
	     Y2:=new BettiTally from apply(pairs X2_0, i->{first i,lift(last i,ZZ)});
	     Components = append(Components, hold(X2_1) * Y2));
     	sum Components
     )     
)

--input: a BettiTally
--output: The routine prints the Boij-Soederberg summands.
--prints "not in convex hull" if the given BettiTally is not in the convex
--hull of the allowable pure betti diagrams. Prints an error message
--if the decomposition fails. Returns a list of the components as a list of pairs,
--each a rational coefficient followed by a hash table representing a pure betti diagram,
--if it succeeds.
decompose BettiTally := B-> (
    	decomposeBetti B
     )
 

-- Test 9
TEST ///
M=matrix "1,0,0,0;
        0,4,4,1"
B=mat2betti M

C1=decomposeBetti(B, TableEntries => LeastIntegerEntries )
L=set apply(toList C1,x->x#1)
m1=mat2betti matrix "1,0,0,0;
                     0,6,8,3"
m2=mat2betti matrix "1,0,0;
                     0,3,2"
M'=set{m1,m2}
assert(L===M')

C2=decomposeBetti(B, TableEntries => HerzogKuhl )
L2=set apply(toList C2,x->x#1)
m1c2=mat2betti matrix "1/24,0,0,0;
                     0,1/4,1/3,1/8"
m2c2=mat2betti matrix "1/6,0,0;
                     0,1/2,1/3"		     
Mc2=set{m1c2,m2c2}
assert(L2===Mc2)

C3=decomposeBetti (B, TableEntries => RealizationModules )
L=set apply(toList C3,x->x#1)
m1=mat2betti matrix "1,0,0,0;
                     0,6,8,3"
m2=mat2betti matrix "1,0,0;
                     0,3,2"		     
M'=set{m1,m2}
assert(L===M')




M=matrix "1,0,0,0;
     	  0,5,5,1;
	  0,0,1,1"		    
B=mat2betti M

C1=decomposeBetti(B, TableEntries => LeastIntegerEntries )
L=set apply(toList C1,x->x#1)
m1=mat2betti matrix "1,0,0,0;
                     0,6,8,3"
m2=mat2betti matrix "1,0,0,0;
                     0,5,5,0;
		     0,0,0,1"
m3=mat2betti matrix "3,0,0,0;
                     0,10,0,0;
		     0,0,15,8"
M'=set{m1,m2,m3}
assert(L===M')

C2=decomposeBetti(B, TableEntries => HerzogKuhl )
L=set apply(toList C2,x->x#1)
m1=mat2betti matrix "1/24,0,0,0;
                     0,1/4,1/3,1/8"
m2=mat2betti matrix "1/30,0,0,0;
                     0,1/6,1/6,0;
		     0,0,0,1/30"
m3=mat2betti matrix "1/40,0,0,0;
                     0,1/12,0,0;
		     0,0,1/8,1/15"
M'=set{m1,m2,m3}
assert(L===M')


C3=decomposeBetti(B, TableEntries => RealizationModules )
L=set apply(toList C3,x->x#1)
m1=mat2betti matrix "1,0,0,0;
                     0,6,8,3"
m2=mat2betti matrix "4,0,0,0;
                     0,20,20,0;
		     0,0,0,4"
m3=mat2betti matrix "3,0,0,0;
                     0,10,0,0;
		     0,0,15,8"
M'=set{m1,m2,m3}
assert(L===M')




M=matrix"1,0,0,0;
     	 0,2,0,0;
	 0,1,3,1"
B=mat2betti M

C1=decomposeBetti(B, TableEntries => LeastIntegerEntries )
L=set apply(toList C1,x->x#1)
m1=mat2betti matrix"1,0,0;
     	  0,2,0;
	  0,0,1"
m2=mat2betti matrix"3,0,0,0;
     	  0,10,0,0;
	  0,0,15,8"
m3=mat2betti matrix"1,0,0;
     	  0,0,0;
	  0,4,3"
M'=set{m1,m2,m3}
assert(L===M')

C2=decomposeBetti(B, TableEntries => HerzogKuhl )
L=set apply(toList C2,x->x#1)
m1=mat2betti matrix"1/8,0,0;
     	  0,1/4,0;
	  0,0,1/8"
m2=mat2betti matrix"1/40,0,0,0;
     	  0,1/12,0,0;
	  0,0,1/8,1/15"
m3=mat2betti matrix"1/12,0,0;
     	  0,0,0;
	  0,1/3,1/4"
M'=set{m1,m2,m3}
assert(L===M')

C3=decomposeBetti(B, TableEntries => RealizationModules )
L=set apply(toList C3,x->x#1)
m1=mat2betti matrix"3,0,0;
     	  0,6,0;
	  0,0,3"
m2=mat2betti matrix"3,0,0,0;
     	  0,10,0,0;
	  0,0,15,8"
m3=mat2betti matrix"1,0,0;
     	  0,0,0;
	  0,4,3"
M'=set{m1,m2,m3}
assert(L===M')
///

-- Similar to decompose but with options.
-- this was done in order to preserve the old functionality and give 
-- the ability to add options to the method.
-- This does not output the Betti tables themselves,
-- but instead outputs their associated degree sequences. 
decomposeDegrees = method(Options => {TableEntries => LeastIntegerEntries})
decomposeDegrees BettiTally := o -> B -> (
    
    Components:={};
    B1:= new MutableHashTable from B;
    local X; local Y;
    
     if o.TableEntries == LeastIntegerEntries 
     then (
           while min values B1 >= 0 and max values B1 > 0 do (
          X=decompose1(new BettiTally from B1);
          B1=new MutableHashTable from X_2;
          --change the type of the values in X_0 to ZZ
          Y=new BettiTally from apply(pairs X_0, i->{first i, lift(last i, ZZ)});
          Components = append(Components, (X_1,listPureDegrees(Y))));
          Components
     )
     else if o.TableEntries == HerzogKuhl 
     then (
     while min values B1 >= 0 and max values B1 > 0 do (
       X=decompose3(new BettiTally from B1);
       B1=new MutableHashTable from X_2;
       --change the type of the values in X_0 to ZZ
       Y=new BettiTally from apply(pairs X_0, i->{first i,last i});
       Components = append(Components, (X_1,listPureDegrees(Y))));
     Components
     )     
     else if o.TableEntries == RealizationModules
     then (
           while min values B1 >= 0 and max values B1 > 0 do (
          X=decompose2(new BettiTally from B1);
          B1=new MutableHashTable from X_2;
          --change the type of the values in X_0 to ZZ
          Y=new BettiTally from apply(pairs X_0, i->{first i,last i});
          Components = append(Components, (X_1,listPureDegrees(Y))));
     Components
     )     
 )

-- Test 10
TEST ///
restart
loadPackage"BoijSoederberg"

M=matrix "1,0,0,0;
        0,4,4,1"
B=mat2betti M	

D = decomposeDegrees(B, TableEntries => LeastIntegerEntries )
C = {(1/3,{0,2,3,4}), (2/3,{0,2,3})}
assert(C===D)

D = decomposeDegrees(B, TableEntries => HerzogKuhl )
C = {(8/1,{0,2,3,4}), (4/1,{0,2,3})}
assert(C===D)

D = decomposeDegrees(B, TableEntries => RealizationModules )
C = {(1/3,{0,2,3,4}), (2/3,{0,2,3})}
assert(C===D)


M=matrix "1,0,0,0;
     	  0,5,5,1;
	  0,0,1,1"		    
B=mat2betti M

D = decomposeDegrees(B, TableEntries => LeastIntegerEntries )
C = {(1/3,{0,2,3,4}), (7/15,{0,2,3,5}), (1/15,{0,2,4,5})}
assert(C===D)

D = decomposeDegrees(B, TableEntries => HerzogKuhl )
C = {(8/1,{0,2,3,4}), (14/1,{0,2,3,5}), (8/1,{0,2,4,5})}
assert(C===D)

D = decomposeDegrees(B, TableEntries => RealizationModules )
C = {(1/3,{0,2,3,4}), (7/60,{0,2,3,5}), (1/15,{0,2,4,5})}
assert(C===D)


M=matrix"1,0,0,0;
     	 0,2,0,0;
	 0,1,3,1"
B=mat2betti M

D = decomposeDegrees(B, TableEntries => LeastIntegerEntries )
C = {(1/8,{0,2,4,5}), (3/8,{0,2,4}), (1/4,{0,3,4})}
assert(C===D)

D = decomposeDegrees(B, TableEntries => HerzogKuhl )
C = {(15/1,{0,2,4,5}), (3/1,{0,2,4}), (3/1,{0,3,4})}
assert(C===D)

D = decomposeDegrees(B, TableEntries => RealizationModules )
C = {(1/8,{0,2,4,5}), (1/8,{0,2,4}), (1/4,{0,3,4})}
assert(C===D)
///


---------------------------------------------
-- Cohomology Tables ------------------------
---------------------------------------------

pureCohomologyTable = method(TypicalValue => CohomologyTally)
-*
pureCohomologyTable(List, ZZ, ZZ, Symbol) := (zeros, lo, hi, old) -> (
     R := QQ (monoid [z]);
     hp = product(zeros, a -> (R_0 - a));
     n := #zeros;  -- in PP^n
     b := gcd apply(n+1, i -> sub(hp, R_0=>i_QQ));
     hp = hp//b;
     w := 0;
     new BettiTally from for i from lo-n to hi list (
	  v := abs sub(hp, R_0=> i_QQ);
	  if v == 0 then (w=w+1; continue;);
	  d := i+w;
	  (n+i-w, {i},i) => lift(v,ZZ)
	  ));
*-
pureCohomologyTable(List, ZZ, ZZ) := (zeros, lo, hi) -> (
     z := local z;
     R := QQ (monoid [z]);
     hp := product(zeros, a -> (R_0 - a));
     n := #zeros;  -- in PP^n
     b := gcd apply(n+1, i -> sub(hp, R_0=>i_QQ));
     hp = hp//b;
     w := 0;
     new CohomologyTally from for i from lo-n to hi list (
	  v := abs sub(hp, R_0=> i_QQ);
	  if v == 0 then (w=w+1; continue;);
	  (n-w,i) => lift(v,ZZ)
	  ));
  
-- Test 11
TEST ///
m = matrix "4,3,2,1,0,0,0,0;
            0,0,0,0,1,2,3,4"
A= mat2cohom (m,-3)
assert(pureCohomologyTable({0},-3,4) == A)

m2 = matrix "120,70,36,15,4,0,0,0,0,0,0;
     	     0,0,0,0,0,0,0,0,0,0,0;
	     0,0,0,0,0,1,0,0,0,0,0;
	     0,0,0,0,0,0,6,20,45,84,140" 
A2 = mat2cohom(m2, -5)
assert(pureCohomologyTable({-3,-2,0},-5,5)==A2)

///

---------------------------------------------
-- Facet equations and the quadratic form ---
---------------------------------------------
-- The only exported functions here: facetEquation, dotProduct

bettiMatrix = (L,lo,hi) -> matrix(pureBettiDiagram L, lo, hi)

rangeOK=method()

rangeOK(List,ZZ,ZZ,ZZ) := (d,lowestDegree, highestDegree, n) -> (
    --tests whether degree seq is >=lowestDegree, strictly increasing, 
    -- of the right length n, and last elt
    -- is bounded by highestDegree + num vars.
    for i from 0 to n-1 do if d#(i+1) <= d#i then return false;
    #d == n+1 and lowestDegree <= d_0 and d_n <= highestDegree+n
    )

nextLower=method()
nextLower(List, ZZ, ZZ) := (d,lowestDegree, highestDegree) -> (
     --returns A deg seq adjacent to d and below it (chooses one such)
     n:=#d-1;
     if d_0>lowestDegree then join( {d_0-1},d_{1..n}) 
     else if d_0==lowestDegree 
       then (
	    k:=1; 
	    while (d_k-1==d_(k-1) ) do (k=k+1);
	    join(d_{0..k-1},{d_k-1},d_{k+1..n}))
       else error("lowestDegree is too high")
       )

nextUpper=method()
nextUpper(List, ZZ, ZZ):=(d,lowestDegree, highestDegree)->(
	  --same but above
	  n:=#d-1;
	  if d_n<n+highestDegree then join(d_{0..n-1},{d_n+1}) 
	  else if d_n==n+highestDegree then
	  (k:=n; 
	  while (d_k-1==d_(k-1) ) do (k=k-1);
	  join(d_{0..k-2},{d_(k-1)+1},d_{k..n}))
     	  else error"highestDegree is too low")

lowerRange=method()

lowerRange(List,ZZ,ZZ):=(d,lowestDegree, highestDegree)->(
	  --returns a maximal chain of deg seqs below d
	  n:=#d-1;
	  rangeOK(d,lowestDegree, highestDegree,n);
	  A:={d};
	  if d =!= toList(lowestDegree..n+lowestDegree)  then ( 
          e:=nextLower(d,lowestDegree,highestDegree);
	  A=join(A,lowerRange(e,lowestDegree,highestDegree)));
     	  A)

upperRange=method()

upperRange(List,ZZ,ZZ):=(d,lowestDegree, highestDegree)->(
	  --returns a maximal chain of deg seqs below d
	  n:=#d-1;
	  rangeOK(d,lowestDegree, highestDegree,n);
	  A:={d};
	  if d =!= toList(highestDegree..n+highestDegree) then ( 
          e:=nextUpper(d,lowestDegree,highestDegree);
	  A=join(A,upperRange(e,lowestDegree,highestDegree))); --EEE
     	  A)

rangeMatrices=method()
rangeMatrices(List,ZZ,ZZ):=(e,lowestDegree,highestDegree)->( 
	       --takes a deg seq, returns list of mats, k-th has 
	       --a one in posn e_k-k-lowestDegree, k
	       n:=#e-1;
	       r:=highestDegree-lowestDegree;
               apply(n+1,k->
		    map(ZZ^(r+1), ZZ^(n+1),
			 (i,j)-> if i==e_k-k-lowestDegree and j==k then 1 else 0)  -- subtracted lowestDegree
		    )
	       )

-*
peskineSzpiro=(r,n)->apply(n+r+1,k->matrix apply(r+1,i->apply(n+1,j->
--returns (redundant) list of n+r+1 Peskine-Szpiro relations in hilb function form
--(the Hilb fcn values from *** to ***) Note: the P-S eqns in this sense are
--\chi(O(m)\otimes M)= 0 .
	       (if k<r then 1 else (-1)^(k-r))*(-1)^(n-j)*
	       binomial(n+r-k-i-j+n-1,n-1)
	       )))
--PS=peskineSzpiro(r,n); 
--This was a global variable used in numericalComplex and flipEquation only

numericalComplex=A->(
     n:=rank source A-1;
     r:=rank target A-1;
     AA:=A;
     apply(n+r+1,i->(
	       PS:=peskineSzpiro(r,n); 
	       ss:=if i<=r then AA_(r-i,n) else AA_(0,n-i+r);
	       AA=AA-ss*PS_i;
	       ss)))
--The numerical complex that flips the "upper" eqn to the "lower" eqn, written
--as the sequence of coefficients of the PS-equations.

flipEquation=(A)->(
     n:=rank source A-1;
     r:=rank target A-1;
     aa:=numericalComplex(A);
     PS:=peskineSzpiro(r,n); 
     apply(n+r+1,c->A-sum(c,i->aa_(i)*PS_(i))))

upperEquation=(A)->(L:=flipEquation(A);L_(#L-1))
--the necessary lin comb of the PS equations.

TEST ///
load "BoijSoederberg.m2"
d={0,1,4}
F=facetEquation(d,0,0,3)
numericalComplex F
flipEquation F
upperEquation F

d={0,3,4,5,8,10}
F=facetEquation(d,2,-3,10)
numericalComplex F
flipEquation F
upperEquation F

--Note: UpperEquation has zeros above and ON the places where the betti diagram
--for the degree sequence d is nonzero. But this fact follows from the vanishing of
--the lower equation at such sequences.
///
*-

middleComplex=(d,e)->(
     n:=#d-1;
     t:=1;
     L:=apply(n+1,i->(t=t*if d_i==e_i then 1 else -1 ));
     apply(n+1,c->if L_c==1 then e_c else d_c))

nextPure=(d,k)->if d_k+1==d_(k+1) and (k==#d-2 or d_(k+2)>d_(k+1)+1) then 
     apply(#d,i->if i<k or i>( k+1) then d_i else d_i+1) else error("no next pure sequence")
--in the case of two degree sequences differing by one in two consec places, computes
--the degree sequence in between.

lowerAndUpper = (de,k) -> (
     -- returns null if this is not a valid facet
     -- otherwise returns (d, e), where d < de < e
     p := new MutableList from de;
     q := new MutableList from de;
     p#(k+1) = de#(k+1) - 1;
     q#k = de#k + 1;
     -- This is not valid exactly when:
     (new List from p, new List from q)     
     )

checkAllowedFacet = (de,i) -> (
     n := #de - 1;
     if de#i + 2 != de#(i+1) then error "expected de#(i+1)-de#i == 2";
     if i < 0 or i >= n then error ("expected i in range 0.."|n);
     )

facetEquation=method();

facetEquation(List,ZZ,ZZ,ZZ) := (de,i,lowestDegree, highestDegree) -> (
     --i an integer between 0 and #de-2
     --de a strictly increasing list of integers 
     --such that de_(i+1)=de_i+2
     --lowestDegree < highestdegree, integers 
     --lowest degree should be <=de_0, highestDegree-lowestDegree >=d_n-n, and >=d_n-n+1 (!!check!!) when i=n-1,
     --where n=#d-1.
     --routine produces the "upper" equation of the supporting hyperplane
     --of the facet corresponding to d< (...d_i+1, d_(i+1)+1,...)=nextpure(d,k).
     --the equation is presented as a 
     -- map ZZ^(#d) --> ZZ^(highest-lowest+1).
     n := #de-1;
     rangeOK(de,lowestDegree,highestDegree,n);
     checkAllowedFacet(de,i); -- will give an error if not
     (d,e) := lowerAndUpper(de,i);
     A:=matrix apply(lowerRange(d,lowestDegree, highestDegree),
	  c->join(toSequence entries bettiMatrix(c,lowestDegree,highestDegree)));
     B:=matrix apply(upperRange(e,lowestDegree,highestDegree),
	  c->join(toSequence entries bettiMatrix(c,lowestDegree,highestDegree)));
     C:=matrix apply(rangeMatrices(e,lowestDegree,highestDegree),c->join(toSequence entries c));
     D:=(entries transpose syz(A||B||C))_0;
     F:=matrix apply(highestDegree-lowestDegree+1,i->apply(n+1,j->D_((n+1)*i+j)));
     B1:=bettiMatrix(de,lowestDegree,highestDegree);
     if dotProduct(F,B1)>0 then F else -F)

-- Test 12
TEST ///
m = matrix "0,1,-2;
            0,0,0;
	    0,0,0"
assert(facetEquation({0,1,3},1,0,2) == m)

m2 = matrix "24,-7,0,0,4;
     	     7,0,0,-4,9;
	     0,0,4,-9,12;
	     0,0,0,0,10;
	     0,0,0,0,0"
assert(facetEquation({1,2,3,5,7}, 2,-1,3) == m2)
///

dotProduct=method()

dotProduct(Matrix, Matrix):=(A,B)->
     --dot product of matrices as vectors
     sum(rank target A,i->sum(rank source A,j->A_(i,j)*B_(i,j)))

dotProduct(BettiTally, BettiTally):=(A,B) ->
     --dot product of two hash tables with similar structure
     sum(keys A, k-> if B#?k then B#k * A#k else 0)

dotProduct(Matrix, ZZ, BettiTally):=(A,lowest, B)->(
     --lowest is an integer specifying what degree the first row of A is supposed to have.
     nr:=rank target A;
     highest:=nr+lowest-1;
     nc:=rank source A;
     d0:=min((keys B)/last);
     regB:=max(((keys B)/last)-(keys B)/first);
     lengthB:=max((keys B)/first);
     if d0<lowest then error "matrix A begins in too high a degree";
     if regB>highest then error "matrix A stops in too low a degree";
     if nc < 1+lengthB then error "matrix A has too few columns";
     sum(keys B, k-> (B#k)*(A_(last k-first k-lowest, first k)))
     )

-* -- A newer version, but perhaps with less warnings
dotProduct(Matrix, ZZ, BettiTally) := (A,lowest, B) -> dotProduct(mat2betti(A,lowest),B)
*-

dotProduct(Matrix, BettiTally) := (A,B) -> dotProduct(A,0,B)

-- Test 13
TEST ///
A = matrix"1,1,0;
     	   0,1,1;
	   0,1,1"
B = matrix"0,1,-2;
     	   0,0,0;
	   0,0,0"
assert(dotProduct(A, B) == 1)

A1 = mat2betti A
B1 = mat2betti B
assert(dotProduct(A1, B1)==1)

assert(dotProduct(A, 0, B1)==1)

assert(dotProduct(A, B1)==1)

A2=matrix"1,0,0,0;
    0,5,5,1;
    0,0,1,1" 
B2 = facetEquation({0,2,4,5}, 1,0,2)
assert(dotProduct(A2,B2)==2)
assert(dotProduct(mat2betti A2, mat2betti B2)==2)
assert(dotProduct(A2, mat2betti B2) == 2)

///


-* supportFunctional is NOT functional yet *-
supportFunctional=method()

supportFunctional(ChainComplex, ChainComplex):=(E,F)->(
     --E should be a chain complex starting in degree 0 and going to negative degrees.
     --F should be a chain complex starting in a postive degree and going to degree 0
     -- the code is meant to execute 
     --\langle E, \beta\rangle = \sum_{j\leq i}(-1)^{i-j}\sum_k\beta_{i,k}h_{-k}(H^j(E)),
     --
     lengthF := length F;
     degreesF:=flatten (for i from 0 to lengthF list flatten degrees F_i);
     minF := min degreesF;
     maxF := max degreesF;
     HHE:=HH E;
     L:=for i from 0 to length E list matrix{{hf(minF..maxF, (HH E)#(-i))}};
     A:=transpose L_0;
     for i from 1 to length L -1 do A = A|(transpose L_i);
     AA:=map(ZZ^(maxF-minF-lengthF+1), ZZ^(lengthF+1), (p,q)->
     	  sum(0..min(q,length E), 
	       j->if HHE#?(-j) then (-1)^(q-j)*hilbertFunction(-p-q, (HHE)#(-j)) else 0));
     dotProduct(AA, minF, betti F)
     )

supportFunctional(ChainComplex, BettiTally):=(E,B)->(
     lengthF := max apply(keys B, K->first K);
     degreesF := apply(keys B, K->last K);
     minF := min degreesF;
     maxF := max degreesF;
     HHE := HH E;
     L := for i from 0 to length E list matrix{{hf(minF..maxF, (HH E)#(-i))}};
     A := transpose L_0;
     for i from 1 to length L-1 do A = A|(transpose L_i);
     AA := map(ZZ^(maxF-minF-lengthF+1), ZZ^(lengthF+1), (p,q)->
     	  sum(0..min(q,length E), 
	       j->if HHE#?(-j) then (-1)^(q-j)*hilbertFunction(-p-q, (HHE)#(-j)) else 0));
     dotProduct(AA, minF, B)
     )

---------------------------------------------
-- Pure Betti diagrams that actually occur --
---------------------------------------------
rkSchur = (n,L) -> (
--rank of a Schur functor on a module
--input: a non-neg integer n and a non-increasing sequence of non-neg integers L
--output: the rank of the representation S_L(C^n). Here (11111) represents an exterior power, (k) a symmetrci power.
     M:=L;
     if #M<n then M=L|toList(n-#M:0);
     det map(ZZ^n, ZZ^n, (i,j)->binomial(M_i+n-1-i+j, n-1)))

-- Test 14
TEST ///
debug BoijSoederberg
rkSchur(6,{1,1,1,1}) -- exterior power
rkSchur(6,{2}) -- symmetric power
rkSchur(3,{3,2,0})
rkSchur(3,{4,3,1}) -- the previous tensored with the top exterior power

needsPackage "SchurRings"
R = schurRing(s,6)
assert(dim s_{1,1,1,1} == rkSchur(6,{1,1,1,1})) -- exterior power
assert(dim s_{2} == rkSchur(6,{2})) -- symmetric power
R = schurRing(s,3)
assert(dim s_{3,2,0} == rkSchur(3,{3,2,0}))
assert(dim s_{4,3,1} == rkSchur(3,{4,3,1})) -- the previous tensored with the top exterior power
///

pureCharFree = method()
pureCharFree List := (L) -> product(1..#L-1, i->binomial(L_i-L_0-1, L_i-L_(i-1)-1))
--gives beta_0 for the pure complex described in Eisenbud-Schreyer

pureTwoInvariant = method()
pureTwoInvariant List := (L) -> (
--gives beta_0 for the pure complex described in Eisenbud-Weyman-Floeystad for two modules
   n:=#L-1;
   T:={};
     E:=for i from 1 to #L-1 list (L_i-L_(i-1));
     for i from 1 to #E do
     for j from 1 to E_(n-i)-1 do (T= T| {n-i});
     T=T |  {0};
     rkSchur(#T, toSequence T)
     )

pureWeyman = method()
pureWeyman List := (L) -> (
     D:=for i from 1 to #L-1 list L_i-L_0;
     E0:=for i from 1 to #D-1 list  D_(i)-D_(i-1)-1;
     E:={D_0-1}|E0;
     Eplus1:=E+toList(#D:1);
     lambda := for i from 1 to #D list sum E_{i..#D-1};
     rkSchur(#D,lambda)
     )

pureAll = method()
pureAll List := (L) -> (pureCharFree L, pureTwoInvariant L, pureWeyman L)

-- Test 15
TEST ///
assert(pureAll{0,1,2,3,4} == (1,1,1))
assert(pureAll{0,1,3,4} == (2,2,3))
W = pureAll{0,4,6,9,11}
assert(W == (1400, 14700, 175))
P = pureBetti{0,4,6,9,11}
for i from 0 to #W-1 list (W_i/P_0)
///

----------------------------------------------------
-- Constructions often producing pure resolutions --
----------------------------------------------------

--Given a strictly increasing degree sequence L and a number of gneerators m,
--this routine produces a "generic" module of finite length with the 
--m generators and number of socle elements  and regularity corresponding
--to the pure resolution with degree sequence L. The module is constructed
--by taking a certain number of generic elements inside an appropriate direct
--sum of copies of a zero-dimensional complete intersection. We use the fact
--that in a polynomial ring in c variables, 
--modulo the r+1 st power of each variable, the part of
--generated in degree (c-1)r looks like the part of the injective hull
--of the residue class field generated in degree -r.

randomSocleModule = method(Options => {CoefficientRing => ZZ/101})
randomSocleModule(List, ZZ) := opts -> (L, m) -> (
     c:=#L-1; -- codimension
     r:=last L-first L-c; -- regularity
     s:=c*r; -- socle degree mod (vars)^[r+1]
     kk := opts.CoefficientRing;
     R:=kk[vars(0..c-1)];
     mR := ideal apply(gens R, x -> x^(r+1));
     B:=pureBetti L;
     f:=random(R^(m*B_c), R^{m*B_0:-s+r});
     prune (image (f**(R^{s-r}/mR)))
     )
     
-- Test 16     
TEST ///
setRandomSeed()
L={0,1,3,4}
B = pureBettiDiagram L
assert(betti res randomSocleModule(L,1) == mat2betti matrix"1,2,1,0;
					0,1,2,1")
assert(2*B == betti res randomSocleModule(L,2))
assert(3*B == betti res randomSocleModule(L,3))

L={0,2,3,6}
B = pureBettiDiagram L
assert(B == betti res randomSocleModule(L,1))
assert(2*B == betti res randomSocleModule(L,2))
assert(3*B == betti res randomSocleModule(L,3))
///

randomModule = method(Options => {CoefficientRing => ZZ/101})
randomModule(List,ZZ) := opts -> (L, m) -> (
     c:=#L-1; -- codimension
     kk := opts.CoefficientRing;
     R:=kk[vars(0..c-1)];
     B:=pureBetti L;
     coker (M:=random(R^{m*B_0:-L_0}, R^{m*B_1:-L_1})))

-- Test 17
TEST ///
setRandomSeed()
L={0,4,9,10}
B = pureBetti L

B'= betti res randomModule(L,1)
M = mat2betti matrix"1,0,0,0;
     	       	     0,0,0,0;
		     0,0,0,0;
		     0,3,0,0;
		     0,0,0,0;
		     0,0,0,0;
		     0,0,3,0;
		     0,0,0,0;
		     0,0,0,0;
		     0,0,0,1"
assert(B' == M)

B'=betti res randomModule(L,2)
M = mat2betti matrix"2,0,0,0;
     	       	     0,0,0,0;
		     0,0,0,0;
		     0,6,0,0;
		     0,0,0,0;
		     0,0,0,0;
		     0,0,0,0;
		     0,0,16,12"
assert(B'==M)

B'=betti res randomModule(L,3)
M = mat2betti matrix"3,0,0,0;
     	       	     0,0,0,0;
		     0,0,0,0;
		     0,9,0,0;
		     0,0,0,0;
		     0,0,0,0;
		     0,0,0,0;
		     0,0,24,18"
assert(B'==M)

B'=betti res randomModule(L,4)
M = mat2betti matrix"4,0,0,0;
     	       	     0,0,0,0;
		     0,0,0,0;
		     0,12,0,0;
		     0,0,0,0;
		     0,0,0,0;
		     0,0,0,0;
		     0,0,32,24"
assert(B'==M)

B'=betti res randomModule(L,2, CoefficientRing=>ZZ/32003)
M = mat2betti matrix"2,0,0,0;
     	       	     0,0,0,0;
		     0,0,0,0;
		     0,6,0,0;
		     0,0,0,0;
		     0,0,0,0;
		     0,0,0,0;
		     0,0,16,12"
assert(B'==M)
///

-------------------------------------------
-- Bott algorithm -------------------------
-------------------------------------------
bott=method()
bott(List, ZZ):=(L,u)->(
     --given a weakly decreasing list of integers L of length n and an integer u,
     --uses Bott's algorithm to compute the cohomology of the vector bundle 
     -- E=O(-u) \tensor S_L(Q), on P^n = PP(V)
     --where Q is the tautological rank n quotient bundle in the sequence 
     --   0--> O(-1) --> O^(n+1) --> Q -->0
     --and S_L(Q) is the Schur functor with the convention S_(d,0..0) = Sym_d, S_(1,1,1) = \wedge^3 etc.
     --returns either 0, if all cohomology is zero,
     -- or a list of three elements: A weakly decreasing list of n+1 integers M;
     -- a number i such that H^i(E)=S_M(V); and 
     -- the rank of this module.
     M:=new MutableList from join(L,{u});
     i:=0;
     j:=#M-1;
     while j>0 do(
     	  while M#(j-1)>=M#j and j>0 do j=j-1;
	  if j==0 then break;
     	  if M#j==M#(j-1)+1 then return 0 else (
	       i=i+1;
	       k:=M#(j-1);
	       M#(j-1)=(M#j)-1;
	       M#j=k+1;
	       j=#M-1);
	         );
	    	 M=toList apply(M, t->t-(last M));
	  {M,i, rkSchur(#M,M)}
     )

bott(List,ZZ,ZZ,Symbol):=(L,low,high,old)->(
     --produces the betti diagram of the tate resolution of the sheaf S_L(Q),
     --between the column whose index is "low" and the column whose index is "high"
     n:=#L;
     r:=high-low-n;
     V:= mutableMatrix map(ZZ^(n+1),ZZ^(r+1), 0);
     apply(high-low+1, u->(
	       B:=bott(L,-(low+u));
	       if not B===0 then V_(n-B_1,u-(n-B_1))=B_2)
	  );
     matrix V
     )

bott(List,ZZ,ZZ):=(L,low,high)->(
     --produces the betti diagram of the tate resolution of the sheaf S_L(Q),
     --between the column whose index is "low" and the column whose index is "high"
     n:=#L;
     C := for u from low-n to high list (
	  B := bott(L,-u);
	  if B=!=0 then (B_1, u) => B_2 else null
	  );
     new CohomologyTally from select(C, k -> k =!= null)
     )

-- Test 18     
TEST ///
B1=bott({3,2,1},-10,10)
M=matrix"924,640,420,256,140,64,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0;
     	 0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0;
	 0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0;
	 0,0,0,0,0,0,0,0,0,20,64,140,256,420,640,924,1280,1716,2240,2860,3584"
M1=mat2cohom(M,-10)
assert(B1==M1)

B2=bott({0,0,0},-5,5)
M=matrix"35,20,10,4,1,0,0,0,0,0,0;
         0,0,0,0,0,0,0,0,0,0,0;
         0,0,0,0,0,0,0,0,0,0,0;
         0,0,0,0,0,1,4,10,20,35,56"
M2=mat2cohom(M,-5)
assert(B2==M2)


L={0,0,0}
A=apply(7,i-> bott(L,i))
AA={{{0, 0, 0, 0}, 0, 1}, 0, 0, 0, {{0, 0, 0, 0}, 3, 1}, {{1, 0, 0, 0}, 3, 4}, {{2, 0, 0, 0}, 3, 10}}
assert(A==AA)

for u from 0 to 6 do
print bott(L,u)

L={5,2,1,1}
A=apply(10,i->bott(L,i))
AA={{{5, 2, 1, 1, 0}, 0, 945}, {{4, 1, 0, 0, 0}, 0, 224}, 0, 0, {{3, 0, 0, 0, 0}, 2, 35}, 0, {{3, 1, 1, 0, 0}, 3, 126}, {{3, 2, 1, 0, 0}, 3, 280}, {{3, 3, 1, 0, 0}, 3, 315}, 0}
assert(A==AA)

for u from 0 to 10 do
print bott(L,u)
///

beginDocumentation()

document { Key => BoijSoederberg,
     Headline => "Betti diagram routines",
     EM "BoijSoederberg", " is a package designed to help with the investigation of 
     the Boij-Soederberg conjectures and theorems.  For the definitions and conjectures, see
     math.AC/0611081, \"Graded Betti numbers of Cohen-Macaulay modules and 
     the Multiplicity conjecture\", by Mats Boij, Jonas Soederberg.",
     PARA{},
     SUBSECTION "Manipulation of Betti diagrams",
     UL {
	  TO mat2betti,
	  TO (matrix,BettiTally,ZZ,ZZ),
	  TO lowestDegrees,
	  TO highestDegrees,
	  TO BettiTally,
	  TO makeCI
          },
     SUBSECTION "Pure Betti diagrams",
     UL {
	  TO pureBetti,
	  TO makePureBetti,
	  TO pureBettiDiagram,
	  TO makePureBettiDiagram,
	  TO isPure
	  },
     SUBSECTION "Cohomology tables",
     UL {
	  TO CohomologyTally,
	  TO pureCohomologyTable,
	  TO bott
	  },
     SUBSECTION "Decomposition into pure diagrams",
     UL {
	  TO (decompose,BettiTally),
	  TO decomposeBetti,
	  TO decomposeDegrees,
	  TO eliminateBetti,
	  TO isMassEliminate
	  },
     SUBSECTION "Three constructions for pure resolutions.  These routines provide the
     zero-th betti number given a degree sequence.",
     UL {
	  TO pureTwoInvariant,
	  TO pureWeyman,
	  TO pureCharFree,
	  TO pureAll
	  },
     SUBSECTION "Constructions often leading to pure resolutions",
     UL {
	  TO randomModule,
	  TO randomSocleModule
	  },
     SUBSECTION "Facet equation and the dot product between Betti diagrams and cohomology tables",
     UL {
	  TO facetEquation,
	  TO dotProduct,
	  TO supportFunctional,
	  TO (symbol*,BettiTally,CohomologyTally)
	  }
     }

document {
     Key => CohomologyTally,
     Headline => "cohomology table",
     "A ", TT "CohomologyTally", " is designed to hold cohomology dimensions 
     h^i(E(d-i)) in the i-th row and the d-th column of the table, for some sheaf or vector bundle E on P^n.  The initial motivation
     was to provide a nice visual display of this information.  However, some
     computations involving CohomologyTally are implemented."
     
     }
 
document {
     Key => BettiEliminationTally,
     Headline => "Betti elimination table",
     "A ", TT "Betti elimination table", " is designed to show the order 
     in which Betti numbers are eliminated by the Boij-Soederberg algorithm."
     
     } 

document { 
     Key => {(lowestDegrees,BettiTally),lowestDegrees},
     Headline => "list of lowest degree shifts",
     Usage => "lowestDegrees B",
     Inputs => {
	  "B"
	  },
     Outputs => {
	  List => "of lowest degree shifts occuring in B"
	  },
     EXAMPLE lines ///
     	  R = ZZ/101[a..e];
	  B = betti res ideal"ab,ac,bd,be,ae,cd,ce,a3,b3,c3,d3,e3"
	  B1 = lowestDegrees B
	  pureBettiDiagram B1
	  ///,
     SeeAlso => {highestDegrees,isPure}
     }

document { 
     Key => {(highestDegrees,BettiTally),highestDegrees},
     Headline => "list of highest degree shifts",
     Usage => "highestDegrees B",
     Inputs => {
	  "B"
	  },
     Outputs => {
	  List => "of highest degree shifts occuring in B"
	  },
     EXAMPLE lines ///
     	  R = ZZ/101[a..e];
	  B = betti res ideal"ab,ac,bd,be,ae,cd,ce,a3,b3,c3,d3,e3"
	  highestDegrees B
	  lowestDegrees B
	  ///,
     SeeAlso => {lowestDegrees,isPure}
     }

document { 
     Key => {(isPure,BettiTally),isPure},
     Headline => "is a Betti diagram pure?",
     Usage => "isBure B",
     Inputs => {
	  "B"
	  },
     Outputs => {
	  Boolean
	  },
     "A Betti diagram is pure if in each column there is exactly one entry.",
     EXAMPLE lines ///
     	  R = ZZ/101[a..e];
	  B = betti res ideal"ab,ac,bd,be,ae,cd,ce,a3,b3,c3,d3,e3"
	  B1 = pureBettiDiagram highestDegrees B
	  isPure B
	  isPure B1
	  ///,
     SeeAlso => {lowestDegrees, highestDegrees, pureBettiDiagram}
     }

document { 
     Key => {(pureBetti,List),pureBetti},
     Headline => "list of smallest integral Betti numbers corresponding to a degree sequence",
     Usage => "pureBetti L",
     Inputs => {
	  "L" => "of strictly increasing integers"
	  },
     Outputs => {
	  List => "a list of the minimal integral Betti numbers which satisfy the Herzog-Kuhl equations"
	  },
     "The numerator P(t) of the Hilbert function of a module whose free resolution has a pure resolution
     of type L has the form P(t) = b_0 t^(d_0) - b_1 t^(d_1) + ... + (-1)^c b_c t^(d_c), 
     where L = {d_0, ..., d_c}.  If (1-t)^c divides P(t), as in the case where the module has codimension c,
     then the b_0, ..., b_c are determined up to a unique scalar multiple.  This 
     routine returns the smallest positive integral solution of these (Herzog-Kuhl) equations.",
     EXAMPLE lines ///
     	  pureBetti{0,2,4,5}
	  pureBetti{0,3,4,5,6,7,10}
	  ///,
     Caveat => {},
     SeeAlso => {pureBettiDiagram}
     }
 
 document { 
     Key => {(makePureBetti,List),makePureBetti,[makePureBetti, TableEntries]},
     Headline => "list of Betti numbers corresponding to a degree sequence",
     Usage => "pureBetti L",
     Inputs => {
	  "L" => "of strictly increasing integers",
	  TableEntries => String => "proscribes the scaled versions of the entries of the pure diagram; options are LeastIntegerEntries, HerzogKuhl, and RealizationModules."
	  },
     Outputs => {
	  List => "a list of the Betti numbers which satisfy the Herzog-Kuhl equations"
	  },
     "The numerator P(t) of the Hilbert function of a module whose free resolution has a pure resolution
     of type L has the form P(t) = b_0 t^(d_0) - b_1 t^(d_1) + ... + (-1)^c b_c t^(d_c), 
     where L = {d_0, ..., d_c}.  If (1-t)^c divides P(t), as in the case where the module has codimension c,
     then the b_0, ..., b_c are determined up to a unique scalar multiple.  This 
     routine returns the smallest positive integral solution of these (Herzog-Kuhl) equations.",
     EXAMPLE lines ///
     	  makePureBetti{0,2,4,5}
	  makePureBetti({0,2,4,5},TableEntries => HerzogKuhl)
	  makePureBetti({0,2,4,5},TableEntries => RealizationModules)
	  makePureBetti{0,3,4,5,6,7,10}
	  makePureBetti({0,3,4,5,6,7,10},TableEntries => HerzogKuhl)
	  makePureBetti({0,3,4,5,6,7,10},TableEntries => RealizationModules)
	  ///,
     Caveat => {},
     SeeAlso => {pureBetti,makePureBettiDiagram}
     }

document { 
     Key => {(pureBettiDiagram,List),pureBettiDiagram},
     Headline => "pure Betti diagram given a list of degrees",
     Usage => "pureBettiDiagram L",
     Inputs => {
	  "L" => "of strictly increasing integers"
	  },
     Outputs => {
	  BettiTally => "containing the minimal integral Betti numbers which satisfy the Herzog-Kuhl equations"
	  },
     "See ", TO "pureBetti", " for a description of the Herzog-Kuhl equations.",
     EXAMPLE lines ///
     	  pureBettiDiagram{0,2,4,5}
	  pureBettiDiagram{0,3,4,5,6,7,10}
	  pureBettiDiagram{0,3,4,5,6,7,8,11}
	  ///,
     Caveat => {},
     SeeAlso => {pureBetti, betti}
     }
 
 document { 
     Key => {makePureBettiDiagram, (makePureBettiDiagram,List),[makePureBettiDiagram, TableEntries]},
     Headline => "makes a pure Betti diagram given a list of degrees",
     Usage => "makePureBettiDiagram L",
     Inputs => {
	  "L" => "of strictly increasing integers",
	  TableEntries => String => "proscribes the scaled versions of the entries of the pure diagram; options are LeastIntegerEntries, HerzogKuhl, and RealizationModules."
	  },
     Outputs => {
	  BettiTally => "containing the Betti numbers which satisfy the Herzog-Kuhl equations according to given options. Defults to the minimal integral Betti numberswhich satisfy the Herzog-Kuhl equations."
	  },
     "See ", TO "pureBetti", " for a description of the Herzog-Kuhl equations.",
     EXAMPLE lines ///
     	  makePureBettiDiagram{0,2,4,5}
	  makePureBettiDiagram({0,2,4,5}, TableEntries => HerzogKuhl)
	  makePureBettiDiagram{0,3,4,5,6,7,10}
	  makePureBettiDiagram({0,3,4,5,6,7,10}, TableEntries => RealizationModules)
	  makePureBettiDiagram{0,3,4,5,6,7,8,11}
	  makePureBettiDiagram({0,3,4,5,6,7,8,11}, TableEntries => HerzogKuhl)
	  makePureBettiDiagram({0,3,4,5,6,7,8,11}, TableEntries => RealizationModules)
	  ///,
     Caveat => {},
     SeeAlso => {LeastIntegerEntries, HerzogKuhl,RealizationModules}
     }



document { 
     Key => (decompose,BettiTally),
     Headline => "write a Betti diagram as a positive combination of pure integral diagrams",
     Usage => "decompose B",
     Inputs => {
	  "B" => "not necessarily Cohen-Macaulay"
	  },
     Outputs => {
	  Expression => "a positive combination of pure integral Betti diagrams"
	  },
     "This applies the algorithm implied by the Boij-Soederberg conjecture, and also works 
     even if the diagram does not corresponds to a Cohen-Macaulay module.",
     EXAMPLE lines ///
     	  R = ZZ/103[a,b,c]
	  I = ideal"a3,abc,b4,c4,b2c2"
	  B = betti res I
	  C = decompose B
	  ///,
     "Check that this really does sum to B:",
     EXAMPLE lines ///
     	  value C
     	  ///,
     "Note that the entries are displayed in a peculiar manner.  Let's lift this to the integers.",
     EXAMPLE lines ///
     	  lift(value C, ZZ)
	  B == oo
     	  ///,
     "Let's display the list of Betti diagrams in the decomposition, and also the list of multipliers.",
     EXAMPLE lines ///
     	  netList pack(3, apply(toList C, x -> x#1))
	  apply(toList C, first)
     	  ///,
     "Here is an example where the Betti diagram is not Cohen-Macaulay.",
     EXAMPLE lines ///
     	  R = ZZ/103[a,b,c]
	  I = ideal"a3,abc,b4,b2c2"
	  B = betti res I
	  C = decompose B
     	  ///,
     "The following example cannot be decomposed.  This means that there is no module with
     this Betti diagram.",
     EXAMPLE lines ///
     	  M = matrix"1,0,0,0;0,0,0,0;0,3,0,0;0,0,5,3"	  
	  B = mat2betti M
	  codim B
	  degree B
     	  try decompose B else "Betti diagram cannot exist"
	  pureBettiDiagram lowestDegrees B
     	  ///,
     SeeAlso => {decomposeBetti, decomposeDegrees, makePureBettiDiagram, betti, value, lift, toList, pack}
     }

document { 
     Key => {decomposeBetti,[decomposeBetti,TableEntries]},
     Headline => "write a Betti diagram as a positive combination of pure integral diagrams",
     Usage => "decomposeBetti B",
     Inputs => {
	  "B" => "not necessarily Cohen-Macaulay"
	  },
     Outputs => {
	  Expression => "a positive combination of pure integral Betti diagrams"
	  },
     "This applies the algorithm implied by the Boij-Soederberg conjecture, and also works 
     even if the diagram does not corresponds to a Cohen-Macaulay module.",
     EXAMPLE lines ///
     	  R = ZZ/103[a,b,c]
	  I = ideal"a3,abc,b4,c4,b2c2"
	  B = betti res I
	  decomposeBetti(B)
	  ///,
     "We can see what the pure diagrams should be using the Herzog-Kuhl equations from Boij-Soederberg's initial paper",
     EXAMPLE lines ///
     	 decomposeBetti(B,TableEntries => HerzogKuhl)
     	  ///,
     "And we can also see what the realization modules from the Eisenbud-Schreyer paper will be.",
     EXAMPLE lines ///
     	 decomposeBetti(B,TableEntries => RealizationModules)
	 ///,
     SeeAlso => {(decompose,BettiTally),decomposeDegrees}
     }

document {
    Key => TableEntries,
    Headline => "Set the convention for what kind of pure Betti diagrams to use in a decomposition.",
    "The possible options are LeastIntegerEntries, HerzogKuhl, and RealizationModules.",
    SeeAlso => {decomposeBetti}
    } 

document {
    Key => LeastIntegerEntries,
    Headline => "An argument for the option TableEntries",
    SeeAlso => {decomposeBetti,TableEntries}
    } 

document {
    Key => HerzogKuhl,
    Headline => "An argument for the option TableEntries",
    SeeAlso => {decomposeBetti,TableEntries}
    }

document {
    Key => RealizationModules,
    Headline => "An argument for the option TableEntries",
    SeeAlso => {decomposeBetti,TableEntries}
    }

document { 
     Key => decomposeDegrees,
     Headline => "Find the degree sequences of pure diagrams occuring in a Boij-Soederberg decomposition of B",
     Usage => "decomposeDegrees B",
     Inputs => {
	  "B" => "not necessarily Cohen-Macaulay"
	  },
     Outputs => {
	  Expression => "a positive combination of pure integral Betti diagrams"
	  },
     "This applies the algorithm implied by the Boij-Soederberg conjecture, and also works 
     even if the diagram does not corresponds to a Cohen-Macaulay module.",
     EXAMPLE lines ///
     	  R = ZZ/103[a,b,c]
	  I = ideal"a3,abc,b4,c4,b2c2"
	  B = betti res I
	  decomposeBetti(B)
	  ///,
     "We can see what the pure diagrams should be using the Herzog-Kuhl equations from Boij-Soederberg's initial paper",
     EXAMPLE lines ///
     	 decomposeBetti(B,TableEntries => HerzogKuhl)
     	  ///,
     "And we can also see what the realization modules from the Eisenbud-Schreyer paper will be.",
     EXAMPLE lines ///
     	 decomposeBetti(B,TableEntries => RealizationModules)
	 ///,
     SeeAlso => {(decompose,BettiTally),decomposeDegrees}
     }
 
document { 
     Key => {(mat2betti,Matrix,ZZ),(mat2betti,Matrix),mat2betti},
     Headline => "matrix to Betti diagram",
     Usage => "mat2betti(M,lowDegree)\nmat2betti M",
     Inputs => {
	  "M" => "A matrix of Betti numbers: each row corresponds to a slanted degree",
	  "lowDegree" => ZZ => "default value is 0"
	  },
     Outputs => {
	  BettiTally
	  },
     "Change from matrix representation to Betti diagram representation.",
     EXAMPLE lines ///
     	  M = matrix"1,0,0,0;0,3,0,0;0,0,5,3"
	  mat2betti M
	  mat2betti(M,3)
	  ///,
     SeeAlso => {(matrix,BettiTally)}
     }
 
document {
    Key => {makeCI},
    Headline => "Make the Betti diagram of a complete intersection ideal",
    Usage => "makeCI(degrees)",
    Inputs => {
	"degrees" => "A list of degrees of the forms generating the complete intersection ideal",
	},
    Outputs => {
	BettiTally
	}
    } 

document { 
     Key => {(matrix,BettiTally,ZZ,ZZ),(matrix,BettiTally,ZZ),(matrix,BettiTally)},
     Headline => "Betti diagram to matrix",
     Usage => "matrix B\nmatrix(B,lowDegree)\nmatrix(B,lowDegree,highDegree)",
     Inputs => {
	  "B" => BettiTally,
	  "lowDegree" => ZZ,
	  "highDegree" => ZZ
	  },
     Outputs => {
	  Matrix => "The Betti diagram as a matrix"
	  },
     "If either lowDegree or highDegree is not given, then they are inferred from the Betti
     diagram itself.  The result matrix has highDegree-lowDegree+1 rows, 
     corresponding to these (slanted) degrees. ",
     EXAMPLE lines ///
     	  B = pureBettiDiagram {0,1,4,7}
	  matrix B
	  matrix(B,-2)
	  matrix(B,-2,5)
	  ///,
     "This function is essentially the inverse of ", TO "mat2betti", ".",
     EXAMPLE lines ///
         R = ZZ/101[a..e];
     	 I = ideal borel monomialIdeal"abc,ad3,e4";
     	 B = betti res I
     	 C = matrix B
     	 B == mat2betti C
	 ///,
     "If the lowest degree of the matrix is not 0, then this information must be
     supplied in order to obtain the inverse operation.",
     EXAMPLE lines ///
     	  B = pureBettiDiagram {-2,0,1,2,5}
	  C = matrix B
	  mat2betti(C,-2)
     	  ///,
     Caveat => {"Currently, the error messages are not that illuminating.  
	  The [lowDegree, highDegree], if given, must be 
	  as large as the actual degree range"},
     SeeAlso => {mat2betti, lowestDegrees, highestDegrees, isPure, pureBettiDiagram}
     }

document { 
     Key => {pureCharFree,
	  (pureCharFree,List)},
     Headline => "first betti number of specific exact complex",
     Usage => "pureCharFree",
     Inputs => {
	  List => "a strictly increasing sequence of degrees"
	  },
     Outputs => {
	  ZZ => "The zero-th betti number of the corresponding pure
	  resolution construction"
	  }, 
     TT "pureCharFree", " corresponds to the construction in math.AC/0712.1843v2, \"Betti Numbers of
     Graded Modules and Cohomology of Vector Bundles\", Section 5.", 
     EXAMPLE lines ///
     	  L = {0,2,3,9}
	  B = pureBettiDiagram L
     	  pureCharFree L
     	  L1 = {0,3,4,6}
	  B1 = pureBettiDiagram L1
	  pureCharFree L1
	  ///,
     "Thus, for large enough multiples m, m*B occurs as the Betti diagram of a module from the pureCharFree construction",
     PARA{},
     "However, we can find B itself as the Betti diagram of a module:",
     EXAMPLE lines ///
     	  betti res randomSocleModule(L,1)
     	  betti res randomModule(L,1)
     	  betti res randomModule({0,6,7,9},1)

     	  betti res randomSocleModule(L1,1)
     	  betti res randomModule(L1,1)
     	  betti res randomModule({0,2,3,6},1)
	  betti res randomSocleModule({0,2,3,6},1)
     	  ///,
     SeeAlso => {pureAll, pureWeyman, pureTwoInvariant}
     }

document { 
     Key => {pureTwoInvariant,
	  (pureTwoInvariant,List)},
     Headline => "first betti number of specific exact complex",
     Usage => "pureTwoInvariant",
     Inputs => {
	  List => "a strictly increasing sequence of degrees"
	  },
     Outputs => {
	  ZZ => "The zero-th betti number of the corresponding pure
	  resolution construction"
	  },
     TO "pureTwoInvariant", " corresponds to the construction in math.AC/0709.1529v3 \"The Existence of Pure Free Resolutions\", Section 3.", 
     EXAMPLE lines ///
     	  L = {0,2,3,9}
	  B = pureBettiDiagram L
     	  pureTwoInvariant L 
     	  L1 = {0,4,5,7}
	  B1 = pureBettiDiagram L1
	  pureTwoInvariant L1
	  ///,
     "Thus, for large enough multiples m, m*B occurs as the Betti diagram of a module from the pureTwoInvariant construction",
     PARA{},
     "However, B itself occurs as the betti table of a module:",
     EXAMPLE lines ///
     	  betti res randomSocleModule(L,1)
     	  betti res randomModule(L,1)
     	  betti res randomModule({0,6,7,9},1)

     	  betti res randomSocleModule(L1,1)
     	  betti res randomModule(L1,1)
     	  betti res randomModule({0,2,3,7},1)
     	  betti res randomSocleModule({0,2,3,7},1)
     	  ///,
     SeeAlso => {pureAll, pureWeyman, pureCharFree}
     }

document { 
     Key => {pureWeyman,
	  (pureWeyman,List)},
     Headline => "first betti number of specific exact complex",
     Usage => "pureWeyman L",
     Inputs => {
	  List => "a strictly increasing sequence of degrees"
	  },
     Outputs => {
	  ZZ => "The zero-th betti number of the corresponding pure
	  resolution construction"
	  },
     TO "pureWeyman", " corresponds to the construction in math.AC/0709.1529v3 \"The Existence of Pure Free Resolutions\", Section 4.", 
     EXAMPLE lines ///
     	  L = {0,2,3,9}
	  B = pureBettiDiagram L
     	  pureWeyman L
	  
	  L1 = {0,3,5,6}
	  B1 = pureBettiDiagram L1
	  pureWeyman L1
	  
	  ///,
     "Thus, for large enough multiples m, m*B occurs as the Betti diagram of a module in the Weyman construction",
     PARA{},
     "However, B itself occurs for some modules:",
     EXAMPLE lines ///
     	  betti res randomSocleModule(L,1)
     	  betti res randomModule(L,1)
     	  betti res randomModule({0,6,7,9},1)

     	  betti res randomSocleModule(L1,1)
     	  betti res randomModule(L1,1)
     	  betti res randomModule({0,1,3,6},1)
     	  betti res randomSocleModule({0,1,3,6},1)     	  
	  ///,
     SeeAlso => {pureAll, pureCharFree, pureTwoInvariant}
     }

document { 
     Key => {pureAll,
	  (pureAll, List)},
     Headline => "Vector of first betti number of our three specific exact complexes",
     Usage => "pureAll",
     Inputs => {
	  List => "a strictly increasing sequence of degrees"
	  },
     Outputs => {
	  ZZ => "The vector of zero-th betti numbers of the three corresponding pure
	  resolution construction."
	  },
     TO "pureAll", " returns all three numbers at one time.",
     EXAMPLE lines ///
     	  L = {0,2,3,9}
	  B = pureBettiDiagram L
     	  pureCharFree L
     	  pureTwoInvariant L 
	  pureWeyman L
	  pureAll L
	  gcd pureAll L
	  ///,
     "Thus, for large enough multiples m, m*B occurs as the Betti diagram of a module.",
     PARA{},
     "However, B itself occurs:",
     EXAMPLE lines ///
     	  betti res randomSocleModule(L,1)
     	  betti res randomModule(L,1)
     	  betti res randomModule({0,6,7,9},1)
     	  ///,
     SeeAlso => {pureWeyman, pureTwoInvariant, pureCharFree}
     }


document { 
     Key => {(randomModule,List,ZZ),randomModule},
     Headline => "module with random relations in prescribed degrees",
     Usage => "randomModule(L,m)",
     Inputs => {
	  "L" => "a strictly increasing degree sequence of integers",
	  "m",
	  CoefficientRing => "The base field for the resulting module"
	  },
     Outputs => {
	  Module => "randomly generated having m b_0 generators in degree L_0 and m b_1
	  relations in degree L_1, where b = pureBetti L"
	  },
     EXAMPLE lines ///
          L={0,4,9,10}
	  B = pureBetti L
	  betti res randomModule(L,1)
	  betti res randomModule(L,2)
	  betti res randomModule(L,2, CoefficientRing=>ZZ/5)
	  ///,
     SeeAlso => {randomSocleModule, pureBetti}
     }

document { 
     Key => {(randomSocleModule,List,ZZ),randomSocleModule},
     Headline => "random finite length module with prescribed number of socle elements in single degree",
     Usage => "randomSocleModule(L,m)",
     Inputs => {
	  "L" => "a strictly increasing degree sequence of integers",
	  "m",
	  CoefficientRing => "The base field for the resulting module"
	  },
     Outputs => {
	  Module => "randomly generated having m b_c socle generators in degree L_c and m b_0
	  generators, where b = pureBetti L, and c = length L"
	  },
     "There are many cases where these produce pure resolutions of the minimal size.",
     EXAMPLE lines ///
          L={0,2,3,7}
	  B = pureBetti L
	  betti res randomSocleModule(L,1)
	  betti res randomModule(L,1)
	  ///,
     PARA{},
     "The method used is roughly the following:
     Given a strictly increasing degree sequence L and a number of generators m,
     this routine produces a generic module of finite length with the 
     m generators and number of socle elements  and regularity corresponding
     to the pure resolution with degree sequence L. The module is constructed
     by taking a certain number of generic elements inside an appropriate direct
     sum of copies of a zero-dimensional complete intersection. We use the fact
     that in a polynomial ring in c variables, 
     modulo the r+1 st power of each variable, the part of
     generated in degree (c-1)r looks like the part of the injective hull
     of the residue class field generated in degree -r.",
     SeeAlso => {randomModule, pureBetti}
     }

document {
	Key => {(pureCohomologyTable,List,ZZ,ZZ),pureCohomologyTable},
	Headline => "pure cohomology table given zeros of Hilbert polynomial",
	Usage => "pureCohomologyTable(L,lo,hi)",
	Inputs => { "L" => " a list of distinct integers",
	     "lo" => "the leftmost degree of the table",
	     "hi" => "the rightmost degree of the table"
	     },
	Outputs => {"the cohomology table, truncated at the given degrees "},
	"Given a list of distinct integers this function produces a truncated cohomology table for a 
	supernatural vector bundle with root sequence L. ",
	EXAMPLE lines ///
	   pureCohomologyTable({-3,-2,0},-5,5)
     	///,
	SeeAlso => {bott}
	}

document { 
     Key => bott,
     Headline => "cohomology of Schur functors of tautological bundle on P^n",
     "Given a weakly decreasing sequence of integers L and an integer u, bott(L,u) will 
     perform a cohomology calculation for a vector bundle E=O(-u)\\tensor S_L(Q), on P^n, producing
     another weakly decreasing sequence of integers corresponding to a partition M, a number i such that
     H^i(E)=S_M(V) and the rank of this module.",
     PARA{},
     "The function bott(L,u,v) on a list of weakly decreasing integers L and integers u,v will compute 
     our entire cohomology table for our Schur Functor between degrees u and v.",
     }

document { 
     Key => (bott,List,ZZ),
     Headline => "cohomology of Schur functor of tautological bundle on P^n",
     Usage => "bott(L,u)",
     Inputs => {
	  "L" => "a non-increasing sequence of integers",
	  "u"
	  },
     Outputs => {
	  List => "(List, ZZ, ZZ) or the integer 0.  See below for details."
	  },
     "Given a weakly decreasing list of integers L of length n and an integer u,
     uses Bott's algorithm to compute the cohomology of the vector bundle 
      E=O(-u) \\tensor S_L(Q), on P^n = PP(V)
     where Q is the tautological rank n quotient bundle in the sequence 
        0--> O(-1) --> O^(n+1) --> Q -->0
     and S_L(Q) is the Schur functor with the convention S_(d,0..0) = Sym_d, S_(1,1,1) = \\wedge^3 etc.
     Returns either 0, if all cohomology is zero,
     or a list of three elements: A weakly decreasing list of n+1 integers M;
     a number i such that H^i(E)=S_M(V); and 
     the rank of this module.  For more information on how the partition M is constructed, see
     math.AC/0709.1529v3, \"The Existence of Pure Free Resolutions\", section 3.",
     PARA{},
     "For example, on P^3, E = S_3(Q) has H^0(S_3(Q)) = S_3(kk^4) = kk^20.",
     EXAMPLE lines ///
     	  bott({3,0,0},0)
	  ///,
     "H^*(E(-1)) = H^*(E(-2)) = 0, and H^2(E(-3)) == S_2(kk^4) == kk^10.",
     EXAMPLE lines ///
	  bott({3,0,0},1)
	  bott({3,0,0},2)
     	  bott({3,0,0},3)
	  bott({2,1,0},0)
     	  ///,	  
     SeeAlso => {(bott,List,ZZ,ZZ),pureCohomologyTable}
     }

document { 
     Key => (bott,List,ZZ,ZZ),
     Headline => "cohomology table of Schur functor of tautolgical bundle on P^n",
     Usage => "bott(L,lowDegree,highDegree)",
     Inputs => {
	  "L" => "a non-increasing sequence of integers",
	  "lowDegree",
	  "highDegree"
	  },
     Outputs => {
	  CohomologyTally
	  },
     "Produces a ", TO CohomologyTally, " of the vector bundle ", TT "S_L(Q)", ",
     between the column whose index is ", TT "lowDegree", 
     " and the column whose index is ", TT "highDegree", ".
     See ", TO (bott,List,ZZ), " for the definition of ", TT "Q", ".",
     EXAMPLE lines ///
     	  C1 = bott({3,2,1},-10,10)
	  C2 = pureCohomologyTable({-2,-4,-6},-10,10) 
	  C1 == 4 * C2
	  ///,
     SeeAlso => {(bott,List,ZZ),pureCohomologyTable}
     }

document {
	Key => {(facetEquation,List,ZZ,ZZ,ZZ),facetEquation},
	Headline => "The upper facet equation corresponding to (L,i)",
	Usage => "facetEquation(L,i,lodeg,hideg)",
	Inputs => { "L" => "the degree sequence, a strictly ascending list of integers",
	     "i" => "an index in range 0..#L-2, such that L_(i+1) == L_i + 2",
	     "lodeg" => "the leftmost degree of the table",
	     "hideg" => "the rightmost degree of the table"
	     },
	Outputs => {{"A (hideg-lodeg+1) by #L matrix over ZZ whose rows correspond to slanted degrees lodeg .. hideg, such that the
		  dot product of this matrix with any betti diagram of any finite length module
		  is >= 0."}},
	"The (entry by entry) ", TO2(dotProduct, "dot product"), " of this matrix will be >= 0 for every minimal free resolution.
	Of course, the converse does not hold!",
	EXAMPLE lines ///
	   d  = {0,2,3,6,7,9}
	   de = {0,2,4,6,7,9}
	   e  = {0,3,4,6,7,9}
	   B1 = pureBettiDiagram d
	   B2 = pureBettiDiagram de
	   B3 = pureBettiDiagram e
	   C = facetEquation(de,1,0,6)
     	   dotProduct(C,B1)
     	   dotProduct(C,B2)
     	   dotProduct(C,B3)
     	///,
	"The following example is from Eisenbud and Schreyer,
	math.AC/0712.1843v2, example 2.4.  Notice that the notation here
	differs slightly from theirs.  In both cases i refers to the index,
	but in Macaulay2, the first element of a list has index 0.
	hence in this example, i is 3 and not 4,
	as in the example in the paper.",
	EXAMPLE lines ///
	   d = {-4,-3,0,2,3,6,7,9}
	   de = {-4,-3,0,2,4,6,7,9}
	   e = {-4,-3,0,3,4,6,7,9}
	   pureBettiDiagram d
	   pureBettiDiagram de
	   C = facetEquation(de,3,-6,3)
	///,
	"Let's check that this is zero on the appropriate pure diagrams, and positive on 
	the one corresponding to de:",
	EXAMPLE lines ///
	   dotProduct(C,-6,pureBettiDiagram d)
	   dotProduct(C,-6,pureBettiDiagram de)
	   dotProduct(C,-6,pureBettiDiagram e)
	///,
	SeeAlso => {dotProduct, pureBettiDiagram}
	}

-- Test 19
TEST ///
d={0,2,4}
facetEquation(d,0,-1,3)

d={0,3,5,7}
facetEquation(d,2,-3,4) -- OK
assert try facetEquation(d,2,0,3) else true -- gives error msg.

d={0,2,3}
assert try facetEquation(d,0,0,0) else true --gives error msg

d={0,2,3}
facetEquation(d,0,-1,1)
facetEquation(d,0,0,2)
d={0,2,4}
facetEquation(d,1,-2,2)

d={1,3,4,6,7}
facetEquation(d,2,1,3)
pureBettiDiagram d
d={5,7,9,11}
facetEquation(d,1,5,8)
facetEquation(d,1,0,8)
d={5,7,9,11}
facetEquation(d,2,0,12)
///

-- Test 20
TEST ///
d={1,3,4,5,7}
e={1,3,5,6,7}
de = {1,3,4,6,7}
B = pureBettiDiagram d
Be = pureBettiDiagram e
Bde = pureBettiDiagram de
A=facetEquation(de,2,1,3)
assert(dotProduct(A,1,B) == 0)
assert(dotProduct(A,1,Be) == 0)
assert(dotProduct(A,1,Bde) == 180)

A=facetEquation(de,2,-5,3)
assert(dotProduct(A,-5,B) == 0)
assert(dotProduct(A,-5,Be) == 0)
assert(dotProduct(A,-5,Bde) == 180)
///

document { 
     Key =>  {dotProduct,(dotProduct,Matrix,ZZ,BettiTally),(dotProduct,Matrix,BettiTally),(dotProduct,BettiTally,BettiTally),(dotProduct,Matrix,Matrix)},
     Headline => "entry by entry dot product of two Betti diagrams",
     Usage => "dotProduct(M,lowestDeg,B)\ndotProduct(M,B)\ndotProduct(B,C)\ndotProduct(M,N)",
     Inputs => {
	  "M" => Matrix,
	  "N" => Matrix,
	  "lowestDeg" => ZZ,
	  "B" => BettiTally,
	  "C" => BettiTally
	  },
     "In the first version (M, lowestDeg) refers to
     mat2betti(M, lowestDeg), and in the second version (M,B) refers to (M,0,B).",
     Outputs => {
	  ZZ => "the entry by entry dot product"
	  },
     EXAMPLE lines ///
     	  d = {0,1,3,4}
     	  M = facetEquation(d,1,-5,5)
	  B = pureBettiDiagram d
	  dotProduct(M,-5,B)
	  
	  A = matrix"1,1,0; 0,1,1; 0,1,1"
	  B = matrix"0,1,-2;0,0,0;0,0,0"
	  dotProduct(A, B)
	  A1 = mat2betti A
	  B1 = mat2betti B
	  dotProduct(A1, B1)
	  dotProduct(A, 0, B1)
	  dotProduct(A, B1)
	  ///,
     SeeAlso => {facetEquation, pureBettiDiagram}
     }
 
 document {
    Key => {isMassEliminate, (isMassEliminate,BettiTally)},
    Headline => "determines whether the Boij-Soederberg decomposition algorithm eliminates multiple Betti numbers at the same time",
    Usage => "isMassEliminate(B)",
    Inputs => {
	"B", BettiTally => "a Betti diagram"
	},
    Outputs => {Boolean => "True or false if..."},
    EXAMPLE lines ///
    	  R = ZZ/8821[x,y,z,w]
	  I = ideal(x^2,y^2,z^4,w^8)
	  B = betti res I
	  isMassEliminate(B)
	  J = ideal(x^4,y^5,z^7,w^9)
          C = betti res J
	  isMassEliminate(C)
	///
	}
    
    
document { 
     Key => {eliminateBetti, (eliminateBetti,BettiTally), (eliminateBetti,Ideal)},
     Headline => "elimination table for a Betti diagram",
     Usage => "eliminateBetti(B)\neliminateBetti(I)",
     Inputs => {
	  "B", BettiTally => "a Betti diagram",
	  "I", Ideal => "an Ideal"
	  },
     Outputs => {
	  BettiEliminationTally => "The elimination table of B or of the Betti table of R/I"
	  },
     EXAMPLE lines ///
          R = ZZ/8821[x,y,z,w]
	  I = ideal(x,y^2,z^4,w^8)
	  B = betti res I
	  eliminateBetti(B)
	  ///,
     SeeAlso => {BettiEliminationTally,isMassEliminate}
     }
 
document { --- option names for eliminateBetti
    Key => {EliminationSequence},
    Headline => "option for eliminateBetti"
    }
 

end

restart
loadPackage "BoijSoederberg"
installPackage "BoijSoederberg"
installPackage("BoijSoederberg", RerunExamples=>true)
viewHelp BoijSoederberg
check BoijSoederberg

C = new CohomologyTally from {(0,0) => 2, (0,1) => 5, (0,2) => 8, (2, -3) => 1, (2, -4) => 3}
M = matrix"3,1,0,0,0;0,0,0,0,0;0,0,2,5,8"
mat2cohom(M,-2)
debug BoijSoederberg

B = pureBettiDiagram {0,2,3,4}
C = pureCohomologyTable({1,0,0},-5,-5)
B * C

pureCohomologyTable({1,2,3},-5,5,old)
C = pureCohomologyTable({1,2,3},-5,5)
C(1) + C(-1)
pureCohomologyTable({-3,-1,0},-7,7,old)
pureCohomologyTable({-3,-1,0},-7,7)
pureCohomologyTable({-2,0,2},-7,7)
bott({2,1,0},-7,7)
4 * o15
o15 + o15 + o15 + o15 - oo
bott({5,3,1},-10,10,old)


bott({3,2,1},-7,7,old)
bott({3,2,1},-7,7)
restart
--load "/Users/mike/local/conferences/2008-mrc-june-snowbird/boij-soederberg/boijSoederberg-orig.m2"
load "/Users/mike/local/conferences/2008-mrc-june-snowbird/boij-soederberg/bs.m2"

document { 
     Key => {},
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     EXAMPLE lines ///
	  ///,
     Caveat => {},
     SeeAlso => {}
     }

document { 
     Key => {},
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     EXAMPLE lines ///
	  ///,
     Caveat => {},
     SeeAlso => {}
     }

restart
loadPackage "BGG"
R = ZZ/101[x_0..x_4]
phi = map(R, coefficientRing R)
E = setupBGG(phi,e)

C = res comodule (ideal vars R)^2
M = coker(C.dd_2)
betti res M
tateResolution(presentation M, -5,5)
sheafCohomology(presentation M, E, -5,5)
setupBGG

--
-- Branden's tests
restart
loadPackage"BoijSoederberg"
check BoijSoederberg


restart
uninstallPackage "BoijSoederberg"
restart
installPackage "BoijSoederberg"
check BoijSoederberg
viewHelp BoijSoederberg

makeCI{2,2,3}
monoid[vars(0..2)]
QQ(monoid[vars(0..2)])

S = ZZ/499[monoid[vars(0..2)]]
-- test 1
restart
loadPackage"BoijSoederberg"
M=matrix "1,0,0,0;
        0,4,4,1"
B=mat2betti M

C1=decomposeBetti B
L=set apply(toList C1,x->x#1)
m1=mat2betti matrix "1,0,0,0;
                     0,6,8,3"
m2=mat2betti matrix "1,0,0;
                     0,3,2"
M'=set{m1,m2}
assert(L===M')

C2=decomposeBetti(B, TableEntries => HerzogKuhl )
L2=set apply(toList C2,x->x#1)
m1c2=mat2betti matrix "1/24,0,0,0;
                     0,1/4,1/3,1/8"
m2c2=mat2betti matrix "1/6,0,0;
                     0,1/2,1/3"		     
Mc2=set{m1c2,m2c2}
assert(L2===Mc2)

C3=decomposeBetti (B, TableEntries => RealizationModules )
L=set apply(toList C3,x->x#1)
m1=mat2betti matrix "1,0,0,0;
                     0,6,8,3"
m2=mat2betti matrix "1,0,0;
                     0,3,2"		     
M'=set{m1,m2}
assert(L===M')


M=matrix "1,0,0,0;
     	  0,5,5,1;
	  0,0,1,1"		    
B=mat2betti M
C=decompose B
C=decomposeBetti B
C=decomposeBetti(B, TableEntries => HerzogKuhl )
C=decomposeBetti(B, TableEntries => RealizationModules )
L=set apply(toList C,x->x#1)
m1=mat2betti matrix "1,0,0,0;
                     0,6,8,3"
m2=mat2betti matrix "1,0,0,0;
                     0,5,5,0;
		     0,0,0,1"
m3=mat2betti matrix "3,0,0,0;
                     0,10,0,0;
		     0,0,15,8"
M'=set{m1,m2,m3}
assert(L===M')

M=matrix"1,0,0,0;
     	 0,2,0,0;
	 0,1,3,1"
B=mat2betti M
C=decompose B
L=set apply(toList C,x->x#1)
m1=mat2betti matrix"1,0,0;
     	  0,2,0;
	  0,0,1"
m2=mat2betti matrix"3,0,0,0;
     	  0,10,0,0;
	  0,0,15,8"
m3=mat2betti matrix"1,0,0;
     	  0,0,0;
	  0,4,3"
M'=set{m1,m2,m3}
assert(L===M')


-- end Branden's tests
--

-- Courtney's tests
--

-- Test 21
TEST ///
R = QQ[x,y,z,w]
I = ideal(x^2,y^4,z^5,w^7)
B = makeCI(2,4,5,7)
C = betti res (R^1/I)
assert(B===C)

J = ideal(x^4,y^5,z^7,w^9)
D = makeCI(4,5,7,9)
E = betti res(R^1/J)
assert(D===E)

assert(isMassEliminate(E)===true)
///

-- end Courtney's tests
--