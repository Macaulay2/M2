-- this package is a Macaulay2 implementation of the Suffix Tree as given
-- in Amir et.al.  It is used for debugging and prototyping the C++ implementation
-- of this data structure in e/NCAlgebras/SuffixTree.{hpp,cpp}

newPackage("SuffixTrees",
     Headline => "Data Type for a Suffix Tree as laid out by Amir et.al.",
     Version => "0.1",
     Date => "Jan 22, 2021",
    Authors => {{Name => "Frank Moore", 
	   Email => "moorewf@wfu.edu",
	   HomePage => "https://math.wfu.edu/moore"},
	{Name => "Mike Stillman",  
            Email => "mike@math.cornell.edu", 
            HomePage => "http://www.math.cornell.edu/~mike"}
        },
     DebuggingMode => false,
     Keywords => {"Noncommutative Algebra"}
     )

protect label
protect arcLabel
protect isFullPattern
protect patternLeafCount
protect suffixLink
protect children
protect root
protect wordList
protect IncrementLeafCount

SuffixTree = new Type of MutableHashTable
SuffixTreeNode = new Type of MutableHashTable

nullTreeNode = new SuffixTreeNode from hashTable {}

suffixTreeNode = method()
suffixTreeNode (SuffixTreeNode, List, Boolean) := (par, w, b) -> (
   --- a constructor for the SuffixTreeNode type.  Takes the parent and the
   --- arc label as input, and also places the new node in the children of the parent.
   v := new SuffixTreeNode from hashTable {(symbol parent) => par,
                                           (symbol children) => new MutableHashTable from {},
       	       	                      	   (symbol suffixLink) => nullTreeNode,
				      	   (symbol arcLabel) => w,
					   (symbol patternLeafCount) => 0,
				      	   (symbol isFullPattern) => b,
				      	   (symbol label) => par.label | w};
   par.children#w = v;
   v
)

net SuffixTreeNode := n -> if n === nullTreeNode then "NullTreeNode" else (net "SuffixTreeNode with label " | net n.label | " and edge label " | net n.arcLabel)

Symbol == ZZ := (a,n) -> false;
ZZ == Symbol := (n,a) -> false;
isZZ = f -> class f === ZZ

suffixTree = method()
suffixTree List := words -> (
   --- wordList should be a list of lists of symbols, which represent the noncommutative
   --- monomials for which we are building the suffix table.
   rootNode := new SuffixTreeNode from hashTable {(symbol parent) => nullTreeNode,
        	                                  (symbol children) => new MutableHashTable from {},
					      	  (symbol suffixLink) => nullTreeNode,
					      	  (symbol arcLabel) => {},
						  (symbol patternLeafCount) => 0,
				      	      	  (symbol isFullPattern) => false,
					      	  (symbol label => {})};
   tree := new SuffixTree from hashTable {(symbol root) => rootNode,
                                          (symbol wordList) => {}};
   rightOverlaps := flatten apply(words, w -> suffixTreeInsert(tree, w));
   (tree,rightOverlaps)
)
				  
-- the root node is the unique node in the tree with nullTreeNode as parent
isRoot = method()
isRoot SuffixTreeNode := n -> n.parent == nullTreeNode;

suffixTreeInsert = method()
suffixTreeInsert (SuffixTree, List) := (tree, w) -> (
   -- PURPOSE : Insert suffixes into tree
   -- INPUT   : suffix tree, and a word whose suffixes are to be inserted
   -- OUTPUT  : nothing
   wordNum := #(tree.wordList);
   tree.wordList = append(tree.wordList,w);
   rightOverlaps := {};
   s := w | {wordNum};
   v := tree.root;
   isFullPattern := true;
   while s != {} do (
      (newv,roRoot,newLocus) := suffixTreeInsertWorker(tree,v,s,isFullPattern);
      v = newv;
      if roRoot != {} then (
	 tempRos := apply(patternLeaves(first roRoot), pl -> (last newLocus.label,pl#0,pl#1));
	 -- XXXX
	 tempRos = apply(tempRos, ro -> (tree.wordList#(ro#0),#(tree.wordList#(ro#0)) - #(ro#1),tree.wordList#(ro#2)));
	 rightOverlaps = rightOverlaps | tempRos;
      );
      if v =!= tree.root and #s == 2 then v.suffixLink = tree.root;
      s = drop(s,1);
      isFullPattern = false;
   );
   rightOverlaps
)

suffixTreeInsertWorker = method()
suffixTreeInsertWorker (SuffixTree,SuffixTreeNode, List, Boolean) :=  (tree, v, s, isFullPattern) -> (
   -- PURPOSE : Worker function for suffixTreeInsert
   -- INPUT   : v is the locus of previous head, and s is the suffix to be inserted
   -- OUTPUT  : locus v' of head of s
   if v === tree.root then return suffixTreeStepD(v,
                                                  s,
		     		                  isFullPattern);
   if v.parent === tree.root then return suffixTreeStepC(v,
                                                         tree.root,
							 drop(v.arcLabel,1),
							 s,
							 isFullPattern);
   return suffixTreeStepC(v,
                          v.parent.suffixLink,
			  v.arcLabel,
			  s,
			  isFullPattern);
)

--suffixTreeStepC = method()
--- too many arguments to be a method
suffixTreeStepC = (v,x,beta,s,isFullPattern) -> (
   --- Carries out step C in the algorithm.  This amounts to computing (and building,
   --- if necessary) the suffix link of v.  This function can call the Step D code
   --- as well (see the algorithm in the paper by Amir, et.al.)
   (f,betaHat) := extendedLocus(x,beta);
   if #(f.arcLabel) == #betaHat then (
      --- in this case, f is in fact the locus of beta
      v.suffixLink = f;
      return suffixTreeStepD(f,drop(s,#(f.label)),isFullPattern);
   );
   --- in this case, f is the extended locus of beta.  We need to 
   --- split the arc from f to its parent and insert a node d and a child
   --- w which will be the locus of beta
   d := splitArc(f,betaHat);
   --- set the patternLeafCount of d, and increase if necessary (if s and/or f are pattern leaves)
   d.patternLeafCount = f.patternLeafCount + (if isFullPattern then 1 else 0) + (if f.isFullPattern then 1 else 0);
   --- add in the locus of s
   w := suffixTreeNode(d,drop(s,#(d.label)),isFullPattern);
   --- set the suffix link of v to d
   v.suffixLink = d;
   if #(w.label) == 1 then (d,{f},w) else (d,{},w)
)

suffixTreeStepD = method()
suffixTreeStepD (SuffixTreeNode, List, Boolean) := (y,s,isFullPattern) -> (
   --- Carries out step D in the algorithm.  This amounts to constructing
   --- the locus of the head of s (which is not yet known before calling this function).
   --- find the contracted locus of s, starting from the node y
   (newy,f,pre) := contractedLocus(y,s,IncrementLeafCount => isFullPattern);
   -- drop the letters from s along the path traversed from y to newy
   s = drop(s,#(newy.label) - #(y.label));
   y = newy;
   if f === nullTreeNode then (
      -- in this case, there is no common prefix of a label of any child of y and s
      -- so just create a leaf immediately.
      v := suffixTreeNode(y,s,isFullPattern);
      return if y.label != {} and #(v.arcLabel) == 1 then (y,{y},v) else (y,{},v);      
   );
   --- in this case, f is the extended locus of s.  We need to split the arc from y to f   
   p := splitArc(f,pre);
   --- update the patternLeafCount of p if necessary
   p.patternLeafCount = f.patternLeafCount + (if isFullPattern then 1 else 0) + (if f.isFullPattern then 1 else 0);
   --- drop common prefix
   s = drop(s,#pre);
   --- create new leaf under p for s
   w := suffixTreeNode(p,s,isFullPattern);
   --- return overlap and head information
   if #s == 1 then (p,{p},w) else (p,{},w)
)

splitArc = method()
splitArc (SuffixTreeNode, List) := (f,betaHat) -> (
   --- Here, betaHat is the prefix of f.arcLabel.  We split the arc from
   --- f to its parent by inserting a new internal node with arc label betaHat,
   --- making sure to set the parent of f to this new node.
   p := f.parent;
   d := suffixTreeNode(p,betaHat,false);
   remove(p.children,f.arcLabel);
   f.arcLabel = drop(f.arcLabel,#betaHat);
   d.children#(f.arcLabel) = f;
   f.parent = d;
   d   
)

contractedLocus = method(Options => {IncrementLeafCount => false})
contractedLocus (SuffixTreeNode, List) := opts -> (y,s) -> (
   --- s is a suffix not yet in the table.  This function finds 
   --- the locus of the longest prefix of s whose locus exists.
   --- The search starts at y, and moves down the tree according to the
   --- string s.  The return value is the contracted locus (also called y),
   --- and a node f which is either a child of y sharing a prefix pre with s - y.label,
   --- or f is nullTreeNode if no such child exists.
   c := if opts#IncrementLeafCount then 1 else 0;
   y.patternLeafCount = y.patternLeafCount + c;
   (f,pre) := findMatch(y,s);
   while f =!= nullTreeNode and pre == f.arcLabel do (
      y = f;
      s = drop(s,#pre);
      y.patternLeafCount = y.patternLeafCount + c;
      (f,pre) = findMatch(y,s);
   );
   (y,f,pre)
)

isPrefix = (a,b) -> take(b,#a) == a

extendedLocus = method()
extendedLocus (SuffixTreeNode, List) := (x,beta) -> (
   --- For this function to work, there must be a path starting from x with
   --- beta as a prefix (See e.g. Lemma 1 in Amir, et.al.)
   --- This function finds the locus of the shortest word that has beta as a prefix.
   --- it returns this locus, together with the prefix that needs to be split (if necessary)
   --- if beta is empty, then simply return (x,beta) since x is the extended locus
   if beta == {} then return (x,beta);
   betaHat := beta;
   (f,pre) := findMatch(x,betaHat);
   while #(f.arcLabel) < #betaHat do (
      x = f;
      betaHat = drop(betaHat,#(f.arcLabel));
      (f,pre) = findMatch(x,betaHat);
   );
   (f,betaHat)
)

findMatch = method()
findMatch (SuffixTreeNode, List) := (y,s) -> (
  -- PURPOSE : Find an arc from y to a child f whose label shares a prefix with s           
  -- INPUT   : Node y and a list s.
  -- OUTPUT  : Node f, list pre 
  -- return nullTreeNode if no match is found, i.e. the empty prefix is the only shared prefix with any
  -- child of y
  f := nullTreeNode;
  pre := {};
  for kv in pairs (y.children) do (
     (key,val) := kv;
     pre = sharePrefix(key,s);
     if pre != {} then (
        f = val;
	break;
     );
  );
  (f,pre)
)

sharePrefix = method()
sharePrefix (List,List) := (s,t) -> (
   -- PURPOSE : Return the longest shared prefix of s and t
   -- INPUT   : Lists s and t
   -- OUTPUT  : List that s and t share as prefix
   i := 0;
   while (i < min(#s,#t) and s#i == t#i) do i = i + 1;
   take(s,i)
)

patternLeaves = method()
patternLeaves SuffixTreeNode := v -> (
   --- This function returns all pattern leaves of v
   if #v.children == 0 then return {};
   apply(flatten patternLeavesWorker v, x -> (v.label,last x.label))
)

patternLeavesWorker = method()
patternLeavesWorker SuffixTreeNode := v -> (
   if #v.children == 0 then return (if v.isFullPattern then {v} else {});
   if v.patternLeafCount == 0 then return {};
   flatten apply(values v.children, x -> patternLeavesWorker x)
)

allLeaves = method()
allLeaves SuffixTreeNode := v -> (
   --- This function returns all leaves of v
   if #v.children == 0 then return {(v.label,v.label)};
   apply(flatten allLeavesWorker v, x -> (v.label,x.label))
)

allLeavesWorker = method()
allLeavesWorker SuffixTreeNode := v -> (
   if #v.children == 0 then return {v};
   flatten apply(values v.children, x -> allLeavesWorker x)
)

suffixTreeFirstSubword = method()
suffixTreeFirstSubword (SuffixTree, List) := (tree, s) -> (
   cLocus := tree.root;
   beta := {};
   subwords := {};
   initialS := s;
   pos := 0;
   while (s != {}) do (
      (newcLocus,newbeta,leaf,wasPattern) := suffixTreeSubwordsWorker(tree,cLocus,beta,s);
      --- this version only returns the first subword
      if wasPattern then return {(newcLocus.label | newbeta,pos,initialS)};
      pos = pos + 1;
      s = drop(s,1);
      cLocus = newcLocus;
      beta = newbeta;
   );
   subwords
)

suffixTreeSubwords = method()
suffixTreeSubwords (SuffixTree, List) := (tree, s) -> (
   --- This function finds all occurrences in s of the words in the dictionary 
   --- The return value is a list of pairs of integers (word, position, s)
   cLocus := tree.root;
   beta := {};
   subwords := {};
   initialS := s;
   pos := 0;
   while (s != {}) do (
      (newcLocus,newbeta,leaf,wasPattern) := suffixTreeSubwordsWorker(tree,cLocus,beta,s);
      if (wasPattern and isPrefix(drop(leaf.label,-1),s)) then
      (
	  subwords = subwords | {(newcLocus.label | newbeta,pos,initialS)};
      );
      pos = pos + 1;
      beta = newbeta;
      cLocus = newcLocus;
      s = drop(s,1);
   );
   subwords
)

suffixTreeSubwordsWorker = method()
suffixTreeSubwordsWorker (SuffixTree,SuffixTreeNode, List, List) := (tree,cLocus, beta, s) -> (
   --- finds subwords for a single s, based on (cLocus,beta) of previous
   if cLocus === tree.root then return suffixTreeSubwordsStepD(tree.root,s);
   suffixTreeSubwordsStepC(cLocus.suffixLink,beta,s)
)

suffixTreeSubwordsStepC = method()
suffixTreeSubwordsStepC (SuffixTreeNode, List, List) := (x,beta,s) -> (
   --- Step C in algorithm SEARCH in Amir et.al.
   --- if there is no beta, then begin search at x (no need to traverse the path beta)
   if beta == {} then return suffixTreeSubwordsStepD(x,drop(s,#(x.label)));
   (f,betaHat) := extendedLocus(x,beta);
   if #(f.arcLabel) == #(betaHat) then suffixTreeSubwordsStepD(f,drop(s,#(f.label))) else (f.parent,betaHat,nullTreeNode,false)
)

suffixTreeSubwordsStepD = method()
suffixTreeSubwordsStepD (SuffixTreeNode, List) := (y, s) -> (
   --- Step D in algorithm SEARCH in Amir et.al.
   (newy,f,pre) := contractedLocus(y,s);
   if f === nullTreeNode then return (newy,pre,nullTreeNode,false);
   return (newy,pre,f,f.isFullPattern)
)

suffixTreeSuperwords = method()
suffixTreeSuperwords (SuffixTree, List) := (tree, s) -> (
   --- This function finds all occurrences of suffixes in the tree that have
   --- s as a prefix.  This is equivalent to finding all words in the
   --- dictionary that have s as a factor.
   --- The return value is a list of pairs of integers (word, position, s)
   initialS := s;
   (y,f,pre) := contractedLocus(tree.root,s);
   if (y.label | pre) =!= s then (
      --- no suffix of any word in the dictionary has s as a prefix
      return {};
   )
   else if f === nullTreeNode then (
      --- y is the locus of s.  Process all leaves of y
      return apply(allLeaves y, pl -> 
                   (initialS,#(tree.wordList#(last (pl#1))) - #(pl#1) + 1,tree.wordList#(last (pl#1))));
   )
   else if #(f.label) == #s + 1 and isZZ(last f.label) then (
      --- f is the extended locus of s.  If the label of f is {s,#} then process f
      return {(initialS,#(tree.wordList#(last f.label)) - #(f.label) + 1, tree.wordList#(last f.label))};
   )
   else return {};  --- otherwise return {}
)

suffixTreeLeftOverlaps = method()
suffixTreeLeftOverlaps (SuffixTree, List) := (tree, s) -> (
   --- This function finds all proper prefixes of s that are also
   --- suffixes in the tree.
   --- The return value is a list of pairs of integers (word, position,s)
   y := tree.root;
   initialS := s;
   leftOverlaps := {};
   (f,pre) := findMatch(y,s);
   while f =!= nullTreeNode and pre == f.arcLabel do (
      --- in this case, the prefix found matches the arc label, so we move down
      --- the tree
      y = f;
      s = drop(s,#pre);
      --- after we move down add all children of y that are suffix leaves to leftOverlaps
      --- as long as y is not the root
      suffixLeaves := select(values y.children, c -> isZZ(first c.arcLabel) and not c.isFullPattern);
      leftOverlaps = leftOverlaps | apply(suffixLeaves, c -> (last c.arcLabel, y.label));
      (f,pre) = findMatch(y,s);
   );
   --- At this point, if pre != {} then there is a common prefix to a child, but
   --- the label is not a full match to s.  In this case, we add
   --- the unique suffix leaf that shares a prefix to our list,
   --- as long as it is not a full pattern
   if pre != {} and not f.isFullPattern then leftOverlaps = leftOverlaps | {(last f.arcLabel,pre)};
   apply(leftOverlaps, lo -> (tree.wordList#(lo#0),#(tree.wordList#(lo#0)) - #(lo#1),initialS))
)

--- the right overlaps function is not necessary since
--- it is combined with the insertion algorithm.
--- if we need to search for right overlaps without insertion
--- then we will implement this as well.

--suffixTreeRightOverlaps = method()
--suffixTreeRightOverlaps (SuffixTree, List) := (tree, s) -> (
   --- This function finds all proper prefixes of s that is also
   --- a suffix in the tree.  It is assumed s is not in the dictionary
   --- The return value is a list of pairs of integers (word, position)
--)

checkOverlaps = os -> all(os, o -> ( len := #(o#0) - o#1; take(o#0,-len) == take(o#2,len)))
findOverlapBugs = os -> select(os, o -> ( len := #(o#0) - o#1; take(o#0,-len) != take(o#2,len)))
checkDivisions = sups -> all(sups, sup -> (sup#2)_(toList((sup#1)..(sup#1 + #(sup#0) - 1))) == sup#0)

--- checkTree -- can we write something like this?

beginDocumentation()

doc ///
   Key
    SuffixTree
   Headline
    Macaulay2 implementation of the Suffix Tree data type
   Description
      Text
        This package is a Macaulay2 implementation of the Suffix Tree as given
	in Amir et.al.  It is used for debugging and prototyping the C++ implementation
	of this data structure in e/NCAlgebras/SuffixTree.{hpp,cpp}.  This package has
	no tests or further documentation.
///


end----

restart
debug needsPackage "SuffixTrees"
installPackage "SuffixTrees"

(tree,rightOverlaps) := suffixTree {{c,c,1},{c,a,b,2},{b,a,b,a,3}}

suffixTreeInsert(tree, {c,c,1})
suffixTreeInsert(tree, {c,a,b,2})
suffixTreeInsert(tree, {b,a,b,a,3})

restart
debug needsPackage "SuffixTrees"
--- gens of lead term ideal of generic Sklyanin algebra out to degree 12 (as lists of symbols)
--- I tried putting all this on one line but the parser doesn't like it.
mons = {{Z, X}, {Z, Y}, {Z, Z}, {Y, Y, X}, {Y, Y, Z}, {Y, X, Y, Y}, {Y, Y, Y, Y}, {Y, X, Y, X, X},
        {Y, X, Y, X, Y},{Y, X, Y, X, Z},{Y, X, X, Y, X, X}, {Y, X, X, Y, X, Z}, {Y, X, X, Y, Y, Y},
	{Y, X, X, X, Y, X, Y}, {Y, X, X, X, Y, Y, Y}, {Y, X, X, Y, X, Y, X}, {Y, X, X, Y, X, Y, Z},
	{Y, X, X, X, X, Y, Y, Y}, {Y, X, X, X, Y, X, X, X}, {Y, X, X, X, Y, X, X, Y},
	{Y, X, X, X, Y, X, X, Z}, {Y, X, X, X, X, X, Y, Y, Y}, {Y, X, X, X, X, Y, X, X, X},
	{Y, X, X, X, X, Y, X, X, Z}, {Y, X, X, X, X, Y, X, Y, X}, {Y, X, X, X, X, Y, X, Y, Z},
	{Y, X, X, X, X, X, Y, X, X, Y}, {Y, X, X, X, X, X, Y, X, X, Z}, {Y, X, X, X, X, X, Y, X, Y, X},
	{Y, X, X, X, X, X, Y, X, Y, Z}, {Y, X, X, X, X, Y, X, X, Y, X}, {Y, X, X, X, X, Y, X, X, Y, Y},
	{Y, X, X, X, X, Y, X, X, Y, Z}, {Y, X, X, X, X, X, X, Y, X, X, Z}, {Y, X, X, X, X, X, X, Y, X, Y, X}, 
	{Y, X, X, X, X, X, X, Y, X, Y, Z}, {Y, X, X, X, X, X, Y, X, X, X, X}, {Y, X, X, X, X, X, Y, X, X, X, Y}, 
	{Y, X, X, X, X, X, Y, X, X, X, Z}, {Y, X, X, X, X, X, X, X, Y, X, X, Z}, {Y, X, X, X, X, X, X, X, Y, X, Y, Z}, 
	{Y, X, X, X, X, X, X, Y, X, X, X, X}, {Y, X, X, X, X, X, X, Y, X, X, X, Y}, {Y, X, X, X, X, X, X, Y, X, X, X, Z}, 
	{Y, X, X, X, X, X, X, Y, X, X, Y, X}, {Y, X, X, X, X, X, X, Y, X, X, Y, Y}, {Y, X, X, X, X, X, X, Y, X, X, Y, Z}}
symbolHash = hashTable {(X,0),(Y,1),(Z,2)}
<< endl;
scan(mons, m -> << "Label " << apply(m, s -> symbolHash#s) << ",")
<< endl;
(tree,rightOverlaps) = suffixTree mons;
--- check that the code did not generate spurious right overlaps/superwords/subwords
(tree, rightOverlaps) = suffixTree mons;
assert(#rightOverlaps == 596)
checkOverlaps rightOverlaps

leftOverlaps = suffixTreeLeftOverlaps(tree, {symbol Y, symbol Y, symbol X});
checkOverlaps leftOverlaps
assert(#leftOverlaps == 23)

suffixTreeSubwords(tree, {symbol Y, symbol Y, symbol Z})

superwords = suffixTreeSuperwords(tree, {symbol Y, symbol Y,symbol X});
checkDivisions superwords
assert(#superwords == 1)
superwords = suffixTreeSuperwords(tree, {symbol Y, symbol Y});
checkDivisions superwords
assert(#superwords == 16)

subwords = suffixTreeSubwords(tree, {symbol Z, symbol Z, symbol X, symbol Y, symbol Y, symbol X, symbol Y, symbol X, symbol Y, symbol Y});
checkDivisions subwords
assert(#subwords == 5)

firstSubword = suffixTreeFirstSubword(tree, {symbol Z, symbol Z, symbol X, symbol Y, symbol Y, symbol X, symbol Y, symbol X, symbol Y, symbol Y});
assert(first firstSubword == first subwords)

--- code to generate the above example
restart
debug needsPackage "NCAlgebra"
kk = ZZ/32003
A = threeDimSklyanin(ZZ/32003,{random kk,random kk, random kk},{X,Y,Z})
I = ideal A
J = ncIdeal gens I
Jgb = ncGroebnerBasis(J, DegreeLimit => 5)  -- this takes a bit of time (in bergman)
mons = apply(gens Jgb, f -> (first first pairs (leadMonomial f).terms).monList)
mons = sortUsing(mons, length)

-- subwords bug!!!
restart
debug needsPackage "SuffixTrees"
--- gens of lead term ideal of generic Sklyanin algebra out to degree 12 (as lists of symbols)
--- I tried putting all this on one line but the parser doesn't like it.
mons = {{Z, X}, {Z, Y}, {Z, Z}, {Y, Y, X}}
(tree,rightOverlaps) = suffixTree mons;
subwords = suffixTreeSubwords(tree, {symbol Y, symbol Y, symbol Z})

-- second bug.
restart
debug needsPackage "SuffixTrees"
mons = {{Z,Z},{Z,Y},{Z,X},{Y,Y,X},{Y,Y,Z},{Y,Y,Y,Y},{Y,X,Y,Y},{Y,X,Y,X,Y},
    {Y,X,Y,X,Z},{Y,X,Y,X,X},{Y,X,X,Y,Y,Y},{Y,X,X,Y,X,X},{Y,X,X,Y,X,Z},
    {Y,X,X,Y,X,Y,Z},{Y,X,X,Y,X,Y,X},{Y,X,X,X,Y,X,Y},{Y,X,X,X,Y,Y,Y},
    {Y,X,X,X,Y,X,X,X},{Y,X,X,X,Y,X,X,Y},{Y,X,X,X,Y,X,X,Z},{Y,X,X,X,X,Y,Y,Y},
    {Y,X,X,X,X,Y,X,Y,Z}}
#mons
(tree,rightOverlaps) = suffixTree mons;
subwords = suffixTreeSubwords(tree, {Y,X,X,X,X,Y,X,Y,Z,Z})

restart
debug needsPackage "SuffixTrees"
mons = {{Z,X},{Z,Y},{Z,Z},{Y,Y,X}}
(tree,rightOverlaps) = suffixTree mons;
subwords = suffixTreeSubwords(tree, {Y,Y,Z})

restart
debug needsPackage "AssociativeAlgebras"
kk = ZZ/32003
A = threeDimSklyanin(ZZ/32003,{random kk,random kk, random kk},{X,Y,Z})
I = ideal A
J = ideal gens I
Jgb = NCGB(J, 12)
JgbLT = (ideal Jgb)_* / leadTerm / toVariableList / last / last
print toString JgbLT

-- 'correct' term order
restart
debug needsPackage "SuffixTrees"
mons = {{X, X}, {X, Z}, {X, Y}, {Y, Y, X}, {Y, Y, Z}, {Y, Z, Y, Y}, {Y, Y, Y, Y},
        {Y, Z, Y, Z, Z}, {Y, Z, Y, Z, Y}, {Y, Z, Y, Z, X}, {Y, Z, Z, Y, Y, Y},
	{Y, Z, Z, Y, Z, X}, {Y, Z, Z, Y, Z, Z}, {Y, Z, Z, Z, Y, Y, Y},
	{Y, Z, Z, Y, Z, Y, Z}, {Y, Z, Z, Z, Y, Z, Y}, {Y, Z, Z, Y, Z, Y, X},
	{Y, Z, Z, Z, Y, Z, Z, Z}, {Y, Z, Z, Z, Y, Z, Z, Y}, {Y, Z, Z, Z, Y, Z, Z, X},
	{Y, Z, Z, Z, Z, Y, Y, Y}, {Y, Z, Z, Z, Z, Y, Z, Z, X}, {Y, Z, Z, Z, Z, Y, Z, Y, X},
	{Y, Z, Z, Z, Z, Y, Z, Y, Z}, {Y, Z, Z, Z, Z, Z, Y, Y, Y}, {Y, Z, Z, Z, Z, Y, Z, Z, Z},
	{Y, Z, Z, Z, Z, Y, Z, Z, Y, Y}, {Y, Z, Z, Z, Z, Y, Z, Z, Y, X}, {Y, Z, Z, Z, Z, Z, Y, Z, Y, X},
	{Y, Z, Z, Z, Z, Z, Y, Z, Z, X}, {Y, Z, Z, Z, Z, Y, Z, Z, Y, Z}, {Y, Z, Z, Z, Z, Z, Y, Z, Y, Z}, 
	{Y, Z, Z, Z, Z, Z, Y, Z, Z, Y}, {Y, Z, Z, Z, Z, Z, Y, Z, Z, Z, Z}, {Y, Z, Z, Z, Z, Z, Z, Y, Z, Y, X}, 
	{Y, Z, Z, Z, Z, Z, Y, Z, Z, Z, X}, {Y, Z, Z, Z, Z, Z, Y, Z, Z, Z, Y}, {Y, Z, Z, Z, Z, Z, Z, Y, Z, Z, X}, 
	{Y, Z, Z, Z, Z, Z, Z, Y, Z, Y, Z}, {Y, Z, Z, Z, Z, Z, Z, Y, Z, Z, Y, Z}, {Y, Z, Z, Z, Z, Z, Z, Y, Z, Z, Y, Y},
	{Y, Z, Z, Z, Z, Z, Z, Y, Z, Z, Y, X}, {Y, Z, Z, Z, Z, Z, Z, Y, Z, Z, Z, Z},
	{Y, Z, Z, Z, Z, Z, Z, Z, Y, Z, Y, X}, {Y, Z, Z, Z, Z, Z, Z, Y, Z, Z, Z, Y},
	{Y, Z, Z, Z, Z, Z, Z, Y, Z, Z, Z, X}, {Y, Z, Z, Z, Z, Z, Z, Z, Y, Z, Z, X}}
#mons
(tree,rightOverlaps) = suffixTree mons;
subwords = suffixTreeSubwords(tree, {Y,Z,Z,Z,Z,Y,Z,Y,X,X})
