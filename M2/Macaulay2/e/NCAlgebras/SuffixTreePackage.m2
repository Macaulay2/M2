newPackage("SuffixTreePackage",
     Headline => "Data Type for a Suffix Tree as laid out by Amir et.al.",
     Version => "0.1",
     Date => "Jan 29, 2019",
     Authors => {
	  {Name => "Frank Moore",
	   HomePage => "http://www.math.wfu.edu/Faculty/Moore.html",
	   Email => "moorewf@wfu.edu"},
	  {Name => "Mike Stillman",
	   HomePage => "",
	   Email => ""}},
     PackageImports => {},
     AuxiliaryFiles => true,
     DebuggingMode => true,
     CacheExampleOutput =>true
     )

protect label
protect arcLabel
protect isFullPattern
protect patternLeafCount
protect suffixLink
protect children
protect root
protect wordList

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
      origv := v;
      (newv,roRoot,newLocus) := suffixTreeInsertWorker(tree,v,s,isFullPattern);
      v = newv;
      if roRoot != {} then (
	 tempRos := apply(patternLeaves(first roRoot), pl -> (last newLocus.label,pl#0,pl#1));
	 tempRos = apply(tempRos, ro -> (tree.wordList#(ro#0),#(tree.wordList#(ro#0)) - #(ro#1),tree.wordList#(ro#2)));
	 assert(checkOverlaps tempRos);
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
suffixTreeStepC = (v,x,beta,s,isFullPattern) -> (
   --- Carries out step C in the algorithm.  This amounts to computing (and building,
   --- if necessary) the suffix link of v.  This function can call the Step D code
   --- as well (see the algorithm in the paper by Amir, et.al.)
   c := if isFullPattern then 1 else 0;
   betaHat := beta;
   origx := x;
   (f,pre) := findMatch(x,betaHat);
   while #(f.arcLabel) < #betaHat do (
      x = f;
      betaHat = drop(betaHat,#(f.arcLabel));
      (f,pre) = findMatch(x,betaHat);
   );
   if #(f.arcLabel) == #betaHat then (
      v.suffixLink = f;
      assert(drop(v.label,1) == f.label);
      return suffixTreeStepD(f,drop(s,#(f.label)),isFullPattern);
   );
   p := f.parent;
   d := suffixTreeNode(p,betaHat,false);
   d.patternLeafCount = f.patternLeafCount + c + (if f.isFullPattern then 1 else 0);
   remove(p.children,f.arcLabel);
   f.arcLabel = drop(f.arcLabel,#betaHat);
   d.children#(f.arcLabel) = f;
   f.parent = d;
   w := suffixTreeNode(d,drop(s,#(d.label)),isFullPattern);
   v.suffixLink = d;
   assert(drop(v.label,1) == d.label);
   if #(w.label) == 1 then (d,{f},w) else (d,{},w)
)

suffixTreeStepD = method()
suffixTreeStepD (SuffixTreeNode, List, Boolean) := (y,s,isFullPattern) -> (
   --- Carries out step D in the algorithm.  This amounts to constructing
   --- the locus of the head of s (which is not yet known before calling this function).
   local v;
   origS := s;
   origY := y;
   c := if isFullPattern then 1 else 0;
   y.patternLeafCount = y.patternLeafCount + c;
   (f,pre) := findMatch(y,s);
   if f === y then (
      -- in this case, there is no common prefix of a label of the child of f and s
      -- so just create a leaf immediately.
      v = suffixTreeNode(y,s,isFullPattern);
      return if y.label != {} and #(v.arcLabel) == 1 then (y,{y},v) else (y,{},v);
   );
   while pre == f.arcLabel do (
      y = f;
      s = drop(s,#pre);
      y.patternLeafCount = y.patternLeafCount + c;
      (f,pre) = findMatch(y,s);
      if f === y then (
         -- in this case, there is no common prefix of a label of the child of f and s
      	 -- so just create a leaf immediately.
      	 v = suffixTreeNode(y,s,isFullPattern);
         return if y.label != {} and #(v.arcLabel) == 1 then (y,{y},v) else (y,{},v);
      );
   );
   -- this is the case when alpha is nonempty.  Here, we must add an internal node
   -- as well as a leaf
   p := suffixTreeNode(y,pre,false);
   p.patternLeafCount = f.patternLeafCount + c + (if f.isFullPattern then 1 else 0);
   s = drop(s,#pre);
   v = suffixTreeNode(p,s,isFullPattern);
   -- take out f as a child of y with old label, and put it back in with new label
   remove(y.children,f.arcLabel);
   f.arcLabel = drop(f.arcLabel, #pre);
   (p.children)#(f.arcLabel) = f;
   -- don't forget to change the parent of f!
   f.parent = p;
   return if #s == 1 then (p,{p},v) else (p,{},v);
)

findMatch = method()
findMatch (SuffixTreeNode, List) := (y,s) -> (
  -- PURPOSE : Find an arc from y to a child f whose label shares a prefix with s           
  -- INPUT   : Node y and a list s.
  -- OUTPUT  : Node f, list pre 
  -- return y if no match is found, i.e. the empty prefix is the only shared prefix with any
  -- children of y
  f := y;
  pre := {};
  for kv in pairs (y.children) list (
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
   -- OUTPUT  : List that s and t share as prefix.
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
   if #v.children == 0 then return {v};
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
      (newcLocus,newbeta,wasPattern) := suffixTreeSubwordsWorker(tree,cLocus,beta,s);
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
      (newcLocus,newbeta,wasPattern) := suffixTreeSubwordsWorker(tree,cLocus,beta,s);
      if wasPattern then
         subwords = subwords | {(newcLocus.label | newbeta,pos,initialS)};
      pos = pos + 1;
      s = drop(s,1);
      cLocus = newcLocus;
      beta = newbeta;
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
   betaHat := beta;
   (f,pre) := findMatch(x,betaHat);
   while #(f.arcLabel) < #betaHat do (
      x = f;
      betaHat = drop(betaHat,#(f.arcLabel));
      (f,pre) = findMatch(x,betaHat);
   );
   if #(f.arcLabel) == #betaHat then suffixTreeSubwordsStepD(f,drop(s,#(f.label))) else (x,betaHat,false)
)

suffixTreeSubwordsStepD = method()
suffixTreeSubwordsStepD (SuffixTreeNode, List) := (y, s) -> (
   --- Step D in algorithm SEARCH in Amir et.al.
   local v;
   initialS := s;
   (f,pre) := findMatch(y,s);
   if f === y then (
      -- in this case, there is no common prefix of a label of the child of f and s
      -- here, f is the cLocus of s and we return beta, the 'leftovers'
      return (f,pre,false);
   );
   while pre == f.arcLabel do (
      y = f;
      s = drop(s,#pre);
      (f,pre) = findMatch(y,s);
      if f === y then (
         -- in this case, there is no common prefix of a label of the child of f and s
         -- here, f is the cLocus of s and beta is the 'leftovers'
	 return (f,pre,false);
      );
   );
   -- this is the case when alpha is nonempty.  This occurs when there is an arc whose
   -- prefix matches s.  Report whether f corresponds to a full pattern.
   (y,pre,f.isFullPattern)
)

suffixTreeSuperwords = method()
suffixTreeSuperwords (SuffixTree, List) := (tree, s) -> (
   --- This function finds all occurrences of suffixes in the tree that have
   --- s as a prefix.  This is equivalent to finding all words in the
   --- dictionary that have s as a factor.
   --- The return value is a list of pairs of integers (word, position, s)
   y := tree.root;
   (f,pre) := findMatch(y,s);
   initialS := s;
   if f === y then (
      -- in this case, there is no common prefix with s of a label of the child of the root
      -- so there are no superwords
      return {};
   );
   while pre == f.arcLabel do (
      --- in this case, the prefix found matches the arc label, so we move down
      --- the tree
      y = f;
      s = drop(s,#pre);
      --- after we move down add all children of y that are suffix leaves to leftOverlaps
      --- as long as y is not the root
      (f,pre) = findMatch(y,s);
   );
   --- once here, we simply return all suffix leaves of f, suitably processed
   apply(allLeaves f, pl -> 
       (initialS,#(tree.wordList#(last (pl#1))) - #(pl#1) + 1,tree.wordList#(last (pl#1))))
       
)

suffixTreeLeftOverlaps = method()
suffixTreeLeftOverlaps (SuffixTree, List) := (tree, s) -> (
   --- This function finds all proper prefixes of s that is also
   --- a suffix in the tree.
   --- The return value is a list of pairs of integers (word, position,s)
   --- Must be called before s is inserted into the dictionary, since we do not allow
   --- a word to left overlap with itself (we use right overlaps for that)
   y := tree.root;
   initialS := s;
   leftOverlaps := {};
   (f,pre) := findMatch(y,s);
   if f === y then (
      -- in this case, there is no common prefix of a label of the child of the root
      -- so there are no left overlaps
      return leftOverlaps;
   );
   while pre == f.arcLabel do (
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
--- it is combined with the insertion algoritm.
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

end

restart
debug needsPackage "SuffixTreePackage"

(tree,rightOverlaps) := suffixTree {{c,c,1},{c,a,b,2},{b,a,b,a,3}}

suffixTreeInsert(tree, {c,c,1})
suffixTreeInsert(tree, {c,a,b,2})
suffixTreeInsert(tree, {b,a,b,a,3})

restart
debug needsPackage "SuffixTreePackage"
--- gens of lead term ideal of generic Sklyanin algebra out to degree 12 (as lists of symbols)
--- I tried putting all this on one line but the parser doesn't like it.
mons = {{Z, X}, {Z, Y}, {Z, Z}, {Y, Y, X}, {Y, Y, Z}, {Y, X, Y, Y}, {Y, Y, Y, Y}, {Y, X, Y, X, X},
        {Y, X, Y, X, Y}, {Y, X, Y, X, Z}, {Y, X, X, Y, X, X}, {Y, X, X, Y, X, Z}, {Y, X, X, Y, Y, Y},
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
--- check that the code did not generate spurious right overlaps/superwords/subwords
(tree, rightOverlaps) = suffixTree mons;
#rightOverlaps
checkOverlaps rightOverlaps
leftOverlaps = suffixTreeLeftOverlaps(tree, {symbol Y, symbol Y, symbol X});
checkOverlaps leftOverlaps
assert(#leftOverlaps == 23)
superwords = suffixTreeSuperwords(tree, {symbol Y, symbol Y});
checkDivisions superwords
assert(#superwords == 16)
--- not finding all of them
subwords = suffixTreeSubwords(tree, {symbol Z, symbol Z, symbol X, symbol Y, symbol Y, symbol X, symbol Y, symbol X, symbol Y, symbol Y});
checkDivisions subwords
assert(#subwords == 5)
firstSubword = suffixTreeFirstSubword(tree, {symbol Z, symbol Z, symbol X, symbol Y, symbol Y, symbol X, symbol Y, symbol X, symbol Y, symbol Y});
first firstSubword == first subwords

--- code to generate the above example
restart
debug needsPackage "NCAlgebra"
kk = ZZ/32003
A = threeDimSklyanin(ZZ/32003,{random kk,random kk, random kk},{X,Y,Z})
I = ideal A
J = ncIdeal gens I
Jgb = ncGroebnerBasis(J, DegreeLimit => 12)  -- this takes a bit of time (in bergman)
mons = apply(gens Jgb, f -> (first first pairs (leadMonomial f).terms).monList)
mons = sortUsing(mons, length)
