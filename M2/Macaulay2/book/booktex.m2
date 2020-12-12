-* Copyright 1997 by Daniel R. Grayson *-
-* Copyright 2020 by Mahrud Sayrafi    *-

beginDocumentation()
importFrom_Core {"ForestNode", "TreeNode", "isMissingDoc", "isSecondaryTag", "isUndocumented", "packageTagList", "assembleTree", "getPrimaryTag", "topDocumentTag"}
documentationMemo = memoize help#0

-----------------------------------------------------------------------------
T := {("M", 1000), ("CM", 900), ("D", 500), ("CD", 400), ("C", 100),
                   ("XC", 90),  ("L", 50),  ("XL", 40),  ("X", 10),
                   ("IX", 9),   ("V", 5),   ("IV", 4),   ("I", 1)}
ROMAN = n -> ( s := ""; scan(T, (a, i) -> while n >= i do (n = n - i; s = s | a)); s )

t := {("m", 1000), ("cm", 900), ("d", 500), ("cd", 400), ("c", 100),
                   ("xc", 90),  ("l", 50),  ("xl", 40),  ("x", 10),
		   ("ix", 9),   ("v", 5),   ("iv", 4),   ("i", 1)}
roman = n -> ( s := ""; scan(t, (a, i) -> while n >= i do (n = n - i; s = s | a)); s )

-----------------------------------------------------------------------------
-- we have to keep track of the part and chapter numbers, and *not* reset the
-- chapter number to zero when starting a new part, so:
--     3,8        part 3 (next chapter is chapter 8)
--     3,8,8      part 3, chapter 8
--     3,8,8,5    part 3, chapter 8, section 5
--     3,8,8,5,2  part 3, chapter 8, section 5, subsection 2

-- TODO: make this functional
sectionNumber = {0,0}

getNameFromNumber  = new MutableHashTable
getNumberFromName  = new MutableHashTable
sectionNumberTable = new MutableHashTable
miscNodes          = new MutableHashTable

next := sectionNumber -> (
    if #sectionNumber === 0 then   sectionNumber else
    if #sectionNumber === 2 then ( sectionNumber#0 + 1, sectionNumber#1 ) else
    if #sectionNumber === 3 then ( sectionNumber#0,     sectionNumber#1 + 1, sectionNumber#2 + 1 ) else
    append( drop(sectionNumber, -1), sectionNumber#-1 + 1 ))

fmt := sectionNumber -> demark(".", prepend(ROMAN sectionNumber#0, apply( drop(sectionNumber, 2), toString) ))

counter := 0;
record = node -> (
    counter = counter + 1;
    getNumberFromName#node = counter;
    getNameFromNumber#counter = node;
    sectionNumber = next sectionNumber;
    n := sectionNumberTable#counter = fmt sectionNumber;
    stderr << concatenate("node :", 2 * #sectionNumber, n, ". ", node) << endl;)

descend := (n) -> (sectionNumber = (
    if #sectionNumber === 2 then ( sectionNumber#0, sectionNumber#1, sectionNumber#1 )
    else join(sectionNumber, toList(n:0))); n)

ascend := (n, s) -> (
    if # sectionNumber === 1 then error "oops: ascending too high, producing empty section number";
    if # sectionNumber === 0 then error "oops: empty section number";
    sectionNumber = drop(sectionNumber, -n); s)

crawl := method()
-- Taking advantage of tail-call optimization is crucial here,
-- otherwise we easily run into recursion limits
crawl TreeNode   := x -> ( record format x#0; ascend(descend(1), crawl x#1) )
crawl ForestNode := x -> if #x>0 then scan(toList x, y -> ( -* zero out the section; *- crawl y))

--------------- body of book
topDocumentTag = makeDocumentTag(pkg#"pkgname", Package => pkg);
nodeList := packageTagList(pkg, topDocumentTag);
nodeTree := assembleTree(pkg, getPrimaryTag \ select(nodeList, tag -> not isUndocumented tag));

-- only subnodes of the top node
crawl first nodeTree

--------------- appendix
rawdocs = pkg#"raw documentation"
record "Appendix"; descend(1)
document {
    Key => "Appendix",
    "This appendix contains additional information about the following topics.",
    UL { TO "Miscellaneous documentation" } }

--sectionNumber = {"A"}
record "Miscellaneous documentation";
document {
    Key => "Miscellaneous documentation",
    "We present various additional documentation in this chapter.",
    ascend(descend(1), UL apply(sort keys rawdocs, fkey -> (
		tag := getPrimaryTag makeDocumentTag(fkey, Package => pkg);
		fkey = format tag;
		if  not getNumberFromName#?fkey
		and not miscNodes#?fkey
		and not isMissingDoc tag
		and not isUndocumented tag then (
		    miscNodes#fkey = documentationMemo tag;
		    record fkey; TO2 {tag, TT format tag} ))))}

--sectionNumber = {"B"}
record "Symbol Index";
document {
    Key => "Symbol Index",
    TEX "\\begin{multicols}{2}",
    apply(
	sort join(pkg#"exported symbols", pkg#"exported mutable symbols"),
	symb -> ( TT toString symb, PARA{})),
    TEX "\\end{multicols}\n",
    TEX "\\vfill"
    }
ascend(1, -* "Appendix" *-)

---------------
UnknownReference := "???"

crossReference := (key, text, optional) -> (
    sectionNumber := (
	if getNumberFromName#?key
	then sectionNumberTable#(getNumberFromName#key)
	else (
	    -- error("warning: documentation for key '", key, "' not found");
	    -- stderr << "warning: documentation for key '" << key << "' not found" << endl;
	    UnknownReference));
    if sectionNumber === UnknownReference
    then if optional
    then (                                  "{\\bf ", tex text,  "}" )
    else (                                  "{\\bf ", tex text,  "} [", sectionNumber, "]" )
    else ( "\\hyperlink{", sectionNumber, "}{{\\bf ", tex text, "}} [", sectionNumber, "]" ))

-----------------------------------------------------------------------------

menuLevel := 2

booktex = method(Dispatch => Thing)
booktex Thing := tex
-- TODO: uncomment these lines to actually use booktex and crossreferences
--booktex Sequence  :=
--booktex BasicList := x -> apply(x, booktex)
booktex TO      :=
booktex TO2     :=
booktex TOH     := x -> crossReference(format x#0, format x#0, false)
booktex IMG     :=
booktex Nothing := x -> ""

booktex OL :=
booktex UL := x -> concatenate(
    "\\begingroup", newline,
    "\\parskip=0pt", newline,
    apply(x, x -> if x =!= null then (
	    (menuLevel = menuLevel + 1;),
	    "%", newline, "\\par", newline,
	    apply(menuLevel - 1, i -> "\\indent"),
	    "\\hangindent", toString menuLevel, "\\parindent", newline,
	    -- "\\textindent{$\\bullet$}",
	    booktex if instance(x, TO) then SPAN{ x, headline x#0 } else x,
	    (menuLevel = menuLevel - 1;),
	    "%", newline, "\\par", newline)),
    "%", newline, "\\endgroup", newline)

--------------------------------------------
-- this loop depends on the feature of hash tables that when the keys
-- are consecutive integers starting at 0, the keys are scanned
-- in the natural order, which in turn depends on the hash number of
-- a small integer being the integer itself
levelLimit := 10;
sectionType = sectionNumber -> (
    level := # select(characters sectionNumber, i -> i === ".");
    if level > levelLimit then level = levelLimit;
    if level === 0 then "\\part" else
    if level === 1 then "\\chapter" else
    if level === 2 then "\\section" else
    if level === 3 then "\\subsection" else
    if level === 4 then "\\subsubsection" else
    if level === 5 then "\\paragraph" else
    if level === 6 then "\\subparagraph" else
    if level === 7 then "\\subsubparagraph" else
    if level === 8 then "\\subsubsubparagraph" else
    if level === 9 then "\\subsubsubsubparagraph" else
    "\\subsubsubsubsubparagraph");
