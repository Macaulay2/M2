-* Copyright 1997 by Daniel R. Grayson *-
-* Copyright 2020 by Mahrud Sayrafi    *-

-- TODO: not reentrant yet, see resetCounters
-- TODO: cross references aren't working yet
-- TODO: exclude ways to use sections for options if possible

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

counter   :=  0
menuLevel :=  2
secNumber := {0,0}
getNameFromNumber := new MutableHashTable
getNumberFromName := new MutableHashTable
secNumberTable    := new MutableHashTable
miscNodes         := new MutableHashTable

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

resetCounters := () -> (
    counter   :=  0;
    menuLevel :=  2;
    secNumber := {0,0};
    getNameFromNumber := new MutableHashTable;
    getNumberFromName := new MutableHashTable;
    secNumberTable    := new MutableHashTable;
    miscNodes         := new MutableHashTable;
    -- FIXME: this is a hack so the three nodes "Appendix", etc. get deleted
    User#"raw documentation" = new MutableHashTable;
    )

T := {("M", 1000), ("CM", 900), ("D", 500), ("CD", 400), ("C", 100),
                   ("XC", 90),  ("L", 50),  ("XL", 40),  ("X", 10),
                   ("IX", 9),   ("V", 5),   ("IV", 4),   ("I", 1)}
ROMAN := n -> ( s := ""; scan(T, (a, i) -> while n >= i do (n = n - i; s = s | a)); s )

t := {("m", 1000), ("cm", 900), ("d", 500), ("cd", 400), ("c", 100),
                   ("xc", 90),  ("l", 50),  ("xl", 40),  ("x", 10),
		   ("ix", 9),   ("v", 5),   ("iv", 4),   ("i", 1)}
roman := n -> ( s := ""; scan(t, (a, i) -> while n >= i do (n = n - i; s = s | a)); s )

-----------------------------------------------------------------------------
-- we have to keep track of the part and chapter numbers, and *not* reset the
-- chapter number to zero when starting a new part, so:
--     3,8        part 3 (next chapter is chapter 8)
--     3,8,8      part 3, chapter 8
--     3,8,8,5    part 3, chapter 8, section 5
--     3,8,8,5,2  part 3, chapter 8, section 5, subsection 2

next := secNumber -> (
    if #secNumber === 0 then   secNumber else
    if #secNumber === 2 then ( secNumber#0 + 1, secNumber#1 ) else
    if #secNumber === 3 then ( secNumber#0,     secNumber#1 + 1, secNumber#2 + 1 ) else
    append( drop(secNumber, -1), secNumber#-1 + 1 ))

fmt := secNumber -> demark(".", prepend(ROMAN secNumber#0, apply( drop(secNumber, 2), toString) ))

record := node -> (
    counter = counter + 1;
    getNumberFromName#node = counter;
    getNameFromNumber#counter = node;
    secNumber = next secNumber;
    n := secNumberTable#counter = fmt secNumber;
    printerr((2 * #secNumber):" ", n, ". ", node))

descend := (n) -> (secNumber = (
    if #secNumber === 2 then ( secNumber#0, secNumber#1, secNumber#1 )
    else join(secNumber, toList(n:0))); n)

ascend := (n, s) -> (
    if # secNumber === 1 then error "oops: ascending too high, producing empty section number";
    if # secNumber === 0 then error "oops: empty section number";
    secNumber = drop(secNumber, -n); s)

crawl := method()
-- Taking advantage of tail-call optimization is crucial here,
-- otherwise we easily run into recursion limits
crawl TreeNode   := x -> ( record format x#0; ascend(descend(1), crawl x#1) )
crawl ForestNode := x -> if #x>0 then scan(toList x, y -> ( -* zero out the section; *- crawl y))

-----------------------------------------------------------------------------
UnknownReference := "???"

crossReference := (key, text, optional) -> (
    secNumber := (
	if getNumberFromName#?key
	then secNumberTable#(getNumberFromName#key)
	else ( printerr("warning: missing node: " | format key); UnknownReference) );
    if secNumber === UnknownReference
    then if optional
    then (                              "{\\bf ", tex text,  "}" )
    else (                              "{\\bf ", tex text,  "} [", secNumber, "]" )
    else ( "\\hyperlink{", secNumber, "}{{\\bf ", tex text, "}} [", secNumber, "]" ))

-----------------------------------------------------------------------------

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

-----------------------------------------------------------------------------

installPDF = (pkg, installPrefix, installLayout, verboseLog) -> (
    resetCounters();
    tableOfContents := unbag pkg#"table of contents";
    -- body of book consists only of subnodes of the top node
    verboseLog "making a list of sections";
    crawl first tableOfContents;
    -- the appendix contains everything else
    verboseLog "making a list of orphan sections";
    record "Appendix"; descend(1);
    -- look for orphaned nodes
    -- TODO: implement crawler for drop(tableOfContents, 1)
    apply(sort keys pkg#"raw documentation", fkey -> (
	    tag := getPrimaryTag makeDocumentTag(fkey, Package => pkg);
	    fkey = format tag;
	    if  not getNumberFromName#?fkey
	    and not miscNodes#?fkey
	    and not isMissingDoc tag
	    and not isUndocumented tag
	    then miscNodes#fkey = help tag));
    --
    oldCurrentPackage := currentPackage;
    currentPackage = User;
    document {
	Key => "Appendix",
	"This appendix contains additional information about the following topics.",
	UL nonnull { if #keys miscNodes > 0 then TO "Miscellaneous documentation", TO "Symbol Index" } };
    --secNumber = {"A"}
    if #keys miscNodes > 0 then
    record "Miscellaneous documentation";
    if #keys miscNodes > 0 then
    document {
	Key => "Miscellaneous documentation",
	"We present various additional documentation in this chapter.",
	UL ascend(descend(1), apply(pairs miscNodes,
		(fkey, rawdoc) -> ( record fkey; TO2 {fkey, TT fkey} )))};
    --secNumber = {"B"}
    record "Symbol Index";
    srcpkg := if pkg#"pkgname" == "Macaulay2Doc" then Core else pkg;
    document {
	Key => "Symbol Index",
	TEX "\\begin{multicols}{2}",
	apply(
	    sort join(srcpkg#"exported symbols", srcpkg#"exported mutable symbols"),
	    symb -> ( TT toString symb, PARA{})),
	TEX "\\end{multicols}\n",
	TEX "\\vfill"
	};
    ascend(1, -* "Appendix" *-);
    --
    banner := "Auto-generated for Macaulay2-%M2VERSION%. Do not modify this file manually.";
    -- TODO: can we allow per-package templates, for individualized macros?
    template := prefixDirectory | replace("PKG", "Style", currentLayout#"package") | "M2book.tex.in";
    if not fileExists template then error "installPDF: missing LaTeX template for PDF documentation";
    --
    template = get(template);
    output := concatenate("%% ", banner, newline, newline,     template);
    output = replace("%M2VERSION%",      version#"VERSION",      output);
    output = replace("%M2COMPILETIME%",  version#"compile time", output);
    (preamble, biblio) := toSequence separate("%M2BOOKCONTENT%", output);
    --
    -- TODO: include pkg.Options.Version
    -- NOTE: has to remain in sync with packages/CMakeLists.txt
    bookdir  := replace("PKG", pkg#"pkgname", installLayout#"packagedoc");
    verboseLog("making PDF manual in ", installPrefix | bookdir);
    bookname := installPrefix | bookdir | "manual.tex";
    bookname << preamble;
    scan(pairs getNameFromNumber, (i, node) -> (
	    bookname                                                            << endl << endl
	    << "\\hypertarget{" << (n := secNumberTable#i) << "}{}"                     << endl
	    << sectionType n << "{" << tex format node << "}" << "\\label{" << n << "}" << endl
	    << concatenate booktex help node                                            << endl));
    bookname << biblio << close;
    --
    currentPackage := oldCurrentPackage;
    verboseLog("created ", replace("\\.tex$", ".pdf", bookname)))
