---             Copyright 1993-2004 by Daniel R. Grayson
-*
  The help command returns its output as a Hypertext object.
  Three steps are necessary in order to generate documentation:
  - parse Hypertext nodes into subnodes
  - render subnodes in the appropriate format
  - join the result

  info and net are parsed and rendered in this file.
  When possible, write a new core script that performs the steps above.
  See hypertext.m2 for the comprehensive list of markup types that inherit from
  MarkUpType, and therefore need to be rendered.
*-

needs "document.m2"
needs "hypertext.m2"
needs "nets.m2"

-----------------------------------------------------------------------------
-- Common utilities for formatting documentation nodes
-----------------------------------------------------------------------------

-- Macro for setting default parsing of type T
-- When writing a new formatting tool, call setupRenderer to create the default
-- parsing for Hypertext, then use the examples provided below to test rendering
-- of individual subtypes.
setupRenderer = (parser, joiner, T) -> (
    parser T := node -> joiner apply(node,
	subnode -> if class subnode =!= Option and class subnode =!= OptionTable then parser subnode))

-- Default joiners: (TODO: move to string.m2?)
-- concatenate
-- horizontalJoin
wrapHorizontalJoin := x -> wrap horizontalJoin x

-- Main types: (see hypertext.m2)
-- Hypertext  > HypertextParagraph, HypertextContainer
-- MarkUpType > IntermediateMarkUpType

-- skip Options; TODO: define parser Option := null instead
noopts := x -> select(x,e -> class e =!= Option and class e =!= OptionTable)

-----------------------------------------------------------------------------
-- Setup uniform rendering
-----------------------------------------------------------------------------

-- Rendering TOH and LATER
scan({net, html, markdown, tex}, parser ->
    parser TOH := node -> parser SPAN nonnull { new TO from toList node, commentize headline node#0 } )
scan({net, info, html, markdown, tex}, parser ->
    parser LATER := node -> parser node#0() )

-- Rendering by horizontal join of inputs
scan({net, info},
    parser -> setupRenderer(parser, horizontalJoin, Hypertext))
scan({net, info},
    parser -> setupRenderer(parser, wrapHorizontalJoin, HypertextParagraph))

-----------------------------------------------------------------------------
-- info and net
-----------------------------------------------------------------------------

net  STYLE :=
net  TITLE :=
info TITLE :=
net  COMMENT :=
info COMMENT :=
net  LITERAL :=
info LITERAL := x -> ""

info String  := identity
info Nothing := net

-*
 spacing between lines and paragraphs:
 observation of browsers reveals:
     nonempty PARA items get at least one blank line above and below
     empty PARA items produce just one blank line
     multiple consecutive empty PARA items have the same effect as one
     empty BR items produce one line break, forcing the current line to terminate, and a second one does it again
     empty DIV items produce one line break, forcing the current line to terminate, but a second one has no new effect
     DIV items are single spaced on separate lines
     nested DIV items don't space more widely
     multiple empty BR items produce multiple line breaks
     PARA "a", BR {}, PARA "c"        leads to "\na\n\n\n\nc\n"
     PARA "a", "b", BR {}, PARA "c"   leads to "\na\n\nb\n\nc\n"
     but: DIV elements can contain DIV elements and PARA elements
     and: DIV{DIV PARA "a", DIV PARA "b", DIV PARA "c" } should format just like DIV{ PARA "a", PARA "b", PARA "c" }
     that means the conversion to nets cannot be a totally recursive algorithm

     that leads to this algorithm:
      introduce new symbols: BK SP
      expand PARA{x} to SP x SP
      expand BR{}    to "" BK
      expand DIV{x}  to BK x BK
      do the expansions above recursively, do the following collapses at top level:
           collapse each sequence of consecutive SPs and BKs to BK "" BK if there is at least one SP in there, else to BK
      collect things between BKs and wrap them into nets, with empty sequences,
           if we didn't collapse each BK...BK, becoming empty nets of height 0 and depth 0
      discard each BK
      stack all the nets

     We modify that slightly, by removing all the initial and final BKs and SPs at top level

     One more consideration: info MENUs should not be wrapped, but they can be contained in a DIV, which
     must arrange for the wrapping of strings contained in it.  Also, HypertextParagraphs have already been
     wrapped, so they don't need to be wrapped again.
*-

BK := local BK
SP := local SP

-- Define (net, HypertextContainer) and (info, HypertextContainer)
scan({net, info},
    parser -> (
	-- create a temporary parser this will return either the usual output
	-- object or a sequence of such objects and BKs or SKs for later splicing.
	parser' := value (toString parser | "'") <- method(Dispatch => Thing);
	-- setup default rendering methods
	parser' Thing := parser;
	-- { } indicates wrapping is already done or is not desired
	parser' HypertextParagraph := x -> (SP, {parser x}, SP);
	parser' HypertextContainer := x -> (BK, apply(toSequence x, parser'), BK);
	-- rendering for special types
	parser' String := identity;
	parser' COMMENT :=
	parser' LITERAL :=
	parser' Option  :=
	parser' OptionTable  :=
	parser' Nothing := x -> ();
	parser' BR     := x -> ("", BK);
	-- and rendering for types that inherit from HypertextContainer, but
	-- have special rendering rules which would lost with toSequence
	parser' TABLE :=
	parser' MENU :=
	parser' DL :=
	parser' UL :=
	parser' OL := x -> (BK, {parser x}, BK);
	-- Here is where we define the method
	parser HypertextContainer := x -> (
	    -- Apply parser' first
	    x = deepSplice parser' x;
	    -- Drop the leading and trailing SPs or BKs
	    l := position(x, e -> e =!= SP and e =!= BK);
	    t := position(x, e -> e =!= SP and e =!= BK, Reverse => true);
	    if l =!= null and t =!= null then x = take(x, {l, t});
	    -- ??
	    x = splice sublists(x, i -> i === BK or i === SP,
		SPBKs -> if member(SP,SPBKs) then (BK,"",BK) else BK);
	    x = splice sublists(x, i -> i =!= BK,
		x -> if #x===1 and instance(x#0,List) then horizontalJoin x#0 else wrap horizontalJoin x,
		BK -> ());
	    -- Stack the pieces vertically
	    stack x);
	))

Hop := (op,filler) -> x -> (
     r := horizontalJoin apply(noopts x,op);
     if width r === 1 then r = horizontalJoin(r," ");
     r || concatenate( width r : filler ) )
net  HEADER1 := Hop(net, "*")
net  HEADER2 := Hop(net, "=")
net  HEADER3 := Hop(net, "-")
info HEADER1 := Hop(info,"*")
info HEADER2 := Hop(info,"=")
info HEADER3 := Hop(info,"-")

net  HR :=
info HR := x -> concatenate(printWidth:"-")

net  PRE  :=
net   TT  :=
net CODE  :=
info TT   :=
info CODE :=  x -> horizontalJoin apply(noopts x,net)

info PRE  := x -> wrap(printWidth, "-", concatenate apply(noopts x,toString))

net TH := Hop(net, "-")

ULop := op -> x -> (
     s := "  * ";
     printWidth = printWidth - #s;
     r := stack apply(toList noopts x, i -> s | op i);
     printWidth = printWidth + #s;
     r)
info UL := ULop info
net  UL := ULop net

OLop := op -> x -> (
     s := "000. ";
     printWidth = printWidth - #s;
     x = toList noopts x;
     r := stack apply(#x, i -> pad(3,toString (i+1)) | ". " | op x#i); -- html starts counting from 1!
     printWidth = printWidth + #s;
     r)
info OL := OLop info
net  OL := OLop net

info DL := x -> stack apply(noopts x, info)
net  DL := x -> stack apply(noopts x, net)

info DD := x -> "    " | horizontalJoin apply(noopts x, info)
net  DD := x -> "    " | horizontalJoin apply(noopts x, net)

opSU := (op,n) -> x -> (horizontalJoin apply(noopts x, op))^n
net  SUP := opSU(net, 1)
info SUP := opSU(info,1)
net  SUB := opSU(net, -1)
info SUB := opSU(info,-1)

net  IMG :=
info IMG := x -> (
     (o,cn) := override(IMG.Options,toSequence x);
     if o#"alt" === null then error ("IMG item is missing alt attribute");
     o#"alt")

net  HREF :=
info HREF := x -> (
     if #x === 1 then x#0
     else if match ("^mailto:",x#0) then toString x#1
     -- x#0 is sometimes the relative path to the file, but not from the current directory
     else (
	  r := horizontalJoin \\ net \ toList splice drop(x, 1);
	  r | " (see " | x#0 | " )"
	  )
     )

net TABLE :=  x -> (
     (op,ag) := override(options TABLE, toSequence x);
     save := printWidth;
     printWidth = printWidth - 2;
     r := netList(Boxes => op#"class" === "examples", HorizontalSpace => 2, noopts \ toList \ toList sequence ag);
     printWidth = save;
     r)
info TABLE := x -> (
     s := printWidth;
     if printWidth > 2 then printWidth = printWidth - 2;
     ret := netList(Boxes=>true, applyTable(noopts \ toList \ noopts \\ toList x,info));
     printWidth = s;
     ret)

-----------------------------------------------------------------------------
-- Handling TO and TO2 and MENU
-----------------------------------------------------------------------------

-- node names in info files are delimited by commas and parentheses somehow...
infoLiterals := new MutableHashTable from {
    "(" => "_lp",
    "_" => "_us",
    ")" => "_rp",
    "," => "_cm",
    "." => "_pd",
    "*" => "_st",
    ":" => "_co",
    }
infoLinkConvert := str -> replace(":", "_colon_", str)
infoLiteral     := str -> fold(pairs infoLiterals, str, (c, str) -> replace(regexQuote c#0, c#1, str))
tagConvert      := str -> infoLiteral if match("(^ | $)", str) then concatenate("\"", str, "\"") else str

infoTagConvert = method()
infoTagConvert String      := tagConvert
infoTagConvert DocumentTag := tag -> (
    tag = getPrimaryTag tag;
    (pkgname, fkey) := (tag.Package, format tag);
    fkey  = tagConvert if pkgname === fkey then "Top" else fkey;
    if pkgname =!= currentPackage#"pkgname" then concatenate("(",pkgname,")", fkey) else fkey)

-- TODO: can this be simplified?
-- checking if doc is missing can be very slow if node is from another package
info TO  := x -> info TO2{x#0, format x#0 | if x#?1 then x#1 else ""}
info TO2 := x -> (
     tag := fixup x#0;
     if isMissingDoc tag or isUndocumented tag then concatenate(x#1, " (missing documentation)")
     else concatenate("*note ", infoLinkConvert x#1, ": ", infoTagConvert tag, ","))
info TOH := x -> (
     tag := x#0;
     f := format tag;
     if x#?1 then f = f|x#1;
     concatenate(
	  if isMissingDoc tag or isUndocumented tag then (f, " (missing documentation)")
	  else ("*note ", infoLinkConvert f, ": ", infoTagConvert tag, ","),
	  commentize headline tag))

net TO  := x -> (
     if class x#0 === DocumentTag
     then concatenate( "\"", format x#0, "\"", if x#?1 then x#1)
     else horizontalJoin( "\"", net x#0, "\"", if x#?1 then x#1)
     )
net TO2 := x -> format x#1

-- TODO: move this back from help.m2
net  MENU := x -> net redoMENU x
info MENU := x -> (
    contents := deepApply'(x, identity, item -> instance(item, BasicList) and not isLink item);
    pushvar(symbol printWidth, 0); -- wrapping a menu item makes it hard for emacs info to follow links
    ret := join(
	{"* Menu:", ""},
	nonnull sublists(contents,
	    line    -> isLink line,
	    section -> stack apply(section, line -> "* " | wrap(
		    fkey := format line#0;
		    icon := infoTagConvert line#0;
		    cfkey := infoLinkConvert fkey;
		    text := cfkey | if cfkey === icon then "::" else ": " | icon | ".";
		    title := headline line#0;
		    if title =!= null then concatenate(text, 28-#text:" ", "  ") | title else text)),
	    line -> stack("", info if instance(line, Hypertext) then line else DIV {line})));
     popvar symbol printWidth;
     stack ret)
