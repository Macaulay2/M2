--		Copyright 1993-2003 by Daniel R. Grayson

-----------------------------------------------------------------------------
-- html input
-----------------------------------------------------------------------------

html = method(SingleArgumentDispatch=>true, TypicalValue => String)
text = method(SingleArgumentDispatch=>true, TypicalValue => String)
tex = method(SingleArgumentDispatch=>true, TypicalValue => String)
texMath = method(SingleArgumentDispatch=>true, TypicalValue => String)
mathML = method(SingleArgumentDispatch=>true, TypicalValue => String)

MarkUpList = new Type of BasicList
MarkUpList.synonym = "mark-up list"

     MarkUpType = new Type of Type
MarkUpType.synonym = "mark-up type"

FormattedMarkUpType = new Type of MarkUpType
FormattedMarkUpType.synonym = "formatted mark-up type"

EmptyMarkUpType = new Type of MarkUpType
EmptyMarkUpType.synonym = "empty mark-up type"
     MarkUpType List := (h,y) -> new h from y
EmptyMarkUpType List := (h,y) -> if #y === 0 then new h from y else error "expected empty list"
     MarkUpType Thing := (h,y) -> new h from {y}
     MarkUpType\List := (h,y) -> (i -> h i) \ y
     List/MarkUpType := (y,h) -> y / (i -> h i)
EmptyMarkUpType Thing := (h,y) -> error "expected empty list"

makeList := method()
makeList MarkUpType := X -> toString X
makeList Type       := X -> concatenate("new ", toString X, " from ")
toExternalString MarkUpList := s -> concatenate(makeList class s, toExternalString toList s)
toString         MarkUpList := s -> concatenate(makeList class s, toString         toList s)

htmlMarkUpType = s -> (
     on := "<" | s | ">";
     off := "</" | s | ">";
     t -> concatenate(on, apply(t,html), off))

MarkUpType.GlobalAssignHook = (X,x) -> (
     if not ReverseDictionary#?x then (
	  ReverseDictionary#x = X;
     	  html x := htmlMarkUpType toString X;
	  );
     )

new MarkUpType := theMarkUpType -> new theMarkUpType of MarkUpList

BR         = new EmptyMarkUpType
NOINDENT   = new EmptyMarkUpType
HR         = new EmptyMarkUpType
PARA       = new FormattedMarkUpType
EXAMPLE    = new MarkUpType
new EXAMPLE from List := (EXAMPLE,x) -> select(x,i -> i =!= null)
TABLE      = new MarkUpType
ExampleTABLE = new FormattedMarkUpType
PRE        = new FormattedMarkUpType
TITLE      = new MarkUpType
BASE	   = new MarkUpType
HEAD       = new MarkUpType
BODY       = new MarkUpType
IMG	   = new MarkUpType
HTML       = new MarkUpType
BIG        = new MarkUpType
HEADER1    = new MarkUpType
HEADER2    = new MarkUpType
HEADER3    = new MarkUpType
HEADER4    = new MarkUpType
HEADER5    = new MarkUpType
HEADER6    = new MarkUpType
LISTING    = new MarkUpType
LITERAL    = new MarkUpType
XMP        = new MarkUpType
BLOCKQUOTE = new MarkUpType
VAR        = new MarkUpType
DFN        = new MarkUpType
STRONG     = new MarkUpType
BIG        = new MarkUpType
SMALL      = new MarkUpType
SAMP       = new MarkUpType
KBD        = new MarkUpType
SUB        = new MarkUpType
SUP        = new MarkUpType
ITALIC     = new MarkUpType
UNDERLINE  = new MarkUpType
TEX	   = new MarkUpType
SEQ	   = new MarkUpType
new SEQ from List := (SEQ,v) -> select (splice apply(v,
	  i -> if class i === SEQ then toSequence i
	  else if class i === List then toSequence SEQ i
	  else i ),
     j -> j =!= null)
TT         = new MarkUpType
EM         = new MarkUpType
CITE       = new MarkUpType
BOLD       = new MarkUpType
CODE       = new MarkUpType
HREF       = new MarkUpType
ANCHOR     = new MarkUpType
SHIELD     = new MarkUpType
UL         = new MarkUpType
OL         = new MarkUpType
NL         = new MarkUpType
DL 	   = new MarkUpType
TO         = new MarkUpType
TOH        = new MarkUpType

MarkUpList ^ MarkUpList := (x,y) -> SEQ{x,SUP y}
MarkUpList _ MarkUpList := (x,y) -> SEQ{x,SUB y}

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
