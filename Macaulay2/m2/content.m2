-- from xhtml-math11.dtd:

PCDATA = set {"#PCDATA"}
String.qname = "#PCDATA"


-- <!ENTITY % Inline.extra "" >
InlineExtra = set {}

-- <!ENTITY % Ruby.class "| %ruby.qname;" >
Ruby = set { "ruby" }

-- <!ENTITY % Inlform.class "| %input.qname; | %select.qname; | %textarea.qname; | %label.qname; | %button.qname;" >
Inlform = set { "input", "select", "textarea", "label", "button" }
LABEL.qname = "label"

-- <!ENTITY % Inlspecial.class "| %img.qname; | %map.qname; | %applet.qname;" >
Inlspecial = set { "img", "map", "applet" }
IMG.qname = "img"

-- <!ENTITY % Anchor.class "| %a.qname;" >
AnchorClass = set { "a" }
ANCHOR.qname = "a"

-- <!ENTITY % I18n.class "| %bdo.qname;" >
I18nClass = set { "bdo" }

-- <!ENTITY % Inlpres.class "| %tt.qname; | %i.qname; | %b.qname; | %big.qname; | %small.qname; | %sub.qname; | %sup.qname;" >
Inlpres = set { "tt", "i", "b", "big", "small", "sub", "sup" }
TT.qname = "tt"
ITALIC.qname = "i"
BOLD.qname = "b"
SMALL.qname = "small"
SUB.qname = "sub"
SUP.qname = "sup"

-- <!ENTITY % Inlphras.class "| %em.qname; | %strong.qname; | %dfn.qname; | %code.qname; | %samp.qname; | %kbd.qname; | %var.qname; | %cite.qname; | %abbr.qname; | %acronym.qname; | %q.qname;" >
Inlphras = set { "em", "strong", "dfn", "code", "samp", "kbd", "var", "cite", "abbr", "acronym", "q" }
EM.qname = "em"
STRONG.qname = "strong"
CODE.qname = "code"

-- <!ENTITY % Inlstruct.class "%br.qname; | %span.qname;" >
Inlstruct = set { "br", "span" }
BR.qname = "br"

-- <!ENTITY % Inline-noA.class "%Inlstruct.class; %Inlphras.class; %Inlpres.class; %I18n.class; %Inlspecial.class; %Inlform.class; %Ruby.class; %Inline.extra;">
InlineNoAClass = Inlstruct + Inlphras + Inlpres + I18nClass +          Inlspecial + Inlform + Ruby + InlineExtra

-- <!ENTITY % Inline.class "%Inlstruct.class; %Inlphras.class; %Inlpres.class; %I18n.class; %Anchor.class; %Inlspecial.class; %Inlform.class; %Ruby.class; %Inline.extra;" >
InlineClass    = Inlstruct + Inlphras + Inlpres + I18nClass + AnchorClass + Inlspecial + Inlform + Ruby + InlineExtra

-- <!ENTITY % Block.extra "" >
BlockExtra = set {}

-- <!ENTITY % Blkspecial.class "| %table.qname; | %form.qname; | %fieldset.qname;" >
Blkspecial = set { "table", "form", "fieldset" }
TABLE.qname = "table"

-- <!ENTITY % Blkpres.class "| %hr.qname;" >
Blkpres = set {"hr"}
HR.qname = "hr"

-- <!ENTITY % Blkphras.class "| %pre.qname; | %blockquote.qname; | %address.qname;" >
Blkphras = set {"pre", "blockquote", "address" }
PRE.qname = "pre"

-- <!ENTITY % Blkstruct.class "%p.qname; | %div.qname;" >
Blkstruct = set {"p", "div" }
PARA.qname = "p"
DIV.qname = "div"

-- <!ENTITY % Block.class "%Blkstruct.class; %Blkphras.class; %Blkpres.class; %Blkspecial.class; %Block.extra;">
BlockClass = Blkstruct + Blkphras + Blkpres + Blkspecial + BlockExtra

-- <!ENTITY % List.class "%ul.qname; | %ol.qname; | %dl.qname;" >
ListClass = set {"ul", "ol", "dl" }
UL.qname = "ul"

-- <!ENTITY % Heading.class "%h1.qname; | %h2.qname; | %h3.qname; | %h4.qname; | %h5.qname; | %h6.qname;" >
HeadingClass = set {"h1", "h2", "h3", "h4", "h5", "h6"}
HEADER1.qname = "h1"
HEADER2.qname = "h2"
HEADER3.qname = "h3"
HEADER4.qname = "h4"
HEADER5.qname = "h5"
HEADER6.qname = "h6"

-- <!ENTITY % Misc.extra "" >
MiscExtra = set {}

-- <!ENTITY % Script.class "| %script.qname; | %noscript.qname;" >
ScriptClass = set {"script", "noscript"}

-- <!ENTITY % Edit.class "| %ins.qname; | %del.qname;" >
EditClass = set {"ins", "del"}

-- <!ENTITY % Misc.class "%Edit.class; %Script.class; %Misc.extra;" >
MiscClass = EditClass + ScriptClass + MiscExtra

-- <!ENTITY % Inline.mix "%Inline.class; %Misc.class;" >
InlineMix = InlineClass + MiscClass

-- <!ENTITY % Flow.mix "%Heading.class; | %List.class; | %Block.class; | %Inline.class; %Misc.class;" >
FlowMix =  HeadingClass + ListClass + BlockClass + InlineClass + MiscClass

-- <!ENTITY % Block.mix "%Heading.class; | %List.class; | %Block.class; %Misc.class;" >
BlockMix = HeadingClass + ListClass + BlockClass + MiscClass

-----------------------------------------------------------------------------
-- content
-----------------------------------------------------------------------------

Content = new MutableHashTable

-----------------------------------------------------------------------------
-- <!ENTITY % li.content "( #PCDATA | %Flow.mix; )*" >
Content#"li" = 
-- <!ENTITY % div.content "( #PCDATA | %Flow.mix; )*" >
Content#"div" = PCDATA + FlowMix
-----------------------------------------------------------------------------


-- <!ENTITY % body.content "( %Block.mix; )+" >
Content#"body" = BlockMix
BODY.qname = "body"

-- <!ENTITY % Heading.content  "( #PCDATA | %Inline.mix; )*" >
HeadingContent = PCDATA + InlineMix

-- <!ELEMENT %h1.qname;  %Heading.content; >
-- <!ELEMENT %h2.qname;  %Heading.content; >
-- <!ELEMENT %h3.qname;  %Heading.content; >
-- <!ELEMENT %h4.qname;  %Heading.content; >
-- <!ELEMENT %h5.qname;  %Heading.content; >
-- <!ELEMENT %h6.qname;  %Heading.content; >
Content#"h1" = Content#"h2" = Content#"h3" = Content#"h4" = Content#"h5" = Content#"h6" = HeadingContent

-- <!ENTITY % label.content "( #PCDATA | %input.qname; | %select.qname; | %textarea.qname; | %button.qname; | %Inlstruct.class;
--       %Inlphras.class; %I18n.class; %Inlpres.class; %Anchor.class; %Inlspecial.class; %Inline.extra; %Misc.class; )*" >
-- <!ELEMENT %label.qname;  %label.content; >
Content#"label" = PCDATA + set { "input", "select", "textarea", "button", "Inlstruct" } + Inlphras + Inlpres + AnchorClass + InlineExtra + MiscClass

-- <!ENTITY % a.content "( #PCDATA | %Inline-noA.mix; )*" >
Content#"a" = InlineNoAClass
-----------------------------------------------------------------------------
---- these are all empty:
-- <!ENTITY % hr.content  "EMPTY" >
Content#"hr" =
-- <!ENTITY % img.content  "EMPTY" >
Content#"img" =
-- <!ENTITY % br.content  "EMPTY" >
Content#"br" = set {}
-----------------------------------------------------------------------------
---- these are all the same:
-- <!ENTITY % p.content "( #PCDATA | %Inline.mix; )*" >
Content#"p" =
-- <!ENTITY % strong.content "( #PCDATA | %Inline.mix; )*" >
Content#"strong" =
-- <!ENTITY % code.content "( #PCDATA | %Inline.mix; )*" >
Content#"code" =
-- <!ENTITY % em.content "( #PCDATA | %Inline.mix; )*" >
Content#"em" =
-- <!ENTITY % tt.content "( #PCDATA | %Inline.mix; )*" >
Content#"tt" = 
-- <!ENTITY % i.content "( #PCDATA | %Inline.mix; )*" >
Content#"i" = 
-- <!ENTITY % b.content "( #PCDATA | %Inline.mix; )*" >
Content#"b" = 
-- <!ENTITY % small.content "( #PCDATA | %Inline.mix; )*" >
Content#"small" = 
-- <!ENTITY % sup.content "( #PCDATA | %Inline.mix; )*" >
Content#"sup" = 
-- <!ENTITY % sub.content "( #PCDATA | %Inline.mix; )*" >
Content#"sub" = PCDATA + InlineMix
-----------------------------------------------------------------------------

-- <!ENTITY % pre.content "( #PCDATA | %Inlstruct.class; %Inlphras.class; | %tt.qname; | %i.qname; | %b.qname;
--       %I18n.class; %Anchor.class; | %script.qname; | %map.qname; %Inline.extra; )*" >
Content#"pre" = PCDATA + Inlstruct + Inlphras + set { "tt", "i", "b" } + I18nClass + AnchorClass + set {"script","map"} + InlineExtra


-- <!ENTITY % table.content "( %caption.qname;?, ( %col.qname;* | %colgroup.qname;* ), (( %thead.qname;?, %tfoot.qname;?, %tbody.qname;+ ) | ( %tr.qname;+ )))" >
-- the regular expression is more complicated, but we don't implement those other tags, anyway!  Or should we?
Content#"table" = set { "caption", "col", "colgroup", "thead", "tfoot", "tbody", "tr" }

-- <!ENTITY % ul.content  "( %li.qname; )+" >
Content#"ul" = set { "li" }


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
