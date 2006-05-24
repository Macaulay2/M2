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
Anchor = set { "a" }
ANCHOR.qname = "a"

-- <!ENTITY % I18n.class "| %bdo.qname;" >
I18n = set { "bdo" }

-- <!ENTITY % Inlpres.class "| %tt.qname; | %i.qname; | %b.qname; | %big.qname; | %small.qname; | %sub.qname; | %sup.qname;" >
Inlpres = set { "tt", "i", "b", "big", "small", "sub", "sup" }
TT.qname = "tt"
ITALIC.qname = "i"
BOLD.qname = "bold"
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
InlineNoAClass = Inlstruct + Inlphras + Inlpres + I18n +          Inlspecial + Inlform + Ruby + InlineExtra

-- <!ENTITY % Inline.class "%Inlstruct.class; %Inlphras.class; %Inlpres.class; %I18n.class; %Anchor.class; %Inlspecial.class; %Inlform.class; %Ruby.class; %Inline.extra;" >
InlineClass    = Inlstruct + Inlphras + Inlpres + I18n + Anchor + Inlspecial + Inlform + Ruby + InlineExtra

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

-- <!ENTITY % div.content "( #PCDATA | %Flow.mix; )*" >
Content#"div" = PCDATA + FlowMix

-- <!ENTITY % p.content "( #PCDATA | %Inline.mix; )*" >
Content#"p" = PCDATA + InlineMix

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
Content#"label" = PCDATA + set { "input", "select", "textarea", "button", "Inlstruct" } + Inlphras + Inlpres + Anchor + InlineExtra + MiscClass

-- still to do:

-- <!ENTITY % img.content  "EMPTY" >
Content#"img" = set {}

-- <!ENTITY % a.content "( #PCDATA | %Inline-noA.mix; )*" >
Content#"a" = InlineNoAClass

Content#"tt" = set {}
Content#"i" = set {}
Content#"bold" = set {}
Content#"small" = set {}
Content#"sub" = set {}
Content#"sup" = set {}
Content#"em" = set {}
Content#"strong" = set {}
Content#"code" = set {}
Content#"br" = set {}
Content#"table" = set {}
Content#"hr" = set {}
Content#"pre" = set {}
Content#"ul" = set {}

peek Content
