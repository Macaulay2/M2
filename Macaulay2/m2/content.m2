-- from xhtml-math11.dtd:

Hypertext.qname = "body"				    -- to keep it general (?)

PCDATA = set {"#PCDATA"}
String.qname = "#PCDATA"

-- <!ENTITY % Inline.extra "" >
InlineExtra = set {}

-- <!ENTITY % Ruby.class "| %ruby.qname;" >
Ruby = set { "ruby" }

-- <!ENTITY % Inlform.class "| %input.qname; | %select.qname; | %textarea.qname; | %label.qname; | %button.qname;" >
Inlform = set { "input", "select", "textarea", "label", "button" }

-- <!ENTITY % Inlspecial.class "| %img.qname; | %map.qname; | %applet.qname;" >
Inlspecial = set { "img", "map", "applet" }

-- <!ENTITY % Anchor.class "| %a.qname;" >
AnchorClass = set { "a" }

-- <!ENTITY % I18n.class "| %bdo.qname;" >
I18nClass = set { "bdo" }

-- <!ENTITY % Inlpres.class "| %tt.qname; | %i.qname; | %b.qname; | %big.qname; | %small.qname; | %sub.qname; | %sup.qname;" >
Inlpres = set { "tt", "i", "b", "big", "small", "sub", "sup" }

-- <!ENTITY % Inlphras.class "| %em.qname; | %strong.qname; | %dfn.qname; | %code.qname; | %samp.qname; | %kbd.qname; | %var.qname; | %cite.qname; | %abbr.qname; | %acronym.qname; | %q.qname;" >
Inlphras = set { "em", "strong", "dfn", "code", "samp", "kbd", "var", "cite", "abbr", "acronym", "q" }

-- <!ENTITY % Inlstruct.class "%br.qname; | %span.qname;" >
Inlstruct = set { "br", "span" }

-- <!ENTITY % Inline-noA.class "%Inlstruct.class; %Inlphras.class; %Inlpres.class; %I18n.class; %Inlspecial.class; %Inlform.class; %Ruby.class; %Inline.extra;">
InlineNoAClass = Inlstruct + Inlphras + Inlpres + I18nClass +          Inlspecial + Inlform + Ruby + InlineExtra

-- <!ENTITY % Inline.class "%Inlstruct.class; %Inlphras.class; %Inlpres.class; %I18n.class; %Anchor.class; %Inlspecial.class; %Inlform.class; %Ruby.class; %Inline.extra;" >
InlineClass    = Inlstruct + Inlphras + Inlpres + I18nClass + AnchorClass + Inlspecial + Inlform + Ruby + InlineExtra

-- <!ENTITY % Block.extra "" >
BlockExtra = set {}

-- <!ENTITY % Blkspecial.class "| %table.qname; | %form.qname; | %fieldset.qname;" >
Blkspecial = set { "table", "form", "fieldset" }

-- <!ENTITY % Blkpres.class "| %hr.qname;" >
Blkpres = set {"hr"}

-- <!ENTITY % Blkphras.class "| %pre.qname; | %blockquote.qname; | %address.qname;" >
Blkphras = set {"pre", "blockquote", "address" }

-- <!ENTITY % Blkstruct.class "%p.qname; | %div.qname;" >
Blkstruct = set {"p", "div" }
-- SEQ.qname = "div"					    -- try to phase this one out!!!

-- <!ENTITY % Block.class "%Blkstruct.class; %Blkphras.class; %Blkpres.class; %Blkspecial.class; %Block.extra;">
BlockClass = Blkstruct + Blkphras + Blkpres + Blkspecial + BlockExtra

-- <!ENTITY % List.class "%ul.qname; | %ol.qname; | %dl.qname;" >
ListClass = set {"ul", "ol", "dl" }

-- <!ENTITY % Heading.class "%h1.qname; | %h2.qname; | %h3.qname; | %h4.qname; | %h5.qname; | %h6.qname;" >
HeadingClass = set {"h1", "h2", "h3", "h4", "h5", "h6"}

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

validContent = new MutableHashTable

-----------------------------------------------------------------------------
-- <!ENTITY % li.content "( #PCDATA | %Flow.mix; )*" >
validContent#"li" = 
-- <!ENTITY % div.content "( #PCDATA | %Flow.mix; )*" >
validContent#"div" = PCDATA + FlowMix
-----------------------------------------------------------------------------

-- <!ENTITY % body.content "( %Block.mix; )+" >
validContent#"body" = BlockMix

-- <!ENTITY % Heading.content  "( #PCDATA | %Inline.mix; )*" >
HeadingContent = PCDATA + InlineMix
-- <!ELEMENT %h1.qname;  %Heading.content; >
-- <!ELEMENT %h2.qname;  %Heading.content; >
-- <!ELEMENT %h3.qname;  %Heading.content; >
-- <!ELEMENT %h4.qname;  %Heading.content; >
-- <!ELEMENT %h5.qname;  %Heading.content; >
-- <!ELEMENT %h6.qname;  %Heading.content; >
validContent#"h1" =
validContent#"h2" =
validContent#"h3" =
validContent#"h4" =
validContent#"h5" =
validContent#"h6" = HeadingContent

-- <!ENTITY % label.content "( #PCDATA | %input.qname; | %select.qname; | %textarea.qname; | %button.qname; | %Inlstruct.class;
--       %Inlphras.class; %I18n.class; %Inlpres.class; %Anchor.class; %Inlspecial.class; %Inline.extra; %Misc.class; )*" >
-- <!ELEMENT %label.qname;  %label.content; >
validContent#"label" = PCDATA + set { "input", "select", "textarea", "button", "Inlstruct" } + Inlphras + Inlpres + AnchorClass + InlineExtra + MiscClass

-- <!ENTITY % a.content "( #PCDATA | %Inline-noA.mix; )*" >
validContent#"a" = InlineNoAClass
-----------------------------------------------------------------------------
---- these are all empty:
-- <!ENTITY % link.content  "EMPTY" >
validContent#"link" =
-- <!ENTITY % hr.content  "EMPTY" >
validContent#"hr" =
-- <!ENTITY % img.content  "EMPTY" >
validContent#"img" =
-- <!ENTITY % br.content  "EMPTY" >
validContent#"br" = set {}
-----------------------------------------------------------------------------
---- these are all the same:
-- <!ENTITY % p.content "( #PCDATA | %Inline.mix; )*" >
validContent#"p" =
-- <!ENTITY % strong.content "( #PCDATA | %Inline.mix; )*" >
validContent#"strong" =
-- <!ENTITY % code.content "( #PCDATA | %Inline.mix; )*" >
validContent#"code" =
-- <!ENTITY % em.content "( #PCDATA | %Inline.mix; )*" >
validContent#"em" =
-- <!ENTITY % tt.content "( #PCDATA | %Inline.mix; )*" >
validContent#"tt" = 
-- <!ENTITY % i.content "( #PCDATA | %Inline.mix; )*" >
validContent#"i" = 
-- <!ENTITY % b.content "( #PCDATA | %Inline.mix; )*" >
validContent#"b" = 
-- <!ENTITY % small.content "( #PCDATA | %Inline.mix; )*" >
validContent#"small" = 
-- <!ENTITY % sup.content "( #PCDATA | %Inline.mix; )*" >
validContent#"sup" = 
-- <!ENTITY % sub.content "( #PCDATA | %Inline.mix; )*" >
validContent#"sub" = PCDATA + InlineMix
-----------------------------------------------------------------------------

-- <!ENTITY % pre.content "( #PCDATA | %Inlstruct.class; %Inlphras.class; | %tt.qname; | %i.qname; | %b.qname;
--       %I18n.class; %Anchor.class; | %script.qname; | %map.qname; %Inline.extra; )*" >
validContent#"pre" = PCDATA + Inlstruct + Inlphras + set { "tt", "i", "b" } + I18nClass + AnchorClass + set {"script","map"} + InlineExtra


-- <!ENTITY % table.content "( %caption.qname;?, ( %col.qname;* | %colgroup.qname;* ), (( %thead.qname;?, %tfoot.qname;?, %tbody.qname;+ ) | ( %tr.qname;+ )))" >
-- the regular expression is more complicated, but we don't implement those other tags, anyway!  Or should we?
validContent#"table" = set { "caption", "col", "colgroup", "thead", "tfoot", "tbody", "tr" }

-- <!ENTITY % ul.content  "( %li.qname; )+" >
validContent#"ul" = set { "li" }

validate = method()
validate Option :=
validate TO :=
validate TOH :=
validate String := x -> null
validate ExampleTABLE := x -> scan(x,item -> validate item#1)
validate TO2 := x -> scan(drop(x,1),validate)
validate MarkUpList := x -> validate(class x, x)
validate(Type, BasicList) := (p,x) -> (			    -- regard p as the class of x
     if p.?qname then (
	  n := p.qname;
	  if validContent#?n then validate(p,validContent#n,x)
	  else stderr << "--internal error: valid content for qname " << format n << " not recorded yet" << endl;
	  scan(x,validate))
     else stderr << "--error: " << format toString p << " isn't an html type (has no qname)" << endl;
     x)
validate(Type, Set, BasicList) := (p, valid,x) -> (	    -- top level only
     scan(x, e -> (
	       c := class e;
	       if (not c.?qname or not valid#?(c.qname)) and c =!= Option 
	       then stderr << "--error: element of type " << format toString p << " can't contain an element of type " << format toString c << endl));
     x)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
