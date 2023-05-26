needs "set.m2"

protect qname 						    -- an internal key

-- from xhtml-math11-f.dtd:

-- the one I looked at first:           "http://www.w3.org/TR/MathML2/dtd/xhtml-math11-f.dtd"
---the newer version that works better: "http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd"

PCDATA = set {"#PCDATA"}
String.qname = "#PCDATA"

-- <!ENTITY % Inline.extra "" >
InlineExtra = set {}

-- <!ENTITY % Ruby.class "| %ruby.qname;" >
Ruby = set { "ruby" }

-- <!ENTITY % Inlform.class "| %input.qname; | %select.qname; | %textarea.qname; | %label.qname; | %button.qname;" >
---<!ENTITY % InlForm.class "| %input.qname; | %select.qname; | %textarea.qname; | %label.qname; | %button.qname;" >
InlForm = set { "input", "select", "textarea", "label", "button" }

-- <!ENTITY % Inlspecial.class "| %img.qname; | %map.qname; | %applet.qname;" >
---<!ENTITY % InlSpecial.class "| %img.qname; | %map.qname; | %object.qname;" >
InlSpecial = set { "img", "map", "applet" }

-- <!ENTITY % Anchor.class "| %a.qname;" >
AnchorClass = set { "a" }

-- <!ENTITY % I18n.class "| %bdo.qname;" >
I18nClass = set { "bdo" }

-- <!ENTITY % InlPres.class "| %tt.qname; | %i.qname; | %b.qname; | %big.qname; | %small.qname; | %sub.qname; | %sup.qname;" >
---<!ENTITY % InlPres.class "| %tt.qname; | %i.qname; | %b.qname; | %big.qname; | %small.qname; | %sub.qname; | %sup.qname;" >
InlPres = set { "tt", "i", "b", "big", "small", "sub", "sup" }

-- <!ENTITY % Inlphras.class "| %em.qname; | %strong.qname; | %dfn.qname; | %code.qname; | %samp.qname; | %kbd.qname; | %var.qname; | %cite.qname; | %abbr.qname; | %acronym.qname; | %q.qname;" >
Inlphras = set { "em", "strong", "dfn", "code", "samp", "kbd", "var", "cite", "abbr", "acronym", "q" }

-- <!ENTITY % Inlstruct.class "%br.qname; | %span.qname;" >
Inlstruct = set { "br", "span" }

-- <!ENTITY % Inline-noA.class "%Inlstruct.class; %Inlphras.class; %InlPres.class; %I18n.class; %InlSpecial.class; %InlForm.class; %Ruby.class; %Inline.extra;">
InlineNoAClass = Inlstruct + Inlphras + InlPres + I18nClass +          InlSpecial + InlForm + Ruby + InlineExtra

-- <!ENTITY % Inline.class "%Inlstruct.class; %Inlphras.class; %InlPres.class; %I18n.class; %Anchor.class; %InlSpecial.class; %InlForm.class; %Ruby.class; %Inline.extra;" >
InlineClass    = Inlstruct + Inlphras + InlPres + I18nClass + AnchorClass + InlSpecial + InlForm + Ruby + InlineExtra

-- <!ENTITY % Block.extra "" >
BlockExtra = set {}

-- <!ENTITY % Blkspecial.class "| %table.qname; | %form.qname; | %fieldset.qname;" >
---<!ENTITY % Table.class "| %table.qname;" >
---<!ENTITY % Form.class  "| %form.qname;" >
---<!ENTITY % Fieldset.class  "| %fieldset.qname;" >
---<!ENTITY % BlkSpecial.class "%Table.class; %Form.class; %Fieldset.class;" >
BlkSpecial = set { "table", "form", "fieldset" }

-- <!ENTITY % Blkpres.class "| %hr.qname;" >
---<!ENTITY % BlkPres.class "| %hr.qname;" >
BlkPres = set {"hr"}

-- <!ENTITY % Blkphras.class "| %pre.qname; | %blockquote.qname; | %address.qname;" >
---<!ENTITY % BlkPhras.class "| %pre.qname; | %blockquote.qname; | %address.qname;" >
BlkPhras = set {"pre", "blockquote", "address" }

-- <!ENTITY % Blkstruct.class "%p.qname; | %div.qname;" >
---<!ENTITY % BlkStruct.class "%p.qname; | %div.qname;" >
BlkStruct = set {"p", "div" }

-- <!ENTITY % Block.class "%BlkStruct.class; %BlkPhras.class; %BlkPres.class; %BlkSpecial.class; %Block.extra;">
---<!ENTITY % Block.class "%BlkStruct.class; %BlkPhras.class; %BlkPres.class; %BlkSpecial.class; %Block.extra;">
BlockClass = BlkStruct + BlkPhras + BlkPres + BlkSpecial + BlockExtra

-- <!ENTITY % List.class "%ul.qname; | %ol.qname; | %dl.qname;" >
---<!ENTITY % List.class "%ul.qname; | %ol.qname; | %dl.qname;" >
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
-- <!ENTITY % dd.content "( #PCDATA | %Flow.mix; )*" >
validContent#"dd" =
-- <!ENTITY % li.content "( #PCDATA | %Flow.mix; )*" >
validContent#"li" = 
-- <!ENTITY % div.content "( #PCDATA | %Flow.mix; )*" >
validContent#"div" = PCDATA + FlowMix
-----------------------------------------------------------------------------

-- <!ENTITY % blockquote.content "( %Block.mix; )+" >
validContent#"blockquote" = 
-- <!ENTITY % body.content "( %Block.mix; )+" >
validContent#"body" = BlockMix

-- <!ENTITY % dl.content  "( %dt.qname; | %dd.qname; )+" >
validContent#"dl" = set { "dt", "dd" }

-- <!ENTITY % dt.content "( #PCDATA | %Inline.mix; )*" >
validContent#"dt" =
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
--       %Inlphras.class; %I18n.class; %InlPres.class; %Anchor.class; %InlSpecial.class; %Inline.extra; %Misc.class; )*" >
-- <!ELEMENT %label.qname;  %label.content; >
validContent#"label" = PCDATA + set { "input", "select", "textarea", "button", "Inlstruct" } + Inlphras + InlPres + AnchorClass + InlineExtra + MiscClass

-- <!ENTITY % a.content "( #PCDATA | %Inline-noA.mix; )*" >
validContent#"a" = PCDATA + InlineNoAClass
-----------------------------------------------------------------------------
---- these are all empty:
-- <!ENTITY % link.content  "EMPTY" >
validContent#"link" =
-- <!ENTITY % meta.content  "EMPTY" >
validContent#"meta" =
-- <!ENTITY % base.content  "EMPTY" >
validContent#"base" =
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
-- <!ENTITY % span.content "( #PCDATA | %Inline.mix; )*" >
validContent#"span" =
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
validContent#"kbd" =
validContent#"samp" =
validContent#"var" =
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
-- <!ENTITY % html.content  "( %head.qname;, %body.qname; )" >
validContent#"html" = set {"head", "body"}
-----------------------------------------------------------------------------
-- <!ENTITY % pre.content "( #PCDATA | %Inlstruct.class; %Inlphras.class; | %tt.qname; | %i.qname; | %b.qname;
--       %I18n.class; %Anchor.class; | %script.qname; | %map.qname; %Inline.extra; )*" >
validContent#"pre" = PCDATA + Inlstruct + Inlphras + set { "tt", "i", "b" } + I18nClass + AnchorClass + set {"script","map"} + InlineExtra
-----------------------------------------------------------------------------
-- <!ENTITY % Head-opts.mix "( %script.qname; | %style.qname; | %meta.qname; | %link.qname; )*" >
-- <!ENTITY % head.content
--     "( %Head-opts.mix;,
--        ( ( %title.qname;, %Head-opts.mix;, ( %base.qname;, %Head-opts.mix; )? )
--        | ( %base.qname;, %Head-opts.mix;, ( %title.qname;, %Head-opts.mix; ))))" >
validContent#"head" = set {"title", "base", "script", "style", "meta", "link" }
-----------------------------------------------------------------------------
-- <!ENTITY % td.content "( #PCDATA | %Flow.mix; )*" >
validContent#"th" =
validContent#"td" = PCDATA + FlowMix
-- <!ENTITY % tr.content  "( %th.qname; | %td.qname; )+" >
validContent#"tr" = set { "th", "td" }
-- <!ENTITY % table.content "( %caption.qname;?, ( %col.qname;* | %colgroup.qname;* ), (( %thead.qname;?, %tfoot.qname;?, %tbody.qname;+ ) | ( %tr.qname;+ )))" >
-- the regular expression is more complicated, but we don't implement those other tags, anyway!  Or should we?
validContent#"table" = set { "caption", "col", "colgroup", "thead", "tfoot", "tbody", "tr" }
-----------------------------------------------------------------------------
-- <!ENTITY % title.content  "( #PCDATA )" >
validContent#"title" = PCDATA
-----------------------------------------------------------------------------

-- <!ENTITY % ul.content  "( %li.qname; )+" >
validContent#"ul" = set { "li" }
-- <!ENTITY % ol.content  "( %li.qname; )+" >
validContent#"ol" = set { "li" }

validContent#"comment" = PCDATA
validContent#"cdata" = PCDATA

-- <!ENTITY % style.content  "( #PCDATA )" >
validContent#"style" = PCDATA

-- <!ENTITY % script.content  "( #PCDATA )" >
validContent#"script" = PCDATA

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
