-- Copyright 2009 by Daniel R. Grayson
use common;
use util;

header "
#include <libxml/parser.h>
#include <libxml/tree.h>
";

-- xmlparse(e:Expr):Expr := (
--      when e is s:string do (parse(s); nullE)
--      else WrongArgString()
--      );
-- setupfun("XML$parse",xmlparse);

type(n:xmlNode) ::= Ccode(int, "((", n, ")->type)");
ElementNode() ::= Ccode(int,"XML_ELEMENT_NODE");
TextNode() ::= Ccode(int,"XML_TEXT_NODE");
isElement(n:xmlNode) ::= type(n) == ElementNode();
isText(n:xmlNode) ::= type(n) == TextNode();
Properties(n:xmlNode) ::= xmlAttrOrNull( Ccode(xmlAttr,"((",n,")->properties)") );
Children(n:xmlNode) ::= xmlNodeOrNull( Ccode(xmlNode,"((",n,")->children)") );
Children(n:xmlAttr) ::= xmlNodeOrNull( Ccode(xmlNode,"((",n,")->children)") );
Next(n:xmlNode) ::= xmlNodeOrNull( Ccode(xmlNode,"((",n,")->next)") );
Next(n:xmlAttr) ::= xmlAttrOrNull( Ccode(xmlAttr,"((",n,")->next)") );
Name(n:xmlNode) ::= Ccode(constucharstarOrNull,"((",n,")->name)");
Name(n:xmlAttr) ::= Ccode(constucharstarOrNull,"((",n,")->name)");
Content(n:xmlNode) ::= Ccode(constucharstarOrNull,"((",n,")->content)");

xmlFirstAttribute(e:Expr):Expr := (
     -- # typical value: xmlFirstAttribute, LibxmlNode, LibxmlAttribute
     when e is node:xmlNodeCell do when Properties(node.v) is attr:xmlAttr do toExpr(attr) else nullE
     else WrongArg("an XML node"));
setupfun("xmlFirstAttribute",xmlFirstAttribute);
xmlParse(e:Expr):Expr := (
     -- # typical value: xmlParse, String, LibxmlNode
     when e is s:stringCell do when Parse(s.v) is x:xmlNode do toExpr(x)
     else buildErrorPacket("xml syntax error")
     else WrongArgString());
setupfun("xmlParse",xmlParse);
xmlFirstChild(e:Expr):Expr := (
     -- # typical value: xmlFirstChild, LibxmlNode, LibxmlNode
     -- # typical value: xmlFirstChild, LibxmlAttribute, LibxmlNode
     when e
     is node:xmlNodeCell do when Children(node.v) is child:xmlNode do toExpr(child) else nullE
     is attr:xmlAttrCell do when Children(attr.v) is child:xmlNode do toExpr(child) else nullE
     else WrongArg("an XML node or attribute"));
setupfun("xmlFirstChild",xmlFirstChild);
xmlGetName(e:Expr):Expr := (
     -- # typical value: xmlGetName, LibxmlNode, String
     -- # typical value: xmlGetName, LibxmlAttribute, String
     when e
     is node:xmlNodeCell do toExpr(Name(node.v))
     is attr:xmlAttrCell do toExpr(Name(attr.v))
     else WrongArg("an XML node"));
setupfun("xmlGetName",xmlGetName);
xmlGetContent(e:Expr):Expr := (
     -- # typical value: xmlGetContent, LibxmlNode, String
     when e
     is node:xmlNodeCell do toExpr(Content(node.v))
     else WrongArg("an XML node"));
setupfun("xmlGetContent",xmlGetContent);
xmlGetNext(e:Expr):Expr := (
     -- # typical value: xmlGetNext, LibxmlNode, LibxmlNode
     -- # typical value: xmlGetNext, LibxmlAttribute, LibxmlAttribute
     when e
     is node:xmlNodeCell do when Next(node.v) is next:xmlNode do toExpr(next) else nullE
     is attr:xmlAttrCell do when Next(attr.v) is next:xmlAttr do toExpr(next) else nullE
     else WrongArg("an XML node or attribute"));
setupfun("xmlGetNext",xmlGetNext);
xmlIsElement(e:Expr):Expr := (
     -- # typical value: xmlIsElement, LibxmlNode, Boolean
     when e
     is node:xmlNodeCell do toExpr(isElement(node.v))
     else WrongArg("an XML node"));
setupfun("xmlIsElement",xmlIsElement);
xmlIsText(e:Expr):Expr := (
     -- # typical value: xmlIsText, LibxmlNode, Boolean
     when e
     is node:xmlNodeCell do toExpr(isText(node.v))
     else WrongArg("an XML node"));
setupfun("xmlIsText",xmlIsText);

xmlNewRoot0(e:Expr):Expr := (
     -- # typical value: xmlNewRoot, String, LibxmlNode
     when e is name:stringCell do toExpr(NewRoot("1.0",name.v))
     else WrongArgString());
setupfun("xmlNewRoot",xmlNewRoot0);

xmlAddAttribute(e:Expr):Expr := (
     -- # typical value: xmlAddAttribute, LibxmlNode, String, String, LibxmlAttribute
     when e is args:Sequence do if length(args) != 3 then WrongNumArgs(3) else
     when args.0 is parent:xmlNodeCell do
     when args.1 is name:stringCell do
     when args.2 is value:stringCell do
     toExpr(AddAttribute(parent.v,name.v,value.v))
     else WrongArgString(3)
     else WrongArgString(2)
     else WrongArg(1,"an xml node")
     else WrongNumArgs(3));
setupfun("xmlAddAttribute",xmlAddAttribute);

xmlAddElement(e:Expr):Expr := (
     -- # typical value: xmlAddElement, LibxmlNode, String, LibxmlNode
     when e is args:Sequence do if length(args) != 2 then WrongNumArgs(3) else
     when args.0 is parent:xmlNodeCell do
     when args.1 is name:stringCell do
     toExpr(AddElement(parent.v,name.v))
     else WrongArgString(2)
     else WrongArg(1,"an xml node")
     else WrongNumArgs(2));
setupfun("xmlAddElement",xmlAddElement);

xmlAddText(e:Expr):Expr := (
     -- # typical value: xmlAddText, LibxmlNode, String, LibxmlNode
     when e is args:Sequence do if length(args) != 2 then WrongNumArgs(3) else
     when args.0 is parent:xmlNodeCell do
     when args.1 is content:stringCell do
     toExpr(AddText(parent.v,content.v))
     else WrongArgString(2)
     else WrongArg(1,"an xml node")
     else WrongNumArgs(2));
setupfun("xmlAddText",xmlAddText);

xmlType(e:Expr):Expr := (
     -- # typical value: xmlType, LibxmlNode, ZZ
     when e
     is node:xmlNodeCell do toExpr(Ccode(int,"((xmlNode*)",node.v,")->type"))
     else WrongArg("an XML node"));
setupfun("xmlType",xmlType);

xmlTypes(e:Expr):Expr := (
     -- # typical value: xmlTypes, Sequence, List
     when e
     is s:Sequence do
     if length(s) == 0 then (
	  Expr(list(
		    new Sequence
		    len 20
		    do (
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_ELEMENT_NODE")),toExpr("element node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_ATTRIBUTE_NODE")),toExpr("attribute node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_TEXT_NODE")),toExpr("text node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_CDATA_SECTION_NODE")),toExpr("cdata section node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_ENTITY_REF_NODE")),toExpr("entity ref node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_ENTITY_NODE")),toExpr("entity node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_PI_NODE")),toExpr("pi node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_COMMENT_NODE")),toExpr("comment node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_DOCUMENT_NODE")),toExpr("document node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_DOCUMENT_TYPE_NODE")),toExpr("document type node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_DOCUMENT_FRAG_NODE")),toExpr("document frag node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_NOTATION_NODE")),toExpr("notation node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_HTML_DOCUMENT_NODE")),toExpr("html document node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_DTD_NODE")),toExpr("dtd node")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_ELEMENT_DECL")),toExpr("element decl")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_ATTRIBUTE_DECL")),toExpr("attribute decl")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_ENTITY_DECL")),toExpr("entity decl")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_NAMESPACE_DECL")),toExpr("namespace decl")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_XINCLUDE_START")),toExpr("xinclude start")));
			 provide Expr(Sequence(toExpr(Ccode(int,"XML_XINCLUDE_END")),toExpr("xinclude end")));
			   -- we omit this one, for now
			   -- #ifdef LIBXML_DOCB_ENABLED
			   --    ,XML_DOCB_DOCUMENT_NODE=	21
			   -- #endif
			 ))))
	  else WrongNumArgs(0)
     else WrongNumArgs(0));
setupfun("xmlTypes", xmlTypes);

xmlDocDump0(e:Expr):Expr := (
     when e
     is node:xmlNodeCell do toExpr(DocDump(node.v))
     else WrongArg("an XML node"));
setupfun("xmlDocDump",xmlDocDump0);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d xmlactors.o "
-- End:
