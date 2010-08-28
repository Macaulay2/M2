-- Copyright 2009 by Daniel R. Grayson
use common;
use util;

-- xmlparse(e:Expr):Expr := (
--      when e is s:string do (parse(s); nullE)
--      else WrongArgString()
--      );
-- setupfun("XML$parse",xmlparse);

xmlFirstAttribute(e:Expr):Expr := (
     -- # typical value: xmlFirstAttribute, LibxmlNode, LibxmlAttribute
     when e is node:xmlNodeCell do when Attributes(node.v) is attr:xmlAttr do toExpr(attr) else nullE
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
     is node:xmlNodeCell do when getNodeChildren(node.v) is child:xmlNode do toExpr(child) else nullE
     is attr:xmlAttrCell do when getAttrChildren(attr.v) is child:xmlNode do toExpr(child) else nullE
     else WrongArg("an XML node or attribute"));
setupfun("xmlFirstChild",xmlFirstChild);
xmlGetName(e:Expr):Expr := (
     -- # typical value: xmlGetName, LibxmlNode, String
     -- # typical value: xmlGetName, LibxmlAttribute, String
     when e
     is node:xmlNodeCell do if isElement(node.v) then when getElementName(node.v) is s:string do toExpr(s) else nullE else nullE
     is attr:xmlAttrCell do when getAttrName(attr.v) is s:string do toExpr(s) else nullE
     else WrongArg("an XML node"));
setupfun("xmlGetName",xmlGetName);
xmlGetContent(e:Expr):Expr := (
     -- # typical value: xmlGetContent, LibxmlNode, String
     when e
     is node:xmlNodeCell do if isText(node.v) then when getContent(node.v) is s:string do toExpr(s) else nullE else nullE
     else WrongArg("an XML node"));
setupfun("xmlGetContent",xmlGetContent);
xmlGetNext(e:Expr):Expr := (
     -- # typical value: xmlGetNext, LibxmlNode, LibxmlNode
     -- # typical value: xmlGetNext, LibxmlAttribute, LibxmlAttribute
     when e
     is node:xmlNodeCell do when getNextNode(node.v) is next:xmlNode do toExpr(next) else nullE
     is attr:xmlAttrCell do when getNextAttr(attr.v) is next:xmlAttr do toExpr(next) else nullE
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

xmlNewDoc(e:Expr):Expr := (
     -- # typical value: xmlNewDoc, String, LibxmlNode
     when e is s:stringCell do toExpr(NewDoc("1.0",s.v))
     else WrongArgString());
setupfun("xmlNewDoc",xmlNewDoc);

xmlAddAttribute(e:Expr):Expr := (
     -- # typical value: xmlAddAttribute, LibxmlNode, String, String, LibxmlNode
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

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d xmlactors.o "
-- End:
