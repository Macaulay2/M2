-- Copyright 2009 by Daniel R. Grayson
use common;
use util;

-- xmlparse(e:Expr):Expr := (
--      when e is s:string do (parse(s); nullE)
--      else WrongArgString()
--      );
-- setupfun("XML$parse",xmlparse);

xmlAttributes(e:Expr):Expr := (
     when e is node:xmlNodeCell do when Attributes(node.v) is attr:xmlAttr do toExpr(attr) else nullE
     else WrongArg("an XML node"));
setupfun("xmlAttributes",xmlAttributes);
xmlParse(e:Expr):Expr := (
     when e is s:stringCell do when Parse(s.v) is x:xmlNode do toExpr(x)
     else buildErrorPacket("xml syntax error")
     else WrongArgString());
setupfun("xmlParse",xmlParse);
xmlGetChildren(e:Expr):Expr := (
     when e
     is node:xmlNodeCell do when getNodeChildren(node.v) is child:xmlNode do toExpr(child) else nullE
     is attr:xmlAttrCell do when getAttrChildren(attr.v) is child:xmlNode do toExpr(child) else nullE
     else WrongArg("an XML node or attribute"));
setupfun("xmlGetChildren",xmlGetChildren);
xmlGetName(e:Expr):Expr := (
     when e
     is node:xmlNodeCell do if isElement(node.v) then when getElementName(node.v) is s:string do toExpr(s) else nullE else nullE
     is attr:xmlAttrCell do when getAttrName(attr.v) is s:string do toExpr(s) else nullE
     else WrongArg("an XML node"));
setupfun("xmlGetName",xmlGetName);
xmlGetContent(e:Expr):Expr := (
     when e
     is node:xmlNodeCell do if isText(node.v) then when getContent(node.v) is s:string do toExpr(s) else nullE else nullE
     else WrongArg("an XML node"));
setupfun("xmlGetContent",xmlGetContent);
xmlGetNext(e:Expr):Expr := (
     when e
     is node:xmlNodeCell do when getNextNode(node.v) is next:xmlNode do toExpr(next) else nullE
     is attr:xmlAttrCell do when getNextAttr(attr.v) is next:xmlAttr do toExpr(next) else nullE
     else WrongArg("an XML node or attribute"));
setupfun("xmlGetNext",xmlGetNext);
xmlIsElement(e:Expr):Expr := (
     when e
     is node:xmlNodeCell do toExpr(isElement(node.v))
     else WrongArg("an XML node"));
setupfun("xmlIsElement",xmlIsElement);
xmlIsText(e:Expr):Expr := (
     when e
     is node:xmlNodeCell do toExpr(isText(node.v))
     else WrongArg("an XML node"));
setupfun("xmlIsText",xmlIsText);

xmlNewDoc(e:Expr):Expr := (
     when e is s:stringCell do toExpr(NewDoc("1.0",s.v))
     else WrongArgString());
setupfun("xmlNewDoc",xmlNewDoc);

xmlNewProp(e:Expr):Expr := (
     when e is args:Sequence do if length(args) != 3 then WrongNumArgs(3) else
     when args.0 is parent:xmlNodeCell do
     when args.1 is name:stringCell do
     when args.2 is value:stringCell do
     toExpr(NewProp(parent.v,name.v,value.v))
     else WrongArgString(3)
     else WrongArgString(2)
     else WrongArg(1,"an xml node")
     else WrongNumArgs(3));
setupfun("xmlNewProp",xmlNewProp);

xmlNewChild(e:Expr):Expr := (
     when e is args:Sequence do if length(args) != 2 then WrongNumArgs(3) else
     when args.0 is parent:xmlNodeCell do
     when args.1 is name:stringCell do
     toExpr(NewChild(parent.v,name.v))
     else WrongArgString(2)
     else WrongArg(1,"an xml node")
     else WrongNumArgs(2));
setupfun("xmlNewChild",xmlNewChild);

xmlNewText(e:Expr):Expr := (
     when e is args:Sequence do if length(args) != 2 then WrongNumArgs(3) else
     when args.0 is parent:xmlNodeCell do
     when args.1 is content:stringCell do
     toExpr(NewText(parent.v,content.v))
     else WrongArgString(2)
     else WrongArg(1,"an xml node")
     else WrongNumArgs(2));
setupfun("xmlNewText",xmlNewText);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d xmlactors.o "
-- End:
