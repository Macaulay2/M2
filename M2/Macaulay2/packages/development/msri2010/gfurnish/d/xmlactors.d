-- Copyright 2009 by Daniel R. Grayson
use C;
use xml;
use tokens;
use common;


-- xmlparse(e:Expr):Expr := (
--      when e is s:string do (parse(s); nullE)
--      else WrongArgString()
--      );
-- setupfun("XML$parse",xmlparse);

xmlAttributes(e:Expr):Expr := (
     when e is node:xmlNode do when Attributes(node) is attr:xmlAttr do Expr(attr) else nullE
     else WrongArg("an XML node"));
setupfun("xmlAttributes",xmlAttributes);
xmlParse(e:Expr):Expr := (
     when e is s:string do when Parse(s) is x:xmlNode do Expr(x)
     else buildErrorPacket("xml syntax error")
     else WrongArgString());
setupfun("xmlParse",xmlParse);
xmlGetChildren(e:Expr):Expr := (
     when e
     is node:xmlNode do when getNodeChildren(node) is child:xmlNode do Expr(child) else nullE
     is attr:xmlAttr do when getAttrChildren(attr) is child:xmlNode do Expr(child) else nullE
     else WrongArg("an XML node or attribute"));
setupfun("xmlGetChildren",xmlGetChildren);
xmlGetName(e:Expr):Expr := (
     when e
     is node:xmlNode do if isElement(node) then when getElementName(node) is s:string do Expr(s) else nullE else nullE
     is attr:xmlAttr do when getAttrName(attr) is s:string do Expr(s) else nullE
     else WrongArg("an XML node"));
setupfun("xmlGetName",xmlGetName);
xmlGetContent(e:Expr):Expr := (
     when e
     is node:xmlNode do if isText(node) then when getContent(node) is s:string do Expr(s) else nullE else nullE
     else WrongArg("an XML node"));
setupfun("xmlGetContent",xmlGetContent);
xmlGetNext(e:Expr):Expr := (
     when e
     is node:xmlNode do when getNextNode(node) is next:xmlNode do Expr(next) else nullE
     is attr:xmlAttr do when getNextAttr(attr) is next:xmlAttr do Expr(next) else nullE
     else WrongArg("an XML node or attribute"));
setupfun("xmlGetNext",xmlGetNext);
xmlIsElement(e:Expr):Expr := (
     when e
     is node:xmlNode do toExpr(isElement(node))
     else WrongArg("an XML node"));
setupfun("xmlIsElement",xmlIsElement);
xmlIsText(e:Expr):Expr := (
     when e
     is node:xmlNode do toExpr(isText(node))
     else WrongArg("an XML node"));
setupfun("xmlIsText",xmlIsText);

xmlNewDoc(e:Expr):Expr := (
     when e is s:string do Expr(NewDoc("1.0",s))
     else WrongArgString());
setupfun("xmlNewDoc",xmlNewDoc);

xmlNewProp(e:Expr):Expr := (
     when e is args:Sequence do if length(args) != 3 then WrongNumArgs(3) else
     when args.0 is parent:xmlNode do
     when args.1 is name:string do
     when args.2 is value:string do
     Expr(NewProp(parent,name,value))
     else WrongArgString(3)
     else WrongArgString(2)
     else WrongArg(1,"an xml node")
     else WrongNumArgs(3));
setupfun("xmlNewProp",xmlNewProp);

xmlNewChild(e:Expr):Expr := (
     when e is args:Sequence do if length(args) != 2 then WrongNumArgs(3) else
     when args.0 is parent:xmlNode do
     when args.1 is name:string do
     Expr(NewChild(parent,name))
     else WrongArgString(2)
     else WrongArg(1,"an xml node")
     else WrongNumArgs(2));
setupfun("xmlNewChild",xmlNewChild);

xmlNewText(e:Expr):Expr := (
     when e is args:Sequence do if length(args) != 2 then WrongNumArgs(3) else
     when args.0 is parent:xmlNode do
     when args.1 is content:string do
     Expr(NewText(parent,content))
     else WrongArgString(2)
     else WrongArg(1,"an xml node")
     else WrongNumArgs(2));
setupfun("xmlNewText",xmlNewText);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
