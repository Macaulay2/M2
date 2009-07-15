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
     else WrongArg("an XML node or attribute"));
setupfun("xmlGetName",xmlGetName);
xmlGetContent(e:Expr):Expr := (
     when e
     is node:xmlNode do if isText(node) then when getContent(node) is s:string do Expr(s) else nullE else nullE
     else WrongArg("an XML node or attribute"));
setupfun("xmlGetContent",xmlGetContent);
xmlGetNext(e:Expr):Expr := (
     when e
     is node:xmlNode do when getNextNode(node) is next:xmlNode do Expr(next) else nullE
     is attr:xmlAttr do when getNextAttr(attr) is next:xmlAttr do Expr(next) else nullE
     else WrongArg("an XML node or attribute"));
setupfun("xmlGetNext",xmlGetNext);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
