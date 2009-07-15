-- Copyright 2009 by Daniel R. Grayson
use C;
-- use tokens;
-- use common;

export xmlNode := { xmlNode:void };
export xmlAttr := { xmlAttr:void };
export xmlNodeOrNull := xmlNode or null;
export xmlAttrOrNull := xmlAttr or null;
import Parse(text:string):xmlNodeOrNull;
import Attributes(node:xmlNode):xmlAttrOrNull;
import isElement(node:xmlNode):bool;
import isText(node:xmlNode):bool;
import getElementName(node:xmlNode):string;
import getNextNode(node:xmlNode):xmlNodeOrNull;
import getNextAttr(attr:xmlAttr):xmlAttrOrNull;
import getNodeChildren(node:xmlNode):xmlNodeOrNull;
import getAttrChildren(node:xmlAttr):xmlNodeOrNull;

-- import parse(s:string):void;
-- xmlparse(e:Expr):Expr := (
--      when e is s:string do (parse(s); nullE)
--      else WrongArgString()
--      );
-- setupfun("XML$parse",xmlparse);


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
