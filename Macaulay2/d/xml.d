-- Copyright 2009 by Daniel R. Grayson
use C;
-- use tokens;
-- use common;

export xmlNode := { xmlNode:void };
export xmlAttr := { xmlAttr:void };
export xmlNodeOrNull := xmlNode or null;
export xmlAttrOrNull := xmlAttr or null;
-- these routines are implemented in xml-c.c
import Attributes(node:xmlNode):xmlAttrOrNull;
import Parse(text:string):xmlNodeOrNull;
import examine(x:xmlNode):void;
import getAttrChildren(node:xmlAttr):xmlNodeOrNull;
import getElementName(node:xmlNode):(string or null);
import getAttrName(attr:xmlAttr):(string or null);
import getContent(node:xmlNode):(string or null);
import getNextAttr(attr:xmlAttr):xmlAttrOrNull;
import getNextNode(node:xmlNode):xmlNodeOrNull;
import getNodeChildren(node:xmlNode):xmlNodeOrNull;
import isElement(node:xmlNode):bool;
import isText(node:xmlNode):bool;
-- import parse(s:string):void;

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
