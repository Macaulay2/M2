-- Copyright 2009 by Daniel R. Grayson
use M2;

-- TO DO: declare these types in this file and have xml-c.c include xml-exports.h, for better type checking:
declarations "
#include <libxml/parser.h>
#include <libxml/tree.h>
";
export xmlNode := Pointer "xmlNode *";
export xmlAttr := Pointer "xmlAttr *";
export xmlNodeCell := {+ v:xmlNode };
export xmlAttrCell := {+ v:xmlAttr };
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
-- import parse(s:string):void;
import NewRoot(version:string,name:string):xmlNode;
import AddAttribute(parent:xmlNode,name:string,value:string):xmlAttr;
import AddElement(parent:xmlNode,name:string):xmlNode;
import AddText(parent:xmlNode,content:string):xmlNode;
import toString(n:xmlNode):string;
import DocDump(n:xmlNode):string;

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d xml.o "
-- End:
