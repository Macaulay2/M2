-- Copyright 2009 by Daniel R. Grayson
use C;
use xml;
use tokens;
use common;

import parse(s:string):void;
xmlparse(e:Expr):Expr := (
     when e is s:string do (parse(s); nullE)
     else WrongArgString()
     );
setupfun("XML$parse",xmlparse);

import examine(x:xmlNode):void;

xmlParse(e:Expr):Expr := (
     when e is s:string do (
	  when Parse(s)
	  is x:xmlNode do Expr(x)
	  else buildErrorPacket("xml syntax error")
	  )
     else WrongArgString()
     );
setupfun("XML$Parse",xmlParse);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
