-- Copyright 2009 by Daniel R. Grayson
use C;
use tokens;
use common;

import parse(s:string):void;
xmlparse(e:Expr):Expr := (
     when e is s:string do (parse(s); nullE)
     else WrongArgString()
     );
setupfun("XML$parse",xmlparse);


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
