--		Copyright 2008 by Daniel R. Grayson

use common;
use tokens;

err(e:Expr):Expr := buildErrorPacket("mysql library not available");
setupfun("mysqlRealConnect",err);
setupfun("mysqlRealQuery",err);
setupfun("mysqlQuery",err);
setupfun("mysqlGetHostInfo", err);
setupfun("mysqlStoreResult", err);
setupfun("mysqlListDbs",err);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
