--		Copyright 2008 by Daniel R. Grayson

use common;

err(e:Expr):Expr := buildErrorPacket("mysql library not available");
setupfun("mysqlRealConnect",err);
setupfun("mysqlRealQuery",err);
setupfun("mysqlQuery",err);
setupfun("mysqlGetHostInfo", err);
setupfun("mysqlStoreResult", err);
setupfun("mysqlUseResult",err);
setupfun("mysqlListDbs",err);
setupfun("mysqlFetchRow",err);
setupfun("mysqlFetchField",err);
setupfun("mysqlListFields",err);
setupfun("mysqlGetServerInfo",err);
setupfun("mysqlGetServerVersion", err);
setupfun("mysqlInfo", err);
setupfun("mysqlPing", err);
setupfun("mysqlDebug", err);
setupfun("mysqlNextResult", err);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d mysqldummy.o "
-- End:
