-- Copyright 2008 by Daniel R. Grayson
-- M2 interface to the mysql C library
-- documentation:
--   http://dev.mysql.com/doc/refman/5.0/en/c.html
--   /usr/share/doc/mysql-doc-5.0/refman-5.0-en.html-chapter/index.html

use C;
use stdio;
use gmp;
use tokens;
use common;

mysqlError(mysql:MysqlConnection):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     buildErrorPacket(Ccode(string, "tostring2(mysql_error((MYSQL*)",mysql,"))"))
     );

Mysqlfinalizer(m:MysqlConnectionWrapper, msg:string):void := (
     if m.open then (
	  Ccode(void,"mysql_close((MYSQL*)",m.mysql,")");
	  m.open = false;
	  );
     );

export MysqlFieldOrNULL := MysqlField or null;
export toExpr(mysql:MysqlConnection, x:MysqlFieldOrNULL):Expr := when x is res:MysqlField do Expr(res) else mysqlError(mysql);

export MysqlResultOrNULL := MysqlResult or null;
export toExpr(mysql:MysqlConnection, x:MysqlResultOrNULL):Expr := when x is res:MysqlResult do Expr(res) else mysqlError(mysql);

export MysqlConnectionOrNULL := MysqlConnection or null; 
export toExpr(mysql:MysqlConnection, x:MysqlConnectionOrNULL):Expr := (
     when x is mysql:MysqlConnection do (
	  msg := "MysqlConnection";
	  mw := MysqlConnectionWrapper(mysql,true);
	  Ccode(void, "GC_register_finalizer((void *)",mw,",(GC_finalization_proc)",Mysqlfinalizer,",",msg,",0,0)");
	  Expr(mw))
     else mysqlError(mysql)
     );

mysqlRealConnect(e:Expr):Expr := (
     Ccode(void, "extern char *tocharstar(string)");
     Ccode(void, "const char *nullstringer(const char *)");
     when e is s:Sequence do (
	  if length(s) != 6 then return WrongNumArgs(6);
	  when s.0 is host:string do
	  when s.1 is user:string do
	  when s.2 is passwd:string do
	  when s.3 is db:string do
	  when s.4 is port:ZZ do if !isInt(port) then WrongArgSmallInteger(5) else
     	  when s.5 is unixSocket:string do (
	       mysql := Ccode(MysqlConnection, "(tokens_MysqlConnection)mysql_init(0)");
	       if 0 != Ccode(int,"mysql_options((MYSQL*)",mysql,", MYSQL_SET_CHARSET_NAME, \"utf8\")") then return mysqlError(mysql);
	       toExpr(mysql,Ccode(MysqlConnectionOrNULL, "(mysql_MysqlConnectionOrNULL)mysql_real_connect(",
			 "(MYSQL*)",mysql,",",
			 "nullstringer(tocharstar(",host,")),",
			 "nullstringer(tocharstar(",user,")),",
			 "nullstringer(tocharstar(",passwd,")),",
			 "nullstringer(tocharstar(",db,")),",
			 toInt(port),",",
			 "nullstringer(tocharstar(",unixSocket,")),",
			 "(unsigned long)0",				    -- client_flag
			 ")"
			 )))
	  else WrongArgString(6)
	  else WrongArgZZ(5)
	  else WrongArgString(4)
	  else WrongArgString(3)
	  else WrongArgString(2)
	  else WrongArgString(1)
	  )
     else WrongNumArgs(6));	  
setupfun("mysqlRealConnect",mysqlRealConnect);

mysqlRealQuery(e:Expr):Expr := (
     when e is s:Sequence do 
     when s.0 is m:MysqlConnectionWrapper do
     if !m.open then WrongArg(1,"an open connection to a mysql database") else
     when s.1 is query:string do
     if 0 == Ccode(int, "mysql_real_query(",
	  "(MYSQL*)", m.mysql, ",", 
	  query, "->array_,",
	  query, "->len_",
	  ")")
     then s.0
     else mysqlError(m.mysql)
     else WrongArgString(2)
     else WrongArg(1,"a connection to a mysql database")
     else WrongNumArgs(2));
setupfun("mysqlRealQuery",mysqlRealQuery);
setupfun("mysqlQuery",mysqlRealQuery);

mysqlGetHostInfo(e:Expr):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     when e is m:MysqlConnectionWrapper do Expr(
	  if m.open
	  then Ccode(string, "tostring2(mysql_get_host_info((MYSQL*)", m.mysql, "))" )
	  else "closed")
     else WrongArg("a connection to a mysql database"));
setupfun("mysqlGetHostInfo", mysqlGetHostInfo);

mysqlStoreResult(e:Expr):Expr := (
     when e is m:MysqlConnectionWrapper do (
	  if m.open
	  then toExpr(m.mysql, Ccode(MysqlResultOrNULL, "(mysql_MysqlResultOrNULL)mysql_store_result((MYSQL*)", m.mysql, ")" ))
	  else WrongArg("an open connection to a mysql database"))
     else WrongArg("a connection to a mysql database"));
setupfun("mysqlStoreResult",mysqlStoreResult);

mysqlListDbs(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is m:MysqlConnectionWrapper do 
     if !m.open then WrongArg(1,"an open connection to a mysql database") else
     when s.1 is wild:string do (
	  NotYet("mysqlListDbs")
	  )
     else WrongArgString(2)
     else WrongArg(1,"a connection to a mysql database")
     else WrongNumArgs(2));
setupfun("mysqlListDbs",mysqlListDbs);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
