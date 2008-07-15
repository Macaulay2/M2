-- Copyright 2008 by Daniel R. Grayson
-- M2 interface to the mysql C library
-- documentation:
--   http://dev.mysql.com/doc/refman/5.0/en/c.html
--   /usr/share/doc/mysql-doc-5.0/refman-5.0-en.html-chapter/index.html

use C;
use stdio;
use gmp;
use tokens;
use objects;
use common;
use basic;
use util;

mysqlErrno(mysql:MysqlConnection):int ::= Ccode(int,"mysql_errno((MYSQL*)",mysql,")");

mysqlError(mysql:MysqlConnection):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     buildErrorPacket(
	  if 0 == mysqlErrno(mysql)
	  then "unknown mysql error"
	  else Ccode(string, "tostring2(mysql_error((MYSQL*)",mysql,"))")));
possibleMysqlError(mysql:MysqlConnection):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     if 0 == mysqlErrno(mysql)
     then nullE
     else buildErrorPacket(Ccode(string, "tostring2(mysql_error((MYSQL*)",mysql,"))")));
mysqlConnectionFinalizer(m:MysqlConnectionWrapper, msg:string):void := (
     if m.open then (
	  Ccode(void,"mysql_close((MYSQL*)",m.mysql,")");
	  m.open = false;
	  );
     );
mysqlResultFinalizer(m:MysqlResultWrapper, msg:string):void := Ccode(void,"mysql_free_result((MYSQL_RES*)",m.res,")");

export MysqlFieldOrNull := MysqlField or null;
export toExpr(res:MysqlResultWrapper, x:MysqlFieldOrNull):Expr := (
     when x is fld:MysqlField 
     do Expr(MysqlFieldWrapper(res,fld)) 
     else mysqlError(res.connection.mysql)
     );

export MysqlResultOrNull := MysqlResult or null;
export toExpr(conn:MysqlConnectionWrapper, x:MysqlResultOrNull):Expr := (
     when x is res:MysqlResult do (
	  msg := "MysqlResult";
	  rw := MysqlResultWrapper(conn,res);
	  Ccode(void, "GC_register_finalizer((void *)",rw,",(GC_finalization_proc)",mysqlResultFinalizer,",",msg,",0,0)");
	  Expr(rw))
     else mysqlError(conn.mysql)
     );

export MysqlConnectionOrNull := MysqlConnection or null; 
export toExpr(mysql:MysqlConnection, x:MysqlConnectionOrNull):Expr := (
     when x is mysql:MysqlConnection do (
	  msg := "MysqlConnection";
	  mw := MysqlConnectionWrapper(mysql,true);
	  Ccode(void, "GC_register_finalizer((void *)",mw,",(GC_finalization_proc)",mysqlConnectionFinalizer,",",msg,",0,0)");
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
	       toExpr(mysql,Ccode(MysqlConnectionOrNull, "(mysql_MysqlConnectionOrNull)mysql_real_connect(",
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

mysqlGetHostInfo(e:Expr):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     when e is m:MysqlConnectionWrapper do (
	  if m.open
	  then Expr(Ccode(string, "tostring2(mysql_get_host_info((MYSQL*)", m.mysql, "))" ))
	  else WrongArg("an open connection to a mysql database"))
     else WrongArg("a connection to a mysql database"));
setupfun("mysqlGetHostInfo", mysqlGetHostInfo);

mysqlStoreResult(e:Expr):Expr := (
     when e is m:MysqlConnectionWrapper do (
	  if m.open
	  then toExpr(m, Ccode(MysqlResultOrNull, "(mysql_MysqlResultOrNull)mysql_store_result((MYSQL*)", m.mysql, ")" ))
	  else WrongArg("an open connection to a mysql database"))
     else WrongArg("a connection to a mysql database"));
setupfun("mysqlStoreResult",mysqlStoreResult);

mysqlUseResult(e:Expr):Expr := (
     when e is m:MysqlConnectionWrapper do (
	  if m.open
	  then (
	       res := Ccode(MysqlResultOrNull, "(mysql_MysqlResultOrNull)mysql_use_result((MYSQL*)", m.mysql, ")" );
	       when res is null do (
		    if mysqlErrno(m.mysql) == 0 then return buildErrorPacket("mysqlUseResult: no results available");
		    )
	       else nothing;
	       toExpr(m, res)
	       )
	  else WrongArg("an open connection to a mysql database"))
     else WrongArg("a connection to a mysql database"));
setupfun("mysqlUseResult",mysqlUseResult);

mysqlListDbs(e:Expr):Expr := (
     Ccode(void, "extern char *tocharstar(string)");
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is m:MysqlConnectionWrapper do 
     if !m.open then WrongArg(1,"an open connection to a mysql database") else
     when s.1 is wild:string do
     toExpr(m, Ccode(MysqlResultOrNull, "(mysql_MysqlResultOrNull)mysql_list_dbs((MYSQL*)", m.mysql, ", tocharstar(", wild, "))" ))
     else WrongArgString(2)
     else WrongArg(1,"a connection to a mysql database")
     else WrongNumArgs(2));
setupfun("mysqlListDbs",mysqlListDbs);

CharStarStar := {ptr:CharStarOrNull};
CharStarStarOrNull := CharStarStar or null;
ULongStar := {x:ulong};
ULongStarOrNull := ULongStar or null;

(s:CharStar    ) + (i:int) : CharStar     ::= Ccode(CharStar, "(", s, "+", i, ")" );
(s:CharStarStar) + (i:int) : CharStarStar ::= Ccode(CharStarStar, "(", s, "+", i, ")" );
(s:ULongStar   ) + (i:int) : ULongStar    ::= Ccode(ULongStar, "(", s, "+", i, ")" );

mysqlFetchRow(e:Expr):Expr := (
     Ccode(void, "extern void *tostrings(int,char **)");
     Ccode(void, "extern void *tostringn(char *s,int n)");
     when e is rw:MysqlResultWrapper do (
	  row := Ccode(CharStarStarOrNull, "(CharStarStarOrNull)mysql_fetch_row((MYSQL_RES*)",rw.res,")");
	  when row
	  is null do possibleMysqlError(rw.connection.mysql)
	  is rowp:CharStarStar do (
	       nflds := Ccode(int, "mysql_num_fields((MYSQL_RES*)",rw.res,")");
	       lens  := Ccode(ULongStar, "(ULongStar)mysql_fetch_lengths((MYSQL_RES*)",rw.res,")");
	       Expr(
		    list (
			 new Sequence len nflds do
			 for i from 0 to nflds-1 do 
			 provide (
			      q := (rowp + i).ptr;
			      when q is null do nullE
			      else Expr(Ccode(string,"tostringn((char *)",q, ",(int)", (lens+i).x, ")")))))))
     else WrongArg("a mysql result set"));
setupfun("mysqlFetchRow",mysqlFetchRow);

mysqlFetchField(e:Expr):Expr := (
     when e is rw:MysqlResultWrapper do (
     	  when Ccode(MysqlFieldOrNull, "(mysql_MysqlFieldOrNull)mysql_fetch_field((MYSQL_RES*)",rw.res,")")
	  is fld:MysqlField do Expr(MysqlFieldWrapper(rw,fld))
	  else nullE)
     else WrongArg("a mysql result set"));
setupfun("mysqlFetchField",mysqlFetchField);

mysqlListFields(e:Expr):Expr := (
     Ccode(void, "extern char *tocharstar(string)");
     Ccode(void, "const char *nullstringer(const char *)");
     when e is s:Sequence do
     if length(s) != 3 then WrongNumArgs(3) else
     when s.0 is conn:MysqlConnectionWrapper do
     if !conn.open then WrongArg(1,"an open connection to a mysql database") else
     when s.1 is table:string do 
     when s.2 is wild:string do
     toExpr(conn,Ccode(MysqlResultOrNull,"(mysql_MysqlResultOrNull)mysql_list_fields(",
	       "(MYSQL*)", conn.mysql, ",",
	       "tocharstar(", table, "),",
	       "nullstringer(tocharstar(", wild, "))",
	       ")"))
     else WrongArgString(3)
     else WrongArgString(2)
     else WrongArg(1,"a connection to a mysql database")
     else WrongNumArgs(3));
setupfun("mysqlListFields",mysqlListFields);

mysqlGetServerInfo(e:Expr):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     when e is m:MysqlConnectionWrapper do (
	  if m.open
	  then Expr(Ccode(string, "tostring2(mysql_get_server_info((MYSQL*)", m.mysql, "))" ))
	  else WrongArg("an open connection to a mysql database"))
     else WrongArg("a connection to a mysql database"));
setupfun("mysqlGetServerInfo", mysqlGetServerInfo);

mysqlGetServerVersion(e:Expr):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     when e is m:MysqlConnectionWrapper do (
	  if m.open
	  then toExpr(Ccode(ulong, "mysql_get_server_version((MYSQL*)", m.mysql, ")" ))
	  else WrongArg("an open connection to a mysql database"))
     else WrongArg("a connection to a mysql database"));
setupfun("mysqlGetServerVersion", mysqlGetServerVersion);

mysqlInfo(e:Expr):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     when e is m:MysqlConnectionWrapper do (
	  if m.open
	  then (
	       ptr := Ccode(CharStarOrNull, "(CharStarOrNull)mysql_info((MYSQL*)", m.mysql, ")");
	       when ptr is null do nullE
	       is str:CharStar do Expr(Ccode(string, "tostring2((char *)",str, ")" )))	       
	  else WrongArg("an open connection to a mysql database"))
     else WrongArg("a connection to a mysql database"));
setupfun("mysqlInfo", mysqlInfo);

mysqlPing(e:Expr):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     when e is m:MysqlConnectionWrapper do (
	  if m.open
	  then (
	       if 0 != Ccode(int, "mysql_ping((MYSQL*)", m.mysql, ")")
	       then mysqlError(m.mysql)
	       else Expr("alive"))
	  else WrongArg("an open connection to a mysql database"))
     else WrongArg("a connection to a mysql database"));
setupfun("mysqlPing", mysqlPing);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
