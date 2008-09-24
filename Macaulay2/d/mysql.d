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
     when m.mysql
     is null do nothing
     is n:MysqlConnection do (
	  Ccode(void,"mysql_close((MYSQL*)",n,")");
	  m.mysql = null();
	  );
     );
mysqlResultFinalizer(m:MysqlResultWrapper, msg:string):void := Ccode(void,"mysql_free_result((MYSQL_RES*)",m.res,")");

export MysqlFieldOrNull := MysqlField or null;
export toExpr(conn:MysqlConnection, rw:MysqlResultWrapper, x:MysqlFieldOrNull):Expr := (
     when x is fld:MysqlField 
     do Expr(MysqlFieldWrapper(rw,fld)) 
     else mysqlError(conn));

export MysqlResultOrNull := MysqlResult or null;
export toExpr(mysql:MysqlConnection, conn:MysqlConnectionWrapper, x:MysqlResultOrNull):Expr := (
     when x is res:MysqlResult do (
	  msg := "MysqlResult";
	  rw := MysqlResultWrapper(conn,res);
	  Ccode(void, "GC_register_finalizer((void *)",rw,",(GC_finalization_proc)",mysqlResultFinalizer,",",msg,",0,0)");
	  Expr(rw))
     else mysqlError(mysql));

export MysqlConnectionOrNull := MysqlConnection or null; 
export toExpr(mysql:MysqlConnection, x:MysqlConnectionOrNull):Expr := (
     when x is mysql:MysqlConnection do (
	  msg := "MysqlConnection";
	  mw := MysqlConnectionWrapper(mysql);
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
			 "(unsigned long)(CLIENT_MULTI_STATEMENTS)",
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

errNotOpen():Expr := WrongArg("an open connection to a mysql server");
errNotOpen(i:int):Expr := WrongArg(i,"an open connection to a mysql server");
errNotConn():Expr := WrongArg("a connection to a mysql server");
errNotConn(i:int):Expr := WrongArg(i,"a connection to a mysql server");

mysqlRealQuery(e:Expr):Expr := (
     when e is s:Sequence do 
     when s.0 is mw:MysqlConnectionWrapper do (
	  when mw.mysql is null do errNotOpen(1) is m:MysqlConnection do 
	  when s.1 is query:string do (
	       if 0 == Ccode(int, "mysql_real_query(", "(MYSQL*)", m, ",", query, "->array_,", query, "->len_", ")")
	       then s.0
	       else mysqlError(m))
	  else WrongArgString(2))
     else errNotConn(1)
     else WrongNumArgs(2));
setupfun("mysqlRealQuery",mysqlRealQuery);

mysqlGetHostInfo(e:Expr):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     when e is mw:MysqlConnectionWrapper do (
	  when mw.mysql 
	  is null do errNotOpen()
	  is mysql:MysqlConnection do Expr(Ccode(string, "tostring2(mysql_get_host_info((MYSQL*)", mysql, "))" )))
     else errNotConn());
setupfun("mysqlGetHostInfo", mysqlGetHostInfo);

mysqlStoreResult(e:Expr):Expr := (
     when e is mw:MysqlConnectionWrapper do (
	  when mw.mysql 
	  is null do errNotOpen()
	  is mysql:MysqlConnection do (
	       r := Ccode(MysqlResultOrNull, "(mysql_MysqlResultOrNull)mysql_store_result((MYSQL*)", mysql, ")" );
	       when r is null do (
		    if 0 == mysqlErrno(mysql) then return buildErrorPacket("mysqlStoreResult: no results available");
		    )
	       else nothing;
	       toExpr(mysql, mw, r)
	       ))
     else errNotConn());
setupfun("mysqlStoreResult",mysqlStoreResult);

mysqlUseResult(e:Expr):Expr := (
     when e is mw:MysqlConnectionWrapper do (
	  when mw.mysql 
	  is null do errNotOpen()
	  is mysql:MysqlConnection do (
	       res := Ccode(MysqlResultOrNull, "(mysql_MysqlResultOrNull)mysql_use_result((MYSQL*)", mysql, ")" );
	       when res is null do (
		    if mysqlErrno(mysql) == 0 then return buildErrorPacket("mysqlUseResult: no results available");
		    )
	       else nothing;
	       toExpr(mysql, mw, res)))
     else errNotConn());
setupfun("mysqlUseResult",mysqlUseResult);

mysqlListDbs(e:Expr):Expr := (
     Ccode(void, "extern char *tocharstar(string)");
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is mw:MysqlConnectionWrapper do 
     when mw.mysql
     is mysql:MysqlConnection do 
     when s.1 is wild:string do
     toExpr(mysql, mw, Ccode(MysqlResultOrNull, "(mysql_MysqlResultOrNull)mysql_list_dbs((MYSQL*)", mysql, ", tocharstar(", wild, "))" ))
     else WrongArgString(2)
     else errNotOpen(1)
     else errNotConn(1)
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
     when e is rw:MysqlResultWrapper do 
     when rw.connection.mysql is mysql:MysqlConnection do (
	  row := Ccode(CharStarStarOrNull, "(CharStarStarOrNull)mysql_fetch_row((MYSQL_RES*)",rw.res,")");
	  when row
	  is null do possibleMysqlError(mysql)
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
     else WrongArg("a mysql result set for an open mysql connection")
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
     when conn.mysql is mysql:MysqlConnection do 
     when s.1 is table:string do 
     when s.2 is wild:string do
     toExpr(mysql,conn,Ccode(MysqlResultOrNull,"(mysql_MysqlResultOrNull)mysql_list_fields(",
	       "(MYSQL*)", conn.mysql, ",",
	       "tocharstar(", table, "),",
	       "nullstringer(tocharstar(", wild, "))",
	       ")"))
     else WrongArgString(3)
     else WrongArgString(2)
     else errNotOpen(1)
     else errNotConn(1)
     else WrongNumArgs(3));
setupfun("mysqlListFields",mysqlListFields);

mysqlGetServerInfo(e:Expr):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     when e is mw:MysqlConnectionWrapper do 
     when mw.mysql is mysql:MysqlConnection 
     do Expr(Ccode(string, "tostring2(mysql_get_server_info((MYSQL*)", mysql, "))" ))
     else errNotOpen()
     else errNotConn());
setupfun("mysqlGetServerInfo", mysqlGetServerInfo);

mysqlGetServerVersion(e:Expr):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     when e is mw:MysqlConnectionWrapper do (
	  when mw.mysql is mysql:MysqlConnection 
	  do toExpr(Ccode(ulong, "mysql_get_server_version((MYSQL*)", mysql, ")" ))
	  else errNotOpen())
     else errNotConn());
setupfun("mysqlGetServerVersion", mysqlGetServerVersion);

mysqlInfo(e:Expr):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     when e is mw:MysqlConnectionWrapper do (
	  when mw.mysql is mysql:MysqlConnection do (
	       ptr := Ccode(CharStarOrNull, "(CharStarOrNull)mysql_info((MYSQL*)", mysql, ")");
	       when ptr is null do nullE
	       is str:CharStar do Expr(Ccode(string, "tostring2((char *)",str, ")" )))	       
	  else errNotOpen())
     else errNotConn());
setupfun("mysqlInfo", mysqlInfo);

mysqlPing(e:Expr):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     when e is mw:MysqlConnectionWrapper do (
	  when mw.mysql is mysql:MysqlConnection do
	  if 0 != Ccode(int, "mysql_ping((MYSQL*)", mysql, ")")
	  then mysqlError(mysql)
	  else Expr("alive")
	  else errNotOpen())
     else errNotConn());
setupfun("mysqlPing", mysqlPing);

mysqlDebug(e:Expr): Expr := (
     Ccode(void, "extern char *tocharstar(string)");
     Ccode(void, "const char *nullstringer(const char *)");
     when e is debug:string do (
	  Ccode(void,"mysql_debug(nullstringer(tocharstar(", debug, ")))");
	  nullE)
     else WrongArgString());
setupfun("mysqlDebug", mysqlDebug);

mysqlNextResult(e:Expr): Expr := (
     when e is cw:MysqlConnectionWrapper do (
	  when cw.mysql is mysql:MysqlConnection do (
	       r := Ccode(int,"mysql_next_result((MYSQL*)", mysql, ")");
	       if r > 0 then mysqlError(mysql)
	       else toExpr(r))
	  else errNotOpen()
	  )
     else errNotConn());
setupfun("mysqlNextResult", mysqlNextResult);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
