-- Copyright 2008 by Daniel R. Grayson
-- M2 interface to the mysql C library
-- documentation:
--   http://dev.mysql.com/doc/refman/5.0/en/c.html
--   /usr/share/doc/mysql-doc-5.0/refman-5.0-en.html-chapter/index.html

use strings;
use hashtables;
use tokens;
header "#include <mysql/mysql.h>";
mysqlErrno(mysql:MysqlConnection) ::= Ccode(int,"mysql_errno(",mysql,")");
mysqlError(mysql:MysqlConnection):Expr := (
     buildErrorPacket(
	  if 0 == mysqlErrno(mysql)
	  then "unknown mysql error"
	  else tostring(Ccode(constcharstar, "mysql_error(",mysql,")"))));
possibleMysqlError(mysql:MysqlConnection):Expr := (
     if 0 == mysqlErrno(mysql)
     then nullE
     else buildErrorPacket(tostring(Ccode(constcharstar, "mysql_error(",mysql,")"))));
mysqlConnectionFinalizer(m:MysqlConnectionWrapper, msg:string):void := (
     when m.mysql
     is null do nothing
     is n:MysqlConnection do (
	  Ccode(void,"mysql_close(",n,")");
	  m.mysql = null();
	  );
     );
mysqlResultFinalizer(m:MysqlResultWrapper, msg:string):void := Ccode(void,"mysql_free_result(",m.res,")");

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
	  Ccode(void, "GC_REGISTER_FINALIZER(",rw,",(GC_finalization_proc)",mysqlResultFinalizer,",",msg,",0,0)");
	  Expr(rw))
     else mysqlError(mysql));

export MysqlConnectionOrNull := MysqlConnection or null; 
export toExpr(mysql:MysqlConnection, x:MysqlConnectionOrNull):Expr := (
     when x is mysql:MysqlConnection do (
	  msg := "MysqlConnection";
	  mw := MysqlConnectionWrapper(mysql);
	  Ccode(void, "GC_REGISTER_FINALIZER(",mw,",(GC_finalization_proc)",mysqlConnectionFinalizer,",",msg,",0,0)");
	  Expr(mw))
     else mysqlError(mysql)
     );

mysqlRealConnect(e:Expr):Expr := (
     when e is s:Sequence do (
	  if length(s) != 6 then return WrongNumArgs(6);
	  when s.0 is host:stringCell do
	  when s.1 is user:stringCell do
	  when s.2 is passwd:stringCell do
	  when s.3 is db:stringCell do
	  when s.4 is port:ZZcell do if !isInt(port) then WrongArgSmallInteger(5) else
     	  when s.5 is unixSocket:stringCell do (
	       conn := Ccode(MysqlConnection, "mysql_init(0)");
	       if 0 != Ccode(int,"mysql_options(",conn,", MYSQL_SET_CHARSET_NAME, \"utf8\")") then return mysqlError(conn);
	       toExpr(conn,Ccode(MysqlConnectionOrNull, "mysql_real_connect(",
			 conn,",",
			 tocharstarOrNull(host.v),",",
			 tocharstarOrNull(user.v),",",
			 tocharstarOrNull(passwd.v),",",
			 tocharstarOrNull(db.v),",",
			 toInt(port.v),",",
			 tocharstarOrNull(unixSocket.v),",",
			 "(unsigned long)(CLIENT_MULTI_STATEMENTS)", ")" )))
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
	  when s.1 is query:stringCell do (
	       if 0 == Ccode(int, "mysql_real_query(", m, ",", query.v, "->array,", query.v, "->len", ")")
	       then s.0
	       else mysqlError(m))
	  else WrongArgString(2))
     else errNotConn(1)
     else WrongNumArgs(2));
setupfun("mysqlRealQuery",mysqlRealQuery);

mysqlGetHostInfo(e:Expr):Expr := (
     when e is mw:MysqlConnectionWrapper do (
	  when mw.mysql 
	  is null do errNotOpen()
	  is mysql:MysqlConnection do toExpr(tostring(Ccode(constcharstar, "mysql_get_host_info(", mysql, ")" ))))
     else errNotConn());
setupfun("mysqlGetHostInfo", mysqlGetHostInfo);

mysqlStoreResult(e:Expr):Expr := (
     when e is mw:MysqlConnectionWrapper do (
	  when mw.mysql 
	  is null do errNotOpen()
	  is mysql:MysqlConnection do (
	       r := Ccode(MysqlResultOrNull, "mysql_store_result(", mysql, ")" );
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
	       res := Ccode(MysqlResultOrNull, "mysql_use_result(", mysql, ")" );
	       when res is null do (
		    if mysqlErrno(mysql) == 0 then return buildErrorPacket("mysqlUseResult: no results available");
		    )
	       else nothing;
	       toExpr(mysql, mw, res)))
     else errNotConn());
setupfun("mysqlUseResult",mysqlUseResult);

mysqlListDbs(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is mw:MysqlConnectionWrapper do 
     when mw.mysql
     is mysql:MysqlConnection do 
     when s.1 is wild:stringCell do
     toExpr(mysql, mw, Ccode(MysqlResultOrNull, "mysql_list_dbs(", mysql, ", ", tocharstar(wild.v), ")" ))
     else WrongArgString(2)
     else errNotOpen(1)
     else errNotConn(1)
     else WrongNumArgs(2));
setupfun("mysqlListDbs",mysqlListDbs);

charstarstarOrNull := charstarstar or null;
ulongstar := atomicPointer "unsigned long *";
ulongstarOrNull := ulongstar or null;
(s:charstar    ) + (i:int) ::= Ccode(charstarOrNull, "(", s, "+", i, ")" );
(s:charstarstar) + (i:int) ::= Ccode(charstarstarOrNull, "(", s, "+", i, ")" );
(s:ulongstar   ) + (i:int) ::= Ccode(ulongstarOrNull, "(", s, "+", i, ")" );
(s:charstarOrNull    ) + (i:int) ::= Ccode(charstarOrNull, "(", s, "+", i, ")" );
(s:charstarstarOrNull) + (i:int) ::= Ccode(charstarstarOrNull, "(", s, "+", i, ")" );
(s:ulongstarOrNull   ) + (i:int) ::= Ccode(ulongstarOrNull, "(", s, "+", i, ")" );

mysqlFetchRow(e:Expr):Expr := (
     when e is rw:MysqlResultWrapper do 
     when rw.connection.mysql is mysql:MysqlConnection do (
	  row := Ccode(charstarstarOrNull, "mysql_fetch_row(",rw.res,")");
	  when row
	  is null do possibleMysqlError(mysql)
	  is rowp:charstarstar do (
	       nflds := Ccode(int, "mysql_num_fields(",rw.res,")");
	       lens  := Ccode(ulongstar, "mysql_fetch_lengths(",rw.res,")");
	       Expr(
		    list (
			 new Sequence len nflds do
			 for i from 0 to nflds-1 do 
			 provide (
			      q := rowp + i;
			      when q is null do nullE
			      is qq:charstarstar do toExpr(tostringn(
					Ccode(charstar,"*(",qq,")"),
					int(Ccode(ulong,"*(",lens+i,")")))))))))
     else WrongArg("a mysql result set for an open mysql connection")
     else WrongArg("a mysql result set"));
setupfun("mysqlFetchRow",mysqlFetchRow);

mysqlFetchField(e:Expr):Expr := (
     when e is rw:MysqlResultWrapper do (
     	  when Ccode(MysqlFieldOrNull, "mysql_fetch_field(",rw.res,")")
	  is fld:MysqlField do Expr(MysqlFieldWrapper(rw,fld))
	  else nullE)
     else WrongArg("a mysql result set"));
setupfun("mysqlFetchField",mysqlFetchField);

mysqlListFields(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 3 then WrongNumArgs(3) else
     when s.0 is conn:MysqlConnectionWrapper do
     when conn.mysql is mysql:MysqlConnection do 
     when s.1 is table:stringCell do 
     when s.2 is wild:stringCell do
     toExpr(mysql,conn,Ccode(MysqlResultOrNull,"mysql_list_fields(",
	       conn.mysql, ",", tocharstar(table.v), ",", tocharstarOrNull(wild.v), ")"))
     else WrongArgString(3)
     else WrongArgString(2)
     else errNotOpen(1)
     else errNotConn(1)
     else WrongNumArgs(3));
setupfun("mysqlListFields",mysqlListFields);

mysqlGetServerInfo(e:Expr):Expr := (
     when e is mw:MysqlConnectionWrapper do 
     when mw.mysql is mysql:MysqlConnection 
     do toExpr(tostring(Ccode(constcharstar, "mysql_get_server_info(", mysql, ")" )))
     else errNotOpen()
     else errNotConn());
setupfun("mysqlGetServerInfo", mysqlGetServerInfo);

mysqlGetServerVersion(e:Expr):Expr := (
     when e is mw:MysqlConnectionWrapper do (
	  when mw.mysql is mysql:MysqlConnection 
	  do toExpr(Ccode(ulong, "mysql_get_server_version(", mysql, ")" ))
	  else errNotOpen())
     else errNotConn());
setupfun("mysqlGetServerVersion", mysqlGetServerVersion);

mysqlInfo(e:Expr):Expr := (
     when e is mw:MysqlConnectionWrapper do (
	  when mw.mysql is mysql:MysqlConnection do (
	       ptr := Ccode(constcharstarOrNull, "mysql_info(", mysql, ")");
	       when ptr is null do nullE
	       is str:constcharstar do toExpr(tostring(str)))
	  else errNotOpen())
     else errNotConn());
setupfun("mysqlInfo", mysqlInfo);

mysqlPing(e:Expr):Expr := (
     when e is mw:MysqlConnectionWrapper do (
	  when mw.mysql is mysql:MysqlConnection do
	  if 0 != Ccode(int, "mysql_ping(", mysql, ")")
	  then mysqlError(mysql)
	  else toExpr("alive")
	  else errNotOpen())
     else errNotConn());
setupfun("mysqlPing", mysqlPing);

mysqlDebug(e:Expr): Expr := (
     when e is debug:stringCell do (
	  Ccode(void,"mysql_debug(", tocharstarOrNull(debug.v), ")");
	  nullE)
     else WrongArgString());
setupfun("mysqlDebug", mysqlDebug);

mysqlNextResult(e:Expr): Expr := (
     when e is cw:MysqlConnectionWrapper do (
	  when cw.mysql is mysql:MysqlConnection do (
	       r := Ccode(int,"mysql_next_result(", mysql, ")");
	       if r > 0 then mysqlError(mysql)
	       else toExpr(r))
	  else errNotOpen()
	  )
     else errNotConn());
setupfun("mysqlNextResult", mysqlNextResult);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d mysql.o "
-- End:
