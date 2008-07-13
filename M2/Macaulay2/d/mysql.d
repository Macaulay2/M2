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
use basic;
use util;

mysqlError(mysql:MysqlConnection):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     buildErrorPacket(Ccode(string, "tostring2(mysql_error((MYSQL*)",mysql,"))")));
possibleMysqlError(mysql:MysqlConnection):Expr := (
     Ccode(void, "extern string tostring2(const char *)");
     if 0 == Ccode(int,"mysql_errno((MYSQL*)",mysql,")")
     then nullE
     else buildErrorPacket(Ccode(string, "tostring2(mysql_error((MYSQL*)",mysql,"))")));
mysqlConnectionFinalizer(m:MysqlConnectionWrapper, msg:string):void := (
     if m.open then (
	  Ccode(void,"mysql_close((MYSQL*)",m.mysql,")");
	  m.open = false;
	  );
     );
mysqlResultFinalizer(m:MysqlResultWrapper, msg:string):void := Ccode(void,"mysql_free_result((MYSQL_RES*)",m.res,")");

export MysqlFieldOrNULL := MysqlField or null;
export toExpr(res:MysqlResultWrapper, x:MysqlFieldOrNULL):Expr := (
     when x is fld:MysqlField 
     do Expr(MysqlFieldWrapper(res,fld)) 
     else mysqlError(res.connection.mysql)
     );

export MysqlResultOrNULL := MysqlResult or null;
export toExpr(conn:MysqlConnectionWrapper, x:MysqlResultOrNULL):Expr := (
     when x is res:MysqlResult do (
	  msg := "MysqlResult";
	  rw := MysqlResultWrapper(conn,res);
	  Ccode(void, "GC_register_finalizer((void *)",rw,",(GC_finalization_proc)",mysqlResultFinalizer,",",msg,",0,0)");
	  Expr(rw))
     else mysqlError(conn.mysql)
     );

export MysqlConnectionOrNULL := MysqlConnection or null; 
export toExpr(mysql:MysqlConnection, x:MysqlConnectionOrNULL):Expr := (
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
	  then toExpr(m, Ccode(MysqlResultOrNULL, "(mysql_MysqlResultOrNULL)mysql_store_result((MYSQL*)", m.mysql, ")" ))
	  else WrongArg("an open connection to a mysql database"))
     else WrongArg("a connection to a mysql database"));
setupfun("mysqlStoreResult",mysqlStoreResult);

mysqlUseResult(e:Expr):Expr := (
     when e is m:MysqlConnectionWrapper do (
	  if m.open
	  then toExpr(m, Ccode(MysqlResultOrNULL, "(mysql_MysqlResultOrNULL)mysql_use_result((MYSQL*)", m.mysql, ")" ))
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
     toExpr(m, Ccode(MysqlResultOrNULL, "(mysql_MysqlResultOrNULL)mysql_list_dbs((MYSQL*)", m.mysql, ", tocharstar(", wild, "))" ))
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
			 provide Expr(Ccode(string,"tostringn((char *)",(rowp + i).ptr, ",(int)", (lens+i).x, ")"))))))
     else WrongArg("a mysql result set"));
setupfun("mysqlFetchRow",mysqlFetchRow);

--  MYSQL_FIELD *mysql_fetch_field(MYSQL_RES *result)
--  MYSQL_FIELD *mysql_fetch_fields(MYSQL_RES *result)
-- 
--     typedef char **MYSQL_ROW;		/* return data as array of strings */
-- 
--     typedef struct st_mysql_field {
--       char *name;                 /* Name of column */
--       char *org_name;             /* Original column name, if an alias */
--       char *table;                /* Table of column if column was a field */
--       char *org_table;            /* Org table name, if table was an alias */
--       char *db;                   /* Database for table */
--       char *catalog;	      /* Catalog for table */
--       char *def;                  /* Default value (set by mysql_list_fields) */
--       unsigned long length;       /* Width of column (create length) */
--       unsigned long max_length;   /* Max width for selected set */
--       unsigned int name_length;
--       unsigned int org_name_length;
--       unsigned int table_length;
--       unsigned int org_table_length;
--       unsigned int db_length;
--       unsigned int catalog_length;
--       unsigned int def_length;
--       unsigned int flags;         /* Div flags */
--       unsigned int decimals;      /* Number of decimals in field */
--       unsigned int charsetnr;     /* Character set */
--       enum enum_field_types type; /* Type of field. See mysql_com.h for types */
--     } MYSQL_FIELD;
-- 
--     enum enum_field_types { MYSQL_TYPE_DECIMAL, MYSQL_TYPE_TINY, MYSQL_TYPE_SHORT, MYSQL_TYPE_LONG, MYSQL_TYPE_FLOAT,
-- 	 MYSQL_TYPE_DOUBLE, MYSQL_TYPE_NULL, MYSQL_TYPE_TIMESTAMP, MYSQL_TYPE_LONGLONG, MYSQL_TYPE_INT24,
-- 	 MYSQL_TYPE_DATE, MYSQL_TYPE_TIME, MYSQL_TYPE_DATETIME, MYSQL_TYPE_YEAR, MYSQL_TYPE_NEWDATE, MYSQL_TYPE_VARCHAR,
-- 	 MYSQL_TYPE_BIT, MYSQL_TYPE_NEWDECIMAL, MYSQL_TYPE_ENUM, MYSQL_TYPE_SET, MYSQL_TYPE_TINY_BLOB,
-- 	 MYSQL_TYPE_MEDIUM_BLOB, MYSQL_TYPE_LONG_BLOB, MYSQL_TYPE_BLOB, MYSQL_TYPE_VAR_STRING, MYSQL_TYPE_STRING,
-- 	 MYSQL_TYPE_GEOMETRY };
-- 
--     typedef struct st_mysql_res {
--       my_ulonglong row_count;
--       MYSQL_FIELD	*fields;
--       MYSQL_DATA	*data;
--       MYSQL_ROWS	*data_cursor;
--       unsigned long *lengths;		/* column lengths of current row */
--       MYSQL		*handle;		/* for unbuffered reads */
--       MEM_ROOT	field_alloc;
--       unsigned int	field_count, current_field;
--       MYSQL_ROW	row;			/* If unbuffered read */
--       MYSQL_ROW	current_row;		/* buffer to current row */
--       my_bool	eof;			/* Used by mysql_fetch_row */
--       /* mysql_stmt_close() had to cancel this result */
--       my_bool       unbuffered_fetch_cancelled;  
--       const struct st_mysql_methods *methods;
--     } MYSQL_RES;
-- 

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
