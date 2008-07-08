-- Copyright 2008 by Daniel R. Grayson
-- M2 interface to the mysql C library
-- documentation:
--   http://dev.mysql.com/doc/refman/5.0/en/c.html
--   /usr/share/doc/mysql-doc-5.0/refman-5.0-en.html-chapter/index.html

use C;
use tokens;
use common;

export MYSQL := {MYSQL:void};
export MYSQLorNULL := MYSQL or null; 
newMYSQL():MYSQL := (
     m := Ccode(MYSQL, "(mysql_MYSQL)getmem(sizeof(MYSQL))");
     m);
mysqlError(m:MYSQL):Expr := buildErrorPacket(Ccode(string, "tostring((Cstring)mysql_error((MYSQL*)",m,"))"));
mysqlRealConnect(e:Expr):Expr := (
     m := newMYSQL();
     n := "utf8";
     ch := Ccode(Cstring, "(Cstring)tocharstar(", n, ")");
     if 0 != Ccode(int,"mysql_options((MYSQL*)",m,", MYSQL_SET_CHARSET_NAME, (char *)",ch,")") then return mysqlError(m);
     r := Ccode(MYSQLorNULL, "(mysql_MYSQLorNULL)mysql_real_connect((MYSQL*)",m,",(char *)0,(char *)0,(char *)0,(char *)0,0,(char *)0,(unsigned long)0)");
     when r is m:MYSQL do nullE
     else mysqlError(m)
     );
setupfun("mysqlRealConnect",mysqlRealConnect);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
