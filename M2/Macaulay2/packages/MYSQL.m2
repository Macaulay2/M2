m = mysqlRealConnect ("localhost","dan","foobar","",0,"")
mysqlPing m
mysqlGetHostInfo m
mysqlGetServerInfo m
mysqlGetServerVersion m
d = mysqlListDbs(m,"")
mysqlFetchRow d
mysqlFetchRow d
mysqlFetchRow d
mysqlFetchRow d
mysqlFetchRows mysqlListDbs(m,"")
mysqlFetchRows mysqlListDbs(m,"f_")
mysqlFetchRows mysqlListDbs(m,"f__")
mysqlFetchRows mysqlListDbs(m,"f%")

n = new MysqlBuffer from m
n << "use foo" << submit
mysqlFetchRows mysqlListFields(m,"clients","")		    -- ???
mysqlFetchRows mysqlListFields(m,"clients","i%")	    -- ???

m << "show processlist"
mysqlFetchRows mysqlStoreResult m

m << "show columns from `clients`"
mysqlFetchRows mysqlStoreResult m

m << "describe `clients`"
r = mysqlStoreResult m
mysqlFetchFields r
mysqlFetchRows r

n << "select * " << " from `clients`" << submit
r = mysqlStoreResult m
mysqlFetchFields r
mysqlFetchRows r

m << "select name from `clients` where idnum = 213"
value first first mysqlFetchRows mysqlStoreResult m

n << "select * from `clients`" << submit
mysqlFetchRows mysqlUseResult m

m << "use mysql" << "show tables"
mysqlFetchRows mysqlStoreResult m

m << "show columns from `columns_priv`"
mysqlFetchRows mysqlStoreResult m

m << "select * from `columns_priv`"
mysqlFetchRows mysqlStoreResult m

m << "show columns from `db`"
mysqlFetchRows mysqlStoreResult m

m << "select * from `db`"
mysqlFetchRows mysqlStoreResult m

m << "show columns from `func`"
mysqlFetchRows mysqlStoreResult m

m << "select * from `func`"
mysqlFetchRows mysqlStoreResult m

m << "show columns from `user`"
mysqlFetchRows mysqlStoreResult m

m << "select * from user"
mysqlFetchRows mysqlStoreResult m

m << "show columns from help_topic"
mysqlFetchRows mysqlStoreResult m

m << "select * from help_topic"
-- mysqlFetchRows mysqlStoreResult m -- warning, output has 8522 lines, 57268 words and 2905660 characters.
-- we can make a good help facility from this
