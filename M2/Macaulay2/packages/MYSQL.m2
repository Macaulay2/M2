newPackage "MYSQL" -- the name of this package conflicts with the name of the type 
end

m = mysqlRealConnect ("localhost","dan","foobar","",0,"")
mysqlGetHostInfo m
dbs = mysqlListDbs(m,"")
mysqlFetchRow dbs
mysqlFetchRow dbs
mysqlFetchRow dbs
mysqlFetchRow dbs

r = mysqlListDbs(m,"f_")
mysqlFetchRow r

r = mysqlListDbs(m,"f__")
mysqlFetchRow r
mysqlFetchRow r

r = mysqlListDbs(m,"f%")
mysqlFetchRow r
mysqlFetchRow r

n = new MysqlBuffer from m
n << "use foo" << submit
n << "select idnum from clients" << submit
r = mysqlStoreResult m
mysqlFetchRow r
mysqlFetchRow r
mysqlFetchRow r

n << "select idnum from clients" << submit
r = mysqlUseResult m
mysqlFetchRow r
mysqlFetchRow r
mysqlFetchRow r

