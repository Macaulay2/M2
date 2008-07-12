newPackage "MYSQL" -- the name of this package conflicts with the name of the type 
end

m = mysqlRealConnect ("localhost","dan","foobar","",0,"")
mysqlGetHostInfo m
mysqlListDbs(m,"")
mysqlListDbs(m,"f_")
mysqlListDbs(m,"f__")
mysqlListDbs(m,"f%")
n = new MysqlBuffer from m
n << "use foo" << submit
n << "select idnum from clients" << submit
mysqlStoreResult m
n << "select idnum from clients" << submit
mysqlUseResult m

