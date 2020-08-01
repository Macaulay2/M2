-- Copyright 2008 by Daniel R. Grayson

newPackage "Mysql"
exportFrom_Core {
     "MysqlConnection", "mysqlDebug", "mysqlNextResult",
     "mysqlFetchRow", "mysqlFetchField", "mysqlListFields",
     "mysqlGetServerInfo",
     "mysqlGetServerVersion", "mysqlGetHostInfo", "mysqlInfo", "mysqlPing",
     "mysqlListDbs", "mysqlRealConnect", "mysqlRealQuery", "mysqlStoreResult",
     "mysqlUseResult"
     }

export {
     "submit","mysqlFetchRows","MysqlBuffer","MysqlResultList", "mysqlFetchFields"
     }

MysqlConnection.synonym = "mysql connection"
MysqlResult.synonym = "mysql result set"
MysqlField.synonym = "mysql field"

Manipulator MysqlConnection := (m,o) -> m#0 o
MysqlConnection << String := mysqlRealQuery
MysqlBuffer = new Type of MutableList
new MysqlBuffer from MysqlConnection := (MysqlBuffer,m) -> {m,new MutableHashTable}
Manipulator MysqlConnection := MysqlConnection => (m,o) -> m#0 o
Manipulator MysqlBuffer := MysqlBuffer => (m,o) -> m#0 o
MysqlBuffer << Manipulator := MysqlBuffer => (o,m) -> m#0 o;
net MysqlBuffer := toString MysqlBuffer := x -> concatenate("<<MysqlBuffer : ", mysqlGetHostInfo x#0, ">>")
MysqlBuffer << String := (x,s) -> (
     q := x#1;
     q##q = s;
     x)
submitfun = method()
submitfun MysqlBuffer := x -> (
     query := concatenate values x#1;
     x#1 = new MutableHashTable;
     mysqlRealQuery(x#0,query);
     x)
submit = new Manipulator from {submitfun}

MysqlResultList = new Type of List
net MysqlResultList := netList @@ toList

mysqlFetchRows = method()
mysqlFetchRows MysqlResult := res -> (
     rows := new MutableHashTable;
     while null =!= (row := mysqlFetchRow res) do rows##rows = row;
     new MysqlResultList from values rows)
mysqlFetchFields = method()
mysqlFetchFields MysqlResult := res -> (
     rows := new MutableHashTable;
     while null =!= (row := mysqlFetchField res) do rows##rows = row;
     new MysqlResultList from values rows)
