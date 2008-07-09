newPackage "MYSQL"

export { tryit }
tryit = () -> (
     mysqlRealConnect("localhost","dan","foobar","",3306,""),
     mysqlRealConnect("localhost","dan","foobar","",0,"/var/run/mysqld/mysqld.sock")
     )

<< "-- try tryit()" << endl
