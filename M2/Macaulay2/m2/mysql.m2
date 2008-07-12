-- Copyright 2008 by Daniel R. Grayson

Manipulator MYSQL := (m,o) -> m#0 o
MYSQL << String := mysqlQuery
MYSQLbuffer = new Type of MutableList
new MYSQLbuffer from MYSQL := (MYSQLbuffer,m) -> {m,new MutableHashTable}
Manipulator MYSQL := MYSQL => (m,o) -> m#0 o
Manipulator MYSQLbuffer := MYSQLbuffer => (m,o) -> m#0 o
MYSQLbuffer << Manipulator := MYSQLbuffer => (o,m) -> m#0 o;
net MYSQLbuffer := x -> concatenate("<<MYSQLbuffer : ", mysqlGetHostInfo x#0, ">>")
MYSQLbuffer << String := (x,s) -> (
     q := x#1;
     q##q = s;
     x)
submitfun = method()
submitfun MYSQLbuffer := x -> (
     query := concatenate values x#1;
     x#1 = new MutableHashTable;
     mysqlQuery(x#0,query);
     x)
submit = new Manipulator from {submitfun}
