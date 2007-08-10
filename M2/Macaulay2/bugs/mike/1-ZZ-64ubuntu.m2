M = map(ZZ^1,ZZ^1, (i,j) -> i)
m = mutableMatrix M

m = mutableMatrix map(ZZ^5,ZZ^6, (i,j) -> 100*i+j)

m = mutableMatrix map(ZZ^5,ZZ^6, (i,j) -> 100*i+j)
columnPermute(m,1,{2,0,1})
