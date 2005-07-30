n = map(ZZ^5,ZZ^6, (i,j) -> 100*i+j)
m = mutableMatrix n
columnPermute(m,1,{2,0,1})
