R = QQ[x,y,z]
f = vars R
f = f ++ f
m = mutableMatrix f;
m == mutableMatrix f
entries m
toString m
net m
m
m_(0,2)
setRowChange(m,id_(R^2))
setColumnChange(m,id_(R^6))
m_(1,2) = x+y
m == mutableMatrix f
m
rowSwap(m,0,1)
m
columnSwap(m,4,3)
m
