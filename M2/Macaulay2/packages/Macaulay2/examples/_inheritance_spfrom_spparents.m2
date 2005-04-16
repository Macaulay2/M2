X = new Type of BasicList
parent X
code(net,BasicList)
x = new X from {2,3,4}
lookup(symbol -, X) === null
- X := t -> apply(t,i -> -i);
- x
Y = new Type of X;
y = new Y from {4,5,6}
- y
Z = new Type of X;
z = new Z from {7,8,9}
Y + X := (a,b) -> YX;
X + Z := (a,b) -> XZ;
y + z
