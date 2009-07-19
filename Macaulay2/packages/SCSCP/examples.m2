loadPackage "SCSCP";
gap = newConnection "127.0.0.1:26135" -- the default is 26133
-- gap = newConnection "192.168.1.8:26135"
m1 = id_(ZZ^10)_{1,4,7,0,3,6,9,2,5,8};
m2 = id_(ZZ^10)_{1,0,2,3,4,5,6,7,8,9};
G = gap <=== matrixGroup{m1,m2}
<== size G
