m1 = id_(QQ^10)^{1,6,2,7,3,8,4,9,5,0}
m2 = id_(QQ^10)^{1,0,2,3,4,5,6,7,8,9}
GAP = newConnection "127.0.0.1:26135"
G = GAP <=== matrixGroup({m1,m2})
<== size G
close GAP
