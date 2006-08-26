A = ZZ/3[a,b,c];
isSkewCommutative A
B = QQ[a..d,SkewCommutative=>{a,b}]
isSkewCommutative B
C = B[x,y]
isSkewCommutative C
b_C * a_C
D = B/(a*d-b*c)
isSkewCommutative D
