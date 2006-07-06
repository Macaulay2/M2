(n,m) = (2,3);
A1 = QQ[x_1..x_n,y_1..y_m,MonomialOrder=>{n, RevLex=>m},Global=>false];
f = x_1*x_2^2 + 1 + y_1^10 + x_1*y_2^5 + y_3
1_A1 > y_1^10
A2 = QQ[x_1..x_n,y_1..y_m,MonomialOrder=>{RevLex=>n, m},Global=>false];
substitute(f,A2)
x_1*y_2^5 < 1_A2
A3 = QQ[x_1..x_n,y_1..y_m,MonomialOrder=>{n, RevLex=>2, m-2},Global=>false];
substitute(f,A3)
