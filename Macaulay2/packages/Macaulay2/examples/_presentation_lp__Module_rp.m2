R = QQ[a,b,c];
I = ideal"a2-b2,abc"
M = I/(I^2+a*I)
presentation M
