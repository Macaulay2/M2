R = QQ[x,y,w]; U = QQ[s,t,u]/ideal(s^2);
H = map(U,R,matrix{{s^2,t^3,u^4}})
ker H
