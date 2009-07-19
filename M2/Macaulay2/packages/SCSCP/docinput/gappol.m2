QQ[x];
p1 = x^2+1; p2 = x^3-1; p3 = x+17;
GAP = newConnection "127.0.0.1:26135";
gp1 = GAP <=== p1
gp2 = GAP <=== p2; gp3 = GAP <=== p3;
gp = gp1*gp2*gp3
p = p1*p2*p3;
<== (gp == p)
close GAP
