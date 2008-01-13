R = ZZ/101[x,y];
point = ideal(x,y);
line = ideal(2*x+3*y-1);
V=Spec(R/intersect(point,line))
dim V
Z=Spec(R/(point+line))
dim Z
