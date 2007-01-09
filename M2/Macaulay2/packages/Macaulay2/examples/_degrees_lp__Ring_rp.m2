R = ZZ/101[x,y,z];
degrees R
S = ZZ/101[x,y,z,Degrees => {{2,3},{1,2},{2,0}}];
degrees S
I = ideal"xy2,xyz,y3"
degrees I
degrees R^5
degrees R^{1,2,3,4}
