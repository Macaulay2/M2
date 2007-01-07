-- An example of Uli Walther and Anurag Singh
-- Email dated 4/17/2006
R=ZZ[b,c,s,t,u,v,w,x,y,z,MonomialSize=>16];
f1=b*t*v^2+c*s*u^2;
f2=-b*s*u*x+b*t*v*y+c*s*u*x-c*t*v*y;
f3=-b*s*x^2-c*t*y^2;
g1=s*x^2+t*y^2;
g2=-s*u*x-t*v*y;
g3=s*u^2+t*v^2;
p=2
k=2
ff=((f1*g1)^p+(f2*g2)^p+(f3*g3)^p) // p;
f0=ff*((f1*f2*f3)^k);

time assert (f0 % ideal((f1)^(p+k),(f2)^(p+k),(f3)^(p+k)) == 0)

-- this succeeds in 43 seconds with 280MB of memory, with the debug version.
