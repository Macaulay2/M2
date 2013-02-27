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

time assert( f0 % ideal((f1)^(p+k),(f2)^(p+k),(f3)^(p+k)) == 0 )

end

J = ideal((f1)^(p+k),(f2)^(p+k),(f3)^(p+k))
see J
isHomogeneous J
gens gb(J, DegreeLimit=>20)
see ideal oo

gens gb(J, DegreeLimit=>22)
see ideal oo

R=QQ[b,c,s,t,u,v,w,x,y,z,MonomialSize=>16];

restart
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

-- over QQ, this ideal has 845 GB elements (degree up to 46)
-- f0 has degree 38.
-- SO: only compute a GB to degree 38
J = ideal((f1)^(p+k),(f2)^(p+k),(f3)^(p+k))
gbTrace=15
gbTrace=3
G = gb(J, DegreeLimit=>38);
G = gb(J, DegreeLimit=>24);
time assert( f0 % ideal((f1)^(p+k),(f2)^(p+k),(f3)^(p+k)) == 0 )
assert(f0 % G == 0) -- works, just takes a long time to get here.
