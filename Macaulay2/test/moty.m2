R = ZZ[u,v,w,x,y,z];
time J = ideal((v*z-w*y)^6, (w*x-u*z)^6, (u*y-v*x)^6) : (x*y*z)^3;
transpose gens J

I = ideal((v*z-w*y)^6, (w*x-u*z)^6, (u*y-v*x)^6)
time gb I;
A = R/I
F = (x*y*z)^3
s = time syz matrix {{F}};
s
-- Local Variables:
-- compile-command: "make moty.okay"
-- End:
