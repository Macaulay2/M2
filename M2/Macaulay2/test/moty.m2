R = ZZ[u,v,w,x,y,z];
J = ideal((v*z-w*y)^6, (w*x-u*z)^6, (u*y-v*x)^6) : (x*y*z)^3;
transpose gens J
