if getenv "USER" == "dan" then exit 0
R = ZZ[u,v,w,x,y,z];
ideal((v*z-w*y)^6, (w*x-u*z)^6, (u*y-v*x)^6) : (x*y*z)^3;

