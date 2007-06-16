kk=ZZ/101
R=kk[x,y,z]
i=ideal"xyz,x3y,y2z2"
S=kk[x,y,z,u,v,w,Degrees=>{3:{1,0},{3,1},{4,1},{4,1}}]
T=kk[x,y,z,t,Degrees=>{3:{1,0},{0,1}}]
j=gens (t*substitute(i,T))
degrees j
f=map(T,S,(vars T)_{0..2}|j)
use S
assert( kernel f == ideal (y*z*u-x*w,x^2*u-z*v,y*z^2*v-x^3*w,x*y*u^2-v*w))
-- was: ideal (-u,y*z*u-x*w,x^2*u-z*v,-w,-v,-u^2,y*z^2*v-x^3*w,x*y*u^2-v*w,-u^3,-y*u^4,-y^2*u^6))
