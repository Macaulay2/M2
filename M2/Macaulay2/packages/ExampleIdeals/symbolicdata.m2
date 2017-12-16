--------------------------------------------------------
--ZeroDim/example_57  This is a too easy example.
kk = ZZ/32003
R = kk[a,b,x,y,z,u,v,w, MonomialOrder=>Lex]
J = ideal"136z-136,  
  -240a+112y+420z-64v,  
  66az+78zv-1056a+90x+336y-90u,  
  -162a2+50ay+180az+55zu-284av+60yv-112b+260x+70w,  
  28azv-648a2+36bx+128ay+36bz-300au+40yu+44xv+192w,  
  15azu-162a2v+18ayv-312ab+84ax+24by+27xu+24aw+30vw,  
  6abz-162a2u+8ayu+10axv+14bx+48aw+16uw,  
  -162a2b+2aby+3axu+4avw+6bw"
time gb J;
-* -- Singular
ring R=32003,(a,b,x,y,z,u,v,w),lp;
ideal J = 136z-136,  
  -240a+112y+420z-64v,  
  66az+78zv-1056a+90x+336y-90u,  
  -162a2+50ay+180az+55zu-284av+60yv-112b+260x+70w,  
  28azv-648a2+36bx+128ay+36bz-300au+40yu+44xv+192w,  
  15azu-162a2v+18ayv-312ab+84ax+24by+27xu+24aw+30vw,  
  6abz-162a2u+8ayu+10axv+14bx+48aw+16uw,  
  -162a2b+2aby+3axu+4avw+6bw;
timer=1;
option(prot);
std(J);
groebner(J);  
*-
--------------------------------------
--Simson3 from Data/INTPS/Homog/Geometry/Simson_3.sd
kk = frac(ZZ/101[u1,u2,u3,u4])
kk = ZZ/101
R = kk[x1,x2,x3,x4,x5,x6,x7,x8,x9,h,u1,u2,u3,u4,MonomialOrder=>{10,4}]
I = ideal(x1*x8-x2*x8-x1*h*u2+x2*h*u1-x9*h*u1+x9*h*u2,  
  -x2*x6+x7*h*u2,  
  -x1*x4+x5*h*u1,  
  x3^2+h^2*u3^2-2*h^2*u3*u4,  
  -x2*x3+x2*x7+x6*h*u2-h^2*u2*u3,  
  -x1*x3+x1*x5+x4*h*u1-h^2*u1*u3,  
  x1*x3-x2*x3-x1*x9+x2*x9-x8*h*u1+x8*h*u2+h^2*u1*u3-h^2*u2*u3,  
  x2^2+h^2*u2^2-2*h^2*u2*u4,  
  x1^2+h^2*u1^2-2*h^2*u1*u4
  )
gbTrace=3
time gens gb I;


</basis>
<vars>      [x1, x2, x3, x4, x5, x6, x7, x8, x9, h] </vars>
<parameters> [u1, u2, u3, u4]              </parameters>
