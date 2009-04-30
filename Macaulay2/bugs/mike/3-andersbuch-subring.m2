R = ZZ/2003[x11,x12,x13,x21,x22,x23,x31,x32,x33,
            y11,y12,y13,y21,y22,y23,y31,y32,y33];

f = map(R,R,DegreeMap => i -> 2*i, {x11*y11+x12*y21, 
	  x11*y12+x12*y22, 
          x11*y13+x12*y23, 
	  x21*y11+x22*y21, 
	  x21*y12+x22*y22, 
          x21*y13+x22*y23, 
	  x31*y11+x32*y21,
          x31*y12+x32*y22, 
	  x31*y13+x32*y23,
      x13*y11+x12*y21+x13*y21+x11*y31+x12*y31+x13*y31,
      x13*y12+x12*y22+x13*y22+x11*y32+x12*y32+x13*y32,
      x13*y13+x12*y23+x13*y23+x11*y33+x12*y33+x13*y33,
      x23*y11+x22*y21+x23*y21+x21*y31+x22*y31+x23*y31,
      x23*y12+x22*y22+x23*y22+x21*y32+x22*y32+x23*y32,
      x23*y13+x22*y23+x23*y23+x21*y33+x22*y33+x23*y33,
      x33*y11+x32*y21+x33*y21+x31*y31+x32*y31+x33*y31,
      x33*y12+x32*y22+x33*y22+x31*y32+x32*y32+x33*y32,
      x33*y13+x32*y23+x33*y23+x31*y33+x32*y33+x33*y33});


end
-- From sourceforge, Anders Buch

R = ZZ/2003[x11,x12,x13,x21,x22,x23,x31,x32,x33,
            y11,y12,y13,y21,y22,y23,y31,y32,y33];

f = map(R,R,{x11*y11+x12*y21, x11*y12+x12*y22, 
      x11*y13+x12*y23, x21*y11+x22*y21, x21*y12+x22*y22, 
      x21*y13+x22*y23, x31*y11+x32*y21,
      x31*y12+x32*y22, x31*y13+x32*y23,
      x13*y11+x12*y21+x13*y21+x11*y31+x12*y31+x13*y31,
      x13*y12+x12*y22+x13*y22+x11*y32+x12*y32+x13*y32,
      x13*y13+x12*y23+x13*y23+x11*y33+x12*y33+x13*y33,
      x23*y11+x22*y21+x23*y21+x21*y31+x22*y31+x23*y31,
      x23*y12+x22*y22+x23*y22+x21*y32+x22*y32+x23*y32,
      x23*y13+x22*y23+x23*y23+x21*y33+x22*y33+x23*y33,
      x33*y11+x32*y21+x33*y21+x31*y31+x32*y31+x33*y31,
      x33*y12+x32*y22+x33*y22+x31*y32+x32*y32+x33*y32,
      x33*y13+x32*y23+x33*y23+x31*y33+x32*y33+x33*y33});

gbTrace=3
time kernel(f)

R = ZZ/2003 [X_0..X_35, Degrees => {{1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, 
	                        {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}}, 
	MonomialOrder => Eliminate{18}, 
	MonomialSize => 8]
-- this one might be the example referred to in gbA.cpp
J = ideal (-X_0*X_9-X_1*X_12+X_18,
     -X_0*X_10-X_1*X_13+X_19,
     -X_0*X_11-X_1*X_14+X_20,-X_3*X_9-X_4*X_12+X_21,-X_3*X_10-X_4*X_13+X_22,-X_3*X_11-X_4*X_14+X_23,-X_6*X_9-X_7*X_12+X_24,
     -X_6*X_10-X_7*X_13+X_25,-X_6*X_11-X_7*X_14+X_26,-X_2*X_9-X_1*X_12-X_2*X_12-X_0*X_15-X_1*X_15-X_2*X_15+X_27,
     -X_2*X_10-X_1*X_13-X_2*X_13-X_0*X_16-X_1*X_16-X_2*X_16+X_28,-X_2*X_11-X_1*X_14-X_2*X_14-X_0*X_17-X_1*X_17-X_2*X_17+X_29,
     -X_5*X_9-X_4*X_12-X_5*X_12-X_3*X_15-X_4*X_15-X_5*X_15+X_30,-X_5*X_10-X_4*X_13-X_5*X_13-X_3*X_16-X_4*X_16-X_5*X_16+X_31,
     -X_5*X_11-X_4*X_14-X_5*X_14-X_3*X_17-X_4*X_17-X_5*X_17+X_32,-X_8*X_9-X_7*X_12-X_8*X_12-X_6*X_15-X_7*X_15-X_8*X_15+X_33,
     -X_8*X_10-X_7*X_13-X_8*X_13-X_6*X_16-X_7*X_16-X_8*X_16+X_34,
     -X_8*X_11-X_7*X_14-X_8*X_14-X_6*X_17-X_7*X_17-X_8*X_17+X_35)
T = (degreesRing R)_0
hf = 1-18*T^2+153*T^4-816*T^6+3060*T^8-8568*T^10+18564*T^12-31824*T^14+43758*T^16-48620*T^18+43758*T^20-
     31824*T^22+18564*T^24-8568*T^26+3060*T^28-816*T^30+153*T^32-18*T^34+T^36
(cokernel gens J).cache.poincare = hf;
gbTrace=3
time G = gb(J, DegreeLimit=>10);
time G = gb(J);
time G = gb(J, MaxReductionCount => 1);
debug Core
time G = gb(J, Algorithm=>Homogeneous2);
status G

time selectInSubring(1,gens G)
L = ideal oo
gens gb oo
codim L



restart
R1 = ZZ/2003[A..R,a..r,MonomialSize=>8]
I1=ideal"aj+bm,ak+bn,al+bo,dj+em,dk+en,dl+eo,gj+hm,gk+hn,gl+ho,cj+bm+cm+ap+bp+cp,ck+bn+cn+aq+bq+cq,
      cl+bo+co+ar+br+cr,fj+em+fm+dp+ep+fp,fk+en+fn+dq+eq+fq,fl+eo+fo+dr+er+fr,ij+hm+im+gp+hp+ip,
      ik+hn+in+gq+hq+iq,il+ho+io+gr+hr+ir"
J1 = matrix{{A..R}} * syz gens I1;
gbTrace=3
gens gb J1;




syz f.matrix

R = ZZ/2003[x11,x12,x13,x21,x22,x23,x31,x32,x33,
            y11,y12,y13,y21,y22,y23,y31,y32,y33];

F = matrix{{x11*y11+x12*y21, x11*y12+x12*y22, 
      x11*y13+x12*y23, x21*y11+x22*y21, x21*y12+x22*y22, 
      x21*y13+x22*y23, x31*y11+x32*y21,
      x31*y12+x32*y22, x31*y13+x32*y23,
      x13*y11+x12*y21+x13*y21+x11*y31+x12*y31+x13*y31,
      x13*y12+x12*y22+x13*y22+x11*y32+x12*y32+x13*y32,
      x13*y13+x12*y23+x13*y23+x11*y33+x12*y33+x13*y33,
      x23*y11+x22*y21+x23*y21+x21*y31+x22*y31+x23*y31,
      x23*y12+x22*y22+x23*y22+x21*y32+x22*y32+x23*y32,
      x23*y13+x22*y23+x23*y23+x21*y33+x22*y33+x23*y33,
      x33*y11+x32*y21+x33*y21+x31*y31+x32*y31+x33*y31,
      x33*y12+x32*y22+x33*y22+x31*y32+x32*y32+x33*y32,
      x33*y13+x32*y23+x33*y23+x31*y33+x32*y33+x33*y33}}

R1 = ZZ/2003[symbol a..symbol r]
F1 = substitute(F,vars R1)


     
ring R=2003,(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r),dp;
ideal I=aj+bm,ak+bn,al+bo,dj+em,dk+en,dl+eo,gj+hm,gk+hn,gl+ho,cj+bm+cm+ap+bp+cp,ck+bn+cn+aq+bq+cq,
      cl+bo+co+ar+br+cr,fj+em+fm+dp+ep+fp,fk+en+fn+dq+eq+fq,fl+eo+fo+dr+er+fr,ij+hm+im+gp+hp+ip,
      ik+hn+in+gq+hq+iq,il+ho+io+gr+hr+ir;
map F=R,I;     
F;
ideal zero;
option(prot);
preimage(R,F,zero);

ring R=2003,(a,b,c,d),dp;
ideal I=a4,a3b,ab3,b4;
map F=R,I;
ideal zero;
option(prot);
preimage(R,F,zero);
