-- -*- coding: utf-8 -*-
newPackage (
     "Benchmark",
     Headline => "standard Macaulay2 benchmarks",
     Authors => {
	  {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"},
	  {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
	  },
     Keywords => {"Miscellaneous"},
     HomePage => "http://www.math.uiuc.edu/Macaulay2/",
     PackageImports => {"XML"},
     Version => "1.0"
     )

-- we should remove the dependence of this on the unix "date" command!

export {"runBenchmarks"}

benchmarks = new HashTable from {
     "res39" => "res of a generic 3 by 9 matrix over ZZ/101" => () -> (
	  rr := (ZZ/101)(monoid [Variables => 52, MonomialSize => 16]);
	  (ti,re) := toSequence timing res coker genericMatrix(rr,3,9);
	  vv := apply(8,i->rank re_i);
	  assert( vv == {3, 9, 126, 378, 504, 360, 135, 21} );
	  ti),
     "resG25" => "res of the coordinate ring of Grassmannian(2,5)" => () -> (
	  SS := (ZZ/101)(monoid [Variables => 20, MonomialSize => 16]);
	  MM := SS^1/Grassmannian(2,5,SS,Variable => local T);
	  (ti,re) := toSequence timing res MM;
	  vv := apply(11,i->rank re_i);
	  assert( vv == {1, 35, 140, 301, 735, 1080, 735, 301, 140, 35, 1} );
	  ti),
     "gbB148" => "gb of Bayesian graph ideal #148" => () -> (
	  p := local p;
	  R := ZZ/32003[reverse(apply((1,1,1,1,1)..(2,2,2,2,2),s -> p_s)), MonomialSize=>8];
	  J := ideal(
	      -p_(1,1,2,1,1)*p_(2,1,1,1,1)+p_(1,1,1,1,1)*p_(2,1,2,1,1),
	      -p_(1,1,2,1,2)*p_(2,1,1,1,2)+p_(1,1,1,1,2)*p_(2,1,2,1,2),
	      -p_(1,1,2,2,1)*p_(2,1,1,2,1)+p_(1,1,1,2,1)*p_(2,1,2,2,1),
	      -p_(1,1,2,2,2)*p_(2,1,1,2,2)+p_(1,1,1,2,2)*p_(2,1,2,2,2),
	      -p_(1,2,2,1,1)*p_(2,2,1,1,1)+p_(1,2,1,1,1)*p_(2,2,2,1,1),
	      -p_(1,2,2,1,2)*p_(2,2,1,1,2)+p_(1,2,1,1,2)*p_(2,2,2,1,2),
	      -p_(1,2,2,2,1)*p_(2,2,1,2,1)+p_(1,2,1,2,1)*p_(2,2,2,2,1),
	      -p_(1,2,2,2,2)*p_(2,2,1,2,2)+p_(1,2,1,2,2)*p_(2,2,2,2,2),
	      -p_(1,1,1,1,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,1,2),
	      -p_(1,1,1,2,1)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,1),
	      -p_(1,1,1,2,1)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,1),
	      -p_(1,1,1,2,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,2),
	      -p_(1,1,1,2,2)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,2),
	      -p_(1,1,1,2,2)*p_(1,2,1,2,1)+p_(1,1,1,2,1)*p_(1,2,1,2,2),
	      -p_(1,1,2,1,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,1,2),
	      -p_(1,1,2,2,1)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,1),
	      -p_(1,1,2,2,1)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,1),
	      -p_(1,1,2,2,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,2),
	      -p_(1,1,2,2,2)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,2),
	      -p_(1,1,2,2,2)*p_(1,2,2,2,1)+p_(1,1,2,2,1)*p_(1,2,2,2,2),
	      -p_(1,1,1,1,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,1,2),
	      -p_(1,1,1,2,2)*p_(1,2,1,2,1)+p_(1,1,1,2,1)*p_(1,2,1,2,2),
	      -p_(1,1,2,1,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,1,2),
	      -p_(1,1,2,2,2)*p_(1,2,2,2,1)+p_(1,1,2,2,1)*p_(1,2,2,2,2),
	      -p_(1,1,1,2,1)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,1),
	      -p_(1,1,1,2,2)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,2),
	      -p_(1,1,2,2,1)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,1),
	      -p_(1,1,2,2,2)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,2),
	      -p_(1,1,1,1,2)*p_(1,1,1,2,1)+p_(1,1,1,1,1)*p_(1,1,1,2,2)
		+p_(1,1,1,2,2)*p_(1,1,2,1,1)-p_(1,1,1,2,1)*p_(1,1,2,1,2)
		-p_(1,1,1,1,2)*p_(1,1,2,2,1)-p_(1,1,2,1,2)*p_(1,1,2,2,1)
		+p_(1,1,1,1,1)*p_(1,1,2,2,2)+p_(1,1,2,1,1)*p_(1,1,2,2,2)
		+p_(1,1,1,2,2)*p_(1,2,1,1,1)+p_(1,1,2,2,2)*p_(1,2,1,1,1)
		-p_(1,1,1,2,1)*p_(1,2,1,1,2)-p_(1,1,2,2,1)*p_(1,2,1,1,2)
		-p_(1,1,1,1,2)*p_(1,2,1,2,1)-p_(1,1,2,1,2)*p_(1,2,1,2,1)
		-p_(1,2,1,1,2)*p_(1,2,1,2,1)+p_(1,1,1,1,1)*p_(1,2,1,2,2)
		+p_(1,1,2,1,1)*p_(1,2,1,2,2)+p_(1,2,1,1,1)*p_(1,2,1,2,2)
		+p_(1,1,1,2,2)*p_(1,2,2,1,1)+p_(1,1,2,2,2)*p_(1,2,2,1,1)
		+p_(1,2,1,2,2)*p_(1,2,2,1,1)-p_(1,1,1,2,1)*p_(1,2,2,1,2)
		-p_(1,1,2,2,1)*p_(1,2,2,1,2)-p_(1,2,1,2,1)*p_(1,2,2,1,2)
		-p_(1,1,1,1,2)*p_(1,2,2,2,1)-p_(1,1,2,1,2)*p_(1,2,2,2,1)
		-p_(1,2,1,1,2)*p_(1,2,2,2,1)-p_(1,2,2,1,2)*p_(1,2,2,2,1)
		+p_(1,1,1,1,1)*p_(1,2,2,2,2)+p_(1,1,2,1,1)*p_(1,2,2,2,2)
		+p_(1,2,1,1,1)*p_(1,2,2,2,2)+p_(1,2,2,1,1)*p_(1,2,2,2,2));
	  (ti,re) := toSequence timing gens gb (J,MaxReductionCount => 3000);
	  assert ( rank source re == 3626 );
	  ti),
     "gb3445" => "gb of an ideal with elements of degree 3,4,4,5 in 8 variables" => ()  -> (
	  R := ZZ/101[local a, local b, local c, local d, local e, local f, local g, local h,MonomialSize=>8];
	  setRandomSeed();
	  I := ideal random(R^1, R^{-3,-4,-4,-5});
	  J := ideal (R_0^3,R_1^4,R_2^4,R_3^5);
	  installHilbertFunction(I, poincare J);
	  (ti,re) := toSequence timing gens gb(I);
	  assert( tally degrees source re === new Tally from { {12} => 3, {13} => 1, {3} => 1, {4} => 2, {5} => 3, {6} => 5, {7} => 6, {8} => 8, {9} => 10, {10} => 9, {11} => 6} );
	  ti),
     "gb4by4comm" => "gb of the ideal of generic commuting 4 by 4 matrices over ZZ/101" => () -> (
	  R := ZZ/101[
	       local a, local b, local c, local d, local e, local f, local g,
	       local h, local i, local j, local k, local l, local m, local n,
	       local o, local p, local q, local r, local s, local t, local u,
	       local v, local w, local x, local y, local z, local A, local B,
	       local C, local D, local E, local F,
	       MonomialOrder=>ProductOrder{8,12,12},MonomialSize=>8];
	  I := ideal( -j*o+i*p-v*A+u*B-x*C+w*D, -a*p+b*o+c*p-d*o+k*B-l*A+m*D-n*C, -a*B+b*A+e*B-f*A+p*q-o*r-z*C+y*D, -a*D+b*C+g*D-h*C+p*s-o*t+B*E-A*F,
	       a*j-b*i-c*j+d*i-q*v+r*u-s*x+t*w, j*o-i*p-l*q+k*r-n*s+m*t, -c*r+d*q+e*r-f*q-i*B+j*A-s*z+t*y, -c*t+d*s+g*t-h*s-i*D+j*C-q*F+r*E,
	       a*v-b*u-e*v+f*u-j*k+i*l-x*E+w*F, c*l-d*k-e*l+f*k+m*F-n*E+o*v-p*u, l*q-k*r+v*A-u*B-z*E+y*F, -e*F+f*E+g*F-h*E+l*s-k*t+v*C-u*D,
	       a*x-b*w-g*x+h*w-j*m+i*n-v*y+u*z, c*n-d*m-g*n+h*m+k*z-l*y+o*x-p*w, e*z-f*y-g*z+h*y+n*q-m*r+x*A-w*B, n*s-m*t+x*C-w*D+z*E-y*F);
	  (ti,re) := toSequence timing gens gb(I, Strategy => LongPolynomial, Algorithm => Homogeneous2);
	  assert( tally degrees source re === new Tally from {{2} => 15, {3} => 23, {4} => 22, {5} => 67, {6} => 72, {7} => 85, {8} => 10} );
	  ti),
     "deg2generic" => "gb of a generic ideal of codimension 2 and degree 2" => () -> (
	  R1 := ZZ/32003[
	       local a, local b, local c, local d, local e, local f, local g,
	       local h, local i, local j, local k, local l, local m, local n,
	       local o, local p, local q, local r, local s, local t, local u,
	       local v, local w, local x, local y, local z, local A, local B,
	       local C, local D, local E, local F, local G, local H, local I,
	       local J, local K, local L, local M, local N,
	       MonomialSize=>8];
          I1 := ideal(
	       b^2*d*e+b*d^2*f+a^2*d*g+a*b*d*h+a*d^2*i+b^2*c*j+b*c^2*k+b*c*d*l+a^2*c*m+a*b*c*n+a*c^2*o+a*c*d*p,
	       b^2*d*q+b*d^2*r+a^2*d*s+a*b*d*t+a*d^2*u+b^2*c*v+b*c^2*w+b*c*d*x+a^2*c*y+a*b*c*z+a*c^2*A+a*c*d*B,
	       b^2*d*C+b*d^2*D+a^2*d*E+a*b*d*F+a*d^2*G+b^2*c*H+b*c^2*I+b*c*d*J+a^2*c*K+a*b*c*L+a*c^2*M+a*c*d*N
     	       );
	  (ti,re) := toSequence timing gens gb(I1, MaxReductionCount=>3000);
	  --(ti,re) := toSequence timing gens gb(I1, Algorithm=>LinearAlgebra);
	  assert(44 === numgens source gens gb I1);
	  ti),
     "yang-subring" => "an example of Yang-Hui He" => () -> (
     	  kk := ZZ/101;
	  R := kk[local a, local b, local c, local d, local e, local f, local g, local h, local i, local j, local k, local l, local m];
	  fterms := ideal(4*e*f^2-4*d*f*g+10*a*h+3*b*h+3*c*h+3*a*j+5*b*j+5*c*j+7*a*l+4*b*l+3*c*l+7*f,2*e^2*f-2*d*e*g-13*g*h*i+18*f*i^2-7*g*i*j-15*g*h*k+24*f*i*k-11*g*j*k+20*f*k^2-11*g*i*l-12*g*k*l-12*g*h*m+16*f*i*m-8*g*j*m+26*f*k*m-19*g*l*m+8*f*m^2+7*e,3*e*h-3*d*i+5*e*j-5*d*k+4*e*l-4*d*m,18*g^2*h-12*f*g*i+13*g^2*j-15*f*g*k+13*g^2*l-5*f*g*m+10*a*e+3*b*e+3*c*e,3*e*h-3*d*i+5*e*j-5*d*k+3*e*l-3*d*m,9*g^2*h-5*f*g*i+2*g^2*j-4*f*g*k+14*g^2*l-17*f*g*m+3*a*e+5*b*e+5*c*e,8*g^2*h-17*f*g*i+8*g^2*j-16*f*g*k+18*g^2*l-13*f*g*m+7*a*e+4*b*e+3*c*e,10*e*h-10*d*i+3*e*j-3*d*k+7*e*l-7*d*m,-16*e*f*g+16*d*g^2-10*a*i-3*b*i-3*c*i-3*a*k-5*b*k-5*c*k-7*a*m-4*b*m-3*c*m-7*g,-6*d*e*f+6*d^2*g+16*g*h^2-19*f*h*i+28*g*h*j-12*f*i*j+16*g*j^2-5*f*h*k-2*f*j*k+10*g*h*l-10*f*i*l+24*g*j*l-10*f*k*l+18*g*l^2-7*f*h*m-13*f*j*m-10*f*l*m-7*d,-12*f*g*h+6*f^2*i-10*f*g*j+8*f^2*k-10*f*g*l+11*f^2*m-10*a*d-3*b*d-3*c*d,-13*f*g*h+11*f^2*i-12*f*g*j+10*f^2*k-13*f*g*l+14*f^2*m-3*a*d-5*b*d-5*c*d,-20*f*g*h+13*f^2*i-13*f*g*j+16*f^2*k-3*f*g*l+16*f^2*m-7*a*d-4*b*d-3*c*d);
	  dterms := matrix {{g*h-f*i, g*j-f*k, g*l-f*m, e*f-d*g, a*i*j-a*h*k, b*i*j-b*h*k, c*i*j-c*h*k, a*i*l-a*h*m, b*i*l-b*h*m, c*i*l-c*h*m, a*k*l-a*j*m, b*k*l-b*j*m, c*k*l-c*j*m, a*e*h-a*d*i, b*e*h-b*d*i, c*e*h-c*d*i, a*e*j-a*d*k, b*e*j-b*d*k, c*e*j-c*d*k, a*e*l-a*d*m, b*e*l-b*d*m, c*e*l-c*d*m}};
	  y := symbol y;
	  S := kk[apply(1..22,i -> y_i), Degrees=>degrees source dterms];
	  A := R/fterms;
	  F := map(A,S,sub(dterms,A));
	  (ti,re) := toSequence timing ker F;
	  assert(tally degrees source gens re === new Tally from {{2} => 1, {3} => 12, {5} => 3, {6} => 16, {7} => 27});
	  ti),
     "yang-gb1" => "an example of Yang-Hui He arising in string theory" => () -> (
	  -- see also bugs/mike/1-yang-gbexamples.m2
	  kk := QQ;
	  R1 := kk[
	       local a, local b, local c, local d, local e, local f, local g,
	       local h, local i, local j, local k, local l, local m, local n,
	       local o, local p, local q, local r, local s, local t, local u,
	       local v, local w, local x, local y, local z, local A, local B,
	       local C, local D, local E, local F, local G, local H, local I,
	       local J, local K, local L, local M, local N, local O, local P,
	       local Q, local R, local S, local T, local U, local V,
	       MonomialSize=>8];
	  J1 := ideal(d*g*j*m-c*h*j*m-d*f*k*m+b*h*k*m+c*f*l*m-b*g*l*m-d*g*i*n+c*h*i*n+d*e*k*n-a*h*k*n-c*e*l*n+a*g*l*n+d*f*i*o-b*h*i*o-d*e*j*o+a*h*j*o+b*e*l*o-a*f*l*o-
		c*f*i*p+b*g*i*p+c*e*j*p-a*g*j*p-b*e*k*p+a*f*k*p,d*g*j*q-c*h*j*q-d*f*k*q+b*h*k*q+c*f*l*q-b*g*l*q-d*g*i*r+c*h*i*r+d*e*k*r-a*h*k*r-c*e*l*r+a*g*l*r+d*f*i*
		s-b*h*i*s-d*e*j*s+a*h*j*s+b*e*l*s-a*f*l*s-c*f*i*t+b*g*i*t+c*e*j*t-a*g*j*t-b*e*k*t+a*f*k*t,d*g*n*q-c*h*n*q-d*f*o*q+b*h*o*q+c*f*p*q-b*g*p*q-d*g*m*r+c*h*
		m*r+d*e*o*r-a*h*o*r-c*e*p*r+a*g*p*r+d*f*m*s-b*h*m*s-d*e*n*s+a*h*n*s+b*e*p*s-a*f*p*s-c*f*m*t+b*g*m*t+c*e*n*t-a*g*n*t-b*e*o*t+a*f*o*t,d*k*n*q-c*l*n*q-d*
		j*o*q+b*l*o*q+c*j*p*q-b*k*p*q-d*k*m*r+c*l*m*r+d*i*o*r-a*l*o*r-c*i*p*r+a*k*p*r+d*j*m*s-b*l*m*s-d*i*n*s+a*l*n*s+b*i*p*s-a*j*p*s-c*j*m*t+b*k*m*t+c*i*n*t-
		a*k*n*t-b*i*o*t+a*j*o*t,h*k*n*q-g*l*n*q-h*j*o*q+f*l*o*q+g*j*p*q-f*k*p*q-h*k*m*r+g*l*m*r+h*i*o*r-e*l*o*r-g*i*p*r+e*k*p*r+h*j*m*s-f*l*m*s-h*i*n*s+e*l*n*
		s+f*i*p*s-e*j*p*s-g*j*m*t+f*k*m*t+g*i*n*t-e*k*n*t-f*i*o*t+e*j*o*t,d*g*j*u-c*h*j*u-d*f*k*u+b*h*k*u+c*f*l*u-b*g*l*u-d*g*i*v+c*h*i*v+d*e*k*v-a*h*k*v-c*e*
		l*v+a*g*l*v+d*f*i*w-b*h*i*w-d*e*j*w+a*h*j*w+b*e*l*w-a*f*l*w-c*f*i*x+b*g*i*x+c*e*j*x-a*g*j*x-b*e*k*x+a*f*k*x,d*g*n*u-c*h*n*u-d*f*o*u+b*h*o*u+c*f*p*u-b*
		g*p*u-d*g*m*v+c*h*m*v+d*e*o*v-a*h*o*v-c*e*p*v+a*g*p*v+d*f*m*w-b*h*m*w-d*e*n*w+a*h*n*w+b*e*p*w-a*f*p*w-c*f*m*x+b*g*m*x+c*e*n*x-a*g*n*x-b*e*o*x+a*f*o*x,
		d*k*n*u-c*l*n*u-d*j*o*u+b*l*o*u+c*j*p*u-b*k*p*u-d*k*m*v+c*l*m*v+d*i*o*v-a*l*o*v-c*i*p*v+a*k*p*v+d*j*m*w-b*l*m*w-d*i*n*w+a*l*n*w+b*i*p*w-a*j*p*w-c*j*m*
		x+b*k*m*x+c*i*n*x-a*k*n*x-b*i*o*x+a*j*o*x,h*k*n*u-g*l*n*u-h*j*o*u+f*l*o*u+g*j*p*u-f*k*p*u-h*k*m*v+g*l*m*v+h*i*o*v-e*l*o*v-g*i*p*v+e*k*p*v+h*j*m*w-f*l*
		m*w-h*i*n*w+e*l*n*w+f*i*p*w-e*j*p*w-g*j*m*x+f*k*m*x+g*i*n*x-e*k*n*x-f*i*o*x+e*j*o*x,d*g*r*u-c*h*r*u-d*f*s*u+b*h*s*u+c*f*t*u-b*g*t*u-d*g*q*v+c*h*q*v+d*
		e*s*v-a*h*s*v-c*e*t*v+a*g*t*v+d*f*q*w-b*h*q*w-d*e*r*w+a*h*r*w+b*e*t*w-a*f*t*w-c*f*q*x+b*g*q*x+c*e*r*x-a*g*r*x-b*e*s*x+a*f*s*x,d*k*r*u-c*l*r*u-d*j*s*u+
		b*l*s*u+c*j*t*u-b*k*t*u-d*k*q*v+c*l*q*v+d*i*s*v-a*l*s*v-c*i*t*v+a*k*t*v+d*j*q*w-b*l*q*w-d*i*r*w+a*l*r*w+b*i*t*w-a*j*t*w-c*j*q*x+b*k*q*x+c*i*r*x-a*k*r*
		x-b*i*s*x+a*j*s*x,h*k*r*u-g*l*r*u-h*j*s*u+f*l*s*u+g*j*t*u-f*k*t*u-h*k*q*v+g*l*q*v+h*i*s*v-e*l*s*v-g*i*t*v+e*k*t*v+h*j*q*w-f*l*q*w-h*i*r*w+e*l*r*w+f*i*
		t*w-e*j*t*w-g*j*q*x+f*k*q*x+g*i*r*x-e*k*r*x-f*i*s*x+e*j*s*x,d*o*r*u-c*p*r*u-d*n*s*u+b*p*s*u+c*n*t*u-b*o*t*u-d*o*q*v+c*p*q*v+d*m*s*v-a*p*s*v-c*m*t*v+a*
		o*t*v+d*n*q*w-b*p*q*w-d*m*r*w+a*p*r*w+b*m*t*w-a*n*t*w-c*n*q*x+b*o*q*x+c*m*r*x-a*o*r*x-b*m*s*x+a*n*s*x,h*o*r*u-g*p*r*u-h*n*s*u+f*p*s*u+g*n*t*u-f*o*t*u-
		h*o*q*v+g*p*q*v+h*m*s*v-e*p*s*v-g*m*t*v+e*o*t*v+h*n*q*w-f*p*q*w-h*m*r*w+e*p*r*w+f*m*t*w-e*n*t*w-g*n*q*x+f*o*q*x+g*m*r*x-e*o*r*x-f*m*s*x+e*n*s*x,l*o*r*
		u-k*p*r*u-l*n*s*u+j*p*s*u+k*n*t*u-j*o*t*u-l*o*q*v+k*p*q*v+l*m*s*v-i*p*s*v-k*m*t*v+i*o*t*v+l*n*q*w-j*p*q*w-l*m*r*w+i*p*r*w+j*m*t*w-i*n*t*w-k*n*q*x+j*o*
		q*x+k*m*r*x-i*o*r*x-j*m*s*x+i*n*s*x,a*y+b*z+c*A+d*B,e*y+f*z+g*A+h*B,i*y+j*z+k*A+l*B,m*y+n*z+o*A+p*B,q*y+r*z+s*A+t*B,u*y+v*z+w*A+x*B,a*C+b*D+c*E+d*F,e*
		C+f*D+g*E+h*F,i*C+j*D+k*E+l*F,m*C+n*D+o*E+p*F,q*C+r*D+s*E+t*F,u*C+v*D+w*E+x*F,a*G+b*H+c*I+d*J,e*G+f*H+g*I+h*J,i*G+j*H+k*I+l*J,m*G+n*H+o*I+p*J,q*G+r*H+
		s*I+t*J,u*G+v*H+w*I+x*J,a*K+b*L+c*M+d*N,e*K+f*L+g*M+h*N,i*K+j*L+k*M+l*N,m*K+n*L+o*M+p*N,q*K+r*L+s*M+t*N,u*K+v*L+w*M+x*N,B*E*H*K-A*F*H*K-B*D*I*K+z*F*I*
		K+A*D*J*K-z*E*J*K-B*E*G*L+A*F*G*L+B*C*I*L-y*F*I*L-A*C*J*L+y*E*J*L+B*D*G*M-z*F*G*M-B*C*H*M+y*F*H*M+z*C*J*M-y*D*J*M-A*D*G*N+z*E*G*N+A*C*H*N-y*E*H*N-z*C*
		I*N+y*D*I*N,a*O+b*P+c*Q+d*R,e*O+f*P+g*Q+h*R,i*O+j*P+k*Q+l*R,m*O+n*P+o*Q+p*R,q*O+r*P+s*Q+t*R,u*O+v*P+w*Q+x*R,B*E*H*O-A*F*H*O-B*D*I*O+z*F*I*O+A*D*J*O-z*
		E*J*O-B*E*G*P+A*F*G*P+B*C*I*P-y*F*I*P-A*C*J*P+y*E*J*P+B*D*G*Q-z*F*G*Q-B*C*H*Q+y*F*H*Q+z*C*J*Q-y*D*J*Q-A*D*G*R+z*E*G*R+A*C*H*R-y*E*H*R-z*C*I*R+y*D*I*R,
		B*E*L*O-A*F*L*O-B*D*M*O+z*F*M*O+A*D*N*O-z*E*N*O-B*E*K*P+A*F*K*P+B*C*M*P-y*F*M*P-A*C*N*P+y*E*N*P+B*D*K*Q-z*F*K*Q-B*C*L*Q+y*F*L*Q+z*C*N*Q-y*D*N*Q-A*D*K*
		R+z*E*K*R+A*C*L*R-y*E*L*R-z*C*M*R+y*D*M*R,B*I*L*O-A*J*L*O-B*H*M*O+z*J*M*O+A*H*N*O-z*I*N*O-B*I*K*P+A*J*K*P+B*G*M*P-y*J*M*P-A*G*N*P+y*I*N*P+B*H*K*Q-z*J*
		K*Q-B*G*L*Q+y*J*L*Q+z*G*N*Q-y*H*N*Q-A*H*K*R+z*I*K*R+A*G*L*R-y*I*L*R-z*G*M*R+y*H*M*R,F*I*L*O-E*J*L*O-F*H*M*O+D*J*M*O+E*H*N*O-D*I*N*O-F*I*K*P+E*J*K*P+F*
		G*M*P-C*J*M*P-E*G*N*P+C*I*N*P+F*H*K*Q-D*J*K*Q-F*G*L*Q+C*J*L*Q+D*G*N*Q-C*H*N*Q-E*H*K*R+D*I*K*R+E*G*L*R-C*I*L*R-D*G*M*R+C*H*M*R,a*S+b*T+c*U+d*V,e*S+f*T+
		g*U+h*V,i*S+j*T+k*U+l*V,m*S+n*T+o*U+p*V,q*S+r*T+s*U+t*V,u*S+v*T+w*U+x*V,B*E*H*S-A*F*H*S-B*D*I*S+z*F*I*S+A*D*J*S-z*E*J*S-B*E*G*T+A*F*G*T+B*C*I*T-y*F*I*
		T-A*C*J*T+y*E*J*T+B*D*G*U-z*F*G*U-B*C*H*U+y*F*H*U+z*C*J*U-y*D*J*U-A*D*G*V+z*E*G*V+A*C*H*V-y*E*H*V-z*C*I*V+y*D*I*V,B*E*L*S-A*F*L*S-B*D*M*S+z*F*M*S+A*D*
		N*S-z*E*N*S-B*E*K*T+A*F*K*T+B*C*M*T-y*F*M*T-A*C*N*T+y*E*N*T+B*D*K*U-z*F*K*U-B*C*L*U+y*F*L*U+z*C*N*U-y*D*N*U-A*D*K*V+z*E*K*V+A*C*L*V-y*E*L*V-z*C*M*V+y*
		D*M*V,B*I*L*S-A*J*L*S-B*H*M*S+z*J*M*S+A*H*N*S-z*I*N*S-B*I*K*T+A*J*K*T+B*G*M*T-y*J*M*T-A*G*N*T+y*I*N*T+B*H*K*U-z*J*K*U-B*G*L*U+y*J*L*U+z*G*N*U-y*H*N*U-
		A*H*K*V+z*I*K*V+A*G*L*V-y*I*L*V-z*G*M*V+y*H*M*V,F*I*L*S-E*J*L*S-F*H*M*S+D*J*M*S+E*H*N*S-D*I*N*S-F*I*K*T+E*J*K*T+F*G*M*T-C*J*M*T-E*G*N*T+C*I*N*T+F*H*K*
		U-D*J*K*U-F*G*L*U+C*J*L*U+D*G*N*U-C*H*N*U-E*H*K*V+D*I*K*V+E*G*L*V-C*I*L*V-D*G*M*V+C*H*M*V,B*E*P*S-A*F*P*S-B*D*Q*S+z*F*Q*S+A*D*R*S-z*E*R*S-B*E*O*T+A*F*
		O*T+B*C*Q*T-y*F*Q*T-A*C*R*T+y*E*R*T+B*D*O*U-z*F*O*U-B*C*P*U+y*F*P*U+z*C*R*U-y*D*R*U-A*D*O*V+z*E*O*V+A*C*P*V-y*E*P*V-z*C*Q*V+y*D*Q*V,B*I*P*S-A*J*P*S-B*
		H*Q*S+z*J*Q*S+A*H*R*S-z*I*R*S-B*I*O*T+A*J*O*T+B*G*Q*T-y*J*Q*T-A*G*R*T+y*I*R*T+B*H*O*U-z*J*O*U-B*G*P*U+y*J*P*U+z*G*R*U-y*H*R*U-A*H*O*V+z*I*O*V+A*G*P*V-
		y*I*P*V-z*G*Q*V+y*H*Q*V,F*I*P*S-E*J*P*S-F*H*Q*S+D*J*Q*S+E*H*R*S-D*I*R*S-F*I*O*T+E*J*O*T+F*G*Q*T-C*J*Q*T-E*G*R*T+C*I*R*T+F*H*O*U-D*J*O*U-F*G*P*U+C*J*P*
		U+D*G*R*U-C*H*R*U-E*H*O*V+D*I*O*V+E*G*P*V-C*I*P*V-D*G*Q*V+C*H*Q*V,B*M*P*S-A*N*P*S-B*L*Q*S+z*N*Q*S+A*L*R*S-z*M*R*S-B*M*O*T+A*N*O*T+B*K*Q*T-y*N*Q*T-A*K*
		R*T+y*M*R*T+B*L*O*U-z*N*O*U-B*K*P*U+y*N*P*U+z*K*R*U-y*L*R*U-A*L*O*V+z*M*O*V+A*K*P*V-y*M*P*V-z*K*Q*V+y*L*Q*V,F*M*P*S-E*N*P*S-F*L*Q*S+D*N*Q*S+E*L*R*S-D*
		M*R*S-F*M*O*T+E*N*O*T+F*K*Q*T-C*N*Q*T-E*K*R*T+C*M*R*T+F*L*O*U-D*N*O*U-F*K*P*U+C*N*P*U+D*K*R*U-C*L*R*U-E*L*O*V+D*M*O*V+E*K*P*V-C*M*P*V-D*K*Q*V+C*L*Q*V,
		J*M*P*S-I*N*P*S-J*L*Q*S+H*N*Q*S+I*L*R*S-H*M*R*S-J*M*O*T+I*N*O*T+J*K*Q*T-G*N*Q*T-I*K*R*T+G*M*R*T+J*L*O*U-H*N*O*U-J*K*P*U+G*N*P*U+H*K*R*U-G*L*R*U-I*L*O*
		V+H*M*O*V+I*K*P*V-G*M*P*V-H*K*Q*V+G*L*Q*V);
	  --(ti,re) := toSequence timing gb(J1, Algorithm=>LinearAlgebra);
	  (ti,re) := toSequence timing gb(J1);
	  ti)
     }

runBenchmark = n -> (
     << "-- " << n << ": " << flush;
     ti := benchmarks#n#1();
     << benchmarks#n#0 << ": " <<  toString ti << " seconds" << endl;
     )
runBenchmarks0 = method()
runBenchmarks0 Function := x -> (
     if x === all then runBenchmarks sort keys benchmarks
     else error("expected 'all', but got: ", toString x);
     )
runBenchmarks0 String := x -> runBenchmarks0 {x}
runBenchmarks0 List := x -> (
     << "-- beginning computation " << get "!date";
     << "-- " << first lines get "!uname -a" << endl;
     if fileExists "/proc/cpuinfo"
     then (
	  cpuinfo := get "/proc/cpuinfo";
	  << "-- ";
	  scan({
		    ("model name[[:space:]]*: (.*)","\\1"),
		    ("vendor_id[[:space:]]*: (.*)","\\1"),
		    ("(cpu MHz)[[:space:]]*: (.*)","\\1 \\2"),
		    ("(BogoMIPS)[[:space:]]*: (.*)","\\1 \\2")
		    },
	       (pattern,repl) -> ( 
		    t := select(pattern,repl,cpuinfo);
		    if #t > 0 then << replace("[[:space:]]+"," ",t#0) << "  ";
		    ));
	  << endl;
	  );
     if fileExists "/usr/sbin/system_profiler"
     then (
	  r := get "! true | /usr/sbin/system_profiler SPHardwareDataType";
	  << "-- ";
	  scan({
		    "Model Name: (.*)",
		    "Processor Name: (.*)",
		    "(Processor Speed: .*)",
		    "(Cores: .*)",
		    "(Bus Speed: .*)"
		    },
	       s -> ( t := select(s,"\\1, ",r); if #t > 0 then << t#0));
	  << endl;
	  );
     runsysctl := sysctl -> (
	  args := "-n machdep.cpu.brand_string";
	  if 0 == run (sysctl| " "|args|" 2>/dev/null 1>/dev/null") then get ("!"|sysctl|" "|args));
     brandString := (
     	  if fileExists "/usr/sbin/sysctl" then runsysctl "/usr/sbin/sysctl" 
	  else if fileExists "/sbin/sysctl" then runsysctl "/sbin/sysctl" 
	  else if fileExists "/proc/sys/machdep/cpu/brand_string" then get "/proc/sys/machdep/cpu/brand_string");
     if brandString =!= null then << "-- Processor: " << brandString;
     << "-- Macaulay2 " << version#"VERSION";
     << ", compiled with " << version#"compiler";
     << endl;
     scan(x,runBenchmark))
installMethod(runBenchmarks0, () -> runBenchmarks0 {"res39","resG25","gbB148"})
runBenchmarks = Command runBenchmarks0

beginDocumentation()

scan(keys benchmarks, b -> TEST ("runBenchmarks "|format b))

multidoc ///
Node
 Key
  Benchmark
 Headline
  standard Macaulay2 benchmarks
 Description
  Text
   This package provides some standard benchmarks for gauging the speed of
   algorithms and machines.
Node
 Key
  runBenchmarks
 Usage
  runBenchmarks
  runBenchmarks x
  runBenchmarks all
 Inputs
  x:
   a string or list of strings
 Consequences
  Item
   the benchmarks whose names occur in @ TT "x" @ are run.  The output is in a standard
   format that can be inserted into a Macaulay2 source file as a comment.
   If @ TT "x" @ is omitted, then the "res39", "resG25", and "gbB148" are run.
   If @ TT "x" @ is @ TT "all" @, then all the benchmarks are run.
 Description
  Text
   The tests available are: @
   BR{}, 
   apply(sort pairs benchmarks, (n,x) -> { toExternalString n, " -- ", x#0, BR{}})
   @
  Example
   runBenchmarks "res39"
///

end

Here is another possible benchmark, but it doesn't work for us yet:

    R = ZZ/5[a..d,MonomialSize=>16];
    I = ideal{a^3-2*a^2*b-a*b^2-2*b^3+a*b*c-2*b^2*c+2*a*c^2-2*b*c^2-c^3+2*a*b*d
	      -2*b^2*d-a*c*d-2*b*c*d-2*c^2*d+a*d^2+c*d^2-d^3, a^125, b^125,
	      c^125, d^125}
    time gb I;

-- Results:

---- Intel i7-2600 3.4GHz "Sandy Bridge" processor with 16G of RAM
  -- beginning computation Mon Sep 26 13:08:57 PDT 2011
  -- Linux gatto 2.6.32-33-server #72-Ubuntu SMP Fri Jul 29 21:21:55 UTC 2011 x86_64 GNU/Linux
  -- Intel(R) Core(TM) i7-2600 CPU @ 3.40GHz  GenuineIntel  cpu MHz 1600.000
  -- Macaulay2 1.4, compiled with gcc 4.4.5
  -- deg2generic: gb of a generic ideal of codimension 2 and degree 2: 29.2788 seconds
  -- gb4by4comm: gb of the ideal of generic commuting 4 by 4 matrices over ZZ/101: 1.03668 seconds
  -- gb3445: gb of an ideal with elements of degree 3,4,4,5 in 8 variables: 13.7588 seconds
  -- gbB148: gb of Bayesian graph ideal #148: 12.3361 seconds
  -- res39: res of a generic 3 by 9 matrix over ZZ/101: .0820532 seconds
  -- resG25: res of the coordinate ring of Grassmannian(2,5): 1.28195 seconds
  -- yang-gb1: an example of Yang-Hui He arising in string theory: 31.6871 seconds
  -- yang-subring: an example of Yang-Hui He: 11.6752 seconds

---- Intel Core i7-950 "Bloomfield" 3.06GHz with 12GB of RAM
  -- beginning computation Sat Dec 18 17:55:13 PST 2010
  -- Linux atlas.internal 2.6.32-26-server #48-Ubuntu SMP Wed Nov 24 10:28:32 UTC 2010 x86_64 GNU/Linux
  -- Intel(R) Core(TM) i7 CPU 950 @ 3.07GHz  GenuineIntel  cpu MHz 1600.000
  -- Macaulay2 1.4, compiled with gcc 4.4.5
  -- res39: res of a generic 3 by 9 matrix over ZZ/101: .09054 seconds
  -- resG25: res of the coordinate ring of Grassmannian(2,5): 1.28264 seconds
  -- gbB148: gb of Bayesian graph ideal #148: 12.8432 seconds
  -- deg2generic: gb of a generic ideal of codimension 2 and degree 2: 36.9496 seconds
  -- gb4by4comm: gb of the ideal of generic commuting 4 by 4 matrices over ZZ/101: 1.34551 seconds
  -- gb3445: gb of an ideal with elements of degree 3,4,4,5 in 8 variables: 20.5233 seconds
  -- yang-gb1: an example of Yang-Hui He arising in string theory: 37.0758 seconds
  -- yang-subring: an example of Yang-Hui He: 12.646 seconds

---- Intel(R) Xeon(R) CPU X5550  @ 2.67GHz
  -- beginning computation Mon Aug 23 16:04:22 CDT 2010
  -- Linux euler.math.illinois.edu 2.6.33.5-112.fc13.i686.PAE #1 SMP Thu May 27 02:56:20 UTC 2010 i686 i686 i386 GNU/Linux
  -- Macaulay2 1.3.1, compiled with gcc 4.4.3
  -- res39: res of a generic 3 by 9 matrix over ZZ/101: .148977 seconds
  -- resG25: res of the coordinate ring of Grassmannian(2,5): 1.86072 seconds
  -- gbB148: gb of Bayesian graph ideal #148: 14.2298 seconds

---- an Intel Core 2 Quad with 8 GB of RAM in Las Vegas
  -- beginning computation Wed Mar 25 17:53:19 PDT 2009
  -- Linux ubuntu 2.6.24-23-server #1 SMP Thu Nov 27 18:45:02 UTC 2008 x86_64 GNU/Linux
  -- Macaulay 2 1.2, compiled with gcc 4.2.3
  -- res39: res of a generic 3 by 9 matrix over ZZ/101: .11 seconds
  -- resG25: res of the coordinate ring of Grassmannian(2,5): 1.69 seconds
  -- gbB148: gb of Bayesian graph ideal #148: 15.79 seconds
  -- running four copies at once, the results were just a bit slower - the slowest for each test was .18/1.85/16.38.

-- a Core i5-650
  -- beginning computation Fri Oct  8 09:38:53 CDT 2010
  -- CYGWIN_NT-6.1-WOW64 freedom 1.7.7(0.230/5/3) 2010-08-31 09:58 i686 Cygwin
  -- Macaulay2 1.3.1, compiled with gcc 3.4.4
  -- res39: res of a generic 3 by 9 matrix over ZZ/101: .266 seconds
  -- resG25: res of the coordinate ring of Grassmannian(2,5): 4.01 seconds
  -- gbB148: gb of Bayesian graph ideal #148: 17.207 seconds

  -- beginning computation Thu Dec 26 14:01:15 PST 2013
  -- Darwin einsteinium.pololu.internal 13.0.2 Darwin Kernel Version 13.0.2: Sun Sep 29 19:38:57 PDT 2013; root:xnu-2422.75.4~1/RELEASE_X86_64 x86_64
  -- MacBook Pro, Intel Core i7, Processor Speed: 2.6 GHz, Cores: 4, 
  -- Processor: Intel(R) Core(TM) i7-4960HQ CPU @ 2.60GHz
  -- Macaulay2 1.6.0.1, compiled with gcc 4.2.1
  -- res39: res of a generic 3 by 9 matrix over ZZ/101: .10664 seconds
  -- resG25: res of the coordinate ring of Grassmannian(2,5): 2.06104 seconds
  -- gbB148: gb of Bayesian graph ideal #148: 21.5273 seconds

---- a MacBook Pro (core 2 duo 2.4 GHZ, 4 GB ram):
  --- Mac OS X side:
    -- beginning computation Mon Sep 17 21:52:30 EDT 2007
    -- Darwin indigo.local 8.10.1 Darwin Kernel Version 8.10.1: Wed May 23 16:33:00 PDT 2007; root:xnu-792.22.5~1/RELEASE_I386 i386 i386
    -- Macaulay 2 0.9.97, compiled with gcc 4.0.1
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.25 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 2.43 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 19.15 seconds
  --- 32 bit GNU/Linux side, under vmware:
    -- beginning computation Mon Sep 17 21:56:26 EDT 2007
    -- Linux indigo-ubuntu32 2.6.20-16-generic #2 SMP Thu Jun 7 20:19:32 UTC 2007 i686 GNU/Linux
    -- Macaulay 2 0.9.97, compiled with gcc 4.1.2
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.204013 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 2.77217 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 19.4452 seconds
  --- 64 bit GNU/Linux side, under vmware:
    -- beginning computation Thu Aug  9 20:23:47 EDT 2007
    -- Linux indigo-ubuntu 2.6.20-16-generic #2 SMP Thu Jun 7 19:00:28 UTC 2007 x86_64 GNU/Linux
    -- Macaulay 2 0.9.97, compiled with gcc 4.1.2
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.188012 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 2.54816 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 19.8732 seconds

-- beginning computation Mon Oct 28 13:29:27 EDT 2013
-- Linux dublin.math.ias.edu 2.6.32-358.11.1.el6.x86_64 #1 SMP Wed Jun 12 03:01:25 EDT 2013 x86_64 x86_64 x86_64 GNU/Linux
-- Intel(R) Xeon(R) CPU X5460 @ 3.16GHz  GenuineIntel  cpu MHz 3159.040  
-- Macaulay2 1.6, compiled with gcc 4.4.7
-- res39: res of a generic 3 by 9 matrix over ZZ/101: .148295 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 2.50223 seconds
-- gbB148: gb of Bayesian graph ideal #148: 19.7324 seconds

-- beginning computation Thu Jan  4 20:25:24 EST 2007
-- Darwin bayer.math.columbia.edu 8.8.1 Darwin Kernel Version 8.8.1: Mon Sep 25 19:42:00 PDT 2006; root:xnu-792.13.8.obj~1/RELEASE_I386 i386 i386 iMac5,1 Darwin
-- Macaulay 2 0.9.96, compiled with gcc 4.0.1
-- res39: 0.21 seconds
-- resG25: 2.51 seconds

-- Quad-Core AMD Opteron(tm) Processor 2354, 2200 Mhz, 512 KB cache
  -- beginning computation Fri Mar 27 11:01:31 EDT 2009
  -- Linux habanero 2.6.24-21-generic #1 SMP Mon Aug 25 16:57:51 UTC 2008 x86_64 GNU/Linux
  -- Macaulay 2 1.2, compiled with gcc 4.2.3
  -- res39: res of a generic 3 by 9 matrix over ZZ/101: .292018 seconds
  -- resG25: res of the coordinate ring of Grassmannian(2,5): 3.1402 seconds
  -- gbB148: gb of Bayesian graph ideal #148: 23.6535 seconds

  -- MacBook, Intel Core 2 Duo, Processor Speed: 2 GHz, Cores: 2, Bus Speed: 1.07 GHz, 
  -- Darwin thallium.local 10.4.0 Darwin Kernel Version 10.4.0: Fri Apr 23 18:28:53 PDT 2010; root:xnu-1504.7.4~1/RELEASE_I386 i386 i386 MacBook5,1 Darwin
  -- Macaulay2 1.3.1, compiled with gcc 4.2.1
    -- beginning computation Sat Sep 11 20:24:27 CDT 2010
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: .232967 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 3.0506 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 23.7603 seconds
  -- Macaulay2 1.3.9, compiled with gcc 4.2.1
    -- beginning computation Sat Sep 11 20:24:27 CDT 2010
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: .270459 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 3.36741 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 24.0283 seconds

-- Lenovo/IBM Thinkpad R61i with Core 2 Duo at 1.5 Ghz
  -- Linux gallium 2.6.22-14-generic #1 SMP Sun Oct 14 23:05:12 GMT 2007 i686 GNU/Linux
    -- beginning computation Wed Nov 14 18:58:46 CET 2007
    -- Macaulay 2 1.0beta, compiled with gcc 4.1.3
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.352022 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 4.10826 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 31.818 seconds
  -- Linux gallium 2.6.22-14-generic #1 SMP Sun Oct 14 21:45:15 GMT 2007 x86_64 GNU/Linux
    -- beginning computation Wed Nov 14 10:52:29 CST 2007
    -- Macaulay 2 1.0beta, compiled with gcc 4.1.3
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.252016 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 3.66823 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 32.8221 seconds
  -- CYGWIN_NT-6.0 gallium 1.5.25(0.156/4/2) 2007-12-14 19:21 i686 Cygwin 
       -- this one is TOO SLOW
    -- beginning computation Tue Mar 18 15:14:38 GMTST 2008
    -- CYGWIN_NT-6.0 gallium 1.5.25(0.156/4/2) 2007-12-14 19:21 i686 Cygwin
    -- Macaulay 2 1.1, compiled with gcc 3.4.4
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: .811 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 12.496 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 47.565 seconds

-- Lenovo Thinkpad X61, new
  -- Linux
    -- beginning computation Mon Sep 17 15:03:59 CDT 2007
    -- Linux dubnium 2.6.20-16-generic #2 SMP Fri Aug 31 00:55:27 UTC 2007 i686 GNU/Linux
    -- Macaulay 2 0.9.97, compiled with gcc 4.1.2
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.304019 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 3.02419 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 22.0254 seconds
  -- Windows Vista
    -- beginning computation Mon Sep 17 20:37:59 GMTDT 2007
    -- CYGWIN_NT-6.0 LENOVO-PC 1.5.24(0.156/4/2) 2007-01-31 10:57 i686 Cygwin
    -- Macaulay 2 0.9.97, compiled with gcc 3.4.4
    -- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.578 seconds
    -- resG25: res of the coordinate ring of Grassmannian(2,5): 8.424 seconds
    -- gbB148: gb of Bayesian graph ideal #148: 32.916 seconds

-- beginning computation Tue Apr 11 20:14:38 EDT 2006
-- Linux mathvader1.math.cornell.edu 2.6.9-34.ELsmp #1 SMP Fri Feb 24 16:56:28 EST 2006 x86_64 x86_64 x86_64 GNU/Linux
-- Macaulay 2 0.9.8, compiled with gcc 3.4.5
-- res39: 0.260961 seconds
-- resG25: 3.89541 seconds

-- beginning computation Mon Dec 23 00:06:16 CST 2002
-- Linux u126.math.uiuc.edu 2.4.18-18.7.xsmp #1 SMP Wed Nov 13 19:01:42 EST 2002 i686 unknown
--    Dell_dualX/2600 
-- Macaulay 2 0.9, compiled with gcc 3.0
-- res39: 0.285138 seconds
-- resG25: 4.98438 seconds
--loaded benchmark.m2 from /home/17/dan/tmp/M2-linux/Macaulay2-0.9/lib/Macaulay2-0.9/packages/benchmark.m2

-- beginning computation Mon Dec 31 19:22:33 CST 2001
-- Linux capybara 2.4.18 #25 Sun Jul 28 15:59:31
-- Macaulay 2 0.9.2, compiled with gcc 2.95
-- res39: 0.38 seconds
-- resG25: 6.05 seconds

-- Dell Latitude C840
-- beginning computation Mon Sep 17 15:10:14 CDT 2007
-- Linux rhodium 2.6.20-16-generic #2 SMP Thu Jun 7 20:19:32 UTC 2007 i686 GNU/Linux
-- Macaulay 2 0.9.97, compiled with gcc 4.1.2
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.584037 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 6.90443 seconds
-- gbB148: gb of Bayesian graph ideal #148: 52.2593 seconds

-- beginning computation Tue Sep 30 22:05:23 CDT 2003
-- Linux rhodium 2.4.20 #2 Mon Mar 17 22:02:15 PST 2003 i686 unknown
-- Macaulay 2 0.9.2, compiled with gcc 3.2
-- res39: 0.37 seconds
-- resG25: 6.1 seconds

-- beginning computation Mon Jan  8 11:44:18 EST 2007
-- Linux rhodium 2.6.18.1 #1 SMP PREEMPT Sat Nov 11 02:11:17 CET 2006 i686 pentium4 i386 GNU/Linux
-- Macaulay 2 0.9.96, compiled with gcc 4.1.1
-- res39: 0.48003 seconds
-- resG25: 6.56441 seconds
-- gbB148: 59.9957 seconds
-- beginning computation Tue Jan  9 06:57:58 EST 2007
-- res39: 0.520033 seconds
-- resG25: 6.66042 seconds
-- gbB148: 63.472 seconds
-- res39: 0.532033 seconds
-- resG25: 6.65642 seconds
-- gbB148: 64.004 seconds
----- a bit of optimization to find_divisors:
-- res39: 0.504031 seconds
-- resG25: 6.72442 seconds
-- gbB148: 62.4079 seconds
-- res39: 0.516032 seconds
-- resG25: 6.81643 seconds
-- gbB148: 61.9039 seconds
-- res39: 0.528033 seconds
-- resG25: 6.76842 seconds
-- gbB148: 61.6319 seconds
---- start letting the matches percolate up, and insert new ones at the end
-- res39: 0.536033 seconds
-- resG25: 6.89243 seconds
-- gbB148: 57.1316 seconds
-- res39: 0.532034 seconds
-- resG25: 7.17245 seconds
-- gbB148: 51.5232 seconds
-- res39: 0.540033 seconds
-- resG25: 7.08444 seconds
-- gbB148: 52.1673 seconds
-- res39: 0.528033 seconds
-- resG25: 6.94443 seconds
-- gbB148: 52.2953 seconds
---- safe::add using long long
-- res39: 0.500032 seconds
-- resG25: 6.74442 seconds
-- gbB148: 51.5032 seconds
-- res39: 0.516033 seconds
-- resG25: 6.85243 seconds
-- gbB148: 51.8792 seconds
-- res39: 0.548034 seconds
-- resG25: 6.83643 seconds
-- gbB148: 52.0313 seconds
---- safe: back to not using long long
-- res39: 0.508031 seconds
-- resG25: 6.86843 seconds
-- gbB148: 52.3553 seconds
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.524033 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 6.84043 seconds
-- gbB148: gb of Bayesian graph ideal #148: 51.9993 seconds
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.532034 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 6.86843 seconds
-- gbB148: gb of Bayesian graph ideal #148: 52.1553 seconds
--   rework gb-default.cpp for local orderings
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.552035 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 7.42846 seconds
-- gbB148: gb of Bayesian graph ideal #148: 151.697 seconds
--   oops, re-enable the use of the previous divisor during reduction
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.532033 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 7.74848 seconds
-- gbB148: gb of Bayesian graph ideal #148: 84.3813 seconds
--   use the old code in the global case
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 0.544034 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 7.68448 seconds
-- gbB148: gb of Bayesian graph ideal #148: 54.1754 seconds <-- not quite as good

-- Darwin habanero.local 7.7.0 Darwin Kernel Version 7.7.0: Sun Nov  7 16:06:51 PST 2004; root:xnu/xnu-517.9.5.obj~1/RELEASE_PPC  Power Macintosh powerpc
-- Macaulay 2 0.9.5, compiled with gcc 3.3.0
-- res39: 0.72 seconds
-- resG25: 6.82 seconds

-- beginning computation Fri May  5 14:27:51 CDT 2006
-- Linux u123.math.uiuc.edu 2.4.22-1.2199.4.legacy.nptl #1 Sun Feb 20 18:21:21 EST 2005 i686 i686 i386 GNU/Linux
-- Macaulay 2 0.9.8, compiled with gcc 4.1.0
-- res39: 0.5 seconds
-- resG25: 6.99 seconds

-- beginning computation Fri May  5 13:38:03 CDT 2006
-- Linux u123.math.uiuc.edu 2.4.22-1.2199.4.legacy.nptl #1 Sun Feb 20 18:21:21 EST 2005 i686 i686 i386 GNU/Linux
-- Macaulay 2 0.9.8, compiled with gcc 3.3.2
-- res39: 0.56 seconds
-- resG25: 7.38 seconds

-- beginning computation Thu Oct 17 00:26:02 CEST 2002
-- Linux abuch.imf.au.dk 2.4.9-34smp #1 SMP Sat Jun 1 05:54:57 EDT 2002 i686 unknown
-- Macaulay 2 0.9.2, compiled with gcc 2.96
-- res39: 0.47 seconds
-- resG25: 7.82 seconds

-- beginning computation Sat May 25 17:27:49 CDT 2002
-- Linux lisboa.ks.uiuc.edu 2.4.16 #9 Tue Feb 19 14:11:28 CST 2002 i686 unknown
-- model name      : Intel(R) Pentium(R) 4 CPU 1700MHz
-- Macaulay 2 0.9, compiled with gcc 3.0
-- res39: 0.44 seconds
-- resG25: 8.43 seconds

-- FreeBSD euclid.math.purdue.edu 4.5-RELEASE FreeBSD i386
-- Macaulay 2 0.9.2, compiled with gcc 2.95
-- res39: 0.523437 seconds
-- resG25: 8.14062 seconds

-- Linux hn03 2.4.9-31pctr #1 SMP Thu May 9 13:22:43 CDT 2002 i686 unknown
-- Macaulay 2 0.9.2, compiled with gcc 3.0
-- res39: 0.66 seconds
-- resG25: 10.23 seconds

-- OSF1 cosimo.medicis.polytechnique.fr V4.0 1229 alpha
-- 0.683216 seconds, Macaulay 2 0.8.57, compiled with gcc 2.95

-- OSF1 agnesi.matematik.su.se V4.0 1091 alpha
-- 0.708576 seconds, Macaulay 2 0.8.57, compiled with gcc 2.95

-- OSF1 despina.ks.uiuc.edu V4.0 1229 alpha
-- 0.75152 seconds, Macaulay 2 0.8.52

-- CYGWIN_NT-5.0 NIOBIUM 1.3.2(0.39/3/2) 2001-05-20 23:28 i686 unknown (1 Ghz pentium III)
-- Macaulay 2 0.9, compiled with gcc 2.95
-- res39: 0.802 seconds
-- resG25: 10.936 seconds

-- beginning computation Tue Apr 11 19:53:29 EDT 2006
-- Linux rhodium 2.6.11 #3 SMP Tue Jun 28 07:39:29 CDT 2005 i686 unknown unknown GNU/Linux
-- Macaulay 2 0.9.8, compiled with gcc 4.0.0
-- res39: 0.749886 seconds
-- resG25: 11.1823 seconds

-- beginning computation Wed Jan 28 11:31:26 CST 2009
-- this is an eMac with an 800 Mhz Power PC G4 CPU
-- Darwin cesium.local 8.11.0 Darwin Kernel Version 8.11.0: Wed Oct 10 18:26:00 PDT 2007; root:xnu-792.24.17~1/RELEASE_PPC Power Macintosh powerpc
-- Macaulay 2 1.1.99, compiled with gcc 4.0.1
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 1.15 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 14.73 seconds
-- gbB148: gb of Bayesian graph ideal #148: 104.29 seconds

-- Linux rhenium 2.2.16 #24 Sat Jun 10 15:07:27 CDT 2000 i686 unknown
-- Macaulay 2 0.8.61, compiled with gcc 2.95
-- res39: 1.12 seconds
-- resG25: 18.45 seconds

-- Linux noether.matematik.su.se 2.1.121 #3 Wed Sep 16 10:05:16 CEST 1998 alpha unknown (600 mHz)
-- 1.20106 seconds, 0.8.46

-- SunOS sg0.math.cornell.edu 5.7 Generic_106541-08 sun4u sparc SUNW,Ultra-Enterprise
-- Macaulay 2 0.8.60, compiled with gcc 2.95
-- res39: 1.37 seconds
-- resG25: 18.29 seconds

-- Linux localhost.localdomain 2.2.1 #101 Fri Feb 5 16:17:12 EST 1999 ppc unknown
-- (250 MHz Macintosh Powerbook G3)
-- 1.67 seconds, Macaulay 2 version 0.8.52

-- beginning computation Thu Aug  9 18:07:41 CDT 2001
-- Linux rhenium 2.4.6 #61 Fri Jul 13 09:17:38 CDT 2001 i686 unknown
-- Macaulay 2 0.9, compiled with gcc 2.95
-- res39: 1.71 seconds
-- resG25: 25. seconds

-- beginning computation Sun Sep  9 19:33:49 CDT 2001
-- Linux rhenium 2.4.9 #67 Mon Sep 3 11:10:40 CDT 2001 i686 unknown
-- Macaulay 2 0.9, compiled with gcc 3.0.1  (faster than 3.0 below)
-- res39: 1.71 seconds
-- resG25: 27.27 seconds

-- Linux rhenium 2.4.4 #55 Tue Jun 26 13:44:25 CDT 2001 i686 unknown
-- Macaulay 2 0.9, compiled with gcc 3.0
-- res39: 1.93 seconds
-- resG25: 28.47 seconds

-- Linux hypatia.matematik.su.se 2.0.34 #2 Thu May 7 10:48:04 EDT 1998 alpha unknown
-- 2.01366 seconds, Macaulay 2 0.8.52a, compiled with gcc 2.90 -- with DEBUG on!

-- SunOS orion.math.uiuc.edu 5.5.1 Generic_103640-26 sun4u sparc SUNW,Ultra-4
-- 2.14 seconds, Macaulay 2 0.8.53, compiled with gcc 2.8

-- Linux yttrium 2.2.6 #82 Sun Apr 18 15:06:16 CDT 1999 i686 unknown
-- e-machine: Cyrix M II - 333 MMX
-- 2.45 seconds, Macaulay 2 0.8.53, compiled with gcc 2.91

-- SunOS andy 5.5.1 Generic_103640-29 sun4u sparc SUNW,Ultra-2
-- Macaulay 2 0.8.60, compiled with gcc 2.95
-- res39: 2.7 seconds
-- resG25: 37.77 seconds

-- beginning computation Thu Dec 26 21:48:58 UTC 2013
-- Linux pi-dan 3.10.24+ #614 PREEMPT Thu Dec 19 20:38:42 GMT 2013 armv6l GNU/Linux
-- ARMv6-compatible processor rev 7 (v6l)  BogoMIPS 2.00  
-- Macaulay2 1.6.0.1, compiled with gcc 4.6.3
-- res39: res of a generic 3 by 9 matrix over ZZ/101: 3.45706 seconds
-- resG25: res of the coordinate ring of Grassmannian(2,5): 49.9111 seconds
-- gbB148: gb of Bayesian graph ideal #148: 404.365 seconds

-- Linux geometry 2.3.18 #2 Thu Sep 16 17:50:47 CDT 1999 i586 unknown
-- 3.46 seconds, Macaulay 2 0.8.55, compiled with gcc 2.95

-- Linux geometry 2.1.121 #33 SMP Tue Sep 15 21:44:25 CDT 1998 i586
-- 4.01 seconds, Macaulay 2 version 0.8.47, compiled with gcc

-- Linux geometry 2.2.0-pre4 #65 Mon Jan 4 20:14:06 CST 1999 i586 unknown
-- 4.17 seconds, Macaulay 2 version 0.8.50, compiled with gcc 2.8.1

----- with SHAREDLIBS, including interpreter but not engine:
-- Linux geometry 2.2.0-pre4 #65 Mon Jan 4 20:14:06 CST 1999 i586 unknown
-- 4.27 seconds, Macaulay 2 version 0.8.50

-- Linux geometry 2.2.2 #77 Wed Feb 24 10:40:05 EST 1999 i586 unknown
-- 4.31 seconds, Macaulay 2 0.8.53, compiled with gcc 2.91, statically linked

-- CYGWIN_NT-4.0 GEOMETRY 20.1 (0.3/1/1) 1998-12-3 20:39:18 i586 unknown
-- 4.327 seconds, Macaulay 2 version 0.8.52
-- gcc version egcs-2.91.60 19981201 (egcs-1.1.1 release)

-- Linux geometry 2.2.2 #77 Wed Feb 24 10:40:05 EST 1999 i586 unknown
-- 4.36 seconds, Macaulay 2 0.8.53, compiled with gcc 2.8, dynamically linked

-- Linux geometry 2.2.2 #77 Wed Feb 24 10:40:05 EST 1999 i586 unknown
-- 4.38 seconds, Macaulay 2 0.8.53, compiled with gcc 2.91

----- with SHAREDLIBS, including engine and interpreter:
-- Linux geometry 2.2.0-pre4 #65 Mon Jan 4 20:14:06 CST 1999 i586 unknown
-- 4.81 seconds, Macaulay 2 version 0.8.50

-- IRIX illi 5.3 12201932 IP22 mips
-- 	5.49 seconds		0.8.10

-- with SHAREDLIBS and DEBUG and egcs compiler
-- Linux geometry 2.2.2 #77 Wed Feb 24 10:40:05 EST 1999 i586 unknown
-- 5.65 seconds, Macaulay 2 0.8.52, compiled with gcc 2.91

-- HP-UX ux1 B.10.10 U 9000/819 65611361 unlimited-user license
-- 	6.26 seconds

-- NeXTstep mms-hal 3.2 2 i586
-- 	6.74973 seconds

-- SunOS saturn.math.uiuc.edu 5.5.1 Generic sun4m sparc SUNW,SPARCstation-5
-- 7.71 seconds, Macaulay 2 version 0.8.41

-- SunOS orion.math.uiuc.edu 5.5 Generic sun4d sparc SUNW,SPARCserver-1000
-- 	8.41 seconds		0.8.46

-- SunOS kilburn 4.1.4 2 sun4m
-- Macaulay 2 version 0.8.46
-- 10.17 seconds

-- HP-UX skolem A.09.05 A 9000/750 2013387634 two-user license
-- 	10.25 seconds

-- Linux homotopy 2.0.14, NEC Versa E, i486 laptop, DX4/100, 25 mhz
-- 	Macaulay 2, version 0.8.26
--      	22.04 seconds

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages  PACKAGES=Benchmark RemakePackages=true RerunExamples=true IgnoreExampleErrors=false RemakeAllDocumentation=true"
-- End:
