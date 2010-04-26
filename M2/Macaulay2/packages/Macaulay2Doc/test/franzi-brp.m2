-- load "franzi-brp.m2" 
R = ZZ/2[x,y,z, MonomialOrder=>Lex];
l = apply(gens R, x-> x^2+x);
QR = R/l;
I = ideal(x+y, y*z);
time C = gens gb I;
time B = gbBoolean I;
assert( gens B - C == 0 )

R = ZZ/2[x,y,z, MonomialOrder=>Lex];
l = apply(gens R, x-> x^2+x);
QR = R/l;
I = ideal(x+y, y*z, x, y);
time C = gens gb I;
time B = gbBoolean I;
assert( gens B - C == 0 )

R = ZZ/2[a..t, MonomialOrder=>Lex]
l = apply(gens R, x-> x^2+x);
QR = R/l;
I = ideal {
  b*c+1,
  a*b*c*d*f*g*h*t + i*o*p*q*r*s*t + r + s, 
  a*c*e*i*q + d*m*o*q + f*g
};
time C = gens gb I;
time B = gbBoolean I;
assert( sort gens B - sort C == 0 ) 
-- we computed 10279 S Polynomials and added 73 of them to the intermediate basis.
     -- used 0.082521 seconds, Revision: 10970

R = ZZ/2[a..t, MonomialOrder=>Lex]
l = apply(gens R, x-> x^2+x);
QR = R/l;
I = ideal {
  b*c+1,
  a*b*c*d*f*g*h*t + i*o*p*q*r*s*t + r + s, 
  b*c*l*o*r*s + b*s + i + m*n*q, 
  a*c*e*i*q + d*m*o*q + f*g, 
  i + l*m*n + q*r + q +1
};
time C = gens gb I;
time B = gbBoolean I;
assert( sort gens B - sort C == 0 ) 
-- we computed 44332 S Polynomials and added 157 of them to the intermediate basis.
     -- used 0.626917 seconds, Revision: 10970 


R = ZZ/2[a..t, MonomialOrder=>Lex]
l = apply(gens R, x-> x^2+x);
QR = R/l;
I = ideal {
  b*c,
  a*b*c*d*f*g*h*t + i*o*p*q*r*s*t + r + s, 
  b*c*l*o*r*s + b*s + i + m*n*q, 
  a*c*e*i*q + d*m*o*q + f*g, 
  i + l*m*n + q*r + q
};
time C = gens gb I;
time B = gbBoolean I;
assert( sort gens B - sort C == 0 ) -- we computed 9791 S Polynomials and added 61 of them to the intermediate basis.
     -- used 0.063325 seconds, Revision: 10970 


R = ZZ/2[a..t, MonomialOrder=>GRevLex]
l = apply(gens R, x-> x^2+x);
QR = R/l;
I = ideal {a*b*c*d*e,
  a+b*c+d*e+a+b+c+d , 
  j*h+i+f +a*b+c*d+e*f+g*h*i+i*j+a*t+s*r, 
  g+f +m*n+o + o*p + r*s*t+a*l+h*i*q*s + k*c,
  j+i+d*c, 
  r+s+t*a*b*c*d*f*g*h+i*o*p*q*r*s*t, 
  m*n+o*p, 
  b*s+q+p*n*m+i + i*j*h*a*c*t, 
  b*s+q*n*m+i+b*l*o*r*s*c, 
  b*k+q+l*n*m +n,
  i*q*a*c*e+f*g+o*q*d*m +b+d, 
  b*s+q*n*m+i+j*s*t+s, 
  b*k+r*q+l*m+i*j+n, 
  b*k+d*n*m+i, 
  b+q+l*n*m+i*d, 
  a*k+c*l*n*f, 
  q*r+c+q+l*n*m+i
};
time C = gens gb I;
R = ZZ/2[a..t, MonomialOrder=>Lex]
l = apply(gens R, x-> x^2+x);
QR = R/l;
C = sub(C, QR)
time C = gens gb C;
I = sub(I,QR)
time B = gbBoolean I; -- used 3.48974 seconds at Revision: 10960 -- used 1.43021 seconds Revision: 10961
assert( sort gens B - sort C == 0 )
-- we computed 102631 S Polynomials and added 301 of them to the intermediate basis.
     -- used 2.14443 seconds, Revision: 10970


R = ZZ/2[vars(0..14), MonomialOrder=>Lex]
l = apply(gens R, x-> x^2+x);
QR = R/l;
I = ideal(b*k+a+o+1,a*k+b,a*c*i+c*d*i+a*i*o+c*d+1,h*i*j*l+c*h*j+i*l+d+l,b*c*d*f*n+c*d*f+b*d*n+b*c+b*f+d*f+b*n+b+c+d+e,f,a*b*g*j*n+b*g*n+a*b+b*g+b+g,e*i*m*o+e*h*i+e*i*o,d*f*g+c*f*o+f+i,f*g*j+h*m+h+j,b*d*i+d*f*j+f*i*j+k,e*o+o,d*i*k+d*i+m,d*e*k*o+d*e*k+d*g*o+e*g*o+e*k*o+d*e+e*g+g*o+n+1,a*d*e+a*e*j+a*d*m+a*e*m+d*j*m+a*m+j+o+1)
time C = gens gb I;
time B = gbBoolean I;
assert( sort gens B - sort C == 0 ) 

R = ZZ/2[vars(0..17), MonomialOrder=>Lex]
l = apply(gens R, x-> x^2+x);
QR = R/l;
I = ideal(0,0,0,0,b*f*i*k+h*i*j+f*j*k+h*j*k+b*k*l+f*i+f*j+a,d*g*m*o+h*m*o+h*o*r+g*h+d*o+i*r+m*r+b,a*d*j*l+c,b*c*k*l*q+b*c*g*l+c*k*l*n+c*l*n*q+k*l*n*q+b*g*l+g*k*l+b*c*q+l*n*q+b*n+d,b*h*j*l+d*h*l*p+b*d*h+b*j*l+j*l*p+e*p+e,i*m*p*r+j*m*p*r+f,c*f*g*i*n+g,c*f*j*l*o*r+j*q*r+h,a*f*h*p+h*m*o*r+a*m*p+a*o*p+h*o*p+f*m*r+h*m*r+o*r+i,0,e*f*g*h*m*q+e*f*g*h+b*f*g+e*m*q+f*m*q+k,a*e*f*r+e*f*g+c*g*r+c*p+g*p+f*r+e+l,d*e*h*i*k*m+d*e*f*h*m+d*e*i*m+e*i*m+d*k*m+m,a*c*e*j*n*q+c*e*m*n+n,a*f*n*o+o,a*j*k*l*n+p,b*c*e*n*q+q,d*e*i+r);
time C = gens gb(I, Algorithm=>Sugarless);
time B = gbBoolean I;
assert( sort gens B - sort C == 0 ) 
