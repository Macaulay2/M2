-- generated with routine to make random networks in 2010March/BRP/test-results/

--rungb = (I) -> gens gb(I, Algorithm=>Sugarless)
rungb = (I) -> gens gb(I, Algorithm=>Test)
--rungb = (I) -> gbBoolean I

GBExamples  = {
     -- format: {header string, 
     --            string defining: I, maybe answer'gb, maybe test'code
     --	           }
  {"random18-1",
   ///
      I = ideal(a*c*f*q*r+a*c*j*q+b*c*j*q+a*b*c*r+a*b*f*r+b*c*j*r+a*b*q*r+b*c*q+b*j*r+f*j+a,e*g*j*k*l*n*q+e*g*j*l*n+j*k*l*n+e*g*n*q+j*k*n*q+e*g*k+g*k+e*l+k*q+b,f*g*i+g*i*q+g*j+g*q+i*q+j*q+c,a*f*h*l+c*h*k+d,c*l*n*q*r+c*g*n*r+c*j*l+e,g*h*k*m*q+f*k*m+g*m+f,c*f*i*j*k*m+c*f*j*m*r+f*k+g,d*f*g*i*p*q+f*g*o*p*q+d*f*i*p+d*g*p+f*o*p+d*g+g*q+p*q+h,a*d*h*i*j+d*h*i*k*m+a*h*j*k+a*i*j*k+a*i*j+d+i,a*d*f*k*l+a*f*j*l*q+a*d*k*l*q+d*f*k*l+d*k*l*q+a*d*l+d*k*q+f*k*q+a*j+l*q+j,b*c*e*i*j*m+b*c*j*m*o+k,d*g*k*o*p+d*k*m*o*p+d*g*j*k+d*j*k*o+d*g*o+d*k*p+l,h*l*n*p+g*p+m,a*l*m*q*r+a+n,d*g*l*r+o,b*e*f*k*p+b*c*f*n*p+c*k*n*p+b*e*f+f*k*n+b*k*p+c*n*p+b*c+e*n+p,f*i*q+f*n+q,a*j*l*q+a*f*m*q+h*l*q+h*l+r)
      str = "we computed 205383 S Polynomials and added 513 of them to the intermediate basis.
           -- used 11.0317 seconds
           -- used 0.422026 seconds"
   ///},
   {"random18-20",
   ///
      I = ideal(d*e*f+a,a*b*h*i*j*n*o+a*b*h*i*j*o+a*b*h*j*o+a*b*h*i+a*h*n*o+a*j*o+a*n*o+j*o+b,e*k*l*m+c*m+c*o+c,a*c*e*h*j*q+a*h*j*q+a*c*q+d+h,j*l*m*n+d*k*m+f*j*n+d*m*n+f*m*n+d*j+e,b*c*h*m*n+b*c*m*n*q+b*h*n*q+h*n*q+e*h+b+f,b*c*e*j*n*r+b*c*j*n+b*c*n*r+e*n*r+j*n*r+n*r+g,b*e*m*n*p+b*d*e*p+b*d*n+e*m*n+d*f*p+m*n+h+1,c*d*e*j*p*q+c*e*j*p*q+c*e*j*p+e*j*p*q+c*d*j+i,b*g*h*i*l+b*h*i+b*m*q+l*m+i*q+j,b*e*o*q*r+e*i*l*r+b*i*l+e*l*q+i*q*r+e*i+b*o+k+1,b*e*g*i*m+b*g*i*l+e*g*i+e*i*m+l,b*d*f*g*j+b*d*f*g*n+b*d*g*h*n+b*d*f*j*n+b*f*g*j+d*f*j*n+b*d*f+f+m,d*g*l+n,c*l*q+o,a*d*l*m*n*r+a*d*l*n+a*k*m*n+k*l*m*r+a*d*n*r+d*k*m+d*l*n+l*m*r+k*n*r+p,c*f*j*k*m*o+c*d*j*k*o+f*j*k*o+c*d*m*o+q,c*k*o*r+k*m*o*r+h*k*r+k*r+r)
        str = "we computed 46765 S Polynomials and added 209 of them to the intermediate basis.
     -- used 0.969434 seconds
     -- used 12.4416 seconds"
   ///},
   {"random18-21",
   ///
      I = ideal(b*i*k*n*q+g*k*p*q+a,e*k*q+a*c+b,a*d*i*l*m*p+a*c*i*m*p+a*d*i+c,a*j*o+k*o+d,e*f*h*m*n*r+j*r+e,f*h*i*p*q+f+g+i,f*g*j*n+c*g*n*r+f*g*n*r+g*j*n*r+a*c*g+f*j*n+a*r+g+1,h*j*m*n*o*q+h*n*o*q+m*o+q,c*d*e*l*o*q+d*e*f*l*q+d*e*l*o*q+c*d*f*l+c*d*e*o+c*d*f+d*f*l+o*q+i,d*h*j*o*r+d*h*j*o+h*j*m*r+j,b*k*l*n*q*r+k,c*e*h+e*f*h+l,d*k*l*m*n+a*d*j*k+a*k*l*m+d*k*l*n+j*k*l+d*j*n+l*n+m,a*c*f*j*o*r+a*c*f*o+f*j*l+a*f*o+c*f*r+n+r,e*i*n*q*r+o,c*d*f*m*n+b*c*d+p,a*b*d*j+b*d*n*q+a*b*d+b*n+q,h*k*n+k*l+l+r)
        str = "we computed 16230 S Polynomials and added 105 of them to the intermediate basis.
     -- used 0.195925 seconds
     -- used 0.188992 seconds"
   ///},
   {"random18-5",
   ///
      I = ideal(a*c*h*n*q+c*n*p*q+a*g*n+h*n*p+a+n,d*g*i*j*k*q+d*i*j*k*q+d*j*k*q*r+i*j*q*r+d*i+g*r+b,a*d*g+d*g*h+g*h*p+d*h+c,a*b*c*f*k*l*m+a*b*c*k*l+a*c*l+a*c*m+c*f*m+d,a*d*g*l*n+d*g*n*o*r+g*l*n*r+g*n*o+e,f*g*i*r+f,a*k*p*q*r+n*o*p*q+a*k+g+o,b*c*i*j*k+a*b*c*g+b*c*i*k+a*g*j+b*g*j+c*g+h,a*b*j*n+a*h*j*n+a*h*k*n+a*b*k+h*j*k+e*k+h+i+1,g*i*l*m*o*r+g*i*j*l*r+j*l*m*o*r+j*l*m*r+g*l*m+i*l*o+j*m*o+i*l+o*r+j,a*c*e*g*h*i*m+a*g*i*m+g*h*i*m+c*g*m+k,c*g*i*m*p+c*j*m*o*p+c*g*o*p+c*m*o*p+c*g*i+g*i*j+c*i*o+c*j*p+c*j+o*p+l,a*d*h*n+a*m+e*m+m,b*c*d*e*n*p*r+b*c*d*n*p+b*d*e*n*r+n+1,d*g*l+g*i*r+c*l+o,e*f*n*o+n*o+p,e*h*j*p*q+h*l*p+j*l*p+d*h*q+d*q+l+q,e*h*l*m*q*r+d*e*l*q*r+e*h*m*q*r+e*h*l*q+h*m*q*r+d*h*l+d*q*r+r)
      str = "we computed 300641 S Polynomials and added 500 of them to the intermediate basis.
     -- used 14.7089 seconds
     -- used 1.37147 seconds"
   ///},
   {"random18-7",
   ///
      I = ideal(a*c*f*h+c*h*j+a,b+f,a*c*e*h*n+a*c*e*k*n+a*c*e*h+a*c*h*o+a*e*h*o+a*k*n*o+a*c*h+c*h*k+a*h*o+c+o,c*e*h*i*o*q+c*e*i*m*o+c*e*h*q+e*h+d,b*e*i*o*p*q+b*e*o*q+b*e*p*q+b*e*p*r+e*o*q*r+o*q*r+b*i+e,a*c*k+b*c*k+f,c*d*i*n+b*i*n+g,b*e*f*q+b*g*n*q+f*g*n+e*g*p+f*p*q+n*q+h,b*f*i*k*p*q+b*k*l*q+b*k*p*q+k*p*q+i*k+i+k+q,g*k*l*q*r+f*g*k*r+f*g*l+f*k*q+l*q*r+k*q+j,c*h*i*k*l*p+c*h*i*k*l+c*h*k*l+g*h*i*p+c*g*l*p+g*i*l*p+g*h*p+i*l*p+k,a*f*g*h*l+a*f*h*l*o+a*h*k*l*o+f*g*h*k+g*h*k*l+f*g*l+g*l+k+l,d*i*l*o+h*i*k*r+h*i*o*r+m,g*i*k*l*q+d*k*o*q+d*k*l+d*i*o+l*o*q+n,b*c*h*o*r+c*h*l*o+l*n*o*r+b*l*o+l*o,c*d*h*j*k+c*d*j*k+a*d*h+a*c+j*k+p,a*b*g*j*o*q+a*b*m*o*q+a*j*m*o*q+a*b*g*j+b*g*j*m+q,k*n*p+r)
      str = "we computed 143057 S Polynomials and added 341 of them to the intermediate basis.
     -- used 4.6263 seconds
     -- used 1.35071 seconds"
   ///},
   {"random18-8",
   ///
      I = ideal(a*h*m*o+a*e*h*r+e*h*o+a*j*r+h*j*r+e*m*r+j*m*r+m*o+a,a*b*g*k*l*o*q+a*b*g*k*o+b*g*l*o+k*o+b,a*l*n*o*r+a*n*o*r+e*n*o*r+a*n*r+e*o+n*o+c,a*i*j*m*q+a*b*l*m*q+i*j*l*q+a*b*i+b*i*j+d,n*p+p*q+e,a*f*h*i*o*p+f*k*o*p+a*f*h+a*i*p+a*i+h*k+f*p+i*p+f+1,a*i*l*n*o*r+a*f*i*n*r+a*i*l*n*r+f*i*n*o*r+i*l*o*r+i*n*r+g,f*g*h*o*r+f*g*h*p*r+g*h*o*p+f*k*p*r+f*h*p+f*k*p+h*o*r+o*p+f*r+h,h*j*l*p+h*j+i,b*d*g*l*q+c*g*q+j,d*f*l*m*n*p+d*f*m*o*p+d*l*n+l*n*p+m*o*p+d*f+f*o+d+k,f*g*i*k*m*q+f*h*i*k+g*i*k+h*i*m+f*i*q+h+l+1,b*c*i*j*p+b*e*i*p+c*d*p+d*e+m,d*e*g*k+n,d*h*m*q+d*f*q+j*l*q+o,e*f*p+e*f*q+p,a*c*h*i*j*m+a*c*h*i*j+a*h*i*j*l+a*i*j*l+a*c*j*m+a*i*j*m+i*j*l+h*i*m+a*l*m+a*c+q,b*c*d*e*f*n+b*c*e*f*g*n+b*c*d*e*n+b*c*e+r)
        str = "we computed 20079 S Polynomials and added 116 of them to the intermediate basis.
     -- used 0.400399 seconds
     -- used 0.041512 seconds"
   ///},
   {"random18-14",
   ///
      I = ideal(b*e*g*m*o+e*g*n*o+m*n*o*r+e*o+a,a*c*f*g*h+f*h*o*p+b+c,c*d*g*m*n+c*d*g*n*q+c*d*g*m+c*j*m*q+g*j*m*q+c*n*q+m*q+1,a*b*i*j*k+a*b*i+b*i*j+a*h*k+a*j*p+d,b*c*d*e*j*l+b*d*e*f*l+e,c*d*j*m+d*g*m*o+e*m*o+j*m*o+c*m+g*o+d+f,c*g*h*k*o+g,d*o*p+h,a*e*h*k*q*r+i,a*b*j*m*n+a*d*f*m+a*d*j+a*j*n+b*d+d+j,d*g*j*n*o+d*g*k*n*r+g*j*k*o+d*k*n*o+g*j*o*r+j*n*r+k*n*r+d*g+d*j+k,a*i*k*n*q+a*i*k*n+d*i*j*q+d*n+j*n+a+l,d*j*k*o*p+f*j*m*o+d*k*m*p+j*k*m*p+d*f*k+j*k*m+k*m*p+m*o+d+m+1,e*g*k*m*n+g*k*m*n*p+e*g*n*p+g*k*n*p+e*g*n+k*n*p+k*m+n,a*g*h*m*q*r+o,c*e*n*o*p+b*c*g*n+b*c*e*o+c*e*o*p+b*e*n+g*n*o+c*o*p+e*p+p,a*b*d*i+a*b*d*k+a*d*i*k+b*k+q,a*b*h+a*f*n+b*h+r)
        str = "we computed 30440 S Polynomials and added 162 of them to the intermediate basis.
     -- used 0.488664 seconds
     -- used 4.28971 seconds"
   ///},
   {"random18-15",
   ///
      I = ideal(e*k*n+b*n+e*q+a,c*d*e*g*k*o*q+c*d*k*o+c*g*o*q+b,a*e*f*g*q*r+a*e*f*g*q+a*e*f*j*r+a*j+c,a*g*i*k*l+a*b*g*k+a*b*h+a*h*i+h*i+d,a*d*e*i*k*q+a*b*e*k*q+a*d*i*k*q+b*e*k+e*k+e+i,a*h*j*k+h*j*k*l+a*c*k+j*k*l+k*l*p+f+k,a*b*j*m*o*p+g,a*b*p*r+a*c*q+b*c*q+h,f*g*m*n*o+c*f*n*o+f*r+i,b*h*n*p*r+b*c*h*r+c*g*h*r+b*n*p+g+j,f*j*k*l*m*q+f*g*j*k+f*g*j*l+f*k*l*q+g*j*k+g*k*l+j*k*l+f*l*q+k,g*h*j*k*n*q+g*h*j*n*q+g*j*l*n*q+h*j*l*n*q+g*h*j*l+g*h*j+g*j*q+h*l+l,a*c*g*h*i*k+c*g*h*i+a*h*k*n+g*n+m,d*f*g*h*k*p*q+f*g*k*p*q+g*h*k*q+n,a*d*g*q+d*i*q+o,a*f*j+b*q*r+b*q+f*r+p,g*h*n*r+q,a*c*f*k*m+a*f*k*o*r+a*f*k*o+a*f*m*r+c*k*m*r+f*k*o+a*f*r)
        str = "we computed 83381 S Polynomials and added 236 of them to the intermediate basis.
     -- used 7.65315 seconds
     -- used 0.140846 seconds"
   ///},
   {"random18-16",
   ///
      I = ideal(e*h*i*j+f*h*i*j+e*f*h*n+e*h*j*n+e*f*h+f*i*j+h*i*n+f*j*n+a,d*e*m*o*r+i*m*o*q*r+d*e*i*o+e*m*o*r+i*o*r+e*i+o*q+o*r+b,i*l*n*o*q+c*l*n*q*r+l*n+o*r+c,d*l*m*n*o*r+i*l*m*n*r+d*l*m*o*r+i*m*o*r+d*i*l+i*l*o+i*o+d*r+d,i*j*l+e,a*f*o*p+a*n*o*p+a*l*o+a*o*p+f+o,a*f*j*q+f*j*k*r+d*j*k+d*f*q+d*k*r+f*r+g,g*l*p*q+b*q+b+h,b*c*m*n*p*q+b*m*n*p*r+b*m*n*p+b*n*q*r+b*p*q*r+c*m*p+n*p*q+m*p+i,c*f*j*n+e*f*k*q+e*f*n*q+e*f*j+f*j*q+j,g*h*k*n+g*j*m*n+j*k*m*n+g*j*m+j*l*n+g*h+k,l*n*p*q*r+i*p*q*r+l*p*q*r+l*n*p+i*l*q+a*q+l,a*c*f*l+c*j*l+a*f*o+a*c+j*l+c*o+m,c*d*l*m*p+b*c*l*m+b*c*m*r+n,a*c*d*f*o*q+a*c*e*f*o*q+a*c*e*f*o+a*c*e*o+c*d*f+a*e*f+e*o*q+a*c+o+q,a*l*q*r+p,a*h*i*m+g*h*i*m+a*h*m*p+q,a*e*f*g*r+a*e*i*n*r+a*e*i*n+f*g*n*r+e*i*n*r+e*g*i+e*g*n+a*n*r+r)
        str = "we computed 23136 S Polynomials and added 146 of them to the intermediate basis.
     -- used 0.354617 seconds
     -- used 0.073074 seconds"
   ///},
   {"random18-17",
   ///
      I = ideal(d*g*i*r+a,a*d*e*l*n*r+a*d*l*n*p*r+d*e*n*p*r+d*l*n*p*r+d*l*n+d*e*p+d*r+a+b+e+n,b*g*n*p+g*k*p*q+h*k*p*q+g*n+h*p+c,a*d*k*m*n+d*l*n+d,f*m*n*r+k*m*n*r+e,b*c*d*e*h+b*d*e*h*k+c*d*h*k*l+b*c*d*h+b*d*h*l+c*h*k*l+d*e*h+d*h*k+d*h*l+b*h+f,c*d*g*n+g,a*j*l*m*q+a*i*m*o*q+a*l*m*o*q+a*m*o*q+l*o*q+j*q+h+q,a*d*h*o*q*r+d*o*p*q+a*d*h*r+h*o*q*r+h*p*r+a*o+i,c*e*g*l*o*r+c*e*l*o*r+c*e*o*r+e*g*o*r+b*l*o*r+b*e*g+b*l*r+b*o*r+c*g+b*o+j,d*f*i*l*q+c*d*f*l+b*f*i*l+b*c*i*q+c*d*i+c+k+q,b*c*h*k*q+b*c*g*k+b*h*i*k+b*i+l,b*c*i*o+d*i*n*p+b*d*p+d*o*p+m,d*f*i*k+e*f*k*q+c*e*i+k*q+n,g*h*k*l*n+c*h*l*n+h*k*l*n+c*k*l+o,a*e*g*i*j*n*o+i*o+p,a*h*i*k*p*q+a*k*p*q*r+h*k*p*q*r+i*k*q*r+a*h*k+k*p*r+q,d*g*k*o*q+b*g*o*q+d*g*k+d*j*k+b*g*o+b*j*o+g*k+k*o+r)
        str = "we computed 50634 S Polynomials and added 255 of them to the intermediate basis.
     -- used 1.53709 seconds
     -- used 2.59945 seconds"
   ///},
   {"random18-18",
   ///
      I = ideal(a*l*m*n*q*r+h*l*m*n*r+a*l*n*q*r+a*l*m*n+a*h*l*r+h*l*q*r+h*m*q*r+l*m*r+a,c*f*l*n*r+f*j*m*n+c*f*l+f*n+c*r+b+f+r+1,a*d*g*j*k+a*d*g*k*p+a*g*j*o+d*j*k*p+g*j*o*p+j*k*o*p+d*o*p+o*p+c+o,d*f*k*l+d*f*l*o+f*g*l*o+d*g*o+d,j*k*m*o+a*i*o+c*k+e,d*f*o*q*r+f*l*o*q*r+d*f*n*o+f*l*o+f*o*q+d*q*r+d*n+f+r,c*i*j*l*r+c*i*l*p+c*e*j*r+c*i*l*r+c*j*l*r+e*j*p*r+j*l*r+c*p*r+g,a*b*d*m*q+b*d+b*o+h+1,a*m*r+i,e*f*g*h*o+e*g*o+e*h*o+e*l+j,b*c*e*l*m*o+b*e*m*n*o+b*l*n*o+c*l*n+k+1,a*e*f*j*m*o*q+a*e*j*m*q+e*f*j*o*q+a*e*f*j+e*f*j*o+f*m*o*q+e*m*o+e*q+l,c*d*n*o+m,b*i*j*n*p*q+i*j*m*p*q+i*j*n*q+i*j*p*q+n,e*f*h*l*r+d*e*f*l+f*h*m+o,a*e*g*m*p+b*e*g*m+a*b*j*m+a*b*e*p+a*b*m*p+b*g*m*p+e*g*m+a*j*m+e*j+p,a*d*g*h*i*j*p+a*d*h*i*j+a*g*j*p+a*g*p+q,a*b*d*j*k*p+a*b*f*k+b*d*j*k+a*b*j+a*f*k+f*j*k+d*f+a*p+k+r)
        str = "we computed 137437 S Polynomials and added 420 of them to the intermediate basis.
     -- used 4.20666 seconds
     -- used 0.905 seconds"
   ///},
   {"random18-19",
   ///
      I = ideal(a*b*d*f*n+a*d*f*k*o+a*d*k*n+a*d*n+a*k+b*n+a,f*j*k*l*q+f*j*l*n*q+f*i*j*l+f*j*k*n+f*k*n*q+j*k*l+k*l*n+f*i+b,d*i*k*r+d*k*l+c,b*c*i*l*q+b*c*l*m*q+a*c*i*m+a*q+d,c*g*n*o+c*g*o*q+g*n*o+e*l*q+c*o+e*o+e+1,c*e*f*o*q+e*f*l*q+f*o*q+c*e+e*o+f,a*d*e+g*j*o+g*j+g+1,g*k*m*q*r+c*g*n*q*r+g*k*n*q*r+c*g*m*q+c*k*n*q+c*g*n*r+k*m*r+k*q*r+q*r+h,a*d*e*k*l+d*e*l+i*k*l+i,b*i*l*m*q+b*f*i*q+b*f*p+f*i*p+b*p+i*p+j+l,b*c*g*i*j*m+b*h*i*j*m+g*i*j*m+k,c*d*g*p+l*p+l,a*f*h*k*n+a*c*f*h*o+a*c*k*n*o+a*c*f*n+a*h*n*o+m,a*b*k*p+a*b*q+k*o+n,d*e*j*l*m+e*j*k*l+d*f*l*m+e*l*m+f*l*m+o,a*d*g+a*g*q+a*d+a*g+a*p+p,b*e*l*r+b*d+q,a*i*l*m*n+a*g*i*n+a*l*m*q+i*l*m*q+a*g*m+g*l*q+i+r)
        str = "we computed 401560 S Polynomials and added 681 of them to the intermediate basis.
     -- used 26.5565 seconds
     -- used 0.316019 seconds"
   ///},
   {"random18-11",
   ///
      I = ideal(c*i*k+a,c*i*k*l*p*q+c*i*k*l*q+b+c,a*f*h*i*j*n+a*h*j*m*n+a*f*i*m+a*h*i*m+f*h*i*m+a*f*i*n+a*h*m*n+f*m+c,b*j*k*l*n+b*c*k*n+b*k*l*n+j*k*l*n+b*n*o+c*j+c*n+d,c*i*k*l*p*r+m*p+e,e*f*g*r+d*f*j*r+f*g*j*r+e*g*j+f*g*j+f,c*e*g*m*q*r+f*g*m*r+e*f*m+c*f*r+g*q+f+g,a*f*j*r+e*f*l*r+h*j*r+a*h+h,a*e*g*m+d*g*k+i,c*d*g*h*l*r+d*h*n*r+d*g*l+j,b*i*m*p*q+c*i*m*p*r+c*i*m*p+c*i*m*r+b*i*q*r+m*p*q+b*p*r+m*r+q*r+b+k,c*f*h*i*n*p+c*f*h*i*n+c*i*n*p*r+c*h*i+c*i*p+h*r+f+l+r+1,c*f*g*j*l*p+c*f*l*p+f*m+f+m,a*d*f*j*l+a*b*d*l*o+a*d*f*o+f*o+n,b*c*i*n*q*r+a*b*c*q*r+a*c*i*n+a*c*i*q+b*i*n*r+a*b*c+a*b*i+b*c*i+b*i*q+b*i+o,a*b*d*k+c*d*k*n+c*g*k*n+p,b*f*g*i*m+b*c*g*j*m+b*f*i*m+b*i*j*m+b*c*j+b+q,c*f*h*i+d*f*h*o+c*h*j+h+r)
        str = "we computed 540062 S Polynomials and added 879 of them to the intermediate basis.
     -- used 59.927 seconds
     -- used 4.95242 seconds"
   ///},
   {"random18-13",
   ///
      I = ideal(a*d*f+a,g*k*o*p*q+i*m*p*q+b+1,d*e*h*m*o+d*f*m*o+d*e*f*r+d*h*m*r+d*f*h+d*h*m+f*o*r+e*r+c,c*e*f*i*l*q+f*i*l*q+e*f*i+d+e,d*e*k*o*p*q+d*k*o*p*q+d*e*o*p+e*p*q+e,a*b*e*f*g*j+b*e*f*g+a*g+f,b*c*h*i*l+c*d*h*i*l+b*c*d*i*q+c*d*h*i+b*c*d*l+b*d*h*q+h*l*q+d*i+g+h,e*j*k*q*r+j*k*o*q+e*o*q*r+j*k*o+e*o*q+k*o+h,c*f*l*p+c*d*e+c*d*o+c*d*p+i,b*e*l*o*r+e*f*n*o+b*e*l+e*f*l+b*e*r+e*l+j,a*b*i*n*p*r+a*i*n*o*p*r+b*n*o*p*r+a*i*o*r+a*o*p+b*o*p+b*p+i+k,d*j*m*n*p*q+d*f*j*n*q+d*f*j*p*q+d*f*p*q+n*p+l,g*h*i*k*n*q+g*h*k*q*r+i*k*n*q+i*k*n*r+h*k*n+i*k*q+h*q+m,b*f*i*l*n*p+b*i*l*n*o*p+b*f*i*n*o+i*l*n*o*p+f*i*l*o+f*i*p+l*o*p+n+o,d*i*l*m*n*o+f*i*l*m*n*o+d*f*i*l*m+d*f*m*n+i*l*m*o+d*f*o+d*f+o,e*i*j*m*n*o*p+e*i*m*n*p+e*i*j*o*p+e*n*o*p+i*j*o+i*m*o+e*i+i*n+p,a*f*i*k+a*f*k*l+a*i*l+f*k*n+f*l*r+a*n*r+k*n+a+q,c*d*l*n+d*n*o+h*n*o+d*o+r)
        str = "we computed 584692 S Polynomials and added 675 of them to the intermediate basis.
     -- used 69.407 seconds
     -- used 59.4056 seconds"
   ///},
   {"random18-22",
   ///
      I = ideal(a*d*e*f*j*n+a*b*d*n+a*d*j*n+e*f*j*n+d*e+a+d,a*b*c*e*j*l+a*b*c*e*j*m+b*e*j*l*m+b*j*l+e*j*l+c*l*m+b,a*c*f*g*j*p+c*d*j*p+c+p,c*d*h*r+b*h*i*r+b*h*l*r+b*c*l+h*i*r+d+h,d*h*i*k*l*m+d*h*i*k*l*r+d*i*k*l*m+h*i*k*m*r+d*i*l*m*r+h*i*k*m+d*h*i*r+d*h*l*r+d*h*k+e,b*g+f,c*h*j*n*p+c*g*k*n+c*k*n+j*k*n+h*j+g,b*c*f*h*k+b*c*f*k*m+c*h*k*m*q+b*c*k*q+b*f*h+c*f*k+m*q+h,b*c*d*k*o+a*c*d*k+a*c*f*o+b*c*k*o+b*c*k+c*f*k+d*f*o+a*f+i,f*h*i*n*r+f*g*i*r+b*f*n*r+b*g*n*r+j,a*d*p+a*n+d*p+g+k+l,b*c*e*l*q+l,a*c*h*k*m*q*r+a*h*m*q+a*h*k*r+c*k*q+a*q*r+a*r+m*r+m,b*e*f*g*p+b*d*e*j*p+d*e*f*j*p+e*f*g*j*p+b*d*e*f+b*d*e*j+d*e*f*j+b*d*p+g*j+n,k*n*q+o*p+n+o+1,i*j*l*m*o*q+h*i*l*q+j*m*q+l*m*q+i*o*q+p,b*e*h*i*k*q+e*h*i*k*o+e*h*k*o+e*i*k*o+b*e*o*q+h*i*q+b*k*q+b*e+q,c*d*g*h*i+c*d*i*l+d*g*i*l+c*d*i+d*j*l+d*g+c*j+g*j+i*j+r+1)
        str = "we computed 647329 S Polynomials and added 751 of them to the intermediate basis.
     -- used 93.7956 seconds
     -- used 0.407131 seconds"
   ///},
   {"random18-9",
   ///
      I = ideal(i*k*l*m*q+b*e*i*k+b*i*k*l+e*l*m*q+i*k*q+k*l*q+e*m+k*q+a,c*g*m*p*q+c*i*m+c*m+g*p+c*q+b,a*d*f*l*r+a*f*l+c,d*e*g*h*i*k+e*g*i*o+d*i*k+g*i*o+h*k*o+e*g+k*o+d,b*e*j*q*r+e*h*j*q+h*j*m*q+e*h*j*r+e*j*m*r+b*e*m+b*e*q+h*q+e,b*c*f*j*q+b*d*j*q+d*h*q+f,b*d*j*k*n*r+b*d*j*k*q+b*j*n*q+j*k*q*r+k*n*q+d*j*r+j*n+g,c*i+h,a*c*f*i+c*f*j*p+a*i*j*p+c*f*g+a*f*j+c*f+i*j+i+p,a*h*j*m*n+a*h*n*o+a*m*n*o+a*h*m+a*k*m+h*k*m+h*m*n+j*o,c*i*k*p*r+a*i*k*p+i*k*n*r+a*i*n+a*i*p+c*p+k+n+p,e*g*o+e*k+l,a*b*d*h*r+a*d*h*i*r+b*g*i+a*b+m,c*k*l*m*p*q+c*l*m*o*p*q+k*l*m*q+c*k*p*q+c*k*o+l*o*p+k*m*q+k*m+k*q+l*q+n,h*i*m+d*i*o+m*o*r+i*p*r+d*h+i*p+o,d*k*o*r+d*o+p,c*j*l*o*q+b*c*j*o+b*c*m*o+c*j*l*q+j*l*m*q+j*m*o*q+b*c*l+b*j*l+j*l*o+j*m*o+q,a*k*l*n*o+a*k*l*n*q+a*k*n*o+a*l*o*q+a*o*q*r+k*n+n+r)
        str = "we computed 1392832 S Polynomials and added 1378 of them to the intermediate basis.
     -- used 581.545 seconds
     -- used 0.586478 seconds"
   ///},
   {"random18-10",
   ///
      I = ideal(d*e*g*l*p+d*k*l*n+d*k*p+g*l*p+d*n+a,c*e*g*i*p+c*f*i*p+g*i+b,b*g*i*l*q+g*i*k*l*q+g*i*l*q*r+b*k*l*r+b*k*l+k*l*r+i*r+c,d*e*g*i*k+b*d*g*l+d*e*i*l+d*g*k*l+e*g*l+b*k+d+i,a*e*g*j*l*q+a*e*g*l*q+e*h*j*l*q+a*e*g*q+a*j+g*q+e,b*d*e*h*n+b*h*k*n+e*h*k+h*i*k+f+n,a*e*f*j*m*o+a*e*m*o+a*m*o+a*m+e+g,b*d*j*k*m*n*p+b*d*j*k*m*n+d*j*m*n+d*m*p+d*k+b*p+h,a*b*g*l*n+a*b*g*i+g*i*m*n+a*g*l+i,d*i*j*m*n*o+d*h*i*m*n+d*h*i*j*o+d*h*j*m*o+d*i*j*n*o+d*i*m*o+h*i*n+j,a*h*i*l*r+h*i*l*n*r+a*i*j*l+a*h*j*n+a*j*l*n+a*h+k,b*c*k*l*p*q+b*c*k*l+b*c*p*q+l,a*c*d*f*k*l*o+a*d*f*k*l+a*c*l+a*d*o+a*k+m,a*e*j*k*o*p*r+e*j*k*o*p+a*e*k*r+e*j*o*r+e*o*p*r+a*e*p+a*j*p+n+1,a*e*h*i*p+a*e*i*o+h*p*q+h*i+o+1,c*f*h*i*m+f*h*i*m*r+c*h*i*m+c*d*f*r+c*f*h*r+c*f*m+c*i*m+d*i*m+c*i+i+p,d*f*g*i*j*o*r+d*f*g*j*o*r+d*f*g*i*r+f*g*i*o*r+d*g*j*o*r+d*g*o*r+f*i*j+f*g*o+d*j*o+d*g+q,e*f*g*m*p*r+f*g*m*o*p*r+f*g*m*p+e*f*o+f*m*p+e*p+r)
        str = "we computed 2597863 S Polynomials and added 1709 of them to the intermediate basis.
     -- used 876.801 seconds
     -- used 1.31533 seconds"
   ///},
   {"random18-12",
   ///
      I = ideal(b*h*n*o*r+e*h*o+g*n+a,b*j*k*l*o*p+b*c*j*l+b*j*l*o+b*k*o*p+b*j*k+b*l*o+b*l*p+c*o*p+b,b*d*h*j*n*p*r+b*h*j*n+b*d*h+d*h*j+b*n+c,c*d*e*h*j*m*o+e*h*j*m*o+h*j*m+j*m*o+d*e+j*m+d,d*l*n*p+d*i*l+e,e*j*n*o*r+i*j*m+j*n*r+n*o*r+f,b*e*i*k*r+e*h*j+e*i*r+i*j*r+b*i+i*r+g,e*f*h*p+e*h*m*p+f*h*m+e*f*p+h+j,c*f*i*q*r+c*i*m*q*r+c*i*j*m+c*i*m*r+f*i*q*r+i*m*q*r+c*q*r+i+q,b*g*h*j*q*r+b*c*h*j*q+h*j,g*h*i*l+h*k*l*r+g*k*o*r+g*h+k*o+g,c*d*e*k*p*q*r+d*e*k*p*q*r+c*k*q+k*q+l+1,c*d*f*i*k+a*c*d*f+a*i+d+m,a*f*h*m+f*h*m*r+d*f*q+n+q,a*c*d*f*h*o*q+a*f*h*q+a*c*f+a*h*o+a*h*q+c*d+d*o+o,a*i*j*l*r+a*j*l*q*r+j*m*q+l*m*r+j*q+j*r+p,a*e*g*l*r+a*e*k*l*r+a*g*k*r+e*g*k*r+a*e*g+g*i*k+a*k+k*l+e*r+a+q,a*b*f*k*o+a*f*o+b+r)
        str = "we computed 1733240 S Polynomials and added 1199 of them to the intermediate basis.
     -- used 312.215 seconds
     -- used 22.1389 seconds"
   ///},
   {"random18-4",
   ///
      I = ideal(a*f*j*n*r+a+o,a*c*d*k*p+c*d*f*o+b,b*d*f*k*p*r+b*j*k*p*r+b*j*k*r+b*d*k+c+k,d*i*l*p*r+i*j*l*q+i*j*p*r+i*l*q*r+j*l*p+j*l*q+d*j+d,e*g*i*l*n+c*e*j*l*n+e*g*j*l*n+g*i*j*l+c*g*j+e*i*j+e*j*l+c*i*n+e,c*d*e*i+e*i*p*q+d*e*p+i*l*p+l*p*q+c*d+f,g*j*k*l*r+b*g*r+g,b*d*e*k*q+d*e*g*m+d*g*q+b*k+h,c*g*h*l*m+g*h*l*m+c*d*l*n+g*l*m+d*h+c+i,b*h*j*n+b*h*m+b*n+j*n+b+j,a*d*e*l*n*o+a*e*h*l*n*o+a*d*e*h*l+a*d*l*n+e*h*l*o+a*l*n*o+a*e*l+a*n*o+k,a*d*f*g*n*o+a*f*g*o+f*l*o+f*l+l,a*b*c*n*o+a*c*j*n*o+b*j*n*o*q+b*c*j+a*n*o+b*c*q+a*n*q+j*n*q+a*c+m+n,a*c*e*f*j*k*o+c*e*j*k+c*e*f+e*k*o+f*k*o+n,b*d*e*l*p+b*e*k*p+b*d*k+b*e*l+d*j*l+d*e*p+b*j*p+e*k*p+k+l+o,a*b*i*m*n+b*i*l*m+a*l*m*n+b*i*m+g*n+g+p,a*d*f*h*n*r+a*d*h*n*p*r+d*f*h*n*r+d*n*p*r+d*h*r+n*p+n+p+q+1,f*j*k*l*m*n+j*k*l*m*n+j*k*l*n*p+f*k*m*n+f*n+r)
      str = "we computed 3072683 S Polynomials and added 2256 of them to the intermediate basis.
     -- used 1235.32 seconds
     -- used 51.4413 seconds"
   ///},
   {"random18-6",
   ///
      I = ideal(g*h*l*n*r+h*i*n*o*r+g*i*n*o+h*i*n*r+h*i*l+h*n*r+i*n+a,n*o*q+e*n+b+r,a*e*h*k*p+c,g*j*l*p*r+d,a*b*h*i*j*p+a*b*h*i*m*p+b*h*j*p+b*i*j+b*j*p+i*m+h*p+j*p+e,d*e*f*g*i+d*e+f,a*e*f*g*r+a*f*h*j+g*h*j*r+a*f*h+g*j*r+g,b*e*h*l+f*m+h,b*f*g*i*m*q+f*i*m*n*q+b*f*g*m+f*i*n+b*g*q+f*m*q+i,a*b*h*l*q+a*b*g*q+g*h*l*q+h*l*n*q+a*h*n+b*h*n+g*l*n+b*g+l*q+j,a*b*c*d*l+a*c*d*j*l+a*b*c*d*o+a*c*d*l*o+b*l*o+a*d+c*j+j*l+k+1,c*d*f*k*p*q+d*f*k*n*q+c*f*k*p*q+c*k*n*q+f*n*p+c*d*q+d*p*q+l,h*m*p*r+c*e*q+c*e+c*h+h+m,d*h*i*j*m*q+d*g*h*i*m+d*g*h*m+g*h*j*m+d*g*i+h*j+d*m+j*m+n,a*e*k*n+e*p+n+o,b*d*h*p*q+b*e*l*p*q+e*h*l*p*q+b*e*h*p+d*e*l+p,e*h*i*k*n+c*e*h*i+c*e*h*k+c*e*i*k+e*h*i*k+c*e*h*p+i*k*n+e*i+i*p+q,a*b*h*l*m*r+b*g*h*l*r+b*g*l*m*r+b*h*m*r+g*h*m*r+a*h*m+g*h*r+b*g+a*h+b*h+r)
      str = "we computed 652999 S Polynomials and added 744 of them to the intermediate basis.
     -- used 564.545 seconds
     -- used 8.10589 seconds"
   ///},
   {"random18-23",
   ///
      I = ideal(b*d*n*o*p*r+c*n*o*p*r+b*d*n*o+c*d*r+a+d,c*m*n*p*r+b,e*g*h*l*p+c,f*l*n*o+l*m*n*p+f*k*o+k*l*o+m*n*p+k*n+m*n+d,e*i*l*m+b*c*l*r+c*i*m*r+c*e*l+c*i*l+e*i*m+m*r+e,a*d*l*p*r+a*l*r+k*l+l*p+f,g*i*j*o*q+e*g*i*q+a*g*j*q+e*i*j+e*g+e+g+1,e*g*h*j*p+b*d*e*g+e*g*j+h,d*h*i*m+c*i*k*m+c*d*k+c*d*q+d*m+i,a*b*d*j*q*r+a*b*d*j*q+a*d*q*r+a*f*q*r+a*b*d+b*d*f+d*f*j+d*j*r+j,a*f*h*j*k*q+a*f*h*k+a*h*j+a*g*k+k,g*h*j*k*p+g*j*k*p+h*k*p*q+a*h*p+g*k*p+a*p*q+g*j+a*p+j*q+l,a*d*e*i*j+d*e*i*j*m+a*d*e*m*n+a*m*n+a*i+j*n+m,a*g*h*j*k*p+g*h*k*p+a*g*k+j*k*p+n,a*d*h*i*k*r+d*e*h*i*k+d*h*k*r+a*i*k+a*i*r+h*k+o,a*c*d*f*r+a*b*c*p*r+b*c*d*p*r+a*f*p*r+d*f*p+b*f*r+f*p+b*r+p*r+p,b*f*i*o+q,c*d*i*n*o+c*i*m*n+d*i*n*r+c*m*o*r+d*i*r+m*r+r)
      str = "this never finished"
   ///}
 }

--R = ZZ/2[vars(0..17), MonomialOrder=>Lex]; -- everything in this file has 18 variables
R = ZZ/2[vars(0..17)]; -- everything in this file has 18 variables
L = ideal apply(gens R, x -> x^2+x);
R = R/L;

for i from 0 to #GBExamples-1 do (
     answer'gb = null;
     I = null;
     test'code = null;
     E := GBExamples#i;
     << "--example " << (i+1) << ": " << E#0 << endl;
     value E#1;
     --gbBoolean I;
     t1 := timing(J = rungb I);
     if answer'gb =!= null then assert(J == answer'gb);
     if test'code =!= null then value test'code;
     << "  time:                     " << t1#0 << " seconds" << endl;
     << "  At r 10992, " << str << endl;
     )

--
end


restart
path = prepend("~/src/M2-branch-mike/Macaulay2/bugs/mike/gbB-refactor-gb", path)
gbTrace=15
load "gb-random-18.m2"
R = ZZ/2[vars(0..17), MonomialOrder=>Lex]; -- everything in this file has 18 variables
L = ideal apply(gens R, x -> x^2+x);
load "../../../packages/ExampleIdeals.m2";
tmpfile := openOut ("~/tmp.me");
i = 0 
-- make Singular code -- 
for i from 0 to #GBExamples-1 do (
   E := GBExamples#i;
   tmpfile << "print (\"example " << (i+1) << ": " << E#0 << "\");" << endl;
   I = null;
   value E#1;
   tmpfile << toSingular R  ;
   tmpfile << toSingular (I,"I") ;
   tmpfile << toSingular (L,"L") ;
   tmpfile << "I = I+L;" << endl;
   tmpfile << "option(redSB);" << endl;
   tmpfile << "option(prot);" << endl;
   tmpfile << "groebner(I);" << endl;
   tmpfile << endl;
)
close tmpfile;



end      

-- MES notes:
-- r11022, random18-1, gens gb(I, Algorithm=>Test), Lex order, time: 296.9 seconds
--   (includes change in reduceit).
