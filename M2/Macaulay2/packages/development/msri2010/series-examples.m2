loadPackage "Series"

methods series 

s1 = series(1/(1-x),Degree=>10) --(series, RingElement) 
s2 = series(x,i->i^2) --(series, RingElement, Function) 
s3 = series(20,1+x) --(series, ZZ, RingElement) 

s1 + s2
