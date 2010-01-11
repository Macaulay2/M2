restart
loadPackage "Series"

methods series 

ZZ[x]
s1 = series(1/(1-x),Degree=>4) --(series, RingElement) 
s2 = series(x,i->i^2) --(series, RingElement, Function) 
s3 = series(20,1+x+x^2+x^3 + x^4 + x^5 + x^6 + x^10) --(series, ZZ, RingElement) 

s4 = setDegree(10,s1)
peek s4
s5 = setDegree(2,s1)
peek s5


s6 = setDegree(10,s2)
peek s6
s7 = setDegree(2,s2)
peek s7


s6 = setDegree(10,s3)
peek s6
s7 = setDegree(2,s3)
peek s7


s1 + s2

S = series(4,x^2 + x)  
-S
T = series(x,i -> i)
S
S.computedDegree
peek S

T - T + T
setDegree(9,T - T + T)
setDegree(12,T*T)

