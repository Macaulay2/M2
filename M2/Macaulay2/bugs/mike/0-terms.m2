-- fixed, 1/5/09, test in test/terms.m2

restart
K = toField(QQ[a]/(a^2+1))
R = K[x, y]/(x^30,x^28*y,x^26*y^2,x^24*y^3,x^22*y^4,x^20*y^5,x^18*y^6,x^16*y^7,x^14*y^8,x^
       12*y^9,x^10*y^10,x^8*y^11,x^6*y^12,x^4*y^13,x^2*y^14,y^15)
G = (21875/128)*a*x^28
terms G

restart
K = toField(QQ[a]/(a^2+1))
R = K[x, y]
G = (21875/128)*a*x^28
terms G
debug Core
raw G

-- Use this one to debug:
restart
debug Core
kk = QQ[a]/(a^2+1)
K = toField kk
R = K[x, y]
G = a*x^28+x^28 + a*x^2*y^5
terms G
terms(coefficientRing ring G, G)
terms(coefficientRing coefficientRing ring G, G)
first oo;
raw oo
raw G

restart
K = toField(QQ[a]/(a^2+1))
R = K[x, y]
G = a*x
terms G
debug Core
raw G


-(55625/256)*x^26+(17125/1024)*a*x^24-(1271975/8192)*x^22-(7914393/32768)*a*x^20+
      (272795/8192)*x^18-(194855/4096)*a*x^16+(3145/32)*x^14+(4927/512)*a*x^12+(713/128)*x^10+(1105/64)*a*x^8
terms G
