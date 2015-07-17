-- It would be nice if this computation could be made faster.  25 May 2013.

R = ZZ[y,x,MonomialOrder=>Lex]
I = ideal(1886592*y^5-3537360*y^4-3329280*y^3*x+2122416*y^3-8489664*y^2*x^4+3745440*y^2*x-397953*y^2+4244832*y*x^4+1468800*y*x^2-936360*y*x+2496960*x^5-550800*x^2,-832320*y^4-11319552*y^3*x^3+1248480*y^3+8489664*y^2*x^3+1468800*y^2*x-468180*y^2+12484800*y*x^4-1101600*y*x-93386304*x^10-648000*x^2,314432*y^6-707472*y^5-832320*y^4*x+530604*y^4-2829888*y^3*x^4+1248480*y^3*x-132651*y^3+2122416*y^2*x^4+734400*y^2*x^2-468180*y^2*x+2496960*y*x^5-550800*y*x^2-8489664*x^11-216000*x^3)
see I
gcdCoefficients(leadCoefficient(I_0), leadCoefficient I_1)
(leadCoefficient I_0)//(-leadCoefficient I_1)
I_0 + 2 * y * I_1
lc = leadCoefficient
F = I_0
G = I_1
H = I_2
F = F + 2*y*G
gcdCoefficients(lc F, lc G)
F1 = 4*F + y*G
F2 = 15*F + 4*y*G
gcd(lc F, lc H)
gcd(lc F1, lc H)
gcd(lc F2, lc H)
contract(y^4, F2)
gcd(707472, 832320)
