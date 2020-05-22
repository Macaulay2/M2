--this has, in the past, run quickly out of memory on 32 bit machines but not on 64 bit machines
d = 16
k = QQ
m = 3
R = k[take(a..z,-m)]
N = binomial(m+d-1,m-1)
r = floor(N/2)
r' = floor(r * 1.3)
f = random(R^1,R^{r:-d}) * random(R^{r:-d},R^{r':-d});
gbTrace = 3;
M = image f;
time G = gb(M, DegreeLimit => d);
