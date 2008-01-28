d = 16
k = QQ
m = 3
R = k[take(a..z,-m)]
N = binomial(m+d-1,m-1)
r = floor(N/2)
r' = floor(r * 1.3)
f = random(R^1,R^{r:-d}) * random(R^{r:-d},R^{r':-d});
-- transpose f;
gbTrace = 3;
M = image f;
time G = gb(M, DegreeLimit => d);
