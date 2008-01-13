R = ZZ/31[x]
f = (x^110+1)*(x^13+1)
time factor f
g = () -> factor f
g = profile g
h = profile("h", () -> factor f)
for i to 10 do (g();h();h())
profileSummary
