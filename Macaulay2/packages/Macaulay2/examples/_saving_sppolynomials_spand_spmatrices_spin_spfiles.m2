R = QQ[x..z]
p = (x-y-1)^3
m = matrix {{x^2, x^2-y^2, x*y*z^7 }}
M = image m
f = temporaryFileName()
f << toString (p,m,M) << close
get f
(p',m',M') = value get f
p == p'
m == m'
M == M'
removeFile f
