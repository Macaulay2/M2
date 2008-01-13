-- Fixed 13 Jan 2008

-- Found while talking to Tara Holm.

R = ZZ[a,c]
I = ideal"ac-a-c+1, a2+3c2-2a-6c+4, c3-3c2+3c-1"
gbTrace=10
mingens I
gens gb I == gens gb mingens I
