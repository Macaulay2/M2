-- illustrative example for the JSAG paper
-- lucky seed appears to be 35580
loadPackage("NumericalSchubertCalculus");
n = 8
m = matrix{{5, 3, 5, 7, 8}, {1, 4, 6, 7, 8}}
print LRrule(n, m)
(f, p, s) = LRtriple(n, m, luckySeed=>35370);
print f
print p
print s
exit()
