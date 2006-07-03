f = matrix"2213712321;0"
g = matrix"33267327846238746;0"
g%f
g//f
assert((g//f) ** f + (g % f) == g)
