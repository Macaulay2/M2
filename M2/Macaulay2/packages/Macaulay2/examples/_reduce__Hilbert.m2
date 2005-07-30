R = ZZ/101[x, Degrees => {2}];
I = ideal x^2;
s = hilbertSeries I
reduceHilbert s
