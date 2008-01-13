R=ZZ/32003[a..c];
I=intersect(ideal(a,b),ideal(b,c),ideal(c,a),ideal(a^2,b^3,c^4));
topComponents I
