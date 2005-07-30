R=ZZ/32003[a..d];
I=intersect(ideal(a*b+a^2,b^2),ideal(a^2,b^2,c^2),ideal(b^3,c^3,d^3))
removeLowestDimension I
