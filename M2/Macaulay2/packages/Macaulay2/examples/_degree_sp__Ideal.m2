S = QQ[a..f];
I = ideal(a^195, b^195, c^195, d^195, e^195);
degree I
degree(S^1/I)
I = intersect(ideal(a-1,b-1,c-1),ideal(a-2,b-1,c+1),ideal(a-4,b+7,c-3/4));
degree I
