temp = z -> (
     R := ZZ/101[x,y,w];
     oldw = w;
     x/y;
     )

temp()

assert( w === oldw )

