Qu = new Type of List
w = new Qu from {1,2,3,4}
w+w
Qu * Qu := (x,y) -> new Qu from { 
          x#0*y#0 - x#1*y#1 - x#2*y#2 - x#3*y#3,
          x#0*y#1 + x#1*y#0 + x#2*y#3 - x#3*y#2,
          x#0*y#2 + x#2*y#0 + x#3*y#1 - x#1*y#3,
          x#0*y#3 + x#3*y#0 + x#1*y#2 - x#2*y#1
          };
w*w
