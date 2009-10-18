assert( factor 100! 
     ===
     new Product from {new Power from {2,97},new Power from {3,48},new Power from {5,24},new Power from {7,16},new Power from {11,9},new Power from
     {13,7},new Power from {17,5},new Power from {19,5},new Power from {23,4},new Power from {29,3},new Power from {31,3},new Power from {37,2},new Power
     from {41,2},new Power from {43,2},new Power from {47,2},new Power from {53,1},new Power from {59,1},new Power from {61,1},new Power from {67,1},new
     Power from {71,1},new Power from {73,1},new Power from {79,1},new Power from {83,1},new Power from {89,1},new Power from {97,1}}
     )

ZZ[x..z]
f = (x+3*y-14)^15*(x^2+y^4+z^7-x*y-13*x*z^2+12)^3;
factor f
collectGarbage ()					    -- GC_check_heap_block: found smashed heap objects:
