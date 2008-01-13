R = ZZ/101[x,y];
hilbertSeries(R/x^3)
hilbertSeries(R/x^3, Order =>5)
R = ZZ/101[x,y, Degrees=>{{1,2},{2,3}}];
hilbertSeries(R/x^3, Order =>5)
