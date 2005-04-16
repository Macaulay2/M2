sq = i -> i^2
sq 10
sq(5+5)
tm = (i,j) -> i*j
tm(5,7)
(i -> i^2) 7
sincos = sin @@ cos
sincos 2.2
sin(cos(2.2))
compose = (f,g) -> x -> f(g(x))
sincos = compose(sin,cos)
cossin = compose(cos,sin)
sincos 2.2
cossin 2.2
