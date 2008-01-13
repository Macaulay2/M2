opts = {Slope => 1, Intercept => 1}
f = opts >> o -> x -> x * o.Slope + o.Intercept
f 5
f(5, Slope => 100)
f(5, Slope => 100, Intercept => 1000)
f = {a => 1000} >> o -> (x,y) -> x * o.a + y;
f(3,7)
f(5,11,a=>10^20)
