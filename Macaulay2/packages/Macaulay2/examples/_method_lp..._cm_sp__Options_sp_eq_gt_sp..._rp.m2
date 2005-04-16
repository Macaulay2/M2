protect Slope; protect Intercept;
f = method(Options => {Slope => 1, Intercept => 1})
f RR := o -> x -> o.Slope * x + o.Intercept
f(5.)
f(5.,Slope=>100)
options f
