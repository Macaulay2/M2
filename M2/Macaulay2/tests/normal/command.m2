m := "value of m"
f = x -> ( y -> commandInterpreter (()->()) ) "value of y"
f "value of x"
t := "value of t"
assert ( keys \ localDictionaries(()->()) === {{"t"}, {"y"}, {"x"}, {"m"}} )
assert ( value \ flatten \\ values \ localDictionaries(()->()) === flatten {{"value of t"}, {"value of y"}, {"value of x"}, {"value of m"}} )
x = "value of x"
assert (y === "value of y")

