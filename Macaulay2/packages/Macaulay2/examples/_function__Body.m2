f = a -> b -> a+b+a*b
functionBody f 1
f 1 === f 2
functionBody f 1 === functionBody f 2
