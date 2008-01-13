f = i -> i+1
g = i -> i^2
apply(0 .. 10, f @@ g)
apply(0 .. 10, g @@ f)
