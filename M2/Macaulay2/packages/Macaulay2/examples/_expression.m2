egyptian = method();
egyptian QQ := x -> if x == 0 then 0 else (
     n := ceiling(1/x);
     expression(1/n) + egyptian(x - 1/n));
egyptian(30/31)
