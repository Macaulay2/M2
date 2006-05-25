(hold 2)^5 * (hold 3)^3 * (hold 5) * (hold 11)^2
egyptian = method();
egyptian QQ := x -> if x == 0 then 0 else (
n := ceiling(1/x);
hold(1/n) + egyptian(x - 1/n));
egyptian(30/31)
