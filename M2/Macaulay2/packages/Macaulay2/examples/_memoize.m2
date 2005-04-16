fib = n -> if n <= 1 then 1 else fib(n-1) + fib(n-2)
time fib 16
fib = memoize fib
time fib 16
time fib 16
