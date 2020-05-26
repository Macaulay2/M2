collectPrimes = method()
collectPrimes(ZZ, ZZ) := (lo, len) -> (
    for i from 0 to len list (
        a := isPrime (lo+i);
        if a then i else continue
        )
    )
end--

restart
load "isprime-tests.m2"
collectPrimes(10, 100000)
collectPrimes(100000, 100000);
collectPrimes(200000, 100000);
collectPrimes(300000, 100000);
collectPrimes(10^10, 100000);
collectPrimes(10^20, 100000);
collectPrimes(2^30, 100000);
collectPrimes(2^32, 100000);
collectPrimes(2^62, 100000);
collectPrimes(2^60, 100000);
collectPrimes(2^50, 2^10);
collectPrimes(2^48, 2^10);
collectPrimes(2^52, 2^10);
collectPrimes(2^52 - 2^10, 2^10);
collectPrimes(2^52 - 2^5, 2^10);

factor 100
factor(-100)
value factor 100 == 100
value factor(2^15+1) == 2^15+1

factor(2^40+1)
isPrime 4278255361
factor(2^50+1)
factor(2^60+1)
factor(2^70+1)
factor(2^80+1)
factor(2^100+1)
factor(2^100+12312)

value factor(2^70+1) == 2^70+1
value factor(2^70+1111) == 2^70+1111
for i from 2^3 to 2^10 list (
    << "factoring: " << 2^70 + i << endl;
    elapsedTime f := factor(2^70 + i);
    << f << endl;
    assert(value f == 2^70 + i)
    )
