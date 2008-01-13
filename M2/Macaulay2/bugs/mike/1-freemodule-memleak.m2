-- Seems fixed: 12 Jan 2008.

restart
R = ZZ/32003[x,y,z]
time scan(1..100000, i -> R^1000);
time scan(1..10000, i -> R^i);
R^100000

time scan(1..10000, i -> R^10000);

time for i from 1 to 1000000 do R^{i}; -- no leak

R = ZZ/32003[x,y,z]
m = vars R
time for i from 1 to 1000000 do schreyerOrder m;
