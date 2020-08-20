end--
restart
needs "test-mem-leaks.m2"
getRSS()

restart
needs "test-mem-leaks.m2"
setRandomSeed "ab"
R = ZZ/101[a..d];
I = ideal random(R^1, R^{5:-2});
f = () -> (gb (ideal I_*);)
testF(100000,f)
  -- M2 eigen branch 5 June 2019
  --   elapsed time = 26.5119
  --   leaks 6.59456 bytes, takes .265119 ms. (per call)
  -- M2 master branch 5 June 2019 (no leak?)
  --   elapsed time = 25.4852
  --   leaks 4.38272 bytes, takes .254852 ms. (per call)

restart
needs "test-mem-leaks.m2"
setRandomSeed "ab"
R = ZZ[a..d];
I = ideal random(R^1, R^{5:-2});
f = () -> (gb (ideal I_*);)
testF(3*10000,f)
  -- M2 eigen branch 5 June 2019 (SLOW, LEAKS)
  --   elapsed time = 54.7941
  --   leaks 801.997 bytes, takes 1.82647 ms. (per call)
  -- M2 master branch 5 June 2019 (SLOWER THAN M2 1.12?)
  --   elapsed time = 35.999
  --   leaks 23.3472 bytes, takes 1.19997 ms. (per call)

restart
needs "test-mem-leaks.m2"
setRandomSeed "ab"
R = QQ[a..d];
I = ideal random(R^1, R^{5:-2});
f = () -> (gb (ideal I_*);)
testF(5*10000,f) -- .85 ms per call, 8.5 sec total
  --testF(100000,f) -- .84 ms per call, no leak, 84.9 sec total
  -- M2 eigen branch 5 June 2019 (MUCH SLOWER, no leaks?)
  --   elapsed time = 39.225
  --   leaks 13.5987 bytes, takes .7845 ms. (per call)
  -- M2 master branch 5 June 2019
  --   elapsed time = 27.6002
  --   leaks 10.0762 bytes, takes .552004 ms. (per call)

restart
needs "test-mem-leaks.m2"
setRandomSeed "ab"
R = QQ[a..f]
I = ideal random(R^1, R^{-2,-3,-4,-5});
elapsedTime gens gb I; -- somewhat slower than 1.12?
  -- M2 eigen branch 5 June 2019
  --   7.28963 seconds elapsed
  -- M2 master branch 5 June 2019
  --   5.7731 seconds elapsed  

restart
needs "test-mem-leaks.m2"
setRandomSeed "ab"
R = QQ[a..f]
I = ideal random(R^1, R^{-3,-3,-4,-5});
elapsedTime gens gb I;
  -- M2 eigen branch 5 June 2019
  --   31.8464 seconds elapsed
  -- M2 master branch 5 June 2019
  --   30.0926 seconds elapsed
  
restart
needs "test-mem-leaks.m2"
setRandomSeed "ab"
R = ZZ[a..f]
I = ideal random(R^1, R^{-2,-3,-3,-3});
elapsedTime gens gb I; -- eigen branch:  sec, 1.12:  sec
  -- M2 eigen branch 5 June 2019
  -- M2 master branch 5 June 2019

restart
needs "test-mem-leaks.m2"
setRandomSeed "ab"
R = QQ[a..f]
I = ideal random(R^1, R^{-3,-3,-4,-5});
elapsedTime gens gb I; -- eigen branch:  32.0 sec, 1.12: 29.6 sec
  -- M2 eigen branch 5 June 2019
  --   32.0907 seconds elapsed
  -- M2 master branch 5 June 2019
  --   29.7198 seconds elapsed
  