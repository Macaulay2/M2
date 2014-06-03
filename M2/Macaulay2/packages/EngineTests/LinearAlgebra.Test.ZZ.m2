export {
    }

///
  restart
  R = ZZ
  N = 1000
  m = mutableMatrix(R,N,N)
  fillMatrix m;
  
  -- all entries < 10
  -- naive multiplication in mat-linalg:
  time (m*m); -- 100:   .02
              -- 200:   .16
              -- 300:   .66
              -- 400:  2.05
              -- 500:  5.22
              -- 600: 10.34
              -- 700: 16.30
              -- 800: 27.38
              -- 900: 34.83
              --1000: 48.86 sec

  restart
  R = ZZ
  N = 1000
  m = mutableMatrix(R,N,N)
  fillMatrix(m, Height=>2^70);
  time (m*m);

  -- all entries < 2^70
  -- naive multiplication in mat-linalg:
              -- 100:   .07
              -- 200:   .27
              -- 300:  1.34
              -- 400:  3.76
              -- 500:  7.85
              -- 600: 13.98
              -- 700: 26.97
              -- 800: 43.97
              -- 900: 60.88
              --1000: 87.77 sec
              
   time m2 = m*m;
   time m4 = m2*m2;
   time m8 = m4*m4;
   time m16 = m8*m8;
///