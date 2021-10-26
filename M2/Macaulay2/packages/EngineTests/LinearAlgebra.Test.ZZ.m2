export {
    }

///
  R = ZZ
  N = 1000
  m = mutableMatrix(R,N,N)
  fillMatrix m;
  
  -- all entries < 10
  -- naive multiplication in mat-linalg:
  -- with flint mult routines, times are much better, e.g.: N=1000, this is .78 sec
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

///
  restart
  R = QQ
  N = 100
  m = mutableMatrix(R,N,N)
  fillMatrix m;
  time (m*m);
  
  -- all entries < 10/10
  -- naive multiplication in mat-linalg:
  time (m*m); -- 100:   .05 (old .44)
              -- 200:   .06 ( old 3.57)
              -- 300:   .10
              -- 400:   .22
              -- 500:   .39
              -- 600:   .60
              -- 700:   .75
              -- 800:  1.11
              -- 900:  1.43
              --1000:  1.77 sec

  R = QQ
  N = 100
  m = mutableMatrix(R,N,N)
  fillMatrix(m, Height=>2^70);
  time (m*m);

  -- all entries < 2^70
  -- flint mult in mat-linalg:
              -- 100: 13.97
              -- 200: 
              -- 300: 
              -- 400: 
              -- 500: 
              -- 600: 
              -- 700: 
              -- 800: 
              -- 900: 
              --1000: sec
              
   time m2 = m*m;
   time m4 = m2*m2;
   time m8 = m4*m4;
   time m16 = m8*m8;
///
