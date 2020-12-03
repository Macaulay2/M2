-- sometimes this works, and sometimes this just needs a bit more memory...

-- this is a hard example from John Harrison
-- can we do it without running out of memory?

-- we'll fix this bug later: 
-- MES: I'm reinstating this test, 31 May 2017.  Does this still fail on some machines?  On my MBP, it takes about 440 MB of RAM

-- DRG: I'm disabling this test, as it runs out of memory with our new memory limit of 400M heap size on my Macbook
end -- deferred

-- *** buffer overflow detected ***: /home/dan/src/M2-1.3/BUILD/dan/builds.tmp/ubuntu32.production/StagingArea/i486-Linux-Ubuntu-9.04/bin/M2 terminated
-- ======= Backtrace: =========
-- /lib/tls/i686/cmov/libc.so.6(__fortify_fail+0x48)[0xb7045da8]
-- /lib/tls/i686/cmov/libc.so.6[0xb7043eb0]
-- /lib/tls/i686/cmov/libc.so.6[0xb70435a8]
-- /lib/tls/i686/cmov/libc.so.6(_IO_default_xsputn+0xc8)[0xb6fb5bb8]
-- /lib/tls/i686/cmov/libc.so.6(_IO_vfprintf+0x133)[0xb6f87963]
-- /lib/tls/i686/cmov/libc.so.6(__vsprintf_chk+0xa4)[0xb7043654]
-- /lib/tls/i686/cmov/libc.so.6(__sprintf_chk+0x2d)[0xb704359d]
-- /home/dan/src/M2-1.3/BUILD/dan/builds.tmp/ubuntu32.production/StagingArea/i486-Linux-Ubuntu-9.04/bin/M2[0x8064191]
-- /home/dan/src/M2-1.3/BUILD/dan/builds.tmp/ubuntu32.production/StagingArea/i486-Linux-Ubuntu-9.04/bin/M2[0x8064408]
-- /home/dan/src/M2-1.3/BUILD/dan/builds.tmp/ubuntu32.production/StagingArea/i486-Linux-Ubuntu-9.04/bin/M2[0x81980ad]
-- /home/dan/src/M2-1.3/BUILD/dan/builds.tmp/ubuntu32.production/StagingArea/i486-Linux-Ubuntu-9.04/bin/M2[0x81c4460]


  A = ZZ[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13, MonomialSize=>8]
  f0 = ((x3 * x0) - (1))
  f1 = ((x1 * x3) - (((3) * (x5 ^ 2)) + (((2) * (x9 * x5)) + (x10 - (x6 * x4)))))
  f2 = ((x2 * x3) - (-((x5 ^ 3)) + ((x10 * x5) + (((2) * x12) - (x7 * x4)))))
  f3 = (x11 - ((x1 ^ 2) + (((x6 * x1) - x9) - ((2) * x5))))
  f4 = (x8 - (((-((x1 + x6)) * x11) - x2) - x7))
  f5 = (x3 - (((2) * x4) + ((x6 * x5) + x7)))
  f6 = (((x4 ^ 2) + ((x6 * (x5 * x4)) + (x7 * x4))) - ((x5 ^ 3) + ((x9 * (x5^ 2)) + ((x10 * x5) + x12))))
  f7 = (((((x8 ^ 2) + ((x6 * (x11 * x8)) + (x7 * x8))) - ((x11 ^ 3) + ((x9 * (x11 ^ 2)) + ((x10 * x11) + x12)))) * x13) - (1))
  I = ideal(f0,f1,f2,f3,f4,f5,f6,f7)

time gens gb I;
assert(1 % I == 0)
gbTrace = 3
time(h = 1 // (gens I))
assert((gens I) * h == 1)
end--
