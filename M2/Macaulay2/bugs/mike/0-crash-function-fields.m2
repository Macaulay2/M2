R = frac ( GF(9)[x] )
x
1/x


end

i1 : frac ( GF(9)[x] )

o1 = frac((GF 9)[x])

o1 : FractionField

i2 : x

o2 = x

o2 : (GF 9)[x]

i3 : use o1

o3 = frac((GF 9)[x])

o3 : FractionField

i4 : x

o4 = x

o4 : frac((GF 9)[x])

i5 : 1/x
-- SIGSEGV
-- stack trace:
level 0 -- return addr: 0x00404ff5 -- frame: 0x0022b014
level 1 -- return addr: 0x61017c10 -- frame: 0x0022b034
level 2 -- return addr: 0x610980b8 -- frame: 0x0022b124
level 3 -- return addr: 0x61018d09 -- frame: 0x0022b244
level 4 -- return addr: 0x76e61039 -- frame: 0x0022b354
level 5 -- return addr: 0x76e6100b -- frame: 0x0022b378
level 6 -- return addr: 0x76e60e97 -- frame: 0x0022b420
level 7 -- return addr: 0x005034a5 -- frame: 0x0022b758
level 8 -- return addr: 0x00555039 -- frame: 0x0022b788
level 9 -- return addr: 0x0056eb7b -- frame: 0x0022b7a8
level 10 -- return addr: 0x00494508 -- frame: 0x0022b838
level 11 -- return addr: 0x0045fde5 -- frame: 0x0022b848
level 12 -- return addr: 0x0045fee5 -- frame: 0x0022b878
level 13 -- return addr: 0x00464ed6 -- frame: 0x0022b8a8
level 14 -- return addr: 0x0045a977 -- frame: 0x0022b9e8
level 15 -- return addr: 0x004652aa -- frame: 0x0022ba38
level 16 -- return addr: 0x00467dff -- frame: 0x0022bb78
level 17 -- return addr: 0x0046ba33 -- frame: 0x0022bbd8
level 18 -- return addr: 0x0045fd63 -- frame: 0x0022bc08
level 19 -- return addr: 0x0045fee5 -- frame: 0x0022bc38
level 20 -- return addr: 0x00464ed6 -- frame: 0x0022bc68
-- end stack trace
/bin/sh: line 1:   868 Aborted                 (core dumped) M2.exe --no-readline --print-width 142

Process M2 exited abnormally with code 134
