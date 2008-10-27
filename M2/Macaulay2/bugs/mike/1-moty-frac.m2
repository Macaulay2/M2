-- 10/26/08: now get an error message on y/(u^2+v^2)
pp=5
K=frac(ZZ/pp[u,v]);
R=K[x,y,z_0..z_(2*pp-1)];
y/(u^2+v^2)

end

-- From Moty Katzman, 4 Dec 2007:
Dear Dan and Mike,
 
Here is something weird:
 
$ M2
Macaulay 2, version 0.9.95
with packages: Classic, Core, Elimination, IntegralClosure, LLLBases, Parsing, PrimaryDecomposition, SchurRings, TangentCone
 
i1 :
     pp=5;
 
i2 : K=frac(ZZ/pp[u,v]);
 
i3 :
     R=K[x,y,z_0..z_(2*pp-1)];
 
i4 :
     y/(u^2+v^2)
=============================================================================
SIGSEGV -- back trace
level 0 -- return addr: 0x61017b80 -- frame: 0x0022aefc
level 1 -- return addr: 0x61096adc -- frame: 0x0022afec
level 2 -- return addr: 0x61018c6c -- frame: 0x0022b10c
level 3 -- return addr: 0x7c9037bf -- frame: 0x0022b21c
level 4 -- return addr: 0x7c90378b -- frame: 0x0022b240
level 5 -- return addr: 0x7c90eafa -- frame: 0x0022b2f0
level 6 -- return addr: 0x004ec897 -- frame: 0x0022b628
level 7 -- return addr: 0x00528865 -- frame: 0x0022b648
level 8 -- return addr: 0x0041e701 -- frame: 0x0022b668
level 9 -- return addr: 0x0045ea62 -- frame: 0x0022b698
-----------------------------------------------------------------------------
Aborted (core dumped)
 