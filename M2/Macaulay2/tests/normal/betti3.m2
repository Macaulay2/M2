assert ( toString infinity === "infinity" )
assert ( toString(-infinity) === "-infinity" )
assert ( (min {} .. max {}) === () )
assert ( (infinity .. infinity) === () )
assert ( (infinity .. 0) === () )
assert ( (0 .. -infinity) === () )

R = QQ[x]
betti res R^0

kk = QQ -- ??

-- Date: Wed, 3 Mar 2004 21:28:06 -0800
-- From: David Eisenbud <de@msri.org>
-- To: Dan Grayson <dan@math.uiuc.edu>, Mike Stillman <mike@math.cornell.edu>
-- Subject: the empty betti diagram
-- Reply-To: de@msri.org
-- Content-Type: text/plain; charset=us-ascii
-- Content-Disposition: inline
-- 
-- Dear Mike and Dan,
-- 
-- Just wanted you to know that your program is doing good things for me,
-- Green, Hulek, and Popescu in finding limiting examples for a paper we're
-- working on. Here's a piece of code we're using:
-- 
-- 
-- restart

S=kk[a..h]
gen4=transpose genericMatrix(S,a,4,2)
FF=res (M= ((S^1)/ minors(2,gen4)))
T=kk[u,v,x,y]
phi=map(T,S,{u,v,0,x,
             0,u,v,0})
TFF=phi FF
N=coker TFF.dd_1
GG=res N
betti GG
f=extend(GG,TFF,inducedMap(GG_0,TFF_0))
prune coker f

-- Of course whenever I use M2 I also find thing to improve.
-- This is a pretty fussy point, but the following gives an error --
-- should instead give the "zero" betti diagram 

betti prune coker f

-- FYI, when I do "version" I get another minor problem:
-- 
-- o15 = HashTable{architecture => Power Macintosh                    }
--                 compile node name => David-Eisenbuds-Computer.local
--                 compile time => Dec 16 2003 21:20:19
--                 compiler => gcc 3.3
--                 factory version => 1.3b
--                 gc version => 6.2 alpha 4
--                 gmp version => 4.1.2
--                 libfac version => 0.3.2
--                 operating system => Darwin
--                 operating system release => 7.0.0
--                 VERSION =>
-- 
-- but the source file is 0.9.2 . Fink Commander claims that I'm up-to-date.
-- 
-- 	Regards,
-- 		David
-- -- 
-- 
--  David Eisenbud, Director, MSRI              http://www.msri.org
--  President, American Mathematical Society    http://www.ams.org
-- 
--  Mathematical Sciences Research Institute    510-642-0143
--  17 Gauss Way                                510-642-8609 FAX
--  Berkeley CA 94720-5070                      

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test betti3.out"
-- End:
