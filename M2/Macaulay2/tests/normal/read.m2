-- check for bug involving "read" changing the strings it returns
setRandomSeed()
n = temporaryFileName() | "-randombytes"
n << concatenate ascii apply(10000, i -> random 256) << close
assert (fileLength n == 10000)
f = openIn n
x = read f;
assert( #x == 4096 )
i = 1; j = 100;
y = substring(x,i,j);
x' = read f;
assert( #x' == 4096 )
y' = substring(x',i,j);
assert( y == substring(x,i,j) ) -- could fail, because the bytes in x change
assert( x' != x )		   -- could fail, because the bytes in x change
close f
removeFile n
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test read.out"
-- End:
