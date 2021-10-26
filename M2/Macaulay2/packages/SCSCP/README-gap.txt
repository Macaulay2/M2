The documentation for the GAP scscp package is at:
http://www.cs.st-andrews.ac.uk/~alexk/scscp.htm

1. To use the OpenMath package it is probably necessary to build the IO 
package. To do this,

# cd <where your gap is>/4.0/pkg/io/
# ./configure <where your gap is>/4.0/
# make

I also have the magic lines
# cd <where your gap is>/4.0/bin/i686-apple-darwin`uname -r`-gcc
# ./gac -o gap -P "-lm  -lpanel -lncurses" -p "-DEDIVSTATIC -DNCURSESSTATIC -DIOSTATIC" ../../pkg/edim/src/ediv.c ../../pkg/Browse/src/ncurses.c

(last command is one line), but I'm not sure whether you need those.

2. To solve a problem with remote references, edit 
pkg/scscp/lib/openmath.g, and apply the following patch (with me around 
line 487) :

-    if [address,port]=[SCSCPserverAddress,SCSCPserverPort] then
+    if [address,port]=[SCSCPserverAddress,SCSCPserverPort] or [address,port] = ["0.0.0.0", SCSCPserverPort] then

3. Try

    gap> LoadPackage("scscp");
    gap> SCSCPserverAcceptsOnlyTransientCD := false;;
    gap> InstallSCSCPprocedure( "MatrixGroup", Group );
    gap> RunSCSCPserver(true, 26135);

in order to reproduce the setting for creating the M2 SCSCP package
documentation.

The 1st line loads the package;

The 2nd line drops the "security" that GAP only accepts
scscp_transient_1.... symbols (so that GAP also accepts e.g.
arith1.plus(1,2))

The 3rd line maps the scscp_transient_1.MatrixGroup OpenMath symbol to the
internal Group procedure (this matches line 9 of
Macaulay2/packages/OpenMath/cds/scscp_transient_1.m2)

The 4th line starts the server, accepting connections on every interface (by
the "true" parameter) on port 26135. (You could try "localhost" instead of
true, but I know that on the Mac that causes problems with ipv4/ipv6. I forget
the particulars).
