The documentation for the GAP scscp package is at:
http://www.cs.st-andrews.ac.uk/~alexk/scscp.htm

Try

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
