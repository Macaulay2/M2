# BEGIN { printf "s := currentPackage\n" }
# BEGIN { printf "currentPackage = Macaulay2Core\n" }
{ printf "load \"%s\"\n", $0 }
# END { printf "currentPackage = s\n" }
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
# End:

