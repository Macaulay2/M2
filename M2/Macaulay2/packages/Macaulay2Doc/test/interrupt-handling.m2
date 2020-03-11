assert ( 9 == run ( commandLine#0 | " --silent --stop -q --int -e ' run ( \" kill -9 \" | toString processID() | \" &\" ) ' " ))
assert ( 3 * 256 === run ( commandLine#0 | " --silent --stop -q -e ' exit 3 ' " ))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test interrupt-handling.out"
-- End:
