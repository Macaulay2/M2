-- this is the command we'll run
-- it's the binary, and that's okay, because the environment is already set up
commandLine#0

run ( commandLine#0 | " --silent --stop -q -e ' exit 3 ' " )
assert ( 3 * 256 === oo )

run ( commandLine#0 | " --silent --stop -q -e ' exit 4 ' " )
assert ( 4 * 256 === oo )

-- this test seems to work better if it isn't a race to interrupt it before it exits (?), hence the loop
run ( commandLine#0 | " --silent --stop -q --int -e ' run ( \" kill -9 \" | toString processID() | \" &\" ) ' -e 'while true do 1' " )
assert ( 9 == oo )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/tests/normal interrupt-handling.out"
-- End:
