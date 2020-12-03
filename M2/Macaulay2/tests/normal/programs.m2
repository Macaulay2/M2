fn = temporaryFileName()
fn << ///printf "Hello %s!" "$1"/// << close
name = baseFilename fn
dir = replace(name | "$", "", fn)
programPaths#name = dir

program = findProgram(name, name, RaiseError => false)
assert(program === null)

fileMode(7*64 + 5*8 + 5, fn)
program = findProgram(name, name)
assert(program#"name" == name)
assert(program#"path" == dir)
assert(program#"prefix" == (".*", ""))
assert(net program == name)

pr = runProgram(program, "world", KeepFiles => true)
assert(pr#"command" == fn | " world")
assert(pr#"output" == "Hello world!")
assert(pr#"error" == "")
assert(pr#"return value" == 0)
assert(get pr#"output file" == "Hello world!")
assert(get pr#"error file" == "")
assert(net pr == "0")

copyFile(fn, dir | "/foo-bar")
prefix = ("bar", "foo-")
program = findProgram(name, {name, "bar"}, Prefix => {prefix})
assert(program#"prefix" == prefix)

fn << "touch baz" << close
program = findProgram(name, name)
runProgram(program, name, RunDirectory => dir | "/foo/bar")
assert(fileExists(dir | "/foo/bar/baz"))

program = findProgram("foo", name, AdditionalPaths => {dir})
assert(program#"path" == dir)


-- note: Sometimes the built-in sh command "echo" does not know about "-n".
	-- testing: /Users/dan/src/M2/M2.git/M2/Macaulay2/tests/normal/programs.m2
	-- ../../../../../../../Macaulay2/tests/normal/programs.m2:40:11:(3):[4]: error: could not find 0
	-- programs.errors:0: error output left here for the errors above:
	-- gmake[4]: *** [../Makefile.test:67: programs.out] Error 1
--       Another work-around might be to use /bin/echo.
fn << "echo 1.0 | tr -d '\n'" << endl << close


program = findProgram(name, name, MinimumVersion => ("0.9", name))
assert(program#"version" == "1.0")
program = findProgram(name, name, MinimumVersion => ("1.1", name),
    RaiseError => false)
assert(program === null)
