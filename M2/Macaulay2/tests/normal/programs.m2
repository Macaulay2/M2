fn = temporaryFileName()
fn << "echo -n Hello $1!" << close
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
