fn = temporaryFileName()
fn << "echo -n Hello $1!" << close
name = baseFilename fn
dir = replace(name | "$", "", fn)
programPaths#name = dir

program = loadProgram(name, name, RaiseError => false)
assert(program === null)

fileMode(7*64 + 5*8 + 5, fn)
program = loadProgram(name, name)
assert(program#"name" == name)
assert(program#"path" == dir)

pr = runProgram(program, "world", KeepFiles => true)
assert(pr#"command" == fn | " world")
assert(pr#"output" == "Hello world!")
assert(pr#"error" == "")
assert(pr#"return value" == 0)
assert(get pr#"output file" == "Hello world!")
assert(get pr#"error file" == "")
assert(net pr == "0")
