inputFile = temporaryFileName()
outputFile = temporaryFileName()

inputFile << ///1
code a
f = (x -> (
    x + 1
    ))
code f
/// << close

importFrom(Core, "srcdirs")
sourceArgs = concatenate apply(srcdirs, dir -> (" --srcdir ", format toAbsolutePath dir))
cmd = concatenate(
    format commandLine#0,
    sourceArgs,
    " --no-readline --silent --no-debug -q",
    " < ", format inputFile,
    " > ", format outputFile,
    " 2>&1")

assert(run cmd == 0)
output = get outputFile
assert not match("error:", output)
assert(match(///stdio:2:5-2:6: --source code:
     code a///
     ,output))
assert(match(///stdio:3:5-5:5: --source code:
     f = \(x -> \(
         x \+ 1
         \)\)///
     ,output))

removeFile inputFile
removeFile outputFile
