fn = temporaryFileName()
f = fn << "hi there"
fileMode f
close f
removeFile fn
