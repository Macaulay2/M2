fn = temporaryFileName()
f = fn << "hi there"
m = 7 + 7*8 + 7*64
fileMode(m,f)
fileMode f
close f
fileMode fn
removeFile fn
