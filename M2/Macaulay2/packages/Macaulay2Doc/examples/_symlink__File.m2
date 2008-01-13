fn = temporaryFileName()
symlinkFile("qwert", fn)
fileExists fn
readlink fn
removeFile fn
