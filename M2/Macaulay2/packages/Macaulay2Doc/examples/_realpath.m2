realpath "."
p = temporaryFileName()
q = temporaryFileName()
symlinkFile(p,q)
readlink q
realpath q
p << "hi there" << close
realpath q
removeFile q
