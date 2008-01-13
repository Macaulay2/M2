src = temporaryFileName()
dst = temporaryFileName()
src << "hi there" << close
moveFile(src,dst,Verbose=>true)
get dst
bak = moveFile(dst,Verbose=>true)
removeFile bak
