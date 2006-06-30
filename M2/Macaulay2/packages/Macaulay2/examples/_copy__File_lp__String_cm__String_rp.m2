src = temporaryFileName()
dst = temporaryFileName()
src << "hi there" << close
copyFile(src,dst,Verbose=>true)
get dst
copyFile(src,dst,Verbose=>true,UpdateOnly => true)
src << "ho there" << close
copyFile(src,dst,Verbose=>true,UpdateOnly => true)
get dst
removeFile src
removeFile dst
