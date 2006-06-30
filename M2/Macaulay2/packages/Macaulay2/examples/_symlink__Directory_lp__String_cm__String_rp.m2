src = temporaryFileName() | "/"
dst = temporaryFileName() | "/"
makeDirectory (src|"a/")
makeDirectory (src|"b/")
makeDirectory (src|"b/c/")
src|"a/f" << "hi there" << close
src|"a/g" << "hi there" << close
src|"b/c/g" << "ho there" << close
symlinkDirectory(src,dst,Verbose=>true)
get (dst|"b/c/g")
symlinkDirectory(src,dst,Verbose=>true,Undo=>true)
rm = d -> if isDirectory d then removeDirectory d else removeFile d
scan(reverse findFiles src, rm)
scan(reverse findFiles dst, rm)
