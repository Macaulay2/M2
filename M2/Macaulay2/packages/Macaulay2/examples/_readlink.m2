p = temporaryFileName ()
symlinkFile ("foo", p)
readlink p
removeFile p
