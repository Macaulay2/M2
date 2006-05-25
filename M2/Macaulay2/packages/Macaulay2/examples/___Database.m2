filename = temporaryFileName () | ".dbm"
x = openDatabaseOut filename
x#"first" = "hi there"
x#"first"
x#"second" = "ho there"
scanKeys(x,print)
x#"second" = null
scanKeys(x,print)
close x
removeFile filename
