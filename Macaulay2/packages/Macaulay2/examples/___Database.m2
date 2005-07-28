filename = temporaryFileName () | ".dbm"
x = openDatabaseOut filename
x#"first" = "hi there"
x#"first"
x#"second" = "ho there"
scanKeys(x,print)
close x
unlinkFile filename
