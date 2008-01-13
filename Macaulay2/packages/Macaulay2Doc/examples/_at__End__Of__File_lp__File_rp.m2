f = openInOut "!cat"
f << "hi there" << closeOut;
atEndOfFile f
peek read f
atEndOfFile f
