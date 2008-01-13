f = openInOut "!cat"
isReady f
f << "hi there" << flush;
isReady f
