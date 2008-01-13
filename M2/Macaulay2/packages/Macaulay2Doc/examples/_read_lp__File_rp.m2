f = openInOut "!cat"
isReady f
f << "hi there" << flush;
isReady f
read f
isReady f
