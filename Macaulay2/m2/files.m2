--		Copyright 1993-1999 by Daniel R. Grayson

tmpname = (x) -> "/tmp/" | string processID() | x

String << Thing := File => (filename,x) -> openOut filename << x


