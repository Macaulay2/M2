--		Copyright 1994 by Daniel R. Grayson

tmpname = (x) -> "/tmp/" | string processID() | x

document { quote tmpname,
     TT "tmpname x", " -- create a temporary file name based on the string x
     unique to this process.",
     PARA,
     "The code for this function is Unix dependent at the moment.",
     PARA,
     "The routine doesn't actually check to see whether file exists."
     }

String << Thing := (filename,x) -> openOut filename << x


