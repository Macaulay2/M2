-- examples of manual optimization through unsafe C code

max(x:int, y:int):int ::= Ccode(int, "(", x, "<", y, "?", y, ":", x, ")");

use stdio;

stdout << max(4,9) << endl;
