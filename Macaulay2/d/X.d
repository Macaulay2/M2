--		Copyright 1994 by Daniel R. Grayson

use system;

export XID := uint;
import XCreateWindow(
     parent:XID,
     x:int, y:int,
     width:int, height:int,
     borderwidth:int,
     name:string):XID;
import XDefaultRootWindow():uint;
