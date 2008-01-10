--		Copyright 1994 by Daniel R. Grayson

import abort():void;
import pow(x:double,y:double):double;
import lgamma(x:double):double;
import cos(x:double):double;
import sin(x:double):double;
import tan(x:double):double;
import sqrt(x:double):double;
import acos(x:double):double;
import asin(x:double):double;
import atan(x:double):double;
import atan2(y:double,x:double):double;
import cosh(x:double):double;
import sinh(x:double):double;
import tanh(x:double):double;
import exp(x:double):double;
import log(x:double):double;
import floor(x:double):double;
import ldexp(x:double,exp:int):double;
import alarm(x:int):int;
import fork():int;
rand():int ::= Ccode(int,"rand()");
export drand(x:double,y:double):double := (
     x + double(rand()%30000)/30000. * (y-x)
     );
export sqr(x:double):double := x*x;
import close(fd:int):int;
import fchmod(fd:int,mode:int):int;
import isatty(fd:int):int;
import dup2(fildes:int,fildes2:int):int;

-- grx stuff
import getkey():char;
import getxkey():char;
import MouseUnInit():void;
import mouse():void;
import GrSaveContext(x:null):int;
import GrCreateContext(w:int, h:int, x:null, y:null):int;
import GrSetContext(context:int):void;
import GrBitBlt(dest:int,x:int,y:int,
	src:int,x1:int,y1:int,x2:int,y2:int,operation:int):void; 
import Box(x:int,y:int,width:int,height:int,color:int):void;
import GrFilledCircle(xc:int,yc:int,r:int,color:int):void;
import GrNumColors():int;
import GrWhite():int;
import GrBlack():int;
import GrAdapterType():int;
import GrPlot(x:int,y:int,color:int):void;
import GrLine (x:int,y:int,xx:int,yy:int,color:int):void;
import GrSetMode(mode:int,width:int,height:int,colors:int):void;
import GrSizeX():int;
import GrSizeY():int;
import GrAllocColor(r:int,g:int,b:int):int;
import kbhit():bool;
import kill(pid:int,sig:int):int;
export string := array(char);
export Cstring := {Cstring:void} or null ;
import tostring(s:Cstring):string;
import gbTrace:int;
export notify:bool := false;

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
