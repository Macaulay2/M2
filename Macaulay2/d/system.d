--		Copyright 1994 by Daniel R. Grayson

use C;
import sleep(t:int):int;
import getpid():int;
import exit(x:int):void;
import exceptionFlag:bool;				    -- indicates interrupt, stepping, or alarm
import interruptShield:bool;
import interruptedFlag:bool;
import interruptPending:bool;
import alarmedFlag:bool;
import write(fd:int, buffer:string, buffersize:int):int;
import read(fd:int, buffer:string, buffersize:int):int;
import read(fd:int, buffer:string, buffersize:int, offset:int):int;
import readline(buffer:string, buffersize:int, offset:int, prompt:string):int;
import link(oldfilename:string,newfilename:string):int;
import symlink(oldfilename:string,newfilename:string):int;
import unlink(filename:string):int;
import openin(filename:string):int;
import openout(filename:string):int;
import openoutappend(filename:string):int;
import opensocket(host:string,serv:string):int;
import openlistener(serv:string):int;
import acceptBlocking(sd:int):int;
import acceptNonblocking(sd:int):int;
import newline:string;
import envp:array(string);
import argv:array(string);
import args:array(string);
import syserrmsg():string;
import atend(f:function():void):void;
import run(command:string):int;
import pipe(fildes:array(int)):int;
import exec(argv:array(string)):int;
import getenv(s:string):string;
import stime():void;
import etime():double;
import strcmp(s:string,t:string):int;
import strnumcmp(s:string,t:string):int;
import gc:bool;
import randomint():int;
import wait(pid:int):int;
import waitNoHang(pid:array(int)):array(int);
import select(s:array(int)):array(int);
import hash(x:double):int;
import getcwd():string;
import dumpdata(filename:string):int;
import loaddata(filename:string):int;
import errfmt(filename:string,lineno:int,colno:int,loaddepth:int):string;
import loadDepth:int;
import dbmopen(filename:string,write:bool):int;
import dbmerror():string;
import dbmclose(handle:int):int;
import dbmstore(handle:int,key:string,content:string):int;
import chmod(filename:string,mode:int):int;
import dbmfetch(handle:int,key:string):(null or string);
import dbmdelete(handle:int,key:string):int;
import dbmfirst(handle:int):(null or string);
import dbmnext(handle:int):(null or string);
import dbmreorganize(handle:int):int;
import dbmstrerror():string;
import readfile(fd:int):(null or string);
import fileLength(fd:int):int;
import fileLength(filename:string):int;
import fileExists(name:string):bool;
import fileTime(name:string):int;
import setFileTime(name:string,modtime:int):int;
import currentTime():int;
import fileMode(name:string):int;
import fileModeFD(fd:int):int;
import mkdir(name:string):int;
import rmdir(name:string):int;
import isDirectory(name:string):int;
import isRegularFile(name:string):int;
export ArrayStringOrNull := null or array(string);
import wordexp(word:string):ArrayStringOrNull;
import readlink(filename:string):string;
import realpath(filename:string):string;
import noErrorMessage:string;
import regexmatchErrorMessage:string;
import regexmatch(pattern:string, offset:int, text:string):array(int);
import regexreplace(pattern:string, replacement: string, text:string, errflag:string):string;
import regexselect(pattern:string, replacement: string, text:string, errflag:array(string)):array(string);
import readDirectory(name:string):(null or array(string));
import strncmp(s:string,t:string,n:int):int;
import history():array(string);
import chdir(name:string):int;

everytimeCell := { f:function():void, next:everytimeCell };
dummyfun():void := nothing;
everytimeList := everytimeCell(dummyfun,self);
export everytime(f:function():void):void := everytimeList = everytimeCell(f,everytimeList);
export everytimeRun():void := (
     x := everytimeList;
     while x.next != x do (x.f(); x = x.next;));

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
