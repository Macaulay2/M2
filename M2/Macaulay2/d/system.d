--		Copyright 1994 by Daniel R. Grayson

use M2;
header "
#ifdef HAVE_SYS_TYPES_H
 #include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
 #include <sys/stat.h>
#endif
#ifdef HAVE_TIME_H
 #include <time.h>
#endif
#ifdef HAVE_ASSERT_H
 #include <assert.h>
#endif
#ifdef HAVE_PTHREAD_H
 #include <pthread.h>
#endif
#ifdef HAVE_SYS_WAIT_H
 #include <sys/wait.h>
#endif
";

declarations "
#ifndef _GNU_SOURCE
 #define _GNU_SOURCE
#endif
#ifdef HAVE_UNISTD_H
 #include <unistd.h>
#endif
#ifdef HAVE_MATH_H
 #include <math.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
 #include <sys/resource.h>
#endif
";

export isatty(fd:int) ::= Ccode(int, "isatty(", fd, ")" );
export fork() ::= Ccode(int, "
     #ifdef HAVE_FORK
      fork()
     #else
      -1
     #endif
     " );
export close(fd:int) ::= Ccode(int, "close(", fd, ")" );
export dup2(fd:int,fd2:int) ::= Ccode(int, "dup2(", fd, ",", fd2, ")" );
export pow(x:double,y:double) ::= Ccode(double, "pow(", x, ",", y, ")" );
export abort() ::= Ccode(exits,"abort()");
export sleep(t:int):int := Ccode(int,"sleep(t)");
export nanosleep(t:long):int := Ccode(returns,"struct timespec ts_sleep; ts_sleep.tv_sec = ",t,"/1000000000; ts_sleep.tv_nsec=",t,"%1000000000; return nanosleep(&ts_sleep, NULL);");
export getpid():int := Ccode(int, "getpid()");
export getpgrp():int := Ccode(int, "
     #ifdef HAVE_GETPGRP
      getpgrp()
     #else
      -1
     #endif
     ");
export setpgid(pid:int,pgid:int):int := Ccode(int, "
     #ifdef HAVE_SETPGID
      setpgid(", pid, ",", pgid,")
     #else
      -1
     #endif
     "); 
export exit(x:int):exits := Ccode( exits, "
     extern void clean_up();
     clean_up();
     exit(",x,");
     ");
export write(fd:int, buffer:string, buffersize:int):int := Ccode(returns, "
     if ((int)buffer->len < buffersize) fatalarrayindex(buffersize-1,buffer->len,__FILE__,__LINE__,-1);
     return write(fd,buffer->array,buffersize);
     ");
export write(fd:int, buffer:string):int := write(fd,buffer,length(buffer));
export write(fd:int, buffer:string, buffersize:int, offset:int):int := Ccode(returns, "
     if (offset < 0) fatalarrayindex(offset,buffer->len,__FILE__,__LINE__,-1);
     if ((int)buffer->len < buffersize+offset) fatalarrayindex(buffersize+offset-1,buffer->len,__FILE__,__LINE__,-1);
     return write(fd,buffer->array+offset,buffersize);
     ");
export read(fd:int, buffer:string, buffersize:int):int := Ccode(returns, "
     if ((int)buffer->len < buffersize) fatalarrayindex(buffersize,buffer->len,__FILE__,__LINE__,-1);
     if (buffersize == 0) return 0;
     return read(fd,buffer->array,buffersize);
     ");
export read(fd:int, buffer:string, buffersize:int, offset:int):int := Ccode(returns, "
     if (offset < 0) fatalarrayindex(offset,buffer->len,__FILE__,__LINE__,-1);
     if ((int)buffer->len < buffersize+offset) fatalarrayindex(buffersize+offset,buffer->len,__FILE__,__LINE__,-1);
     if (buffersize == 0) return 0;
     return read(fd,buffer->array+offset,buffersize);
     ");
import readline(buffer:string, buffersize:int, offset:int, prompt:string):int;
import link(oldfilename:string,newfilename:string):int;
export fchmod(fd:int,mode:int):int := Ccode(int, "
     #ifdef HAVE_FCHMOD
      fchmod(fd,mode)
     #else
      -1
     #endif
     ");
import symlink(oldfilename:string,newfilename:string):int;
import unlink(filename:string):int;
import openin(filename:string):int;
import openout(filename:string):int;
import openoutappend(filename:string):int;
import opensocket(host:string,serv:string):int;
import openlistener(interface:string,serv:string):int;
import acceptBlocking(sd:int):int;
import acceptNonblocking(sd:int):int;
import syserrmsg():string;				    -- uses errno
import strerror():constcharstar;			    -- uses errno 
export strerror(errnum:int):string := tostring(Ccode(constcharstar,"strerror(",errnum,")"));
import atend(f:function():void):void;
import run(command:string):int;
export pipe(fildes:array(int)):int := Ccode(returns,"
     #ifdef HAVE_PIPE
      assert(fildes->len == 2);
      return pipe(fildes->array);
     #else
      return -1;
     #endif
     ");
import exec(argv:array(string)):int;	-- beware: this routine calls GC_malloc
export execstar(argv:charstarstar) ::= Ccode(int,"execvp(",argv,"[0],",argv,")");
import getenv(s:string):string;
import cpuTime():double;
import strcmp(s:string,t:string):int;
import strnumcmp(s:string,t:string):int;
import randomint():int;
export wait(pid:int):int := Ccode(returns, "
     #ifdef HAVE_WAITPID
      int status;
      if (waitpid(pid,&status,0) == -1) return -1;
      return status;
     #else
      return -1;
     #endif
     ");
import waitNoHang(pid:array(int)):array(int);
import select(s:array(int)):array(int);
import hash(x:double):int;
import getcwd():string;
import errfmt(filename:string,lineno:int,colno:int,loaddepth:int):string;
threadLocal export loadDepth := ushort(0);
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
import fileReadable(name:string):bool;
import fileWritable(name:string):bool;
import fileExecutable(name:string):bool;
import fileTime(name:string):int;
import setFileTime(name:string,modtime:int):int;
export currentTime():int := Ccode(int,"time(NULL)");
import fileMode(name:string):int;
import fileModeFD(fd:int):int;
import mkdir(name:string):int;
import rmdir(name:string):int;
import isDirectory(name:string):int;
import isRegularFile(name:string):int;
import wordexp(word:string):ArrayStringOrNull;
import readlink(filename:string):string;
import realpath(filename:string):(null or string);
import readDirectory(name:string):(null or array(string));
import strncmp(s:string,t:string,n:int):int;
import history():array(string);
import chdir(name:string):int;
import initReadlineVariables():void;
import handleInterruptsSetup(handleInterrupts:bool):void;
export segmentationFault():void := Ccode(void, "*((int*)1)=0"); -- for debugging our handling of segmentation faults
import isReady(fd:int):int;
import hasException(fd:int):int;

everytimeCell := { f:function():void, next:everytimeCell };
dummyfun():void := nothing;
everytimeList := everytimeCell(dummyfun,self);
export everytime(f:function():void):void := everytimeList = everytimeCell(f,everytimeList);
export everytimeRun():void := (
     x := everytimeList;
     while x.next != x do (x.f(); x = x.next;));

export limitFiles(n:int):int := Ccode(returns, "
     #ifdef HAVE_SETRLIMIT
      struct rlimit lim;
      lim.rlim_cur = lim.rlim_max = n;
      return setrlimit(RLIMIT_NOFILE,&lim);
     #else
      return -1;
     #endif
     ");
export limitProcesses(n:int):int := Ccode(returns, "
     #ifdef RLIMIT_NPROC
       struct rlimit lim;
       lim.rlim_cur = lim.rlim_max = n;
       return setrlimit(RLIMIT_NPROC,&lim);
     #else
       return -1;
     #endif
     ");

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d system.o "
-- End:
