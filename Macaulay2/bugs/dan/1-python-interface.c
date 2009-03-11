/*

William Stein says:

If one wants to use Sage from a C program, e.g., like this (see below), then
it's important that "from sage.all import *" not import Ipython. The point of
this ticket is make the import of IPython lazy -- and only happen if
needed. This will also make "sage -python" and "sage -c" faster, since Ipython
startup takes significant time.



sage -sh
gcc -I$SAGE_LOCAL/include/python2.5 $SAGE_LOCAL/lib/python/config/libpython2.5.a embed.c -o embed; ./embed

See http://docs.python.org/extending/embedding.html

*/

#include <Python.h>

void use(void *p){}

int
main(int argc, char *argv[])
{
  /* use((void *)&PyExc_ValueError); */
  Py_Initialize();
  printf("Loading the Sage library...\n");
#if 1
  PyRun_SimpleString("from sage.all import *");
  printf("Factoring an integer:\n");
  PyRun_SimpleString("print factor(193048120380)");
  printf("Popping up plot of a function:\n");
  PyRun_SimpleString("x=var('x'); show(plot(sin(x)))");
  printf("Popping up plot of a 3-d function:\n");
  PyRun_SimpleString("x,y=var('x,y'); show(plot3d(sin(x*y)-cos(x-y), (x,-4,4),(y,-4,4)))");
  printf("Type 0 then return\n");
  int n;
  scanf("%d",&n);
#else
  PyRun_SimpleString("print(1+1)");
#endif
  printf("Exiting...\n");
  Py_Finalize();
  return 0;
}

/*
 -Wl,-Map,mapfile
 -Wl,-Bstatic
 -Wl,-Bdynamic
 -L/aux/dan/sage/sage-3.2.1-ubuntu8.04-32bit-PENTIUM-M-i686-Linux/local/lib -L/aux/dan/sage/sage-3.2.1-ubuntu8.04-32bit-PENTIUM-M-i686-Linux/local/lib/python/config
 -I/aux/dan/sage/sage-3.2.1-ubuntu8.04-32bit-PENTIUM-M-i686-Linux/local/include/python2.5

    Local Variables:
    compile-command: "set -x ; make 1-python-interface CFLAGS='-g' CPPFLAGS='-I/aux/dan/sage/sage-3.2.1-ubuntu8.04-32bit-PENTIUM-M-i686-Linux/local/include/python2.5' LDFLAGS='-Wl,-Map,mapfile' LDLIBS='-lpython2.5 -lpthread -lutil -ldl -lm' && env PYTHONPATH=:/aux/dan/sage/sage-3.2.1-ubuntu8.04-32bit-PENTIUM-M-i686-Linux/local/lib/python PYTHONHOME=/aux/dan/sage/sage-3.2.1-ubuntu8.04-32bit-PENTIUM-M-i686-Linux/local SAGE_ROOT=/aux/dan/sage/sage-3.2.1-ubuntu8.04-32bit-PENTIUM-M-i686-Linux LD_LIBRARY_PATH=/aux/dan/sage/sage-3.2.1-ubuntu8.04-32bit-PENTIUM-M-i686-Linux/local/lib:$LD_LIBRARY_PATH ./1-python-interface"
    End:
*/
