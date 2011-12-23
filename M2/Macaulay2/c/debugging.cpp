#include "scc.h"
#include "debugging.h"
bool debug = FALSE;
int debugLevel = 0;

void trap() {}			/* set a breakpoint here */

int tty(){
     return NULL != freopen("/dev/tty","w",stdout);
     }

/* to be called from the debugger */
void d(node n){
     debug=1;
     tty();
     pp(n);
     }

/* to be called from the debugger */
void dt(node n){
     debug=1;
     tty();
     if (istype(n))
	  dprinttype(n);
     else put("not a type");
     put("\n");
     }

/* to be called from the debugger */
void ds(scope v) {
     debug=1;
     tty();
     #define DS(X) if (v->X) { put("->" #X ": "); pp(v->X); }
     #define DSR(X) if (v->X) { put("->" #X ", reversed: "); pp(reverse(v->X)); }
     DS(current_package);
     DS(symbols);
     DSR(decls);
     DSR(tmpdecls);
     DSR(before);
     DS(after);
     DS(finals);
     DSR(signature);
     DS(rettype);
     }

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
 End:
*/
