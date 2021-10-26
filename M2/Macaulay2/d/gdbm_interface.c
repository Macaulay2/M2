/**********************************************
 *                  dbm stuff                 *
 **********************************************/

#include <stdio.h>
#include <string.h>
#include <gdbm.h>
#include <stdlib.h>
#include "M2mem.h"

#define TRUE 1
#define FALSE 0
#define ERROR (-1)

typedef char bool;
static int numfiles = 0;
static GDBM_FILE *gdbm_files = NULL;
void close_all_dbms(void) {
     int i;
     for (i=0; i<numfiles; i++) {
	  if (gdbm_files[i] != NULL) gdbm_close(gdbm_files[i]);
	  }
     }

#include "M2-exports.h"

int system_dbmopen(M2_string filename, M2_bool mutable) {
     int gdbm_handle;
     int flags = mutable ? GDBM_WRCREAT : GDBM_READER;
     int mode = 0666;
     char *FileName = M2_tocharstar(filename);
     GDBM_FILE f = gdbm_open(FileName, 4096, flags, mode, NULL);
     freemem(FileName);
     if (f == NULL) return ERROR;
     if (numfiles == 0) {
	  int i;
	  numfiles = 10;
	  gdbm_files = (GDBM_FILE *) getmem(numfiles * sizeof(GDBM_FILE));
	  for (i=0; i<numfiles; i++) gdbm_files[i] = NULL;
	  gdbm_handle = 0;
	  }
     else {
	  for (gdbm_handle=0; TRUE ; gdbm_handle++) {
	       if (gdbm_handle==numfiles) {
		    GDBM_FILE *p;
		    int j;
		    numfiles *= 2;
		    p = (GDBM_FILE *) getmem(numfiles * sizeof(GDBM_FILE));
		    for (j=0; j<gdbm_handle; j++) p[j] = gdbm_files[j];
		    gdbm_files = p;
	  	    for (j=gdbm_handle; j<numfiles; j++) gdbm_files[j] = NULL;
		    break;
		    }
	       else if (gdbm_files[gdbm_handle] == NULL) break;
	       }
	  }
     gdbm_files[gdbm_handle] = f;
     return gdbm_handle;
     }

int system_dbmclose(int handle) {
     gdbm_close(gdbm_files[handle]);
     gdbm_files[handle] = NULL;
     return 0;
     }

static datum todatum(M2_string x) {
     datum y;
     y.dptr = (char *)x->array;
     y.dsize = x->len;
     return y;
     }

static M2_string fromdatum(datum y) {
     M2_string x;
     if (y.dptr == NULL) return NULL;
     x = (M2_string)getmem(sizeofarray(x,y.dsize));
     x->len = y.dsize;
     memcpy(x->array, y.dptr, y.dsize);
     return x;
     }

static M2_string fromdatum_and_free(datum y) {
     M2_string x = fromdatum(y);
     free(y.dptr);
     y.dptr = NULL;
     y.dsize = 0;
     return x;
     }

int system_dbmstore(int handle, M2_string key, M2_string content) {
     return gdbm_store(gdbm_files[handle],todatum(key),todatum(content),GDBM_REPLACE);
     }

M2_string /* or NULL */ system_dbmfetch(int handle, M2_string key) {
     return fromdatum_and_free(gdbm_fetch(gdbm_files[handle],todatum(key)));
     }

int system_dbmdelete(int handle, M2_string key) {
     return gdbm_delete(gdbm_files[handle],todatum(key));
     }

static datum lastkey;
static bool hadlastkey = FALSE;

M2_string /* or NULL */ system_dbmfirst(int handle) {
     lastkey = gdbm_firstkey(gdbm_files[handle]);
     hadlastkey = TRUE;
     return fromdatum(lastkey);
     }

M2_string /* or NULL */ system_dbmnext(int handle) {
     if (hadlastkey) {
	  datum x = gdbm_nextkey(gdbm_files[handle] ,lastkey);
	  free(lastkey.dptr);
	  lastkey = x;
	  return fromdatum(lastkey);
	  }
     else {
	  return system_dbmfirst(handle);
	  }
     }

int system_dbmreorganize(int handle) {
     return gdbm_reorganize(gdbm_files[handle]);
     }

M2_string system_dbmstrerror(void) {
     return M2_tostring(gdbm_strerror(gdbm_errno));
     }

/*
 Local Variables:
 compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d gdbm_interface.o "
 End:
*/
