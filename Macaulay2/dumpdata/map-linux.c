#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include "warning.h"
#include "map.h"
#include "file.h"
#include "std.h"

static char mapfilename[] = "/proc/self/maps";
static char mapfmt[] = "%p-%p %c%c%c%*c %*08x %*02x:%*02x %*8d %255s\n";

int haveDumpdata() {
  return TRUE;
}

int nummaps() {
  return fnumlines(mapfilename);
}

int getmaps(int nmaps, struct MAP maps[nmaps]) {
  int mlen = filelen(mapfilename);
  if (mlen == ERROR) return ERROR;
  else {
    char buf[mlen];
    char filename[256];		/* 1 more that the 255 in %255s above */
    char *line[nmaps];
    int i;
    if (ERROR == getfile(mapfilename,mlen,buf)) return ERROR;
    if (getenv("LOADDATA_DEBUG")) {
	 write(STDERR,buf,mlen);
    }
    lines(mlen,buf,nmaps,line);
    for (i=0; i<nmaps; i++) {
      int ret;
      char r, w, x;
      int len = linelen(line[i]);
      char buf2[len + 1];
      memcpy(buf2,line[i],len);
      buf2[len]=0;
      maps[i].filename = NULL;
      filename[0] = 0;
      ret = sscanf(buf2, mapfmt, &maps[i].from, &maps[i].to, &r, &w, &x, &filename /* may be absent */);
      if (ret < 5) {
	warning("can't parse map '%s' from file '%s' using format '%s'\n", buf2, mapfilename, mapfmt);
	return ERROR;
      }
      if (ret == 6 && filename[0] != 0) maps[i].filename = strdup(filename);
      maps[i].r = r == 'r';
      maps[i].w = w == 'w';
      maps[i].x = x == 'x';
      maps[i].checksum = 0;
    }
    return OKAY;
  }
}

#include "config.h"

#ifndef HAVE_ELF_H
#error expected HAVE_ELF_H to be defined
#endif

#if HAVE_ELF_H == 1 && HAVE_UNISTD_H == 1 && HAVE___ENVIRON == 1
#define ELF
#include <elf.h>
static void *elf_header_location() {
     char **p = __environ;
     while (*p++ != NULL);
     p++;
#if __WORDSIZE == 32
     Elf32_auxv_t *auxv = (Elf32_auxv_t *)p;
#else
     Elf64_auxv_t *auxv = (Elf64_auxv_t *)p;
#endif
     for ( ; auxv->a_type != AT_NULL; auxv++) {
	  switch( auxv->a_type ) {
	  case AT_SYSINFO_EHDR: return (void *)auxv->a_un.a_val;
	  }
     }
#if __WORDSIZE == 32
     return (void *)0xffffe000;	/* the usual 32-bit linux value */
#else
     return (void *)0xffffffffff600000L; /* the usual 64-bit linux value */
#endif
}
#endif

#ifdef ELF
#include <regex.h>
static int matches(char *pattern, char *text) {
  regex_t regex_pattern;
  if (regcomp(&regex_pattern, pattern, REG_EXTENDED|REG_NEWLINE|REG_NOSUB) != 0) return ERROR;
  return REG_NOMATCH != regexec(&regex_pattern, text, 0,0,0);
}
#endif

int isNotCheckable(map m) {
#ifdef ELF
     /* return TRUE for a few extra static memory map sections for which we don't care if the checksum changes */
     /* in GNU-Linux, this applies to the linux-gate.so.1 [vdso] section */
     return
       m->filename != NULL && matches("^/lib.*/ld-.*\\.so$",m->filename)
       ||
       m->from == elf_header_location() && getenv("COMPAREVDSO") == NULL;
#else
     return FALSE;
#endif
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
