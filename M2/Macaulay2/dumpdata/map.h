#ifndef MAP_H
#define MAP_H

typedef struct MAP {
  /* these fields filled in by machine dependent code */
  void *from, *to;
  char r, w, x;
  /* extra fields ignored by machine dependent code, filled in later */
  unsigned int checksum;
} *map;

extern int nummaps(), haveDumpdata();
extern int getmaps(int nmaps, struct MAP[nmaps]);

#endif

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/dumpdata "
 End:
*/
