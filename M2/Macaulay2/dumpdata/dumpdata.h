#define TRUE 1
#define FALSE 0

typedef struct MAP {
  void *from, *to;
  char r, w, x, p;
  int offset;
  int dev_major, dev_minor, inode;
  char *filename;
} * map;

void dumpdata(const char *);
