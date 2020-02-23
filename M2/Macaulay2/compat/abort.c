#define STDERR 2
static char m[] = "abort() called\n";
extern int write();
extern void exit();
void abort() {
  int *p = (int*)(-1), x;
  write(STDERR,m,sizeof m-1);
  x = *p;			/* provoke a segmentation fault if possible */
  exit(1);
}
