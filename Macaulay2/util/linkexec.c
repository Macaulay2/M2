#include <malloc.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>

static char *dirname(char *p) {	/* with the slash */
  int n;
  char *s = strrchr(p,'/'), *d;
  if (s == NULL) return "";
  n = s-p+1;
  d = malloc(n+1);
  strncpy(d,p,n);
  d[n] = 0;
  return d;
}

static char *basename(char *p) {
  char *s = strrchr(p,'/');
  if (s == NULL) return p;
  return s+1;
}  

static char *stringcat(char *p, char *q) {
  char *x;
  x = malloc(strlen(p) + strlen(q) + 1);
  strcpy(x,p);
  strcat(x,q);
  return x;
}

static char *dircat(char *p, char *q) {
  if (q[0] == '/') return q;
  return stringcat(p,q);
}

static char *progname;

static char *getreadlink(char *p) {
  char buf[5000], *x;
  if (readlink(p,buf,sizeof buf) == -1) {
    fprintf(stderr,"%s: not a symbolic link, or expansion is too long\n", p);
    exit(1);
  }
  x = malloc(strlen(buf)+1);
  strcpy(x,buf);
  return x;
}

int main(int argc, char **argv, char **envp) {
  char *c;
  progname = basename(argv[0]);
  c = stringcat(dircat(dirname(argv[0]),dirname(getreadlink(argv[0]))),basename(argv[0]));
  if (0 == strcmp(c,argv[0])) {
    fprintf(stderr,"%s: executes itself\n", argv[0]);
    exit(1);
  }
  argv[0] = c;
  execve(argv[0],argv,envp);
  perror(argv[0]);
  exit(1);
  }
