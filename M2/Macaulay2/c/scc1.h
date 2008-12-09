/*		Copyright 1993 by Daniel R. Grayson		*/

struct ENV global;

extern FILE *dependfile;
extern char *targetname;
extern bool gc;
extern bool noline;
extern bool do_refctchks;
extern bool do_memstats;
extern bool do_countstats;
extern bool arraychks;
extern bool noinits;
extern bool oldc;
extern bool gcc;
extern char *getmem(unsigned);
extern char *strnperm(const char *, unsigned);
extern char *strperm(const char *);
extern node newnode1(unsigned int, enum TAG);
extern void fail (char *, int);
extern void myexit(int);
extern const struct POS empty_pos;
extern char *intToString(int);
extern int substr(char *, char *);
extern int strequaln(char *, char *, unsigned int);    
extern char *tail(char *);
extern char *BaseName(char *) ;
extern char *newsuffix(char *, char *);
extern char *newsuffixbase(char *, char *);
extern bool debug;
extern int tty();
