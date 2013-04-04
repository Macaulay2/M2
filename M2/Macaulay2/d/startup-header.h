/* the header of startup.c, from file startup-header.c: */

typedef struct {
     const char *filename, *contents;
     } cached_file;
extern cached_file startupFile;
extern cached_file testStrings[];
extern int num_testStrings;
