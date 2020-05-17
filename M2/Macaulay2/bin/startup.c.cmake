/* Macaulay2/bin/startup.c is generated from Macaulay2/bin/startup.c.cmake. */

typedef struct {
  const char *filename, *contents;
} cached_file;

extern cached_file startupFile;
extern cached_file testStrings[];
extern int num_testStrings;

cached_file startupFile =
  {
   @STARTUP_M2_ADDR@,
   @STARTUP_M2_CONTENT@
  };

cached_file testStrings[] =
  {
@TEST_STRINGS@
  };

int num_testStrings = sizeof(testStrings)/sizeof(testStrings[0]);
