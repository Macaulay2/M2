
/* configuration section */
/* make intP an integer type the size of a pointer */
#if defined(__i386__) || defined(__sparc__)
typedef int intP;
typedef unsigned int uintP;
#elif defined(__alpha__)
typedef long long intP;
typedef unsigned long long uintP;
#else
typedef long long intP;
typedef unsigned long long uintP;
#endif
/* end configuration section */

#define TRUE 1
#define FALSE 0
#define OKAY 0
#define ERROR (-1)
#define STDIN 0
#define STDOUT 1
#define STDERR 2
#define numberof(x) (sizeof(x)/sizeof(x[0]))
