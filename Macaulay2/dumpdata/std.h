#define TRUE 1
#define FALSE 0
#define OKAY 0
#define ERROR (-1)
#define STDIN 0
#define STDOUT 1
#define STDERR 2
#define numberof(x) (sizeof(x)/sizeof(x[0]))

#if defined(__i386__) || defined(__sparc__)
typedef unsigned int intP;
#elif defined(__alpha__)
typedef unsigned long long intP;
#else
typedef unsigned long long intP;
#endif
