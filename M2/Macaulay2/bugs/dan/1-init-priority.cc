// we might be able to use this feature to control order of running constructors in gcc

#include <stdio.h>
struct a { a() { printf("a()\n"); } };
struct b { b() { printf("b()\n"); } };
struct c { c(int i) { printf("c(%d)\n",i); } };

c x(1);
a p  __attribute__ ((init_priority (101)));
c y(2);
b r __attribute__ ((init_priority (65535))) ;
c z(3);

main () {
}
