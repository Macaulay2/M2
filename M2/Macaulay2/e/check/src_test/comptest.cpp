#include <stdio.h>
#include <assert.h>
#include "ctest.h"      /* the Test "class" */

/* Stuff to test (usually #include'd) */
typedef struct
{
    double real, imag;
} complex;

complex c_add(complex c1, complex c2)
{
    complex r;
    r.real = c1.real + c2.real;
    r.imag = c1.imag + c2.imag;
    return r;
}

complex c1 = {1.0, 1.0};
complex c2 = {2.0, 2.0};
complex c3 = {3.0, 3.0};

void testEqual(Test* pTest)
{
    complex z = {1.0, 1.0};
    ct_test(pTest, z.real == c1.real && z.imag == c1.imag);
    ct_test(pTest, z.real != c2.real && z.imag != c2.imag);
}

void testAdd(Test* pTest)
{
    complex r = c_add(c1, c2);
    ct_test(pTest, r.real == c3.real && r.imag == c3.imag);
}

int main()
{
    Test* pTest = ct_create("Complex", NULL);
    bool rc = ct_addTestFun(pTest, testEqual);
    rc = ct_addTestFun(pTest, testAdd);
    assert(rc);
    ct_setStream(pTest, stdout);
    ct_run(pTest);
    ct_report(pTest);
    ct_destroy(pTest);
    return 0;
}

/* Output:
Test "Complex":
Passed: 3
Failed: 0
*/

