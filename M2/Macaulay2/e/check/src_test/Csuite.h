/* csuite.h */

#ifndef CSUITE_H
#define CSUITE_H

#include <stdio.h>
#include "Ctest.h"

typedef struct Suite Suite;

Suite* cs_create(const char* name);
void cs_destroy(Suite* pSuite, bool freeTests);

const char* cs_getName(Suite* pSuite);
long cs_getNumPassed(Suite* pSuite);
long cs_getNumFailed(Suite* pSuite);
long cs_getNumTests(Suite* pSuite);
FILE* cs_getStream(Suite* pSuite);
void cs_setStream(Suite* pSuite, FILE* stream);

bool cs_addTest(Suite* pSuite, Test* pTest);
bool cs_addSuite(Suite* pSuite, Suite* pSuite2);
void cs_run(Suite* pSuite);
long cs_report(Suite* pSuite);
void cs_reset(Suite* pSuite);

#endif
