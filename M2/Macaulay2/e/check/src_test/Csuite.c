/* csuite.c */

#include "Csuite.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

enum {CHUNK = 10};

struct Suite
{
    char* name;
    FILE* pStream;
        size_t nTests;
        size_t maxTests;
    Test** pTests;
};


Suite* cs_create(const char* name)
{
        int backOutLevel = 0;
        Suite* pSuite = malloc(sizeof(Suite));
        if (pSuite)
        {
                pSuite->nTests = 0;
                pSuite->pStream = stdout;

                /* Allocate array of fptrs: */
                assert(CHUNK);
                pSuite->pTests = calloc(CHUNK, sizeof(Test*));
                if (pSuite->pTests)
                {
                        pSuite->maxTests = CHUNK;
                        /* Allocate test name: */
                        pSuite->name = malloc(strlen(name) + 1);
                        if (name)
                                strcpy(pSuite->name, name);
                        else
                                ++backOutLevel;
                }
                else
                        ++backOutLevel;
        }

        /* Back-out allocations if memory failed: */
        if (backOutLevel)
        {
                switch(backOutLevel)
                {
                case 2:
                        free(pSuite->pTests);
                case 1:
                        free(pSuite);
                        pSuite = NULL;
                }
        }
        return pSuite;
}

void cs_destroy(Suite* pSuite, bool dropTests)
{
        assert(pSuite);
        assert(pSuite->pTests);
        if (dropTests)
        {
                size_t i;
                for (i = 0; i < pSuite->nTests; ++i)
                        ct_destroy(pSuite->pTests[i]);
        }
        free(pSuite->pTests);
        assert(pSuite->name);
        free(pSuite->name);
        free(pSuite);
}

const char* cs_getName(Suite* pSuite)
{
        assert(pSuite);
    return pSuite->name;
}

FILE* cs_getStream(Suite* pSuite)
{
        assert(pSuite);
    return pSuite->pStream;
}

void cs_setStream(Suite* pSuite, FILE* pStream)
{
        assert(pSuite);
    pSuite->pStream = pStream;
}

bool cs_addTest(Suite* pSuite, Test* pTest)
{
        FILE* pTestStream;
        assert(pSuite);
        assert(pTest);
        pTestStream = ct_getStream(pTest);
    if (pSuite->pStream && pTestStream == NULL)
        ct_setStream(pTest, pSuite->pStream);

        assert(pSuite->pTests);
        if (pSuite->nTests == pSuite->maxTests)
        {
                size_t newSize = pSuite->nTests + CHUNK;
                Test** new_pTests =
                        realloc(pSuite->pTests, newSize * sizeof(Test*));
                if (!new_pTests)
                        return FALSE;
                pSuite->pTests = new_pTests;
                pSuite->maxTests += CHUNK;
        }
        assert(pSuite->nTests < pSuite->maxTests);
        pSuite->pTests[pSuite->nTests++] = pTest;
    ct_reset(pTest);
        return TRUE;
}

bool cs_addSuite(Suite* pSuite, Suite* pSuite2)
{
        size_t i;
        bool rc;
        assert(pSuite);
        assert(pSuite2);
    for (i = 0; i < pSuite->nTests; ++i)
        rc = cs_addTest(pSuite, pSuite2->pTests[i]);
        return rc;
}

void cs_run(Suite* pSuite)
{
        size_t i;
        assert(pSuite);
    cs_reset(pSuite);
    for (i = 0; i < pSuite->nTests; ++i)
    {
        assert(pSuite->pTests[i]);
        ct_run(pSuite->pTests[i]);
    }
}


long cs_report(Suite* pSuite)
{
        assert(pSuite);
    if (pSuite->pStream)
    {
        size_t i;
        long totFail = 0;
                FILE* out = pSuite->pStream;
                const char* name = pSuite->name;
                size_t nameLen = strlen(name);

        fprintf(out, "Suite \"%s\n=======", name);
        for (i = 0; i < nameLen; ++i)
            fputc('=', out);
        fputs("=\n", out);

        for (i = 0; i < pSuite->nTests; ++i)
        {
            assert(pSuite->pTests[i]);
            totFail += ct_report(pSuite->pTests[i]);
        }

        fputs("=======", out);
        for (i = 0; i < nameLen; ++i)
            fputc('=', out);
        fputs("=\n", out);
        return totFail;
    }
    else
        return cs_getNumFailed(pSuite);
}

long cs_getNumPassed(Suite* pSuite)
{
        size_t i;
    long totPass = 0;
        assert(pSuite);
        assert(pSuite->pTests);
    for (i = 0; i < pSuite->nTests; ++i)
    {
        assert(pSuite->pTests[i]);
        totPass += ct_getNumPassed(pSuite->pTests[i]);
    }
    return totPass;
}

long cs_getNumFailed(Suite* pSuite)
{
        size_t i;
    long totFail = 0;
        assert(pSuite);
        assert(pSuite->pTests);
    for (i = 0; i < pSuite->nTests; ++i)
    {
        assert(pSuite->pTests[i]);
        totFail += ct_getNumFailed(pSuite->pTests[i]);
    }
    return totFail;
}

void cs_reset(Suite* pSuite)
{
        size_t i;
        assert(pSuite);
        assert(pSuite->pTests);
    for (i = 0; i < pSuite->nTests; ++i)
    {
        assert(pSuite->pTests[i]);
        ct_reset(pSuite->pTests[i]);
    }
}

long cs_getNumTests(Suite* pSuite)
{
        assert(pSuite);
        return pSuite->nTests;
}
