// t_test.cpp: Tests the Test and Suite classes

#include <iostream>
#include <sstream>
#include <complex>
#include "suite.h"
using namespace std;

class ComplexTest : public Test
{
    complex<double>* m_c1;
    complex<double>* m_c2;
    complex<double>* m_c3;

public:
    ComplexTest()
    {
        m_c1 = new complex<double>(1,1);
        m_c2 = new complex<double>(2,2);
        m_c3 = new complex<double>(3,3);
    }
    ~ComplexTest()
    {
        delete m_c1;
        delete m_c2;
        delete m_c3;
    }
    void run()
    {
        testEqual();
        testAdd();
    }
    void testEqual()
    {
        complex<double> c1(1,1);
        _test(*m_c1 == c1);
        _test(!(m_c1 == m_c2));
    }
    void testAdd()
    {
        _test(*m_c1 + *m_c2 != *m_c3);   // failure
    }
};

class VoidTest : public Test
{
public:
    void run()
    {
        _test(true);
    }
};

int main()
{
    Suite s("Suite Test");

    // Catch null test:
    try
    {
        s.addTest(0);
    }
    catch (TestSuiteError&)
    {
        cout << "Caught bad addTest\n";;
    }

    // Use null stream:
    s.addTest(new ComplexTest);
    s.addTest(new VoidTest);
    s.run();
    long fails = s.report();
    cout << "fails == " << fails << " (should be 1)\n\n";

    // Test addSuite; use a string stream
    Suite s2(s.getName());
    ostringstream os;
    s2.setStream(&os);
    s2.addSuite(s);
    s2.run();
    fails = s2.report();
    cout << "Total Passes: " << s2.getNumPassed()
         << " (should be 3)\n";
    cout << "Total failures: " << fails << " (should be 1)\n";
    cout << os.str();
    cout << endl;

    s2.free();
    // Don't free s!
}

/* Output:
Caught bad addTest
fails == 1 (should be 1)

Total Passes: 3 (should be 3)
Total failures: 1 (should be 1)
Failed:: complex: (*m_c1 + *m_c2 != *m_c3) , c:\reuse\testsuite\test\main.cpp (line 42)
Suite "Suite Test"
==================
Test "complex":
    Passed: 2    Failed: 1
Test "void":
    Passed: 1    Failed: 0
==================

Stream operator test:

Suite "Suite Test"
==================
Test "complex":
    Passed: 2    Failed: 1
Test "void":
    Passed: 1    Failed: 0
==================
*/

