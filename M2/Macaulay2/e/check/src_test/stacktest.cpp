#include "Stack.h"
#include "test.h"
#include <iostream>
using namespace std;

class StackTest : public Test
{
    enum {SIZE = 5};
    Stack<int> stk;

public:
    StackTest() : stk(SIZE)
    {}

    void run()
    {
        testUnderflow();
        testPopulate();
        testOverflow();
        testPop();
        testBadSize();
    }

    void testBadSize()
    {
        try
        {
            Stack<int> s(0);
            _fail("Bad Size");
        }
        catch (StackError&)
        {
            _succeed();
        }
    }

    void testUnderflow()
    {
        _test(stk.size() == 0);

        try
        {
            stk.top();
            _fail("Underflow");
        }
        catch (StackError&)
        {
            _succeed();
        }

        try
        {
            stk.pop();
            _fail("Underflow");
        }
        catch (StackError&)
        {
            _succeed();
        }
    }

    void testPopulate()
    {
        try
        {
            for (int i = 0; i < SIZE; ++i)
                stk.push(i);
            _succeed();
        }
        catch (StackError&)
        {
            _fail("Populate");
        }

        _test(stk.size() == SIZE);
        _test(stk.top() == SIZE-1);
    }

    void testOverflow()
    {
        try
        {
            stk.push(SIZE);
            _fail("Overflow");
        }
        catch (StackError&)
        {
            _succeed();
        }
    }

    void testPop()
    {
        for (int i = 0; i < SIZE; ++i)
            _test(stk.pop() == SIZE-i-1);
        _test(stk.size() == 0);
    }
};

int main()
{
    StackTest t;
    t.setStream(&cout);
    t.run();
    t.report();
}

/* Output:
Test "class StackTest":
        Passed: 14      Failed: 0
*/

