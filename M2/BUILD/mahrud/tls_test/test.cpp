#include <chrono>
#include <iostream>
#include <mutex>
#include <thread>
#include <vector>

using namespace std;

extern "C" thread_local unsigned int tlcounter;
extern "C" unsigned int counter;
extern "C" mutex cout_mutex;
extern "C" void incr(const char name);

int main()
{
  vector<thread> v;

  for(size_t i = 0; i < 26; ++i) {
    v.emplace_back(incr, 'A' + i);
  }

  {
    this_thread::sleep_for(700000ns);
    lock_guard<mutex> lock(cout_mutex);
    printf("Value for main:\t%d\tvs %d\n", ++counter, ++tlcounter);
  }

  for(auto &t : v) { t.join(); }

  return 0;
}

/*
  gcc -c test.c -o c11.o --std=c11
  g++ -c test.cpp -o cxx14.o --std=c++14
  g++ cxx14.o c11.o -pthread
  ./a.out
*/
