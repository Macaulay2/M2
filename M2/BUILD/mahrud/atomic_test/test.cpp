#include <vector>
#include <atomic>
#include <thread>

using namespace std;

extern "C" void adding();
extern "C" atomic_int acnt;
extern "C" int cnt;

int main()
{
  vector<thread> v;

  for(size_t i {0}; i < 9; ++i) {
    v.emplace_back(adding);
  }

  for(int i=0; i<10000; i++) {
    acnt++;
    cnt++;
  }

  for(auto &t : v) { t.join(); }

  printf("the value of acnt is %d\n", acnt.load());
  printf("the value of cnt is %d\n", cnt);

  return 0;
}

/*

  gcc -c test.c -o c11.o --std=c11
  g++ -c test.cpp -o cxx14.o --std=c++14
  g++ cxx14.o c11.o -pthread
  ./a.out

*/
