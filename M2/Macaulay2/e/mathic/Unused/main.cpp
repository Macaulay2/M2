#include "divsim/divMain.h"
#include "pqsim/pqMain.h"
#include <string>
#include <iostream>

int main(int argc, const char** args) {
  std::string name = args[0];
  const size_t offset = name.find_last_of("/\\");
  if (offset != std::string::npos)
	name = name.substr(offset + 1);
  if (name == "div")
	return divMain();
  else if (name == "pq")
	return pqMain(argc, args);
  std::cout << "Name of executable must be div or pq.\n";
  return 1;
}
