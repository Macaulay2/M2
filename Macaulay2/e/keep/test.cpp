
#include "newZ.hh"

int main(int argc, char **argv)
{
  i_myZ();
  // Input numbers from argv, and do various operations on them
  
  Zelem f = myZ::from_string(argv[1]);
  cout << "f = ";
  myZ::elem_text_out(cout, f);
  cout << endl;
}
