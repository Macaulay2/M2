#include "exception.hh"

int main(int argc, char **argv)
{
  try {
    if (argc > 1) throw Not_Implemented_Exception("hi there");
    else throw Exception();
  }
  catch (Not_Implemented_Exception &e) {cerr << "NOTIMP " << e;}
  catch (Exception &e) {cerr << "GENERAL " << e;}

  cerr << endl << "here I am" << endl;
}
