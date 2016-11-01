#ifndef __stash_hpp_
#define __stash_hpp_
class stash
{
public:
  stash(const char *s, size_t len) : element_size(len) {}
  ~stash() {}

  void *new_elem() { return new char[element_size]; }
  void delete_elem(void *p) {
    if (p != nullptr)
      {
        char* q = static_cast<char*>(p);
        delete [] q;
      }
  }

  //  void text_out(buffer &o) const; // Display statistics about this stash.
  //  static void stats(buffer &o);
private:
  size_t element_size;          // In bytes
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions "
// indent-tabs-mode: nil
// End:
