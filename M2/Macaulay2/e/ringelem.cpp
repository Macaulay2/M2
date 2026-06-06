#include <ringelem.hpp>

// TODO: don't use NULL or 0 to check end of Nterm
TermIterator<Nterm> begin(Nterm* ptr) { return TermIterator<Nterm>(ptr); }
TermIterator<Nterm> end(Nterm*)       { return TermIterator<Nterm>{}; }

TermIterator<vecterm> begin(vecterm* ptr) { return TermIterator<vecterm>(ptr); }
TermIterator<vecterm> end(vecterm*)       { return TermIterator<vecterm>{}; }
