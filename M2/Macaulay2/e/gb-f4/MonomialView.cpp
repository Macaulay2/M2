#include "MonomialView.hpp"
#include "../exceptions.hpp"
namespace newf4 {

// a general splice command which handles lcm, product and quotient
MonomialView MonomialView::combine(const MonomialView& left,
				   const MonomialView& right,
				   bool useLeft,
				   bool useRight,	
				   std::function<MonomialInt(MonomialInt,MonomialInt)> bothFunc,
				   MemoryBlock &block)
{
    auto rng = block.allocateArray<MonomialInt>(left.size() + right.size() - 1);
    MonomialView result(rng.first);

    auto leftIt = left.begin();
    auto rightIt = right.begin();
    auto leftEnd = left.end();
    auto rightEnd = right.end();
    auto resultIt = result.begin();

    while (true)
    {
       if (leftIt == leftEnd)
       {
	 // take from right and increment right and result
	 if (useRight)
	 {
	   std::copy(rightIt.loc(),rightEnd.loc(),resultIt.loc());
	   resultIt += ((rightEnd.loc() - rightIt.loc()) >> 1);
	 }
	 break;
       }
       if (rightIt == rightEnd)
       {
         // take from left and increment left and result
	 if (useLeft)
	 {
	    std::copy(leftIt.loc(),leftEnd.loc(),resultIt.loc());
	    resultIt += ((leftEnd.loc() - leftIt.loc()) >> 1);
	 }
	 break;
       }
       if (leftIt.var() < rightIt.var())
       {
	  // take from left and increment left and result
	  std::copy(leftIt.loc(), leftIt.loc() + 2, resultIt.loc());
	  ++resultIt;
	  ++leftIt;
       }
       else if (leftIt.var() > rightIt.var())
       {
	  // take from right and increment right and result
	  std::copy(rightIt.loc(), rightIt.loc() + 2, resultIt.loc());
	  ++resultIt;
	  ++rightIt;
       }
       else
       {
	  // take maximum and increment both
	  resultIt.loc()[0] = leftIt.var();
	  auto temp = bothFunc(leftIt.power(), rightIt.power());
	  if (temp > 0) // if we actually need this variable, store power and increment result
	  {
	    resultIt.loc()[1] = bothFunc(leftIt.power(), rightIt.power());
	    ++resultIt;
	  }
	  ++leftIt;
	  ++rightIt;
       }
    }
    block.shrinkLastAllocate(rng.first, rng.second, resultIt.loc());
    *rng.first = resultIt.loc() - rng.first;
    return result;
}

MonomialView MonomialView::lcm(const MonomialView& left,
			       const MonomialView& right,
			       MemoryBlock &block)
{
  return MonomialView::combine(left,
			       right,
			       true,
			       true,
			       [](MonomialInt l, MonomialInt r) -> MonomialInt { return std::max(l,r); },
			       block);
}
  
MonomialView MonomialView::product(const MonomialView& left,
				   const MonomialView& right,
				   MemoryBlock &block)
{
  return MonomialView::combine(left,
			       right,
			       true,
			       true,
			       [](MonomialInt l, MonomialInt r) -> MonomialInt { return l+r; },
			       block);
}
  

// returns monomial m such that right*m is a multiple of left.
MonomialView MonomialView::quotient(const MonomialView& left,
				    const MonomialView& right,
				    MemoryBlock &block)
{
  return MonomialView::combine(left,
			       right,
			       true,
			       false,
			       [](MonomialInt l, MonomialInt r) -> MonomialInt { return std::max(0,l-r); },
			       block);
}

void MonomialView::display(std::ostream& o,
                           const std::vector<std::string>& varnames,
                           const newf4::MonomialView& m
                           )
{
  // Note: if the monomial is '1', nothing is printed.
  // TODO: possibly allow print_one boolean argument, instead of handling it at calling point?
  bool first = true;
  for (auto t = m.begin(); t != m.end(); ++t)
    {
      auto v = t.var();
      auto e = t.power();
      if (v >= varnames.size() or v < 0)
        {
          throw exc::engine_error("variable out of range");
        }
      if (not first)
        {
          o << '*';
        }
      first = false;
      o << varnames[v];
      if (e != 1)
        o << '^' << e;
    }
}
  
} // end namespace f4

