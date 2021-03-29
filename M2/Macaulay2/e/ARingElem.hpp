#ifndef _aring_elem_hpp_
#define _aring_elem_hpp_

template<typename RT>  // RT = ring type, a type of ARing, should be a concept
class ARingElem
{
public:
  using Element = typename RT::ElementType;

  ARingElem(const RT* F)
    : mRing(F)
  {
    // init sets it equal to 0 (?)
    mRing->init(mElement);
  }
  ARingElem(const RT* F, const Element& f)
    : mRing(F)
  {
    mRing->init_set(mElement,f);
  }
  ~ARingElem()
  {
    mRing->clear(mElement);
  }
  const ARing& ring() const
  {
    return *mRing;
  }
  Element& operator*()
  {
    return mElement;
  }
  const Element& operator*() const
  {
    return mElement;
  }
  bool operator==(const ARingElem& g) const
  {
    assert(mRing == g.mRing);
    return mRing->is_equal(mElement,*g);
  }
  ARingElem operator+(const ARingElem& g)
  {
    assert(mRing == g.mRing);
    ARingElem result(mRing);
    mRing->add(*result, mElement, *g);
    return result;
  }
  ARingElem operator-(const ARingElem& g)
  {
    assert(mRing == g.mRing);
    ARingElem result(mRing);
    mRing->subtract(*result, mElement, *g);
    return result;
  }
  ARingElem operator-() const
  {
    assert(mRing == g.mRing);
    ARingElem result(mRing);
    mRing->negate(*result, mElement);
    return result;
  }
  ARingElem operator*(const ARingElem& g)
  {
    assert(mRing == g.mRing);
    ARingElem result(mRing);
    mRing->mult(*result, mElement, *g);
    return result;
  }
  // also add operator *= and += etc
  ARingElem operator^(int n)
  {
    assert(mRing == g.mRing);
    ARingElem result(mRing);
    mRing->power(*result, mElement, n);
    return result;
  }
private:
  const RT* mRing;
  Element mElement;
};

#endif
