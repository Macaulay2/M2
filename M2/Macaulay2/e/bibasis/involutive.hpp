/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef BIBASIS_INVOLUTIVE_HPP
#define BIBASIS_INVOLUTIVE_HPP

#include <list>
#include <algorithm>
#include "pcomparator.hpp"
#include "qset.hpp"
#include "tset.hpp"
#include "matrix.hpp"
#include "matrix-con.hpp"

namespace BIBasis
{
    template <typename MonomType>
    class BooleanInvolutiveBasis
    {
    private:
        std::list<Polynom<MonomType>*> GBasis;
        TSet<MonomType> IntermediateBasis;
        QSet<MonomType> ProlongationsSet;
        const PolynomialRing* const PRing;

    public:
        BooleanInvolutiveBasis(const Matrix* matrix, bool toGroebner);
        ~BooleanInvolutiveBasis();
        
        const Polynom<MonomType>& operator[](int number) const;
        unsigned Length() const;

        const Matrix* ToMatrix() const;

    private:
        void FillInitialSet(const Matrix* matrix, std::list<Polynom<MonomType>*>& initialSet) const;
    
        Polynom<MonomType>* NormalForm(const Triple<MonomType>* triple) const;
        const Polynom<MonomType>* FindDivisor(const Polynom<MonomType>* polynom
                                            , const std::list<Polynom<MonomType>*>& set
                                            , bool toGroebner) const;
        Polynom<MonomType>* Reduce(Polynom<MonomType>* polynom
                                 , const std::list<Polynom<MonomType>*>& set
                                 , bool toGroebner) const;
        void ReduceSet(bool toGroebner);
        void Reset();
        void Construct(const std::list<Polynom<MonomType>*>& set, bool toGroebner);
        void ConstructInvolutiveBasis();
    };
    
    template <typename MonomType>
    BooleanInvolutiveBasis<MonomType>::BooleanInvolutiveBasis(const Matrix* matrix, bool toGroebner)
        : GBasis()
        , IntermediateBasis()
        , ProlongationsSet()
        , PRing(matrix->get_ring()->cast_to_PolynomialRing())
    {
        try
        {
            std::list<Polynom<MonomType>*> initialSet;
            FillInitialSet(matrix, initialSet);

            Construct(initialSet, toGroebner);

            initialSet.clear();
        }
        catch(std::string& errorString)
        {
            ERROR(errorString.c_str());
            Reset();
        }
        catch(...)
        {
            ERROR("BIBasis::BooleanInvolutiveBasis::BooleanInvolutiveBasis(): unknown error.");
            Reset();
        }
    }

    template <typename MonomType>
    BooleanInvolutiveBasis<MonomType>::~BooleanInvolutiveBasis()
    {
        Reset();
    }

    template <typename MonomType>
    const Polynom<MonomType>& BooleanInvolutiveBasis<MonomType>::operator[](int num) const
    {
        typename std::list<Polynom<MonomType>*>::const_iterator it(GBasis.begin());
        for (unsigned i = Length()-1-num; i > 0; i--) 
        {
            ++it;
        }
        return **it;
    }

    template <typename MonomType>
    unsigned BooleanInvolutiveBasis<MonomType>::Length() const
    {
        return GBasis.size();
    }

    template <typename MonomType>
    const Matrix* BooleanInvolutiveBasis<MonomType>::ToMatrix() const
    {
        MatrixConstructor matrixConstructor(PRing->make_FreeModule(1), 0);
        const Monoid* monoid = PRing->getMonoid();
        const ring_elem coefficientUnit = PRing->getCoefficients()->one();
        monomial tmpRingMonomial = monoid->make_one();
        
        for (typename std::list<Polynom<MonomType>*>::const_iterator currentPolynom = GBasis.begin(); 
             currentPolynom != GBasis.end(); 
             ++currentPolynom)
        {
            if (!*currentPolynom)
            {
                continue;
            }
            
            ring_elem currentRingPolynomial;

            for (const MonomType* currentMonom = &(**currentPolynom).Lm(); 
                 currentMonom; 
                 currentMonom = currentMonom->Next)
            {
                exponents currentExponent = newarray_atomic_clear(int, MonomType::GetDimIndepend());
                typename std::set<typename MonomType::Integer> variablesSet = currentMonom->GetVariablesSet();
                
                for (typename std::set<typename MonomType::Integer>::const_iterator currentVariable = variablesSet.begin();
                     currentVariable != variablesSet.end();
                     ++currentVariable)
                {
                    currentExponent[*currentVariable] = 1;
                }
                
                monoid->from_expvector(currentExponent, tmpRingMonomial);
                freemem(currentExponent);

                ring_elem tmpRingPolynomial = PRing->make_flat_term(coefficientUnit, tmpRingMonomial);
                PRing->add_to(currentRingPolynomial, tmpRingPolynomial);
            }
            
            matrixConstructor.append(PRing->make_vec(0, currentRingPolynomial));
        }
        
        return matrixConstructor.to_matrix();
    }

    template <typename MonomType>
    void BooleanInvolutiveBasis<MonomType>::FillInitialSet(const Matrix* matrix, std::list<Polynom<MonomType>*>& initialSet) const
    {
        const Monoid* monoid = PRing->getMonoid();
        typename MonomType::Integer independ = MonomType::GetDimIndepend();
        
        //construct Polynom for every column in matrix
        for (int column = 0; column < matrix->n_cols(); ++column)
        {
            vec polynomVector = matrix->elem(column);
            if (!polynomVector)
            {
                continue;
            }

            Polynom<MonomType>* currentPolynom = new Polynom<MonomType>();

            for (Nterm* currentTerm = polynomVector->coeff; currentTerm; currentTerm = currentTerm->next)
            {
                exponents monomVector = newarray_atomic(int, independ);
                monoid->to_expvector(currentTerm->monom, monomVector);
                
                //construct Monom for every term
                MonomType* currentMonom = new MonomType();
                if (!currentMonom)
                {
                    freemem(monomVector);
                    throw std::string("BIBasis::BooleanInvolutiveBasis::FillInitialSet(): got NULL instead of new monom.");
                }
                
                for (typename MonomType::Integer currentVariable = 0; currentVariable < independ; ++currentVariable)
                {
                    if (monomVector[currentVariable])
                    {
                        *currentMonom *= currentVariable;
                    }
                }
                
                *currentPolynom += *currentMonom;
                freemem(monomVector);
                delete currentMonom;
            }
            
            initialSet.push_back(currentPolynom);
        }
    }
    
    template <typename MonomType>
    Polynom<MonomType>* BooleanInvolutiveBasis<MonomType>::NormalForm(const Triple<MonomType>* triple) const
    {
        /* As far as currentTriple can't be 0 (checked in QSET and TSET),
         * no need to check for NULL pointer. 
         */

        const Triple<MonomType>* involutiveDivisor = 0;
        Polynom<MonomType>* originalForm = 0;
        Polynom<MonomType>* normalForm = new Polynom<MonomType>();

        if (triple->GetVariable() == -1)
        {
            originalForm = new Polynom<MonomType>(*triple->GetPolynom());
        }
        else
        {
            originalForm = new Polynom<MonomType>(*triple->GetWeakAncestor()->GetPolynom());
            (*originalForm) *= triple->GetVariable();
        }

        while (!originalForm->IsZero())
        {
            involutiveDivisor = IntermediateBasis.Find(originalForm->Lm());
            while (involutiveDivisor)
            {
                originalForm->HeadReduction(*involutiveDivisor->GetPolynom());
                if (!originalForm->IsZero())
                {
                    involutiveDivisor = IntermediateBasis.Find(originalForm->Lm());
                }
                else
                {
                    involutiveDivisor = 0;
                }
            }

            if (!originalForm->IsZero())
            {
                (*normalForm) += originalForm->Lm();
                originalForm->RidOfLm();
            }
        }

        delete originalForm;
        return normalForm;
    }

    template <typename MonomType>
    const Polynom<MonomType>* BooleanInvolutiveBasis<MonomType>::FindDivisor(const Polynom<MonomType>* polynom
                                                                  , const std::list<Polynom<MonomType>*>& set
                                                                  , bool toGroebner) const
    {
        if (!polynom || polynom->IsZero()) 
        {
            return 0;
        }

        typename std::list<Polynom<MonomType>*>::const_iterator it(set.begin()), setEnd(set.end());
        const MonomType& plm = polynom->Lm();

        while (it != setEnd)
        {
            if (toGroebner && plm.IsDivisibleBy((**it).Lm())) 
            {
                return *it;
            }
            else if (!toGroebner && plm.IsPommaretDivisibleBy((**it).Lm()))
            {
                return *it;
            }
            ++it;
        }

        return 0;
    }

    template <typename MonomType>
    Polynom<MonomType>* BooleanInvolutiveBasis<MonomType>::Reduce(Polynom<MonomType>* polynom
                                                       , const std::list<Polynom<MonomType>*>& set
                                                       , bool toGroebner) const
    {
        if (!polynom)
        {
            return 0;
        }

        Polynom<MonomType>* result = new Polynom<MonomType>();
        const Polynom<MonomType>* currentReducer = 0;

        while (!polynom->IsZero())
        {
            currentReducer = FindDivisor(polynom, set, toGroebner);
            while (currentReducer)
            {
                polynom->Reduction(*currentReducer);
                currentReducer = FindDivisor(polynom, set, toGroebner);
            }
            if (!polynom->IsZero())
            {
                (*result) += polynom->Lm();
                polynom->RidOfLm();
            }
        }

        polynom = result;
        return result;
    }

    template <typename MonomType>
    void BooleanInvolutiveBasis<MonomType>::ReduceSet(bool toGroebner)
    {
        std::list<Polynom<MonomType>*> tmpPolySet;
        GBasis.sort(PointerMoreComparator<Polynom<MonomType> >());

        while (!GBasis.empty())
        {
            Polynom<MonomType>* currentPolynom = GBasis.front();
            GBasis.pop_front();
            currentPolynom = Reduce(currentPolynom, tmpPolySet, toGroebner);

            if (currentPolynom && !currentPolynom->IsZero())
            {
                const MonomType& hLm = currentPolynom->Lm();
                typename std::list<Polynom<MonomType>*>::iterator iteratorTmpPolySet = tmpPolySet.begin();
                while (iteratorTmpPolySet != tmpPolySet.end())
                {
                    if ((**iteratorTmpPolySet).Lm().IsDivisibleBy(hLm))
                    {
                        GBasis.push_back(*iteratorTmpPolySet);
                        iteratorTmpPolySet = tmpPolySet.erase(iteratorTmpPolySet);
                    }
                    else
                    {
                        ++iteratorTmpPolySet;
                    }
                }
                tmpPolySet.push_back(currentPolynom);
            }
        }

        unsigned tmpPolySetSize = static_cast<unsigned int>(tmpPolySet.size());
        for (unsigned i = 0; i < tmpPolySetSize; ++i)
        {
            Polynom<MonomType>* currentPolynom = tmpPolySet.front();
            tmpPolySet.pop_front();
            currentPolynom = Reduce(currentPolynom, tmpPolySet, toGroebner);
            if (!currentPolynom || currentPolynom->IsZero())
            {
                tmpPolySetSize--;
            }
            else
            {
                tmpPolySet.push_back(currentPolynom);
            }
        }

        GBasis = tmpPolySet;
    }

    template <typename MonomType>
    void BooleanInvolutiveBasis<MonomType>::Reset()
    {
        IntermediateBasis.Clear();
        ProlongationsSet.Clear();
        GBasis.clear();
    }
    
    template <typename MonomType>
    void BooleanInvolutiveBasis<MonomType>::Construct(const std::list<Polynom<MonomType>*>& set, bool toGroebner)
    {
        Reset();
        GBasis = set;
        ReduceSet(true);
        
        ProlongationsSet.Insert(GBasis);
        GBasis.clear();

        ConstructInvolutiveBasis();
        ProlongationsSet.Clear();

        typename TSet<MonomType>::ConstIterator i2(IntermediateBasis.Begin());
        while (i2 != IntermediateBasis.End())
        {
            GBasis.push_back(const_cast<Polynom<MonomType>*>((**i2).GetPolynom()));
            ++i2;
        }
        ReduceSet(toGroebner);
    }
    
    template <typename MonomType>
    void BooleanInvolutiveBasis<MonomType>::ConstructInvolutiveBasis()
    {
        typename TSet<MonomType>::Iterator tit(IntermediateBasis.Begin());
        Polynom<MonomType>* newNormalForm = 0;
        Triple<MonomType>* currentTriple = 0;

        while (!ProlongationsSet.Empty())
        {
            currentTriple = ProlongationsSet.Get();
            newNormalForm = NormalForm(currentTriple);
            /* As far as currentTriple can't be 0 (checked in QSET and TSET),
             * NormalForm can't return 0.
             */

            std::set<typename MonomType::Integer> currentNmpSet;
            const Triple<MonomType>* currentAncestor = 0;
            if (!newNormalForm->IsZero() && newNormalForm->Lm() == currentTriple->GetPolynomLm())
            {
                currentNmpSet = currentTriple->GetNmp();
                currentAncestor = currentTriple->GetAncestor();
                if (currentAncestor == currentTriple) 
                {
                    currentAncestor = 0;
                }
            }
            delete currentTriple;

            if (!newNormalForm->IsZero())
            {
                std::list<Triple<MonomType>*> newProlongations;
                tit = IntermediateBasis.Begin();

                while (tit != IntermediateBasis.End())
                {
                    if ((**tit).GetPolynomLm().IsTrueDivisibleBy(newNormalForm->Lm()))
                    {
                        ProlongationsSet.DeleteDescendants(*tit);
                        newProlongations.push_back(*tit);
                        tit = IntermediateBasis.Erase(tit);
                    }
                    else
                    {
                        ++tit;
                    }
                }

                IntermediateBasis.PushBack(new Triple<MonomType>(newNormalForm, currentAncestor, currentNmpSet, 0, -1));
                if (!newNormalForm->Degree())
                {
                    return;
                }

                IntermediateBasis.CollectNonMultiProlongations(--IntermediateBasis.End(), newProlongations);
                ProlongationsSet.Insert(newProlongations);
            }
            else
            {
                delete newNormalForm;
            }
        }
    }

}

#endif // BIBASIS_INVOLUTIVE_HPP
