/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef BIBASIS_TRIPLE_HPP
#define BIBASIS_TRIPLE_HPP

/**
 * @file bibasis/triple.hpp
 * @brief `BIBasis::Triple<MonomType>` --- (polynomial, ancestors, non-multiplicative variables) record driving Janet division.
 *
 * Declares and defines the bundle the involutive engine carries
 * around: a `Polynom<MonomType>` together with a cached pointer
 * to its leading monomial, the strong and weak `Ancestor` /
 * `WeakAncestor` links that record how this triple was derived
 * from earlier basis elements, the `Variable` index that
 * generated the prolongation (or `-1` for an initial input), and
 * the `Nmp` set of non-multiplicative variables Janet division
 * forbids multiplying by. Two constructors cover the two
 * lifecycle states --- an initial triple seeded from an input
 * polynomial vs. a derived triple created by a non-
 * multiplicative prolongation --- and the destructor owns the
 * polynomial pointer.
 *
 * `Compare(a, b)` returns true when `a`'s leading monomial is
 * greater than `b`'s, so `QSet` sorts triples by leading
 * monomial and the algorithm's `back()`-popping loop processes
 * the lightest pending prolongation first; `SetNmp` / `TestNmp`
 * are the bookkeeping the algorithm uses to remember which
 * variables are still eligible for prolongation at each step.
 *
 * @see polynom.hpp
 * @see janettree.hpp
 * @see tset.hpp
 * @see qset.hpp
 * @see involutive.hpp
 */

#include <set>
#include <string>
#include "allocator.hpp"
#include "polynom.hpp"
#include "error.h"

namespace BIBasis
{
    template <typename MonomType>
    class Triple 
    {
    private:
        const MonomType* Lm;
        Polynom<MonomType>* Polynomial;
        const Triple* Ancestor;
        const Triple* WeakAncestor;
        std::set<typename MonomType::Integer> Nmp;
        const typename MonomType::Integer Variable;

    public:
        Triple(Polynom<MonomType>* initialPolynom);

        Triple(Polynom<MonomType>* initialPolynom
             , const Triple* initialAncestor
             , const std::set<typename MonomType::Integer>& initialNmp
             , const Triple* weakAncestor
             , typename MonomType::Integer nmVar);

        ~Triple();

        const Polynom<MonomType>* GetPolynom() const;
        const MonomType& GetPolynomLm() const;
        const Triple* GetAncestor() const;
        const Triple* GetWeakAncestor() const;
        typename MonomType::Integer GetVariable() const;
        const std::set<typename MonomType::Integer>& GetNmp() const;

        void SetNmp(const std::set<typename MonomType::Integer>& newNmp);
        void SetNmp(typename MonomType::Integer variable);
        bool TestNmp(typename MonomType::Integer variable) const;

        static bool Compare(const Triple* tripleA, const Triple* tripleB);
    };

    template <typename MonomType>
    Triple<MonomType>::Triple(Polynom<MonomType>* initialPolynom)
        : Lm(0)
        , Polynomial(initialPolynom)
        , Ancestor(0)
        , WeakAncestor(0)
        , Nmp()
        , Variable(-1)
    {
        if (Polynomial)
        {
            Lm = &Polynomial->Lm();
            Ancestor = this;
            Nmp = std::set<typename MonomType::Integer>();
        }
    }

    template <typename MonomType>
    Triple<MonomType>::Triple(Polynom<MonomType>* initialPolynom
                            , const Triple<MonomType>* initialAncestor
                            , const std::set<typename MonomType::Integer>& initialNmp
                            , const Triple<MonomType>* weakAncestor
                            , typename MonomType::Integer nmVar)
        : Lm(0)
        , Polynomial(0)
        , Ancestor(0)
        , WeakAncestor(0)
        , Nmp()
        , Variable(nmVar)
    {
        if (initialPolynom)
        {
            if (Variable == -1)
            {
                Polynomial = initialPolynom;
                if (initialAncestor)
                {
                    Ancestor = initialAncestor;
                }
                else
                {
                    Ancestor = this;
                }
            }
            else
            {
                Polynomial = new Polynom<MonomType>();
                (*Polynomial) += initialPolynom->Lm();

                Ancestor = initialAncestor;
                WeakAncestor = weakAncestor;
            }

            Lm = &Polynomial->Lm();
            Nmp = initialNmp;
        }
    }

    template <typename MonomType>
    Triple<MonomType>::~Triple() 
    {
        delete Polynomial;
    }

    template <typename MonomType>
    const Polynom<MonomType>* Triple<MonomType>::GetPolynom() const
    {
        return Polynomial;
    }

    template <typename MonomType>
    const MonomType& Triple<MonomType>::GetPolynomLm() const 
    { 
        return *Lm;
    }

    template <typename MonomType>
    const Triple<MonomType>* Triple<MonomType>::GetAncestor() const 
    { 
        return Ancestor;
    }

    template <typename MonomType>
    const Triple<MonomType>* Triple<MonomType>::GetWeakAncestor() const 
    { 
        return WeakAncestor;
    }

    template <typename MonomType>
    typename MonomType::Integer Triple<MonomType>::GetVariable() const 
    { 
        return Variable;
    }

    template <typename MonomType>
    const std::set<typename MonomType::Integer>& Triple<MonomType>::GetNmp() const 
    { 
        return Nmp;
    }

    template <typename MonomType>
    void Triple<MonomType>::SetNmp(const std::set<typename MonomType::Integer>& newNmp) 
    { 
        Nmp = newNmp; 
    }

    template <typename MonomType>
    void Triple<MonomType>::SetNmp(typename MonomType::Integer var) 
    { 
        Nmp.insert(var);
    }

    template <typename MonomType>
    bool Triple<MonomType>::TestNmp(typename MonomType::Integer var) const
    {
        return Nmp.count(var);
    }

    template <typename MonomType>
    bool Triple<MonomType>::Compare(const Triple<MonomType>* tripleA, const Triple<MonomType>* tripleB)
    {
        if (!tripleA || !tripleB)
        {
            throw std::string("BIBasis::Triple::Compare(): at least one argument is NULL.");
        }
        return *tripleA->Lm > *tripleB->Lm;
    }
}

#endif // BIBASIS_TRIPLE_HPP
