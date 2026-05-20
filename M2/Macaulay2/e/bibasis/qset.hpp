/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef BIBASIS_QSET_HPP
#define BIBASIS_QSET_HPP

/**
 * @file bibasis/qset.hpp
 * @brief `BIBasis::QSet<MonomType>` --- prolongation work-queue sorted by leading monomial.
 *
 * Declares and defines the pending-work container that drives
 * the involutive engine's outer loop. Holds an owned
 * `std::list<Triple*>` kept sorted under `Triple::Compare`
 * (which returns `*a->Lm > *b->Lm`, so STL sort lays the list
 * out in descending leading-monomial order); `Get()` pops
 * `back()`, returning the triple with the smallest leading
 * monomial --- the standard normal-strategy choice that keeps
 * the working basis small. Two `Insert` overloads cover the
 * two flavours of incoming work: one takes a polynomial list,
 * wraps each entry in a fresh `Triple`, and re-sorts the
 * whole queue (`std::list::sort`); the other takes a
 * pre-built triple list, sorts it, and `std::list::merge`s it
 * into the queue. `DeleteDescendants(ancestor)` walks the
 * queue and discards every triple whose recorded `Ancestor`
 * **or** `WeakAncestor` matches; the driver calls this
 * whenever a new basis element makes a chunk of scheduled work
 * obsolete.
 *
 * `Empty()` is the algorithm's termination test: when no
 * prolongations remain, the intermediate basis stored in `TSet`
 * is involutive.
 *
 * @see triple.hpp
 * @see tset.hpp
 * @see involutive.hpp
 */

#include <list>
#include <algorithm>
#include "triple.hpp"

namespace BIBasis
{
    template <typename MonomType>
    class QSet
    {
    private:
        std::list<Triple<MonomType>*> TripleList;

    public:
        QSet();
        QSet(const std::list<Polynom<MonomType>*>& basis);
        ~QSet();

        void Insert(std::list<Polynom<MonomType>*>& addList);
        void Insert(std::list<Triple<MonomType>*>& addList);

        Triple<MonomType>* Get();

        void Clear();
        bool Empty() const;
        std::size_t Size() const;
        void DeleteDescendants(const Triple<MonomType>* ancestor);
    };

    template <typename MonomType>
    QSet<MonomType>::QSet()
        : TripleList()
    {
    }

    template <typename MonomType>
    QSet<MonomType>::QSet(const std::list<Polynom<MonomType>*>& basis)
        : TripleList()
    {
        typename std::list<Polynom<MonomType>*>::const_iterator itBasis(basis.begin());
        while (itBasis != basis.end())
        {
            if (*itBasis)
            {
                TripleList.push_back(new Triple<MonomType>(*itBasis));
            }
            ++itBasis;
        }
        TripleList.sort(Triple<MonomType>::Compare);
    }

    template <typename MonomType>
    QSet<MonomType>::~QSet()
    {
        Clear();
    }

    template <typename MonomType>
    void QSet<MonomType>::Insert(std::list<Polynom<MonomType>*>& addList)
    {
        typename std::list<Polynom<MonomType>*>::const_iterator itBasis(addList.begin());
        while ( itBasis != addList.end() )
        {
            if (*itBasis)
            {
                TripleList.push_back(new Triple<MonomType>(*itBasis));
            }
            ++itBasis;
        }
        TripleList.sort(Triple<MonomType>::Compare);
    }

    template <typename MonomType>
    void QSet<MonomType>::Insert(std::list<Triple<MonomType>*>& addList)
    {
        addList.sort(Triple<MonomType>::Compare);
        TripleList.merge(addList, Triple<MonomType>::Compare);
    }

    template <typename MonomType>
    Triple<MonomType>* QSet<MonomType>::Get()
    {
        Triple<MonomType>* result = TripleList.back();
        TripleList.pop_back();
        return result;
    }

    template <typename MonomType>
    void QSet<MonomType>::Clear()
    {
        typename std::list<Triple<MonomType>*>::iterator it(TripleList.begin());
        while (it != TripleList.end())
        {
            delete *it;
            ++it;
        }
        TripleList.clear();
    }

    template <typename MonomType>
    inline bool QSet<MonomType>::Empty() const
    {
        return TripleList.empty();
    }

    template <typename MonomType>
    inline std::size_t QSet<MonomType>::Size() const
    {
        return TripleList.size();
    }

    template <typename MonomType>
    void QSet<MonomType>::DeleteDescendants(const Triple<MonomType>* ancestor)
    {
        if (!ancestor)
        {
            return;
        }

        typename std::list<Triple<MonomType>*>::iterator it(TripleList.begin());
        while ( it != TripleList.end() )
        {
            if ((**it).GetAncestor() == ancestor || (**it).GetWeakAncestor() == ancestor)
            {
                delete *it;
                it = TripleList.erase(it);
            }
            else
            {
                ++it;
            }
        }
    }
}

#endif // BIBASIS_QSET_HPP
