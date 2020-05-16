/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef BIBASIS_TSET_HPP
#define BIBASIS_TSET_HPP

#include <list>
#include <algorithm>
#include "janettree.hpp"

namespace BIBasis
{
    template <typename MonomType>
    class TSet
    {
    private:
        std::list<Triple<MonomType>*> TripleList;
        JanetTree<MonomType> JTree;

    public:
        typedef typename std::list<Triple<MonomType>*>::iterator Iterator;
        typedef typename std::list<Triple<MonomType>*>::const_iterator ConstIterator;

        TSet();
        ~TSet();

        Iterator Begin();
        ConstIterator Begin() const;
        Iterator End();
        ConstIterator End() const;

        void Clear();
        Iterator Erase(Iterator it);
        void PushBack(Triple<MonomType>* newTriple);
        std::size_t Size() const;

        const Triple<MonomType>* Find(const MonomType& monom) const;
        Triple<MonomType>* const Back() const;
        
        void CollectNonMultiProlongations(Iterator& iterator, std::list<Triple<MonomType>*>& set);
    };

    template <typename MonomType>
    TSet<MonomType>::TSet()
        : TripleList()
        , JTree()
    {
    }

    template <typename MonomType>
    TSet<MonomType>::~TSet()
    {
        Clear();
    }

    template <typename MonomType>
    typename TSet<MonomType>::Iterator TSet<MonomType>::Begin()
    { 
        return TripleList.begin();
    }

    template <typename MonomType>
    typename TSet<MonomType>::ConstIterator TSet<MonomType>::Begin() const
    { 
        return TripleList.begin();
    }

    template <typename MonomType>
    typename TSet<MonomType>::Iterator TSet<MonomType>::End()
    { 
        return TripleList.end();
    }

    template <typename MonomType>
    typename TSet<MonomType>::ConstIterator TSet<MonomType>::End() const 
    { 
        return TripleList.end(); 
    }

    template <typename MonomType>
    void TSet<MonomType>::Clear()
    {
        JTree.Clear();

        Iterator it = TripleList.begin();
        while (it != TripleList.end())
        {
            delete *it;
            ++it;
        }
        TripleList.clear();
    }

    template <typename MonomType>
    typename TSet<MonomType>::Iterator TSet<MonomType>::Erase(typename TSet<MonomType>::Iterator it)
    {
        JTree.Delete(*it);
        return TripleList.erase(it);
    }

    template <typename MonomType>
    void TSet<MonomType>::PushBack(Triple<MonomType>* newTriple)
    {
        TripleList.push_back(newTriple);
        JTree.Insert(newTriple);
    }

    template <typename MonomType>
    std::size_t TSet<MonomType>::Size() const 
    { 
        return TripleList.size();
    }

    template <typename MonomType>
    const Triple<MonomType>* TSet<MonomType>::Find(const MonomType& monom) const 
    { 
        return JTree.Find(monom);
    }

    template <typename MonomType>
    Triple<MonomType>* const TSet<MonomType>::Back() const 
    { 
        return TripleList.back();
    }

    template <typename MonomType>
    void TSet<MonomType>::CollectNonMultiProlongations(typename TSet<MonomType>::Iterator& iterator, std::list<Triple<MonomType>*>& set)
    {
        if (iterator == TripleList.end() || !(*iterator))
        {
            return;
        }

        typename MonomType::Integer firstMultiVar = (**iterator).GetPolynomLm().FirstMultiVar();
        for (typename MonomType::Integer var = 0; var < firstMultiVar; ++var)
        {
            if (!(**iterator).TestNmp(var))
            {
                Polynom<MonomType>* tmpPolynom = new Polynom<MonomType>(*(**iterator).GetPolynom());
                (*tmpPolynom) *= var;

                (**iterator).SetNmp(var);

                if (!tmpPolynom->IsZero())
                {
                    set.push_back(new Triple<MonomType>(tmpPolynom
                                                      , (**iterator).GetAncestor()
                                                      , (**iterator).GetNmp()
                                                      , (*iterator)
                                                      , var)
                                 );
                }
                delete tmpPolynom;
            }
        }
    }
}

#endif // BIBASIS_TSET_HPP
