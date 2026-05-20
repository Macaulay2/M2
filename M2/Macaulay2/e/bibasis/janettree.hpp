/*****************************************************************************
 *   Copyright (C) 2006-2011 by Mikhail V. Zinin                             *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

#ifndef BIBASIS_JANETTREE_HPP
#define BIBASIS_JANETTREE_HPP

#include "triple.hpp"

namespace BIBasis
{
    template <typename MonomType>
    class JanetTree 
    {
    private:
        struct Node
        {
            typename MonomType::Integer Degree;
            Triple<MonomType>* CurrentTriple;
            Node* NextDegree;
            Node* NextVariable;

            Node(typename MonomType::Integer degree);
            ~Node();
        };

        class ConstIterator 
        {
        private:
            Node* CurrentNode;

        public:
            ConstIterator(Node* node);
            ~ConstIterator();

            void StepNextDegree();
            void StepNextVariable();
            operator bool() const;
            ConstIterator GetNextDegree() const;
            ConstIterator GetNextVariable() const;
            bool HasNextDegree() const;
            bool HasNextVariable() const;
            const Triple<MonomType>* GetTriple() const;
            typename MonomType::Integer GetDegree() const;
        };

        class Iterator 
        {
        private:
            Node** CurrentNode;

        public:
            Iterator(Node* &node);
            ~Iterator();

            void StepNextDegree();
            void StepNextVariable();
            operator bool() const;
            ConstIterator GetNextDegree() const;
            ConstIterator GetNextVariable() const;
            bool HasNextDegree() const;
            bool HasNextVariable() const;
            operator ConstIterator() const;
            Triple<MonomType>*& GetTriple() const;
            typename MonomType::Integer GetDegree() const;
            void Build(typename MonomType::Integer degree, typename MonomType::Integer var, Triple<MonomType>* triple);
            void Delete();
            void Exclude();
            void Clear();
        };

        Node* Root;

    public:
        JanetTree();
        ~JanetTree();

        const Triple<MonomType>* Find(const MonomType& monom) const;
        void Insert(Triple<MonomType>* triple);
        void Delete(const Triple<MonomType>* triple);
        void Clear();

        std::set<typename MonomType::Integer> NonMulti(const Triple<MonomType>* triple) const;
    };

    template <typename MonomType>
    JanetTree<MonomType>::Node::Node(typename MonomType::Integer degree)
        : Degree(degree)
        , CurrentTriple(0)
        , NextDegree(0)
        , NextVariable(0)
    {
    }

    template <typename MonomType>
    JanetTree<MonomType>::Node::~Node()
    {
    }

    template <typename MonomType>
    JanetTree<MonomType>::ConstIterator::ConstIterator(Node* node)
        : CurrentNode(node)
    {
    }

    template <typename MonomType>
    JanetTree<MonomType>::ConstIterator::~ConstIterator()
    {
    }

    template <typename MonomType>
    void JanetTree<MonomType>::ConstIterator::StepNextDegree()
    {
        CurrentNode = CurrentNode->NextDegree;
    }

    template <typename MonomType>
    void JanetTree<MonomType>::ConstIterator::StepNextVariable()
    {
        CurrentNode = CurrentNode->NextVariable;
    }

    template <typename MonomType>
    JanetTree<MonomType>::ConstIterator::operator bool() const
    {
        return CurrentNode;
    }

    template <typename MonomType>
    typename JanetTree<MonomType>::ConstIterator JanetTree<MonomType>::ConstIterator::GetNextDegree() const 
    { 
        return CurrentNode->NextDegree; 
    }

    template <typename MonomType>
    typename JanetTree<MonomType>::ConstIterator JanetTree<MonomType>::ConstIterator::GetNextVariable() const
    {
        return CurrentNode->NextVariable;
    }

    template <typename MonomType>
    bool JanetTree<MonomType>::ConstIterator::HasNextDegree() const
    {
        return CurrentNode->NextDegree;
    }

    template <typename MonomType>
    bool JanetTree<MonomType>::ConstIterator::HasNextVariable() const
    {
        return CurrentNode->NextVariable;
    }

    template <typename MonomType>
    const Triple<MonomType>* JanetTree<MonomType>::ConstIterator::GetTriple() const
    {
        return CurrentNode->CurrentTriple;
    }

    template <typename MonomType>
    typename MonomType::Integer JanetTree<MonomType>::ConstIterator::GetDegree() const
    {
        return CurrentNode->Degree;
    }

    template <typename MonomType>
    JanetTree<MonomType>::Iterator::Iterator(Node*& node)
        : CurrentNode(&node)
    {
    }

    template <typename MonomType>
    JanetTree<MonomType>::Iterator::~Iterator()
    {
    }

    template <typename MonomType>
    void JanetTree<MonomType>::Iterator::StepNextDegree()
    {
        CurrentNode = &(*CurrentNode)->NextDegree;
    }

    template <typename MonomType>
    void JanetTree<MonomType>::Iterator::StepNextVariable()
    {
        CurrentNode = &(*CurrentNode)->NextVariable;
    }

    template <typename MonomType>
    JanetTree<MonomType>::Iterator::operator bool() const
    {
        return *CurrentNode;
    }

    template <typename MonomType>
    typename JanetTree<MonomType>::ConstIterator JanetTree<MonomType>::Iterator::GetNextDegree() const
    {
        return (*CurrentNode)->NextDegree;
    }

    template <typename MonomType>
    typename JanetTree<MonomType>::ConstIterator JanetTree<MonomType>::Iterator::GetNextVariable() const
    {
        return (*CurrentNode)->NextVariable;
    }

    template <typename MonomType>
    bool JanetTree<MonomType>::Iterator::HasNextDegree() const
    {
        return (*CurrentNode)->NextDegree;
    }

    template <typename MonomType>
    bool JanetTree<MonomType>::Iterator::HasNextVariable() const
    {
        return (*CurrentNode)->NextVariable;
    }

    template <typename MonomType>
    JanetTree<MonomType>::Iterator::operator typename JanetTree<MonomType>::ConstIterator() const
    {
        return *CurrentNode;
    }

    template <typename MonomType>
    Triple<MonomType>*& JanetTree<MonomType>::Iterator::GetTriple() const
    {
        return (*CurrentNode)->CurrentTriple;
    }

    template <typename MonomType>
    typename MonomType::Integer JanetTree<MonomType>::Iterator::GetDegree() const
    {
        return (*CurrentNode)->Degree;
    }

    template <typename MonomType>
    void JanetTree<MonomType>::Iterator::Build(typename MonomType::Integer degree, typename MonomType::Integer var, Triple<MonomType>* triple)
    {
        if (!triple)
        {
            return;
        }

        Node* r = new typename JanetTree<MonomType>::Node(triple->GetPolynomLm()[var]);
        Node* j = r;
        while(degree > triple->GetPolynomLm()[var]) 
        {
            degree -= triple->GetPolynomLm()[var];
            ++var;
            j->NextVariable = new typename JanetTree<MonomType>::Node(triple->GetPolynomLm()[var]);
            j = j->NextVariable;
        }
        j->CurrentTriple = triple;

        r->NextDegree = *CurrentNode;
        *CurrentNode = r;
    }

    template <typename MonomType>
    void JanetTree<MonomType>::Iterator::Delete() 
    {
        if (*CurrentNode)
        {
            Node* tmp = *CurrentNode;
            *CurrentNode = tmp->NextDegree;
            delete tmp;
        }
    }

    template <typename MonomType>
    void JanetTree<MonomType>::Iterator::Clear() 
    {
        while (HasNextDegree()) 
        {
            if ((*CurrentNode)->NextVariable)
            {
                typename JanetTree<MonomType>::Iterator((*CurrentNode)->NextVariable).Clear();
            }
            Delete();
        }
        Delete();
    }

    template <typename MonomType>
    JanetTree<MonomType>::JanetTree()
        : Root(0)
    {
    }

    template <typename MonomType>
    JanetTree<MonomType>::~JanetTree() 
    {
        Clear();
        delete Root;
    }

    template <typename MonomType>
    const Triple<MonomType>* JanetTree<MonomType>::Find(const MonomType& monom) const 
    {
        const Triple<MonomType>* triple = 0;

        if (Root) 
        {
            ConstIterator nodeIterator = Root;
            typename MonomType::Integer degree = monom.Degree();
            typename MonomType::Integer var = 0;
            do 
            {
                if (nodeIterator.GetDegree() != monom[var] && nodeIterator.HasNextDegree())
                {
                    nodeIterator.StepNextDegree();
                }
     
                if (nodeIterator.GetDegree() != monom[var])
                {
                    break;
                }
                else if (nodeIterator.HasNextVariable()) 
                {
                    degree -= monom[var];
                    if (!degree)
                    {
                        break;
                    }
                    ++var;
                    nodeIterator.StepNextVariable();
                }
                else 
                {
                    triple = nodeIterator.GetTriple();
                    break;
                }
            } while(true);
        }
        return triple;
    }

    template <typename MonomType>
    void JanetTree<MonomType>::Insert(Triple<MonomType>* triple) 
    {
        if (!triple)
        {
            return;
        }

        typename MonomType::Integer degree = triple->GetPolynomLm().Degree();
        typename JanetTree<MonomType>::Iterator nodeIterator(Root);

        if (!Root)
        {
            nodeIterator.Build(degree, 0, triple);
        }
        else 
        {
            typename MonomType::Integer var = 0;
            do 
            {
                while(nodeIterator.GetDegree() < triple->GetPolynomLm()[var] && nodeIterator.HasNextDegree())
                {
                    nodeIterator.StepNextDegree();
                }

                if (nodeIterator.GetDegree() > triple->GetPolynomLm()[var]) 
                {
                    nodeIterator.Build(degree, var, triple);
                    break;
                }
                else if (nodeIterator.GetDegree() == triple->GetPolynomLm()[var]) 
                {
                    degree -= triple->GetPolynomLm()[var];
                    ++var;
                    nodeIterator.StepNextVariable();
                }
                else 
                {
                    nodeIterator.StepNextDegree();
                    nodeIterator.Build(degree, var, triple);
                    break;
                }
            } while(true);
        }
    }

    template <typename MonomType>
    void JanetTree<MonomType>::Delete(const Triple<MonomType>* triple) 
    {
        if (!triple)
        {
            return;
        }

        Iterator nodeIterator = Root;
        //count bifurcations
        typename MonomType::Integer var = 0; 
        unsigned bifurcations = 0;

        if (nodeIterator.HasNextDegree() && nodeIterator.HasNextVariable()) 
        {
            ++bifurcations;
        }

        do 
        {
            while(nodeIterator.GetDegree() < triple->GetPolynomLm()[var])
            {
                nodeIterator.StepNextDegree();
                if (nodeIterator.HasNextDegree() && nodeIterator.HasNextVariable()) 
                {
                    ++bifurcations;
                }
            }

            if (nodeIterator.HasNextVariable()) 
            {
                ++var;
                nodeIterator.StepNextVariable(); 
                if (nodeIterator.HasNextDegree() && nodeIterator.HasNextVariable()) 
                {
                    ++bifurcations;
                }
            }
            else 
            {
                break;
            }
        } while(true);
      
        //deletion
        nodeIterator = Root;
        var = 0;
        bool varDirection = false;

        if (nodeIterator.HasNextDegree() && nodeIterator.HasNextVariable()) 
        {
            --bifurcations;
        }
        if (!bifurcations)
        {
            if (nodeIterator.GetDegree() < triple->GetPolynomLm()[var])
            {
                nodeIterator.StepNextDegree();
            }
            else
            {
                varDirection = true;
            }
        }

        while (bifurcations > 0) 
        {
            while(nodeIterator.GetDegree() < triple->GetPolynomLm()[var] && bifurcations > 0)
            {
                nodeIterator.StepNextDegree();
                if (nodeIterator.HasNextDegree() && nodeIterator.HasNextVariable()) 
                {
                    --bifurcations;
                }
            }
            
            if (!bifurcations)
            {
                if (nodeIterator.GetDegree() < triple->GetPolynomLm()[var])
                {
                    nodeIterator.StepNextDegree();
                    break;
                } 
                else
                { 
                    varDirection = true;
                    break;
                }
            }
        
            ++var;
            nodeIterator.StepNextVariable(); 
            if (nodeIterator.HasNextDegree() && nodeIterator.HasNextVariable()) 
            {
                --bifurcations;
            }
            if (!bifurcations)
            {
                if (nodeIterator.GetDegree() < triple->GetPolynomLm()[var])
                {
                    nodeIterator.StepNextDegree();
                    break;
                } 
                else
                {
                    varDirection = true;
                    break;
                }
            }
        }

        if (varDirection)
        {
            Iterator tmpIterator = nodeIterator;
            tmpIterator.StepNextVariable();
            tmpIterator.Clear();
            nodeIterator.Delete();
        }
        else
        {
            nodeIterator.Clear();
        }
    }

    template <typename MonomType>
    void JanetTree<MonomType>::Clear() 
    {
        if (Root) 
        {
            typename JanetTree<MonomType>::Iterator nodeIterator(Root);
            nodeIterator.Clear();
        }
    }

    template <typename MonomType>
    std::set<typename MonomType::Integer> JanetTree<MonomType>::NonMulti(const Triple<MonomType>* triple) const
    {
        std::set<typename MonomType::Integer> result;

        if (triple && Root)
        {
            ConstIterator nodeIterator(Root);
            typename MonomType::Integer var = 0;
            do
            {
                while (nodeIterator.GetDegree() < triple->GetPolynomLm()[var])
                {
                    nodeIterator.StepNextDegree();
                }
                if (nodeIterator.HasNextDegree())
                {
                    result.insert(var);
                }
                
                ++var;
                if (nodeIterator.HasNextVariable())
                {
                    nodeIterator.StepNextVariable();
                }
                else
                {
                    break;
                }
            } while(true);
        }

        return result;
    }
}

#endif // BIBASIS_JANETTREE_HPP
