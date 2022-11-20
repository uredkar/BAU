#pragma once
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <queue>
using namespace std;
#ifndef LIST_NODE
#define LIST_NODE
struct ListNode {
    int val;
    ListNode* next;
    ListNode() : val(0), next(nullptr) {}
    ListNode(int x) : val(x), next(nullptr) {}
    ListNode(int x, ListNode* next) : val(x), next(next) {}
    
};
#endif


class MergeKSortedLists
{
public:
    MergeKSortedLists() {
        
    }
    virtual void test()
    {
        case1();
        case2();
        case3();
    }
    void case1() {
        //[[1, 4, 5], [1, 3, 4], [2, 6]]
        vector<ListNode*> vec = {{ new ListNode(1,new ListNode(4,new ListNode(5))) },
                                 { new ListNode(1,new ListNode(3,new ListNode(4))) },
                                 { new ListNode(2,new ListNode(6)) }
                               };

        auto mergedVec = mergeKLists(vec);

        vector<int> result = { 1,1,2,3,4,4,5,6 };
        for (auto i:result)
        {
            _ASSERT(mergedVec->val == i);
            mergedVec = mergedVec->next;

        }

    }
    void case2() {
        vector<ListNode*> vec = {};
        auto mergedVec = mergeKLists(vec);
        _ASSERT(mergedVec == nullptr);
    }

    void case3() {
        vector<ListNode*> vec = { {} };
        auto mergedVec = mergeKLists(vec);
        _ASSERT(mergedVec == nullptr);
    }

    ListNode* add_new_node(ListNode*& tail, const ListNode* l1)
    {
        if (tail == nullptr)
            return tail;
        
        tail->next = new ListNode(l1->val);
        
        return tail->next;
    }

    ListNode* mergeTwoList(const ListNode* l1, const ListNode* l2)
    {
        ListNode* mergedVec = new ListNode(-44444); // add a head and remove it before return
        ListNode* tailMergedVec = mergedVec;
        int index = 0;
        
        while (l1 != nullptr || l2 != nullptr) {
            if (l1 != nullptr && l2 != nullptr)
            {
                if (l1->val < l2->val) {
                    tailMergedVec = add_new_node(tailMergedVec, l1);
                    l1 = l1->next;
                }
                else
                if (l1->val > l2->val){
                    tailMergedVec = add_new_node(tailMergedVec, l2);
                    l2 = l2->next;
                }
                else
                if (l1->val == l2->val) {
                    tailMergedVec = add_new_node(tailMergedVec, l1); // fix this as the head is not updated
                    tailMergedVec = add_new_node(tailMergedVec, l2);
                    l1 = l1->next;
                    l2 = l2->next;
                }
            }
            else
            if (l1 == nullptr && l2 != nullptr)
            {
                tailMergedVec = add_new_node(tailMergedVec, l2);
                l2 = l2->next;
            }
            else
            if (l1 != nullptr && l2 == nullptr)
            {
                tailMergedVec = add_new_node(tailMergedVec, l1);
                l1 = l1->next;
            }
            
            
        }
        
        auto head = mergedVec->next;
        delete mergedVec; // remove dummy head
        return head;
    }



    virtual ListNode* mergeKLists(vector<ListNode*>& lists) 
    {
        
        if (lists.size() == 0)
            return nullptr;
        ListNode* ml = *lists.begin();
        for (auto l = lists.begin()+1; l != lists.end();++l) {
            //auto temp = l;
            /*cout << "\n---------- input -----------";
            while (temp != nullptr) {
                cout << "\nval_l=" << temp->val;
                temp = temp->next;
            }*/
            ml = mergeTwoList(ml, *l);

            //temp = ml;
            /*cout << "\n----------output -----------";
            while (temp != nullptr) {
                cout << "\nval_ml=" << temp->val;
                temp = temp->next;
            }*/
        }
        /*auto temp = ml;
        while (temp != nullptr) {
            cout << "\nval_ml=" << temp->val;
            temp = temp->next;
        }*/
        return ml;

    }
};

class MergeKSortedLists_PriorityQ : public MergeKSortedLists {
public:
    MergeKSortedLists_PriorityQ() {
        
    }
    
    virtual void test() override {
        case1(); // calling parents test method and this could call the childs virtual function
        case2();
        case3();
    }
    struct Compare
    {
        bool operator()(ListNode* a, ListNode* b) {
            if (a->val > b->val)
                return true;
            else
                return false;
        }

    };

    virtual ListNode* mergeKLists(vector<ListNode*>& lists) override {

        priority_queue<ListNode*,vector<ListNode *>, Compare> pq;

        for (int i =0; i < lists.size(); i++) {
            if (lists[i] != nullptr) {
                pq.push(lists[i]); // push head node into the priority que
            }
        }
        ListNode* ml = new ListNode(-44444); // dummy head
        ListNode* tail = ml;

        while (pq.empty() == false)
        {
            auto curr = pq.top(); // get the smallest
            pq.pop();
            //cout << "\ncurr " << curr->val;
            tail->next = curr;
            tail = tail->next;
            if (curr->next != nullptr)
            {
                pq.push(curr->next);
            }

        }
        auto dummy = ml;
        ml = ml->next;
        delete dummy;
        return ml;

    }
};