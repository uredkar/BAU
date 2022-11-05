#pragma once


#include <iostream>

struct ListNode {
    int val;
    ListNode* next;
    ListNode() : val(0), next(nullptr) {}
    ListNode(int x) : val(x), next(nullptr) {}
    ListNode(int x, ListNode* next) : val(x), next(next) {}

};

class AddTwoNumbers {
public:
    AddTwoNumbers() {
        case1();
        case2();
        case3();
    }
    void case1()
    {
        ListNode* l1_head = NULL;
        ListNode* l1_tail = NULL;
        ListNode* l2_head = NULL;
        ListNode* l2_tail = NULL;

        for (int n : {2, 4, 3})
        {
            create_link_list(n, l1_head, l1_tail);

        }
        display_listnode(l1_head);
        for (int n : {5, 6, 4})
        {
            create_link_list(n, l2_head, l2_tail);

        }
        display_listnode(l2_head);
        auto result = addTwoNumbers(l1_head, l2_head);
        display_listnode(result);

        delete_link_list(result);
        delete_link_list(l1_head);
        delete_link_list(l2_head);

    }
    void case2()
    {
        ListNode* l1_head = NULL;
        ListNode* l1_tail = NULL;
        ListNode* l2_head = NULL;
        ListNode* l2_tail = NULL;

        for (int n : {0})
        {
            create_link_list(n, l1_head, l1_tail);

        }
        display_listnode(l1_head);
        for (int n : {0})
        {
            create_link_list(n, l2_head, l2_tail);

        }
        display_listnode(l2_head);
        auto result = addTwoNumbers(l1_head, l2_head);
        display_listnode(result);

        delete_link_list(result);
        delete_link_list(l1_head);
        delete_link_list(l2_head);
    }
    void case3()
    {
        ListNode* l1_head = NULL;
        ListNode* l1_tail = NULL;
        ListNode* l2_head = NULL;
        ListNode* l2_tail = NULL;

        for (int n : { 9, 9, 9, 9, 9, 9, 9 })
        {
            create_link_list(n, l1_head, l1_tail);

        }
        display_listnode(l1_head);
        for (int n : { 9, 9, 9, 9 })
        {
            create_link_list(n, l2_head, l2_tail);

        }
        display_listnode(l2_head);
        auto result = addTwoNumbers(l1_head, l2_head);
        display_listnode(result);

        delete_link_list(result);
        delete_link_list(l1_head);
        delete_link_list(l2_head);
    }
    void delete_link_list(ListNode* head)
    {
        while (head)
        {
            auto temp = head->next;
            delete head;
            head = temp;
        }
    }
    void create_link_list(int n, ListNode*& l1_head, ListNode*& l1_tail)
    {

        ListNode* temp = new ListNode(n);
        if (l1_head == NULL)
        {
            l1_head = temp;
            l1_tail = temp;
        }
        else {
            l1_tail->next = temp;
            l1_tail = temp;
        }
    }
    void display_listnode(ListNode* head)
    {
        std::cout << "\ndisplay list node";
        while (head)
        {
            std::cout << "\nval=" << head->val;
            head = head->next;
        }
        std::cout << "\nend list";
    }

    void add_number_to_llist(ListNode** head, ListNode** tail, int number)
    {
        if (*head == NULL)
        {
            *head = new ListNode(number);
            *tail = *head;
            (*head)->next = NULL;
        }
        else {
            (*tail)->next = new ListNode(number);
            *tail = (*tail)->next;
        }
    }

    ListNode* addTwoNumbers(ListNode* l1, ListNode* l2) {
        int carry = 0;

        ListNode* result_head = NULL;
        ListNode* result_tail = NULL;
        int number = 0;
        while (l1 || l2)
        {

            if (l1 && l2) {
                auto sum = (l1->val + l2->val + carry);
                //std::cout << "\nsum " << sum;
                number = calc_number_and_carry(sum, carry);
                add_number_to_llist(&result_head, &result_tail, number);
                l1 = l1->next;
                l2 = l2->next;
            }
            else if (l1)
            {
                auto sum = l1->val + carry;
                //std::cout << "\nsum " << sum;
                number = calc_number_and_carry(sum, carry);
                add_number_to_llist(&result_head, &result_tail, number);
                l1 = l1->next;

            }
            else if (l2)
            {
                auto sum = l2->val + carry;
                //std::cout << "\nsum " << sum;
                number = calc_number_and_carry(sum, carry);
                add_number_to_llist(&result_head, &result_tail, number);
                l2 = l2->next;
            }
        }
        if (carry > 0) {
            number = carry;
            add_number_to_llist(&result_head, &result_tail, number);
        }
        return result_head;
    }


    int calc_number_and_carry(int sum, int& carry)
    {
        int number = 0;
        if (sum < 10) {
            carry = 0;
            number = sum;
        }
        else {
            carry = sum / 10;
            number = sum % 10;
        }
        //std::cout << "\nnumber is " << number;
        //std::cout << "\ncarry " << carry;
        return number;
    }

};


