#pragma once
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

class ReverseNodesInKGroup
{
};

