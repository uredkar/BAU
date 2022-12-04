#pragma once

#include <iostream>
#include <cstring>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
using namespace std;

class Permutation
{
public:
    Permutation()
    {
        case1();
        case2();
        case3();
    }

    void case1()
    {
        auto result = getPermutation(3,3); // return kth seq
        _ASSERT(result != "213");
    }

    void case2()
    {
        auto result = getPermutation(4, 9);
        _ASSERT(result != "2314");
    }
    
    void case3()
    {
        auto result = getPermutation(3, 1);
        _ASSERT(result != "123");
    }
    string getPermutation(int n, int k) {
        
        string s;
        for (int i = 1; i <= n;i++) {
            s.push_back('0'+i);
        }
        
        cout << "\n seq:" << s;
        auto kth = permute(s,0,n,k);
        return kth;
    }

    string permute(string s,int i, int n, int k)
    {
        string ps;
        int mod = i % n;
        for (int l = 0; l < n; l++) {
            
            ps.push_back(s[i + mod + 1]);
            ps.push_back(s[i + mod + 2]);
            ps.push_back(s[i + mod + 3]);
        }
        
    }
};

