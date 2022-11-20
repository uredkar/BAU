#pragma once

#include <iostream>
#include <cstring>
#include <string>
#include <vector>
#include <map>
using namespace std;
class RegularExpressionMatching
{
public:
    RegularExpressionMatching()
    {
       /* case1();
        case2();
        case3();
        case4();*/
     
        case5();
    }

    void case1()
    {
        cout << "\ncase1";
        _ASSERT(isMatch("aa", "a") == false);

    }

    void case2()
    {
        cout << "\ncase2";
        _ASSERT(isMatch("aa", "a*") == true);

    }

    void case3()
    {
        cout << "\ncase3";
        _ASSERT(isMatch("ab", ".*") == true);

    }

    void case4()
    {
        cout << "\ncase4";
        // even though c is not preset in the input
        // match is any pattern so *a matches *b matches
        _ASSERT(isMatch("aab", "c*a*b") == true);
    }
   
    void case5()
    {
        cout << "\ncase5";
        // even though c is not preset in the input
        // match is any pattern so *a matches *b matches
        _ASSERT(isMatch("aaa", "a*a") == true);
    }
    
    // https://github.com/keineahnung2345/leetcode-cpp-practices/blob/master/10.%20Regular%20Expression%20Matching.cpp
    // learning from the above code
    bool isMatch(string s, string p) {

        cout << "\nisMatch " << s << " p " << p;

        if (p.size() == 0) {

            if (s.size() == 0) {
                cout << "\np and s are zero size";
                return true;
            }
            else {
                return false;
            }
        }


        bool first_match = (s.size() > 0) && (p[0] == '.' || p[0] == s[0]); // at least one s and pattern is . or pattern matches s
        cout << "\nfirst match input > 0 and first pattern is . or second string match each other=" << first_match;
        if (p.size() >= 2 && p[1] == '*') {
            cout << "\npattern is > 2 and second is *";
            cout << "\nif first match is true then match with pattern starting 2rd position";
            cout << "\nif first match is false then match with pattern starting 3rd position";
            return (first_match == true && isMatch(s.substr(1), p)) || isMatch(s, p.substr(2));
            
            
        }
        else {
            cout << "\npattern is < 2 || first second is NOT * then take substring from 1 to rest";
            bool matched = first_match && isMatch(s.substr(1), p.substr(1));
            return matched;
        }
    }
};

