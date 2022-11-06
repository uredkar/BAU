#pragma once
#include <iostream>
#include <string>
#include <vector>
#include <map>
// this seems like to find a substring that has no duplicate then find the longest of those
class LongestSubstringWithoutRepeatingChars
{
public:
    LongestSubstringWithoutRepeatingChars()
    {
        case1();
        case2();
        case3();
        case4();
        case5();
        case6();
        case7();
    }
    void case1()
    {
        auto longest_length = lengthOfLongestSubstring("abcabcbb");
        _ASSERT(longest_length == 3);
    }
    void case2()
    {
        auto longest_length = lengthOfLongestSubstring("bbbbb");
        _ASSERT(longest_length == 1);
    }
    void case3()
    {
        auto longest_length = lengthOfLongestSubstring("pwwkew");
        _ASSERT(longest_length == 3);
    }

    void case4()
    {
        auto longest_length = lengthOfLongestSubstring(" ");
        _ASSERT(longest_length == 1);
    }
    void case5()
    {
        auto longest_length = lengthOfLongestSubstring("dvdf");
        _ASSERT(longest_length == 3);
    }
    void case6()
    {
        auto longest_length = lengthOfLongestSubstring("qrsvbspk");
        _ASSERT(longest_length == 5);
    }
    void case7()
    {
        auto longest_length = lengthOfLongestSubstring("yfsrsrpzuya");
        _ASSERT(longest_length == 7);
    }
    /* from other web
    int lengthOfLongestSubstring(std::string s) {
        std::vector<int> dict(256, -1);
        int maxLen = 0, start = -1;
        for (int i = 0; i != s.length(); i++) {
            if (dict[s[i]] > start)
                start = dict[s[i]];
            dict[s[i]] = i;
            maxLen = std::max(maxLen, i - start);
        }
        return maxLen;
    }
    */

   


    void find_longestSubString(std::string& s, int i,int& max_length) {
        
        std::string unique_string;
        for (auto& ch : s.substr(i))
        {
            if (unique_string.find(ch) == std::string::npos)
            {
                unique_string.push_back(ch); // add this char as it is not found
            }
            else
            {
                break;
            }
            
            
        }
        
        if (max_length < unique_string.length())
        {
            max_length = unique_string.length();
        }
    }

    int lengthOfLongestSubstring(std::string s) {
        
        int max_length = 0;
        std::string longest_substring;
        std::string s1 = s;
        
        for (int i = 0;i < s.length(); i++)
        {
            find_longestSubString(s,i,max_length);
        }
        
        return max_length;
    }
    
};

