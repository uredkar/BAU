#pragma once

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
//https://en.wikipedia.org/wiki/Longest_palindromic_substring
using namespace std;
class LongestPalindrome
{
public:
    LongestPalindrome() {
        
       /* isPalidrome("aba");
        isPalidrome("aaaa");
        isPalidrome("kayak");
        isPalidrome("reviver");
        isPalidrome("madam");
        isPalidrome("refer");
        isPalidrome("1refer2");
        isPalidrome("2121212");*/

        case1();
        case2();
    }

    void case1()
    {
        auto s = longestPalindrome("babad");
        _ASSERT(s == "bab" || s == "aba");
    }

    void case2()
    {
        auto s = longestPalindrome("cbbd");
        _ASSERT(s == "bb");
    }

    bool isPalidrome(string s) {
        cout << "\nstring " << s;
        auto len = s.length();
        for (auto i =0; i < len;i++)
        {
            if (s[i] != s[len - i - 1]) {
                cout << " is false";
                return false;
            }
        }
        cout << " is true";
        return true;
    }

    string insert_delimited_per_char(string &s, char delimit_char)
    {
        string ds ;
        ds.push_back(delimit_char);
        for (auto i = 0; i < s.length(); i++)
        {
            ds.push_back(s[i]);
            ds.push_back(delimit_char);
        }
        return ds;
        
    }
    string remove_delimited_per_char(string& s, char delimit_char)
    {
        string ds;
        
        for (auto i = 0; i < s.length(); i++)
        {
            if (s[i] != delimit_char)
            {
                ds.push_back(s[i]);
            }

        }
        return ds;

    }

    // slow algorithm 
    string longestPalindrome(string s) {
        char delimit_char = '|';
        string ds = insert_delimited_per_char(s, delimit_char);
        //cout << "\ndelimited string " << ds;
        vector<int> radii(s.size() * 2 + 1);
        int center = 0;
        while (center < ds.size()) {
            int radius = 0;
            while ( (center - (radius + 1)) >= 0 &&  // lower limit
                    (center + (radius + 1) < ds.size()) &&  // upper limit
                    (ds[center - (radius + 1)] == ds[center + (radius + 1)])) { // it is a palindrome
                radius = radius + 1;
            }
            //cout << "\n Center " << center;
            radii[center] = 2 * radius;
            center = center + 1; // go to the next center
        }
        int longest_palindrome_ds = *max_element(radii.begin(),radii.end());
        int longest_palindrome_s = (longest_palindrome_ds-1)/2;
        int longest_palindrome_center = distance(radii.begin(), max_element(radii.begin(), radii.end()));
        int longest_plaindrom_radius = radii[longest_palindrome_center]/2;
        //cout << "Index of max element: " << distance(radii.begin(), max_element(radii.begin(), radii.end())) << endl;
        //cout << "\nstart " << radii[longest_palindrome_ds] - longest_palindrome_ds / 2;
        string longest_ds = ds.substr(longest_palindrome_center - longest_plaindrom_radius, longest_plaindrom_radius*2);
        //cout << "\n longest" << longest_ds;
        return remove_delimited_per_char(longest_ds, delimit_char);
    }
};

