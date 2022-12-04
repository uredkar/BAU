#pragma once

#include <iostream>
#include <cstring>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
using namespace std;

class TextJustification
{
public:
    TextJustification()
    {
        /*
        case1();
        case2();
        case3();
        */
        case4();
    }
    
    void print_results(vector<string>& results, int maxWidth)
    {
        cout << "\n--------------------------x--------------------------";
    
        for (auto s : results){
            cout << "\n[" << s << "]";
        }
    }
    void case1()
    {
        vector<string> words =
            { "This", "is", "an", "example", "of", "text", "justification." };
           
        auto results = fullJustify(words,16);
        print_results(results,16);

    }

    void case2()
    {
        vector<string> words =
        { "What","must","be","acknowledgment","shall","be" };

        auto results = fullJustify(words, 16);
        print_results(results,16);

    }

    void case4()
    {
        
        vector<string> words =
        { "ask", "not", "what", "your", "country", "can", "do", "for", "you", "ask", "what", "you", "can", "do", "for", "your", "country" };

        auto results = fullJustify(words, 16);
        print_results(results, 16);

    }

    void case3()
    {
        vector<string> words =
        { "Science","is","what","we","understand","well","enough","to","explain","to","a","computer.","Art","is","everything","else","we","do" };

        auto results = fullJustify(words, 20);
        print_results(results,20);

    }
    
    string addBlanksAtEndOfLine(string& s,int maxWidth) {
        string justified = s;

        int dif = maxWidth - s.size();
        
        while(justified.size() < maxWidth)
        {
            //justified.push_back('|');
            justified.push_back(' ');
        }
        return justified;

    }

    string redistributeBlanks(string& s,int start) {
        string justified = s;
        int length = s.length();
        bool found = false;
        int prev_start = start;
        //while (length > 2 && justified[length - 1] == '|') { 
        while (length > 2 && justified[length - 1] == ' ') {
            //cout << "\nlast char is |";
            for (int i = start+1; i < length; i++) {
                //if (justified[i] != '|' && justified[i - 1] == '|')
                if (justified[i] != ' ' && justified[i - 1] == ' ')
                {
                    found = true;
                    //justified.insert(i, "|");
                    justified.insert(i, " ");
                    justified.pop_back();
                    
                    start = i + 1;
                    if (start >= length) {
                        start = 0;
                    }
                    //cout << "\n" << justified << " size=" << justified.size() << " start=" << start;
                    break;
                }
                start = 0;
            }
            if (found == false)
            {
                break;
            }
            //if (prev_start == start)
            //    break;
        }
        //cout << "shift one " << justified;
        return justified;
        
    }

    vector<string> fullJustify(vector<string>& words, int maxWidth) {
        vector<int> wordlength;
        vector<string> justified;
        string current_line;
        int total_length = 0;
        for (auto word : words) {
            
            
            if (word.size() + current_line.size() <= maxWidth) {
                total_length += word.size(); // default spacing
                current_line.append(word);
                total_length += 1;
                //current_line.append("|");
                current_line.append(" ");
            }
            else {
                justified.push_back(current_line.substr(0,current_line.size() - 1)); // remove last | 
                //justified.push_back(current_line);
                //current_line = word + "|";
                current_line = word + " ";
                total_length = word.size() + 1; // default spacing
            }
            
            
        }
        if (current_line.size() > 0)
        {
            justified.push_back(current_line.substr(0, current_line.size() - 1)); // remove last | 
            //justified.push_back(current_line);
        }
        int length = justified.size();
        vector<string> result;
        for (auto s : justified) {
            length--;
            //cout << "\ninput " << s;
            auto sendwithblanks = addBlanksAtEndOfLine(s, maxWidth);
            auto j = sendwithblanks;
            if (length >= 1) {
                j = redistributeBlanks(sendwithblanks, 0);
            }
            //cout << "\nbefore " << j;
            //replace(j.begin(), j.end(), '|', ' ');
            //cout << "\nafter " << j;
            result.push_back(j);
        }


        return result;
    }
};

// from web
class TextJustification2 {
public:
    vector<string> fullJustify(vector<string>& words, int maxWidth) {
        vector<string> res;
        if (maxWidth == 0)
            return { "" };

        int i = 0, j = 0;
        while (j != words.size()) {
            int len = -1;
            while (j < words.size() && len + words[j].size() + 1 <= maxWidth)
                len += words[j++].size() + 1;
            int space = maxWidth - len + j - i - 1;
            int k = i;
            while (space) {
                words[k++] += " ";
                space--;
                if (j != words.size() && (k == j - 1 || k == j)) k = i;
                if (j == words.size() && k == j) k = j - 1;
            }

            string line = "";
            for (int l = i; l < j; l++)
                line += words[l];
            res.push_back(line);
            i = j;
        }

        return res;
    }
};