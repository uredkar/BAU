// LeetCode.cpp : This file contains the 'main' function. Program execution begins and ends there.
//
#include "AddTwoNumbers.h"
#include "LongestSubstringWithoutRepeatingChars.h"
#include "MedianSortedArray.h"
#include "LongestPalindrome.h"
#include "RegularExpressionMatching.h"
#include "MergeKSortedLists.h"
#include "Sudoku.h"
#include "NQueens.h"
#include "Permutation.h"
#include "TextJustification.h"
// not started 
void permute(int i, int n)
{
    if (n == 0)
        return;
    for (int i = 0; i < n; i++) {
        int mod = i % n;
        cout << "\ni" << i << " mod " << mod;
        for (int l = 0; l < n; l++) {
            int next_start = 1 + ((i + l) % n);
            cout << "\nmod i=" << i << " clock index " << 1 + ((i + l) % n);
            permute(next_start, n - 1);
        }
    }

}

int main()
{
    //uncomment to run
    //AddTwoNumbers s;
    //LongestSubstringWithoutRepeatingChars c;
    //MedianSortedArray m;
    //MedianSortedArrayPartitioned mp;
    //LongestPalindrome lpal;
    //RegularExpressionMatching rex;
    //MergeKSortedLists mks; // call test method to test

    //MergeKSortedLists_PriorityQ mks_priorityQ;
    //mks_priorityQ.test();
    //Sudoku s;
    //NQueensOnMyOwn q0; // this is not optimal at all
    //NQueens q1; // improved  when copied valid approach and removed all junk maps :-(
    //NQueensAS q2; // found on leet code
    //Permutation per;
    
    //permute(0, 3); not done 

    TextJustification tj;

    
}

