#pragma once
#include <vector>
#include <algorithm>

using namespace std;

class MedianSortedArray
{
public:
    MedianSortedArray()
    {
        case1();
        case2();
        
    }
    void case1()
    {
        vector<int> nums1 = { 1,3 };
        vector<int> nums2 = { 2 };

        auto median = findMedianSortedArrays(nums1, nums2);
        _ASSERT(median == 2.0);
    }

    void case2()
    {
        vector<int> nums1 = { 1,2 };
        vector<int> nums2 = { 3,4 };

        auto median = findMedianSortedArrays(nums1, nums2);
        _ASSERT(median == 2.5);
    }

    double findMedianSortedArrays(vector<int>& nums1, vector<int>& nums2) {

        vector<int> num = nums1;
        num.insert(num.end(),nums2.begin(), nums2.end());
        sort(num.begin(), num.end());
        auto length = num.size();
        auto middle = length / 2;
        if (length % 2 == 0) {
            // even
            
            return (double) (num[middle - 1] + num[middle])/2.0;
        }
        else
        {
            return (double) num[middle];
        }
        
    }
};

