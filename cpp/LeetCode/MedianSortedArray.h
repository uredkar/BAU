#pragma once
#include <vector>
#include <algorithm>
#include <iostream>

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

    virtual double findMedianSortedArrays(vector<int>& nums1, vector<int>& nums2) {
        cout << "\noriginal findMedianSortedArrays";
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

class MedianSortedArrayPartitioned : public MedianSortedArray {
public:    
    MedianSortedArrayPartitioned()
    {
        case1(); // uses parents case1
        case2(); // uses parents case2

    }
    // web
    // overwrites parent for actual function
    virtual double findMedianSortedArrays(vector<int>& nums1, vector<int>& nums2) {
        cout << "partial sort from web";
        nums1.insert(nums1.end(), nums2.begin(), nums2.end());

        auto length = nums1.size();
        auto middle_ind = length / 2;
        // why sort the whole vectory when you can do partial sort
        std::partial_sort(nums1.begin(),
            nums1.begin() + middle_ind + 1,
            nums1.end(),
            std::less<int>{});

        auto middle = nums1[middle_ind];

        if (length % 2 == 0)
        {
            auto middle1 = nums1[middle_ind - 1];
            return static_cast<double>(middle1 + middle) / 2.0F;
        }

        return static_cast<double>(middle);

    }
};