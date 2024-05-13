//Author : Prem
#include <iostream>
#include <vector>
#include<cmath>
#include <chrono>
using namespace std;

// Segment Tree class
class SegmentTree {
private:
    vector<int> tree;
    int n; // size of the input array

    // Helper function to build the segment tree
    void buildTree(vector<int>& nums, int node, int start, int end) {
        if (start == end) {
            // Leaf node will have a single element
            tree[node] = nums[start];
        } else {
            int mid = (start + end) / 2;
            // Recursively build the segment tree
            buildTree(nums, 2 * node + 1, start, mid); // Left child
            buildTree(nums, 2 * node + 2, mid + 1, end); // Right child
            // Internal node stores the sum of its children
            tree[node] = tree[2 * node + 1] + tree[2 * node + 2];
        }
    }

    // Helper function to update a value at index idx in the segment tree
    void updateTree(int node, int start, int end, int idx, int value) {
        if (start == end) {
            // Leaf node
            tree[node] = value;
        } else {
            int mid = (start + end) / 2;
            if (start <= idx && idx <= mid) {
                // idx is in the left child
                updateTree(2 * node + 1, start, mid, idx, value);
            } else {
                // idx is in the right child
                updateTree(2 * node + 2, mid + 1, end, idx, value);
            }
            // Update current node
            tree[node] = tree[2 * node + 1] + tree[2 * node + 2];
        }
    }

    // Helper function to query sum in range [l, r]
    int rangeSum(int node, int start, int end, int l, int r) {
        if (r < start || end < l) {
            // Out of range
            return 0;
        }
        if (l <= start && end <= r) {
            // Current segment is fully in range
            return tree[node];
        }
        // Partially in range
        int mid = (start + end) / 2;
        int leftChildSum = rangeSum(2 * node + 1, start, mid, l, r);
        int rightChildSum = rangeSum(2 * node + 2, mid + 1, end, l, r);
        return leftChildSum + rightChildSum;
    }

public:
    // Constructor
    SegmentTree(vector<int>& nums) {
        n = nums.size();
        if (n > 0) {
            // Height of segment tree
            int height = (int)ceil(log2(n));
            // Maximum size of segment tree
            int maxSize = 2 * (int)pow(2, height) - 1;
            tree.resize(maxSize);
            buildTree(nums, 0, 0, n - 1);
        }
    }

    // Update the value at index idx in the input array to value
    void update(int idx, int value) {
        updateTree(0, 0, n - 1, idx, value);
    }

    // Return the sum of elements in range [l, r]
    int sumRange(int l, int r) {
        return rangeSum(0, 0, n - 1, l, r);
    }
};

// Example usage
int main() {
    vector<int> nums = {1, 3, 5, 7, 9, 11};
    SegmentTree segTree(nums);

    // Sum of elements from index 1 to 3 (1 + 3 + 5 = 9)
    cout << segTree.sumRange(1, 3) << endl;

    // Update element at index 2 to 6
    segTree.update(2, 6);

    // Sum of elements from index 1 to 3 after update (1 + 6 + 7 = 14)
    cout << segTree.sumRange(1, 3) << endl;

    return 0;
}
