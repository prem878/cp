
//Given a sequence of n numbers a1, a2, ..., an and a number of k- queries.
//A k-query is a triple (i, j, k) (1 ≤ i ≤ j ≤ n). For each k-query (i, j, k),
//you have to return the number of elements greater than k in the subsequence ai, ai+1, ..., aj.
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
using namespace std;

struct Query {
    int L, R, k, idx;
};

// Comparator to sort queries
bool compare(Query q1, Query q2) {
    int blockSize = sqrt(q1.L); // Calculate block size dynamically for sorting
    if (q1.L / blockSize != q2.L / blockSize)
        return q1.L / blockSize < q2.L / blockSize;
    return q1.R < q2.R;
}

// Function to process queries using Mo's Algorithm
vector<int> moAlgorithm(const vector<int>& arr, vector<Query>& queries) {
    int n = arr.size();
    int q = queries.size();
    int blockSize = sqrt(n);
    vector<int> results(q);

    // Sort queries using the compare function
    sort(queries.begin(), queries.end(), compare);

    // Initialize current range and answer
    int currentL = 0, currentR = -1, currentAnswer = 0;

    // Function to add an element
    auto add = [&](int index, int k) {
        if (arr[index] > k) currentAnswer++;
    };

    // Function to remove an element
    auto remove = [&](int index, int k) {
        if (arr[index] > k) currentAnswer--;
    };

    // Process each query
    for (Query query : queries) {
        while (currentR < query.R) {
            currentR++;
            add(currentR, query.k);
        }
        while (currentR > query.R) {
            remove(currentR, query.k);
            currentR--;
        }
        while (currentL < query.L) {
            remove(currentL, query.k);
            currentL++;
        }
        while (currentL > query.L) {
            currentL--;
            add(currentL, query.k);
        }
        // Store the result for the current query
        results[query.idx] = currentAnswer;
    }

    return results;
}

int main() {
    // Array and queries initialization
    vector<int> arr = {1, 5, 2, 6, 3, 7};
    vector<Query> queries = {
        {1, 4, 3, 0},
        {2, 5, 4, 1},
        {0, 3, 1, 2}
    };

    // Process queries using Mo's Algorithm
    vector<int> results = moAlgorithm(arr, queries);

    // Output the results
    for (int res : results) {
        cout << res << endl; // Output: 2 2 3
    }

    return 0;
}

