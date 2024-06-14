#include <iostream>
#include <vector>
#include <unordered_map>
#include <stack>
#include <set>
using namespace std;

// Global data structures
unordered_map<int, vector<int>> graph;
unordered_map<int, int> index;
unordered_map<int, int> lowlink;
stack<int> S;
set<int> onStack;
vector<vector<int>> sccs;
int idx = 0;

void tarjanDFS(int node) {
    index[node] = lowlink[node] = idx++;
    S.push(node);
    onStack.insert(node);

    for (int neighbor : graph[node]) {
        if (index.find(neighbor) == index.end()) {
            tarjanDFS(neighbor);
            lowlink[node] = min(lowlink[node], lowlink[neighbor]);
        } else if (onStack.find(neighbor) != onStack.end()) {
            lowlink[node] = min(lowlink[node], index[neighbor]);
        }
    }

    if (lowlink[node] == index[node]) {
        vector<int> scc;
        int w;
        do {
            w = S.top();
            S.pop();
            onStack.erase(w);
            scc.push_back(w);
        } while (w != node);
        sccs.push_back(scc);
    }
}

vector<vector<int>> tarjan(vector<pair<int, int>>& edges) {
    // Clear global structures for fresh computation
    graph.clear();
    index.clear();
    lowlink.clear();
    while (!S.empty()) S.pop();
    onStack.clear();
    sccs.clear();
    idx = 0;

    // Build the graph
    for (auto edge : edges) {
        graph[edge.first].push_back(edge.second);
    }

    // Perform Tarjan's algorithm
    for (auto node : graph) {
        if (index.find(node.first) == index.end()) {
            tarjanDFS(node.first);
        }
    }

    return sccs;
}

int main() {
    vector<pair<int, int>> edges = {{1, 2}, {2, 3}, {3, 1}, {4, 5}, {5, 6}, {6, 4}};
    vector<vector<int>> result = tarjan(edges);

    for (const auto& scc : result) {
        cout << "[";
        for (int node : scc) {
            cout << node << " ";
        }
        cout << "] ";
    }
    cout << endl;

    return 0;
}
