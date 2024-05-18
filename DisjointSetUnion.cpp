#include<bits/stdc++.h>
using namespace std;
class DisjointSetUnio{
  private:
    int *parent;  
    int *rank;
    int *size;
  public:
    DisjointSetUnio(int n){
      parent=new int[n];
      rank=new int[n];
      size=new int[n];
    }
    int find(int v){
      if(parent[v]==v)
        return v;
      return parent[v]=find(parent[v]);
    }
    void make_set(int x,int y){
      parent[x]=x;
      parent[y]=y;
      rank[x]=0;
      rank[y]=0;
      size[x]=1;
      size[y]=1;
    }
    void union_sets(int a, int b) {
    a = find(a);
    b = find(b);
    if (a != b) {
        if (rank[a] < rank[b])
            swap(a, b);
        parent[b] = a;
        if (rank[a] == rank[b])
            rank[a]++;
    }
  }    
};
