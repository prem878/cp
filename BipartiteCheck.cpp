#include<bits/stdc++.h>
using namespace std;
vector<vector<int>> adj; 
bool isBipartite(){
  int n=adj.size();
  bool bipartite=false;
  vector<int> side(n,-1);
  Queue<int> q;
  for(size_t st=0;st<n;++st){
    if(side[st]==-1){
      q.push(st);
      side[st]=1;
      while(q.isEmpty()==false){
        int v=q.front();
        q.pop();
        for(int u : adj[v]){
          if(side[u]==-1){
            side[u]=side[v]^1;
            q.push(u);
          }else{
            bipartite & = side[u]!=sode[v];
          }
        }
      }
    }
  }
  return biparitie;
}
int main(){
  //adj
  return 0;
}
