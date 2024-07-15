
//Topo sort

#include<bits/stdc++.h>
#define add push_back
using namespace std;
vector<int> adj[6969];
vector<bool> vis(6969);
vector<int> res;
void dfs(int u){
  vis[u]=1;
  for(int v : adj[u])
    if(vis[v]==false) dfs(v);
  res.add(u);
}
int main(){
  int n,m;
  cin>>n>>m;
  while(m--){
    int x,y;
    cin>>x>>y;
    adj[x].add(y);
    adj[y].add(x);
  }
  for(int i=1;i<=n;++i) vis[i]=false;
  for(int i=1;i<=n;++i){
    if(vis[i]==false) dfs(i);
  }
  reverse(res.begin(),res.end());
  for(auto it : res) cout<<it<<" ";
  return 0;
  
}
