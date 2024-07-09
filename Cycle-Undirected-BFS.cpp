#include <bits/stdc++.h>
using namespace std;
#define ll long long 
#define pb push_back
vector<int> adj[696969];
vector<bool> vis(6969669);
bool bfs(int u){
	vis[u]=true;
	queue<pair<int,int>> q;
	q.push({u,-1});
	while(!q.empty()){
		int node=q.front().first;
		int parent=q.front().second();
		q.pop();
		for(auto it : adj[node]){
			if(!vis[it]){
				vis[it]=1;
				q.push({it,node});
			}else if(parent!=it){
				return true;
			}
		}
	}
	return false;
}
signed main() {
	int n,m;
	cin>>n>>m;
	bool ans=false;
	while(m--){
		int x,y;
		cin>>x>>y;
		adj[x].pb(y);
		adj[y].pb(x);
	}
	for(int i=1;i<=n;++i) vis[i]=false;
	for(int i=1;i<=n;++i){
		if(vis[i]==false){
			if(bfs(i)) ans=true;
		}
	}
	if(ans){
		cout<<"Cycle found";
	}else{
		cout<<"Not found";
	}
    cout << endl;
}
