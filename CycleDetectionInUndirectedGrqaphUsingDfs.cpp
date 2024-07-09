#include <bits/stdc++.h>
using namespace std;
#define ll long long 
#define pb push_back
vector<int> adj[696969];
vector<bool> vis(6969669);
bool dfs(int v,int parent){
	vis[v]=true;
	for(auto it : adj[v]){
		if(vis[it]==false){
			if(dfs(it,v)) return true;
		}else if(it!=parent)
			return true;
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
			if(dfs(i,-1)) ans=true;
		}
	}
	if(ans){
		cout<<"Cycle found";
	}else{
		cout<<"Not found";
	}
    cout << endl;
}
