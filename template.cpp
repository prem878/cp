#include<iostream>
#include<stdint.h>
#include<set>
#include<vector>
#include<cmath>
#include<algorithm>
#define yes cout<<"Yes"
#define no cout<<"No"
#define INF 1e10
#define nl cout<<"\n"
#define nINF INF*-1
#define MOD 100000007
#define Y cout<<"YES"
#define N cout<<"NO"
#define SIZE 1e7
#define f first
#define s second
#define ll long long
#define pb push_back
using namespace std;
typedef long long int myint;
int ar[100005];
ll min(ll a,ll b){ if(a<b)return a; return b;}
ll max(ll a,ll b){if(a>b)return a; return b;}
ll gcd(ll a,ll b){ if( b==0) return a; return gcd(b,a%b);}
ll lcm(ll a,ll b){return a/gcd(a,b)*b;}
int min(int a ,int b){if(a<b)return a;return b;}
int suma(int a[],int size){int rs=0;for(int i=0;i<size;++i)rs+=a[i];return rs;}
void dbg(int a[],int k,int j){for(int i=k;i<=j;++i){ cout<<a[i]<<" ";}cout<<"\n";}
void v_print(vector<int> v){for(int val: v){cout<<val<<"" ;}}
void v_print(vector<ll>v){for(int val: v){cout<<val<<" ";}}
bool sq(ll n){ return (sqrt(n)==floor(sqrt(n))); }
bool cb(ll n){ return (cbrt(n)==floor(cbrt(n))); }
bool isPrime(int n){if(n<=1) {return false;} for(int i=2;i<=n/2;++i){if(n%i==0){return false; }}return true;}
ll pow(ll a,ll b){ll res=1;while(b>0){if(b&1){res=res*a;}a=a*a;b>>=1;}return res;}
ll reverse(ll n){ll ans=0,rem;while(n!=0){rem=n%10;ans=ans*10+rem;n/=10;}return ans;}
void dbgv(vector<int> v){for(auto& x: v)cout<<x<<" ";cout<<"\n";}
void dbgvc(vector<char> v){for(auto& x : v)cout<<x<<" ";cout<<"\n";}
//入力する前に問題を 回よく読んでください
//解決する
void solve(){
	//CODE
} 
int main(){
	ios_base::sync_with_stdio(false); 
    cin.tie(NULL);
	int tt;
	tt=1;
	//cin>>tt;
	while(tt--){
		solve();
		cout<<"\n";
	}
	return 0;
}
