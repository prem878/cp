#include <bits/stdc++.h>
#define MOD 9876543210
#define ll long long
#define pb push_back
#define vi vector<int>
#define vl vector<ll>
#define pii pair<int,int>
#define arr array
#define fi first
#define se second
#define mp make_pair
#define all(a) (a).begin(),(a).end()
using namespace std;
const int pinf=INT_MAX;
const int ninf=INT_MIN;
const ll mod=1e9+7;
template <typename T> 
T max2(T x,T y){return x>y ? x: y;}
template <typename T>
T max3(T x,T y,T z){T mx=Max(x,y);mx=Max(mx,z);return mx;}
template <typename T>
T min2(T x,T y){return x>y ? y:x; }
template <typename T>
T min3(T x,T y,T z){T mn=Min(x,y);mn=Min(mn,z);return mn;}
template <typename T>
void debug(const vector<T>& v){for(const auto& it:v)cout<<it<<" "; cout<<"\n";}
void see() {}
template<typename T, typename... Args>
void see(T& first, Args&... args) {std::cin >> first; see(args...);}
void put() {}
template<typename T, typename... Args>
void put(T&& first, Args&&... args) {std::cout << first << " ";put(std::forward<Args>(args)...);}
void putl() { std::cout << std::endl;}
template<typename T, typename... Args>
void putl(T&& first, Args&&... args) {std::cout << first << " ";putl(std::forward<Args>(args)...);}
/* 便利な方法 */
ll gcd(ll a, ll b) { if (b == 0) return a; return gcd(b, a % b); }
ll lcm(ll a, ll b) { return a / gcd(a, b) * b; }
ll pow(ll a, ll b) { ll res = 1; while (b > 0) { if (b & 1) { res = res * a; } a = a * a; b >>= 1; } return res; }
bool sq(ll n) { return (sqrt(n) == floor(sqrt(n))); }
bool cb(ll n) { return (cbrt(n) == floor(cbrt(n))); }
bool isPrime(int n) { if (n <= 1) { return false; } for (int i = 2; i * i <= n; ++i) { if (n % i == 0) { return false; } } return true; }
bool isSubstr(const string& t,const string& s){ return t.find(s)!=string::npos;}
vector<int> primes(int n) { bool isPrime[n+1]; memset(isPrime, true, sizeof(isPrime)); vector<int> primes; for (int p = 2; p <= n; ++p) { if (isPrime[p]) { for (int i = p*p; i <= n; i += p) { isPrime[i] = false; } } } for (int i = 2; i <= n; ++i) { if (isPrime[i]) primes.push_back(i); } return primes; }
void solve();
void timeTaken(){auto start=chrono::high_resolution_clock::now();solve();cout<<"\n";auto end=chrono::high_resolution_clock::now();auto time=chrono::duration_cast<chrono::microseconds>(end-start);	cout<<"Time: "<<time.count();}
//入力する前に問題を 回よく読んでください
//解決する
void solve(){
	
}
int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);
	int tt;	tt=1;
//	cin>>tt;
    while (tt--) {
        solve();
		//timeTaken();
        cout << "\n";
    }
    return 0;
}
