//1 is not prime
// tc O(n*log(n))

#include<bits/stdc++.h>
using namespace std;
vector<int> prime;
void SieveOfEratosthenes(int n){
  bool isPrime[n+1];
  memset(isPrime,true,sizeof(isPrime));
  for(int p=2;p*p<=n;++p){
    if(isPrime[p]){
      for(int i=p*p;i<=n;i+=p){
        isPrime[i]=false;
      }
    }
  }
  for(int i=2;i<=n;++i){
    if(isPrime[i]){
      prime.push_back(i);
    }
  }
}
int main(){
  int n;
  cin>>n;
  SieveOfEratosthenes(n);
  return 0;
}
