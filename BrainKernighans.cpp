#include<bits/stdc++.h>
using namespace std;
//This is to number of 1's in binary form of number n
int counSetBits(int n){
  int answer=0;
  while(n){
    n=n&(n-1);
    answer++;
  }
  return answer;
}
