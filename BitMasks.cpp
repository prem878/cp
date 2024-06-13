#include "bits/stdc++.h"
using namespace std;
//BITMASKing
bool isEven(int n){
  return (n & 1);
}

int getBit(int& n,int idx){
  int mask=(1<<idx);
  int bit = (mask & n) > 0 ? 1 : 0;
  return bit;
}

int setBit(int n,int idx){
  int mask=1<<idx;
  int bit=(n | mask);
  return bit;
}

void clearBit(int& n,int i){
  int preMask=(1<<i);
  int mask=~(preMask);
  n=(n & mask);
}

void clearLastIBits(int& n,int i){
  int mask=(-1<<i);
  n=n&mask;
}



void update(int& n,int i,int val){
  int premask=1<<i;
  int mask=~premask;
  n=(n&mask) | val>>i;
}


int main(){
  //Perform bit masks
  
  return 0;
}

