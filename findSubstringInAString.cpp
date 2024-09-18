#include<bits/stdc++.h>
#define ll long long 
#define pb push_back
using namespace std;
void fun(){
	string s,t;
	size_t pos=s.find(t);
	while(pos!=string::npos){
		s.replace(pos,t.length(),string(t.length(),' '));
		pos=s.find(t,pos+t.length());
	}
}
