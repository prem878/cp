//TC wille be O(logmax(a,b))

#include<bits/stdc++.h>
using namespace std;
int Binarygcd(int x, int y)
{
    if (x == 0) return y;
    if (x < y) return gcd(y, x);
    if (x%2 == 1)
        if (y%2 == 1)
            return gcd(x - y, y);
        else
            return gcd(x, y/2);
    else
        if (y%2 == 1)
            return gcd(x/2, y);
        else
            return gcd(x/2, y/2)*2;
}
//Euclidean algorithm
int Euclideangcd(int x, int y)
{
    if (x == 0) return y;
    else return gcd(y%x, x);
}
//Extended Euclidean algorithm
int gcd_extended(int a, int b, int* x, int* y)
{
    if (a == 0)
    {
        *x = 0, *y = 1; /* 0(0) + 1(b) = b = gcd(a,b) */
        return b;
    }
    else
    {
        int k = b/a;
        int r = b%a;
        int _x, _y;
        int d = gcd_extended(r, a, &_x, &_y);
        *x = _y - k*_x;
        *y = _x;
        return d;
    }
}
int main(){
  //GCD
  return 0;
}
