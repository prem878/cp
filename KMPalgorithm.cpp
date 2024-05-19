#include <bits/stdc++.h>
#define ll long long
using namespace std;
vector<int> ans;
int *construct(string s) {
  int m = s.length();
  int *lps = new int[m];
  int len = 0;
  int i = 1;
  while (i < m) {
    if (s[i] == s[len]) {
      len++;
      lps[i] = len;
      i++;
    } else {
      if (len != 0) {
        len = lps[len - 1];
      } else {
        lps[i] = 0;
        i++;
      }
    }
  }
  return lps;
}
void KMPalgo(string txt, string pat) {
  int n = txt.length();
  int m = pat.length();
  int *lps = construct(pat);
  int i = 0;
  int j = 0;
  while ((n - i) >= (m - j)) {
    if (pat[j] == txt[i]) {
      i++;
      j++;
    }
    if (j == m) {
      ans.push_back(i - j);
      j = lps[j - 1];
    } else {
      if (i < n && pat[j] != txt[i]) {
        if (j != 0) {
          j = lps[j - 1];
        } else {
          i++;
        }
      }
    }
  }
}
int main() {
  string txt, pat;
  cin >> txt >> pat;
  int *lps = construct(pat);
  cout << "Substrings are found at indices"
       << "\n";
  KMPalgo(txt, pat);
  for (const auto &x : ans) {
    cout << x << " ";
  }
  return 0;
}
