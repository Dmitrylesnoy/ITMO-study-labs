#include <iostream>
using namespace std;

int main() {
  long long a, b, c, d, k;
  cin >> a >> b >> c >> d >> k;
  
  long long cur_a = a;
  for (long long day = 1; day <= k; ++day) {
    long long prev_a = cur_a;
    cur_a = cur_a * b;
    if (cur_a <= c) {
      cur_a = 0;
      break;
    }
    cur_a -= c;
    if (cur_a > d) {
      cur_a = d;
    }
    if (cur_a == prev_a) {
      break;
    }
  }
  cout << cur_a << endl;
  return 0;
}