#include <iostream>
#include <vector>

using namespace std;

int main() {
  int n;
  cin >> n;

  vector<int> a(n);
  for (int i = 0; i < n; i++) {
    cin >> a[i];
  }

  if (n <= 2) {
    cout << 1 << " " << n << endl;
    return 0;
  }

  int l = 0;
  int max_len = 2;
  int bst_l = 0, bst_r = 1;

  for (int r = 2; r < n; r++) {
    if (a[r] == a[r - 1] && a[r] == a[r - 2]) {
      l = r - 1;
    }

    int cur_len = r - l + 1;
    if (cur_len > max_len) {
      max_len = cur_len;
      bst_l = l;
      bst_r = r;
    }
  }
  cout << bst_l + 1 << " " << bst_r + 1 << endl;
  return 0;
}