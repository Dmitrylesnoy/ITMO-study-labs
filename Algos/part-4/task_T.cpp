#include <iostream>
#include <map>
#include <vector>

using namespace std;

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);

  int n;
  if (!(cin >> n))
    return 0;

  vector<int> a(n);
  for (int i = 0; i < n; ++i) {
    cin >> a[i];
  }

  vector<map<int, int>> next_pos(n);
  long long total_segments = 0;

  for (int i = n - 1; i >= 0; --i) {
    next_pos[i][a[i]] = i + 1;

    int current_val = a[i];

    while (true) {
      int mid = next_pos[i][current_val];

      if (mid >= n || !next_pos[mid].count(current_val)) {
        break;
      }

      int after_right = next_pos[mid][current_val];
      current_val++;
      next_pos[i][current_val] = after_right;
    }

    total_segments += next_pos[i].size();
  }

  cout << total_segments << "\n";

  return 0;
}