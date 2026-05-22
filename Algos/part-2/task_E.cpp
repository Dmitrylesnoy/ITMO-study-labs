#include <iostream>
#include <vector>

bool is_placed(const std::vector<int> &coords, int k, int min_dist) {
  int count = 1;
  int last_pos = coords[0];

  for (size_t i = 1; i < coords.size(); ++i) {
    if (coords[i] - last_pos >= min_dist) {
      count++;
      last_pos = coords[i];
    }
  }
  return count >= k;
}

int main() {
  int n, k;
  if (!(std::cin >> n >> k)) {
    return 0;
  }

  std::vector<int> coords(n);
  for (int i = 0; i < n; ++i) {
    std::cin >> coords[i];
  }

  int left = 0;
  int right = coords[n - 1] - coords[0];
  int result = 0;

  while (left <= right) {
    int mid = left + (right - left) / 2;

    if (is_placed(coords, k, mid)) {
      result = mid;
      left = mid + 1;
    } else {
      right = mid - 1;
    }
  }

  std::cout << result << std::endl;

  return 0;
}