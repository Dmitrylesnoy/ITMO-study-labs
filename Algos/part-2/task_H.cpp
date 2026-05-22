#include <algorithm>
#include <iostream>
#include <vector>

int main() {
  int n, k;
  if (!(std::cin >> n >> k)) {
    return 0;
  }

  std::vector<int> prices(n);
  for (int i = 0; i < n; ++i) {
    std::cin >> prices[i];
  }

  std::sort(prices.begin(), prices.end(), std::greater<int>());

  long long total = 0;
  for (int i = 0; i < n; ++i) {
    if ((i + 1) % k == 0) {
      continue;
    }
    total += prices[i];
  }

  std::cout << total << std::endl;

  return 0;
}