// NOLINT
#include <iostream>
#include <string>

int main() {
  int t;
  std::cin >> t;
  while (t--) {
    std::string s;
    std::cin >> s;
    int n = s.length();
    if (n % 2 == 0 && s.substr(0, n / 2) == s.substr(n / 2)) {
      std::cout << "YES\n";
    } else {
      std::cout << "NO\n";
    }
  }
  return 0;
}