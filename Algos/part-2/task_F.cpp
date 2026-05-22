#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

bool compareNumbers(const std::string &a, const std::string &b) {
  return a + b > b + a;
}

int main() {
  std::vector<std::string> parts;
  std::string temp;

  while (std::cin >> temp) {
    parts.push_back(temp);
  }

  if (parts.empty()) {
    return 0;
  }

  std::sort(parts.begin(), parts.end(), compareNumbers);

  for (const std::string &s : parts) {
    std::cout << s;
  }
  std::cout << std::endl;

  return 0;
}