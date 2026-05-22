#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

struct CharInfo {
  char c;
  long long weight;
  int count;
};

bool compareWeights(const CharInfo &a, const CharInfo &b) {
  if (a.weight != b.weight) {
    return a.weight > b.weight;
  }
  return a.c < b.c;
}

int main() {
  std::string s;
  if (!(std::cin >> s)) {
    return 0;
  }

  std::vector<long long> weights(26);
  for (int i = 0; i < 26; ++i) {
    std::cin >> weights[i];
  }

  std::vector<int> counts(26, 0);
  for (char c : s) {
    counts[c - 'a']++;
  }

  std::vector<CharInfo> info;
  for (int i = 0; i < 26; ++i) {
    if (counts[i] > 0) {
      info.push_back({static_cast<char>('a' + i), weights[i], counts[i]});
    }
  }

  std::sort(info.begin(), info.end(), compareWeights);

  std::string left_side = "";
  std::string right_side = "";
  std::string middle = "";

  for (auto &item : info) {
    if (item.count >= 2) {
      left_side += item.c;
      right_side = item.c + right_side;
      item.count -= 2;
    }
    while (item.count > 0) {
      middle += item.c;
      item.count--;
    }
  }

  std::cout << left_side << middle << right_side << std::endl;

  return 0;
}