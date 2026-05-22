#include <deque>
#include <iostream>
#include <vector>

using namespace std;

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  int length;
  int window;
  if (!(cin >> length >> window)) {
    return 0;
  }
  vector<int> numbers(length);
  for (int i = 0; i < length; ++i) {
    cin >> numbers[i];
  }
  deque<int> indices_of_tiny_things;
  for (int i = 0; i < length; ++i) {
    if (!indices_of_tiny_things.empty() &&
        indices_of_tiny_things.front() <= i - window) {
      indices_of_tiny_things.pop_front();
    }
    while (!indices_of_tiny_things.empty() &&
           numbers[indices_of_tiny_things.back()] >= numbers[i]) {
      indices_of_tiny_things.pop_back();
    }
    indices_of_tiny_things.push_back(i);
    if (i >= window - 1) {
      cout << numbers[indices_of_tiny_things.front()]
           << (i == length - 1 ? "" : " ");
    }
  }
  cout << endl;
  return 0;
}