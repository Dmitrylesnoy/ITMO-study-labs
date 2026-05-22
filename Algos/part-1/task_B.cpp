#include <cctype>
#include <cstddef>
#include <iostream>
#include <stack>
#include <string>
#include <vector>
using namespace std;

struct Element {
  char val;
  int id;
};

bool is_match(char a, char b) {
  if (tolower(a) != tolower(b))
    return false;
  return (islower(a) && isupper(b)) || (isupper(a) && islower(b));
}

int main() {
  string s;
  if (!(cin >> s))
    return 0;

  int n = s.length() / 2;
  vector<int> trap2animal(n + 1);
  stack<Element> st;

  int animal_idx = 0;
  int trap_idx = 0;

  for (size_t i = 0; i < s.length(); ++i) {
    char curChar = s[i];
    int cur_id;

    if (islower(curChar)) {
      cur_id = ++animal_idx;
    } else {
      cur_id = ++trap_idx;
    }

    if (!st.empty() && is_match(st.top().val, curChar)) {
      if (isupper(curChar)) {
        trap2animal[cur_id] = st.top().id;
      } else {
        trap2animal[st.top().id] = cur_id;
      }
      st.pop();
    } else {
      st.push({curChar, cur_id});
    }
  }

  if (st.empty()) {
    cout << "Possible" << endl;
    for (int i = 1; i <= n; ++i) {
      cout << trap2animal[i] << (i == n ? "" : " ");
    }
    cout << endl;
  } else {
    cout << "Impossible" << endl;
  }

  return 0;
}