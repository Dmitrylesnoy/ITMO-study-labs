#include <iostream>
#include <stack>
#include <string>
#include <unordered_map>
#include <vector>
using namespace std;

int main() {
  unordered_map<string, stack<int>> vals;
  vector<vector<string>> scopes;
  scopes.push_back({});

  string line;
  while (cin >> line) {
    if (line == "{") {
      scopes.push_back({});
    } else if (line == "}") {
      for (string var_name : scopes.back()) {
        vals[var_name].pop();

        if (vals[var_name].empty()) {
          vals.erase(var_name);
        }
      }
      scopes.pop_back();

    } else {
      size_t pos = line.find('=');
      string var1 = line.substr(0, pos);
      string r = line.substr(pos + 1);

      int val;
      if (isdigit(r[0]) || (r.length() > 1 && r[0] == '-')) {
        val = stoi(r);
      } else {
        val = vals.count(r) ? vals[r].top() : 0;
        cout << val << "\n";
      }
      scopes.back().push_back(var1);
      vals[var1].push(val);
    }
  }
  return 0;
}