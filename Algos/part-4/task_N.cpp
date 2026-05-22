#include <iostream>
#include <vector>

using namespace std;

void dfs(int v, const vector<int> &next_pig, vector<int> &visited,
         int &cycles) {
  visited[v] = 1;

  int to = next_pig[v];
  if (visited[to] == 0) {
    dfs(to, next_pig, visited, cycles);
  } else if (visited[to] == 1) {
    cycles++;
  }

  visited[v] = 2;
}

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);

  int n;
  if (!(cin >> n))
    return 0;

  vector<int> next_pig(n);
  for (int i = 0; i < n; ++i) {
    cin >> next_pig[i];
    next_pig[i]--;
  }

  vector<int> visited(n, 0);
  int cycles = 0;

  for (int i = 0; i < n; ++i) {
    if (visited[i] == 0) {
      dfs(i, next_pig, visited, cycles);
    }
  }

  cout << cycles << "\n";

  return 0;
}