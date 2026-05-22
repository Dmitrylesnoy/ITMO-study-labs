#include <algorithm>
#include <fstream>
#include <vector>

using namespace std;

void dfs1(int v, int max_w, const vector<vector<int>> &adj,
          vector<bool> &visited, int n) {
  visited[v] = true;
  for (int to = 0; to < n; ++to) {
    if (!visited[to] && adj[v][to] <= max_w) {
      dfs1(to, max_w, adj, visited, n);
    }
  }
}

void dfs2(int v, int max_w, const vector<vector<int>> &adj,
          vector<bool> &visited, int n) {
  visited[v] = true;
  for (int to = 0; to < n; ++to) {
    if (!visited[to] && adj[to][v] <= max_w) {
      dfs2(to, max_w, adj, visited, n);
    }
  }
}

bool check(int max_w, const vector<vector<int>> &adj, int n) {
  vector<bool> visited(n, false);

  dfs1(0, max_w, adj, visited, n);
  for (int i = 0; i < n; ++i) {
    if (!visited[i])
      return false;
  }

  fill(visited.begin(), visited.end(), false);
  dfs2(0, max_w, adj, visited, n);
  for (int i = 0; i < n; ++i) {
    if (!visited[i])
      return false;
  }

  return true;
}

int main() {
  ifstream cin("avia.in");
  ofstream cout("avia.out");

  int n;
  if (!(cin >> n))
    return 0;

  vector<vector<int>> adj(n, vector<int>(n));
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      cin >> adj[i][j];
    }
  }

  if (n <= 1) {
    cout << 0 << "\n";
    return 0;
  }

  int left = 0;
  int right = 1e9 + 7;
  int ans = right;

  while (left <= right) {
    int mid = left + (right - left) / 2;
    if (check(mid, adj, n)) {
      ans = mid;
      right = mid - 1;
    } else {
      left = mid + 1;
    }
  }

  cout << ans << "\n";

  return 0;
}