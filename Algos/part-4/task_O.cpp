#include <iostream>
#include <vector>

using namespace std;

bool dfs(int v, int c, const vector<vector<int>> &adj, vector<int> &color) {
  color[v] = c;

  for (int to : adj[v]) {
    if (color[to] == 0) {
      int next_color = (c == 1) ? 2 : 1;
      if (!dfs(to, next_color, adj, color)) {
        return false;
      }
    } else if (color[to] == color[v]) {
      return false;
    }
  }
  return true;
}

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);

  int n, m;
  if (!(cin >> n >> m))
    return 0;

  vector<vector<int>> adj(n);
  for (int i = 0; i < m; ++i) {
    int u, v;
    cin >> u >> v;
    u--;
    v--;
    adj[u].push_back(v);
    adj[v].push_back(u);
  }

  vector<int> color(n, 0);
  bool possible = true;

  for (int i = 0; i < n; ++i) {
    if (color[i] == 0) {
      if (!dfs(i, 1, adj, color)) {
        possible = false;
        break;
      }
    }
  }

  if (possible) {
    cout << "YES\n";
  } else {
    cout << "NO\n";
  }

  return 0;
}