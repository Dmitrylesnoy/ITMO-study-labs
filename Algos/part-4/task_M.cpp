#include <algorithm>
#include <iostream>
#include <queue>
#include <string>
#include <vector>

using namespace std;

struct Cell {
  int dist;
  int x, y;

  bool operator>(const Cell& other) const {
    return dist > other.dist;
  }
};

struct Point {
  int x, y;
  char dir;
};

const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, 1, 0, -1};
const char dir_chars[] = {'N', 'E', 'S', 'W'};

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);

  int n, m;
  if (!(cin >> n >> m))
    return 0;

  int start_x, start_y, end_x, end_y;
  cin >> start_x >> start_y >> end_x >> end_y;

  start_x--;
  start_y--;
  end_x--;
  end_y--;

  vector<string> grid(n);
  for (int i = 0; i < n; ++i) {
    cin >> grid[i];
  }

  const int INF = 1e9;
  vector<vector<int>> dist(n, vector<int>(m, INF));
  vector<vector<Point>> parent(n, vector<Point>(m, {-1, -1, ' '}));

  priority_queue<Cell, vector<Cell>, greater<Cell>> pq;

  dist[start_x][start_y] = 0;
  pq.push({0, start_x, start_y});

  while (!pq.empty()) {
    Cell curr = pq.top();
    pq.pop();

    int x = curr.x;
    int y = curr.y;
    int d = curr.dist;

    if (d > dist[x][y])
      continue;

    if (x == end_x && y == end_y)
      break;

    for (int i = 0; i < 4; ++i) {
      int nx = x + dx[i];
      int ny = y + dy[i];

      if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] != '#' && grid[nx][ny] != ' ') {
        int weight = (grid[nx][ny] == 'W') ? 2 : 1;

        if (dist[x][y] + weight < dist[nx][ny]) {
          dist[nx][ny] = dist[x][y] + weight;
          parent[nx][ny] = {x, y, dir_chars[i]};
          pq.push({dist[nx][ny], nx, ny});
        }
      }
    }
  }

  if (dist[end_x][end_y] == INF) {
    cout << -1 << "\n";
    return 0;
  }

  cout << dist[end_x][end_y] << "\n";

  string path = "";
  int cur_x = end_x, cur_y = end_y;

  while (cur_x != start_x || cur_y != start_y) {
    path.push_back(parent[cur_x][cur_y].dir);
    Point p = parent[cur_x][cur_y];
    cur_x = p.x;
    cur_y = p.y;
  }

  reverse(path.begin(), path.end());
  cout << path << "\n";

  return 0;
}