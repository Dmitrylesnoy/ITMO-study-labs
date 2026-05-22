#include <iostream>
#include <map>
#include <set>
#include <vector>

using namespace std;

struct Chunk {
  int start;
  int size;

  bool operator<(const Chunk &other) const {
    if (size != other.size) {
      return size < other.size;
    }
    return start < other.start;
  }
};

struct AddrComp {
  bool operator()(const Chunk &a, const Chunk &b) const {
    return a.start < b.start;
  }
};

set<Chunk> by_size;
set<Chunk, AddrComp> by_addr;
map<int, Chunk> history;

void add_block(int s, int n) {
  if (n <= 0) {
    return;
  }
  Chunk c = {s, n};
  by_size.insert(c);
  by_addr.insert(c);
}

void del_block(int s, int n) {
  Chunk c = {s, n};
  by_size.erase(c);
  by_addr.erase(c);
}

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(NULL);
  int n, m;
  if (!(cin >> n >> m)) {
    return 0;
  }
  add_block(1, n);
  for (int i = 1; i <= m; ++i) {
    int req;
    cin >> req;
    if (req > 0) {
      auto it = by_size.lower_bound({0, req});
      if (it == by_size.end()) {
        cout << "-1\n";
        history[i] = {0, 0};
      } else {
        Chunk found = *it;
        del_block(found.start, found.size);
        cout << found.start << "\n";
        history[i] = {found.start, req};
        if (found.size > req) {
          add_block(found.start + req, found.size - req);
        }
      }
    } else {
      Chunk old = history[-req];
      if (old.size > 0) {
        int cur_s = old.start;
        int cur_n = old.size;
        auto it_up = by_addr.lower_bound({old.start, 0});
        if (it_up != by_addr.end() && it_up->start == cur_s + cur_n) {
          cur_n += it_up->size;
          del_block(it_up->start, it_up->size);
        }
        auto it_dn = by_addr.lower_bound({old.start, 0});
        if (it_dn != by_addr.begin()) {
          it_dn--;
          if (it_dn->start + it_dn->size == cur_s) {
            cur_s = it_dn->start;
            cur_n += it_dn->size;
            del_block(it_dn->start, it_dn->size);
          }
        }
        add_block(cur_s, cur_n);
      }
    }
  }
  return 0;
}