#include <deque>
#include <iostream>

struct GoblinQueue {
  std::deque<int> head;
  std::deque<int> tail;

  void balance() {
    if (head.size() < tail.size()) {
      head.push_back(tail.front());
      tail.pop_front();
    } else if (head.size() > tail.size() + 1) {
      tail.push_front(head.back());
      head.pop_back();
    }
  }

  void push_back(int id) {
    tail.push_back(id);
    balance();
  }

  void push_mid(int id) {
    tail.push_front(id);
    balance();
  }

  int pop_front() {
    int id = head.front();
    head.pop_front();
    balance();
    return id;
  }
};

int main() {
  std::ios_base::sync_with_stdio(false);
  std::cin.tie(nullptr);

  int n_queries;
  if (!(std::cin >> n_queries))
    return 0;

  GoblinQueue shaman_line;

  while (n_queries--) {
    char op_type;
    std::cin >> op_type;

    if (op_type == '+') {
      int goblin_id;
      std::cin >> goblin_id;
      shaman_line.push_back(goblin_id);
    } else if (op_type == '*') {
      int vip_goblin_id;
      std::cin >> vip_goblin_id;
      shaman_line.push_mid(vip_goblin_id);
    } else if (op_type == '-') {
      std::cout << shaman_line.pop_front() << "\n";
    }
  }

  return 0;
}