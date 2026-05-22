#include <iostream>
#include <queue>
#include <set>
#include <vector>

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int N, K, P;
    if (!(std::cin >> N >> K >> P)) {
        return 0;
    }

    std::vector<int> petyas_wishlist(P);
    std::vector<std::queue<int>> future_dates(N + 1);

    for (int i = 0; i < P; ++i) {
        std::cin >> petyas_wishlist[i];
        future_dates[petyas_wishlist[i]].push(i);
    }

    std::set<std::pair<int, int>> floor_party;
    std::vector<bool> is_on_floor(N + 1, false);
    int total_mom_efforts = 0;

    for (int i = 0; i < P; ++i) {
        int current_car = petyas_wishlist[i];
        future_dates[current_car].pop();

        int next_time_petya_screams =
            future_dates[current_car].empty() ? P + 1 : future_dates[current_car].front();

        if (is_on_floor[current_car]) {
            floor_party.erase(floor_party.find({i, current_car}));
            floor_party.insert({next_time_petya_screams, current_car});
        } else {
            if (static_cast<size_t>(floor_party.size()) >= static_cast<size_t>(K)) {
                auto boring_car = std::prev(floor_party.end());
                is_on_floor[boring_car->second] = false;
                floor_party.erase(boring_car);
            }

            is_on_floor[current_car] = true;
            floor_party.insert({next_time_petya_screams, current_car});
            ++total_mom_efforts;
        }
    }

    std::cout << total_mom_efforts << '\n';

    return 0;
}