//Final version
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <algorithm>
#include <vector>
#include <queue>
#include <chrono>
#include <cmath>
#include <cassert>
#include <cstring>
#include <numeric>
#include <random>
constexpr int MAXN = 100 + 1;
constexpr int MAXK = 10 + 1;
constexpr int MAXT = 1000 + 1;
constexpr int MAXR = 10 + 1;
constexpr int MAXJ = 5000 + 1;
constexpr int MAXBUFFER = 1024 * 1024;
constexpr double EPS = 2e-6;
constexpr double four = 4.0 - 1e-9;
int N, K, T, R, J;
double sinr[MAXN][MAXT][MAXR][MAXK], D[MAXK][MAXR][MAXN][MAXN];
int id[MAXJ], tbs[MAXJ], belong[MAXJ], start[MAXJ], length[MAXJ];
char buffer[MAXBUFFER * 2];
const char *pointer = buffer + MAXBUFFER;
double sinr_sum[MAXN][MAXT][MAXR];
int query[MAXT][MAXN];
const auto start_time = std::chrono::high_resolution_clock::now();
struct item_t {
    double add;
    double raw;
    double log;
    double cap;
    int index;
    int state;
};
template<typename T, int maxsize> class dynamic_array {
public:
    T data[maxsize];
    int sz;
public:
    dynamic_array() : sz(0) {}
    dynamic_array(const dynamic_array<T, maxsize> &rhs) noexcept : sz(rhs.sz) {
        copy(rhs.begin(), rhs.begin() + sz, data);
    }
    dynamic_array<T, maxsize> &operator= (const dynamic_array<T, maxsize> &rhs) noexcept {
        sz = rhs.sz;
        copy(rhs.begin(), rhs.begin() + sz, data);
        return *this;
    }
    void push_back(T val) noexcept {
#ifdef __SMZ_NATIVE
        assert(sz < maxsize);
#endif
        data[sz++] = std::move(val);
    }
    void expand() noexcept {
#ifdef __SMZ_NATIVE
        assert(sz < maxsize);
#endif
        sz += 1;
    }
    void erase(T val) noexcept {
        for (int i = 0; i < sz; ++i) {
            if (data[i] == val) {
                data[i] = std::move(data[sz - 1]);
                sz -= 1;
                return;
            }
        }
    }
    void replace(T old_val, T new_val) noexcept {
        for (int i = 0; i < sz; ++i) {
            if (data[i] == old_val) {
                data[i] = new_val;
                return;
            }
        }
    }
    void set(T val) noexcept {
        data[0] = val;
        sz = 1;
    }
    bool contains(T val) const noexcept {
        for (int i = 0; i < sz; ++i) {
            if (data[i] == val) {
                return true;
            }
        }
        return false;
    }
    void pop_back() {
#ifdef __SMZ_NATIVE
        assert(sz > 0);
#endif
        sz -= 1;
    }
    void clear() noexcept {
        sz = 0;
    }
    auto size() const noexcept {
        return sz;
    }
    bool empty() const noexcept {
        return sz == 0;
    }
    auto &front() noexcept {
        return data[0];
    }
    auto &back() noexcept {
        return data[sz - 1];
    }
    auto &operator[] (int index) noexcept {
        return data[index];
    }
    auto *begin() noexcept {
        return data;
    }
    auto *end() noexcept {
        return data + sz;
    }
    const auto *begin() const noexcept {
        return data;
    }
    const auto *end() const noexcept {
        return data + sz;
    }
};
inline auto tle(const int limit=1600) noexcept {
	auto now = std::chrono::high_resolution_clock::now();
	auto runtime = std::chrono::duration_cast<std::chrono::milliseconds>(now - start_time);
    if (runtime.count() > limit) {
        return true;
    }
    return false;
}
inline int read_int() noexcept {
    const int rest = buffer + MAXBUFFER - pointer;
    if (rest < 128) {
        std::memcpy(buffer, pointer, rest);
        auto size = fread(buffer + rest, 1, MAXBUFFER - rest, stdin);
        buffer[rest + size] = 0;
        pointer = buffer;
    }
    while (!std::isdigit(*pointer)) {
        pointer += 1;
    }
    int x = 0;
    char c = *pointer++;
    while (c >= '0' && c <= '9') {
        x = x * 10 + c - '0';
        c = *pointer++;
    }
    return x;
}
inline double read_double() noexcept {
    const int rest = buffer + MAXBUFFER - pointer;
    if (rest < 128) {
        std::memcpy(buffer, pointer, rest);
        auto size = fread(buffer + rest, 1, MAXBUFFER - rest, stdin);
        buffer[rest + size] = 0;
        pointer = buffer;
    }
    double flag = 1, x = 0, p = 1;
    while (!std::isdigit(*pointer)) {
        if (*pointer == '-')
            flag = -1;
        pointer += 1;
    }
    char c = *pointer++;
    while (c >= '0' && c <= '9') {
        x = x * 10 + c - '0';
        c = *pointer++;
    }
    if (c == '.') {
        c = *pointer++;
        while (c >= '0' && c <= '9') {
            p /= 10;
            x += (c - '0') * p;
            c = *pointer++;
        }
    }
    return flag * x;
}
double power[MAXN][MAXT][MAXR][MAXK], answer[MAXN][MAXT][MAXR][MAXK];
double rest[MAXT][MAXK];
double product[MAXT][MAXK];
int thickness[MAXT], disable[MAXT][MAXR], size[MAXT][MAXK], position[MAXT][MAXR];
long long visit[MAXT][MAXK], lock[MAXT][MAXR][MAXK], froze[MAXT][MAXR], timestamp = 1;
double change[MAXN];
dynamic_array<int, MAXN> cache[MAXT][MAXR];
dynamic_array<int, MAXR> RBG[MAXT];
dynamic_array<std::tuple<int, int, int>, MAXT * MAXR * MAXK> changed;
dynamic_array<item_t, MAXT * MAXR * MAXK> nodes;
double weight[MAXN][MAXT][MAXR][MAXK], capacity[MAXN][MAXT][MAXR][MAXK], attachment[MAXN][MAXT][MAXR][MAXK];
bool processed[MAXJ] = {};
dynamic_array<int, MAXN> vect[MAXJ];
double flow = 0;
inline double calculate_weight(int n, int t, int r, int k, double answer[MAXN][MAXT][MAXR][MAXK]) noexcept {
    double numerator = sinr[n][t][r][k] * answer[n][t][r][k];
    double denominator = 1;
    for (int m = 0; m < N; ++m) if (m != n && answer[m][t][r][k] > 0) {
        numerator *= D[k][r][n][m];
    }
    for (int x = 0; x < K; ++x) if (x != k) {
        for (int m = 0; m < N; ++m) if (m != n && answer[m][t][r][x] > 0) {
            denominator += sinr[n][t][r][x] * answer[m][t][r][x] / D[x][r][n][m];
            abort();
        }
    }
    return numerator / denominator;
}
inline void check_answer(int best, double (&answer)[MAXN][MAXT][MAXR][MAXK]=answer) noexcept {
#ifdef __SMZ_NATIVE
    for (int j = 0; j < J; ++j) {
        const int n = belong[j];
        double sum = 0;
        for (int t = start[j]; t < start[j] + length[j]; ++t) {
            for (int k = 0; k < K; ++k) {
                double prod = 1.0, cnt = 0.0;
                for (int r = 0; r < R; ++r) {
                    if (answer[n][t][r][k] > 0) {
                        prod *= calculate_weight(n, t, r, k, answer);
                        cnt += 1.0;
                    }
                }
                sum += cnt * std::log2(1.0 + std::pow(prod, 1.0 / cnt));
            }
        }
        if (sum > 1e-9) {
            if (sum < tbs[j] / 192.0) {
                abort();
            }
            if (fabs(sum - tbs[j] / 192.0) > 1e-2) {
                abort();
            }
            best -= 1;
        }
    }
    if (best != 0) {
        abort();
    }
#endif
}
inline void init() noexcept {
    for (int j = 0; j < J; ++j) {
        processed[j] = false;
        const int n = belong[j];
        for (int t = start[j]; t < start[j] + length[j]; ++t) {
            for (int r = 0; r < R; ++r) {
                for (int k = 0; k < K; ++k) {
                    power[n][t][r][k] = 0;
                }
            }
        }
    }
    for (int t = 0; t < T; ++t) {
        thickness[t] = 0;
        for (int r = 0; r < R; ++r) {
            disable[t][r] = false;
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            rest[t][k] = R - EPS;
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int r = 0; r < R; ++r) {
            cache[t][r].clear();
        }
    }
}
inline void update(const int n) noexcept {
#ifdef __SMZ_NATIVE
    for (auto [t, r, k] : changed) if (lock[t][r][k] != timestamp) {
        if (power[n][t][r][k] > capacity[n][t][r][k]) {
            abort();
        }
    }
#endif
    nodes.clear();
    double number = 0;
    bool ok = false;
    for (int i = 0; i < changed.size(); ++i) {
        auto [t, r, k] = changed[i];
        if (lock[t][r][k] != timestamp) {
            auto logv = std::log2(weight[n][t][r][k]);
            nodes.push_back(item_t{
                attachment[n][t][r][k],
                weight[n][t][r][k],
                logv,
                capacity[n][t][r][k],
                i,
                0
            });
            if (power[n][t][r][k] < capacity[n][t][r][k]) {
                ok = true;
            }
            number += std::log2(1.0 + weight[n][t][r][k] * power[n][t][r][k]) - logv;
        }
    }
    if (!ok) {
        return;
    }
    std::sort(nodes.begin(), nodes.end(), [](const auto &A, const auto &B) {
        if (A.raw != B.raw) {
            return A.raw > B.raw;
        }
        return A.add < B.add;
    });
    int size = nodes.size();
    double common = std::exp2(number / size);
    for (;;) {
        for (int i = nodes.size() - 1; i >= 0; --i) if (nodes[i].state == 0) {
            if (number < -nodes[i].log * size) {
                number += nodes[i].log;
                size -= 1;
                nodes[i].state = -1;
            }
            else {
                break;
            }
        }
        double common = std::exp2(number / size);
        bool flag = false;
        for (int i = 0; i < nodes.size(); ++i) if (nodes[i].state == 0) {
            if (common - 1.0 / nodes[i].raw > nodes[i].cap) {
                auto [t, r, k] = changed[nodes[i].index];
                rest[t][k] += power[n][t][r][k] - nodes[i].cap;
                flow += nodes[i].cap - power[n][t][r][k];
                power[n][t][r][k] = nodes[i].cap;
                number -= log2(1.0 + nodes[i].cap * nodes[i].raw);
                number += nodes[i].log;
                nodes[i].state = 1;
                size -= 1;
                flag = true;
                common = std::exp2(number / size);
            }
        }
        for (int i = 0; i < nodes.size(); ++i) if (nodes[i].state == -1) {
            if (number > -nodes[i].log * size) {
                number -= nodes[i].log;
                size += 1;
                nodes[i].state = 0;
            }
            else {
                break;
            }
        }
        if (!flag) {
            goto finish;
        }
    }
    finish:;
    common = std::exp2(number / size);
    for (int i = 0; i < nodes.size(); ++i) {
        auto [t, r, k] = changed[nodes[i].index];
        if (nodes[i].state == 0) {
            double value = common - 1.0 / nodes[i].raw;
            if (value + 5e-7 < nodes[i].cap) {
                value += 5e-7;
            }
            rest[t][k] += power[n][t][r][k] - value;
            flow += value - power[n][t][r][k];
            power[n][t][r][k] = value;
        }
        else if (nodes[i].state == -1) {
            rest[t][k] += power[n][t][r][k];
            flow -= power[n][t][r][k];
            power[n][t][r][k] = 0;
            cache[t][r].erase(n);
            if (k == position[t][r] && !disable[t][r]) {
                for (auto m : cache[t][r]) {
                    auto aim = power[m][t][r][k] * D[k][r][n][m];
                    rest[t][k] += power[m][t][r][k] - aim;
                    flow -= power[m][t][r][k] - aim;
                    power[m][t][r][k] = aim;
                }
            }
            if (cache[t][r].empty()) {
                thickness[t] -= 1;
            }
            if (froze[t][r]) {
                if (cache[t][r].size() == 0) {
                    disable[t][r] = false;
                }
            }
            else {
                if (cache[t][r].size() <= 1) {
                    disable[t][r] = false;
                    for (int k = 0; k < K; ++k) {
                        if (power[n][t][r][k] > 0) {
                            position[t][r] = k;
                            break;
                        }
                    }
                }
            }
        }
    }
}
inline double add(int j, const double step=0.5) noexcept {
    const int n = belong[j];
    const double tbs = ::tbs[j] / 192.0;
    double ret = 0, sum = 0;
    std::priority_queue<std::tuple<double, int, int>> candidates;
    std::vector<std::tuple<int, int, int, int, double>> backup;
    timestamp += 1;
    flow = 0;
    changed.clear();
    for (int t = start[j]; t < start[j] + length[j]; ++t) {
        RBG[t].clear();
        for (int r = 0; r < R; ++r) {
            if (cache[t][r].size() == 0) {
                candidates.emplace(sinr_sum[n][t][r], t, r);
            }
            else if (!disable[t][r]) {
                const int k = position[t][r];
                double cof = sinr[n][t][r][k] / cache[t][r].size();
                for (auto m : cache[t][r]) {
                    cof *= D[k][r][n][m];
                }
                double value = std::log2(1.0 + step * cof) * 4 / step;
                candidates.emplace(value, t, r);
            }
        }
    }
    while (candidates.size()) {
        auto [tmp, t, r] = candidates.top(); candidates.pop();
        if (cache[t][r].empty()) {
            RBG[t].push_back(r);
            if (RBG[t].size() == 1) {
                for (int k = 0; k < K; ++k) {
                    product[t][k] = 1.0;
                    size[t][k] = 0;
                }
            }
            dynamic_array<int, MAXK> indices;
            for (int k = 0; k < K; ++k) if (rest[t][k] > 0 && visit[t][k] != -timestamp) {
                indices.push_back(k);
            }
            std::sort(indices.begin(), indices.end(), [rest=::rest[t], sinr=::sinr[n][t][r]](int x, int y) {
                if (rest[x] != rest[y])
                    return rest[x] > rest[y];
                else
                    return sinr[x] > sinr[y];
            });
            int modify = 0;
            for (int i = 0; i < indices.size(); ++i) {
                int k = indices[i];
                if (rest[t][k] <= 0) {
                    continue;
                }
                double distribute = std::min(rest[t][k], 1.0);
                if (thickness[t] + modify == R - 1) {
                    distribute = std::min(rest[t][k], 4.0);
                }
                attachment[n][t][r][k] = 0;
                if (RBG[t].size() == 1) {
                    if (distribute > 0) {
                        power[n][t][r][k] += distribute;
                        capacity[n][t][r][k] = power[n][t][r][k];
                        ret += 3;
                        flow += distribute;
                        rest[t][k] -= distribute;
                        product[t][k] *= power[n][t][r][k] * sinr[n][t][r][k];
                        size[t][k] += 1;
                        sum += std::log2(1.0 + power[n][t][r][k] * sinr[n][t][r][k]);
                        visit[t][k] = timestamp;
                        cache[t][r].push_back(n);
                        changed.push_back(std::make_tuple(t, r, k));
                        weight[n][t][r][k] = sinr[n][t][r][k];
                        position[t][r] = k;
                        if (cache[t][r].size() == 1) {
                            thickness[t] += 1;
                            modify -= 1;
                        }
                        if (cache[t][r].size() >= 2) {
                            disable[t][r] = true;
                        }
                        if (sum > tbs) {
                            double newval = tbs - sum + std::log2(1.0 + power[n][t][r][k] * sinr[n][t][r][k]);
                            double delta = (std::exp2(newval) - 1) / sinr[n][t][r][k] + EPS - power[n][t][r][k];
                            delta = std::min(delta, 0.0);
                            power[n][t][r][k] += delta;
                            flow += delta;
                            rest[t][k] -= delta;
                            if (cache[t][r].size() >= 2) for (int x = i + 1; x < indices.size(); ++x) {
                                double distribute = std::min(rest[t][indices[x]], 1.0);
                                if (thickness[t] + modify == R - 1) {
                                    distribute = std::min(rest[t][indices[x]], 4.0);
                                }
                                if (distribute > 0) {
                                    cache[t][r].push_back(n);
                                    changed.push_back(std::make_tuple(t, r, indices[x]));
                                    weight[n][t][r][indices[x]] = sinr[n][t][r][indices[x]];
                                    capacity[n][t][r][indices[x]] = distribute;
                                }
                            }
                            else {
                                ret -= 2;
                            }
                            goto finish;
                        }
                    }
                    continue;
                }
                double factor = 1.0, tot = 0.0;
                for (auto s : RBG[t]) if (power[n][t][s][k] > 0) {
                    tot += power[n][t][s][k];
                    factor /= power[n][t][s][k];
                }
                double avg = (tot + distribute) / size[t][k];
                factor *= std::pow(avg, size[t][k]);
                if (avg > 4.0) {
                    factor = 0;
                }
                double inner1 = std::pow(product[t][k] * factor, 1.0 / size[t][k]);
                double val1 = std::log2(1.0 + inner1) * size[t][k];
                double inner2 = std::pow(product[t][k] * sinr[n][t][r][k] * distribute, 1.0 / (size[t][k] + 1));
                double val2 = std::log2(1.0 + inner2) * (size[t][k] + 1);
                double tmp = std::pow(product[t][k], 1.0 / size[t][k]);
                double previous = std::log2(1.0 + tmp) * size[t][k];
                double sum_old = sum - previous;
                if (val1 > val2 && val1 > previous) {
                    sum -= previous;
                    sum += val1;
                    for (auto s : RBG[t]) if (power[n][t][s][k] > 0) {
                        power[n][t][s][k] = avg;
                        capacity[n][t][s][k] = avg;
                    }
                    product[t][k] *= factor;
                    rest[t][k] -= distribute;
                    flow += distribute;
                    ret += 3;
                }
                else if (val2 > previous) {
                    sum -= previous;
                    sum += val2;
                    power[n][t][r][k] = distribute;
                    flow += distribute;
                    capacity[n][t][r][k] = power[n][t][r][k];
                    product[t][k] *= power[n][t][r][k] * sinr[n][t][r][k];
                    size[t][k] += 1;
                    rest[t][k] -= distribute;
                    ret += 3;
                }
                if (power[n][t][r][k] > 0) {
                    visit[t][k] = timestamp;
                    cache[t][r].push_back(n);
                    changed.push_back(std::make_tuple(t, r, k));
                    weight[n][t][r][k] = sinr[n][t][r][k];
                    if (cache[t][r].size() == 1) {
                        thickness[t] += 1;
                        modify -= 1;
                    }
                    for (auto s : RBG[t]) if (cache[t][s].size()) {
                        disable[t][s] = true;
                        lock[t][s][k] = timestamp;
                        froze[t][s] = timestamp;
                    }
                }
                if (sum > tbs) {
                    if (sum > tbs + EPS) {
                        double newval = tbs + EPS - sum_old;
                        double pw = std::pow(std::exp2(newval / size[t][k]) - 1.0, size[t][k]);
                        for (auto s : RBG[t]) if (power[n][t][s][k] > 0) {
                            pw /= sinr[n][t][s][k];
                        }
                        pw = std::pow(pw, 1.0 / size[t][k]);
                        for (auto s : RBG[t]) if (power[n][t][s][k] > 0) {
                            rest[t][k] += power[n][t][s][k] - pw;
                            flow += pw - power[n][t][s][k];
                            power[n][t][s][k] = pw;
                        }
                    }
                    goto finish;
                }
            }
        }
        else {
            const int k = position[t][r];
            if (rest[t][k] <= 0) {
                continue;
            }
            if (power[n][t][r][k] == 0) {
                if (visit[t][k] == timestamp || visit[t][k] == -timestamp) {
                    continue;
                }
                double tot = 0, pw = 0;
                for (auto m : cache[t][r]) {
                    auto delta = power[m][t][r][k] / D[k][r][n][m] - power[m][t][r][k];
                    change[m] = delta;
                    tot += delta;
                    pw += power[m][t][r][k] + delta;
                }
                if (rest[t][k] - tot <= EPS || pw > 4.0) {
                    continue;
                }
                visit[t][k] = -timestamp;
                attachment[n][t][r][k] = 0;
                auto cof = sinr[n][t][r][k];
                for (auto m : cache[t][r]) {
                    backup.emplace_back(m, t, r, k, change[m]);
                    power[m][t][r][k] += change[m];
                    attachment[n][t][r][k] += change[m];
                    cof *= D[k][r][n][m];
                }
                cache[t][r].push_back(n);
                changed.push_back(std::make_tuple(t, r, k));
                weight[n][t][r][k] = cof;
                auto need = (std::exp2(tbs - sum) - 1.0) / cof + EPS;
                power[n][t][r][k] = std::min({rest[t][k] - tot, four - pw, need, step});
                flow += tot + power[n][t][r][k];
                capacity[n][t][r][k] = std::min({rest[t][k] - tot, four - pw});
                sum += std::log2(1.0 + power[n][t][r][k] * cof);
                rest[t][k] -= tot + power[n][t][r][k];
                double value = std::log2(1.0 + (power[n][t][r][k] + step) * cof / cache[t][r].size()) - std::log2(1.0 + power[n][t][r][k] * cof / cache[t][r].size());
                candidates.emplace(value * (capacity[n][t][r][k] - power[n][t][r][k]) / step, t, r);
                ret += 1;
            }
            else {
                sum -= std::log2(1.0 + power[n][t][r][k] * weight[n][t][r][k]);
                auto need = (std::exp2(tbs - sum) - 1.0) / weight[n][t][r][k] + EPS;
                double distribute = std::min({capacity[n][t][r][k] - power[n][t][r][k], need - power[n][t][r][k], step}); 
                power[n][t][r][k] = std::min(power[n][t][r][k] + distribute, capacity[n][t][r][k]);
                flow += distribute;
                double contribution = std::log2(1.0 + power[n][t][r][k] * weight[n][t][r][k]);
                sum += contribution;
                rest[t][k] -= distribute;
                if (distribute > 0) {
                    double value = std::log2(1.0 + (power[n][t][r][k] + step) * weight[n][t][r][k] / cache[t][r].size()) - std::log2(1.0 + power[n][t][r][k] * weight[n][t][r][k] / cache[t][r].size());
                    candidates.emplace(value * (capacity[n][t][r][k] - power[n][t][r][k]) / step, t, r);
                }
            }
            if (sum > tbs) {
                goto finish;
            }
        }
    }
    finish:;
    if (sum < tbs) {
        for (auto [m, t, r, k, delta] : backup) {
            rest[t][k] += delta;
            power[m][t][r][k] -= delta;
        }
        for (auto [t, r, k] : changed) {
            disable[t][r] = false;
            rest[t][k] += power[n][t][r][k];
            power[n][t][r][k] = 0;
            cache[t][r].pop_back();
            if (cache[t][r].empty()) {
                thickness[t] -= 1;
            }
        }
        return 0.0;
    }
    update(n);
    return ret;
}
inline double undo(int j) noexcept {
    const int n = belong[j];
    double ret = 0;
    processed[j] = false;
    for (int t = start[j]; t < start[j] + length[j]; ++t) {
        for (int r = 0; r < R; ++r) {
            if (disable[t][r]) {
                if (cache[t][r].front() == n) {
                    disable[t][r] = false;
                    thickness[t] -= 1;
                    cache[t][r].clear();
                    for (int k = 0; k < K; ++k) {
                        if (power[n][t][r][k] > 0) {
                            rest[t][k] += power[n][t][r][k];
                            ret += power[n][t][r][k];
                            power[n][t][r][k] = 0;
                        }
                    }
                }
            }
            else if (cache[t][r].size()) {
                const int k = position[t][r];
                if (power[n][t][r][k] > 0) {
                    rest[t][k] += power[n][t][r][k];
                    ret += power[n][t][r][k];
                    power[n][t][r][k] = 0;
                    cache[t][r].erase(n);
                    for (auto m : cache[t][r]) {
                        auto aim = power[m][t][r][k] * D[k][r][n][m];
                        rest[t][k] += power[m][t][r][k] - aim;
                        ret += power[m][t][r][k] - aim;
                        power[m][t][r][k] = aim;
                    }
                    if (cache[t][r].empty()) {
                        thickness[t] -= 1;
                    }
                }
            }
        }
    }
    return ret;
}
inline void resume() noexcept {
    init();
    for (int j = 0; j < J; ++j) {
        const int n = belong[j];
        for (int t = start[j]; t < start[j] + length[j]; ++t) {
            for (int r = 0; r < R; ++r) {
                int sum = 0;
                for (int k = 0; k < K; ++k) {
                    if (answer[n][t][r][k] > 0) {
                        power[n][t][r][k] = answer[n][t][r][k];
                        rest[t][k] -= power[n][t][r][k];
                        cache[t][r].push_back(n);
                        position[t][r] = k;
                        processed[query[t][n]] = true;
                        sum += 1;
                        if (cache[t][r].size() == 1) {
                            thickness[t] += 1;
                        }
                    }
                }
                if (sum >= 2) {
                    disable[t][r] = true;
                }
            }
            for (int k = 0; k < K; ++k) {
                dynamic_array<int, MAXR> row;
                for (int r = 0; r < R; ++r) {
                    if (power[n][t][r][k] > 0) {
                        row.push_back(r);
                    }
                }
                if (row.size() >= 2) {
                    for (auto r : row) {
                        disable[t][r] = true;
                    }
                }
            }
        }
    }
}
inline void resume(const dynamic_array<int, MAXJ> &vec) noexcept {
    for (auto j : vec) {
        processed[j] = true;
        const int n = belong[j];
        for (int t = start[j]; t < start[j] + length[j]; ++t) {
            for (int r = 0; r < R; ++r) {
                if (cache[t][r].size() && !disable[t][r]) {
                    int k = position[t][r];
                    if (answer[n][t][r][k] > 0) {
                        for (auto m : cache[t][r]) {
                            auto aim = answer[m][t][r][k];
                            rest[t][k] += power[m][t][r][k] - aim;
                            power[m][t][r][k] = aim;
                        }
                    }
                }
                int sum = 0;
                for (int k = 0; k < K; ++k) {
                    if (answer[n][t][r][k] > 0) {
                        rest[t][k] += power[n][t][r][k] - answer[n][t][r][k];
                        power[n][t][r][k] = answer[n][t][r][k];
                        cache[t][r].push_back(n);
                        position[t][r] = k;
                        sum += 1;
                        if (cache[t][r].size() == 1) {
                            thickness[t] += 1;
                        }

                    }
                }
                if (sum >= 2) {
                    disable[t][r] = true;
                }
            }
            for (int k = 0; k < K; ++k) {
                dynamic_array<int, MAXR> row;
                for (int r = 0; r < R; ++r) {
                    if (power[n][t][r][k] > 0) {
                        row.push_back(r);
                    }
                }
                if (row.size() >= 2) {
                    for (auto r : row) {
                        disable[t][r] = true;
                    }
                }
            }
        }
    }
}
inline void check(int best) noexcept {
#ifdef __SMZ_NATIVE
    for (int t = 0; t < T; ++t) {
        int sum = 0;
        for (int r = 0; r < R; ++r) {
            if (cache[t][r].size()) {
                sum += 1;
            }
        }
        if (thickness[t] != sum) {
            abort();
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            if (rest[t][k] <= -EPS) {
                abort();
            }
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            double sum = 0;
            for (int r = 0; r < R; ++r) {
                double total = 0;
                for (int n = 0; n < N; ++n) {
                    sum += power[n][t][r][k];
                    total += power[n][t][r][k];
                    if (power[n][t][r][k] < 0) {
                        abort();
                    }
                }
                if (total > 4.0) {
                    abort();
                }
            }
            if (sum > R) {
                abort();
            }
            if (fabs(rest[t][k] - (R - sum)) > 1e-5) {
                printf("[rest] %f %f\n", rest[t][k], R - sum);
                abort();
            }
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int r = 0; r < R; ++r) {
            int vis[MAXK] = {};
            for (int k = 0; k < K; ++k) {
                for (int n = 0; n < N; ++n) {
                    if (power[n][t][r][k] > 0) {
                        vis[k] = true;
                    }
                }
            }
            if (std::accumulate(vis, vis + K, 0) >= 2) {
                if (!disable[t][r]) {
                    abort();
                }
            }
            if (cache[t][r].size() == 0 && disable[t][r]) {
                abort();
            }
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int n = 0; n < N; ++n) {
            for (int k = 0; k < K; ++k) {
                dynamic_array<int, MAXR> row;
                for (int r = 0; r < R; ++r) {
                    if (power[n][t][r][k] > 0) {
                        row.push_back(r);
                    }
                }
                if (row.size() >= 2) {
                    for (auto r : row) {
                        if (!disable[t][r]) {
                            abort();
                        }
                    }
                }
            }
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int r = 0; r < R; ++r) {
            if (disable[t][r]) {
                continue;
            }
            int k = position[t][r];
            for (auto n : cache[t][r]) {
                if (power[n][t][r][k] == 0) {
                    abort();
                }
            }
        }
    }
    check_answer(best, power);
#endif
}
inline void save() noexcept {
    for (int j = 0; j < J; ++j) {
        const int n = belong[j];
        for (int t = start[j]; t < start[j] + length[j]; ++t) {
            for (int r = 0; r < R; ++r) {
                for (int k = 0; k < K; ++k) {
                    answer[n][t][r][k] = power[n][t][r][k];
                }
            }
        }
    }
}
void solve() noexcept {
    using item_t = std::pair<double, int>;
    std::priority_queue<item_t, std::vector<item_t>, std::greater<item_t>> queue;
    std::mt19937 engine;
    for (int j = 0; j < J; ++j) {
        queue.emplace(0.0, j);
    }
    queue.emplace(int(1e9), -1);
    std::vector<int> indices;
    for (int j = 0; j < J; ++j) {
        indices.push_back(j);
    }
    int best = 0, cnt = 0;
    init();
    while (queue.size() > 1 && !tle(600)) {
        auto [prev, j] = queue.top(); queue.pop();
        auto val = add(j);
#ifdef __SMZ_NATIVE
        check(cnt + bool(val));
#endif
        if (val == 0) {
            continue;
        }
        if (prev == 0 || val > queue.top().first) {
            undo(j);
            queue.emplace(val, j);
        }
        else {
            processed[j] = true;
            cnt += 1;
#ifdef __SMZ_NATIVE
            check(cnt);
#endif
        }
    }
    while (queue.size() > 1) {
        auto [_, j] = queue.top(); queue.pop();
        if (tle()) {
            goto next;
        }
        processed[j] = add(j);
        cnt += processed[j];
#ifdef __SMZ_NATIVE
        check(cnt);
#endif
    }
    next: if (cnt > best) {
        best = cnt;
        save();
    }
    while (!tle()) {
#ifdef __SMZ_NATIVE
        check(cnt);
#endif
        for (auto j : indices) {
            if (processed[j]) {
                processed[j] = false;
                undo(j);
                cnt -= 1;
                if (tle()) {
                    goto finish;
                }
#ifdef __SMZ_NATIVE
                check(cnt);
#endif
            }
            processed[j] = add(j);
            cnt += processed[j];
#ifdef __SMZ_NATIVE
            check(cnt);
#endif
        }
        if (cnt > best) {
            best = cnt;
            save();
        }
        else {
            break;
        }
        shuffle(indices.begin(), indices.end(), engine);
    }
    if (!tle()) {
        resume();
        cnt = best;
    }
    while (!tle(600)) {
        for (int t = 0; t < T && !tle(600); ++t) {
            for (int r = 0; r < R; ++r) {
                if (disable[t][r] || cache[t][r].empty()) {
                    continue;
                }
                dynamic_array<int, MAXJ> deleted, inserted;
                int k = position[t][r];
                for (auto n : cache[t][r]) {
                    if (tle()) {
                        goto finish;
                    }
                    int j = query[t][n];
                    undo(j);
                    cnt -= 1;
                    deleted.push_back(j);
                }
                shuffle(vect[t].begin(), vect[t].end(), engine);
                for (auto j : vect[t]) {
                    if (!processed[j]) {
                        if (tle()) {
                            goto finish;
                        }
                        processed[j] = add(j);
                        cnt += processed[j];
                        if (processed[j]) {
                            inserted.push_back(j);
                        }
                    }
                }
                if (cnt > best) {
                    best = cnt;
                    save();
                }
                else {
                    for (auto j : inserted) {
                        undo(j);
                        cnt -= 1;
                    }
#ifdef __SMZ_NATIVE
                    check(cnt);
#endif
                    resume(deleted);
                    cnt += deleted.size();
#ifdef __SMZ_NATIVE
                    check(cnt);
                    if (cnt != best) {
                        abort();
                    }
#endif
                }
            }
        }
    }
    while (!tle()) {
        std::vector<int> times;
        for (int t = 0; t < T; ++t) {
            times.push_back(t);
        }
        std::shuffle(times.begin(), times.end(), engine);
        for (auto t : times) {
            dynamic_array<int, MAXJ> deleted, inserted;
            double delta = 0;
            int fail = 0;
            for (auto j : vect[t]) if (!processed[j]) {
                fail += 1;
            }
            if (fail == 0) {
                continue;
            }
            for (auto j : vect[t]) if (processed[j] && rand() % 2 == 0) {
                if (tle()) {
                    goto finish;
                }
                delta -= undo(j);
                cnt -= 1;
                deleted.push_back(j);
            }
            shuffle(vect[t].begin(), vect[t].end(), engine);
            for (auto j : vect[t]) {
                if (!processed[j]) {
                    if (tle()) {
                        goto finish;
                    }
                    processed[j] = add(j);
                    cnt += processed[j];
                    if (processed[j]) {
                        delta += flow;
                        inserted.push_back(j);
                    }
                    else {
                        fail -= 1;
                        if (fail < 0) {
                            break;
                        }
                    }
                }
            }
            if (cnt > best || (cnt == best && delta < 0)) {
                best = cnt;
                save();
            }
            else {
                for (auto j : inserted) {
                    undo(j);
                    cnt -= 1;
                }
                if (tle()) {
                    goto finish;
                }
#ifdef __SMZ_NATIVE
                check(cnt);
#endif
                resume(deleted);
                cnt += deleted.size();
#ifdef __SMZ_NATIVE
                check(cnt);
                if (cnt != best) {
                    abort();
                }
#endif
            }
        }
    }
    finish:;
#ifdef __SMZ_NATIVE
    printf("%d\n", best);
    check_answer(best);
#else
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            for (int r = 0; r < R; ++r) {
                char *pointer = buffer;
                for (int n = 0; n < N; ++n) {
                    std::sprintf(pointer, "%.8f ", answer[n][t][r][k]);
                    while (*pointer) {
                        ++pointer;
                    }
                }
                *pointer++ = '\n';
                *pointer = 0;
                std::fwrite(buffer, 1, pointer - buffer, stdout);
            }
        }
    }
    std::fflush(stdout);
#endif
}
void preprocess() noexcept {
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            for (int r = 0; r < R; ++r) {
                for (int n = 0; n < N; ++n) {
                    sinr_sum[n][t][r] += std::log2(1.0 + sinr[n][t][r][k]);
                }
            }
        }
    }
    for (int j = 0; j < J; ++j) {
        for (int t = start[j]; t < start[j] + length[j]; ++t) {
            const int n = belong[j];
            query[t][n] = j;
            vect[t].push_back(j);
        }
    }
}
int main() {
#ifdef __SMZ_NATIVE
    freopen("in2.txt", "r", stdin);
#endif
    ::N = read_int();
    ::K = read_int();
    ::T = read_int();
    ::R = read_int();
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            for (int r = 0; r < R; ++r) {
                for (int n = 0; n < N; ++n) {
                    sinr[n][t][r][k] = read_double();
                }
            }
        }
    }
    for (int i = 0; i < K; ++i) {
        for (int j = 0; j < R; ++j) {
            for (int x = 0; x < N; ++x) {
                for (int y = 0; y < N; ++y) {
                    D[i][j][x][y] = std::exp(read_double());
                }
            }
        }
    }
    ::J = read_int();
    for (int i = 0; i < J; ++i) {
        id[i] = read_int();
        tbs[i] = read_int();
        belong[i] = read_int();
        start[i] = read_int();
        length[i] = read_int();
    }
    preprocess();
    solve();
    return 0;
}