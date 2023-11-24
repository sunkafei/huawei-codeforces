#include <bits/stdc++.h>
#include <sys/time.h>
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
int order[MAXN][MAXT][MAXR];
uint64_t start_time;
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
                data[i] = move(data[sz - 1]);
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
inline auto mutime() {
    timeval v;
    gettimeofday(&v, nullptr);
    return v.tv_usec + v.tv_sec * 1000000;
}
inline auto tle() noexcept {
    auto now = mutime();
    auto runtime = now - start_time;
    if (runtime > 1500 * 1000) {
        return true;
    }
    return false;
}
inline int read_int() {
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
inline double read_double() {
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
int cover[MAXT][MAXR], thickness[MAXT], disable[MAXT][MAXR], size[MAXT][MAXK];
long long visit[MAXT][MAXK], timestamp = 1;
double change[MAXN];
dynamic_array<std::pair<int, int>, MAXK * MAXN> cache[MAXT][MAXR];
inline double calculate_weight(int n, int t, int r, int k) {
    double numerator = sinr[n][t][r][k] * answer[n][t][r][k];
    double denominator = 1;
    for (int m = 0; m < N; ++m) if (m != n && answer[m][t][r][k] > 0) {
        numerator *= D[k][r][n][m];
    }
    for (int x = 0; x < K; ++x) if (x != k) {
        for (int m = 0; m < N; ++m) if (m != n && answer[m][t][r][x] > 0) {
            denominator += sinr[n][t][r][x] * answer[m][t][r][x] / D[x][r][n][m];
        }
    }
    return numerator / denominator;
}
inline void check_answer(int best) {
    for (int j = 0; j < J; ++j) {
        const int n = belong[j];
        double sum = 0;
        for (int t = start[j]; t < start[j] + length[j]; ++t) {
            for (int k = 0; k < K; ++k) {
                double prod = 1.0, cnt = 0.0;
                for (int r = 0; r < R; ++r) {
                    if (answer[n][t][r][k] > 0) {
                        prod *= calculate_weight(n, t, r, k);
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
}
inline void init() {
    for (int n = 0; n < N; ++n) {
        for (int t = 0; t < T; ++t) {
            for (int r = 0; r < R; ++r) {
                for (int k = 0; k < K; ++k) {
                    power[n][t][r][k] = 0;
                }
            }
        }
    }
    for (int t = 0; t < T; ++t) {
        thickness[t] = 0;
        for (int r = 0; r < MAXR; ++r) {
            cover[t][r] = 0;
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
inline bool add(int j, int limit) {
    static dynamic_array<std::tuple<int, double, int, int>, MAXT> candidates; candidates.clear();
    static dynamic_array<std::tuple<double, int, int, int>, MAXT * MAXR> cells; cells.clear();
    static dynamic_array<int, MAXR> RBG[MAXT];
    timestamp += 1;
    const int n = belong[j];
    double tbs = ::tbs[j] / 192.0;
    for (int t = start[j]; t < start[j] + length[j]; ++t) {
        RBG[t].clear();
        int counter = 0;
        for (int i = 0; i < R; ++i) {
            const int r = order[n][t][i];
            if (!cover[t][r]) {
                candidates.push_back(std::make_tuple(thickness[t], -sinr_sum[n][t][r], t, r));
                if (++counter >= limit) {
                    break;
                }
            }
        }
    }
    std::sort(candidates.begin(), candidates.end());
    double sum = 0;
    std::vector<std::tuple<int, int, int, int, double>> backup;
    for (auto [_, val, t, r] : candidates) {
        thickness[t] += 1;
        RBG[t].push_back(r);
        if (RBG[t].size() == 1) {
            for (int k = 0; k < K; ++k) {
                product[t][k] = 1.0;
                size[t][k] = 0;
            }
        }
        dynamic_array<int, MAXK> indices;
        for (int i = 0; i < K; ++i) {
            indices.push_back(i);
        }
        std::sort(indices.begin(), indices.end(), [rest=::rest[t], sinr=::sinr[n][t][r]](int x, int y) {
            if (rest[x] != rest[y])
                return rest[x] > rest[y];
            else
                return sinr[x] > sinr[y];
        });
        for (auto k : indices) {
            double distribute = std::min(rest[t][k], 1.0);
            if (thickness[t] == R && limit == R) {
                distribute = std::min(rest[t][k], 4.0);
            }
            if (RBG[t].size() == 1) {
                if (distribute > 0) {
                    power[n][t][r][k] += distribute;
                    rest[t][k] -= distribute;
                    product[t][k] *= power[n][t][r][k] * sinr[n][t][r][k];
                    size[t][k] += 1;
                    sum += std::log2(1.0 + power[n][t][r][k] * sinr[n][t][r][k]);
                    cover[t][r] += 1;
                    visit[t][k] = timestamp;
                    cache[t][r].push_back(std::make_pair(k, n));
                    if (sum > tbs) {
                        double newval = tbs - sum + std::log2(1.0 + power[n][t][r][k] * sinr[n][t][r][k]);
                        double delta = (std::exp2(newval) - 1) / sinr[n][t][r][k] + EPS - power[n][t][r][k];
                        delta = std::min(delta, 0.0);
                        power[n][t][r][k] += delta;
                        rest[t][k] -= delta;
                        goto finish;
                    }
                }
                continue;
            }
            double old_inner = std::pow(product[t][k], 1.0 / size[t][k]);
            double old_val = std::log2(1.0 + old_inner) * size[t][k];
            double new_inner = std::pow(product[t][k] * sinr[n][t][r][k] * distribute, 1.0 / (size[t][k] + 1));
            double new_val = std::log2(1.0 + new_inner) * (size[t][k] + 1);
            if (new_val > old_val) {
                sum -= old_val;
                sum += new_val;
                power[n][t][r][k] = distribute;
                rest[t][k] -= power[n][t][r][k];
                product[t][k] *= power[n][t][r][k] * sinr[n][t][r][k];
                size[t][k] += 1;
            }
            if (power[n][t][r][k] > 0) {
                cover[t][r] += 1;
                visit[t][k] = timestamp;
                cache[t][r].push_back(std::make_pair(k, n));
                for (auto s : RBG[t]) {
                    disable[t][s] = true;
                }
            }
            if (sum > tbs) {
                if (sum > tbs + EPS) {
                    double newval = tbs + EPS - sum + new_val;
                    double pw = std::pow(std::exp2(newval / size[t][k]) - 1.0, size[t][k]);
                    for (auto s : RBG[t]) {
                        pw /= sinr[n][t][s][k];
                    }
                    pw = std::pow(pw, 1.0 / size[t][k]);
                    for (auto s : RBG[t]) {
                        rest[t][k] += power[n][t][s][k] - pw;
                        power[n][t][s][k] = pw;
                    }
                }
                goto finish;
            }
        }
    }
    if (sum < tbs) {
        for (int t = start[j]; t < start[j] + length[j]; ++t) {
            for (int r = 0; r < R; ++r) if (cover[t][r] == 1 && !disable[t][r]) {
                int k = cache[t][r].front().first;
                if (power[n][t][r][k] > 0 || visit[t][k] == timestamp) {
                    continue;
                }
                double value = -sinr[n][t][r][k];
                for (auto [_, m] : cache[t][r]) {
                    value *= D[k][r][n][m];
                }
                cells.push_back(std::make_tuple(value, t, r, k));
            }
        }
        std::sort(cells.begin(), cells.end());
        int counter = 0;
        for (auto [_, t, r, k] : cells) {
            if (visit[t][k] == timestamp) {
                continue;
            }
            visit[t][k] = timestamp;
            double tot = 0, pw = 0;
            for (auto [x, m] : cache[t][r]) {
                auto cof = sinr[m][t][r][k];
                for (auto [y, u] : cache[t][r]) {
                    if (u != m) {
                        cof *= D[k][r][m][u];
                    }
                }
                auto value = std::log2(1.0 + power[m][t][r][k] * cof);
                auto delta = (std::exp2(value) - 1.0) / cof / D[k][r][n][m] - power[m][t][r][k];
                change[m] = delta;
                tot += delta;
                pw += power[m][t][r][k] + delta;
            }
            if (rest[t][k] - tot <= EPS || pw > 4.0) {
                continue;
            }
            auto cof = sinr[n][t][r][k];
            for (auto [x, m] : cache[t][r]) {
                backup.emplace_back(m, t, r, k, change[m]);
                power[m][t][r][k] += change[m];
                cof *= D[k][r][n][m];
            }
            cache[t][r].push_back(std::make_pair(k, n));
            auto need = (std::exp2(tbs - sum) - 1.0) / cof + EPS;
            power[n][t][r][k] = std::min({rest[t][k] - tot, four - pw, need});
            sum += std::log2(1.0 + power[n][t][r][k] * cof);
            rest[t][k] -= tot + power[n][t][r][k];
            if (sum > tbs) {
                goto finish;
            }
            if (++counter >= limit) {
                break;
            }
        }
    }
    finish:;
    if (sum > tbs) {
        return true;
    }
    for (auto [m, t, r, k, delta] : backup) {
        rest[t][k] += delta + power[n][t][r][k];
        power[m][t][r][k] -= delta;
        if (power[n][t][r][k] > 0) {
            cache[t][r].pop_back();
        }
        power[n][t][r][k] = 0;
    }
    for (auto [_, val, t, r] : candidates) {
        thickness[t] -= 1;
        disable[t][r] = false;
        for (int k = 0; k < K; ++k) {
            if (power[n][t][r][k] > 0) {
                cover[t][r] -= 1;
                rest[t][k] += power[n][t][r][k];
                power[n][t][r][k] = 0;
                cache[t][r].pop_back();
            }
        }
    }
    return false;
}
inline void check() {
#ifdef __SMZ_NATIVE
    for (int t = 0; t < T; ++t) {
        if (thickness[t] < 0) {
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
    int counter = 0;
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
                    if (power[n][t][r][k] > 0) {
                        counter += 1;
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
            for (auto [k, n] : cache[t][r]) {
                counter -= 1;
                if (power[n][t][r][k] == 0) {
                    abort();
                }
            }
        }
    }
    if (counter != 0) {
        abort();
    }
#endif
}
void solve() {
    std::mt19937 engine;
    std::vector<int> indices;
    for (int j = 0; j < J; ++j) {
        indices.push_back(j);
    }
    std::sort(indices.begin(), indices.end(), [](int x, int y) {
        return tbs[x] > tbs[y];
    });
    int best = 0;
    while (!tle()) {
        init();
        int cnt = 0;
        bool processed[MAXJ] = {};
        for (int limit = 1; limit <= R; limit += 1) {
            for (auto j : indices) {
                if (processed[j]) {
                    continue;
                }
                if (tle()) {
                    goto finish;
                }
                processed[j] = add(j, limit);
                cnt += processed[j];
#ifdef __SMZ_NATIVE
                check();
#endif
            }
        }
        finish:;
        if (cnt > best) {
            best = cnt;
            for (int n = 0; n < N; ++n) {
                for (int t = 0; t < T; ++t) {
                    for (int r = 0; r < R; ++r) {
                        for (int k = 0; k < K; ++k) {
                            answer[n][t][r][k] = power[n][t][r][k];
                        }
                    }
                }
            }
        }
        std::shuffle(indices.begin(), indices.end(), engine);
    }
#ifdef __SMZ_NATIVE
    printf("%d\n", best);
    check_answer(best);
#else
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            for (int r = 0; r < R; ++r) {
                for (int n = 0; n < N; ++n) {
                    printf("%.9f ", answer[n][t][r][k]);
                }
                puts("");
            }
        }
    }
#endif
}
void preprocess() {
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            for (int r = 0; r < R; ++r) {
                for (int n = 0; n < N; ++n) {
                    sinr_sum[n][t][r] += std::log2(1.0 + sinr[n][t][r][k]);
                }
            }
        }
    }
    for (int n = 0; n < N; ++n) {
        for (int t = 0; t < T; ++t) {
            for (int r = 0; r < R; ++r) {
                order[n][t][r] = r;
            }
            std::sort(order[n][t], order[n][t] + R, [val=sinr_sum[n][t]](int x, int y) {
                return val[x] > val[y];
            });
        }
    }
}
int main() {
    start_time = mutime();
#ifdef __SMZ_NATIVE
    freopen("21", "r", stdin);
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