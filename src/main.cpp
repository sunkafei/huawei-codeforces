#include <bits/stdc++.h>
#include <sys/time.h>
constexpr int MAXN = 100 + 1;
constexpr int MAXK = 10 + 1;
constexpr int MAXT = 1000 + 1;
constexpr int MAXR = 10 + 1;
constexpr int MAXJ = 5000 + 1;
constexpr int MAXBUFFER = 1024 * 1024;
constexpr double EPS = 1e-6;
int N, K, T, R, J;
double sinr[MAXN][MAXT][MAXR][MAXK], D[MAXK][MAXR][MAXN][MAXN];
int id[MAXJ], tbs[MAXJ], belong[MAXJ], start[MAXJ], length[MAXJ];
char buffer[MAXBUFFER * 2];
const char *pointer = buffer + MAXBUFFER;
double sinr_sum[MAXN][MAXT][MAXR];
int order[MAXN][MAXT][MAXR], order2[MAXN][MAXT][MAXR][MAXK];
uint64_t start_time;
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
int cover[MAXT][MAXR], thickness[MAXT], position[MAXT][MAXR];
long long visit[MAXT][MAXK], timestamp = 1;
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
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            rest[t][k] = R - EPS;
        }
    }
}
inline bool add(int j, int limit=4) {
    timestamp += 1;
    const int n = belong[j];
    double tbs = ::tbs[j] / 192.0;
    using item_t = std::tuple<int, double, int, int>;
    std::vector<item_t> candidates; 
    for (int t = start[j]; t < start[j] + length[j]; ++t) {
        for (int i = 0; i < R; ++i) {
            const int r = order[n][t][i];
            if (!cover[t][r]) {
                candidates.emplace_back(thickness[t], -sinr_sum[n][t][r], t, r);
                break;
            }
        }
    }
    std::sort(candidates.begin(), candidates.end());
    double sum = 0;
    std::vector<std::tuple<int, int, int, int, double>> backup;
    for (int iteration = 0; iteration < limit; ++iteration) {
        for (auto [_, val, t, r] : candidates) {
            if (iteration == 0) {
                thickness[t] += 1;
            }
            for (int x = 0; x < K; ++x) {
                auto k = order2[n][t][r][x];
                auto prev = power[n][t][r][k];
                sum -= std::log2(1.0 + power[n][t][r][k] * sinr[n][t][r][k]);
                double delta = std::min(rest[t][k], 1.0);
                power[n][t][r][k] += delta;
                rest[t][k] -= delta;
                sum += std::log2(1.0 + power[n][t][r][k] * sinr[n][t][r][k]);
                if (prev == 0 && power[n][t][r][k] > 0) {
                    cover[t][r] += 1;
                    position[t][r] = k;
                    visit[t][k] = timestamp;
                }
                if (sum > tbs) {
                    double delta = tbs - sum + std::log2(1.0 + power[n][t][r][k] * sinr[n][t][r][k]);
                    power[n][t][r][k] = std::min(power[n][t][r][k], (std::exp2(delta) - 1) / sinr[n][t][r][k] + EPS);
                    sum = tbs + delta;
                    goto finish;
                }
            }
        }
    }
    if (sum < tbs) {
        std::vector<std::tuple<double, int, int, int, int>> cells;
        for (int t = start[j]; t < start[j] + length[j]; ++t) {
            for (int r = 0; r < R; ++r) if (cover[t][r] == 1) {
                int k = position[t][r];
                if (power[n][t][r][k] > 0 || visit[t][k] == timestamp) {
                    continue;
                }
                int m = 0;
                for (; m < N; ++m) {
                    if (power[m][t][r][k] > 0) {
                        break;
                    }
                }
                cells.emplace_back(-sinr[n][t][r][k] * D[k][r][n][m], t, r, k, m);
            }
        }
        std::sort(cells.begin(), cells.end());
        int counter = 0;
        for (auto [_, t, r, k, m] : cells) {
            if (visit[t][k] == timestamp) {
                continue;
            }
            visit[t][k] = timestamp;
            auto value = std::log2(1.0 + power[m][t][r][k] * sinr[m][t][r][k]);
            auto delta = (std::exp2(value) - 1.0) / sinr[m][t][r][k] / D[k][r][n][m] - power[m][t][r][k];
            if (rest[t][k] - delta <= EPS || power[m][t][r][k] + delta > 4.0) {
                continue;
            }
            backup.emplace_back(m, t, r, k, delta);
            power[m][t][r][k] += delta;
            power[n][t][r][k] = std::min(rest[t][k] - delta, 4.0 - power[m][t][r][k]);
            sum += std::log2(1.0 + power[n][t][r][k] * sinr[n][t][r][k] * D[k][r][n][m]);
            cover[t][r] += 1;
            rest[t][k] -= delta + power[n][t][r][k];
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
        cover[t][r] -= 1;
        rest[t][k] += delta + power[n][t][r][k];
        power[m][t][r][k] -= delta;
        power[n][t][r][k] = 0;
    }
    for (auto [_, val, t, r] : candidates) {
        thickness[t] -= 1;
        for (int k = 0; k < K; ++k) {
            if (power[n][t][r][k] > 0) {
                cover[t][r] -= 1;
                rest[t][k] += power[n][t][r][k];
                power[n][t][r][k] = 0;
            }
        }
    }
    return false;
}
inline void check() {
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
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            double sum = 0;
            for (int r = 0; r < R; ++r) {
                double total = 0;
                for (int n = 0; n < N; ++n) {
                    sum += answer[n][t][r][k];
                    total += answer[n][t][r][k];
                    if (answer[n][t][r][k] < 0) {
                        abort();
                    }
                }
                if (total > 4) {
                    abort();
                }
            }
            if (sum > R) {
                abort();
            }
        }
    }
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
        for (int limit = 1; limit < 4; limit += 1) {
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
    check();
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
    for (int n = 0; n < N; ++n) {
        for (int t = 0; t < T; ++t) {
            for (int r = 0; r < R; ++r) {
                for (int k = 0; k < K; ++k) {
                    order2[n][t][r][k] = k;
                }
                std::sort(order2[n][t][r], order2[n][t][r] + K, [val=sinr[n][t][r]](int x, int y) {
                    return val[x] > val[y];
                });
            }
        }
    }
}
int main() {
    start_time = mutime();
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