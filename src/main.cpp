#include <bits/stdc++.h>
#include <sys/time.h>
constexpr int MAXN = 100 + 1;
constexpr int MAXK = 10 + 1;
constexpr int MAXT = 1000 + 1;
constexpr int MAXR = 10 + 1;
constexpr int MAXJ = 5000 + 1;
constexpr int MAXBUFFER = 1024 * 1024;
constexpr double EPS = 1e-5;
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
    if (runtime > 1200 * 1000) {
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
int vis[MAXT][MAXR], thickness[MAXT];
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
            vis[t][r] = false;
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            rest[t][k] = R;
        }
    }
}
inline bool add(int j) {
    const int n = belong[j];
    double tbs = ::tbs[j] / 192.0;
    using item_t = std::tuple<int, double, int, int>;
    std::vector<item_t> candidates; 
    for (int t = start[j]; t < start[j] + length[j]; ++t) {
        for (int i = 0; i < R; ++i) {
            const int r = order[n][t][i];
            if (!vis[t][r]) {
                candidates.emplace_back(thickness[t], -sinr_sum[n][t][r], t, r);
                break;
            }
        }
    }
    std::sort(candidates.begin(), candidates.end());
    double sum = 0;
    for (double v = 1; v <= 4 + 1e-5; v += 1) {
        for (auto [_, val, t, r] : candidates) {
            vis[t][r] = true;
            for (int x = 0; x < K; ++x) {
                int k = order2[n][t][r][x];
                sum -= std::log2(1.0 + power[n][t][r][k] * sinr[n][t][r][k]);
                power[n][t][r][k] = std::min(v, rest[t][k]);
                sum += std::log2(1.0 + power[n][t][r][k] * sinr[n][t][r][k]);
                if (sum > tbs) {
                    double delta = tbs - sum + std::log2(1.0 + power[n][t][r][k] * sinr[n][t][r][k]);
                    power[n][t][r][k] = (std::exp2(delta + EPS) - 1) / sinr[n][t][r][k];
                    sum = tbs + delta;
                    goto finish;
                }
            }
        }
    }
    finish:;
    if (sum > tbs) {
        for (auto [_, val, t, r] : candidates) {
            if (vis[t][r]) {
                thickness[t] += 1;
            }
            for (int k = 0; k < K; ++k) {
                rest[t][k] -= power[n][t][r][k];
            }
        }
        return true;
    }
    for (auto [_, val, t, r] : candidates) {
        vis[t][r] = false;
        for (int k = 0; k < K; ++k) {
            power[n][t][r][k] = 0;
        }
    }
    return false;
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
        for (auto j : indices) {
            cnt += add(j);
        }
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
    freopen("in.txt", "r", stdin);
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
                    D[i][j][x][y] = read_double();
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