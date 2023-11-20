#include <bits/stdc++.h>
constexpr int MAXN = 100 + 1;
constexpr int MAXK = 10 + 1;
constexpr int MAXT = 1000 + 1;
constexpr int MAXR = 10 + 1;
constexpr int MAXJ = 5000 + 1;
constexpr int MAXBUFFER = 1024 * 1024;
int N, K, T, R, J;
double sinr[MAXT][MAXK][MAXR][MAXN], D[MAXK][MAXR][MAXN][MAXN];
int id[MAXJ], tbs[MAXJ], belong[MAXJ], start[MAXJ], length[MAXJ];
double power[MAXT][MAXK][MAXR][MAXN];
char buffer[MAXBUFFER * 2];
const char *pointer = buffer + MAXBUFFER;
double sinr_sum[MAXT][MAXN][MAXR];
int order[MAXT][MAXN][MAXR];
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
int vis[MAXT][MAXR];
void add(int j) {
    const int n = belong[j];
    double tbs = ::tbs[j] / 192.0;
    std::vector<std::pair<int, int>> occupy; 
    for (int t = start[j]; t < start[j] + length[j]; ++t) {
        for (int i = 0; i < R; ++i) {
            const int r = order[t][n][i];
            if (!vis[t][r]) {
                occupy.emplace_back(t, r);
                for (int k = 0; k < K; ++k) {
                    tbs -= std::log2(1.0 + sinr[t][k][r][n]);
                }
                if (tbs < 0) {
                    goto end;
                }
                break;
            }
        }
    }
    end:;
    if (tbs < 0) {
        for (auto [t, r] : occupy) {
            vis[t][r] = true;
            for (int k = 0; k < K; ++k) {
                power[t][k][r][n] = 1;
            }
        }
    }
}
void solve() {
    for (int j = 0; j < J; ++j) {
        add(j);
    }
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            for (int r = 0; r < R; ++r) {
                for (int x = 0; x < N; ++x) {
                    if (x != 0) putchar(' ');
                    printf("%.9f", power[t][k][r][x]);
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
                    sinr_sum[t][n][r] += std::log2(1.0 + sinr[t][k][r][n]);
                }
            }
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int n = 0; n < N; ++n) {
            for (int r = 0; r < R; ++r) {
                order[t][n][r] = r;
            }
            std::sort(order[t][n], order[t][n] + R, [val=sinr_sum[t][n]](int x, int y) {
                return val[x] > val[y];
            });
        }
    }
}
int main() {
#ifdef __SMZ_NATIVE
    freopen("in.txt", "r", stdin);
#endif
    ::N = read_int();
    ::K = read_int();
    ::T = read_int();
    ::R = read_int();
    for (int i = 0; i < T; ++i) {
        for (int j = 0; j < K; ++j) {
            for (int k = 0; k < R; ++k) {
                for (int x = 0; x < N; ++x) {
                    sinr[i][j][k][x] = read_double();
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