#include <bits/stdc++.h>
constexpr int MAXN = 100 + 1;
constexpr int MAXK = 10 + 1;
constexpr int MAXT = 1000 + 1;
constexpr int MAXR = 10 + 1;
constexpr int MAXJ = 5000 + 1;
constexpr int MAXBUFFER = 1024 * 1024;
int N, K, T, R, J;
double SINR[MAXT][MAXK][MAXR][MAXN], D[MAXK][MAXR][MAXN][MAXN];
int id[MAXJ], tbs[MAXJ], belong[MAXJ], start[MAXJ], length[MAXJ];
double power[MAXT][MAXK][MAXR][MAXN];
char buffer[MAXBUFFER * 2];
const char *pointer = buffer + MAXBUFFER;
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
int count[MAXT], vis[MAXT][MAXN];
void solve() {
    for (int j = 0; j < J; ++j) {
        for (int i = start[j]; i < start[j] + length[j]; ++i) {
            count[i] += 1;
            vis[i][belong[j]] = true;
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            for (int r = 0; r < R; ++r) {
                for (int x = 0; x < N; ++x) {
                    if (vis[t][x]) {
                        power[t][k][r][x] = 1.0 / count[t];
                    }
                }
            }
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            for (int r = 0; r < R; ++r) {
                for (int x = 0; x < N; ++x) {
                    if (x != 0) putchar(' ');
                    printf("%f", power[t][k][r][x]);
                }
                puts("");
            }
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
                    SINR[i][j][k][x] = read_double();
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
    solve();
    return 0;
}