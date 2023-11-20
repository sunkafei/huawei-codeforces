#include <bits/stdc++.h>
constexpr int MAXN = 100 + 1;
constexpr int MAXK = 10 + 1;
constexpr int MAXT = 1000 + 1;
constexpr int MAXR = 10 + 1;
constexpr int MAXJ = 5000 + 1;
int N, K, T, R, J;
double SINR[MAXT][MAXK][MAXR][MAXN], D[MAXK][MAXR][MAXN][MAXN];
int id[MAXJ], tbs[MAXJ], belong[MAXJ], start[MAXJ], length[MAXJ];
double power[MAXT][MAXK][MAXR][MAXN];
void solve() {
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            for (int r = 0; r < R; ++r) {
                for (int x = 0; x < N; ++x) {
                    power[t][k][r][x] = 1.0 / N;
                }
            }
        }
    }
    for (int t = 0; t < T; ++t) {
        for (int k = 0; k < K; ++k) {
            for (int r = 0; r < R; ++r) {
                for (int x = 0; x < N; ++x) {
                    if (x != 0) printf(" ");
                    printf("%f", power[t][k][r][x]);
                }
                printf("\n");
            }
        }
    }
}
int main() {
#ifdef __SMZ_NATIVE
    freopen("in.txt", "r", stdin);
#endif
    scanf("%d %d %d %d", &N, &K, &T, &R);
    for (int i = 0; i < T; ++i) {
        for (int j = 0; j < K; ++j) {
            for (int k = 0; k < R; ++k) {
                for (int x = 0; x < N; ++x) {
                    scanf("%lf", &SINR[i][j][k][x]);
                }
            }
        }
    }
    for (int i = 0; i < K; ++i) {
        for (int j = 0; j < R; ++j) {
            for (int x = 0; x < N; ++x) {
                for (int y = 0; y < N; ++y) {
                    scanf("%lf", &D[i][j][x][y]);
                }
            }
        }
    }
    scanf("%d", &J);
    for (int i = 0; i < J; ++i) {
        scanf("%d %d %d %d %d", &id[i], &tbs[i], &belong[i], &start[i], &length[i]);
    }
    solve();
    return 0;
}