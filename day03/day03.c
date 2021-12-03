#include <stdio.h>

#define N 1000
#define M 12

int main() {
  char c = 0;
  int i = 0, j = 0;
  char in[N][M] = {0};

  while ((c = getchar()) != EOF) {
    switch (c) {
    case '\n': i++; j = 0; break;
    case '1': in[i][j] = 1; j++; break;
    case '0': j++; break;
    default: fprintf(stderr, "unknown digit %c", c); return 1;
    }
  }

  // Part 1
  int counts[M] = {0};
  for (i = 0; i < N; ++i) {
    for (j = 0; j < M; ++j) {
      counts[j] += in[i][j];
    }
  }
  int gamma = 0, epsilon = 0;
  for (j = 0; j < M; ++j) {
    gamma = (gamma << 1) + (counts[j] > N/2);
    epsilon = (epsilon << 1) + (counts[j] <= N/2);
  }
  printf("%d\n", gamma * epsilon);

  return 0;
}
