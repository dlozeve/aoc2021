#include <stdio.h>

int main(void)
{
  int buf[5000] = {0};
  int count = 0;
  while (scanf("%d\n", &buf[count++]) != EOF);
  count--;

  int part1 = 0;
  for (int i = 1; i < count; ++i) {
    part1 += buf[i] > buf[i - 1];
  }
  printf("%d\n", part1);

  int part2 = 0;
  for (int i = 3; i < count; ++i) {
    part2 += buf[i] > buf[i-3];
  }
  printf("%d\n", part2);

  return 0;
}
