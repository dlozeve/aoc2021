#include <stdlib.h>
#include <stdio.h>

int main(void)
{
  int buf[5000] = {0};
  int count = 0;
  while (scanf("%d\n", &buf[count++]) != EOF);
  count--;

  int part1 = 0;
  for (int i = 1; i < count; ++i) {
    if (buf[i] > buf[i - 1]) part1++;
  }
  printf("Part 1: %d\n", part1);

  int part2 = 0;
  for (int i = 3; i < count; ++i) {
    if (buf[i] + buf[i-1] + buf[i-2] > buf[i-1] + buf[i-2] + buf[i-3])
      part2++;
  }
  printf("Part 2: %d\n", part2);

  return 0;
}
