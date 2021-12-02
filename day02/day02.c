#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
  int pos;
  int depth;
  int aim;
} State;

int main()
{
  char instr[256] = {0};
  int n = 0;

  State part1 = {0};
  State part2 = {0};

  while (scanf("%s %d\n", instr, &n) == 2) {
    if (!strncmp(instr, "down", 4)) {
      part1.depth += n;
      part2.aim += n;
    } else if (!strncmp(instr, "up", 2)) {
      part1.depth -= n;
      part2.aim -= n;
    } else if (!strncmp(instr, "forward", 7)) {
      part1.pos += n;
      part2.pos += n;
      part2.depth += part2.aim * n;
    } else {
      fprintf(stderr, "unknown instruction: %s", instr);
    }
  }
  printf("%d\n", part1.pos * part1.depth);
  printf("%d\n", part2.pos * part2.depth);

  return 0;
}
