#include <stdio.h>

int main() {
  char instr[9] = {0};
  int n = 0;

  int pos = 0;
  int depth = 0;
  int aim = 0;

  while (scanf("%s %d\n", instr, &n) == 2) {
    switch (instr[0]) {
    case 'd':
      aim += n;
      break;
    case 'u':
      aim -= n;
      break;
    case 'f':
      pos += n;
      depth += aim * n;
      break;
    default:
      fprintf(stderr, "unknown instruction: %s", instr);
      break;
    }
  }
  printf("%d\n", pos * aim);
  printf("%d\n", pos * depth);

  return 0;
}
