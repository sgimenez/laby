#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

enum tile {
  laby_name_Void,
  laby_name_Wall,
  laby_name_Rock,
  laby_name_Web,
  laby_name_Exit,
  laby_name_Unknown
};

void output(char *s) {
  printf("%s\n", s);
  fflush(stdout);
}

char *input() {
  char *line = NULL;
  size_t len = 0;
  getline(&line, &len, stdin);
  if (strcmp(line, "quit\n") == 0) exit(0);
  return line;
}

void laby_name_left() { output("left"); free(input()); }

void laby_name_right() { output("right"); free(input()); }

void laby_name_forward() { output("forward"); free(input()); }

void laby_name_take() { output("take"); free(input()); }

void laby_name_drop() { output("drop"); free(input()); }

void laby_name_escape() { output("escape"); free(input()); }

enum tile laby_name_look() {
  output("look");
  char *s = input();
  enum tile answer = laby_name_Unknown;
  if (strcmp(s, "void\n") == 0) answer = laby_name_Void;
  if (strcmp(s, "wall\n") == 0) answer = laby_name_Wall;
  if (strcmp(s, "rock\n") == 0) answer = laby_name_Rock;
  if (strcmp(s, "web\n") == 0) answer = laby_name_Web;
  if (strcmp(s, "exit\n") == 0) answer = laby_name_Exit;
  free(s);
  return answer;
}

void laby_name_say(char *s) {
  printf("say %s\n", s);
  fflush(stdout);
  free(input());
}

void laby_name_ant();

int main() {
  output("start"); free(input());
  laby_name_ant();
  return 0;
}

