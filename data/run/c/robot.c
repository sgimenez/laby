#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

const int VIDE = 0;
const int MUR = 1;
const int CAILLOU = 2;
const int TOILE = 3;
const int SORTIE = 4;

void output(char *s) {
  printf("%s\n", s);
  fflush(stdout);
}

char *input() {
  char *line = NULL;
  size_t len = 0;
  getline(&line, &len, stdin);
  return line;
}

void gauche() { output("left"); free(input()); }

void droite() { output("right"); free(input()); }

void avance() { output("forward"); free(input()); }

int regarde() {
  output("look");
  char *s = input();
  if (strcmp(s, "void\n") == 0) { free(s); return VIDE; }
  if (strcmp(s, "wall\n") == 0) { free(s); return MUR; }
  if (strcmp(s, "rock\n") == 0) { free(s); return CAILLOU; }
  if (strcmp(s, "web\n") == 0) { free(s); return TOILE; }
  if (strcmp(s, "exit\n") == 0) { free(s); return SORTIE; }
  fprintf(stderr,"robot: unknown tile : %s\n", s);
  exit(1);
}

void ouvre() { output("open"); free(input()); }

void prend() { output("take"); free(input()); }

void pose() { output("drop"); free(input()); }

void fourmi();

int main() {
  output("start"); free(input());
  fourmi();
  return 0;
}

