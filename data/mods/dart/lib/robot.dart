import 'dart:io';

import 'program.dart';

const laby_name_Void = 'Void';
const laby_name_Wall = 'Wall';
const laby_name_Rock = 'Rock';
const laby_name_Web = 'Web';
const laby_name_Exit = 'Exit';
const laby_name_Unknown = 'Unknown';

String input() {
  String line = stdin.readLineSync();

  if (line == 'quit') {
    exit(0);
  }

  return line;
}

void laby_name_start() {
  stdout.writeln('start');
  input();
}

void laby_name_left() {
  stdout.writeln('left');
  input();
}

void laby_name_right() {
  stdout.writeln('right');
  input();
}

void laby_name_forward() {
  stdout.writeln('forward');
  input();
}

void laby_name_take() {
  stdout.writeln('take');
  input();
}

void laby_name_drop() {
  stdout.writeln('drop');
  input();
}

void laby_name_escape() {
  stdout.writeln('escape');
  input();
}

void laby_name_say(String s) {
  stdout.writeln('say $s');
  input();
}

String laby_name_look() {
  stdout.writeln('look');

  var ans = input();

  switch (ans) {
    case 'void':
      return laby_name_Void;
    case 'wall':
      return laby_name_Wall;
    case 'rock':
      return laby_name_Rock;
    case 'web':
      return laby_name_Web;
    case 'exit':
      return laby_name_Exit;
    default:
      return laby_name_Unknown;
  }
}

void main() {
  laby_name_start();
  laby_name_ant();
}
