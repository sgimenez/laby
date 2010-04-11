import sys;

def output(s):
  sys.stdout.write(s + "\n");
  sys.stdout.flush();

def input():
  l = sys.stdin.readline();
  if l == "quit\n": exit(0);
  return l;

def laby_name_left():
  output("left");
  input();

def laby_name_right():
  output("right"); input();

def laby_name_forward():
  output("forward"); input();

def laby_name_take():
  output("take"); input();

def laby_name_drop():
  output("drop"); input();

def laby_name_escape():
  output("escape"); input();

def laby_name_say(s):
  output("say " + s); input();

laby_name_Void = 0;
laby_name_Wall = 1;
laby_name_Rock = 2;
laby_name_Web = 3;
laby_name_Exit = 4;
laby_name_Unknown = 5;

def laby_name_look():
  output("look");
  ans = input();
  if (ans == "void\n"): return laby_name_Void;
  if (ans == "wall\n"): return laby_name_Wall;
  if (ans == "rock\n"): return laby_name_Rock;
  if (ans == "web\n"): return laby_name_Web;
  if (ans == "exit\n"): return laby_name_Exit;
  return laby_name_Unknown;

output("start");
input()
