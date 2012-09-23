importPackage(java.io);
importPackage(java.lang);

const stdin = new BufferedReader( new InputStreamReader(System['in']) )

function output(s)
{
  print(s);
}

function input()
{
  var s = stdin.readLine();
}

function laby_name_left()
{
  output("left"); var s = input();
}
function laby_name_right()
{
  output("right"); var s = input();
}
function laby_name_forward()
{
  output("forward"); var s = input();
}
function laby_name_take()
{
  output("take"); var s = input();
}
function laby_name_drop()
{
  output("drop"); var s = input();
}
function laby_name_escape()
{
  output("escape"); var s = input();
}
function laby_name_say(s)
{
  output("say "+s); var s = input();
}



const laby_name_Void = 0;
const laby_name_Wall = 1;
const laby_name_Rock = 2;
const laby_name_Web = 3;
const laby_name_Exit = 4;
const laby_name_Unknown = 5;

function laby_name_look()
{
  output("look");
  var ans = input();
    if (ans == "void\n")
{ return laby_name_Void;}
    if (ans == "wall\n")
{ return laby_name_Wall;}
    if (ans == "rock\n")
{ return laby_name_Rock;}
    if (ans == "web\n")
{ return laby_name_Web;}
    if (ans == "exit\n")
{ return laby_name_Exit;}
    return laby_name_Unknown;
}

output("start");
var s = input();
