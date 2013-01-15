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
  return s;
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
    if (ans.match("void"))
{ return laby_name_Void;}
    if (ans.match("wall"))
{ return laby_name_Wall;}
    if (ans.match("rock"))
{ return laby_name_Rock;}
    if (ans.match("web"))
{ return laby_name_Web;}
    if (ans.match("exit"))
{ return laby_name_Exit;}
    return laby_name_Unknown;
}

output("start");
var s = input();
