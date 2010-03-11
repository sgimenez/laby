function output(s)
  io.write(s, "\n");
  io.flush();
end

function input()
    r = io.read();
    if (r == "quit") then exit(0) end;
    return r;
end

function laby_name_left()
  output("left"); input();
end

function laby_name_right()
  output("right"); input();
end

function laby_name_forward()
  output("forward"); input();
end

function laby_name_take()
  output("take"); input();
end

function laby_name_drop()
  output("drop"); input();
end

function laby_name_escape()
  output("escape"); input();
end

function laby_name_say(s)
  output("say " .. s); input();
end

laby_name_Void = 0;
laby_name_Wall = 1;
laby_name_Rock = 2;
laby_name_Web = 3;
laby_name_Exit = 4;
laby_name_Unknown = 5;

function laby_name_look()
  output("look");
  local ans = input();
  if (ans == "void") then return laby_name_Void end;
  if (ans == "wall") then return laby_name_Wall end;
  if (ans == "rock") then return laby_name_Rock end;
  if (ans == "web") then return laby_name_Web end;
  if (ans == "exit") then return laby_name_Exit end;
  return laby_name_Unknown;
end

output("start");
input()
