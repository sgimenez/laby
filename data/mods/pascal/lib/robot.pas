unit robot;


interface

type tile = (
  laby_name_Void,
  laby_name_Wall,
  laby_name_Rock,
  laby_name_Web,
  laby_name_Exit,
  laby_name_Unknown
);

procedure laby_name_left();
procedure laby_name_right();
procedure laby_name_forward();
procedure laby_name_escape();
procedure laby_name_take();
procedure laby_name_drop();
function laby_name_look(): tile;
procedure laby_name_say(s: string);


implementation

procedure output_s(s: string);
begin
  writeln(s);
  flush(Output);
end;

function input_s(): string;
var line: string;
begin
  readln(Input, line);
  if(line = 'quit') then halt(0);
  input_s := line;
end;

procedure laby_name_left();
begin
  output_s('left');
  input_s;
end;

procedure laby_name_right();
begin
  output_s('right');
  input_s;
end;

procedure laby_name_forward();
begin
  output_s('forward');
  input_s;
end;

procedure laby_name_take();
begin
  output_s('take');
  input_s;
end;

procedure laby_name_drop();
begin
  output_s('drop');
  input_s;
end;

procedure laby_name_escape();
begin
  output_s('escape');
  input_s;
end;

function laby_name_look(): tile;
var s: string;
	answer: tile;
begin
  output_s('look');
  s := input_s();
  answer := laby_name_Unknown;
  if (s = 'void') then answer := laby_name_Void;
  if (s = 'wall') then answer := laby_name_Wall;
  if (s = 'rock') then answer := laby_name_Rock;
  if (s = 'web') then answer := laby_name_Web;
  if (s = 'exit') then answer := laby_name_Exit;
  laby_name_look := answer;
end;

procedure laby_name_say(s: string);
begin
  writeln('say ', s);
  flush(Output);
  input_s;
end;

procedure laby_name_ant();
begin
end;

begin
  output_s('start');
  input_s;
  laby_name_ant();
end.