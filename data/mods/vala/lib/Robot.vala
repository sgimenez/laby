enum Tile {
  laby_name_Void,
  laby_name_Wall,
  laby_name_Rock,
  laby_name_Web,
  laby_name_Exit,
  laby_name_Unknown
}

string outin (string s) {
  stdout.printf ("%s\n", s);
  stdout.flush ();
  string line = stdin.read_line ();
  if (line == "quit") {
    Process.exit (0);
  }
  return line;
}

void laby_name_left ()    { outin ("left");    }
void laby_name_right ()   { outin ("right");   }
void laby_name_forward () { outin ("forward"); }
void laby_name_take ()    { outin ("take");    }
void laby_name_drop ()    { outin ("drop");    }
void laby_name_escape ()  { outin ("escape");  }

Tile laby_name_look() {
  Tile answer;
  switch (outin ("look")) {
    case "void": answer = Tile.laby_name_Void;    break;
    case "wall": answer = Tile.laby_name_Wall;    break;
    case "rock": answer = Tile.laby_name_Rock;    break;
    case "web" : answer = Tile.laby_name_Web;     break;
    case "exit": answer = Tile.laby_name_Exit;    break;
    default    : answer = Tile.laby_name_Unknown; break;
  }
  return answer;
}

void laby_name_say (string s) { outin (@"say $s\n"); }

int main () {
  outin ("start"); 
  laby_name_ant ();
  return 0;
}

