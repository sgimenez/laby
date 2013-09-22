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
  unowned EnumValue? eval = ((EnumClass) typeof (Tile).class_ref ()).get_value_by_name (outin ("look"));
  if (eval == null) {
    return Tile.Unknown;
  }
  else {
    return (Tile) eval.value;
  }
}

void laby_name_say (string s) { outin (@"say $s\n"); }

int main () {
  outin ("start"); 
  laby_name_ant ();
  return 0;
}

