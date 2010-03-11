import java.io.InputStreamReader;
import java.io.BufferedReader;

public class Robot {

  BufferedReader r;
  public enum Tile {
      laby_name_Void, laby_name_Wall, laby_name_Rock,
      laby_name_Web, laby_name_Exit, laby_name_Unknown
  }

  public Tile laby_name_Void = Tile.laby_name_Void;
  public Tile laby_name_Wall = Tile.laby_name_Wall;
  public Tile laby_name_Rock = Tile.laby_name_Rock;
  public Tile laby_name_Web = Tile.laby_name_Web;
  public Tile laby_name_Exit = Tile.laby_name_Exit;
  public Tile laby_name_Unknown = Tile.laby_name_Unknown;

  Robot() {
      r = new BufferedReader(new InputStreamReader(System.in));
      output("start"); input();
  }

  public void output(String s) {
      System.out.format("%s%n", s);
  }

  String input() {
      try {
	  String s = r.readLine();
	  if (s.equals("quit")) System.exit(0);
	  return s;
      }
      catch (java.io.IOException e) {
	  System.exit(1);
	  return null;
      }
  }

  public void laby_name_left() {
      output("left"); input();
  }

  public void laby_name_right() {
      output("right"); input();
  }

  public void laby_name_forward() {
      output("forward"); input();
  }

  public void laby_name_take() {
      output("take"); input();
  }

  public void laby_name_drop() {
      output("drop"); input();
  }

  public void laby_name_escape() {
      output("escape"); input();
  }

  public Tile laby_name_look() {
    output("look");
    String ans = input ();
    if (ans.equals("void")) return Tile.laby_name_Void;
    if (ans.equals("wall")) return Tile.laby_name_Wall;
    if (ans.equals("rock")) return Tile.laby_name_Rock;
    if (ans.equals("web")) return Tile.laby_name_Web;
    if (ans.equals("exit")) return Tile.laby_name_Exit;
    return Tile.laby_name_Unknown;
  }

  public void laby_name_say(String s) {
      System.out.format("say %s%n", s); input();
  }
}
