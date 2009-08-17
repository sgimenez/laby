import java.io.InputStreamReader;
import java.io.BufferedReader;

public class Robot {

  BufferedReader r;
  public enum Tile {
      laby_name_Void, laby_name_Wall, laby_name_Rock,
      laby_name_Web, laby_name_Exit
  }

  public Tile laby_name_Void = Tile.laby_name_Void;
  public Tile laby_name_Wall = Tile.laby_name_Wall;
  public Tile laby_name_Rock = Tile.laby_name_Rock;
  public Tile laby_name_Web = Tile.laby_name_Web;
  public Tile laby_name_Exit = Tile.laby_name_Exit;

  Robot() {
      r = new BufferedReader(new InputStreamReader(System.in));
      output("start"); input();
  }

  public void output(String s) {
      System.out.format("%s%n", s);
  }

  String input() {
      try {
	  return r.readLine();
      }
      catch (java.io.IOException e) {
	  return "";
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

  public Tile laby_name_look() {
    output("look");
    String ans = input ();
    if (ans.equals("void")) return Tile.laby_name_Void;
    if (ans.equals("wall")) return Tile.laby_name_Wall;
    if (ans.equals("rock")) return Tile.laby_name_Rock;
    if (ans.equals("web")) return Tile.laby_name_Web;
    if (ans.equals("exit")) return Tile.laby_name_Exit;
    assert false;
    return Tile.laby_name_Void;
  }

  public void laby_name_escape() {
      output("escape"); input();
  }
  public void laby_name_take() {
      output("take"); input();
  }
  public void laby_name_drop() {
      output("drop"); input();
  }

}