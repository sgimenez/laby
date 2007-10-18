import java.io.InputStreamReader;
import java.io.BufferedReader;

public class Robot {

  BufferedReader r;

  public enum Case { VIDE, MUR, ROCHE, TOILE, SORTIE }

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

  public void gauche() {
      output("left"); input();
  }
  public void droite() {
      output("right"); input();
  }
  public void avance() {
      output("forward"); input();
  }

  public Case regarde() {
    output("look");
    String ans = input ();
    if (ans.equals("void")) return Case.VIDE;
    if (ans.equals("wall")) return Case.MUR;
    if (ans.equals("rock")) return Case.ROCHE;
    if (ans.equals("web")) return Case.TOILE;
    if (ans.equals("exit")) return Case.SORTIE;
    assert false;
    return Case.VIDE;
  }

  public void ouvre() {
      output("open"); input();
  }
  public void prend() {
      output("take"); input();
  }
  public void pose() {
      output("drop"); input();
  }

}