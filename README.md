Laby
====

Laby is an easy to use IDE to program a rock-carrying ant-robot trapped in a
maze. Usually there is a door the robot wants to reach, but before that, it
needs to sort out several traps on its way. Help the robot by writing a
program that guides it out of the maze. Even if you have not programmed
before this is a great game to start. Help the robot and learn to
program several languages at the same time: Ruby, Pascal, Perl,
OCaml, and Java. They are mini-versions of the original languages, but you
will still learn the fundamental of loops, conditionals and functions and what is
better you will have fun while doing that.

Enjoy!

Example
-------

Here is a code example and everything you need for Ruby, let us know if
you want this example in the other languages too. It is the solution of
the level 2b. We are planning adding a lot of levels and some AI for 
complex path finding.


  require "./robot"

  # Level 2a
  #   ....+....1....
  # 1 +-------------
  # . |a           d
  # . +-------------
  # a = Ant Robot
  # d = Door
  #
  # Help:
  #
  # Constants:
  #   Void - empty space
  #   Rock - there is a Rock
  #   Web - there is a Web
  #   (more...) later :)
  #
  # Built-in functions:
  #   look() - look forward and returns and object name
  #
  # Basic Movement:
  #   forward
  #   left
  #   right
  #   escape - open door
  #
  # Rocks:
  #   take - take rock in front
  #   drop - drop rock to front
  #
  # Loops:
  #   while look() == Void
  #     ...
  #   end
  #
  # Conditionals:
  #   if look() == Web
  #      ...
  #   else
  #      ...
  #   end
  #
  # Functions:
  #   def f
  #     ...
  #   end

  # Example Code to solve Level 2b:
  class Ant < Robot
    def ant
      while look() == Void
        forward
      end
      escape
    end
  end

  Ant.new
