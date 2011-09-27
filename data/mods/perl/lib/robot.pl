use strict;
use warnings;

sub output {
     print "$_[0]\n";
     $|++;
}

sub input {
    my $line = <STDIN>;
    if ($line eq "quit\n")
    { exit(0);}
    return $line;
}

sub laby_name_left {
  output("left");
  input();
}

sub laby_name_right {
  output("right");
  input();
}

sub laby_name_forward {
  output("forward");
  input();
}

sub laby_name_take {
  output("take");
  input();
}

sub laby_name_drop {
  output("drop");
  input();
}

sub laby_name_escape {
    output("escape");
    input();
}

sub laby_name_say {
    output("say $_[0]");
    input();
}


use constant laby_name_Void => 0;
use constant laby_name_Wall => 1;
use constant laby_name_Rock => 2;
use constant laby_name_Web => 3;
use constant laby_name_Exit => 4;
use constant laby_name_Unknown => 5;

sub laby_name_look {
  output("look");
  my $ans = input();
  if ($ans eq "void\n"){ return laby_name_Void;}
  if ($ans eq "wall\n"){ return laby_name_Wall;}
  if ($ans eq "rock\n"){ return laby_name_Rock;}
  if ($ans eq "web\n"){ return laby_name_Web;}
  if ($ans eq "exit\n"){ return laby_name_Exit;}
  return laby_name_Unknown;
}

output("start");
input();
