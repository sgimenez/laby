<?php

  define('laby_name_Void', 0);
  define('laby_name_Wall', 1);
  define('laby_name_Rock', 2);
  define('laby_name_Web', 3);
  define('laby_name_Exit', 4);
  define('laby_name_Unknown', 5);

  function output($s) {
    echo $s . PHP_EOL;
  }

  function check($s, $expected) {
    return $s === ($expected . PHP_EOL);
  }

  function input() {
    $line = fgets(STDIN);
    
    if (check($line, 'quit')) {
      exit(0);
    }
    
    return $line;
  }

  function laby_name_left() {
    output('left');
    input();
  }

  function laby_name_right() {
    output('right');
    input();
  }

  function laby_name_forward() {
    output('forward');
    input();
  }

  function laby_name_take() {
    output('take');
    input();
  }

  function laby_name_drop() {
    output('drop');
    input();
  }

  function laby_name_escape() {
    output('escape');
    input();
  }

  function laby_name_say( $s ) {
    output('say ' . $s);
    input();
  }

  function laby_name_look() {
    output('look');
    $ans = input();
    
    if (check($ans, 'void')) {
      return laby_name_Void;
    }
    
    if (check($ans, 'wall')) {
      return laby_name_Wall;
    }
    
    if (check($ans, 'rock')) {
      return laby_name_Rock;
    }
    
    if (check($ans, 'web')) {
      return laby_name_Web;
    }
    
    if (check($ans, 'exit')) {
      return laby_name_Exit;
    }
    
    return laby_name_Unknown;
  }

  output('start');
  input();

