input(X) :- read_atom(X), \+ (X == 'quit'); halt(1).

laby_name_left :- write('left\n'), input(_).
laby_name_right :- write('right\n'), input(_).
laby_name_forward :- write('forward\n'), input(_).
laby_name_take :- write('take\n'), input(_).
laby_name_drop :- write('drop\n'), input(_).
laby_name_escape :- write('escape\n'), input(_).

laby_name_look(X) :- write('look\n'), input(X).

laby_name_say(X) :- write('say '), write(X), write('\n'), input(_).

start :- write('start\n'), input(_), laby_name_ant.
:- initialization(start).
